## Functions to search metadata using raw user input

#' Retrieve ontology term maps from a directory with .csv files
#' 
#' @importFrom tidyverse str_match
#' 
#' @param directory Path to directory containing .csv map files
#' @param map_file_pattern Regular expression to detect map files; May contain a capturing group to retrieve feature names from map filenames
#' 
#' @return List of maps as data frames
#' 
getMaps <- function(directory, map_file_pattern) {
  files <- list.files(directory, pattern = map_file_pattern)
  filepaths <- sapply(files, function (x) paste0(directory, "/", x), USE.NAMES = FALSE)
  maps <- lapply(filepaths, read.csv)
  features <- str_match(files, map_file_pattern)
  if (ncol(features) > 1) {
    features <- features[,2]
    names(maps) <- features
  }
  
  return(maps)
}

#' Retrieve and save ancestors of ontology terms
#'
#' @param ids Character vector of term ids
#' @param dbs Character vector of corresponding ontology names. Single string also accepted if all terms share a single ontology.
#' 
#' @return List of character vectors of ancestors named by original term
#' 
getAncestors <- function(ids, dbs) {
  map <- data.frame(id = ids,
                    db = dbs)
  tryCatch({
    onto_frames <- split(map, map$db)
    onto_terms <- lapply(onto_frames, function(x) x$id)
    onto_nodes <- mapply(function(n, t) getNodes(n, t), names(onto_terms), onto_terms, SIMPLIFY = FALSE)
    onames <- unlist(lapply(onto_nodes, function(x) names(x)), use.names = FALSE)
    oancs <- unlist(onto_nodes, recursive = FALSE, use.names = FALSE)
    names(oancs) <- onames
    return(oancs)
  }, error = function(e) {
    print(e)
  })
}

#' Search OLS for ontology terms with plain text input
#' 
#' @importFrom rols OlsSearch olsSearch
#' 
#' @param search Character search string
#' @param exact Optional Boolean; Defaults to FALSE; Whether to restrict the OLS search to exact matches
#' @param ontology Optional character vector of ontology names to restrict OLS search to
#' @param num_results Optional number of results to return; default = 20
#' 
#' @return Character vector of ontology term ids
#' 
ontoSearch <- function(search, exact = FALSE, ontology = NA, num_results = 20) {
  if (is.na(ontology)) {
    qry <- OlsSearch(q = search, exact = exact, rows = num_results)
  } else {
    qry <- OlsSearch(q = search, exact = exact, ontology = ontology, rows = num_results)
  }
  
  qry <- olsSearch(qry)
  qdf <- as(qry, "data.frame")
  ids <- unique(qdf$obo_id)
  return(ids)
}

#' Find base terms for a single attribute that are descendants of search results
#' 
#' @param anc_lists List of term ancestors for a single attribute
#' @param result_ids List of ids returned by search function
#'
#' @return Character vector of ontology term ids
#'
findRelated <- function(anc_lists, result_ids) {
  selected_ids <- names(which(lapply(anc_lists, function(x) any(result_ids %in% x)) == TRUE))
  return(selected_ids)
}

#' Filter metadata table by ontology term ids in specific attribute
#' 
#' @param metadata Metadata table
#' @param feature String; Column name to filter by
#' @param ids List of ids to filter by
#' 
#' @return Metadata table filtered by provided ontology term ids in provided attribute
#' 
filterMetadata <- function(metadata, feature, ids) {
  meta_ids <- strsplit(metadata[,feature], ";")
  all_selected_ids <- lapply(meta_ids, function(x) intersect(x, ids))
  #selected_ids <- all_selected_ids[which(lengths(all_selected_ids) != 0)]
  selected_meta <- metadata[which(lengths(all_selected_ids) != 0),]
  return(selected_meta)
}

#' Wrapper to save ancestor info for all curated features within a metadata database (cMD/cBioPortalData)
#' 
#' @importFrom tidyverse select all_of bind_rows separate_longer_delim distinct
#' 
#' @param map_directory Path to directory containing .csv map files
#' @param target_file Path to file to save info to
#' @param map_file_pattern Regular expression to detect map files
#' @param id_column_name Name of ontology term id column; Must be the same across all maps; Defaults to "curated_ontology_term_id"
#' @param db_column_name Name of ontology name column; Must be the same across all maps; Defaults to "curated_ontology_term_db"
#'  
saveInfo <- function(map_directory, target_file, map_file_pattern,
                     id_column_name = "curated_ontology_term_id",
                     db_column_name = "curated_ontology_term_db") {
  # retrieve maps from directory
  all_maps <- getMaps(map_directory, map_file_pattern)
  
  # transform maps; should be unnecessary after maps are fixed
  #all_ids <- lapply(all_maps, function(x) unique(unlist(strsplit(x[, id_column_name], split = ";"))))
  
  # pull unique terms and dbs from all maps
  all_terms <- lapply(all_maps, function(x) select(x, all_of(c(id_column_name, db_column_name)))) %>%
    bind_rows() %>%
    separate_longer_delim(all_of(c(id_column_name, db_column_name)), delim = ";") %>% # should be able to be removed, could keep in just in case
    distinct()
  
  # get ancestors
  all_ancs <- getAncestors(all_terms[, id_column_name], all_terms[, db_column_name])
  
  # save ancestors to file
  comp_ancs <- lapply(all_ancs, function(x) paste(x, collapse = ";"))
  anc_frame <- data.frame(terms = names(comp_ancs),
                          ancestors = unname(unlist(comp_ancs)))
  write.csv(anc_frame, target_file, row.names = FALSE)
}

#' Wrapper to easily search metadata with plain text
#' 
#' @param search Character search string
#' @param metadata Metadata table
#' @param anc_file Character string indicating ancestor file to access; Either "cMD" or "cBioPortalData"
#' @param feature String; Column name to filter by
#' @param exact Optional Boolean; Defaults to FALSE; Whether to restrict the OLS search to exact matches
#' @param onto Optional character vector of ontology names to filter OLS search results; Defaults to NA
#' @param num_results Optional number of results to accept from the OLS search; Defaults to 20
#' 
#' @return Metadata table filtered by provided ontology term ids in provided attribute
#' 
searchMetadata <- function(search, metadata, feature, exact = FALSE, onto = NA, num_results = 20) {
  # get ontology search results
  anc_terms <- ontoSearch(search, exact = exact, ontology = onto, num_results = num_results)
  
  # get stored info on selected feature
  dir <- system.file("extdata", package = "OmicsMLRepoR")
  f_name <- paste0(anc_file, "_ancestors.csv")
  feature_info <- readLines(file.path(dir, "cMD_ancestors.csv"))
  anc_list <- lapply(feature_info$ancestors, function(x) unlist(strsplit(x, split = ";")))
  names(anc_list) <- feature_info$terms
  
  # get feature terms that are descendants of search results
  terms_to_find <- findRelated(feature_info, anc_terms)
  
  # filter metadata table
  filtered_meta <- filterMetadata(metadata, feature, terms_to_find)
  return(filtered_meta)
}