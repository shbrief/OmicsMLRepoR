# temporary library loading
library(rols)
library(tidyverse)

# Temporary variables
data_repo_dir <- "/home/kaelyn/Desktop/Work/OmicsMLRepoData"

cMD_mapdir <- paste0(data_repo_dir, "/curatedMetagenomicData/maps")
cBioPortalData_mapdir <- paste0(data_repo_dir, "/cBioPortalData/maps")

## Functions to retrieve used terms from raw user input

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

#' Get ontology database from ontology term id
#' 
#' @param ids Character vector of term ids
#' 
#' @return Character vector of corresponding ontology names
#' 
getDBs <- function(ids) {
  
}

#' Retrieve and save ancestors of ontology terms
#'
#' @importFrom OmicsMLRepoR getNodes
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
#' @param search search string
#' @param ontology optional character vector of ontology names
#' @param num_results optional number of results to return; default = 20
#' 
#' @return Character vector of ontology term ids
#' 
ontoSearch <- function(search, ontology = NA, num_results = 20) {
  if (is.na(ontology)) {
    qry <- OlsSearch(q = search, rows = num_results)
  } else {
    qry <- OlsSearch(q = search, ontology = ontology, rows = num_results)
  }
  
  qry <- olsSearch(qry)
  qdf <- as(qry, "data.frame")
  ids <- unique(qdf$obo_id)
  return(ids)
}

#' Find base terms for a single attribute that are descendants of search results
#' 
#' @param anc_lists list of term ancestors for a single attribute
#' @param result_ids list of ids returned by search function
#'
#' @return Character vector of ontology term ids
#'
findRelated <- function(anc_lists, result_ids) {
  selected_ids <- names(which(lapply(anc_lists, function(x) any(result_ids %in% x)) == TRUE))
  return(selected_ids)
}

#' Filter metadata table by ontology term ids in specific attribute
#' 
#' @param metadata metadata table
#' @param feature column name
#' @param ids list of ids to filter by
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
#' @param map_directory Path to directory containing .csv map files
#' @param map_file_pattern Regular expression to detect map files; Must contain a capturing group to retrieve feature names from map filenames
#' @param id_column_name Name of ontology term id column; Must be the same across all maps
#' @param target_directory Path to directory to save info to
#'  
saveInfo <- function(map_directory, map_file_pattern, id_column_name, target_directory) {
  # retrieve maps from directory
  all_maps <- getMaps(map_directory, map_file_pattern)
  
  # transform maps
  all_ids <- lapply(all_maps, function(x) unique(unlist(strsplit(x[, id_column_name], split = ";"))))
  
  # get ancestors for each map
  all_ancs <- lapply(all_maps, function())
}

#' Wrapper to easily search metadata with plain text
#' 
#' @param search search string
#' @param metadata metadata table
#' @param feature column name
#' 
#' @return Metadata table filtered by provided ontology term ids in provided attribute
#' 
searchMetadata <- function(search, metadata, feature) {
  # get ontology search results
  anc_terms <- ontoSearch(search)
  
  # get stored info on selected feature
  feature_info <- NA # location of .rds for feature
  
  # get feature terms that are descendants of search results
  terms_to_find <- findRelated(feature_info, anc_terms)
  
  # filter metadata table
  filtered_meta <- filterMetadata(metadata, feature, terms_to_find)
  return(filtered_meta)
}


#' save ancestors for features
#'
#' @param anc_list list of ancestors named by original term
#' @param feature feature name
#' 
saveAncs <- function(anc_list, feature) {
  
}
