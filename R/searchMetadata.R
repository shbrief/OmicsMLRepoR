## Functions to search metadata using a user-provided query term

#' Load ontology mapping files
#' 
#' @param map_directory A path to the directory containing .csv map files
#' @return A list of maps as data frames
#' 
getMaps <- function(map_directory) {
    files <- list.files(map_directory)
    fpaths <- file.path(map_directory, files[grep("_map.csv", files)])
    maps <- lapply(fpaths, read.csv)
    return(maps)
}

#' Retrieve and save ancestors of a given ontology term
#'
#' @param ids A character vector of ontology term ids
#' @param dbs A character vector of corresponding ontologies, with the same
#' length of `ids`. Single string is
#' also accepted if all terms are from a single ontology.
#' 
#' @return A named list of character vectors containing the ancestors of 
#' the original term. Name of each element is the original term the ancestors
#' are traced from.
#' 
#' @examples
#' ids <- c("EFO:0005856", "NCIT:C122177", "HP:0001541", 
#'          "NCIT:C28397", "EFO:0003914", "EFO:0003780")
#' dbs <- c("EFO", "NCIT", "HP", "NCIT", "EFO", "EFO")
#' getAncestors(ids, dbs)
#' 
#' 
getAncestors <- function(ids, dbs) {
    
    map <- data.frame(id = ids, db = dbs)
    tryCatch({
        onto_frames <- split(map, map$db)
        onto_terms <- lapply(onto_frames, function(x) x$id)
        onto_nodes <- mapply(function(n, t) getNodes(n, t), 
                             names(onto_terms), onto_terms, 
                             SIMPLIFY = FALSE)
        onames <- unlist(lapply(onto_nodes, function(x) names(x)), 
                         use.names = FALSE)
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
#' @importFrom methods as
#' 
#' @param term A character (1) containing the search query, either a term or
#' ontology term id.
#' @param ontology A character (1) defining the ontology to be queried. 
#' Default is the empty character, to search all ontologies.
#' @param exact A logical (1) defining if OLS search is restricted to exact
#' matches. Defaults is `FALSE`. 
#' @param rows An integer (1) defining the number of query returns. 
#' Default is 20L. Maximum number of values returned by the server is 1000.
#' 
#' @return A character vector of matched ontology term ids
#' 
#' @examples
#' ontoSearch("plasma,membrane", ontology = "go")
#' ontoSearch("NCIT:C4872", rows = 5)
#' ontoSearch("Skin Infection", rows = 5)
#' 
ontoSearch <- function(term, ontology = "", exact = FALSE, rows = 20) {
    qry <- OlsSearch(q = term, 
                     ontology = ontology, 
                     exact = exact, 
                     rows = rows)
    qry <- olsSearch(qry)
    qdf <- as(qry, "data.frame")
    ids <- unique(qdf$obo_id)
    return(ids)
}

#' Find base terms that are equivalent to or descendants of search results
#' 
#' @param pool A named list of all the ancestor terms for a given ontology 
#' term. The name of the element is the queried term for ancestor search. 
#' @param target A character vector of ontology ids.
#'
#' @return A character vector of ontology term ids having any of `target` terms
#' as their ancestor.
#'
#' @examples
#' ancestors <- list("EFO:0000228" = c("EFO:0000228", "EFO:0000616"),
#'                   "NCIT:C2855" = c("NCIT:C2855", "NCIT:C3262"))
#' results <- c("NCIT:C3262", "EFO:0000616")
#' findRelated(pool = ancestors, target = results)
#' 
findRelated <- function(pool, target) {
    match <- lapply(pool, 
                    function(x) length(intersect(x, target)) != 0) %>% unlist
    res <- names(pool)[which(match)]
    return(res)
}

#' Collect both terms and ids related to the query
.getOntoInfo <- function(query, return = NULL) {
    res <- c()
    for (i in seq_along(query)) {
        qry <- OlsSearch(q = query[i], exact = TRUE)
        qry <- olsSearch(qry)
        qdrf <- as(qry, "data.frame")
        
        obo_ids <- unique(qdrf$obo_id)
        label <- unique(qdrf$label)
        res <- c(res, obo_ids, label)
    }
    return(res)
}
    
#' Filter metadata table by ontology term ids in specific attribute
#' 
#' @param metaTb Metadata table
#' @param feature A character vector (1). Column name to filter by
#' @param ids List of ids to filter by
#' 
#' @return Metadata table filtered by provided ontology term ids in 
#' provided attribute
#' 
#' @examples
#' dir <- system.file("extdata", package="OmicsMLRepoR")
#' df <- read.csv(file.path(dir, "sample_metadata.csv"))
#' filterMetadata(df,
#'                "curated_disease",
#'                c("NCIT:C2855", "EFO:0000228"))
#'                
filterMetadata <- function(metaTb, feature, ids) {
    
    meta_ids <- strsplit(metaTb[,feature], ";")
    targets <- .getOntoInfo(ids) # Both terms and ids 
    all_selected_ids <- lapply(meta_ids, function(x) intersect(x, targets))
    selected_meta <- metaTb[which(lengths(all_selected_ids) != 0),]
    return(selected_meta)
}

#' Wrapper to save ancestor info for all curated features within a metadata 
#' database. Currently, we have established curated features for 
#' curatedMetagenomicData (cMD) and cBioPortalData.
#' 
#' @importFrom dplyr select all_of bind_rows distinct
#' @importFrom tidyr separate_longer_delim 
#' 
#' @param curatedMetaTb A curated metadata table. Curated ontology terms should
#' be under the column name satisfying the patten, `^.*_ontology_term_id$`
#' @param saveAs A character (1). The file path for the returned list.
#'  
saveAncestors <- function(curatedMetaTb, saveAs) {
    
    ## Extract ontology terms incorporated into the curated metadata
    idColInds <- grep("_ontology_term_id", colnames(curatedMetaTb))
    idCols <- colnames(curatedMetaTb)[idColInds]
    
    ## All the unique ontology terms used for curated attributes
    ## A named list, where the name of the element is the target attribute
    allIds <- apply(curatedMetaTb[idCols], 2, 
                    function(x) strsplit(x, split = ";") %>%
                        unlist %>% na.omit %>% unique)
    names(allIds) <- gsub("curated_|_ontology_term_id", "", names(allIds))
    
    ## Create a `tb` with three columns:
    ## `attributes` (curated column name), `ontology_term_id`, and `ontology_term_db`
    tb <- stack(allIds)
    colnames(tb) <- c("ontology_term_id", "attributes")
    tb$ontology_term_db <- get_ontologies(tb$ontology_term_id)
    
    ## Get all the ancestors for each ontology term
    allAncestors <- getAncestors(tb[,"ontology_term_id"],
                                 tb[,"ontology_term_db"])
    
    ## Collapse all the ancestor terms
    compact <- lapply(allAncestors, function(x) paste(x, collapse = ";"))
    ancestorTb <- stack(compact)
    colnames(ancestorTb) <- c("ancestors", "ontology_term_id")
    
    ## Final table with three columns:
    ## `attributes` (curated column name), `ontology_term_id`, and `ancestors`
    res <- dplyr::full_join(tb[c("attributes", "ontology_term_id")], 
                            ancestorTb, 
                            by = "ontology_term_id")

    write.csv(res, saveAs, row.names = FALSE)
}

#' Wrapper to easily search metadata with plain text
#' 
#' @param term A character (1) containing the search query.
#' @param metaTb Metadata table
#' @param targetDB A character (1) indicating ancestor file to access. 
#' Available options are `cMD` (curatedMetagenomicData) and `cBioPortalData`.
#' @param feature A character vector (1). Column name to filter by. Under the
#' default (`NULL`), all the columns using ontology terms are browsed.
#' @param exact A logical (1) defining if OLS search is restricted to exact
#' matches. Defaults is `FALSE`. 
#' @param onto A character (1) defining the ontology to be queried for a 
#' provided `term`. Default is the empty character, to search all ontologies.
#' @param rows An integer (1) defining the number of query returns. 
#' Default is 20L. Maximum number of values returned by the server is 1000.
#' 
#' @return Subset of the input metadata table (`metaTb`) filtered by provided 
#' ontology term ids (`onto`) under the feature attribute (`feature`). 
#' 
#' @examples
#' dir <- system.file("extdata", package = "OmicsMLRepoR")
#' metaTb <- read.csv(file.path(dir, "cMD_curated_metadata_all.csv"), header = TRUE)
#' ontoBrowse(term = "cancer", 
#'            metaTb = metaTb, 
#'            targetDB = "cMD", 
#'            feature = "curated_disease")
#'            
#' metaTb <- read.csv(file.path(dir, "cMD_curated_metadata_all.csv"), header = TRUE)
#' ontoBrowse(term = "cancer", 
#'            metaTb = metaTb, 
#'            targetDB = "cMD", 
#'            feature = "curated_disease")          
#' 
#' @export
ontoBrowse <- function(term, 
                       metaTb, #<<<<<<<<<<<<<<<<<<<< Potentially collapse with `targetDB` argument
                       targetDB,
                       feature = NULL, # <<<<<<<<<<<<<<<<<<<< Sanity check that feature exist in metaTb & Do we need to specify this?
                       exact = FALSE, 
                       onto = "", 
                       rows = 20) {
    
    ## Sanity check
    if (!is.null(feature)) {
        if (!feature %in% colnames(metaTb)) {
            msg <- paste0("The feature column, '", feature,
                          "', does not exist in the metadata")
            stop(msg)
        }
    }
        
    ## Get ontology terms related to the queried `term`
    anc_terms <- ontoSearch(term, 
                            exact = exact, 
                            ontology = onto, 
                            rows = rows)
    
    ## Load all the ancestor terms for ontology terms in the database
    dir <- system.file("extdata", package = "OmicsMLRepoR")
    fname <- paste0(targetDB, "_ancestors.csv")
    allAncestors <- read.csv(file.path(dir, fname), header = TRUE)
    unlistedAncestors <- lapply(allAncestors$ancestors, 
                                function(x) unlist(strsplit(x, split = ";")))
    names(unlistedAncestors) <- allAncestors$ontology_term_id

    ## Find the feature terms that are descendants of search results
    terms_to_find <- findRelated(unlistedAncestors, anc_terms)
    
    ## Check the curated attribute(s) containing the feature terms
    tb <- allAncestors %>% filter(ontology_term_id %in% terms_to_find)
    targetFeatures <- unique(tb$attributes) %>% 
        paste0("curated_", .)  #<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< REMOVE for release

    ## Term not found in the specified column (i.e., feature)
    if (!is.null(feature)) {
        if (!feature %in% targetFeatures) {
            msg <- paste(paste0("The requested term, ", term, ","), 
                         "is not found in the feature,", feature)
            stop(msg)
        }
    }    
    
    ## Filter metadata table
    if (is.null(feature) & length(targetFeatures) != 1) {
        ## If the input metadata table is the subset of the released 
        ## version of the metadata
        targetFeatures <- intersect(colnames(metaTb), targetFeatures)
        
        ## If feature is not specified and term is found in multiple columns
        filtered_meta <- lapply(targetFeatures, 
                                function(x) {filterMetadata(metaTb, x, terms_to_find)})
        curation_ids <- lapply(filtered_meta, function(x) {x$curation_id}) %>%
            unlist %>% unique
        res <- metaTb %>% filter(curation_id %in% curation_ids)
    } else {
        res <- filterMetadata(metaTb, feature, terms_to_find)
    }
    
    return(res)
}
