## Create a collection of all the ancestors of ontology terms used in the
## curated/harmonized metadata


#' Retrieve and save ancestors of a given ontology term
#'
#' @param ids A character vector of ontology term ids
#' @param dbs A character vector of corresponding ontologies, with the same
#' length of `ids`. Single string is also accepted if all terms are from a 
#' single ontology.
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
getAncestors <- function(ids, dbs) {
    
    map <- data.frame(id = ids, db = dbs)
    tryCatch({
        onto_frames <- split(map, map$db)
        onto_terms <- lapply(onto_frames, function(x) x$id)
        onto_nodes <- mapply(function(n, t) getNodes(n, t), 
                             names(onto_terms), 
                             onto_terms, 
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

#' Wrapper to save ancestor info for all curated features within a metadata 
#' database. Currently, we have established curated features for 
#' curatedMetagenomicData (cMD) and cBioPortalData.
#' 
#' @importFrom dplyr select all_of bind_rows distinct
#' @importFrom tidyr separate_longer_delim 
#' 
#' @param curatedMetaTb A curated metadata table. Curated ontology terms should
#' be under the column name satisfying the pattern, `^.*_ontology_term_id$`
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