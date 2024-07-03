# Collect both labels and obo_ids related to the query term
# 
# This function can take multiple obo_id and labels, and return both 
# labels/obo_ids exactly matching with the `query`. It subsets the output from
# `getOntoInfo` function only to the `label` and/or `obo_id`.
# 
# @param query A character vector of terms or term ids. For example, 
# `c("NCIT:C35025", "HP:0003003", "colon cancer")`.
# @param returns A character vector of returned value's format. Available
# options are `c("label", "obo_id)` (default).
# 
# 
# @return A character vector of all the related terms' label and obo_id.
# 
.getAllTargetForms <- function(query, returns = c("label", "obo_id")) {
    
    resAll <- lapply(query, getOntoInfo) %>%
        bind_rows(.id = colnames(.))
    
    if (returns == "label") {
        res <- unique(c(resAll$label))
    } else if (returns == "obo_id") {
        res <- unique(c(resAll$obo_id))
    } else if (all(returns %in% c("label", "obo_id"))) {
        res <- unique(c(resAll$label, resAll$obo_id))
    }
    
    return(res)
}


# Find distant relatives
# 
# This function takes a list of ontology terms and their ancestors, where
# the lowest descent serves as a name of the element, as a `pool`. Names 
# of all the elements that have the `target` term in their ancestor list 
# (i.e., sharing the ancestors) are returned. 
# 
# This functionality is the base of searching metadata leveraging ontology, 
# because a specific term can be used to query all the related, decendant
# terms used in the metadata.
# 
# @param pool A named list of the ancestors for a given ontology term. The 
# given ontology term is also included in the list and serves as a name of
# the element.
# @param target A character vector of ontology ids.
#'
# @return A character vector of ontology term ids having any of `target` 
# terms as their ancestor.
#'
# @examples
# ancestors <- list(
#     "NCIT:C5490"=c("NCIT:C5490","NCIT:C4910","NCIT:C2955","NCIT:C4978"),
#     "NCIT:C177680"=c("NCIT:C177680","NCIT:C4910","NCIT:C2955","NCIT:C4978"),
#     "NCIT:C2955"=c("NCIT:C2955","NCIT:C4978","NCIT:C3141"))
# targets <- "NCIT:C4910"
# .findDistantRelatives(pool = ancestors, target = targets)
# 
.findDistantRelatives <- function(pool, target) {
    match <- lapply(pool, 
                    function(x) length(intersect(x, target)) != 0) %>% unlist
    res <- names(pool)[which(match)] %>% unique
    return(res)
}


# Keep rows that include the queried terms and identical/similar to them 
# 
# Similar to \code{\link[dplyr]{filter}} function, while its filtering 
# includes ontology terms and ids identical or similar to the query term 
# across different ontologies collected through OLS search.
# 
# @importFrom rlang enquo
# @importFrom dplyr filter
# 
# @param .data A data frame
# @param col A character (1). Column name to filter by.
# @param query A character vector containing words or ids to be used in the
# ontology search
# @param delim A character (1) used to separate multiple values. 
# 
# @return Data frame filtered by provided queries along with their ontology
# synonyms/ids in the specified column. Not case-sensitive.
# 
# @examples
# dir <- system.file("extdata", package="OmicsMLRepoR")
# df <- read.csv(file.path(dir, "sample_metadata.csv"))
# onto_filter(df, curated_disease, c("diabetes", "adenoma"))
# 
onto_filter <- function(.data, col, query, delim = NULL) {
    
    ## Search OLS
    targets <- tolower(c(query, .getAllTargetForms(query)))
    
    ## Filter data
    .data %>%
        rowwise() %>%
        dplyr::filter(any(tolower(unlist(strsplit(!!enquo(col), split = delim)))
                          %in% targets))
}


#' Keep rows that include the queried terms and their descendants  
#' 
#' Similar to \code{\link[dplyr]{filter}} function, while its filtering 
#' includes descendants and synonyms of the query term in addition to ontology 
#' terms and ids identical or similar to the query term across different 
#' ontologies collected through OLS search. 
#' 
#' @importFrom rlang enquo as_name sym
#' 
#' @param .data A data frame
#' @param col A character (1). Column name to filter by.
#' @param query A character vector containing words or ids to be used in the
#' ontology search
#' @param delim A character (1) used to separate multiple values. If your
#' `.data` input is obtained from \code{getMetadata} function, this input is
#' automatically configured.
#' 
#' @return Data frame filtered by provided queries along with child terms in the
#' specified column
#' 
#' @examples
#' meta <- getMetadata("cMD")
#' tree_filter(meta, disease, c("pancreatic disease", "cancer"))
#' 
#' @export
tree_filter <- function(.data, col, query, delim = NULL) {
    
    ## Check that curated feature is present
    feat_name <- as_name(enquo(col))
    id_col <- paste0(feat_name, "_ontology_term_id")
    
    if (!feat_name %in% colnames(.data)) {
        msg <- "The selected column does not exist in the metadata table"
        stop(msg)
    } else if (!id_col %in% colnames(.data)) {
        msg <- "The selected column was not curated with ontology terms"
        stop(msg)
    }
    
    ## Get delimiter
    targetDB <- .getTargetDB(.data)
    delim <- .getDelimiter(.data, feat_name, delim) 
      
    ## Search OLS
    targets <- c(query, .getAllTargetForms(query, "obo_id"))

    ## Load ancestors for the appropriate database
    dir <- system.file("extdata", package = "OmicsMLRepoR")
    fname <- paste0(targetDB, "_ancestors.csv")
    allAncestors <- read.csv(file.path(dir, fname), header = TRUE)
    
    unlistedAncestors <- lapply(allAncestors$ancestors, 
                                function(x) unlist(strsplit(x, split = ";")))
    names(unlistedAncestors) <- allAncestors$ontology_term_id
    
    ## Retrieve ids to filter by
    related_terms <- .findDistantRelatives(unlistedAncestors, targets)
    terms_to_find <- unique(c(related_terms, targets))
    
    ## Filter data
    .data %>%
        rowwise() %>%
        filter(any(unlist(strsplit(!!sym(id_col), split = delim)) %in%
                       terms_to_find))
}
