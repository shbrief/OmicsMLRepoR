#' Analog of Tidyverse 'filter' function that uses ontology search results
#' 
#' @importFrom rlang enquo
#' 
#' @param .data A data frame
#' @param col A character (1). Column name to filter by.
#' @param query A character vector containing words or ids to be used in the
#' ontology search
#' @param delim A character (1) used to separate multiple values. Default ';'.
#' 
#' @return Data frame filtered by provided queries along with their ontology
#' synonyms/ids in the specified column
#' 
#' @examples
#' dir <- system.file("extdata", package="OmicsMLRepoR")
#' df <- read.csv(file.path(dir, "sample_metadata.csv"))
#' onto_filter(df, curated_disease, c("diabetes", "adenoma"))
#' 
onto_filter <- function(.data, col, query, delim = ";") {
    ## Search OLS
    targets <- tolower(c(query, .getAllTargetForms(query)))
    
    ## Filter data
    .data %>%
        rowwise() %>%
        filter(any(tolower(unlist(strsplit(!!enquo(col), split = delim))) %in% targets))
}

#' Analog of Tidyverse 'filter' function that uses ontology search results and
#' saved ontology ancestors
#' 
#' @importFrom rlang enquo as_name sym
#' 
#' @param .data A data frame
#' @param col A character (1). Column name to filter by.
#' @param query A character vector containing words or ids to be used in the
#' ontology search
#' @param db A character (1) indicating ancestor file to access. Available
#' options are 'cMD' (curatedMetagenomicData) and 'cBioPortalData'.
#' @param delim A character (1) used to separate multiple values. Default ';'.
#' 
#' @return Data frame filtered by provided queries along with child terms in the
#' specified column
#' 
#' @examples
#' dir <- system.file("extdata", package="OmicsMLRepoR")
#' df <- read.csv(file.path(dir, "sample_metadata.csv"))
#' curated_filter(df, curated_disease, c("pancreatic disease", "cancer"), "cMD")

curated_filter <- function(.data, col, query, db, delim = ";") {
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
    
    ## Search OLS
    resAll <- lapply(query, getOntoInfo) %>%
        bind_rows(.id = colnames(.))
    res_ids <- unique(resAll$obo_id)
    
    ## Load ancestors for the appropriate database
    dir <- system.file("extdata", package = "OmicsMLRepoR")
    fname <- paste0(db, "_ancestors.csv")
    allAncestors <- read.csv(file.path(dir, fname), header = TRUE)
    
    unlistedAncestors <- lapply(allAncestors$ancestors, 
                                function(x) unlist(strsplit(x, split = ";")))
    names(unlistedAncestors) <- allAncestors$ontology_term_id
    
    ## Retrieve ids to filter by
    related_terms <- findDistantRelatives(unlistedAncestors, res_ids)
    terms_to_find <- unique(c(related_terms, res_ids))
    
    ## Filter data
    .data %>%
        rowwise() %>%
        filter(any(unlist(strsplit(!!sym(id_col), split = delim)) %in% terms_to_find))
}