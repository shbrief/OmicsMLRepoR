#' Collect both labels and obo_ids related to the query term
#' 
#' This function can take multiple obo_id and labels, and return both 
#' labels/obo_ids exactly matching with the `query`.
#' 
#' @param query A character vector of terms or term ids. For example, 
#' `c("NCIT:C35025", "HP:0003003", "colon cancer")`.
#' 
#' @return A character vector of all the related terms' label and obo_id.
#' 
.getAllTargetForms <- function(query) {
    
    resAll <- lapply(query, getOntoInfo) %>%
        bind_rows(.id = colnames(.))
    res <- unique(c(resAll$label, resAll$obo_id))
    return(res)
}


#' Find distant relatives
#' 
#' This function takes a list of ontology terms and their ancestors, where
#' the lowest descent serves as a name of the element, as a `pool`. Names 
#' of all the elements that have the `target` term in their ancestor list 
#' (i.e., sharing the ancestors) are returned. 
#' 
#' This functionality is the base of searching metadata leveraging ontology, 
#' because a specific term can be used to query all the related, decendant
#' terms used in the metadata.
#' 
#' @param pool A named list of the ancestors for a given ontology term. The 
#' given ontology term is also included in the list and serves as a name of
#' the element.
#' @param target A character vector of ontology ids.
#'
#' @return A character vector of ontology term ids having any of `target` 
#' terms as their ancestor.
#'
#' @examples
#' ancestors <- list(
#'     "NCIT:C5490"=c("NCIT:C5490","NCIT:C4910","NCIT:C2955","NCIT:C4978"),
#'     "NCIT:C177680"=c("NCIT:C177680","NCIT:C4910","NCIT:C2955","NCIT:C4978"),
#'     "NCIT:C2955"=c("NCIT:C2955","NCIT:C4978","NCIT:C3141"))
#' targets <- "NCIT:C4910"
#' findDistantRelatives(pool = ancestors, target = targets)
#' 
#' @export
findDistantRelatives <- function(pool, target) {
    match <- lapply(pool, 
                    function(x) length(intersect(x, target)) != 0) %>% unlist
    res <- names(pool)[which(match)] %>% unique
    return(res)
}

    
#' Filter metadata table by ontology term ids 
#' 
#' @param metaTb Metadata table
#' @param query A character vector containing obo_ids to query
#' @param feature A character (1). Column name to filter by.
#' @param delim A character(1) used to separate multiple values. Default `<;>`.
#' 
#' @return Metadata table filtered by provided ontology term ids in 
#' provided attribute
#' 
#' @examples
#' dir <- system.file("extdata", package="OmicsMLRepoR")
#' df <- read.csv(file.path(dir, "sample_metadata.csv"))
#' filterMetadata(df, c("NCIT:C2855", "EFO:0000228"), "curated_disease")
#' filterMetadata(df, c("diabetes", "EFO:0000228"), "curated_disease")
#' 
#' @export
filterMetadata <- function(metaTb, query, feature, delim = "<;>") {
    
    meta_ids <- strsplit(metaTb[[feature]], delim) # for multiple values in a single cell
    targets <- .getAllTargetForms(query) # both terms and ids 
    
    selected_ids <- lapply(meta_ids, function(x) intersect(x, targets))
    selected_meta <- metaTb[which(lengths(selected_ids) != 0),]
    return(selected_meta)
}



#' Wrapper to easily search metadata with plain text
#' 
#' @param term A character (1) containing the search query.
#' @param metaTb A data frame. Metadata table containing `feature` where you
#' want to search the queried `term`.
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
#' @return A data frame which is the subset of the input metadata table 
#' (`metaTb`) filtered by the provided ontology term ids (`onto`) under 
#' the feature attribute (`feature`). 
#' 
#' @examples
#' metaTb <- getMetadata("cMD")
#' searchMetadata("Digestive System Precancerous Condition", metaTb, "cMD")
#' searchMetadata("Intestinal Disorder", metaTb, "cMD", "disease")
#' 
#' @export
searchMetadata <- function(term,
                           metaTb, #<<<<<<<<<<<<<<<<<<<< Potentially collapse with `targetDB` argument
                           targetDB,
                           feature = NULL, # <<<<<<<<<<<<<< Do we need to specify this?
                           exact = FALSE,
                           onto = "",
                           rows = 20,
                           delim = "<;>") {

    ## Sanity check that all the `feature(s)` exists in the metadata table
    if (!is.null(feature)) {
        if (all(!feature %in% colnames(metaTb))) {
            msg <- "The feature column(s) doesn't exits in the metadata table"
            stop(msg)
        }
    }
        
    ## Get ontology terms related to the queried term
    anc_terms <- getOntoInfo(term, "", exact, 20) # similar
    anc_terms_exact <- getOntoInfo(term, "", TRUE, 20) # exact match
    target_terms <- unique(c(anc_terms_exact$obo_id,
                             anc_terms$obo_id))[seq_len(rows)]
    
    ## Load all the ancestor terms for ontology terms in the database
    dir <- system.file("extdata", package = "OmicsMLRepoR")
    fname <- paste0(targetDB, "_ancestors.csv")
    allAncestors <- read.csv(file.path(dir, fname), header = TRUE)
    
    unlistedAncestors <- lapply(allAncestors$ancestors, 
                                function(x) unlist(strsplit(x, split = ";")))
    names(unlistedAncestors) <- allAncestors$ontology_term_id

    ## Find the terms that are descendants of queried term itself and related
    terms_to_find <- findDistantRelatives(unlistedAncestors, anc_terms$obo_id)
    
    ## Identify the curated attribute(s) containing the queried terms
    tb <- allAncestors %>% filter(ontology_term_id %in% terms_to_find)
    targetFeatures <- unique(tb$attributes)
    
    ## Stop if there is no match
    if (!is.null(feature)) {
        ## If feature is specified, narrow down `targetFeatures` 
        targetFeatures <- intersect(feature, targetFeatures)
        
        ## Term not found in the specified column (i.e., feature)
        if (length(targetFeatures) == 0) {
            msg <- "The requested term is not found in the feature(s)."
            stop(msg)
        }
    } else if (is.null(feature)) {
        ## Term is not found in any metadata columns
        if (length(targetFeatures) == 0) {
            msg <- "The requested term is not found in the metadata table."
            stop(msg)
        }
    } 
    
    ## Filter metadata table
    if (length(targetFeatures) > 1) { # term is found in multiple features

        ## Search the query term for each metadata column/feature
        filtered_meta <- lapply(targetFeatures, function(x) {
            filterMetadata(metaTb = metaTb, 
                           query = terms_to_find, 
                           feature = x,
                           delim = delim)})
        curation_ids <- lapply(filtered_meta, function(x) {x$curation_id}) %>%
            unlist %>% unique
        res <- metaTb %>% filter(curation_id %in% curation_ids)
    } else if (length(targetFeatures) == 1) {
        res <- filterMetadata(metaTb = metaTb, 
                              query = terms_to_find, 
                              feature = targetFeatures,
                              delim = delim)
    }
    
    msg <- paste("The term identified under the", 
                 paste(targetFeatures, collapse = "/"), "column.")
    message(msg)
    
    res <- as_tibble(res)
    return(res)
}