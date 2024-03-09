#' Extract both ancestors and descendants of an ontology term 
#' 
#' @import rols
#' 
#' @param term Ontology term id ('obo_id'), such as \code{NCIT:C15280}. 
#' @param returnDescription Default is `FALSE`. If it is set to `TRUE`, the
#' returned value includes description of the term and other information.
#' 
#' @return A character vector of ancestors, self, and descendants ontology
#' term ids. If `returnDescription = TRUE`, it returns a tibble containig
#' details (including description) of the related ontology term ids.
#' 
#' @examples
#' ontoTraverse(term = "NCIT:C15280")
#' ontoTraverse(term = "CHEBI:5262")
#' ontoTraverse(term = "HP:0011793")
#' 
#' @export
ontoTraverse <- function(term, returnDescription = FALSE) {
    
    ## Load ontology
    ol <- Ontologies()
    source <- get_ontologies(term) %>% tolower()
    ontology <- ol[[source]]
    trm <- Term(ontology, term)
    
    ## Get the full tree of direct ancestors/descendants of `term` input
    all_terms <- c(termId(trm)) # self
    if (length(parents(trm)) != 0) { # instead of `is.na` to silence the warning
        parent_terms <- termId(parents(trm))
        all_terms <- c(all_terms, names(parent_terms))} 
    if (length(children(trm)) != 0) {
        children_terms <- termId(children(trm))
        all_terms <- c(all_terms, names(children_terms))}
    
    ## Include definition
    if (isTRUE(returnDescription)) {
        res <- getOntoInfo(all_terms)
    } else {res <- all_terms}
    
    return(res)
}

#' Plot ontology tree
#' 
#' @import rols
#' @import dplyr
#' @importFrom data.tree FromDataFrameNetwork
#' @importFrom jsonlite fromJSON
#' 
#' @example ontoTreePlot("NCIT:C2852")
#' 
ontoTreePlot <- function(term) {
    
    sample_id <- term
    sample_db <- get_ontologies(term)
    
    ## load term object and retrieve link to JSON tree
    ontob <- Ontology(sample_db)
    cur_trm <- Term(ontob, sample_id)
    jstree <- cur_trm@links$jstree$href
    
    ## transform JSON into dataframe
    tree_frame <- jsonlite::fromJSON(jstree)
    
    ## transform dataframe into usable edgelist
    map <- tree_frame %>%
        rowwise() %>%
        mutate(term = unlist(strsplit(iri, split = "/"))[5]) %>%
        select(id, term)
    
    edgelist <- tree_frame %>%
        select(parent, id) %>%
        rename(from = parent,
               to = id) %>%
        filter(from != "#") %>%
        mutate(from = plyr::mapvalues(from, map$id, map$term, warn_missing = FALSE)) %>%
        mutate(to = plyr::mapvalues(to, map$id, map$term, warn_missing = FALSE))
    
    ## plot tree with data.tree package; other packages will also work with the edgelist
    tree <- data.tree::FromDataFrameNetwork(edgelist)
    plot(tree)
}