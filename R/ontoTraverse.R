#' Extract both ancestors and descendants of an ontology term 
#' 
#' @import rols
#' 
#' @param term Ontology term id ('obo_id'), such as \code{NCIT:C15280}. 
#' @param returnDescription Default is `FALSE`. If it is set to `TRUE`, the
#' returned value includes description of the term and other information.
#' 
#' @return A character vector of ancestors, self, and descendants ontology
#' term ids. If `returnDescription = TRUE`, it returns a tibble containing
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
        res <- lapply(all_terms, function(x) {
            getOntoInfo(x, ontology = get_ontologies(x), exact = TRUE)
        }) %>% Reduce(rbind, .)
    } else {
        res <- all_terms
    }
    
    return(res)
}
