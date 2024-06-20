#' Query Ontology Lookup Service (OLS)
#' 
#' Extract identical or similar ontology terms across different ontologies
#'
#' @importFrom methods as
#' @importFrom dplyr filter
#' @importFrom tibble as_tibble
#'
#' @param query A character (1) containing the search query, either a term
#' label or term id.
#' @param ontology A character vector defining the ontology to be queried. 
#' Default is the empty character, to search all ontologies.
#' @param exact A logical (1) defining if OLS search is restricted to exact
#' matches. Defaults is `FALSE`. 
#' @param rows An integer (1) defining the number of query returns. 
#' Default is 20L. Maximum number of values returned by the server is 1000.
#' 
#' @return A tibble containing ontology term label and description
#'
#' @examples
#' getOntoInfo("NCIT:C4872")
#' getOntoInfo("NCIT:C4872", ontology = c("EFO", "MONDO"))
#' getOntoInfo("Skin Infection")
#' getOntoInfo("Sitagliptin", ontology = c("Chebi", "apple"))
#' 
#' ## Multiple query values
#' getOntoInfo("plasma,membrane")
#' getOntoInfo(c("plasma", "membrane"))
#'
#' @export
getOntoInfo <- function(query, 
                        ontology = "",
                        exact = FALSE,
                        rows = 20) {
    qry <- OlsSearch(q = query, ontology = ontology, rows = rows, exact = exact)
    qry <- olsSearch(qry)
    qdrf <- as(qry, "data.frame")
    qdrf <- tibble::as_tibble(qdrf)
    qdrf$description <- as.character(qdrf$description)
    return(qdrf)
}
