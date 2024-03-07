#' Look up the description of ontology terms
#'
#' @param terms A character vector containing ontology term ids to look up
#' their definition. Each ontology term id should be their official format
#' with their ontology symbol and term number separated by colon.
#' 
#' @examples
#' terms <- ontoTraverse(source = "hp", termId = "HP:0003003", plot = FALSE)
#' getOntoDesc(terms)
#' 
#' @export
getOntoDesc <- function(terms) {
    
    res_all <- matrix("", nrow = 0, ncol = 3)
    colnames(res_all) <- c("obo_id", "label", "description")
    
    for (term in terms) {
        
        # Extract ontology from the ontology term id
        ontoDB <- strsplit(term, ":")[[1]][1]
        isSNOMED <- letters_only(ontoDB)
        if (isFALSE(isSNOMED)) {ontoDB <- "SNOMED"}
        
        res <- ontoBrowse(term, ontoDB = ontoDB) #<<<<<<<<< Can we load ontology only once if multiple terms are from a same ontology?
        res_all <- rbind(res_all, res)
    }
    return(res_all)
}