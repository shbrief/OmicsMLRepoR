# Extract similar or identical ontology terms from different ontology databases
# 
# @importFrom methods as
# @importFrom dplyr filter
# @importFrom tibble as_tibble
# 
# @param term Character(1). The ontology term or term id to look up. 
# @param rows Integer(1). The number of rows to be returned.
# 
# @examples
# .getOntoInfo("NCIT:C4872", 5)
# .getOntoInfo("Skin Infection", 5)
# 
.getOntoInfo <- function(term, rows) {
    qry <- OlsSearch(q = toupper(term), rows = rows)
    qry <- olsSearch(qry)
    qdrf <- as(qry, "data.frame")
    qdrf <- qdrf[!duplicated(qdrf$obo_id) | is.na(qdrf$obo_id),] # remove multiplicates
    qdrf <- tibble::as_tibble(qdrf)
    return(qdrf)
}

# UNDER CONSTRUCTION
.getOntoDB <- function(termId, db, id) {
    x <- strsplit(termId, split = "_|:")
    db <- unlist(x)[1]
    id <- unlist(x)[2]

    ol <- Ontologies()
    if (!tolower(db) %in% olsNamespace(ol)) {
        stop("The requested ontology database isn't available.")
    }
    targetdb <- Ontology(db)
    # allterms <- terms(targetdb) # Time consuming step. Save the snapshot for common ontology databases
    term <- term(targetdb, paste(toupper(db), id, sep = ":"))
}



#' Browse ontology databases from ontology term or term id
#' 
#' @import rols
#'
#' @param term Character(1). The ontolgy term or term id you want 
#' to check its definition.
#' @param ontoDB A character vector containing the ontology databases you
#' want to look up the \code{term}. The other ontology databases you want to
#' look for the same/similar term as the input \code{term}.
#' @param rows Integer(1).The maximum number of terms to look up. Default is 10.
#' 
#' @return Data
#'
#' @examples
#' ontoBrowse("Sitagliptin", ontoDB = "Chebi")
#' ontoBrowse("Sitagliptin", ontoDB = c("Chebi", "apple"))
#' ontoBrowse("CHEBI:40237")
#'
#' @export
ontoBrowse <- function(term,
                       ontoDB = NULL,
                       rows = 10) {

    ontoInfo <- .getOntoInfo(term = term, rows = rows)

    if (is.null(ontoDB)) {
        res <- ontoInfo[, c("obo_id", "label", "description")]
    } else {
        present <- match(tolower(ontoDB), ontoInfo$ontology_name)
        if (all(is.na(present))) {
            # If the term is not available in all of the requested ontologies
            stop("The term is not available in queried ontologies")
        } else if (any(is.na(present))) {
            # If the term is not available in some of the requested ontologies
            na_ind <- which(is.na(present))
            na_db <- ontoDB[na_ind]
            ontoDB <- ontoDB[-na_ind] # remove un-available ontology DB
            msg <- paste("The following ontologies are not available:", na_db)
            message(msg)
        }
        ind <- which(ontoInfo$ontology_name %in% tolower(ontoDB))
        res <- ontoInfo[ind, c("obo_id", "label", "description")]
    }
    return(res)
}



#' @param terms A character vector containing ontology term ids to look up
#' their definition. Each ontology term id should be their official format
#' with their ontology symbol and term number separated by colon.
#' 
#' @examples
#' terms <- ontoTraverse(source = "hp", termId = "HP:0003003", plot = FALSE)
#' getOntoDesc(terms)
#' 
#' 
#' 
#' 
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