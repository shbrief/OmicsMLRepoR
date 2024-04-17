#' Get ancestors or descendants of a specified degree from the given term
#' 
#' @importFrom rols Ontology Term ancestors descendants
#' 
#' @param term Character string; term id
#' @param relationship Character string; specifies type of desired result terms;
#' either "ancestors" or "descendants"
#' @param num Integer; how far from the given term to retrieve result terms
#' 
#' @return A list of rols Term objects
#' 
#' @examples
#' getNumRelatives(term = "NCIT:C26902", relationship = "ancestors", num = 2)
#' getNumRelatives(term = "NCIT:C35708", relationship = "descendants", num = 1)
#'
#' @export
getNumRelatives <- function(term, relationship, num) {
    if (!relationship %in% c("ancestors", "descendants")) {
        stop(paste0("\"", relationship, "\" is not a valid relationship. Please choose either \"ancestors\" or \"descendants\".\n"))
    }
    num_error <- paste0(relationship, " with a degree of ", num, " do not exist. Please select a smaller value of \"num.\"")
    
    ## Get initial Term object
    onto <- get_ontologies(term)
    ontob <- Ontology(onto)
    
    ## Initialize term lists
    end_terms <- list(Term(onto, term))
    
    ## Get immediate parents for all terms in each level
    for (i in 1:num) {
        new_terms <- c()
        for (j in 1:length(end_terms)) {
            cur_term <- end_terms[[j]]
            if (relationship == "ancestors") {
                if (!cur_term@is_root) {
                    r <- parents(cur_term)                    
                } else {
                    stop(num_error)
                }
            } else if (relationship == "descendants") {
                if (cur_term@has_children) {
                    r <- children(cur_term)   
                } else {
                    next
                }
            }
            new_terms <- unique(c(new_terms, r@x))
        }
        if (length(new_terms) == 0) {
            stop(num_error)
        }
        end_terms <- new_terms
    }
    names(end_terms) <- lapply(end_terms, function(x) x@obo_id)
    return(end_terms)
}

#' Get same-level terms (cousins) of a term that are related through an ancestor
#' of a specified distance
#' 
#' @param term Character string; term id
#' @param dist Integer; max distance of joining ancestor; defaults to 1
#' 
#' @return A list of rols Term objects
#' 
#' @examples
#' similarTerms(term = "NCIT:C26902", dist = 1)
#' 
#' @export
similarTerms <- function(term, dist = 1) {
    ## Get ancestors of distance indicated
    ancestors <- getNumRelatives(term, "ancestors", dist)
    
    ## Get all descendants of the ancestors that are the same level as the
    ## original term
    cousins <- unique(unlist(lapply(names(ancestors), function(x)
        getNumRelatives(x, "descendants", dist))))
    names(cousins) <- lapply(cousins, function(x) x@obo_id)
    
    return(cousins)
}