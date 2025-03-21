#' Groups ontology terms by the child term of a provided "parent" they fall
#' under
#' 
#' @import rols
#'
#' @param parent Character; Term to use as parent of summarized children
#' @param descendants Character vector; Terms to summarize as children of parent
#' @param ontology Character; Ontology database ID
#' @return A named list of character vectors containing the descendants
#' summarized into groups. Name of each element is the child of the parent that
#' the descendants are grouped under.
#' 
#' @examples
#' ontology <- "ncit"
#' parent <- "NCIT:C17049"
#' descendants <- c("NCIT:C44265", "NCIT:C77811", "NCIT:C43856",
#'                  "NCIT:C43672", "NCIT:C2991", "NCIT:C43860")
#' ontoSummarize(parent, descendants, ontology)
#' 
ontoSummarize <- function(parent, descendants, ontology) {
    # Initialize ontology
    ontob <- Ontology(ontology)
    
    # Get children of parent to establish groups
    pterm <- Term(ontob, parent)
    pchildren <- names(termLabel(children(pterm)))
    
    # Get ancestors of all descendants
    dancs <- sapply(descendants, 
                  function(x) names(termLabel(ancestors(Term(ontob, x)))),
                   simplify = FALSE,
                   USE.NAMES = TRUE)
    
    dall <- mapply(c, names(dancs), dancs)
    
    # Group descendants by children of parents
    dgroups <- sapply(dall, function(x) x[which(x %in% pchildren)],
                      simplify = FALSE,
                      USE.NAMES = TRUE)
    dgroups[lengths(dgroups) == 0] <- paste0("Not a descendant of ", parent)
    
    finalgroups <- split(names(dgroups), unname(unlist(dgroups)))
    return(finalgroups)
}
