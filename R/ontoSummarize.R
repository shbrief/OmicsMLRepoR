#' Groups ontology terms by the child term of a provided "parent" they fall
#' under
#' 
#' @import rols
#' @import stringr
#'
#' @param parent Character; Term to use as parent of summarized children
#' @param descendants Character vector; Terms to summarize as children of parent
#' @param ontology Character; Ontology database ID
#' @return A dataframe containing the descendants summarized into groups. Name
#' of each group is the child of the parent that the descendants are grouped
#' under. Both IDs and labels of the ontology terms are provided.
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
    collapse_groups <- unlist(unname(lapply(finalgroups, function(x)
        paste(x, collapse = ";"))))
    
    ids_to_convert <- unique(c(names(finalgroups), unlist(unname(finalgroups))))
    extract_ids <- str_match(ids_to_convert, "NCIT:.*$")
    termnames <- unlist(lapply(extract_ids, function(x) termLabel(Term(ontob, x))))
    names(termnames) <- extract_ids
    group_names <- str_replace_all(names(finalgroups), termnames)
    original_names <- str_replace_all(collapse_groups, termnames)
    
    finalframe <- data.frame(group_ids = names(finalgroups),
                             group_names = group_names,
                             original_ids = collapse_groups,
                             original_names = original_names)
    
    return(finalframe)
}
