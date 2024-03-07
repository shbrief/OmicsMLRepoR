.get_family <- function(source,
                        termId, 
                        property,
                        plot) {
    
    ontology <- .loadOntology(source)
    res <- get_term_property(ontology = ontology,
                             property = property,
                             term = termId,
                             as_names = TRUE)
    
    if (isTRUE(plot)) {
        ontologyPlot::onto_plot(ontology,
                                terms = c(termId, names(res)),
                                fillcolor = c("yellow", 
                                              rep("powderblue", length(res))))
    } else {
        return(res)
    }
}

#' Get the parents of a requested ontology term
#'
#' @examples
#' get_parents(source = "chebi", termId = "CHEBI:5262")
#' get_parents(source = "hp", termId ="HP:0025031")
#' 
#' @export
get_parents <- function(source, termId, plot = TRUE) {
    .get_family(source, termId, property = "parents", plot = plot)
    
}

#' Get the parents of a requested ontology term
#'
#' @examples
#' get_children(source = "hp", termId ="HP:0025031")
#' 
#' @export
get_children <- function(source, termId, plot = TRUE) {
    .get_family(source, termId, property = "children", plot = plot)
    
}

#' Get the siblings of a requested ontology term
#'
#' @examples
#' get_siblings(source = "hp", termId = "HP:0003003")
#' 
#' @export
get_siblings <- function(source, termId, plot = TRUE) {
    parents <- get_parents(source = source, termId = termId, plot = FALSE)
}


