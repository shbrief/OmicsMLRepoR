# Load ontology
#<<<<<<<<<<<<<<<<< Save ontology in the package or cloud repository? As obo file (in extdata) vs. Rdata (in data)
#<<<<<<<<<<<<<<<<< Long loading time for large ontology like NCIT
.loadOntology <- function(source) {
    
    if (toupper(source) %in% c("NCIT", "CHEBI")) { 
        dir <- system.file("extdata", package = "OmicsMLRepoR")
        ontoTable <- read.csv(file.path(dir, "ontoTable.csv"))
        ind <- which(ontoTable$ontologyDB == toupper(source))
        ontoDB <- file.path(dir, ontoTable$filename[ind])
        
        ontology <- ontologyIndex::get_ontology(ontoDB)
        return(ontology)
    } else if (toupper(source) == "HP") { #<<<<<<<<<< For test: update once all the ontology is provided in the same way
        data("hpo")
        return(hpo)
    }
}


#' Extract ancestors/descendants of a term from ontology tree
#' 
#' @import ontologyIndex
#' @importFrom ontologyPlot onto_plot
#' 
#' @param source Ontology database id. Currently available options are 
#' \code{NCIT, CHEBI, HP}.
#' @param termId Ontology term id started with \code{{ontology database id}:}, 
#' such as \code{NCIT:C15280}. Accept term itself in addition to term id??
#' @param plot Default is \code{TRUE}. If it is set to \code{FALSE}, this 
#' function will return a character vector of associate ontology terms. 
#' 
#' @return Ontology plot including queried term and its associated ancestors
#' and descendants terms. With the \code{plot=FALSE} argument, it will return
#' a character vector of associated ontology term ids.  
#' 
#' @examples
#' ontoTraverse(source = "ncit", termId = "NCIT:C15280")
#' ontoTraverse(source = "chebi", termId = "CHEBI:5262")
#' ontoTraverse(source = "hp", termId = "HP:0003003")
#' 
#' @export
ontoTraverse <- function(source, 
                         termId, 
                         plot = TRUE,
                         includeDefinition = FALSE) {
    
    # Load ontology
    ontology <- .loadOntology(source)
    
    # Get the full tree of direct ancestors/descendants of `termId` input
    all_connected_terms <- c(get_ancestors(ontology = ontology, 
                                           terms = termId), 
                             get_descendants(ontology = ontology, 
                                             roots = termId, 
                                             exclude_roots = TRUE))
    
    if (isFALSE(plot)) {
        return(all_connected_terms)
    } else {
        # Highlight the queied termId in the tree
        term_ind <- which(all_connected_terms == termId)
        fillcolors <- rep("powderblue", length(all_connected_terms))
        fillcolors[term_ind] <- "yellow"
        
        # Plotting
        plot_all <- ontologyPlot::onto_plot(ontology, 
                                            terms = all_connected_terms,
                                            fillcolor = fillcolors)
        return(plot_all)
    }
}


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
get_parents <- function(source, termId, plot = TRUE) {
    .get_family(source, termId, property = "parents", plot = plot)
    
}

#' Get the parents of a requested ontology term
#'
#' @examples
#' get_children(source = "hp", termId ="HP:0025031")
#' 
get_children <- function(source, termId, plot = TRUE) {
    .get_family(source, termId, property = "children", plot = plot)
    
}
