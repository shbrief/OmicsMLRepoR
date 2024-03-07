#' Extract both ancestors and descendants of a term from ontology tree
#' 
#' @import rols
#' @importFrom ontologyPlot onto_plot
#' 
#' @param ontoTermId Ontology term id started with the ontology prefix and id
#' separated by colon, such as \code{NCIT:C15280}. 
#' @param source Ontology database id. Currently available options are 
#' \code{NCIT, CHEBI, HP}.
#' @param plot Under the default (\code{FALSE}), this function returns a 
#' character vector of associate ontology terms. If it is set to \code{TRUE}, 
#' it will plot the ontology tree of the returned term. (Currently, not 
#' supported.) 
#' 
#' @return Ontology plot including queried term and its associated ancestors
#' and descendants terms. With the \code{plot=FALSE} argument, it will return
#' a character vector of associated ontology term ids.  
#' 
#' @examples
#' ontoTraverse(ontoTermId = "NCIT:C15280", source = "ncit")
#' ontoTraverse(ontoTermId = "CHEBI:5262", source = "chebi")
#' ontoTraverse(ontoTermId = "HP:0011793", source = "hp")
#' 
#' @export
ontoTraverse <- function(ontoTermId, 
                         source, 
                         plot = FALSE,
                         includeDefinition = FALSE) {
    
    # Load ontology
    ol <- Ontologies()
    ontology <- ol[[source]]
    
    trm <- Term(ontology, ontoTermId)
    
    # Get the full tree of direct ancestors/descendants of `ontoTermId` input
    all_connected_terms <- c(names(termId(parents(trm))), 
                             names(termId(trm)), 
                             names(termId(children(trm))))
    
    if (isFALSE(plot)) {
        return(all_connected_terms)
    } else {
        # Highlight the quried ontoTermId in the tree
        term_ind <- which(all_connected_terms == ontoTermId)
        fillcolors <- rep("powderblue", length(all_connected_terms))
        fillcolors[term_ind] <- "yellow"
        
        # Plotting
        plot_all <- ontologyPlot::onto_plot(ontology, 
                                            terms = all_connected_terms,
                                            fillcolor = fillcolors)
        return(plot_all)
    }
}
