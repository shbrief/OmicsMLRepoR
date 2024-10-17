#' Plot ontology tree
#' 
#' @import rols
#' @import dplyr
#' @import DiagrammeR 
#' @importFrom plyr mapvalues
#' @importFrom data.tree FromDataFrameNetwork
#' @importFrom jsonlite fromJSON
#' 
#' @param term A character (1). Ontology term id (obo_id)
#' @param display A character (1) specifying a node labeling option. Two 
#' available options are `Term` for ontology term or IRI 
#' (Internationalized Resource Identifier) and `Text` for the label or 
#' preferred name.
#' 
#' @return A ontology tree plot. All the terms used in the output plot are 
#' ancestors of the queried term, so the queried term is the tip.
#' 
#' @examples 
#' ontoTreePlot("NCIT:C2852", "Term")
#' 
#' @export
ontoTreePlot <- function(term, display = c("Term", "Text")) {
    display <- match.arg(display)
    
    sample_id <- term
    sample_db <- get_ontologies(term)
    
    ## load term object and retrieve link to JSON tree
    ontob <- Ontology(sample_db)
    cur_trm <- Term(ontob, sample_id)
    jstree <- cur_trm@links$jstree$href
    
    ## transform JSON into dataframe
    tree_frame <- jsonlite::fromJSON(jstree)
    
    ## transform dataframe into usable edgelist
    map <- tree_frame %>%
        rowwise() %>%
        mutate(term = unlist(strsplit(iri, split = "/"))[5]) %>%
        select(id, term, text)
    
    edgelist_base <- tree_frame %>%
        select(parent, id) %>%
        rename(from = parent,
               to = id) %>%
        filter(from != "#")
    
    ## display options
    foo <- base::switch(display, Term = map$term, Text = map$text)
    edgelist <- edgelist_base %>%
        mutate(from = plyr::mapvalues(from, map$id, foo, warn_missing = FALSE)) %>%
        mutate(to = plyr::mapvalues(to, map$id, foo, warn_missing = FALSE))
    
    ## plot tree with data.tree package; other packages will also work with the edgelist
    tree <- data.tree::FromDataFrameNetwork(edgelist)
    plot(tree)
}