#' Plot ontology tree
#' 
#' @import rols
#' @import dplyr
#' @importFrom plyr mapvalues
#' @importFrom data.tree FromDataFrameNetwork
#' @importFrom jsonlite fromJSON
#' 
#' @param term A charater (1). Ontology term id (obo_id)
#' 
#' @examples 
#' ontoTreePlot("NCIT:C2852")
#' 
#' @export
ontoTreePlot <- function(term) {
    
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
        select(id, term)
    
    edgelist <- tree_frame %>%
        select(parent, id) %>%
        rename(from = parent,
               to = id) %>%
        filter(from != "#") %>%
        mutate(from = plyr::mapvalues(from, map$id, map$term, warn_missing = FALSE)) %>%
        mutate(to = plyr::mapvalues(to, map$id, map$term, warn_missing = FALSE))
    
    ## plot tree with data.tree package; other packages will also work with the edgelist
    tree <- data.tree::FromDataFrameNetwork(edgelist)
    plot(tree)
}