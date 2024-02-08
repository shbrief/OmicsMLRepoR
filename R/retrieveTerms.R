# temporary library loading
library(rols)

## Functions to retrieve used terms from raw user input

#' Take user input and return potential terms within used ontologies
#' 

#' Helper: collect ontologies used for each attribute?
#' Need access to maps?

#' Main function: take query and optional argument for attribute name

qry <- OlsSearch(q = "neoplasm") # if ontology specified: qry <- OlsSearch(q = "query", ontology = "ONTO")
qry <- olsSearch(qry)
qdf <- as(qry, "data.frame")

# NCIT:C3262

trm <- term("ncit", "NCIT:C3262")
d <- descendants(trm)

used <- unique(map_list[[4]]$curated_ontology_term_id)

result <- used[which(used %in% names(d@x))]
# get all ancestors from map terms
# pull out terms where qdf %in% ancestors
