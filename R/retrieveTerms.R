# temporary library loading
library(rols)
#library(svDialogs)

## Functions to retrieve used terms from raw user input

#' Take user input and return potential terms within used ontologies
#' 

#' Helper: collect ontologies used for each attribute? Filter results for terms that exist in ancestors (and use to bypass picking?)
#' Need access to maps?

#' Main function: take query and optional argument for attribute name

qry <- OlsSearch(q = "neoplasm") # if ontology specified: qry <- OlsSearch(q = "query", ontology = "ONTO")
qry <- olsSearch(qry)
qdf <- as(qry, "data.frame")

# NCIT:C3262
t <- "NCIT:C3262"
u <- c("NCIT:C3262", "EFO:0000313")

trm <- term("ncit", "NCIT:C3262")
d <- descendants(trm)

used <- unique(map_list[[4]]$curated_ontology_term_id)

result <- used[which(used %in% names(d@x))]
# get all ancestors from map terms
# pull out terms where qdf %in% ancestors

csc <- read.csv("/home/kaelyn/Desktop/Work/OmicsMLRepoData/curatedMetagenomicData/data/curated_study_condition.csv")

rows <- csc[csc$curated_disease_ontology_term_id %in% result,]
rows <- csc[any(unlist(strsplit(csc$curated_disease_ontology_term_id, ";"))) %in% result,]

tlist <- strsplit(csc$curated_disease_ontology_term_id, ";")
inlist <- lapply(tlist, function(x) intersect(x, result))
llist <- inlist[which(lengths(inlist) != 0)]
lframe <- csc[which(lengths(inlist) != 0),]


#lapply(onto_nodes$NCIT, function(x) t %in% x)
#umatches <- lapply(unlist(onto_nodes, use.names = FALSE), function(x) t %in% x)
#matches <- lapply(onto_nodes, function(x) lapply(x, function(y) t %in% y))

#onto_nodes <- mapply(function(n, t) getNodes(n, t), map$db, map$id, SIMPLIFY = FALSE)

# get all ancestors, first run through half of commonNodes to get 'onto_nodes' object
onames <- unlist(lapply(onto_nodes, function(x) names(x)), use.names = FALSE)
oancs <- unlist(onto_nodes, recursive = FALSE, use.names = FALSE)
names(oancs) <- onames

mresult <- names(which(lapply(oancs, function(x) any(u %in% x)) == TRUE))

tlist <- strsplit(csc$curated_disease_ontology_term_id, ";")
inlist <- lapply(tlist, function(x) intersect(x, mresult))
llist <- inlist[which(lengths(inlist) != 0)]
lframe <- csc[which(lengths(inlist) != 0),]
