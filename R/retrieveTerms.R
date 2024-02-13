# temporary library loading
library(rols)
library(tidyverse) #stringr str_match
#library(svDialogs)

# Temporary variables
data_repo_dir <- "/home/kaelyn/Desktop/Work/OmicsMLRepoData"

cMD_mapdir <- paste0(data_repo_dir, "/curatedMetagenomicData/maps")
cBioPortalData_mapdir <- paste0(data_repo_dir, "/cBioPortalData/maps")

## Functions to retrieve used terms from raw user input

#' Retrieve ontology term maps from a directory with .csv files
#' 
#' @importFrom tidyverse str_match
#' 
#' @param directory Path to directory containing .csv map files
#' @param map_file_pattern Regular expression to detect map files; May contain a capturing group to retrieve feature names from map filenames
#' 
#' @return List of maps as data frames
#' 
getMaps <- function(directory, map_file_pattern) {
  files <- list.files(directory, pattern = map_file_pattern)
  filepaths <- sapply(files, function (x) paste0(directory, "/", x), USE.NAMES = FALSE)
  maps <- lapply(filepaths, read.csv)
  features <- str_match(files, map_file_pattern)
  if (ncol(features) > 1) {
    features <- features[,2]
    names(maps) <- features
  }
  
  return(maps)
}

#' Retrieve and save ancestors of ontology terms
#'
#' @param ids Character vector of term ids
#' @param dbs Character vector of corresponding ontology names. Single string also accepted if all terms share a single ontology.
#' 
#' @return List of character vectors of ancestors named by original term
#' 
getAncestors <- function(ids, dbs) {
  map <- data.frame(id = ids,
                    db = dbs)
  tryCatch({
    onto_frames <- split(map, map$db)
    onto_terms <- lapply(onto_frames, function(x) x$id)
    onto_nodes <- mapply(function(n, t) getNodes(n, t), names(onto_terms), onto_terms, SIMPLIFY = FALSE)
    onames <- unlist(lapply(onto_nodes, function(x) names(x)), use.names = FALSE)
    oancs <- unlist(onto_nodes, recursive = FALSE, use.names = FALSE)
    names(oancs) <- onames
    return(oancs)
  }, error = function(e) {
    print(e)
  })
}

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
