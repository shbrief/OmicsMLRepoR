library(rols)
library(googlesheets4)
library(googledrive)
library(beepr)

getNodes <- function(onto, terms) {
  ## onto = character string; ontology name
  ## terms = character vector of term IDs

  ontob <- Ontology(onto)
  
  all_nodes <- list()
  
  for (i in 1:length(terms)) {
    print(terms[i])
    tryCatch({
      cur_trm <- term(ontob, terms[i])
      ancs <- ancestors(cur_trm)
      not_root <- Filter(function(x) attr(x, "is_root") == FALSE, ancs@x)
      node_names <- names(not_root)
      node_names <- list(node_names[node_names != "NULL"])
    }, error = function(e) {
      print(e)
      print("Original term assigned as node, proceeding to next term")
      node_names <- names(cur_trm)
    })
    all_nodes <- c(all_nodes, node_names)
    names(all_nodes)[i] <- terms[i]
  }
  
  return(all_nodes)
}

findReps <- function(vecs) {
  ## vecs = list of character vectors
  
  nvecs <- list()
  picked_nodes <- c()
  
  for (i in 1:length(vecs)) {
    if (length(vecs[[i]]) == 0) {
      picked_nodes <- c(picked_nodes, names(vecs)[i])
    } else {
      nvecs <- c(nvecs, list(vecs[[i]]))
      names(nvecs)[i] <- names(vecs)[i]
    }
  }
  
  vecs <- nvecs
  
  if (length(vecs) > 0) {
    vals <- unique(do.call(c, vecs))
    valsin <- sapply(vecs, function(x) as.integer(vals %in% x))
    rownames(valsin) <- vals
    
    check <- FALSE    
    
    while(!check) {
      if (is.matrix(valsin)) {
        csums <- rowSums(valsin)
        max_node <- which(csums == max(csums))[1]
        node_name <- names(max_node)
        picked_nodes <- c(picked_nodes, node_name)
        node_row <- valsin[max_node, ]
        covered <- which(node_row == 1)
        
        check <- all(node_row == 1)
        
        valsin <- valsin[-max_node, -covered]
      } else {
        first_node <- names(which(valsin == 1)[1])
        picked_nodes <- c(picked_nodes, first_node)
        check <- TRUE
      }
    }
  }
  return(picked_nodes)
}

displayNodes <- function(nodelist) {
  # nodelist = list of lists of ontology IDs, grouped and named by ontology
  
  dmat <- as.data.frame(matrix(nrow = sum(lengths(nodelist)),
                               ncol = 3,
                               dimnames = list(c(), c("ontology_term",
                                                      "ontology_term_id",
                                                      "original_ontology_term_db"))))
  
  expanded_list <- setNames(unlist(nodelist, use.names = FALSE), rep(names(nodelist), lengths(nodelist)))
  dmat$ontology_term_id <- unname(expanded_list)
  dmat$original_ontology_term_db <- names(expanded_list)
  
  for (i in 1:nrow(dmat)) {
    curont <- dmat$original_ontology_term_db[i]
    curid <- dmat$ontology_term_id[i]
    print(paste0("Retrieving ", curid))
    
    qry <- OlsSearch(q = curid, exact = TRUE)
    qry <- olsSearch(qry)
    qdrf <- as(qry, "data.frame")
    
    if (curont %in% qdrf$ontology_prefix) {
      record <- qdrf[qdrf$ontology_prefix == curont, ][1,]
    } else if (TRUE %in% qdrf$is_defining_ontology) {
      record <- qdrf[qdrf$is_defining_ontology == TRUE, ]
    } else {
      record <- qdrf[1, ]
    }
    dmat$ontology_term[i] <- record$label
  }
  return(dmat)
}

commonNodes <- function(map) {
  # map = ontology term map with columns "curated_ontology_term_id" and "curated_ontology_term_db"
  
  tryCatch({
    onto_frames <- split(map, map$curated_ontology_term_db)
    onto_terms <- lapply(onto_frames, function(x) x$curated_ontology_term_id)
    onto_nodes <- mapply(function(n, t) getNodes(n, t), names(onto_terms), onto_terms, SIMPLIFY = FALSE)
    core_nodes <- lapply(onto_nodes, findReps)
    node_mat <- displayNodes(core_nodes)
    return(node_mat)
  }, error = function(e) {
    print(e)
  })
}