library(rols)
library(googlesheets4)
library(googledrive)
library(tidyverse)
library(beepr)

getNodes <- function(onto, terms) {
  ## onto = character string; ontology name
  ## terms = character vector of term IDs
  
  # Load ontology
  ontob <- Ontology(onto)
  
  # Get unique terms
  terms <- unique(terms)
  
  # Initialize list to store retrieve nodes
  all_nodes <- list()
  
  # Loop through supplied terms
  for (i in 1:length(terms)) {
    print(paste0("Getting possible nodes for ", terms[i]))
    
    tryCatch({
      # Get term information and ancestors from ontology
      cur_trm <- term(ontob, terms[i])
      ancs <- ancestors(cur_trm)
      
      # Filter out ancestors labeled as ontology root
      not_root <- Filter(function(x) attr(x, "is_root") == FALSE, ancs@x)
      
      # Save ancestors
      node_names <- names(not_root)
      node_names <- list(node_names[node_names != "NULL"])
      
    }, error = function(e) {
      print(e)
      print("Original term assigned as node, proceeding to next term")
      node_names <<- terms[i]
    })
    
    # Save ancestors under original term
    all_nodes <- c(all_nodes, node_names)
    names(all_nodes)[i] <- terms[i]
  }
  
  return(all_nodes)
}

getTopDists <- function(onto, nodes) {
  ## onto = character string; ontology name
  ## nodes = character vector of term IDs
  
  # Load ontology
  ontob <- Ontology(onto)
  
  # Initialize dataframe to hold terms and distances
  node_dists <- data.frame(node = rep(NA, length(nodes)),
                           tdist = rep(NA, length(nodes)))
  
  # Loop through supplied terms
  for (i in 1:length(nodes)) {
    print(paste0("Getting distance from top: ", nodes[i]))
    
    tryCatch({
      # Get term and number of ancestors from ontology
      cur_trm <- term(ontob, nodes[i])
      ancs <- ancestors(cur_trm)
      
      # Save term and number of ancestors
      node_dists$node[i] <- nodes[i]
      node_dists$tdist[i] <- length(ancs)
      
    }, error = function(e) {
      print(e)
      print("Error: dists recorded as NA, proceeding to next term")
      node_dists$node[i] <<- nodes[i]
      node_dists$tdist[i] <<- NA
    })
  }
  
  return(node_dists)
}

getBottomDists <- function(onto, nodes) {
  ## onto = character string; ontology name
  ## nodes = character vector of term IDs
  
  # Load ontology
  ontob <- Ontology(onto)
  
  # Initialize dataframe to hold terms and distances
  node_dists <- data.frame(node = rep(NA, length(nodes)),
                           bdist = rep(NA, length(nodes)))
  
  # Loop through supplied terms
  for (i in 1:length(nodes)) {
    print(paste0("Getting distance from bottom: ", nodes[i]))
    
    tryCatch({
      # Get term and number of descendants from ontology
      cur_trm <- term(ontob, nodes[i])
      descs <- descendants(cur_trm)
      
      # Save term and number of descendants
      node_dists$node[i] <- nodes[i]
      node_dists$bdist[i] <- length(descs)
      
    }, error = function(e) {
      print(e)
      print("Error: dists recorded as NA, proceeding to next term")
      node_dists$node[i] <<- nodes[i]
      node_dists$bdist[i] <<- NA
    })
  }
  
  return(node_dists)
}

findReps <- function(onto, vecs) {
  ## onto = character string; ontology name
  ## vecs = list of character vectors
  
  print(onto)
  
  # Initialize storage list and vectors
  nvecs <- list()
  picked_nodes <- c()
  to_remove <- c()
  
  # Save terms with no ancestors as their own top nodes
  for (i in 1:length(vecs)) {
    if (length(vecs[[i]]) == 0) {
      picked_nodes <- c(picked_nodes, names(vecs)[i])
      to_remove <- c(to_remove, i)
    }
    nvecs <- c(nvecs, list(vecs[[i]]))
    names(nvecs)[i] <- names(vecs)[i]
  }
  
  if (length(to_remove > 0)) {
    vecs <- nvecs[-to_remove]    
    
  } else {
    vecs <- nvecs
  }
  
  if (length(vecs) > 0) {
    # Create matrix to represent membership in each term's list of ancestors
    vals <- unique(do.call(c, vecs))
    valsin <- as.matrix(sapply(vecs, function(x) as.integer(vals %in% x)))
    rownames(valsin) <- vals
    
    # Indicate if all child terms have been represented by chosen top nodes
    check <- FALSE
    
    # Initialize data frame to store required distance values
    dmat <- data.frame(node = vals,
                       tdist = NA,
                       bdist = NA)
    
    ## Logic:
    #if multiple picked
    # if empty tdists for picked
    #  get tdists and store
    # filter for nodes picked and compare tdists
    #  if multiple picked
    #   if empty bdists for picked
    #    get bdists and store
    #   filter for nodes picked and compare bdists
    
    # While there are remaining terms to be represented
    while(!check) {
      if (is.matrix(valsin)) {
        # Get node that represents the most terms
        csums <- rowSums(valsin)
        max_node <- which(csums == max(csums))
        
        # Break ties through distance from top, then from bottom
        if (length(max_node) > 1) {
          sdists <- filter(dmat, node %in% names(max_node))
          need_top <- sdists %>%
            filter(is.na(tdist)) %>%
            select(-bdist)
          
          # Get distance from top if not already saved
          if (nrow(need_top) > 0) {
            need_top$tdist <- getTopDists(onto, need_top$node)$tdist
            dmat <- left_join(dmat, need_top, "node") %>%
              mutate(tdist = coalesce(tdist.y, tdist.x)) %>%
              select(-contains("."))
            sdists <- filter(dmat, node %in% names(max_node))
          }
          
          ## TODO: Implement choice of high vs. low nodes
          ##if (node_type == "high") {
          
          highest <- sdists[which(sdists$tdist == min(sdists$tdist)), ]
          
          if (nrow(highest) > 1) {
            need_bottom <- highest %>%
              filter(is.na(bdist)) %>%
              select(-tdist)
            
            # Get distance from bottom if not already saved
            if (nrow(need_bottom) > 0) {
              need_bottom$bdist <- getBottomDists(onto, need_bottom$node)$bdist
              dmat <- left_join(dmat, need_bottom, "node") %>%
                mutate(bdist = coalesce(bdist.y, bdist.x)) %>%
                select(-contains("."))
              sdists <- filter(dmat, node %in% highest$node)
            }
            highest <- sdists[which(sdists$bdist == max(sdists$bdist)), ]
          }
          ##max_node <- which(names(csums) == highest$node)
          ## TODO: find another way to break the tie when neither returns any ancestors or descendants? EFO and maybe SNOMED issue
          max_node <- which(names(csums) == highest$node[order(highest$node)[1]])
          names(max_node) <- highest$node
        }
        
        # Save picked node
        node_name <- names(max_node)
        picked_nodes <- c(picked_nodes, node_name)
        
        # Check which terms are represented
        node_row <- valsin[max_node, ]
        covered <- which(node_row == 1)
        
        # Check if all terms are represented and update indicator
        check <- all(node_row == 1)
        
        # Remove represented terms and chosen node from membership matrix
        valsin <- valsin[-max_node, -covered]
        
      } else {
        # If there is only one possible node, save and exit
        first_node <- names(which(valsin == 1)[1])
        picked_nodes <- c(picked_nodes, first_node)
        check <- TRUE
      }
    }
  }
  return(unique(picked_nodes))
}

displayNodes <- function(nodelist) {
  ## nodelist = list of lists of ontology IDs, grouped and named by ontology
  
  # Initialize dataframe to store term information
  dmat <- as.data.frame(matrix(nrow = sum(lengths(nodelist)),
                               ncol = 3,
                               dimnames = list(c(), c("ontology_term",
                                                      "ontology_term_id",
                                                      "original_ontology_term_db"))))
  
  # Save individual picked nodes with their respective ontologies
  expanded_list <- setNames(unlist(nodelist, use.names = FALSE), rep(names(nodelist), lengths(nodelist)))
  dmat$ontology_term_id <- unname(expanded_list)
  dmat$original_ontology_term_db <- names(expanded_list)
  
  # Loop through picked nodes and get additional information
  for (i in 1:nrow(dmat)) {
    curont <- dmat$original_ontology_term_db[i]
    curid <- dmat$ontology_term_id[i]
    print(paste0("Retrieving info for picked node ", curid))
    
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
    #core_nodes <- lapply(onto_nodes, findReps)
    core_nodes <- mapply(function(o, v) findReps(o, v), names(onto_nodes), onto_nodes, SIMPLIFY = FALSE)
    node_mat <- displayNodes(core_nodes)
    return(node_mat)
  }, error = function(e) {
    print(e)
  })
}
library(beepr)

getNodes <- function(onto, terms) {
  ## onto = character string; ontology name
  ## terms = character vector of term IDs

  # Load ontology
  ontob <- Ontology(onto)
  
  # Get unique terms
  terms <- unique(terms)
  
  # Initialize list to store retrieve nodes
  all_nodes <- list()
  
  # Loop through supplied terms
  for (i in 1:length(terms)) {
    print(paste0("Getting possible nodes for ", terms[i]))
    
    tryCatch({
      # Get term information and ancestors from ontology
      cur_trm <- term(ontob, terms[i])
      ancs <- ancestors(cur_trm)
      
      # Filter out ancestors labeled as ontology root
      not_root <- Filter(function(x) attr(x, "is_root") == FALSE, ancs@x)
      
      # Save ancestors
      node_names <- names(not_root)
      node_names <- list(node_names[node_names != "NULL"])
      
    }, error = function(e) {
      print(e)
      print("Original term assigned as node, proceeding to next term")
      node_names <<- terms[i]
    })
    
    # Save ancestors under original term
    all_nodes <- c(all_nodes, node_names)
    names(all_nodes)[i] <- terms[i]
  }
  
  return(all_nodes)
}

getTopDists <- function(onto, nodes) {
  ## onto = character string; ontology name
  ## nodes = character vector of term IDs
  
  # Load ontology
  ontob <- Ontology(onto)
  
  # Initialize dataframe to hold terms and distances
  node_dists <- data.frame(node = rep(NA, length(nodes)),
                           tdist = rep(NA, length(nodes)))
  
  # Loop through supplied terms
  for (i in 1:length(nodes)) {
    print(paste0("Getting distance from top: ", nodes[i]))
    
    tryCatch({
      # Get term and number of ancestors from ontology
      cur_trm <- term(ontob, nodes[i])
      ancs <- ancestors(cur_trm)
      
      # Save term and number of ancestors
      node_dists$node[i] <- nodes[i]
      node_dists$tdist[i] <- length(ancs)
      
    }, error = function(e) {
      print(e)
      print("Error: dists recorded as NA, proceeding to next term")
      node_dists$node[i] <<- nodes[i]
      node_dists$tdist[i] <<- NA
    })
  }
  
  return(node_dists)
}

getBottomDists <- function(onto, nodes) {
  ## onto = character string; ontology name
  ## nodes = character vector of term IDs
  
  # Load ontology
  ontob <- Ontology(onto)
  
  # Initialize dataframe to hold terms and distances
  node_dists <- data.frame(node = rep(NA, length(nodes)),
                           bdist = rep(NA, length(nodes)))
  
  # Loop through supplied terms
  for (i in 1:length(nodes)) {
    print(paste0("Getting distance from bottom: ", nodes[i]))
    
    tryCatch({
      # Get term and number of descendants from ontology
      cur_trm <- term(ontob, nodes[i])
      descs <- descendants(cur_trm)
      
      # Save term and number of descendants
      node_dists$node[i] <- nodes[i]
      node_dists$bdist[i] <- length(descs)
      
    }, error = function(e) {
      print(e)
      print("Error: dists recorded as NA, proceeding to next term")
      node_dists$node[i] <<- nodes[i]
      node_dists$bdist[i] <<- NA
    })
  }
  
  return(node_dists)
}

findReps <- function(onto, vecs) {
  ## onto = character string; ontology name
  ## vecs = list of character vectors
  
  print(onto)
  
  # Initialize storage list and vectors
  nvecs <- list()
  picked_nodes <- c()
  to_remove <- c()
  
  # Save terms with no ancestors as their own top nodes
  for (i in 1:length(vecs)) {
    if (length(vecs[[i]]) == 0) {
      picked_nodes <- c(picked_nodes, names(vecs)[i])
      to_remove <- c(to_remove, i)
    }
    nvecs <- c(nvecs, list(vecs[[i]]))
    names(nvecs)[i] <- names(vecs)[i]
  }
  
  if (length(to_remove > 0)) {
    vecs <- nvecs[-to_remove]    
    
  } else {
    vecs <- nvecs
  }
  
  if (length(vecs) > 0) {
    # Create matrix to represent membership in each term's list of ancestors
    vals <- unique(do.call(c, vecs))
    valsin <- as.matrix(sapply(vecs, function(x) as.integer(vals %in% x)))
    rownames(valsin) <- vals
    
    # Indicate if all child terms have been represented by chosen top nodes
    check <- FALSE
    
    # Initialize data frame to store required distance values
    dmat <- data.frame(node = vals,
                       tdist = NA,
                       bdist = NA)
    
    ## Logic:
    #if multiple picked
    # if empty tdists for picked
    #  get tdists and store
    # filter for nodes picked and compare tdists
    #  if multiple picked
    #   if empty bdists for picked
    #    get bdists and store
    #   filter for nodes picked and compare bdists
    
    # While there are remaining terms to be represented
    while(!check) {
      if (is.matrix(valsin)) {
        # Get node that represents the most terms
        csums <- rowSums(valsin)
        max_node <- which(csums == max(csums))
        
        # Break ties through distance from top, then from bottom
        if (length(max_node) > 1) {
          sdists <- filter(dmat, node %in% names(max_node))
          need_top <- sdists %>%
            filter(is.na(tdist)) %>%
            select(-bdist)
          
          # Get distance from top if not already saved
          if (nrow(need_top) > 0) {
            need_top$tdist <- getTopDists(onto, need_top$node)$tdist
            dmat <- left_join(dmat, need_top, "node") %>%
              mutate(tdist = coalesce(tdist.y, tdist.x)) %>%
              select(-contains("."))
            sdists <- filter(dmat, node %in% names(max_node))
          }

          ## TODO: Implement choice of high vs. low nodes
          ##if (node_type == "high") {
          
          highest <- sdists[which(sdists$tdist == min(sdists$tdist)), ]
          
          if (nrow(highest) > 1) {
            sdists <- filter(dmat, node %in% highest$node)
            need_bottom <- sdists %>%
              filter(is.na(bdist)) %>%
              select(-tdist)
            
            # Get distance from bottom if not already saved
            if (nrow(need_bottom) > 0) {
              need_bottom$bdist <- getBottomDists(onto, need_bottom$node)$bdist
              dmat <- left_join(dmat, need_bottom, "node") %>%
                mutate(bdist = coalesce(bdist.y, bdist.x)) %>%
                select(-contains("."))
              sdists <- filter(dmat, node %in% highest$node)
            }
            highest <- sdists[which(sdists$bdist == max(sdists$bdist)), ]
          }
          ##max_node <- which(names(csums) == highest$node)
          ##names(max_node) <- highest$node
          ## TODO: find another way to break the tie when neither returns any ancestors or descendants? EFO and maybe SNOMED issue
          max_node <- which(names(csums) == highest$node[order(highest$node)[1]])
          names(max_node) <- highest$node[order(highest$node)[1]]
        }
        
        # Save picked node
        node_name <- names(max_node)
        picked_nodes <- c(picked_nodes, node_name)
        
        # Check which terms are represented
        node_row <- valsin[max_node, ]
        covered <- which(node_row == 1)
        
        # Check if all terms are represented and update indicator
        check <- all(node_row == 1)
        
        # Remove represented terms and chosen node from membership matrix
        valsin <- valsin[-max_node, -covered]
        
      } else {
        # If there is only one possible node, save and exit
        first_node <- names(which(valsin == 1)[1])
        picked_nodes <- c(picked_nodes, first_node)
        check <- TRUE
      }
    }
  }
  return(unique(picked_nodes))
}

displayNodes <- function(nodelist) {
  ## nodelist = list of lists of ontology IDs, grouped and named by ontology
  
  # Initialize dataframe to store term information
  dmat <- as.data.frame(matrix(nrow = sum(lengths(nodelist)),
                               ncol = 3,
                               dimnames = list(c(), c("ontology_term",
                                                      "ontology_term_id",
                                                      "original_ontology_term_db"))))
  
  # Save individual picked nodes with their respective ontologies
  expanded_list <- setNames(unlist(nodelist, use.names = FALSE), rep(names(nodelist), lengths(nodelist)))
  dmat$ontology_term_id <- unname(expanded_list)
  dmat$original_ontology_term_db <- names(expanded_list)
  
  # Loop through picked nodes and get additional information
  for (i in 1:nrow(dmat)) {
    curont <- dmat$original_ontology_term_db[i]
    curid <- dmat$ontology_term_id[i]
    print(paste0("Retrieving info for picked node ", curid))
    
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
    #core_nodes <- lapply(onto_nodes, findReps)
    core_nodes <- mapply(function(o, v) findReps(o, v), names(onto_nodes), onto_nodes, SIMPLIFY = FALSE)
    node_mat <- displayNodes(core_nodes)
    return(node_mat)
  }, error = function(e) {
    print(e)
  })
}