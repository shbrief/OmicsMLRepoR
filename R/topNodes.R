## Functions to find common ontology nodes

#' Retrieves all ancestors for supplied terms
#' 
#' @importFrom rols Ontology term ancestors
#' 
#' @param onto A character vector. Name(s) of ontologies that terms are from.
#' @param terms A haracter vector of ontology term IDs.
#' 
#' @return A named list. Names of elements are original nodes (`terms`). 
#' Each element is a character vectors containing the ancestors of the 
#' element name (i.e., original terms provided).
#' 
#' @examples
#' terms <- c("NCIT:C25301", "NCIT:C29844", "NCIT:C29846", "NCIT:C29848")
#' getNodes(onto = "NCIT", terms = terms)
#' 
#' 
#' @export
getNodes <- function(onto, terms) {
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

#' Retrieves number of ancestors of given term
#' 
#' @importFrom rols Ontology term ancestors
#' 
#' @param onto Character string; name of ontology database
#' @param nodes Character vector of term IDs
#' 
#' @return Dataframe of submitted terms and numbers of ancestors
#'
.getTopDists <- function(onto, nodes) {
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

#' Retrieves number of descendants of given term
#' 
#' @importFrom rols Ontology term descendants
#' 
#' @param onto Character string; name of ontology database
#' @param nodes Character vector of term IDs
#' 
#' @return Dataframe of submitted terms and numbers of descendants
#'
.getBottomDists <- function(onto, nodes) {
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

#' Retrieves ontology terms and database information for given term ids
#'
#' @importFrom rols OlsSearch olsSearch
#' 
#' @param onto Character string; name of ontology database
#' @param nodevec Character vector of term IDs
#' 
#' @return Dataframe of submitted term IDs, term names, and term databases
#' 
.displayNodes <- function(onto, nodevec) {
  # Initialize dataframe to store term information
  dmat <- as.data.frame(matrix(nrow = sum(lengths(nodevec)),
                               ncol = 3,
                               dimnames = list(c(), c("ontology_term",
                                                      "ontology_term_id",
                                                      "original_ontology_term_db"))))
  
  # Save individual picked nodes with their respective ontologies
  dmat$ontology_term_id <- unname(unlist(nodevec))
  dmat$original_ontology_term_db <- onto
  
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

#' Chooses which ancestors cover all given original terms, prioritizing a low number of chosen nodes
#' 
#' @importFrom dplyr filter left_join mutate select
#' 
#' @param onto Character string; name of ontology database
#' @param vecs List of character vectors of ancestors named by original node
#' 
#' @return Dataframe of chosen nodes including information on number of original terms covered
#' 
#' @export
findReps <- function(onto, vecs) {
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
            need_top$tdist <- .getTopDists(onto, need_top$node)$tdist
            dmat <- left_join(dmat, need_top, "node") %>%
              mutate(tdist = coalesce(tdist.y, tdist.x)) %>%
              select(-contains("."))
            sdists <- filter(dmat, node %in% names(max_node))
          }
          
          if (all(is.na(sdists$tdist))) {
            highest <- sdists
          } else {
            # max top distance prioritizes lowest
            highest <- sdists[which(sdists$tdist == max(sdists$tdist, na.rm = TRUE)), ]
          }
          
          if (nrow(highest) > 1) {
            sdists <- filter(dmat, node %in% highest$node)
            need_bottom <- sdists %>%
              filter(is.na(bdist)) %>%
              select(-tdist)
            
            # Get distance from bottom if not already saved
            if (nrow(need_bottom) > 0) {
              need_bottom$bdist <- .getBottomDists(onto, need_bottom$node)$bdist
              dmat <- left_join(dmat, need_bottom, "node") %>%
                mutate(bdist = coalesce(bdist.y, bdist.x)) %>%
                select(-contains("."))
              sdists <- filter(dmat, node %in% highest$node)
            }
            
            if (all(is.na(sdists$bdist))) {
              highest <- sdists
            } else {
              # min bottom distance prioritizes lowest
              highest <- sdists[which(sdists$bdist == min(sdists$bdist, na.rm = TRUE)), ] 
            }
          }
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
  
  nlist <- unique(picked_nodes)
  nmat <- .displayNodes(onto, nlist)
  nmat$num_original_covered <- NA
  nmat$num_original <- length(vecs)
  
  for (i in 1:nrow(nmat)) {
    cur_node <- nmat$ontology_term_id[i]
    num_covered <- 0
    for (j in 1:length(vecs)) {
      if (cur_node %in% vecs[[j]]) {
        num_covered <- num_covered + 1
      }
    }
    nmat$num_original_covered[i] <- num_covered
  }
  
  return(nmat)
}

#' Retrieves top nodes for a given ontology term map
#' 
#' @importFrom dplyr bind_rows
#' 
#' @param ids Character vector of term ids
#' @param dbs Character vector of corresponding ontology database names
#' 
#' @return Dataframe of chosen nodes including information on number of original terms covered
#' 
#' @export
commonNodes <- function(ids, dbs) {
  map <- data.frame(id = ids,
                    db = dbs)
  tryCatch({
    onto_frames <- split(map, map$db)
    onto_terms <- lapply(onto_frames, function(x) x$id)
    onto_nodes <- mapply(function(n, t) getNodes(n, t), names(onto_terms), onto_terms, SIMPLIFY = FALSE)
    core_nodes <- mapply(function(o, v) findReps(o, v), names(onto_nodes), onto_nodes, SIMPLIFY = FALSE)
    node_mat <- bind_rows(core_nodes)
    return(node_mat)
  }, error = function(e) {
    print(e)
  })
}
