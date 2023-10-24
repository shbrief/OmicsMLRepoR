## Functions to expand and compress treatment metadata

#' Character vector of ID columns
ID_COLS <- readLines(system.file("extdata", "id_columns.txt", package = "OmicsMLRepoR"))

#' Character vector of treatment-related columns (current but not complete)
T_COLS <- readLines(system.file("extdata", "treatment_columns.txt", package = "OmicsMLRepoR"))

#' Expands treatment metadata
#' 
#' @importFrom tidyr separate_longer_delim
#' 
#' @param meta A data frame of metadata including all treatment-related columns
#' @param ecols Optional character vector of columns to expand if presesnt, defaults to all treatment-related columns
#' @param delim Optional delimiter string, defaults to "<;>"
#' 
#' @return A data frame of metadata expanded so that each individual treatment has its own row
#' 
expand_metadata <- function(meta, ecols = T_COLS, delim = "<;>") {
  # Validate input
  stopifnot(is.data.frame(meta),
            is.character(ecols),
            is.character(delim))
  
  # Expand data frame
  separate_longer_delim(data = meta,
                        cols = any_of(ecols),
                        delim = delim)
}

#' Compresses expanded treatment columns to original format
#' 
#' @importFrom dplyr group_by summarise ungroup select
#' 
#' @param meta A data frame with expanded treatment columns
#' @param idcols Optional character vector of columns that identify single samples, defaults to standard ID columns
#' @param ccols Optional character vector of columns to compress if present, defaults to all treatment-related columns
#' @param delim Optional delimiter string, defaults to "<;>"
#'
#' @return A data frame where each sample gets a single row
#' 
compress_metadata <- function(meta, idcols = ID_COLS, ccols = T_COLS, delim = "<;>") {
  # Validate input
  stopifnot(is.data.frame(meta),
            is.character(idcols),
            is.character(ccols),
            is.character(delim))
  
  # Print message to user
  cat("Compressing data frame: this may take a few minutes\n")
  
  # Get original column order
  col_order <- colnames(meta)
  
  # Define which columns to group by and leave un-compressed
  gcols <- idcols[which(idcols %in% col_order)]
  ocols <- setdiff(setdiff(col_order, ccols), idcols)
  
  # Compress data frame
  cmeta <- meta %>%
    group_by_at(gcols) %>%
    summarise(
      across(any_of(ccols), ~paste(na.omit(.), collapse = delim)),
      across(any_of(ocols), ~unique(.))
    ) %>%
    ungroup() %>%
    select(all_of(col_order))
  
  # Return as data frame
  as.data.frame(cmeta)
}