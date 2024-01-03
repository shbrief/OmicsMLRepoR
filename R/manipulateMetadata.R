#' Expands treatment metadata
#' 
#' @importFrom tidyr separate_longer_delim
#' 
#' @param meta A data frame of metadata including all treatment-related columns
#' @param ecols Optional. A character vector of columns to expand if present, 
#' defaults to all treatment-related columns
#' @param delim Optional. A character (1) of a delimiter to use before 
#' expansion. Default is \code{"<;>"}.
#' 
#' @return A data frame of metadata expanded so that each individual treatment has its own row
#' 
#' @export
expandMetadata <- function(meta, 
                           ecols = NULL, 
                           delim = "<;>") {
    
    # Character vector of treatment-related columns 
    # Current but not complete <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    if (is.null(ecols)) {
        dir <- system.file("extdata", package = "OmicsMLRepoR")
        ecols <- readLines(file.path(dir, "treatment_columns.txt"))
    }
    
    # Validate input
    stopifnot(is.data.frame(meta),
              is.character(ecols),
              is.character(delim))
  
    # Expand data frame
    res <- separate_longer_delim(data = meta,
                                 cols = any_of(ecols),
                                 delim = delim)
    return(res)
}

#' Compresses expanded treatment columns to original format
#' 
#' @importFrom dplyr group_by summarise ungroup select
#' 
#' @param meta A data frame with expanded treatment columns.
#' @param idcols Optional. A character vector of columns that identify 
#' single samples. Defaults to standard ID columns.
#' @param ccols Optional. A character vector of columns to compress if 
#' present, defaults to all treatment-related columns.
#' @param delim Optional. A delimiter string. Default is \code{"<;>"}.
#'
#' @return A data frame where each sample gets a single row
#' 
#' @export
compressMetadata <- function(meta, 
                             idcols = NULL, 
                             ccols = NULL, 
                             delim = "<;>") {
    
    dir <- system.file("extdata", package = "OmicsMLRepoR")
    if (is.null(idcols)) {
        # Character vector of ID columns
        idcols <- readLines(file.path(dir, "id_columns.txt")) 
    }
    
    if (is.null(ccols)) {
        # Character vector of treatment-related columns (current but not complete) 
        ccols <- readLines(file.path(dir, "treatment_columns.txt"))
    }
    
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
  res <- as.data.frame(cmeta)
  return(res)
}