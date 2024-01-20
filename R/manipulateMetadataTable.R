#' Expands metadata with multiple values belong to a same attribute
#' 
#' @importFrom tidyr separate_longer_delim
#' 
#' @param meta A data frame. Metadata table containing all treatment-related 
#' columns
#' @param targetCols Optional. A character vector of column names to expand if 
#' present. Default is the name of all cBioPortal treatment-related columns.
#' @param delim Optional. A character (1) of a delimiter used to separate 
#' multiple values in the metadata table. Default is \code{"<;>"}.
#' 
#' @return A data frame of metadata expanded so that each individual treatment 
#' has its own row.
#' 
#' @export
longMetadata <- function(meta, 
                         targetCols = NULL, 
                         delim = "<;>") {
    
    # Character vector of treatment-related columns 
    # Current but not complete <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    if (is.null(targetCols)) {
        dir <- system.file("extdata", package = "OmicsMLRepoR")
        targetCols <- readLines(file.path(dir, "treatment_columns.txt"))
    }
    
    # Validate input
    stopifnot(is.data.frame(meta),
              is.character(targetCols),
              is.character(delim))
  
    # Expand data frame
    res <- separate_longer_delim(data = meta,
                                 cols = any_of(targetCols),
                                 delim = delim)
    return(res)
}


#' Compresses expanded metadata columns to one row per sample
#' 
#' @importFrom dplyr group_by reframe ungroup select
#' 
#' @param meta A data frame with expanded treatment columns.
#' @param idCols Optional. A character vector of columns that identify 
#' single samples, such as 'curation_id' and 'sampleId'. Defaults to 
#' standard ID columns.
#' @param targetCols Optional. A character vector of columns to compress if 
#' present. Default is names of all cBioPortal treatment-related columns.
#' @param delim Optional. A delimiter string. Default is \code{"<;>"}.
#'
#' @return A data frame where each sample gets a single row
#' 
#' @export
shortMetadata <- function(meta, 
                          idCols = NULL, 
                          targetCols = NULL, 
                          delim = "<;>") {
    
    dir <- system.file("extdata", package = "OmicsMLRepoR")
    if (is.null(idCols)) {
        # Character vector of ID columns
        idCols <- readLines(file.path(dir, "id_columns.txt")) 
    }
    
    if (is.null(targetCols)) {
        # Character vector of treatment-related columns (current but not complete) 
        targetCols <- readLines(file.path(dir, "treatment_columns.txt"))
    }
    
    # Validate input
    stopifnot(is.data.frame(meta),
              is.character(idCols),
              is.character(targetCols),
              is.character(delim))
    
    # Print message to user
    cat("Compressing data frame: this may take a few minutes\n")
    
    # Get original column order
    col_order <- colnames(meta)
    
    # Define which columns to group by and leave un-compressed
    gcols <- idCols[which(idCols %in% col_order)]
    ocols <- setdiff(setdiff(col_order, targetCols), idCols)
    
    # Compress data frame
    cmeta <- meta %>%
        group_by_at(gcols) %>%
        reframe(
            across(any_of(targetCols), ~paste(na.omit(.), collapse = delim)),
            across(any_of(ocols), ~unique(.))
        ) %>%
        ungroup() %>%
        select(all_of(col_order))
    
    # Return as data frame
    res <- as.data.frame(cmeta)
    return(res)
}


#' Collapse values from multiple columns into one
#' 
#' @importFrom dplyr group_by reframe ungroup select
#' 
#' @param meta A data frame.
#' @param targetCols A character vector. Names of the columns to be collapsed
#' into one column.
#' @param newCol A character (1). Name of the new column to store collapsed
#' values. 
#' @param sep A character(1). Separator to use between values. Default is `<;>`.
#' @param delim A character (1). Delimiter to concatenate column name and its
#' value. Default is double colons, `::`.
#' @param remove If `TRUE`, remove input columns from output data frame.
#'
#' @return A data frame where target columns (\code{targetCols}) are collapsed
#' into a single column. The original column name and its value are 
#' concatenated with double colons (`::`).
#' 
#' @export
narrowMetadata <- function(meta, 
                           targetCols = NULL, 
                           newCol = NULL,
                           sep = "<;;>",
                           delim = "::",
                           remove = TRUE) {

    if (is.null(targetCols)) {
        msg <- "Provide the name of columns to combine."
        stop(msg)
    }
    
    if (is.null(newCol)) {
        msg <- "Provide the name of column to store collpased values."
        stop(msg)
    }

    cols <- targetCols[targetCols %in% colnames(meta)]
    modifiedMeta <- meta
    for (col in cols) {
        modifiedMeta[[col]] <- paste(col, 
                                     as.character(modifiedMeta[[col]]), 
                                     sep = delim)
    }
    
    united <- unite(modifiedMeta, col = {{newCol}}, sep = sep) ## embracing operator to inject
    
    if (isTRUE(remove)) {
        ind <- which(colnames(meta) %in% cols)
        res <- cbind(meta[-ind], united)
    } else {
        res <- cbin(meta, united)
    }
        
    # Return as a tibble
    res <- tibble::as_tibble(res)
    return(res)
}

#' Create individual columns for different attributes stored in one column
#' 
#' @importFrom dplyr group_by reframe ungroup select
#' 
#' @param meta A data frame.
#' @param targetCols A character vector. Names of the column(s) whose contents
#' are exposed as individual columns. Multiple attributes should be separated
#' by the `sep` and the column name and its value should be separated by the
#' provided `delim`.
#' @param sep A character(1). Separator used between values. Default is `<;>`.
#' @param delim A character (1). Delimiter used to concatenate column name 
#' and its value. Default is double colons, `::`.
#' @param remove If `TRUE`, remove input columns from output data frame.
#'
#' @return A data frame where different attributes collapsed into the target 
#' column(s) (\code{targetCols}) are exposed as individual columns.
#' 
#' @export
wideMetadata <- function(meta, 
                         targetCols = NULL, 
                         sep = "<;;>",
                         delim = "::",
                         remove = TRUE) {
    
    if (is.null(targetCols)) {
        msg <- "Provide the name of columns to be expanded."
        stop(msg)
    }
    
    cols <- targetCols[targetCols %in% colnames(meta)]
    modifiedMeta <- meta
    
    for (col in cols) {
        modifiedMeta[[col]] <- paste(col, 
                                     as.character(modifiedMeta[[col]]), 
                                     sep = delim)
    }
    
    united <- unite(modifiedMeta, col = {{newCol}}, sep = sep) ## embracing operator to inject
    
    if (isTRUE(remove)) {
        ind <- which(colnames(meta) %in% cols)
        res <- cbind(meta[-ind], united)
    } else {
        res <- cbin(meta, united)
    }
    
    # Return as a tibble
    res <- tibble::as_tibble(res)
    return(res)
}