#' Expands metadata with multiple values belong to a same attribute
#'
#' Different from the `getWideMetaTb` function, this function accepts multiple
#' target columns (`targetCols`). The difference comes from that the target
#' columns of this function are related, i.e., have the same number of 
#' elements separated by the `delim`, and the multiple values for each column
#' belongs to the same column/attribute/field, i.e., no additional column
#' name is required/provided.
#' 
#' 
#' @param meta A data frame. Metadata table containing all treatment-related 
#' columns
#' @param targetCols Optional. A character vector of column names to expand if 
#' present. Default is the name of all cBioPortal treatment-related columns.
#' @param delim Optional. A character (1) of a delimiter used to separate 
#' multiple values in the metadata table. Default is `<;>`.
#' 
#' @return A data frame of metadata expanded so that each individual treatment 
#' has its own row.
#' 
#' @examples
#' dir <- system.file("extdata", package = "OmicsMLRepoR")
#' meta <- read.csv(file.path(dir, "mini_cbio.csv"), header = TRUE)
#' lmeta <- getLongMetaTb(meta)
#' dim(meta) 
#' dim(lmeta) 
#' 
#' short_tb <- data.frame(
#'     ind = c("A", "B", "C", "D", "E"),
#'     aval = c("cat;dog", "chicken", "horse", "frog;pig", "snake"),
#'     cval = c(1, NA, 3, 4, 5),
#'     bval = c("red;blue", "yellow", "NA", "green;NA", "brown"))
#'     
#' getLongMetaTb(short_tb, c("aval", "bval"), delim = ";")
#' 
#' @export
getLongMetaTb <- function(meta, 
                          targetCols = NULL, 
                          delim = "<;>") {
    
    ## Character vector of treatment-related columns 
    ## Current but not complete <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    if (is.null(targetCols)) {
        dir <- system.file("extdata", package = "OmicsMLRepoR")
        targetCols <- readLines(file.path(dir, "treatment_columns.txt"))
    }
    
    ## Validate input
    stopifnot(is.data.frame(meta),
              is.character(targetCols),
              is.character(delim))
  
    ## Expand data frame
    res <- separate_longer_delim(data = meta,
                                 cols = any_of(targetCols),
                                 delim = delim)
    res[res == "NA"] <- NA
    return(res)
}


#' Compresses expanded metadata columns to one row per sample
#' 
#' @importFrom dplyr reframe across ungroup group_by_at
#' 
#' @param meta A data frame with expanded treatment columns.
#' @param idCols Optional. A character vector of columns that identify 
#' single samples, such as 'curation_id' and 'sampleId'. Defaults to 
#' standard ID columns.
#' @param targetCols Optional. A character vector of columns to compress if 
#' present. Default is names of all cBioPortal treatment-related columns.
#' @param delim Optional. A delimiter string. Default is `<;>`.
#'
#' @return A data frame where each sample gets a single row
#' 
#' @examples
#' dir <- system.file("extdata", package = "OmicsMLRepoR")
#' meta <- read.csv(file.path(dir, "mini_cbio.csv"), header = TRUE)
#' lmeta <- getLongMetaTb(meta)
#' res <- getShortMetaTb(lmeta)
#' dim(res) # 200 x 158 table
#' 
#' long_tb <- data.frame(ind = c("A", "A", "B", "C", "D", "D", "E"),
#'                       aval = c("cat", "dog", "chicken", "horse", 
#'                                "frog", "pig", "snake"),
#'                       cval = c(1, 1, NA, 3, 4, 4, 5),
#'                       bval = c("red", "blue", "yellow", NA, "green", 
#'                                NA, "brown"))
#' getShortMetaTb(long_tb, idCols = "ind", targetCols = c("aval", "bval"))
#' 
#' @export
getShortMetaTb <- function(meta, 
                           idCols = NULL, 
                           targetCols = NULL, 
                           delim = "<;>") {
    
    dir <- system.file("extdata", package = "OmicsMLRepoR")
    if (is.null(idCols)) {
        ## Character vector of ID columns
        idCols <- readLines(file.path(dir, "id_columns.txt")) 
    }
    
    if (is.null(targetCols)) {
        ## Character vector of treatment-related columns 
        ## (current but not complete) 
        targetCols <- readLines(file.path(dir, "treatment_columns.txt"))
    }
    
    ## Validate input
    stopifnot(is.data.frame(meta),
              is.character(idCols),
              is.character(targetCols),
              is.character(delim))
    
    ## Print message to user
    cat("Compressing data frame: this may take a few minutes\n")
    
    ## Get original column order
    col_order <- colnames(meta)
    
    ## Define which columns to group by and leave un-compressed
    gcols <- idCols[which(idCols %in% col_order)]
    ocols <- setdiff(setdiff(col_order, targetCols), idCols)
    
    ## Convert `NA` values to character
    meta[is.na(meta)] <- "NA"
    
    ## Compress data frame
    cmeta <- meta %>%
        group_by_at(gcols) %>%
        reframe(
            across(any_of(targetCols), ~paste(na.omit(.), collapse = delim)),
            across(any_of(ocols), ~unique(.))
        ) %>%
        ungroup() %>%
        select(all_of(col_order))
    
    ## Convert character `NA` as logical
    cmeta[cmeta == "NA"] <- NA
    
    ## Return as data frame
    res <- tibble::as_tibble(cmeta)
    return(res)
}


#' Collapse values from multiple columns into one
#' 
#' @import tidyr
#' 
#' @param meta A data frame.
#' @param newCol A character (1). Name of the new column to store collapsed
#' values.
#' @param targetCols A character vector. Names of the columns to be collapsed
#' into one column.
#' @param sep A character (1). Delimiter used to concatenate column name 
#' and its value. Default is double colons, `:`.
#' @param delim A character(1). Separator to use between values/columns. 
#' Default is `;`.
#' @param remove With the default, `TRUE`, this function will remove input 
#' columns from output data frame.
#' @param na.rm With the default, `TRUE`, missing values will be removed 
#' prior to uniting each value. 
#'
#' @return A data frame where target columns (\code{targetCols}) are collapsed
#' into a single column. The original column name and its value are 
#' concatenated with the `sep` input and the column:value pairs are separated
#' by the `delim` input. Target columns will be merged in the alphabetical 
#' order of their names.
#' 
#' @examples
#' wide_tb <- data.frame(fruit = c("apple", "banana", "pear", "watermelon", 
#'                                 "grape"), 
#'                       shape = c("round", "long", NA, "round", NA),
#'                       color = c("red", "yellow", NA, "green", "purple"),
#'                       size = c("medium", "medium", NA, "large", "small"))
#' getNarrowMetaTb(wide_tb, 
#'                 newCol = "feature", 
#'                 targetCols = c("color", "shape", "size"), 
#'                 sep = ":", delim = ";")
#' 
#' @export
getNarrowMetaTb <- function(meta, 
                            newCol = NULL,
                            targetCols = NULL,
                            sep = ":",
                            delim = ";",
                            remove = TRUE,
                            na.rm = TRUE) {

    if (is.null(targetCols)) {
        msg <- "Provide the name of columns to combine."
        stop(msg)
    }
    
    if (is.null(newCol)) {
        msg <- "Provide the name of column to store collpased values."
        stop(msg)
    }

    target_exist <- targetCols %in% colnames(meta)
    if (sum(target_exist) == 0) {
        msg <- "None of the requested target column(s) exist in the metadata table."
        stop(msg)
    }
    
    cols <- targetCols[target_exist]
    modifiedMeta <- meta
    
    for (col in cols) {
        col_ind <- grep(col, colnames(modifiedMeta))
        modifiedMeta[[col_ind]] <- paste(col,
                                         as.character(modifiedMeta[[col_ind]]),
                                         sep = sep)
    }
    
    ## Help `na.rm = TRUE` formatting: updated characterized NA back to logical NA
    if (isTRUE(na.rm)) {
        modifiedMeta <- apply(modifiedMeta, 2, 
                              function(x) gsub(".*:NA", NA, x)) %>% 
            as.data.frame
    }
    
    ## Unite target columns
    united <- unite(modifiedMeta, 
                    col = {{newCol}}, # embracing operator to inject
                    sort(targetCols), # alphabetical order of the columns
                    sep = delim,
                    remove = remove,
                    na.rm = na.rm)
    
    ## Help `na.rm = TRUE` formatting: update returned empty string to NA
    if (isTRUE(na.rm)) {
        united[united == ""] <- NA
    }
        
    ## Return as a tibble
    res <- tibble::as_tibble(united)
    return(res)
}

#' Create individual columns for different attributes stored in one column
#' 
#' The values stored in one column should include their potential column
#' names to use this function. 
#' 
#' @importFrom stats na.omit
#' 
#' @param meta A data frame.
#' @param targetCol A character. Names of the column whose contents are 
#' exposed as individual columns. Multiple attributes should be separated
#' by the `sep` and the column name and its value should be separated by 
#' the provided `delim`.
#' @param sep A character (1). Delimiter used to concatenate column name 
#' and its value. Default is double colons, `:`.
#' @param delim A character(1). Separator used between values. Default `;`.
#' @param remove If `TRUE`, remove input columns from output data frame.
#'
#' @return A data frame where the contents under `targetCol` is split into
#' individual columns in an alphabetical order. Data type of the expanded 
#' columns is all character.
#' 
#' @examples
#' ## Narrow-table example
#' narrow_tb <- data.frame(fruit = c("apple", "banana", "pear", "watermelon", 
#'                                   "grape"), 
#'                         feature = c("color:red;shape:round;size:medium", 
#'                                     "color:yellow;shape:long;size:medium",
#'                                     "color:brown;shape:NA;size:NA",
#'                                     "color:green;shape:round;size:large",
#'                                     "color:purple;shape:NA;size:small"))
#' getWideMetaTb(narrow_tb, targetCol = "feature", sep = ":", delim = ";")
#' 
#' ## Narrow-table example with missing columns
#' narrow_tb2 <- data.frame(fruit = c("apple", "banana", "pear", 
#'                                    "watermelon", "grape"), 
#'                         feature = c("color:red;shape:round;size:medium", 
#'                                     "color:yellow;shape:long;size:medium",
#'                                     NA,
#'                                     "color:green;size:large",
#'                                     "color:purple;shape:NA;size:small"))
#' getWideMetaTb(narrow_tb2, targetCol = "feature", sep = ":", delim = ";")
#' 
#' @export
getWideMetaTb <- function(meta, 
                          targetCol = NULL, 
                          sep = ":",
                          delim = ";",
                          remove = TRUE) {
    
    if (is.null(targetCol)) {
        msg <- "Provide the name of columns to be expanded."
        stop(msg)
    }
    
    if (!targetCol %in% colnames(meta)) {
        msg <- "The target column doesn't exist in the metadata table."
        stop(msg)
    }
    
    cols <- targetCol[targetCol %in% colnames(meta)]
    
    embeddedColNames <- meta[[targetCol]] %>%
        strsplit(split = paste0(sep, "|", delim)) %>%
        lapply(function(x) {x[c(TRUE, FALSE)]}) 
    
    ## The number of elements in each row
    embeddedColNums <- sapply(embeddedColNames, length) 
    
    ## Alphabetical ordering of all the unique columns
    newColNames <- unique(unlist(embeddedColNames)) %>% na.omit %>% sort 
    base <- paste0(newColNames, sep, "NA") %>%
        paste0(collapse = delim)
    
    ## Rows need to be filled with NAs
    rowToFillInd <- which(sapply(embeddedColNames, 
                                 function(x) any(!newColNames %in% x)))
    
    ## Add NA-placeholder for non-existing columns 
    for (ind in rowToFillInd) {
        updatedVal <- merge_vectors(base, 
                                    update = meta[[ind, targetCol]], 
                                    sep = sep, delim = delim)
        meta[ind, targetCol] <- updatedVal
    }

    res <- meta %>%
        separate_wider_delim(targetCol,
                             delim = delim,
                             names = newColNames,
                             cols_remove = remove)
    
    ## Remove column names in values
    for (newColName in newColNames) {
        res[newColName] <- gsub(paste0(newColName, sep), 
                                "", res[[newColName]], fixed = TRUE)
    } 
    res[res == "NA"] <- NA
    
    return(res)
}