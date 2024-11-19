#' Compresses expanded metadata columns to one row per sample
#' 
#' @importFrom dplyr reframe across ungroup group_by_at
#' 
#' @param meta A data frame with expanded treatment columns.
#' @param idCols Optional. A character vector of columns that identify 
#' single samples, such as 'curation_id' and 'sampleId'. Defaults to 
#' standard ID columns.
#' @param targetCol Optional. A character vector of columns to compress if 
#' present. Default is names of all cBioPortal treatment-related columns.
#' @param delim Optional. A delimiter string. Default is `<;>`.
#'
#' @return A data frame where each sample gets a single row
#' 
#' @examples
#' data(mini_cmd)
#' lmeta <- getLongMetaTb(mini_cmd, "hla")
#' res <- getShortMetaTb(lmeta, targetCol = "hla")
#' dim(res) # 200 x 3 table
#' 
#' long_tb <- data.frame(ind = c("A", "A", "B", "C", "D", "D", "E"),
#'                       aval = c("cat", "dog", "chicken", "horse", 
#'                                "frog", "pig", "snake"),
#'                       cval = c(1, 1, NA, 3, 4, 4, 5),
#'                       bval = c("red", "blue", "yellow", NA, "green", 
#'                                NA, "brown"))
#' getShortMetaTb(long_tb, idCols = "ind", targetCol = c("aval", "bval"))
#' 
#' @export
getShortMetaTb <- function(meta, 
                           idCols = NULL, 
                           targetCol = NULL, 
                           delim = "<;>") {
    
    dir <- system.file("extdata", package = "OmicsMLRepoR")
    if (is.null(idCols)) {
        ## Character vector of ID columns
        idCols <- readLines(file.path(dir, "id_columns.txt")) 
    }
    
    if (is.null(targetCol)) {
        ## Character vector of treatment-related columns 
        ## (current but not complete) 
        targetCol <- readLines(file.path(dir, "treatment_columns.txt"))
    }
    
    ## Validate input
    stopifnot(is.data.frame(meta),
              is.character(idCols),
              is.character(targetCol),
              is.character(delim))
    
    ## Print message to user
    message("Compressing data frame: this may take a few minutes\n")
    
    ## Get original column order
    col_order <- colnames(meta)
    
    ## Include associated attributes (e.g., `_ontology_term_id` or `_unit`) if exist
    targetCol <- .getAssociatedAttr(meta, targetCol)
    
    ## Define which columns to group by and leave un-compressed
    gcols <- idCols[which(idCols %in% col_order)]
    ocols <- setdiff(setdiff(col_order, targetCol), idCols) # keep original form
    
    ## Convert `NA` values to character
    meta[is.na(meta)] <- "NA"
    
    ## Compress data frame
    cmeta <- meta %>%
        group_by_at(gcols) %>%
        reframe(
            across(any_of(targetCol), ~paste(na.omit(.), collapse = delim)),
            across(any_of(ocols), ~unique(.))
        ) %>%
        ungroup() %>%
        select(all_of(col_order))
    
    ## Return as data frame
    res <- tibble::as_tibble(cmeta)
    
    ## Convert "NA" to `NA`
    res_all <- .charToLogicNA(res)
    
    return(res)
}


#' Collapse values from multiple columns into one
#' 
#' @import tidyr
#' 
#' @param meta A data frame.
#' @param newCol A character (1). Name of the new column to store collapsed
#' values.
#' @param targetCol A character vector. Names of the columns to be collapsed
#' into one column.
#' @param sep A character (1). Delimiter used to concatenate column name 
#' and its value. Default is double colons, `:`.
#' @param delim A character(1). Separator to use between values/columns. 
#' Default is `;`.
#' @param remove With the default, `TRUE`, this function will remove input 
#' columns from output data frame.
#' @param na.rm With the default, `TRUE`, missing values will be removed 
#' prior to uniting each value. 
#' @param sort With the default, `TRUE`, the united columns will be ordered
#' alphabetically.
#'
#' @return A data frame where target columns (\code{targetCol}) are collapsed
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
#'                 targetCol = c("color", "shape", "size"), 
#'                 sep = ":", delim = ";")
#' 
#' @export
getNarrowMetaTb <- function(meta, 
                            newCol = NULL,
                            targetCol = NULL,
                            sep = ":",
                            delim = ";",
                            remove = TRUE,
                            na.rm = TRUE,
                            sort = TRUE) {
    
    if (is.null(targetCol)) {
        msg <- "Provide the name of columns to combine."
        stop(msg)
    }
    
    if (is.null(newCol)) {
        msg <- "Provide the name of column to store collpased values."
        stop(msg)
    }
    
    target_exist <- targetCol %in% colnames(meta)
    if (sum(target_exist) == 0) {
        msg <- "None of the requested target column(s) exist in the metadata table."
        stop(msg)
    }
    
    cols <- targetCol[target_exist]
    modifiedMeta <- meta
    
    for (col in cols) {
        colregex <- paste0("^", col, "$")
        col_ind <- grep(colregex, colnames(modifiedMeta))
        modifiedMeta[[col_ind]] <- paste(col,
                                         as.character(modifiedMeta[[col_ind]]),
                                         sep = sep)
    }
    
    ## Help `na.rm = TRUE` formatting: updated characterized NA back to logical NA
    na_str <- paste0(".*", sep, "NA")
    if (isTRUE(na.rm)) {
        modifiedMeta <- apply(modifiedMeta, 2, 
                              function(x) gsub(na_str, NA, x)) %>% 
            as.data.frame
    }
    
    ## Unite target columns
    if (isTRUE(sort)) {targetCol <- sort(targetCol)} # alphabetical order of the columns
    united <- unite(modifiedMeta, 
                    col = {{newCol}}, # embracing operator to inject
                    targetCol, 
                    sep = delim,
                    remove = remove,
                    na.rm = na.rm)
    
    ## Help `na.rm = TRUE` formatting: update returned empty string to NA
    if (isTRUE(na.rm)) {
        united[united == ""] <- NA
    }
    
    ## Return as a tibble
    res <- tibble::as_tibble(united)
    
    ## Convert "NA" to `NA`
    res_all <- .charToLogicNA(res)
    return(res)
}