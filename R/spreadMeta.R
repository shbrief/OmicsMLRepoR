#' Expands metadata with multiple values belong to a same attribute
#'
#' Same as the `getWideMetaTb` function, this function accepts single
#' target column (`targetCol`). The target columns (and linked, accessory
#' columns) should have the same number of elements separated by the `delim`, 
#' and the multiple values for each column belongs to the same column/
#' attribute/field, i.e., no additional column name is required/provided.
#' 
#' @importFrom tidyr separate_longer_delim 
#' 
#' @param meta A data frame. Each column (and associated `ontology_term_id`
#' column) should use the same delimiter to separate multiple, same-numbered
#' values.
#' @param targetCol A character (1). The column name to expand if present. 
#' @param delim Optional. A character (1) of a delimiter used to separate 
#' multiple values in the metadata table. 
#' 
#' @return A data frame of metadata expanded so that each individual treatment 
#' has its own row.
#' 
#' @examples
#' data(mini_cmd)
#' lmeta <- getLongMetaTb(mini_cmd, "hla")
#' dim(mini_cmd) 
#' dim(lmeta) 
#' 
#' data(mini_cmd2)
#' lmeta2 <- getLongMetaTb(mini_cmd2, "target_condition")
#' head(lmeta2, 3)
#' 
#' data(mini_cbio)
#' trt_cols <- grep("^treatment_", colnames(mini_cbio), value = TRUE)
#' lmeta3 <- getLongMetaTb(mini_cbio, targetCol = trt_cols)
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
                          targetCol = NULL, 
                          delim = NULL) {
    
    ## Include associated attributes (e.g., `_ontology_term_id` or `_unit`) if exist
    targetCol <- .getAssociatedAttr(meta, targetCol)
    
    ## Extract the delimiter
    delim <- .getDelimiter(meta, targetCol, delim)
    
    ## Expand data frame
    res <- tidyr::separate_longer_delim(data = meta,
                                        cols = any_of(targetCol),
                                        delim = delim)
    
    ## Convert "NA" to `NA`
    res_all <- .charToLogicNA(res)
    
    return(res_all)
}

#' Create individual columns for different attributes stored in one column
#' 
#' The values stored in one column should include their potential column
#' names to use this function. 
#' 
#' @importFrom stats na.omit
#' 
#' @param meta A data frame.
#' @param targetCol A character (1). The column name to expand if present. 
#' Multiple attributes should be separated by the `sep` and the column name 
#' and its value should be separated by the provided `delim`.
#' @param sep A character (1). Delimiter used to concatenate column name 
#' and its value. Default is double colons, `:`.
#' @param delim A character(1). Separator used between values. Default `<;>`.
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
#' ## Subset of cMD metadata
#' data(mini_cmd3)
#' wtb <- getWideMetaTb(mini_cmd3, targetCol = "probing_pocket_depth")
#' head(wtb)
#' 
#' @export
getWideMetaTb <- function(meta, 
                          targetCol = NULL, 
                          sep = ":",
                          delim = "<;>",
                          remove = TRUE) {
    
    embeddedColNames <- meta[[targetCol]] %>%
        strsplit(split = paste0(sep, "|", delim)) %>%
        lapply(function(x) {x[c(TRUE, FALSE)]}) 
    
    ## The number of elements in each row
    embeddedColNums <- vapply(embeddedColNames, length, integer(1)) 
    
    ## Alphabetical ordering of all the unique columns
    newColNames <- unique(unlist(embeddedColNames)) %>% na.omit %>% sort 
    base <- paste0(newColNames, sep, "NA") %>%
        paste0(collapse = delim)
    
    ## Rows need to be filled with NAs
    rowToFillInd <- which(vapply(embeddedColNames, 
                                 function(x) any(!newColNames %in% x),
                                 FUN.VALUE = logical(1)))
    
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
    
    ## Convert "NA" to `NA`
    res_all <- .charToLogicNA(res)
    
    return(res_all)
}


#' Manipulate metadata into a spread format
#' 
#' This function speads gout multiple values per attribute (‘multi-valued’ 
#' attribute) or multiple features under a single generic attribute 
#' (‘composite’ attribute). Multi-valued attribute will return 'long' table, 
#' while composite attribute will return 'wide' table. 
#' 
#' @param meta A data frame. Harmonized metadata available through the
#' OmicsMLRepoR package. It should have the `package` column.
#' @param targetCol A character (1). The column name to expand if present.
#' 
#' @returns 
#' A metadata table where the `targetCol` is updated to a long form (if it
#' is multi-valued attribute) or spread into multiple columns (if it is 
#' composite attribute).
#' 
#' @examples
#' data(mini_cmd2) # multi-valued attribute
#' data(mini_cmd3) # composite attribute
#' spreadMeta(mini_cmd2, "target_condition")
#' spreadMeta(mini_cmd3, "probing_pocket_depth")
#' 
#' data(mini_cbio)
#' trt_cols <- grep("^treatment_", colnames(mini_cbio), value = TRUE)
#' spreadMeta (mini_cbio, targetCol = trt_cols)
#' 
#' @export
spreadMeta <- function(meta, targetCol) {
    
    ## Validate input
    if (is.null(targetCol)) {
        stop("Provide the name of columns to be spread.")
    }
    if (!is.data.frame(meta)) {
        stop("`meta` input should be a data frame.")
    }
    if (!is.character(targetCol)) {
        stop("`targetCol` input is missing.")
    } else if (!any(targetCol %in% colnames(meta))) {
        stop("The target column doesn't exist in the metadata table.")
    }
    
    ## Extract the delimiter and separater
    delim <- .getDelimiter(meta, targetCol)
    sep <- .getSeparater(meta, targetCol)
    
    if (is.na(sep)) { # multi-valued attribute
        targetCol <- .getAssociatedAttr(meta, targetCol) # Include associated attributes (e.g., `_ontology_term_id` or `_unit`) if exist
        res <- getLongMetaTb(meta, targetCol, delim = delim)
    } else { # composite attribute
        ## [Todo] Handling associated attributes (e.g., `_ontology_term_id`) for composite attribute <<<<<<<<<
        res <- getWideMetaTb(meta, targetCol, sep = sep, delim = delim)
    }
    
    return(res)
}
