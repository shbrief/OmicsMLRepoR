# Formats a multi-line string as it it were on one line
#
# @importFrom stringr str_remove_all
# @param text Any character vector
# @return The same character vector with newlines and subsequent 
# whites-paces removed
#
# @keywords internal
single_line_str <- function(text) {
    stringr::str_remove_all(text, r"(\n\s*)")
}


# Check that a character doesn't match any non-letter
# @param x A character(1).
letters_only <- function(x) !grepl("[^A-Za-z]", x)


# Check that a character doesn't match any non-number
# @param x A character(1).
numbers_only <- function(x) !grepl("\\D", x)


#' Extract ontology from the ontology term ids
#'
#' @param terms A character vector
#' @param delim A character. Delimiter between ontology and its id.
#' Default is `:`.
#' 
#' @return A character vector containing the ontology names of the input 
#' `terms`. The length of this is same as the `terms` input. 
#'
#' @examples
#' terms <- c("HP:0001824", "MONDO:0010200", "NCIT:C122328", "4471000175100")
#' get_ontologies(terms = terms)
#'
#' @export
get_ontologies <- function(terms, delim = ":") {
    
    ontologies <- c()
    for (i in seq_along(terms)) {
        onto <- strsplit(terms[i], delim)[[1]][1]
        isSNOMED <- letters_only(onto)
        if (isFALSE(isSNOMED)) {onto <- "SNOMED"}
        ontologies[i] <- onto
    }
    return(ontologies)
}


#' Custom function to merge vectors 
#' 
#' This function is designed for a group of, collapsible metadata attributes
#' (e.g., 'biomarker' for curatedMetagenomicData).
#' 
#' @importFrom stats setNames
#' 
#' @param base A character. A space-holder version of the key:value 
#' concatenates (e.g., `column1:NA;column2:NA;column3:NA`)
#' @param update A character. The target string to be compared and filled with 
#' `base` if there is missing pairs. (e.g., `column1:value1;column3:value3`) 
#' @param sep A character string to separate the column name and value. 
#' Default is `:`
#' @param delim A character string to separate the column:value pairs. 
#' Default is `;`
#' 
#' @return A character updated the target string (`update`) to follow the
#' reference string (`base`).
#' 
#' @examples
#' x <- "color:NA;shape:NA;size:NA"
#' y <- "color:green;size:large"
#' merge_vectors(x, y)
#' 
#' @export
merge_vectors <- function(base, update, sep = ":", delim = ";") {
    
    ## Split the vectors into key-value pairs
    base_pairs <- strsplit(strsplit(base, delim)[[1]], sep)
    update_pairs <- strsplit(strsplit(update, delim)[[1]], sep)
    
    ## Create a dictionary from base pairs
    base_dict <- setNames(vapply(base_pairs, 
                                 function(x) ifelse(length(x) == 2, x[2], NA),
                                 character(1)), 
                          vapply(base_pairs, 
                                 function(x) ifelse(length(x) == 2, x[1], NA),
                                 character(1)))
    
    ## Update the base dictionary with values from update pairs
    for (pair in update_pairs) {
        if (length(pair) == 2) {
            base_dict[pair[1]] <- pair[2]
        }
    }
    
    ## Recreate the merged vector
    merged_vector <- paste(names(base_dict), base_dict, sep = sep)  
    res <- paste0(merged_vector, collapse = delim)
    return(res)
}


## Get target database information
.getTargetDB <- function(meta) {
    targetDB <- unique(meta$package)
    if (is.null(targetDB)) {message("`meta` table doesn't include `targetDB` information.")}
    return(targetDB)
}


# Get delimiter
# @importFrom utils read.csv

.getDelimiter <- function(meta, targetCol, delim) {
    
    targetDB <- .getTargetDB(meta)
    
    ## Extract the delimiter
    if (is.null(delim) & is.null(targetDB)) {
        stop("Provide the `delim` input")
    } else if (is.null(delim)) {
        ## Load data dictionary
        dir <- system.file("extdata", package = "OmicsMLRepoR")
        fname <- paste0(targetDB, "_data_dictionary.csv")
        dd <- read.csv(file.path(dir, fname), header = TRUE)
        
        ## Get the delimiter(s)
        colInd <- which(dd$col.name %in% targetCol)
        delim <- dd$delimiter[colInd] %>% unique
    }
    
    if (!length(delim)) {stop("The targetCol using different delimiter. Process one at a time.")}

    return(delim)
}


# Get separater
# @importFrom utils read.csv

.getSeparater <- function(meta, targetCol) {
    
    targetDB <- .getTargetDB(meta)
    
    ## Load data dictionary
    dir <- system.file("extdata", package = "OmicsMLRepoR")
    fname <- paste0(targetDB, "_data_dictionary.csv")
    dd <- read.csv(file.path(dir, fname), header = TRUE)
    
    ## Get the separater(s)
    colInd <- which(dd$col.name %in% targetCol)
    seperater <- dd$separater[colInd] %>% unique
    
    if (length(seperater) > 1) {
        stop("The targetCol using different separater. Process one at a time.")
    }
    
    return(seperater)
}

# Get ontology database(s)
# @importFrom utils read.csv

.getOntos <- function(meta, targetCol) {
    
    targetDB <- .getTargetDB(meta)
    
    ## Extract the delimiter
    ## Load data dictionary
    dir <- system.file("extdata", package = "OmicsMLRepoR")
    fname <- paste0(targetDB, "_data_dictionary.csv")
    dd <- read.csv(file.path(dir, fname), header = TRUE)
    
    ## Get the delimiter(s)
    colInd <- which(dd$col.name %in% targetCol)
    ontos <- dd$ontoDB[colInd] %>% unique %>% .[!is.na(.)]
    split_ontos <- unlist(strsplit(ontos, "\\|"))
    
    if (is.null(split_ontos)) {stop("The targetCol do not have listed ontology databases.")}
    
    return(split_ontos)
}

## Convert "NA" to `NA`
.charToLogicNA <- function(tb) {
    timeVar <- sapply(tb, lubridate::is.POSIXct)
    if (any(timeVar)) {
        ## Handle `POSIXct` separately
        timeInd <- which(timeVar)
        tb_sub <- tb[,-timeInd]
        tb_sub[tb_sub == "NA"] <- NA
        res <- cbind(tb_sub, tb[timeInd])
    } else {
        tb[tb == "NA"] <- NA
        res <- tb
    }
    return(res)
}

## Include associated attributes
.getAssociatedAttr <- function(dat, attrName) {
    attrName_onto <- c(attrName, paste0(attrName, "_ontology_term_id"))
    res_onto <- intersect(attrName_onto, colnames(dat))
    
    attrName_unit <- c(attrName, paste0(attrName, "_unit"))
    res_unit <- intersect(attrName_unit, colnames(dat))
    
    resAll <- c(res_onto, res_unit) %>% unique
    return(resAll)
}


