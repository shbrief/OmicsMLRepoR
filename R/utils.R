#' Formats a multi-line string as it it were on one line
#'
#' @importFrom stringr str_remove_all
#' @param text Any character vector
#' @return The same character vector with newlines and subsequent 
#' whites-paces removed
#'
#' @keywords internal
single_line_str <- function(text) {
    stringr::str_remove_all(text, r"(\n\s*)")
}


#' Check that a character doesn't match any non-letter
#' @param x A character(1).
letters_only <- function(x) !grepl("[^A-Za-z]", x)


#' Check that a character doesn't match any non-number
#' @param x A character(1).
numbers_only <- function(x) !grepl("\\D", x)


#' Extract ontology from the ontology term ids
#'
#' @param terms A character vector
#' @param delim A character. Delimiter between ontology and its id.
#' Default is `:`.
#'
#' @examples
#' terms <- c("HP:0001824", "MONDO:0010200", "NCIT:C122328")
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
    base_dict <- setNames(sapply(base_pairs, 
                                 function(x) ifelse(length(x) == 2, x[2], NA)), 
                          sapply(base_pairs, 
                                 function(x) ifelse(length(x) == 2, x[1], NA)))
    
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
