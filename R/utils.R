## Utility scripts that are used internally by the package at runtime
## Copied from https://github.com/shbrief/CuratedAtlasQueryR/blob/master/R/utils.R

#' Gets the file size of a number of remote files
#'
#' @importFrom purrr map_dbl
#' @importFrom httr HEAD
#'
#' @param urls A character vector containing URLs
#'
#' @return The file size of each of the files pointed to by the provided URL,
#' in gigabytes, as double vector
#'
#' @keywords internal
url_file_size <- function(urls) {
    map_dbl(urls, function(url) {
        as.integer(HEAD(url)$headers$`content-length`) / 10^9
    })
}

#' Gets the file size of a number of remote files
#'
#' @importFrom purrr map_dbl
#' @importFrom httr HEAD
#' @importFrom AnVIL gsutil_stat
#'
#' @param gss A character vector containing Google bucket file location(s)
#'
#' @return The file size of each of the files pointed to by the provided
#' Google bucket address, in gigabytes, as double vector
#'
#' @keywords internal
gs_file_size <- function(gss) {
    as.integer(gsutil_stat(gss)$`Content-Length`) / 10^6
}

#' Prints a message indicating the size of a download
#'
#' @importFrom cli cli_alert_info
#'
#' @param urls A character vector containing the location of metadata. All
#' should be HTTP URLs or Google bucket addresses - not accepting any mix of
#' them currently.
#'
#' @return `NULL`, invisibly
#' @keywords internal
#'
report_file_sizes <- function(urls) {

    if (all(grepl("^gs://*", urls))) { # Data from Google Cloud Bucket
        total_size <- gs_file_size(urls) |>
            sum() |>
            round(digits=2)
    } else { # Data from HTTP URLs
        total_size <- url_file_size(urls) |>
            sum() |>
            round(digits=2)
    }

    "Downloading {length(urls)} file{?s}, totalling {total_size} MB" |>
        cli_alert_info()

    invisible(NULL)
}

#' Formats a multi-line string as it it were on one line
#'
#' @importFrom stringr str_remove_all
#' @param text Any character vector
#' @return The same character vector with newlines and subsequent whitespaces removed
#'
#' @keywords internal
single_line_str <- function(text) {
    str_remove_all(text, r"(\n\s*)")
}

#' Returns the default cache directory
#'
#' @importFrom tools R_user_dir
#' @importFrom utils packageName
#'
#' @return A length one character vector
#'
#' @keywords internal
get_default_cache_dir <- function() {
    R_user_dir("OmicsMLRepoR", "cache") |>
        normalizePath() |>
        suppressWarnings()
}


#' Synchronizes a single remote file with a local path
#'
#' @importFrom httr write_disk GET stop_for_status
#' @importFrom cli cli_abort cli_alert_info
#' @importFrom AnVIL gsutil_cp
#'
#' @param full_url A character vector containing the location of metadata. All
#' should be HTTP URLs or Google bucket addresses - not accepting any mix of
#' them currently.
#' @param output_file File path to the cached data.
#'
#' @return `NULL`, invisibly
#' @keywords internal
#'
sync_remote_file <- function(full_url, output_file, ...) {
    user_over <- NA
    if (file.exists(output_file)) {
      user_over <- tolower(readline(prompt = "Cached file already exists. Overwrite? (Y/N):"))
    }
    if (!file.exists(output_file) | user_over == "y") {
        output_dir <- dirname(output_file)
        dir.create(output_dir,
                   recursive = TRUE,
                   showWarnings = FALSE
        )
        cli_alert_info("Downloading {full_url} to {output_file}")

        tryCatch(
            if (grepl("^gs://*", full_url)) {
                gsutil_cp(full_url, output_file)
            } else {
                GET(full_url, write_disk(output_file, overwrite = TRUE), ...) |> stop_for_status()
            },
            error = function(e) {
                # Clean up if we had an error
                file.remove(output_file)
                cli_abort("File {full_url} could not be downloaded. {e}")
            }
        )
    }
    invisible(NULL)
}


#' Check that a character doesn't match any non-letter
#' @param x A character(1).
letters_only <- function(x) !grepl("[^A-Za-z]", x)

#' Check that a character doesn't match any non-number
#' @param x A character(1).
numbers_only <- function(x) !grepl("\\D", x)

#' Extract ontology from the ontology term id
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
#' @param base A character. A space-holder version of the key:value 
#' concatenates (e.g., `column1:NA;column2:NA;column3:NA`)
#' @param update A character. The target string to be compared and filled with 
#' `base` if there is missing pairs. (e.g., `column1:value1;column3:value3`) 
#' @param sep A character string to separate the column name and value. 
#' Default is `:`
#' @param delim A character string to separate the column:value pairs. 
#' Default is `;`
#' 
#' @examples
#' x <- "color:NA;shape:NA;size:NA"
#' y <- "color:green;size:large"
#' merge_vectors(x, y)
#' 
merge_vectors <- function(base, update, sep = ":", delim = ";") {
    # Split the vectors into key-value pairs
    base_pairs <- strsplit(strsplit(base, ";")[[1]], ":")
    update_pairs <- strsplit(strsplit(update, ";")[[1]], ":")
    
    # Create a dictionary from base pairs
    base_dict <- setNames(sapply(base_pairs, 
                                 function(x) ifelse(length(x) == 2, x[2], NA)), 
                          sapply(base_pairs, 
                                 function(x) ifelse(length(x) == 2, x[1], NA)))
    
    # Update the base dictionary with values from update pairs
    for (pair in update_pairs) {
        if (length(pair) == 2) {
            base_dict[pair[1]] <- pair[2]
        }
    }
    
    # Recreate the merged vector
    merged_vector <- paste(names(base_dict), base_dict, sep = sep)  
    res <- paste0(merged_vector, collapse = delim)
    return(res)
}
