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
    as.integer(gsutil_stat(gss)$`Content-Length`) / 10^9
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

    "Downloading {length(urls)} file{?s}, totalling {total_size} GB" |>
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
