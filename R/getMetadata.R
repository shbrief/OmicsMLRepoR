#' @include utils.R
NULL

#' Environment that we use to cache the DuckDB connections
cache <- rlang::env(metadata_table = rlang::env()) #<<<<<<<<<<<<<<<<<<<<<<<<<<<?

DATABASE_URL <- single_line_str( #<<<<<<<<<<<<<<<<<<<<<<< Why they structured this way?
    "gs://omics_ml_repo/cMD_curated_sampleMetadata.parquet"
)

#' Gets the harmonized metadata as a data frame
#'
#' Downloads a parquet database of the harmonized omics metadata deposited in
#' Bioconductor's ExperimentHub.
#'
#' @importFrom DBI dbConnect
#' @importFrom duckdb duckdb
#' @importFrom dplyr tbl
#' @importFrom httr progress
#' @importFrom cli cli_alert_info hash_sha256
#'
#' @param remote_url Optional character vector of length 1, containing the
#' location of metadata as an HTTP URL or a Google bucket address.
#' @param cache_directory Optional character vector of length 1. A file path
#' on your local system to a directory (not a file) that will be used to store
#' the metadata parquet file.
#' @param use_cache Logical. If \code{TRUE} (the default) and this function
#' has been called before with the same parameters, a cached reference to the
#' table will be returned. If \code{FALSE}, a new connection will be created.
#'
#' @return A lazy data.frame subclass containing the metadata. You can interact
#' with this object using most standard dplyr functions. For string matching,
#' it is recommended that you use \code{\link[stringr]{str_like}} to filter
#' character columns, as \code{\link[stringr]{str_match}} will not work.
#'
#' @export
getMetadata <- function(remote_url = DATABASE_URL,
                        cache_directory = get_default_cache_dir(),
                        use_cache = TRUE) {
    hash <- c(remote_url, cache_directory) |>
        paste0(collapse = "") |>
        hash_sha256()
    cached_connection <- cache$metadata_table[[hash]]

    if (!is.null(cached_connection) && isTRUE(use_cache)) {
        cached_connection
    } else {
        report_file_sizes(remote_url)
        db_path <- file.path(cache_directory, "cMD_metadata.parquet")
        sync_remote_file(
            remote_url,
            db_path,
            progress(type = "down", con = stderr())
        )
        table <- duckdb() |>
            dbConnect(drv = _, read_only = TRUE) |>
            tbl(db_path)
        cache$metadata_table[[hash]] <- table
        table
    }
}


