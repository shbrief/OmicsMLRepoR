#' @include utils.R
NULL

#' Base URL pointing to the count data at the current version
OmicsMLRepo_URL <- single_line_str(
    "/mnt/STORE1/bighome/sehyun/OmicsMLRepo/OmicsMLRepoData/
    curatedMetagenomicData/export_example_alabster"
)

#' Check the existence of a column with all unique values
unique_col_exists <- function(x) {
    any(apply(x, 2, function(x) unique(x) %>% length) == nrow(x))
}

#' Current version of the OmicsMLRepo. This will be incremented when a newer
#' version is released.
COUNTS_VERSION <- "0.1.0"

#' 
#'
#' Get OmicsMLRepo data from curated metadata 
#'
#' @param metadata A data frame containing, at minimum, one 'unique_id' column,
#' This input is a subset of the table obtained from \code{\link{getMetadata}}.
#' @param data_type A character(1) describing a data type. Unless all the 
#' queried samples should have data in this data type, it will give an error.
#' @param cache_directory Optional. A character(1). If provided, it should
#' indicate a local file path where any remotely accessed files should be
#' copied.
#' @param repository
#'
#' @importFrom assertthat assert_that
#'
#'
#'
#' @export
getData <- function(metadata,
                    data_type,
                    cache_directory = get_default_cache_dir(),
                    repository = OmicsMLRepo_URL) {

    ## Convert to an in-memory table, unless some of the dplyr operations will
    ## fail when passed a database connection
    cli_alert_info("Realising metadata.")
    raw_data <- collect(metadata) # all-NA columns are automatically removed
    assert_that(
        inherits(raw_data, "tbl"),
        unique_col_exists(raw_data)
    )
    
    ## Parameter validation
    all_data_type <- raw_data |>
        group_by(curated_fpath) |>
        select(curated_available_data_types) |>
        unique() |>
        ungroup() |> 
        select(-curated_fpath) |> 
        unlist()
    all_data_type <- strsplit(all_data_type, split = ";") %>%
        Reduce(intersect, .)
    data_type %in% all_data_type |>
        all() |>
        assert_that(
            msg = paste("Data type must be a character vector containing", 
                        all_data_type)
        )
    subdirs <- all_data_type
    
    ## Versioned cache directory
    versioned_cache_directory <- file.path(cache_directory, COUNTS_VERSION)
    versioned_cache_directory |> dir.create(
        showWarnings = FALSE,
        recursive = TRUE
    )

    ## Download data
    cli_alert_info("Synchronising files")
    # parsed_repo <- parse_url(repository) #<<< Update with the final data storage location
    # parsed_repo$scheme |>
    #     `%in%`(c("http", "https")) |>
    #     assert_that()
    
    ## [Current Version] Assay data is stored at study-level
    files_to_read <- raw_data |>
        pull(.data$curated_fpath) |> #<<< Update with the final column name
        unique() |>
        as.character() |>
        file.path("assay-1/array.h5")  #<<<<<<<<<< Assuming only one assay + deconstruction using alabaster
    # |> sync_assay_files( #<<<<<<<<<<<< Update/Include once our data has URL; alabaster.base::acquireFile?
    #         url = parsed_repo,
    #         cache_dir = versioned_cache_directory,
    #         files = _,
    #         subdirs = subdirs
    #     )
    
    # ## [Pick this] In case the assay data is stored at sample-level
    # files_to_read <-
    #     raw_data |>
    #     pull(.data$curated_fpath) |> #<<< Update with the final column name
    #     unique() |>
    #     as.character() |>
    #     sync_assay_files(
    #         url = parsed_repo,
    #         cache_dir = versioned_cache_directory,
    #         files = _,
    #         subdirs = subdirs
    #     )
}





#' Synchronises one or more assays with a local copy

#' @param url A character vector of length one. The base HTTP URL from which to
#'   obtain the files.
#' @param cache_dir A character vector of length one. The local filepath to
#'   synchronise files to.
#' @param subdirs A character vector of subdirectories within the root URL to
#'   sync. These correspond to assays.
#' @param files A character vector containing one or more file_id_db entries
#' @returns A character vector consisting of file paths to all the newly
#'   downloaded files
#' @return A character vector of files that have been downloaded
#' @importFrom purrr pmap_chr map_chr
#' @importFrom httr modify_url
#' @importFrom dplyr transmute filter
#' @noRd
#'
sync_assay_files <- function (url = parse_url(OmicsMLRepo_URL),
                              cache_dir,
                              subdirs,
                              files) {

    # Find every combination of file name, sample id, and assay, since each
    # will be a separate file we need to download
    files <- expand.grid(
        filename = c("assays.h5", "se.rds"),
        sample_id = files,
        subdir = subdirs,
        stringsAsFactors = FALSE
    ) |>
        transmute(
            # Path to the file of interest from the root path. We use "/"
            # since URLs must use these regardless of OS
            full_url = paste0(
                url$path,
                "/",
                .data$subdir,
                "/",
                .data$sample_id,
                "/",
                .data$filename
            ) |> map_chr(~ modify_url(url, path = .)),
            
            # Path to save the file on local disk (and its parent directory)
            # We use file.path since the file separator will differ on other OSs
            output_dir = file.path(
                cache_dir,
                .data$subdir,
                .data$sample_id
            ),
            output_file = file.path(
                .data$output_dir,
                .data$filename
            )
        ) |>
        filter(
            # Don't bother downloading files that don't exist TODO: use some
            # kind of hashing to check if the remote file has changed, and
            # proceed with the download if it has. However this is low
            # importance as the repository is not likely to change often
            !file.exists(.data$output_file)
        )
    
    report_file_sizes(files$full_url)
    
    pmap_chr(files, function(full_url, output_dir, output_file) {
        sync_remote_file(full_url, output_file)
        output_file
    }, .progress = list(name = "Downloading files"))
}