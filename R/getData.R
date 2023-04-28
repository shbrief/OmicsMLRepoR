#' @include utils.R
NULL

#' Temporary local directory containing exported empty, place-folder data files
TEMP_LOCAL_DIR <- single_line_str(
    "/mnt/STORE1/bighome/sehyun/Packages/OmicsMLRepoData/
    curatedMetagenomicData/export_example_2"
)

#' Get data available through OmicsMLRepo
#'
#' @param metadata A data frame containing, at minimum, a `unique_id` column,
#' which corresponds to a `study_id:sample_id`. This can be obtained from
#' the \code{\link{getMetadata}} function.
#' @param dataType A character(1) describing a data type.
#' @param cache_directory Optional. A character(1). If provided, it should
#' indicate a local file path where any remotely accessed files should be
#' copied.
#' @param repository
#'
#' @importFrom dplyr collect
#'
#'
#'
#' @export
getData <- function(metadata,
                    dataType,
                    cache_directory = get_default_cache_dir(),
                    repository = TEMP_LOCAL_DIR) {

    ## Convert to an in-memory table, unless some of the dplyr operations will
    ## fail when passed a database connection
    raw_data <- collect(metadata) #<<<<<<<<<<<<<<<<< Should I subset columns to make it smaller?




}
