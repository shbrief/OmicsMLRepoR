#' Extract all the terms used in a quired attribute/column
#'
#' @importFrom rlang is_empty
#' 
#' @param attribute A character (1). Name of the attribute/column you want to 
#' extract the terms used under. 
#' @param db A character(1). Currently, `cMD` (curatedMetagenomicData) is only
#' supported.
#' 
#' @return A data frame with two columns - existing values under the quried
#' attribute (`allowedvalues` column) and their ontology term id (`ontology`
#' column).
#' 
#' @examples
#' availableTerms("age_group")
#' availableTerms("disease")
#'
#' @export
availableTerms <- function(attribute, db = "cMD") {

    ## Curated cMD metadata data dictionary
    dir <- system.file("extdata", package = "OmicsMLRepoR")
    fname <- paste0(db, "_data_dictionary.csv") 
    dd <- read.csv(file.path(dir, fname), header = TRUE)

    ## Separate the attribute to check
    ind <- which(dd$col.name == attribute)
    
    ## Sanity check: the presence of attribute in the metadata
    if (is_empty(ind)) {stop("Quried attribute doesn't exist in the metadata.")}

    ## Extract allowed values
    allowedvalues <- strsplit(dd$allowedvalues[ind], "\\|") %>% unlist
    ontology <- strsplit(dd$ontology[ind], "\\|") %>% unlist
    res_tb <- data.frame(allowedvalues = allowedvalues,
                         ontology = ontology)

    return(res_tb)
}
