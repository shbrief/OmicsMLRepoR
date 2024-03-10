#' Extract the terms used under a given attributes/column
#'
#' @param attributes A character (1). Name of the attribute/column you want to 
#' extract the terms used under. 
#' @param db A character(1). Currently, `cMD` (curatedMetagenomicData) is only
#' supported.
#' 
#' @return A named list. Name of the list is the attribute/column name
#' 
#' @examples
#' availableTerms("age_group")
#'
#' @export
availableTerms <- function(attributes, db = "cMD") {

    ## Curated cMD metadata data dictionary
    dir <- system.file("extdata", package = "OmicsMLRepoR")
    fname <- paste0(db, "_data_dictionary.csv") 
    dict <- read.csv(file.path(dir, fname), header = TRUE)

    ## Separate the attributes to check
    ind <- grep(attributes, dict$col.name)

    ## Extract allowed values
    res_ls <- vector(mode = "list", length = length(ind))
    names(res_ls) <- dict$col.name[ind]
    
    for (i in seq_along(ind)) {
        j <- ind[i]
        allowedvalues <- strsplit(dict$allowedvalues[j], "\\|") %>% unlist
        ontology <- strsplit(dict$ontology[j], "\\|") %>% unlist
        res_tb <- data.frame(allowedvalues = allowedvalues,
                             ontology = ontology)
        res_ls[[i]] <- res_tb
    }

    return(res_ls)
}
