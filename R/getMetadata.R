s2p_get_cache <- function() {
    cache <- tools::R_user_dir("OmicsMLRepo", "cache") # create a directory for cached data
    BiocFileCache::BiocFileCache(cache = cache) # directory path of cache 
}

#' @importFrom BiocFileCache bfcneedsupdate bfcdownload bfcadd bfcquery bfcrpath
#'
s2p_cached_url <- function(url, 
                           rname = url, 
                           ask_on_update = FALSE,
                           ...) {
    bfc <- s2p_get_cache()
    bfcres <- bfcquery(x = bfc, 
                       query = rname, # regular expression pattern(s) to match
                       field = "rname") # column names in resource to query
    # %>% subset(select = -expires)
    
    rid <- bfcres$rid # auto-generated resource id
    
    ## Cached file not found
    if (!length(rid)) {
        rid <- names(bfcadd(x = bfc, rname = rname, fpath = url))
    }
    
    ## If needs update, do the download
    if (bfcneedsupdate(bfc, rid)) {
        bfcdownload(bfc, rid, ask = FALSE, ...)
        print("Downloading")
    }
    
    res <- bfcrpath(bfc, rids = rid)
    return(res)
}




#' Download a curated metadata table
#' 
#' @importFrom readr read_csv
#'
#' @param database Name of the database to get the metadata from. Currently, 
#' there are two available options.
#' \itemize{
#'     \item \code{cMD} : metadata for curatedMetagenomicData
#'     \item \code{cBioPortal} : metadata for cBioPortal
#' }
#' @param load Default is \code{TRUE}. If it's set to \code{FALSE}, the 
#' metadata table is downloaded to cache but not loaded into memory.
#'
#' @return Curated metadata table or file cache location, if `load = FALSE`.
#'
#' @examples
#' cmd <- getMetadata("cMD")
#'
#' @export
getMetadata <- function(database = c("cMD", "cBioPortal"), 
                        load = TRUE) {
    
    bucket_name <- "omics_ml_repo"
    request_meta <- paste0(database, "_curated_metadata_release.csv")
    fpath <- file.path("https://storage.googleapis.com",
                       bucket_name, request_meta)

    fpath <- s2p_cached_url(fpath)
    if (isTRUE(load)) {
        model <- readr::read_csv(fpath)
        return(model)
    } else {return(fpath)}
}