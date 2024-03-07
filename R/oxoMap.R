#' Mapping input ontology terms and a target ontology through the OxO API
#' 
#' @importFrom httr2 request req_headers resp_body_json req_perform
#' 
#' @param term_ids A character vector of term ids to retrieve mappings for
#' @param target_ontology A character (1). The target ontology library to 
#' map the `term_ids` to. 
#' @param mapping_distance Optional. Mapping distance as an integer, 1 
#' (default), 2, or 3. 
#'
#' @return A list of of data frames with mapping results for each input term
#'
oxoMap <- function(term_ids, target_ontology, mapping_distance = 1) {
  # Validate input
  stopifnot(is.character(term_ids),
            is.character(target_ontology),
            is.numeric(mapping_distance),
            mapping_distance %in% c(1, 2, 3))
  
  # Prepare inputs  
  term_ids <- as.list(term_ids)
  target_ontology <- as.list(target_ontology)
  
  # Prepare request
  req <- request("https://www.ebi.ac.uk/spot/oxo/api/search?size=200") %>%
    req_headers("Content-Type" = "application/json",
                "Accept" = "application/json") %>%
    req_body_json(list(ids = term_ids,
                       inputSource = NULL,
                       mappingTarget = target_ontology,
                       distance = mapping_distance))
  
  # Perform request
  resp <- req_perform(req)
  
  # Parse response
  resp_json <- resp_body_json(resp)
  resp_list <- lapply(resp_json$`_embedded`$searchResults, 
                      function(x) as.data.frame(do.call(rbind, 
                                                        x$mappingResponseList)))
  names(resp_list) <- term_ids
  
  # Return as list of data frames
  return(resp_list)
}