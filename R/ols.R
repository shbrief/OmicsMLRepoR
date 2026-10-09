## Minimal client for the EBI Ontology Lookup Service (OLS4) REST API
##
## These helpers replace the `rols` package, which was removed from
## Bioconductor 3.24. They cover only the requests OmicsMLRepoR makes.

.olsBase <- "https://www.ebi.ac.uk/ols4/api"

# Build an OLS4 API URL from a path and named query parameters
#
# @keywords internal
.olsUrl <- function(path, params = list()) {
    url <- paste0(.olsBase, path)
    if (length(params)) {
        vals <- vapply(params, function(x) {
            utils::URLencode(as.character(x), reserved = TRUE)
        }, character(1))
        url <- paste0(url, "?",
                      paste(names(params), vals, sep = "=", collapse = "&"))
    }
    url
}


# Search OLS
#
# Equivalent of `as(olsSearch(OlsSearch(...)), "data.frame")` in `rols`.
#
# @return A data.frame with one row per matching term.
#
# @importFrom jsonlite fromJSON
# @keywords internal
.olsSearch <- function(query, ontology = "", exact = FALSE, rows = 20) {
    params <- list(q = paste(query, collapse = ","),
                   rows = rows,
                   exact = tolower(as.character(exact)))
    ontology <- ontology[nzchar(ontology)]
    if (length(ontology)) {
        params$ontology <- paste(tolower(ontology), collapse = ",")
    }
    res <- jsonlite::fromJSON(.olsUrl("/search", params))
    docs <- res$response$docs
    if (!is.data.frame(docs)) {docs <- data.frame()}
    docs
}


# Retrieve a single term by its OBO id (e.g. "NCIT:C2852")
#
# Equivalent of `olsTerm(olsOntology(ontology), id)` in `rols`.
#
# @return A list holding the term's OLS record, including `label`, `obo_id`
# and `_links`.
#
# @keywords internal
.olsTerm <- function(ontology, id) {
    path <- paste0("/ontologies/", tolower(ontology), "/terms")
    res <- jsonlite::fromJSON(.olsUrl(path, list(obo_id = id)),
                              simplifyVector = FALSE)
    terms <- res[["_embedded"]][["terms"]]
    if (!length(terms)) {
        stop("Term '", id, "' was not found in ontology '", ontology, "'.",
             call. = FALSE)
    }
    terms[[1]]
}


# Retrieve the terms related to a term, e.g. its children or ancestors
#
# Equivalent of `termLabel(children(term))` or `termLabel(ancestors(term))`
# in `rols`.
#
# @param term A term record returned by `.olsTerm()`.
# @param relation A character(1). The name of the term's link to follow,
# such as "children" or "ancestors".
#
# @return A character vector of term labels named by their OBO ids. Empty
# when the term has no such relation (e.g. a leaf term has no children).
#
# @keywords internal
.olsRelated <- function(term, relation) {
    url <- term[["_links"]][[relation]][["href"]]
    if (is.null(url)) {return(character(0))}
    url <- paste0(url, "?size=500")

    ids <- character(0)
    labels <- character(0)
    while (!is.null(url)) {
        res <- jsonlite::fromJSON(url, simplifyVector = FALSE)
        terms <- res[["_embedded"]][["terms"]]
        ids <- c(ids, vapply(terms, function(x) {
            if (is.null(x$obo_id)) NA_character_ else x$obo_id
        }, character(1)))
        labels <- c(labels, vapply(terms, function(x) {
            if (is.null(x$label)) NA_character_ else x$label
        }, character(1)))
        url <- res[["_links"]][["next"]][["href"]]
    }
    names(labels) <- ids
    labels
}
