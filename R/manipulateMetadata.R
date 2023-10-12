## Functions to expand and compress treatment metadata

#' Character vector of ID columns
ID_COLS <- c("study_name",
             "patientId",
             "sampleId",
             "curationId")

#' Character vector of treatment-related columns (current but not complete)
T_COLS <- c("original_treatment_name",
             "original_treatment_type",
             "original_split_treatment_name",
             "original_split_treatment_type",
             "original_treatment_name_source",
             "original_treatment_type_source",
             "curated_treatment_name",
             "curated_treatment_type",
             "curated_treatment_name_ontology_term_id",
             "curated_treatment_type_ontology_term_id",
             "original_treatment_dose",
             "original_treatment_number",
             "original_treatment_start",
             "original_treatment_end",
             "original_treatment_frequency",
             "original_treatment_duration",
             "original_split_treatment_dose",
             "original_split_treatment_number",
             "original_split_treatment_start",
             "original_split_treatment_end",
             "original_split_treatment_frequency",
             "original_split_treatment_duration",
             "original_treatment_dose_source",
             "original_treatment_number_source",
             "original_treatment_start_source",
             "original_treatment_end_source",
             "original_treatment_frequency_source",
             "original_treatment_duration_source",
             "curated_treatment_dose_value",
             "curated_treatment_dose_unit",
             "curated_treatment_number_value",
             "curated_treatment_number_unit",
             "curated_treatment_start_value",
             "curated_treatment_start_unit",
             "curated_treatment_end_value",
             "curated_treatment_end_unit",
             "curated_treatment_frequency_value",
             "curated_treatment_frequency_unit",
             "curated_treatment_duration_value",
             "curated_treatment_duration_unit",
             "curated_treatment_dose_unit_ontology_term_id",
             "curated_treatment_number_unit_ontology_term_id",
             "curated_treatment_start_unit_ontology_term_id",
             "curated_treatment_end_unit_ontology_term_id",
             "curated_treatment_frequency_unit_ontology_term_id",
             "curated_treatment_duration_unit_ontology_term_id")

#' Expands treatment metadata
#' 
#' @importFrom tidyr separate_longer_delim
#' 
#' @param meta A data frame of metadata including all treatment-related columns
#' @param ecols Optional character vector of columns to expand if presesnt, defaults to all treatment-related columns
#' @param delim Optional delimiter string, defaults to "<;>"
#' 
#' @return A data frame of metadata expanded so that each individual treatment has its own row
#' 
expand_metadata <- function(meta, ecols = T_COLS, delim = "<;>") {
  separate_longer_delim(data = meta,
                        cols = any_of(ecols),
                        delim = delim)
}

#' Compresses expanded treatment columns to original format
#' 
#' @importFrom dplyr group_by summarise ungroup select
#' 
#' @param meta A data frame with expanded treatment columns
#' @param idcols Optional character vector of columns that identify single samples, defaults to standard ID columns
#' @param ccols Optional character vector of columns to compress if present, defaults to all treatment-related columns
#' @param delim Optional delimiter string, defaults to "<;>"
#'
#' @return A data frame where each sample gets a single row
#' 
compress_metadata <- function(meta, idcols = ID_COLS, ccols = T_COLS, delim = "<;>") {
  cat("Compressing data frame: this may take a few minutes\n")
  col_order <- colnames(meta)
  gcols <- idcols[which(idcols %in% col_order)]
  ocols <- setdiff(setdiff(col_order, ccols), idcols)
  meta %>%
    group_by_at(gcols) %>%
    summarise(
      across(any_of(ccols), ~paste(na.omit(.), collapse = delim)),
      across(any_of(ocols), ~unique(.))
    ) %>%
    ungroup() %>%
    select(all_of(col_order))
}