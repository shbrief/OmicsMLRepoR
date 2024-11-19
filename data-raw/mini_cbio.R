## Subset of cBioPortalData metadata for test
library(dplyr)

extDir <- "~/OmicsMLRepo/OmicsMLRepoData/inst/extdata"
cbio <- read.csv(file.path(extDir, "cBioPortal_curated_metadata_release.csv"))

other_cols <- c("curation_id", "acronym", "acronym_ontology_term_id", "sex","package")
trt_cols <- grep("^treatment_", colnames(mini_cbio), value = TRUE)
non_na_rows <- which(!is.na(cbio$treatment_name))

mini_cbio <- cbio[non_na_rows[1:10], c(other_cols, trt_cols)]

save(mini_cbio, file = "~/OmicsMLRepo/OmicsMLRepoR/data/mini_cbio.RData")
