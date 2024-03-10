## Subset of cBioPortal metadata for test
extDir <- "~/OmicsMLRepo/OmicsMLRepoData/inst/extdata"
cbio <- read.csv(file.path(extDir, "cBioPortal_curated_metadata.csv"))

cbio_sub1 <- cbio[1:200,]
lmeta <- getLongMetaTb(cbio_sub1)
dup_ids <- lmeta$curation_id[duplicated(lmeta$curation_id)]

cbio_sub1 <- cbio_sub1[which(cbio_sub1$curation_id %in% dup_ids),] %>% head
cols <- c("curation_id", 
          "curated_sex", 
          "curated_treatment_type",
          "curated_treatment_name", 
          "curated_treatment_dose",
          "curated_treatment_reason_source")
cbio_sub2 <- cbio_sub1[cols]

dir <- "~/OmicsMLRepo/OmicsMLRepoR/inst/extdata"
readr::write_csv(cbio_sub2, file.path(dir, "mini_cbio.csv"))
