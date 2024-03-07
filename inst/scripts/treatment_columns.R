## cBioPortal `curated_treatment_*` tables
dir <- "~/OmicsMLRepo/OmicsMLRepoData"
dataDir <- file.path(dir, "cBioPortalData/cBioPortal_treatment/data")
curated <- list.files(dataDir)
curatedTrtObj <- gsub(".rds", "", curated)
for (i in seq_along(curated)) {
    res <- readRDS(file.path(dataDir, curated[i]))
    assign(curatedTrtObj[i], res)
}

## All the column names from 4 `curated_treatment_*` data tables
columns <- sapply(curatedTrtObj, function(x) {colnames(get(x))})

extDir <- file.path(dir, "inst/extdata")
ecols <- columns[grep("curated_treatment", names(columns))] %>% 
    unlist %>% unique %>% as.character %>% .[-1]
write.table(ecols, file.path(extDir, "treatment_columns.txt"), 
            row.names = FALSE, col.names = FALSE, quote = FALSE)