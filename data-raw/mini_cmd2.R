## Subset of cMD metadata for test
library(dplyr)

extDir <- "~/OmicsMLRepo/OmicsMLRepoData/inst/extdata"
cmd <- read.csv(file.path(extDir, "cMD_curated_metadata_release.csv"))

# Select samples with multiple values for two attributes - `target_condition` and `disease`
complexInd <- intersect(grep(";", cmd$target_condition), grep(";", cmd$disease))

mini_cmd2 <- cmd %>%
    select(curation_id, target_condition, target_condition_ontology_term_id,
           pmid, disease, disease_ontology_term_id, package) %>%
    .[complexInd[1:200],] 

save(mini_cmd2, file = "~/OmicsMLRepo/OmicsMLRepoR/data/mini_cmd2.RData")
