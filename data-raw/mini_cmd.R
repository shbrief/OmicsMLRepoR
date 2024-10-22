## Subset of cMD metadata for test
library(dplyr)
library(OmicsMLRepoR)

extDir <- "~/OmicsMLRepo/OmicsMLRepoData/inst/extdata"
cmd <- read.csv(file.path(extDir, "cMD_curated_metadata_release.csv"))

mini_cmd <- cmd %>%
    filter(!is.na(hla)) %>%
    .[1:200,] %>%
    select(curation_id, hla, package)

save(mini_cmd, file = "~/OmicsMLRepo/OmicsMLRepoR/data/mini_cmd.RData")
