## Subset of cMD metadata for test
library(dplyr)
library(OmicsMLRepoR)

extDir <- "~/OmicsMLRepo/OmicsMLRepoData/inst/extdata"
cmd <- read.csv(file.path(extDir, "cMD_curated_metadata_release.csv"))

cmd_sub1 <- cmd %>%
    filter(!is.na(hla)) %>%
    .[1:200,] %>%
    select(curation_id, hla, package)

dir <- "~/OmicsMLRepo/OmicsMLRepoR/inst/extdata"
readr::write_csv(cmd_sub1, file.path(dir, "mini_cmd.csv"))
