## Subset of cMD metadata for test
library(dplyr)
library(OmicsMLRepoR)

extDir <- "~/OmicsMLRepo/OmicsMLRepoData/inst/extdata"
cmd <- read.csv(file.path(extDir, "cMD_curated_metadata_release.csv"))

cols <- c("curation_id", "pmid", "package", "target_condition", 
          "feces_phenotype", "probing_pocket_depth")
cols <- .getAssociatedAttr(cmd, cols)
rows <- c(grep(";", cmd$feces_phenotype)[1:5], 
          grep(";", cmd$probing_pocket_depth)[1:5])

mini_cmd3 <- cmd[rows, cols] 

save(mini_cmd3, file = "~/OmicsMLRepo/OmicsMLRepoR/data/mini_cmd3.RData")
