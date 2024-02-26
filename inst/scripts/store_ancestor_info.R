## Collection of ontology term ancestors linked to used terms in both curatedMetagenomicData and cBioPortalData datasets

# Function to access
# saveInfo

## Variables
# Storage filenames
dir <- system.file("extdata", package = "OmicsMLRepoR")
cMD_filepath <- file.path(dir, "cMD_ancestors.csv")
cBioPortalData_filepath <- file.path(dir, "cBioPortalData_ancestors.csv")

# Map directories
data_repo_dir <- system.file("", package = "OmicsMLRepoData")
cMD_mapdir <- file.path(data_repo_dir, "curatedMetagenomicData/maps")
cBioPortalData_mapdir <- file.path(data_repo_dir, "cBioPortalData/maps")

## Save ancestors
# Save cMD ancestors
saveInfo(cMD_mapdir, cMD_filepath, "cMD_.*_map\\.csv")

# Save cBioPortalData ancestors
saveInfo(cBioPortalData_mapdir, cBioPortalData_filepath, "cBioPortalData_.*_map\\.csv")