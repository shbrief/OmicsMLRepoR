## Construction of sample_metadata.csv for testing searchMetadata.R functions

# sample_metadata
meta <- data.frame(curation_id = c("study1:sample1", "study1:sample2",
                                   "study2:sample1", "study2:sample2"),
                   curated_sex = c("Female", "Female", "Male", "Male"),
                   curated_sex_ontology_term_id = c("NCIT:C16576",
                                                    "NCIT:C16576",
                                                    "NCIT:C20197",
                                                    "NCIT:C20197"),
                   curated_disease = c("Diabetes Mellitus", "Diabetes Mellitus",
                                       "adenocarcinoma", "Adenoma"),
                   curated_disease_ontology_term_id = c("NCIT:C2985",
                                                        "NCIT:C2985",
                                                        "EFO:0000228",
                                                        "NCIT:C2855"))
write.csv(meta,
          file.path(system.file("extdata", package = "OmicsMLRepoR"),
                    "sample_metadata.csv"),
          row.names = FALSE)