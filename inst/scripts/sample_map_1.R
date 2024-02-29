## Construction of sample_map_1.csv for testing searchMetadata.R functions

# sample_map_1 ("disease")
s1 <- data.frame(original_value = c("IGT", "acute_diarrhoea", "adenocarcinoma", "adenoma", "carcinoma"),
                 curated_ontology_term = c("abnormal glucose tolerance", "acute diarrhea", "adenocarcinoma", "Adenoma", "carcinoma"),
                 curated_ontology_term_id = c("EFO:0002546", "MONDO:0000257", "EFO:0000228", "NCIT:C2855", "EFO:0000313"),
                 curated_ontology_term_db = c("EFO", "MONDO", "EFO", "NCIT", "EFO"))
write.csv(s1,
          file.path(system.file("extdata", package = "OmicsMLRepoR"),
                    "sample_map_1.csv"),
          row.names = FALSE)