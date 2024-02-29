## Construction of sample_map_2.csv for testing searchMetadata.R functions

# sample_map_2 ("treatment")
s2 <- data.frame(original_value = c("acarbose", "antidiab", "metformin", "novorapid", "solostar"),
                 curated_ontology_term = c("Acarbose", "Anti-diabetic Agent", "Metformin", "Insulin Aspart", "Insulin Glargine"),
                 curated_ontology_term_id = c("NCIT:C983", "NCIT:C29711", "NCIT:C61612", "NCIT:C47563", "NCIT:C47564"),
                 curated_ontology_term_db = c("NCIT", "NCIT", "NCIT", "NCIT", "NCIT"))
write.csv(s2,
          file.path(system.file("extdata", package = "OmicsMLRepoR"),
                    "sample_map_2.csv"),
          row.names = FALSE)