## Construction of sample expanded data frame for testing compress_metadata and expand_metadata

# sample_expanded_df
etf <- data.frame(ind = c("A", "A", "B", "C", "D", "D", "E"),
                  aval = c("cat", "dog", "chicken",
                           "horse", "frog", "pig", "snake"),
                  cval = c(1, 1, NA, 3, 4, 4, 5),
                  bval = c("red", "blue", "yellow",
                           "NA", "green", "NA", "brown"))
write.csv(etf,
          file.path(system.file("extdata", package = "OmicsMLRepoR"),
                    "sample_expanded_df.csv"),
          row.names = FALSE)