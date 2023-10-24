## Construction of sample compressed data frame for testing compress_metadata and expand_metadata

# sample_compressed_df
ctf <- data.frame(ind = c("A", "B", "C", "D", "E"),
                  aval = c("cat;dog", "chicken", "horse", "frog;pig", "snake"),
                  cval = c(1, NA, 3, 4, 5),
                  bval = c("red;blue", "yellow", "NA", "green;NA", "brown"))