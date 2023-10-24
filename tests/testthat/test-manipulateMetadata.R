#' Original data frame:
#' ind = indicator column
#' aval, bval = columns to expand/compress
#' cval = other (non-treatment-related) column

CTF <- read.csv(system.file("extdata", "sample_compressed_df.csv", package = "OmicsMLRepoR"), na.strings = c())
ETF <- read.csv(system.file("extdata", "sample_expanded_df.csv", package = "OmicsMLRepoR"), na.strings = c())

test_that("expand_metadata works properly", {
  expect_equal(expand_metadata(CTF, ecols = c("aval", "bval"), delim = ";"), ETF)
})

test_that("compress_metadata works properly", {
  expect_equal(compress_metadata(ETF, idcols = c("ind"), ccols = c("aval", "bval"), delim = ";"), CTF)
})
