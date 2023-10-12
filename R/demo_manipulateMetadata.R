#' Original data frame:
#' ind = indicator column
#' aval, bval = columns to expand
#' dval = other (non-treatment-related) column

tf <- data.frame(ind = c("A", "B", "C", "D", "E"),
                 aval = c("cat;dog", "chicken", "horse", "frog;pig", "snake"),
                 cval = c(1, NA, 3, 4, 5),
                 bval = c("red;blue", "yellow", "NA", "green;NA", "brown"))

etf <- expand_metadata(tf, ecols = c("aval", "bval"), delim = ";")

ctf <- compress_metadata(etf, idcols = c("ind"), ccols = c("aval", "bval"), delim = ";")