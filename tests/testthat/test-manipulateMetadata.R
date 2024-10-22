##### getLongMetaTb and getShortMetaTb -----------
data(mini_cmd)

short_tb <- data.frame(
    ind = c("A", "B", "C", "D", "E"),
    aval = c("cat;dog", "chicken", "horse", "frog;pig", "snake"),
    cval = c(1, NA, 3, 4, 5),
    bval = c("red;blue", "yellow", "NA", "green;NA", "brown"))

long_tb <- data.frame(
    ind = c("A", "A", "B", "C", "D", "D", "E"),
    aval = c("cat", "dog", "chicken", "horse", "frog", "pig", "snake"),
    cval = c(1, 1, NA, 3, 4, 4, 5),
    bval = c("red", "blue", "yellow", NA, "green", NA, "brown"))

lmeta <- getLongMetaTb(mini_cmd, "hla")
smeta <- getShortMetaTb(lmeta, targetCols = "hla")
                              

test_that("Test getLongMetaTb function", {
    expect_equal(dim(mini_cmd), c(200,3))
    expect_equal(dim(lmeta), c(539,3))
    ltb <- getLongMetaTb(short_tb, c("aval", "bval"), delim = ";")
    expect_equal(dim(ltb), c(7, 4))
})

test_that("Test getShortMetaTb function", {
    expect_equal(dim(smeta), c(200,3))
    stb <- getShortMetaTb(long_tb, 
                          idCols = "ind", 
                          targetCols = c("aval", "bval"))
    expect_equal(dim(stb), c(5, 4))
})



##### getNarrowMetaTb and getWideMetaTb ---------
wide_tb <- data.frame(
    fruit = c("apple", "banana", "pear", "watermelon", "grape"), 
    shape = c("round", "long", NA, "round", NA),
    color = c("red", "yellow", NA, "green", "purple"),
    size = c("medium", "medium", NA, "large", "small"))

ntb <- getNarrowMetaTb(wide_tb, newCol = "feature", 
                       targetCols = c("color", "shape", "size"), 
                       sep = ":", delim = ";")

test_that("Test getNarrowMetaTb function", {
    expect_equal(dim(ntb), c(5,2))
    expect_true(is.na(ntb[3,2]))
})  
              
## Narrow-table example
narrow_tb <- data.frame(fruit = c("apple", "banana", "pear", "watermelon", 
                                  "grape"), 
                        feature = c("color:red;shape:round;size:medium", 
                                    "color:yellow;shape:long;size:medium",
                                    "color:brown;shape:NA;size:NA",
                                    "color:green;shape:round;size:large",
                                    "color:purple;shape:NA;size:small"))
ntb1 <- getWideMetaTb(narrow_tb, targetCol = "feature", sep = ":", delim = ";")

## Narrow-table example with missing columns
narrow_tb2 <- data.frame(fruit = c("apple", "banana", "pear", 
                                   "watermelon", "grape"), 
                        feature = c("color:red;shape:round;size:medium", 
                                    "color:yellow;shape:long;size:medium",
                                    NA,
                                    "color:green;size:large",
                                    "color:purple;shape:NA;size:small"))
ntb2 <- getWideMetaTb(narrow_tb2, targetCol = "feature", sep = ":", delim = ";")


test_that("Test getWideMetaTb function", {
    expect_equal(dim(ntb1), c(5,4))
    expect_true(is.na(ntb1[3,3]))
    expect_equal(rowSums(is.na(ntb1)), c(0,0,2,0,1))
    expect_equal(as.numeric(colSums(is.na(ntb1))), c(0,0,2,1))
    
    expect_equal(dim(ntb2), c(5,4))
    expect_equal(rowSums(is.na(ntb2)), c(0,0,3,1,1))
    expect_equal(as.numeric(colSums(is.na(ntb2))), c(0,1,3,1))
})
