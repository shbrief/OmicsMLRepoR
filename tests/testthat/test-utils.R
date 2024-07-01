test_that("get_ontologies function", {
    terms <- c("HP:0001824", "MONDO:0010200", "NCIT:C122328", "86743009")
    res <- get_ontologies(terms)
    expect_equal(res, c("HP", "MONDO", "NCIT", "SNOMED"))
})

test_that("merge_vectors function", {
    x <- "color:NA;shape:NA;size:NA"
    y <- "color:green;size:large"
    res <- merge_vectors(x, y)
    expect_equal(res, "color:green;shape:NA;size:large")
})