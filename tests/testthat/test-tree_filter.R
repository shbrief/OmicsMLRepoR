library(dplyr)

cmd <- getMetadata("cMD")
cbio <- getMetadata("cBioPortal")

test_that("Check getMetadata function", {
    ## Check the correct 'released-version' format
    expect_equal(colnames(cmd)[ncol(cmd)], "last_updated")
    expect_equal(unique(cmd$package), "cMD")
    expect_equal(colnames(cbio)[ncol(cbio)], "last_updated")
    expect_equal(unique(cbio$package), "cBioPortal")
})

test_that("tree_filter: case-insensitive", {
    res1 <- cmd %>% tree_filter(disease, "Colorectal Carcinoma")
    res2 <- cmd %>% tree_filter(disease, "colorectal carcinoma")
    expect_equal(nrow(res1), nrow(res2))
})

test_that("tree_filter: synonym searching", {
    syn_res1 <- cmd %>% tree_filter(disease, "CRC")
    syn_res2 <- cmd %>% tree_filter(disease, "Colorectal Cancer")
    syn_res3 <- cmd %>% tree_filter(disease, "Colorectal Carcinoma")
    
    expect_equal(nrow(syn_res1), nrow(syn_res2))
    expect_equal(nrow(syn_res1), nrow(syn_res3))
})

test_that("tree_filter: descendt searching", {
    ds_res <- cmd %>% 
        tree_filter(disease, "Intestinal Disorder") %>%
        select(disease)
    res_terms <- strsplit(ds_res$disease, ";") %>% unlist %>% unique
    ds_terms <- c("Crohn Disease",
                  "Colorectal Carcinoma", 
                  "Inflammatory Bowel Disease",
                  "Irritable Bowel Syndrome")
    expect_true(all(ds_terms %in% res_terms))
})

test_that("tree_filter: logical argument", {
    res_m <- cmd %>% tree_filter(disease, "migraine")
    res_d <- cmd %>% tree_filter(disease, "diabetes")
    res_or <- cmd %>% tree_filter(disease, c("migraine", "diabetes"), "OR")
    res_and <- cmd %>% tree_filter(disease, c("migraine", "diabetes"), "AND")
    res_not <- cmd %>% tree_filter(disease, c("migraine", "diabetes"), "NOT")
    
    expect_equal(nrow(res_m) + nrow(res_d) - nrow(res_and), nrow(res_or))
    expect_equal(nrow(res_or) + nrow(res_not), nrow(cmd))
})