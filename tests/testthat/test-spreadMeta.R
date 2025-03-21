data(mini_cbio)
trt_cols <- grep("^treatment_", colnames(mini_cbio), value = TRUE)
res1 <- spreadMeta(mini_cbio, targetCol = trt_cols)

data(mini_cmd2) # multi-valued attribute
data(mini_cmd3) # composite attribute
res2 <- spreadMeta(mini_cmd2, "target_condition")
res3 <- spreadMeta(mini_cmd3, "probing_pocket_depth")

test_that("Test spreadMeta function", {
    expect_equal(dim(res1), c(12,9))
    expect_equal(dim(res2), c(465,7))
    expect_equal(dim(res3), c(10,12))
    
    expect_equal(res1$treatment_name[9:12], 
                 c("Mitotane", "Mitotane", NA, "Mitotane"))
    expect_equal(res1$treatment_name_ontology_term_id[9:12], 
                 c("NCIT:C664", "NCIT:C664", NA, "NCIT:C664"))
    expect_equal(res1$treatment_type[9:12], 
                 c("Pharmacotherapy", "Pharmacotherapy", "Radiation Therapy", "Pharmacotherapy"))
    expect_equal(res1$treatment_type_ontology_term_id[9:12], 
                 c("NCIT:C15986", "NCIT:C15986", "NCIT:C15313", "NCIT:C15986"))
})