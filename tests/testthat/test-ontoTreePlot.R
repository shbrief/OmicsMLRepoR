test_that("ontoTreePlot function works correctly", {

    test_term <- "NCIT:C2852"
    test_plot <- ontoTreePlot(test_term)
    
    # Check if the function produces output (which should be a plot)
    expect_true(all(class(test_plot) == c("grViz", "htmlwidget")))
    
    # Check if the term record is retrieved correctly
    cur_trm <- .olsTerm(get_ontologies(test_term), test_term)
    expect_identical(cur_trm$obo_id, test_term)
    
    # Check if the JSON tree is retrieved and parsed correctly
    jstree <- cur_trm[["_links"]][["jstree"]][["href"]]
    expect_true(is.data.frame(jsonlite::fromJSON(jstree)))
    
})
