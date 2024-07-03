test_that("ontoTreePlot function works correctly", {

    test_term <- "NCIT:C2852"
    test_plot <- ontoTreePlot(test_term)
    
    # Check if the function produces output (which should be a plot)
    expect_true(all(class(test_plot) == c("grViz", "htmlwidget")))
    
    # Check if the Term object is created correctly
    ontob <- Ontology(get_ontologies(test_term))
    expect_s4_class(Term(ontob, test_term), "Term")
    
    # Check if the JSON tree is retrieved and parsed correctly
    cur_trm <- Term(ontob, test_term)
    jstree <- cur_trm@links$jstree$href
    expect_true(is.data.frame(jsonlite::fromJSON(jstree)))
    
})