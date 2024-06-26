% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ontoTreePlot.R
\name{ontoTreePlot}
\alias{ontoTreePlot}
\title{Plot ontology tree}
\usage{
ontoTreePlot(term, display = "Term")
}
\arguments{
\item{term}{A character (1). Ontology term id (obo_id)}

\item{display}{A character (1) specifying a node labeling option. Two 
available options are `Term` (default) for ontology term or IRI 
(Internationalized Resource Identifier) and `Text` for the label or 
preferred name.}
}
\value{
A ontology tree plot. All the terms used in the output plot are 
ancestors of the queried term, so the queried term is the tip.
}
\description{
Plot ontology tree
}
\examples{
ontoTreePlot("NCIT:C2852")

}
