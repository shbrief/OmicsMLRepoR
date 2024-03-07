library(ontologyIndex)
library(ontologyPlot)

# further ideas:
# 1. include ids in node labels
# 2. adjust view (I'm seeing nodes being cut off)
# 3. way to switch current_term easily, maybe use Shiny to allow clicking on tree/nodes

# get data as obo file
# downloaded from https://www.ebi.ac.uk/ols/ontologies/ncit
# converted .owl > .obo with ROBOT (https://github.com/ontodev/robot)
ncit <- "ncit.obo"

# load in ontology
ontology <- get_ontology(ncit)

# define current term
current_term <- "NCIT:C15280"

# get/plot full tree of direct ancestors/descendants
all_connected_terms <- c(get_ancestors(ontology, current_term), get_descendants(ontology, current_term, exclude_roots = TRUE))
plot_all <- onto_plot(ontology, terms = all_connected_terms)

# functions to get parents, children, siblings
get_parents <- function(given_term) {
  parents <- get_term_property(ontology = ontology,
                    property = "parents",
                    term = given_term,
                    as_names = TRUE) 
  return(parents)
}

get_children <- function(given_term) {
  children <- get_term_property(ontology = ontology,
                    property = "children",
                    term = given_term,
                    as_names = TRUE)
  return(children)
}

get_siblings <- function (given_term) {
  siblings <- c()
  parents <- names(get_parents(given_term))
  
  for (i in 1:length(parents)) {
    current_parent <- parents[i]
    children <- get_children(current_parent)
    siblings <- c(siblings, children)
    siblings <- siblings[!duplicated(names(siblings))]
  }
  return(siblings)
}

# plot parents, children, siblings
plot_parents <- onto_plot(ontology, terms = c(current_term, names(get_parents(current_term))))
plot_children <- onto_plot(ontology, terms = c(current_term, names(get_children(current_term))))
plot_siblings <- onto_plot(ontology, terms = get_ancestors(ontology, names(get_siblings(current_term))))


