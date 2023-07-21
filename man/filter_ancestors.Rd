\name{filter_ancestors}
\alias{filter_ancestors}
\title{
  Filter Ancestors of Individuals in a Pedigree
}
\description{
  This function filters and retrieves the ancestors of individuals in a pedigree up to a specified maximum number of generations. It takes a pedigree and an individual's ID as inputs and returns a new pedigree containing the ancestors of the specified individual.
}
\usage{
  filter_ancestors(focal, pedigree, max_generations = 8)
}
\arguments{
  \item{focal}{
    The ID of the individual for whom to retrieve the ancestors.
  }
  \item{pedigree}{
    The original pedigree as a list of lists, where each sublist represents an individual with ego, father, and mother information.
  }
  \item{max_generations}{
    The maximum number of generations to retrieve ancestors. The default is 8.
  }
}
\value{
  Returns a new pedigree containing the ancestors of the specified individual up to the specified maximum number of generations.
}
\examples{
  # Sample pedigree data
  pedigree <- list(
    list(ego = 1, father = 2, mother = 3),
    list(ego = 2, father = 4),
    list(ego = 3, father = 5, mother = 6),
    list(ego = 4, father = 7),
    list(ego = 5),
    list(ego = 6),
    list(ego = 7)
  )

  # Filter ancestors for individual 3 up to 3 generations
  ancestors <- filter_ancestors(focal = 3, pedigree = pedigree, max_generations = 3)
}
