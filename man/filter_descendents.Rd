\name{filter_descendants}
\alias{filter_descendants}
\title{
  Filter Descendants of Individuals in a Pedigree
}
\description{
  This function filters and retrieves the descendants of individuals in a pedigree up to a specified maximum number of generations. It takes a pedigree and an individual's ID as inputs and returns a new pedigree containing the descendants of the specified individual.
}
\usage{
  filter_descendants(focal, pedigree, max_generations = 8)
}
\arguments{
  \item{focal}{
    The ID of the individual for whom to retrieve the descendants.
  }
  \item{pedigree}{
    The original pedigree as a list of lists, where each sublist represents an individual with ego, father, and mother information.
  }
  \item{max_generations}{
    The maximum number of generations to retrieve descendants. The default is 8.
  }
}
\value{
  Returns a new pedigree containing the descendants of the specified individual up to the specified maximum number of generations.
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

  # Filter descendants for individual 2 up to 2 generations
  descendants <- filter_descendants(focal = 2, pedigree = pedigree, max_generations = 2)
}
