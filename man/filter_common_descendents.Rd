\name{filter_common_descendants}
\alias{filter_common_descendants}
\title{
  Filter Common Descendants of Individuals in a Pedigree
}
\description{
  This function filters and retrieves the common descendants of individuals in a pedigree up to a specified maximum number of generations. It takes a pedigree and a vector of individual IDs as inputs and returns a new pedigree containing the common descendants of the specified individuals.
}
\usage{
  filter_common_descendants(focal, pedigree, max_generations = 8)
}
\arguments{
  \item{focal}{
    A vector of individual IDs for whom to retrieve the common descendants.
  }
  \item{pedigree}{
    The original pedigree as a list of lists, where each sublist represents an individual with ego, father, and mother information.
  }
  \item{max_generations}{
    The maximum number of generations to retrieve common descendants. The default is 8.
  }
}
\value{
  Returns a new pedigree containing the common descendants of
