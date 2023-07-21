\name{extend_pedigree}
\alias{extend_pedigree}
\title{
  Extend a Pedigree to Include Multiple Generations of Ancestors
}
\description{
  This function extends a pedigree by adding multiple generations of ancestors for each individual in the original pedigree. It creates an ancestor matrix that includes ego, father, and mother columns for each generation, forming a multi-generational pedigree.
}
\usage{
  extend_pedigree(pedigree, max_generations = 8, verbose = TRUE)
}
\arguments{
  \item{pedigree}{
    The original pedigree as a list of lists, where each sublist represents an individual with ego, father, and mother information.
  }
  \item{max_generations}{
    The maximum number of generations to extend the pedigree. The default is 8.
  }
  \item{verbose}{
    A logical value indicating whether to display progress and information during the extension process. The default is TRUE.
  }
}
\details{
  The function extends the pedigree by iteratively adding generations of ancestors for each individual in the original pedigree. It creates an ancestor matrix with ego, father, and mother columns for each generation, forming a multi-generational pedigree. The maximum number of generations to extend can be specified using the \code{max_generations} argument.
}
\value{
  Returns the ancestor matrix representing the extended pedigree.
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

  # Extend the pedigree to 5 generations
  extended_pedigree <- extend_pedigree(pedigree, max_generations = 5)
}
