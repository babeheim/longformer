\name{make_ids}
\alias{make_ids}
\title{Generate Random ID Codes}
\description{
  Generate random ID codes based on specified parameters.
}
\usage{
  make_ids(n, bag = c(letters, 0:9), reserved = "", seed = NA, nchars = 4)
}
\arguments{
  \item{n}{Number of ID codes to generate.}
  
  \item{bag}{A character vector containing the elements from which to generate the ID codes.
             Defaults to a combination of lowercase letters (a-z) and digits (0-9).}
             
  \item{reserved}{A character vector containing IDs that should be avoided and not generated. 
                  Defaults to an empty character vector.}
                  
  \item{seed}{Optional. Set the seed for the random number generator. If not specified, the seed will be based on the current time.}
  
  \item{nchars}{The number of characters each ID code should have. Defaults to 4.}
}
\value{
  A character vector containing randomly generated ID codes.
}
\author{
  Bret Beheim, bret_beheim@eva.mpg.de
}
\examples{
  # Generate 10 random IDs with the default settings
  make_ids(10)
  
  # Generate 20 random IDs using the "thlhp" bag and 6 characters
  make_ids(20, bag = "thlhp", nchars = 6)
}
