\name{detect_pid_collisions}
\alias{detect_pid_collisions}
\title{
  Detect Collisions in a Population Register Based on PIDs and Names
}
\description{
  This function detects potential collisions in a population register based on Personal Identification Numbers (PIDs) and corresponding names. It takes a vector of PIDs (\code{data_pids}) and a vector of corresponding names (\code{data_names}), along with an optional reference set of PIDs and names (\code{reference_pids} and \code{reference_names}). It calculates the similarity between the names and identifies PIDs in \code{data_pids} that are likely to belong to the same individual based on the similarity threshold.
}
\usage{
  detect_pid_collisions(data_pids, data_names, reference_pids = NA, reference_names = NA, threshold = 0.8, silent = FALSE)
}
\arguments{
  \item{data_pids}{
    A character vector containing the PIDs to be checked for collisions.
  }
  \item{data_names}{
    A character vector containing the corresponding names for the PIDs in \code{data_pids}. It is used to compare similarity.
  }
  \item{reference_pids}{
    A character vector containing the reference PIDs in the population register. Default is NA, in which case \code{data_pids} and \code{data_names} will be used as the reference.
  }
  \item{reference_names}{
    A character vector containing the corresponding names for the reference PIDs in \code{reference_pids}. Default is NA, in which case \code{data_pids} and \code{data_names} will be used as the reference.
  }
  \item{threshold}{
    A numeric value between 0 and 1 specifying the similarity threshold for considering a collision. The default is 0.8, meaning that a similarity of at least 80% is required for considering a match.
  }
  \item{silent}{
    A logical value indicating whether to suppress progress and informational messages. The default is FALSE, which means that progress messages will be shown during the collision detection process.
  }
}
\details{
  The function compares the PIDs in \code{data_pids} to the PIDs in the population register given by \code{reference_pids}. It calculates the similarity between the corresponding names in \code{data_names} and \code{reference_names} using a fuzzy matching algorithm. If the similarity exceeds the specified \code{threshold}, the corresponding PIDs are considered to be potential collisions and are returned as a vector of PIDs that may belong to the same individual in the population register.
}
\examples{
  # Sample data
  data_pids <- c("PID123", "PID456", "PID789")
  data_names <- c("John Doe", "Jane Smith", "Alice Johnson")
  reference_pids <- c("PID234", "PID789", "PID345")
  reference_names <- c("Jon Doe", "Alice Johnston", "Robert Johnson")

  # Detect potential collisions
  potential_collisions <- detect_pid_collisions(data_pids, data_names, reference_pids, reference_names)
}
