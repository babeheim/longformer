\name{review_collisions}
\alias{review_collisions}
\title{
  Review Potential Collisions in a Population Register
}
\description{
  This function allows you to review potential collisions in a population register. It takes a vector of PIDs to check (\code{check}), the population register (\code{d}), a data frame containing detailed information about individuals (\code{people}), and additional optional arguments for customization (\code{pid_column}, \code{display_columns}, \code{reviewed}, \code{refresh}).
}
\usage{
  review_collisions(check, d, people, pid_column = "pid", display_columns = c("pid", "first_name", "last_name_1", "last_name_2"), reviewed = NA, refresh = TRUE)
}
\arguments{
  \item{check}{
    A vector of PIDs that need to be reviewed for potential collisions in the population register.
  }
  \item{d}{
    The population register data frame.
  }
  \item{people}{
    A data frame containing detailed information about individuals in the population register, with each row representing a unique PID.
  }
  \item{pid_column}{
    The name of the column in \code{d} that contains the PIDs.
  }
  \item{display_columns}{
    A character vector specifying the columns from \code{d} to display during the review process. The default columns are "pid", "first_name", "last_name_1", and "last_name_2".
  }
  \item{reviewed}{
    A vector containing previous review decisions for PIDs in \code{check}. This argument can be used to resume the review process from a previous state. The default is NA, which means that all PIDs in \code{check} will be reviewed.
  }
  \item{refresh}{
    A logical value indicating whether to clear the console output and refresh the screen during the review process. The default is TRUE.
  }
}
\details{
  The function provides an interactive process for reviewing potential collisions in a population register. It displays information about the individuals in \code{d} with the specified PIDs in \code{check} along with additional details from the \code{people} data frame. The user is prompted to make a decision for each PID in \code{check}, and the decisions are recorded in the \code{reviewed} vector. The review decisions can be either "no issues", "not sure", or "problem".
}
\examples{
  # Sample data
  d <- data.frame(
    pid = c("PID123", "PID456", "PID789", "PID234", "PID567"),
    first_name = c("John", "Jane", "Alice", "Robert", "Emily"),
    last_name_1 = c("Doe", "Smith", "Johnson", "Lee", "Brown"),
    last_name_2 = c("Johnson", "Lee", "Brown", NA, NA)
  )
  people <- data.frame(
    pid = c("PID123", "PID456", "PID789", "PID234", "PID567"),
    age = c(30, 25, 45, 28, 33),
    sex = c("M", "F", "F", "M", "F")
  )

  # Review potential collisions
  check <- c("PID123", "PID234", "PID789")
  review_results <- review_collisions(check, d, people)
}
