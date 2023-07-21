\name{load_database}
\alias{load_database}
\title{
  Load Database from CSV Files into a List of Data Tables
}
\description{
  This function loads multiple CSV files from the specified path and creates a list of data tables, where each data table corresponds to a CSV file. The function can handle CSV files with different names, and it assumes that each CSV file represents a separate data table. The function is useful for organizing and accessing multiple data tables simultaneously in R.
}
\usage{
  load_database(path = "data/", encoding = "")
}
\arguments{
  \item{path}{
    The path to the directory containing the CSV files to be loaded. The default is "data/".
  }
  \item{encoding}{
    The character encoding of the CSV files. The default is an empty string (i.e., no encoding specified). You can specify a specific encoding (e.g., "UTF-8", "Latin-1") to handle files with non-ASCII characters correctly.
  }
}
\details{
  The function loads all CSV files from the specified path using the \code{read.csv} function. It assumes that each CSV file represents a separate data table and assigns each data table to a list element. The function returns the list of data tables, where each element's name corresponds to the CSV file's basename (i.e., file name without the extension).
}
\author{
  Bret Beheim, bret_beheim@eva.mpg.de
}
\examples{
  # Load CSV files from the "data/" directory
  my_database <- load_database()

  # Load CSV files from a custom directory with Latin-1 encoding
  my_custom_database <- load_database(path = "custom_data/", encoding = "Latin-1")
}
