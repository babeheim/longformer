
load_database <- function(path = "data/") {

  data_files <- list.files(path = path, pattern='*.csv', full.names=TRUE)
  data_tables <- basename(data_files)
  data_tables <- gsub('\\.csv', '', data_tables)

  progbar <- txtProgressBar(1, length(data_files), style=3)

  db <- list()
  for (i in 1:length(data_files)) {
    db[[i]] <- read.csv(data_files[i], stringsAsFactors = FALSE, fileEncoding = "latin1")
    names(db)[i] <- data_tables[i]
    setTxtProgressBar(progbar, i)
  }
  
  close(progbar)
  return(db)

}
