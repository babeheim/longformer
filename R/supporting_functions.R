


clean_text <- function(text) {
  # convert non-ASCII to closest ascii
  # takes care of non-printing ASCII
  text %>% stringi::stri_trans_general("latin-ascii") %>%
    str_replace("[\x01-\x1F]", "")
}


load_database <- function(path = "data/") {

  data_files <- list.files(path = path, pattern='*.csv', full.names=TRUE)
  data_tables <- basename(data_files)
  data_tables <- gsub('\\.csv', '', data_tables)

  progbar <- txtProgressBar(1, length(data_files), style=3)

  db <- list()
  for (i in 1:length(data_files)) {
    db[[i]] <- read.csv(data_files[i], stringsAsFactors = FALSE)
    names(db)[i] <- data_tables[i]
    setTxtProgressBar(progbar, i)
  }
  
  close(progbar)
  return(db)

}

reverse_month_day <- function(date) {
  date <- as.character(date)
  if (length(date) == 1) {
    if (!is.na(date)) {
      date <- strsplit(date, "-")[[1]]
      date <- date[c(1, 3, 2)]
      date <- paste(date, collapse = "-")
      date <- as.Date(date, "%Y-%m-%d")
      date <- as.character(date)
    }
  } else {
    for (i in 1:length(date)) {
      date[i] <- reverse_month_day(date[i])
    }
  }
  return(date)
}

similarity <- function(x, y) 1 - adist(x, y) / pmax(nchar(x), nchar(y))

dir_init <- function(path, verbose=FALSE){
  if(substr(path, 1, 2)!='./') stop('path argument must be formatted
    with "./" at beginning')
  contents <- dir(path, recursive=TRUE)
  if(verbose){
    if(length(contents)==0) print(paste('folder ', path, ' created.', sep=""))
    if(length(contents)>0) print(paste('folder ', path, ' wiped of ', length(contents), ' files/folders.', sep=""))
  }
  if(dir.exists(path)) unlink(path, recursive=TRUE)
  dir.create(path)
}

make_ids <- function(n, bag = c(letters, 0:9), reserved='', seed=NA, nchars=NA){
  if(is.na(seed) | !is.numeric(seed)) set.seed(as.numeric(as.POSIXlt(Sys.time())))
  if(!is.na(seed) & is.numeric(seed)) set.seed(seed)
  output <- replicate(n, paste(sample(bag, nchars, replace=TRUE), 
    collapse=''))
  rejected <- duplicated(output) | output %in% reserved | substr(output, 1, 1) %in% 0:9
  while(any(rejected)){
    output <- output[-which(rejected)]
    remaining <- n-length(output)
    output <- c(output, replicate(remaining, paste(sample(bag, nchars, 
      replace=TRUE), collapse='')))
    rejected <- duplicated(output) | output %in% reserved | substr(output, 1, 1) %in% 0:9
  }
  output
}

review_cases <- function(check, d, reviewed = NA, refresh = TRUE) {

  if (!any(is.na(reviewed))) stop ("invalid reviewed vector")
  start <- min(which(is.na(reviewed)))
  out <- rep(NA, length(check))
  out[1:length(reviewed)] <- reviewed
  if (refresh) system("clear")
  for (i in start:length(check)) {
    print(d[which(d$pid == check[i]), ])
    out[i] <- readline(paste("(", i, "/", length(check), ") 1=no issues, 2=not sure, 3=problem; type 'exit' to end\ndecision: ", sep=""))
    out[out == "1"] <- "no issues"
    out[out == "2"] <- "not sure"
    out[out == "3"] <- "problem"
    if (refresh) system("clear")
    if (out[i] == "exit") break()
  }

  print("all cases reviewed!")

  return(out)

}
