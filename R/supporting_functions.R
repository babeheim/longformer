
clean_text <- function(text) {
  # convert non-ASCII to closest ascii
  # takes care of non-printing ASCII
  text %>% stringi::stri_trans_general("latin-ascii") %>%
    str_replace("[\x01-\x1F]", "") %>%
    str_replace("[\x7F-\x9F]", "") %>%
    str_replace("�", "")
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

make_ids <- function(n, bag = c(letters, 0:9), reserved='', seed=NA, nchars=4){
  if (length(bag) == 1) {
    if (bag == "thlhp") {
      banned_letters <- c("I", "Z", "G", "S", "O", "E")
      banned_numbers <- c(0, 1, 5, 6)
      pid_letters <- setdiff(LETTERS, banned_letters)
      pid_numbers <- setdiff(0:9, banned_numbers)
      bag <- c(pid_letters, pid_numbers)
    }
  }
  if (!is.numeric(nchars)) stop("how many nchars the id will have?")
  if (is.na(seed) | !is.numeric(seed)) set.seed(as.numeric(as.POSIXlt(Sys.time())))
  if (!is.na(seed) & is.numeric(seed)) set.seed(seed)
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



# this function doesn't work!

# apply_id_updates <- function(cdm, d) {

# # step 1, exact name matches

#   cdm$key <- paste(cdm$old_id, cdm$nombre, sep="-")
#   d$key <- paste(d$pid, d$Nombre, sep="-")
#   length(cdm$active_id[match(d$key[d$key %in% cdm$key], cdm$key)])  #155
#   d$pid[d$key %in% cdm$key] <- cdm$active_id[match(d$key[d$key %in% cdm$key], cdm$key)]

#   cdm <- cdm[,-which(colnames(cdm)=="key")]
#   d <- d[,-which(colnames(d)=="key")]

# # step 2, manual updates

#   check <- unique(d$pid[which(d$pid %in% cdm$old_id)])

#   d[which(d$pid==check[i]),c("Fecha", "pid", "Nombre", "Apellido1", "Apellido2")]
#   cdm[cdm$old_id==check[i],]
#   # cens[which(cens$PID %in% cdm$active_id[cdm$old_id==check[i]]),]
#   i <- i + 1

#   manual_updates <- compare_id_records(cdm, d, outpath = "cdm_cleanings.txt")

# # step 3, automatic updates

#   length(cdm$active_id[match(d$pid[which(d$pid %in% cdm$old_id)], cdm$old_id)])  # 4! 
#   d$pid[which(d$pid %in% cdm$old_id)] <- cdm$active_id[match(d$pid[which(d$pid %in% cdm$old_id)], cdm$old_id)]

#   unique(d$pid[which(d$pid %in% cdm$old_id)]) # none!

#   return(d)

# }

