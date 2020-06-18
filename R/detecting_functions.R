
detect_retired_pids <- function(cdm, d, columns = c("pid", "first_name", "last_name_1", "last_name_2"), outpath = NA) {

  check <- unique(d$pid[which(d$pid %in% cdm$old_id)])

  my.script <- character(0)

  for(i in 1:length(check)){

    my.cdm.rows <- which(cdm$old_id==check[i])

    print(d[which(d$pid==check[i]), columns])
    print(cdm[my.cdm.rows,])
    
    output <- character(0)
    
    n <- readline(paste("(", i, "/", length(check), ") 1=use first, 2=use second, etc.?  ", sep=""))

    if(n=="1"){

      active_id <- cdm$active_id[my.cdm.rows[1]]
      output <- paste("d$pid[which(d$pid=='", check[i], "')] <- '", active_id, "'", sep="")

    }

    if(n=="2"){

      active_id <- cdm$active_id[my.cdm.rows[2]]
      output <- paste("d$pid[which(d$pid=='", check[i], "')] <- '", active_id, "'", sep="")

    }

    if(n=="3" & length(my.cdm.rows)>2){

      active_id <- cdm$active_id[my.cdm.rows[3]]
      output <- paste("d$pid[which(d$pid=='", check[i], "')] <- '", active_id, "'", sep="")

    }

    if(n=="4" & length(my.cdm.rows)>3){

      active_id <- cdm$active_id[my.cdm.rows[4]]
      output <- paste("d[which(d$pid=='", check[i], "')] <- '", active_id, "'", sep="")

    }

    
    my.script <- c(my.script, output)
    
  }

  if (!is.na(outpath)) writeLines(my.script, "cdm_cleanings.txt")

  return(my.script)

}



detect_pid_collisions <- function(data_pids, data_names,
  reference_pids = NA, reference_names = NA,
  threshold = 0.8, silent = FALSE) {

  if (class(data_pids) != "character") stop("data_pids must be a character vector")
  if (class(data_names) != "character") stop("data_names must be a character vector")
  if (length(data_pids) != length(data_names)) stop("data_pids and data_names must be vectors of the same length!")
  if (length(reference_pids) != length(reference_names)) stop("reference_pids and reference_names must be vectors of the same length!")
  if (any(duplicated(reference_pids))) stop("reference_pids should be a vector of unique pids - no duplicates!")

  pids <- sort(unique(data_pids))
  n_pids <- length(pids)

  data_names <- clean_text(data_names)

  if (any(is.na(pids))) warning("some pids are NA")

  if (!silent) progbar <- txtProgressBar(1, n_pids, style=3)

  flag <- rep(NA, n_pids)

  for (i in 1:n_pids) {
    my_names <- data_names[which(data_pids == pids[i])]
    drop <- which(my_names == "" | is.na(my_names))
    if (length(drop) > 0) my_names <- my_names[-drop]
    if (all(is.na(reference_names))) {
      reference_name <- my_names[1]
    } else {
      reference_name <- reference_names[which(reference_pids == pids[i])]
    }
    flag[i] <- any(similarity(reference_name, my_names) < threshold)
    if (!silent) setTxtProgressBar(progbar, i)
  }

  if (!silent) close(progbar)

  check <- pids[flag]

  if (length(check) == 0 & !silent) cat("no pids were flagged as possible collisions!\n")

  return(check)

}

review_collisions <- function(check, d, people, reviewed = NA, refresh = TRUE) {

  if (!any(is.na(reviewed))) stop ("invalid reviewed vector")
  start <- min(which(is.na(reviewed)))
  out <- rep(NA, length(check))
  out[1:length(reviewed)] <- reviewed
  if (refresh) system("clear")
  for (i in start:length(check)) {
    print(d[which(d$pid == check[i]), ])
    print(people[which(people$pid == check[i]), ])
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
