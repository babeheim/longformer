

detect_pid_collisions <- function(data_pids, data_names,
  reference_pids = NA, reference_names = NA,
  threshold = 0.8, silent = FALSE) {

  if (class(data_pids) != "character") stop("data_pids must be a character vector")
  if (class(data_names) != "character") stop("data_names must be a character vector")
  if (length(data_pids) != length(data_names)) stop("data_pids and data_names must be vectors of the same length!")
  if (length(reference_pids) != length(reference_names)) stop("reference_pids and reference_names must be vectors of the same length!")

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

