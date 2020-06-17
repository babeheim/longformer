

update_id_changes <- function(new, cdm, verbose = TRUE) {

  if (is.na(new$type) | (!new$type %in% c("vid", "pid"))) stop("type must either be 'vid' or 'pid")

  output <- cdm

  if (new$old_id == "") stop("You must give an old pid.")
  if (new$active_id == "") stop("You must give a new pid.")
  if (new$name == "") stop("You must give a name.")
  if (any(output$old_id == new$old_id & output$active_id == new$active_id)) {
    print(output[which(output$old_id == new$old_id & output$active_id == new$active_id),])
    warning("Looks like this entry already exists")
  }
  if (!all(c("coder", "date", "old_id", "active_id", "name", "type", "reason") %in% names(new))) {
    stop ("some fields in the 'new' entry are missing")
  }

  if (verbose) {
    cat("The following row will be added to the output:\n")
    print(new)
  }
  output <- rbind(output, new)

  update_rows <- which(output$active_id == new$old_id)
  if (length(update_rows) > 0) {
    if (verbose) {
      cat("These existing rows would be updated:\n")
      print(output[update_rows,])
    }
    output$active_id[update_rows] <- new$active_id
  }

  return(output)

}


update_pid <- function(old_pid, active_pid, people, verbose = TRUE) {

  old_pid <- as.character(old_pid)
  active_pid <- as.character(active_pid)

  if (!(is.data.frame(people) & length(dim(people) == 2) & all(dim(people) != 0))) {
    stop("Population register is not a proper data frame!")
  }

  if (
    (old_pid == "" | is.na(old_pid)) |
    (active_pid == "" | is.na(active_pid))
  ) stop("You need to give both PIDs!")
  if (old_pid == active_pid) stop("The old_pid and active_pid have to be different.")

  if (!old_pid %in% people$pid) {
    stop("Error! The old_pid doesn't appear at all in the register!")
  }

  output <- people

  old_pid_row <- which(output$pid == old_pid)

  if (active_pid %in% output$pid) {
    # delete the old_pid row
    active_pid_row <- which(output$pid == active_pid)
    if (verbose == TRUE) {
      print("This row will be deleted:")
      print(output[old_pid_row,])
      print("This person already exists at this row:")
      print(output[active_pid_row,])
    }
    output$notes[active_pid_row] <- paste(
      output$notes[active_pid_row], 
      output$notes[old_pid_row], sep="")
    output <- output[-old_pid_row,]
  } else {
    # update the old_pid row
    if (verbose == TRUE) {
      print("This row will be updated:")
      print(output[old_pid_row,])
    }
    output$pid[old_pid_row] <- active_pid
  }

  if (old_pid %in% output$father_pid) {
    as_father_rows <- which(output$father_pid == old_pid)

    if (verbose == TRUE) {
      print("The following father rows will be changed:")
      print(output[as_father_rows, ])
    }
    if (old_pid %in% output$pid) {
      old_pid_row <- which(output$pid == old_pid)
      if (!is.na(output$male[old_pid_row]) & output$male[old_pid_row] == 0)
      warning("This PID is female but appears as father!")
    }

    output$father_pid[as_father_rows] <- active_pid

  }

  if (old_pid %in% output$mother_pid) {

    as_mother_rows <- which(output$mother_pid == old_pid)

    if (verbose == TRUE) {
        print("The following mother rows will be changed:")
        print(output[as_mother_rows,])
      }
    if (old_pid %in% output$pid) {
      old_pid_row <- which(output$pid == old_pid)
      if (!is.na(output$male[old_pid_row]) & output$male[old_pid_row] == 1) 
      warning("This PID is male but appears as mother!")
    }
    output$mother_pid[as_mother_rows] <- active_pid

  }

  return(output)

}
