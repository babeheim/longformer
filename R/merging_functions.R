
update_pid <- function(old_pid, active_pid, people, verbose = TRUE) {

  old_pid <- as.character(old_pid)
  active_pid <- as.character(active_pid)

  if (!(is.data.frame(people) & length(dim(people) == 2) & all(dim(people) != 0))) {
    stop("Population register is not a proper data frame!")
  }

  if (old_pid == "" | active_pid == "") stop("You need to give both PIDs!")
  if (old_pid == active_pid) stop("The old_pid and active_pid have to be different.")

  if (!old_pid %in% people$pid) {
    stop("Error! The old_pid doesn't appear at all in the register!")
  }

  output <- people

  old_pid_row <- which(output$pid == old_pid)

  if (verbose == TRUE) {
    print("This row will be updated:")
    print(output[old_pid_row,])
  }
  output$pid[old_pid_row] <- active_pid

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

merge_pids <- function(old_pid, active_pid, people, verbose = TRUE) {

  old_pid <- as.character(old_pid)
  active_pid <- as.character(active_pid)

  if (!(is.data.frame(people) & length(dim(people) == 2) & all(dim(people) != 0))) {
    stop("Population register is not a proper data frame!")
  }

  if (old_pid == "" | active_pid == "") stop("You need to give both PIDs!")
  if (old_pid == active_pid) stop("The old_pid and active_pid have to be different.")

  if (!old_pid %in% people$pid) {
    stop("Error! The old_pid doesn't appear at all in the register!")
  }
  if (!active_pid %in% people$pid) {
    stop("Error! The active_pid doesn't appear at all in the register!")
  }

  output <- people

  old_pid_row <- which(output$pid == old_pid)

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
