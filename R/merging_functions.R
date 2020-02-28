
# merge_visits <- function(....)

merge_people <- function(old_pid, active_pid, people, check = TRUE) {

  people <- people

  old_pid <- as.character(old_pid)
  active_pid <- as.character(active_pid)

  if (!(is.data.frame(people) & length(dim(people) == 2) & all(dim(people) != 0))) {
    stop("Population register is not a proper data frame!")
  }

  temp <- people

  if (old_pid == "" | active_pid == "") stop("You need to give both PIDs!")
  if (old_pid == active_pid) stop("The old_pid and active_pid have to be different.")

  if (!old_pid %in% temp$pid) {
    stop("Error! The old_pid doesn't appear at all in the register!")
  }

  old_pid_row <- which(temp$pid == old_pid)

  if (active_pid %in% temp$pid) {
    active_pid_row <- which(temp$pid == active_pid)
    if (check == TRUE) {
      print("This row will be deleted:")
      print(temp[old_pid_row,])
      print("This person already exists at this row:")
      print(temp[active_pid_row,])
    }
    temp$notes[active_pid_row] <- paste(
      temp$notes[active_pid_row], 
      temp$notes[old_pid_row], sep="")
    temp <- temp[-old_pid_row,]
  } else {
    if (check == TRUE) {
      print("This row will be updated:")
      print(temp[old_pid_row,])
    }
    temp$pid[old_pid_row] <- active_pid
  }

  if (old_pid %in% temp$father_pid) {
    as_father_rows <- which(temp$father_pid == old_pid)

    if (check == TRUE) {
      print("The following father rows will be changed:")
      print(temp[as_father_rows, ])
    }
    if (old_pid %in% temp$pid) {
      old_pid_row <- which(temp$pid == old_pid)
      if (!is.na(temp$male[old_pid_row]) & temp$male[old_pid_row] == 0)
      warning("This PID is female but appears as father!")
    }

    temp$father_pid[as_father_rows] <- active_pid

  }

  if (old_pid %in% temp$mother_pid) {

    as_mother_rows <- which(temp$mother_pid == old_pid)

    if (check == TRUE) {
        print("The following mother rows will be changed:")
        print(temp[as_mother_rows,])
      }
    if (old_pid %in% temp$pid) {
      old_pid_row <- which(temp$pid == old_pid)
      if (!is.na(temp$male[old_pid_row]) & temp$male[old_pid_row] == 1) 
      warning("This PID is male but appears as mother!")
    }
    temp$mother_pid[as_mother_rows] <- active_pid

  }
  
  output <- temp
  if (check == TRUE) output <- people

  return(output)

}
