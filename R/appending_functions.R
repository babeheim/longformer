
append_id_change <- function(new, cdm, check = TRUE) {

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

  if (check) {
    cat("The following row will be added to the output:\n")
    print(new)
  }
  output <- rbind(output, new)

  update_rows <- which(output$active_id == new$old_id)
  if (length(update_rows) > 0) {
    if (check) {
      cat("These existing rows would be updated:\n")
      print(output[update_rows,])
    }
    output$active_id[update_rows] <- new$active_id
  }

  if (check) output <- cdm

  return(output)

}
