
match_vids <- function(dates, pids, visits, visit_day_threshold = 45, quiet = FALSE) {
  if (length(dates) != length(pids)) {
    stop("dates and pids have to be the same length!")
  }
  dates <- as.Date(dates)
  visits$date <- as.Date(visits$date)
  matched_vids <- rep(NA, length(dates))
  for(i in 1:length(dates)) {
    if (!is.na(dates[i])) {
      my_visit_rows <- which(visits$pid == pids[i])
      my_existing_vids <- visits$vid[my_visit_rows]
      diffs <- abs(as.numeric(dates[i] - visits$date[my_visit_rows]))
      if (any(!is.na(diffs))) {
        if (any(diffs < visit_day_threshold, na.rm = TRUE)) {
          matched_vids[i] <- my_existing_vids[which.min(diffs)]
          if (!quiet) cat("date matched to existing visit\n")
        }
      }
      if (is.na(matched_vids[i])) {
        date_reversable <- as.numeric(substr(dates[i], 9, 10)) < 13
        if (date_reversable) {
          my_date_reversed <- reverse_month_day(dates[i])
          my_date_reversed <- as.Date(my_date_reversed)
          diffs <- abs(as.numeric(my_date_reversed - visits$date[my_visit_rows]))
          if (any(!is.na(diffs))) {
            if (any(diffs < visit_day_threshold, na.rm = TRUE)) {
              matched_vids[i] <- my_existing_vids[which.min(diffs)]
              if (!quiet) cat("reverse-date matched to existing visit\n")
            }
          }
        }
      }
    }
  }
  return(matched_vids)
}
