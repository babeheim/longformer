
load_database <- function(path = "data", encoding = NULL, fileEncoding = "UTF-8", silent = TRUE) {

  if (!is.null(encoding)) {
    warning("the 'encoding' argument has been deprecating, use 'fileEncoding' instead")
    fileEncoding <- encoding
  }

  data_files <- list.files(path = path, pattern='*.csv', full.names=TRUE)
  data_tables <- basename(data_files)
  data_tables <- gsub('\\.csv', '', data_tables)

  if (!silent) progbar <- txtProgressBar(1, length(data_files), style=3)

  db <- list()
  for (i in 1:length(data_files)) {
    db[[i]] <- read.csv(data_files[i], stringsAsFactors = FALSE, fileEncoding = fileEncoding)
    names(db)[i] <- data_tables[i]
    if (!silent) setTxtProgressBar(progbar, i)
  }
  
  if (!silent) close(progbar)
  return(db)

}


generate_schema <- function(db, keys, sensitive_variables, verbose = TRUE) {

    # generate a list of tables
    tables <- data.frame(
        table = names(db),
        nobs = unlist(lapply(db, nrow)),
        nvar = unlist(lapply(db, ncol)),
        stringsAsFactors = FALSE
    )

    # generate a list of variables
    for(i in 1:nrow(tables)){
        add <- data.frame(variable=colnames(db[[i]]))
        add$variable <- as.character(add$variable)
        add$table <- tables$table[i]
        add$class <- unlist(lapply(db[[i]], class))
        add$n_present <- colSums(!is.na(db[[i]]) & db[[i]]!='')
        add$n_missing <- colSums(is.na(db[[i]]) | db[[i]]=='')
        if (i == 1) {
            variables <- add
        } else {
          variables <- bind_rows(variables, add)
        }
        if (verbose) print(tables$table[i])
    }
    variables$primary_key <- as.numeric(paste(variables$table, variables$variable) %in%
     paste(keys$table, keys$variable))

    variables$confidential <- as.numeric(variables$variable %in% sensitive_variables)

    output <- list(variables = variables, tables = tables)
    return(output)

}
