
extend_pedigree <- function(pedigree, max_generations = 8, verbose = TRUE){
  # key format requirement: ego, father, mother in that order
  ancestor_matrix <- as.matrix(pedigree[,1])
  colnames(ancestor_matrix) <- "ego"
  if (max_generations > 0) {
    gen_names <- character(0)
    find_my_parents <- as.matrix(pedigree[,1])
    for (g in 1:max_generations) {
      if (g > 1) find_my_parents <- next_generation
      if (!all(is.na(find_my_parents))) {
        for (j in 1:ncol(find_my_parents)) {
          new_parents <- pedigree[match(find_my_parents[,j], pedigree[,1]), 2:3]
          if (j == 1) {
            next_generation <- new_parents
          } else {
            next_generation <- cbind(next_generation, new_parents)
          }
        }
        gen_names <- sort(
          c(paste(gen_names, "f", sep = ""), paste(gen_names, "m", sep = ""))
        )
        colnames(next_generation) <- gen_names
        ancestor_matrix <- cbind(ancestor_matrix, next_generation)
        if (verbose) print(g)
      }
    }
  }
  drop <- which(colSums(is.na(ancestor_matrix)) == nrow(ancestor_matrix))
  if (length(drop) > 0) ancestor_matrix <- ancestor_matrix[,-drop]
  return(ancestor_matrix)
}

filter_ancestors <- function(focal, pedigree, max_generations = 8) {
  ancestors <- focal
  for (i in 1:max_generations) {
    ego_rows <- which(pedigree[, 1] %in% ancestors)
    ancestors <- c(ancestors, pedigree[ego_rows, 2])
    ancestors <- c(ancestors, pedigree[ego_rows, 3])
    ancestors <- sort(unique(ancestors))
  }
  pedigree[pedigree[, 1] %in% ancestors,,drop = FALSE]
}

filter_descendants <- function(focal, pedigree, max_generations = 8) {
  descendants <- focal
  for (i in 1:max_generations) {
    parent_rows <- which(pedigree[, 2] %in% descendants | pedigree[, 3] %in% descendants)
    descendants <- c(descendants, pedigree[parent_rows, 1])
  }
  pedigree[pedigree[, 1] %in% descendants,,drop = FALSE]
}

filter_common_descendants <- function(focal, pedigree, max_generations = 8) {
  ancestor_pedigree <- filter_ancestors(focal, pedigree, max_generations = max_generations)
  ancestors <- ancestor_pedigree[,1]
  common_descendants <- filter_descendants(ancestors, pedigree, max_generations = max_generations)
  return(common_descendants)
}

