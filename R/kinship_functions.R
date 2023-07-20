
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

# identify all individuals who are ancestors of a particular focal or set of focals
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

# identify all individuals who are descendants of a particular focal or set of focals
filter_descendants <- function(focal, pedigree, max_generations = 8) {
  descendants <- focal
  for (i in 1:max_generations) {
    parent_rows <- which(pedigree[, 2] %in% descendants | pedigree[, 3] %in% descendants)
    descendants <- c(descendants, pedigree[parent_rows, 1])
  }
  pedigree[pedigree[, 1] %in% descendants,,drop = FALSE]
}

# identify all individuals who are descendants of a particular focal or set of focals
filter_common_descendants <- function(focal, pedigree, max_generations = 8) {
  ancestor_pedigree <- filter_ancestors(focal, pedigree, max_generations = max_generations)
  ancestors <- ancestor_pedigree[,1]
  common_descendants <- filter_descendants(ancestors, pedigree, max_generations = max_generations)
  return(common_descendants)
}



# descendant_counts <- function(reg, am = NA){

#   # compute am if it doesn't already exist
#   if (is.na(am)) am <- ancestors(reg)

#   all.names <- sort(unique(c(am[,1], am[,2], am[,3])))

#   desMat <- all.names

#   types <- nchar(colnames(am))
#   types[1] <- 0

#   g.count <- 0

#   for (i in 1:max(types[-1])){

#     gen <- am[, types == i]
#     gen <- as.matrix(gen)
#     this.gen.names <- all.names[all.names %in% gen]

#     # error when only one person in that generation, because of dimensions!
#     this.gen.counts <- rep(0, length(this.gen.names))
#     for (j in seq_along(this.gen.names)) {
#       this.gen.counts[j] <- sum(rowSums(this.gen.names[j] == gen, na.rm = TRUE) == 1, na.rm = TRUE)
#     }

#     all.names.counts <- rep(0, length(all.names))
#     names(all.names.counts) <- all.names    
#     all.names.counts[as.character(this.gen.names)] <- this.gen.counts

#     desMat <- cbind(desMat, all.names.counts) 
#     colnames(desMat)[i+1] <- paste(paste(rep("g", i - 1), collapse = ""), "k", sep = "")

#     print(i)

#   }

#   colnames(desMat)[1] <- "id"

#   return(desMat)

# }

# my.tree <- function(ancestor.vec){

#   ymax <- max(nchar(names(na.omit(ancestor.vec))))

#   n <- 2^(1:ymax)

#   x <- list(NA)
#   y <- list(NA)
#   scale <-1 
#   x[[1]] <- c(0-scale, 0+scale)
#   y[[1]] <- c(1, 1)
#   male <- 

#   for(i in 2:ymax){
#     x[[i]] <- sort(c(x[[i-1]]+(scale/2^(i-1)), x[[i-1]]-scale/2^(i-1)))
#     y[[i]] <- rep(i, 2^i)

#   }

#   xs <- c(0,unlist(x))
#   ys <- c(0,unlist(y))

#   plot(xs, ys, type = "n",xaxt="n",yaxt="n",xlab="",ylab="")

#   lines(c(x[[1]][1], 0), c(1, 0), col="gray")
#   lines(c(x[[1]][2], 0), c(1, 0), col="gray")

#   for(i in 2:length(x)){
#     for(j in 1:length(x[[i-1]])){
#       lines(c(x[[i]][2*j-1], x[[i-1]][j]), c(i, i-1), col="gray")
#       lines(c(x[[i]][2*j], x[[i-1]][j]), c(i, i-1), col="gray")
#     }
#   }

#   text(xs, ys, labels=ancestor.vec, col=c("black", rep(c("blue", "deeppink"), length(ancestor.vec)-1)))

# }



# kin_coef <- function(pid.one, pid.two, pedigree, ancestor_matrix){

#   if (!pid.one %in% pedigree$pid | !pid.two %in% pedigree$pid) stop("both pids must be in the population register")

#   pedigree$m.pid[which(pedigree$m.pid=="")] <- NA
#   pedigree$f.pid[which(pedigree$f.pid=="")] <- NA
  
#   # reduce the pop reg to those individuals who have both parents' information
#   wipe <- which(is.na(pedigree$m.pid) & !is.na(pedigree$f.pid) | !is.na(pedigree$m.pid) & is.na(pedigree$f.pid))
  
#   pedigree$m.pid[wipe] <- NA
#   pedigree$f.pid[wipe] <- NA
  
#   # pedigree function wants these sex codes: 1 = male, 2 = female, 3 = unknown
#   pedigree$male <- as.numeric(pedigree$male)
#   pedigree$male[pedigree$male==0] <- 2
#   pedigree$male[is.na(pedigree$male)] <- 3

#   # identify all relatives for pid.one
#   # all.relatives.one <- identify_relatives(pid.one, pedigree, am)
#   focal_ancestors <- as.vector(na.omit(am[which(am[,1]==pid.one),]))
#   all.relatives <- character(0)
#   for(i in 1:length(focal_ancestors)){
#     last.gen <- focal_ancestors[i]
#     all.relatives <- c(all.relatives, last.gen)
#     tar <- 1
#     while(length(tar)>0){
#       tar <- which(pedigree$f.pid %in% last.gen | pedigree$m.pid %in% last.gen)
#       if(length(tar)>0){
#         last.gen <- pedigree$pid[tar]
#         all.relatives <- c(all.relatives, last.gen)
#       }
#     }
#   }
#   all.relatives.one <- unique(all.relatives)

#   # identify all relatives for pid.two
#   # all.relatives.two <- identify_relatives(pid.one, pedigree, am)
#   focal_ancestors <- as.vector(na.omit(ancestor_matrix[which(ancestor_matrix[,1]==pid.two),]))
#   all.relatives <- character(0)
#   for(i in 1:length(focal_ancestors)){
#     last.gen <- focal_ancestors[i]
#     all.relatives <- c(all.relatives, last.gen)
#     tar <- 1
#     while(length(tar)>0){
#       tar <- which(pedigree$f.pid %in% last.gen | pedigree$m.pid %in% last.gen)
#       if(length(tar)>0){
#         last.gen <- pedigree$pid[tar]
#         all.relatives <- c(all.relatives, last.gen)
#       }
#     }
#   }
#   all.relatives.two <- unique(all.relatives)

#   # identify all relatives by common descent for either of the pair
#   all.relatives.both <- unique(c(all.relatives.one, all.relatives.two))

#   # identify all ancestors of all relatives by common descent
#   all_relatives_and_ancestors <- as.vector(unique(na.omit(as.vector(ancestor_matrix[which(ancestor_matrix[,1] %in% all.relatives.both),]))))

#   # subset the population register to all relatives and of the focal pair and those relatives' ancestors
#   tar <- which(pedigree$pid %in% all_relatives_and_ancestors)

#   # calculate the pedigree & kinship matrix
#   test <- pedigree(id=pedigree$pid[tar], dadid=pedigree$f.pid[tar], momid=pedigree$m.pid[tar], sex=pedigree$male[tar])
#   kin <- kinship(test)

#   output <- kin[which(rownames(kin) == pid.one), which(colnames(kin) == pid.two)]*2
#   rm(kin) # this is large so make sure it is gone

#   return(output)
# }

# identify_all_relatives <- function(focal, pedigree, am) {
#   # pedigree and am kinda contain the same info so should just be one!
#   focal_ancestors <- as.vector(na.omit(am[which(am[,1] == focal),]))
#   all.relatives.focal <- character(0)
#   if (length(focal_ancestors) > 0) {
#     all.relatives <- character(0)
#     for (i in seq_along(focal_ancestors)){
#       last.gen <- focal_ancestors[i]
#       all.relatives <- c(all.relatives, last.gen)
#       tar <- 1
#       while (length(tar) > 0) {
#         tar <- which(pedigree$f.pid %in% last.gen | pedigree$m.pid %in% last.gen)
#         if (length(tar) > 0){
#           last.gen <- pedigree$pid[tar]
#           all.relatives <- c(all.relatives, last.gen)
#         }
#       }
#     }
#   }
#   all.relatives.focal <- unique(all.relatives)
#   return(all.relatives.focal)
# }
