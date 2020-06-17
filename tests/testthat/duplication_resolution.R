
# basic scenario:
# there's a table of visits with various identifying info
# mostly names and dates and pids

# you don't need vid in your initial example, it is unnecessary
# but we DO need obs_id!

# after the duplication/collision/addition stuff
# THEN we talk about combining visit info
# basic idea is that a date is flexible, the 'same' visit based on field season



library(longformer)

# for each dataset we need to build a pid_updates file
flag_pid_changes <- function(data, pid_changes, name = NA) {
  tar <- which(data$pid %in% pid_changes$old_id)
  pid_updates <- data[tar, c("obs_id", "pid")]
  if (!is.na(name)) pid_updates %>% mutate(file = name) %>% select(file, obs_id, old_pid = pid) -> pid_updates
  # now we need to match...this is tricky when collisions are involved, but we can make it semi-manual
  pid_updates$change_row <- match(pid_updates$old_pid, pid_changes$old_id)
  pid_updates$active_pid <- pid_changes$active_id[pid_updates$change_row]  
  return(pid_updates)
}

update_pids <- function(data, data_pid_column = "pid", pid_updates) {
  tar <- which(data$obs_id %in% pid_updates$obs_id)
  data[[data_pid_column]][tar] <- pid_updates$active_pid[match(data$obs_id[tar], pid_updates$obs_id)]
  return(data)
}


## basic basics

data1 <- list(
  list(
    date = "2020-01-01",
    pid = "A",
    name = "Amanda",
    y = 2
  ),
  list(
    date = "2020-01-03",
    pid = "A",
    name = "Amanda",
    y = 3
  ),
  list(
    date = "2020-01-03",
    pid = "A",
    name = "Amanda",
    y = 3
  ),
  list(
    date = "2020-01-01",
    pid = "D",
    name = "Danielle",
    y = 10
  )
) %>% bind_rows() %>% as.data.frame(stringsAsFactors = FALSE)

# a simple rule: all data tables should have an independent primary key
# do not make it a combination of other data, especially foreign keys

data1 %>% mutate(obs_id = 1:nrow(data1)) -> data1

# we realize there's a duplication
# common to deal with, have to eliminate

# best solution to eliminate entries is the dplyr::filter function
data1 <- data1 %>% filter(obs_id != 3)

# other functions are:
data1 <- data1 %>% slice(-which(obs_id == 3))
data1 <- data1[-which(data1$obs_id == 3),]
# difference is that these will fail noisly if they can't find the entries to drop


# appending entries is straightforwards using bind_rows

new_entry <- list(
  date = "2020-01-10",
  pid = "A",
  y = 32,
  notes = "name missing in interview"
)

data1 <- data1 %>% bind_rows(new_entry)

# advantage of bind_rows over rbind: it doesn't require the entries to be all present, can accomodate new variables too

# in terms of editing specific entries

df <- data.frame(a = c(1, 2, 3), b = c(4, 5, 6))
# using matrix notation
df[c(2, 3), 2] <- c(50, 60)

# using replace
df <- data.frame(a = c(1, 2, 3), b = c(4, 5, 6))
df$b <- replace(df$b, 2:3, c(50, 60))

# using dplyr::mutate
df <- data.frame(a = c(1, 2, 3), b = c(4, 5, 6))
df %>% mutate(b = replace(b, c(2, 3), c(50, 60)))



# we can rewrite entries using standard 


# very useful when different data collection protocols


# as a basic rule, the obs_id should never change. we shouldn't need to 'rekey' the values to be consecutive integers or anything


db <- list()

db$people <- list(
  list(
    pid = "A",
    name = "Amanda",
    female = 1
  ),
  list(
    pid = "B",
    name = "Amanda",
    female = 1
  ),
  list(
    pid = "C",
    name = "Amanda",
    female = 1
  ),
  list(
    pid = "D",
    name = "Danielle",
    female = 1
  ),
  list(
    pid = "E",
    name = "Danielle",
    female = 1
  ),
  list(
    pid = "F",
    name = "Felicity",
    female = 1
  )
) %>% bind_rows() %>% as.data.frame(stringsAsFactors = FALSE)

db$data1 <- list(
  list(
    obs_id = "1",
    date = "2020-01-01",
    pid = "A",
    name = "Amanda",
    y = 2
  ),
  list(
    obs_id = "2",
    date = "2020-01-05",
    pid = "A",
    name = "Amanda",
    y = 3
  ),
  list(
    obs_id = "3",
    date = "2020-01-03",
    pid = "B",
    name = "Amanda",
    y = 3
  ),
  list(
    obs_id = "4",
    date = "2020-01-04",
    pid = "A",
    name = "Amanda",
    y = 7
  ),
  list(
    obs_id = "5",
    date = "2020-01-01",
    pid = "D",
    name = "Danielle",
    y = 10
  )
) %>% bind_rows() %>% as.data.frame(stringsAsFactors = FALSE)


db$data2 <- list(
  list(
    obs_id = "1",
    date = "2020-01-01",
    pid = "A",
    name = "Amanda",
    x = 5
  ),
  list(
    obs_id = "2",
    date = "2020-01-03",
    pid = "A",
    name = "Amanda",
    x = 9
  ),
  list(
    obs_id = "3",
    date = "2020-01-03",
    pid = "E",
    name = "Danielle",
    x = 20
  ),
  list(
    obs_id = "4",
    date = "2020-01-03",
    pid = "B",
    name = "Amanda",
    x = 33
  ),
  list(
    obs_id = "5",
    date = "2020-01-02",
    pid = "C",
    name = "Amanda",
    x = 6
  )
) %>% bind_rows() %>% as.data.frame(stringsAsFactors = FALSE)


## Okay, let's begin

# in this practice dataset, we've already done the following:
# all individuals in all data files have been given pids
# all data observations have been given unique identifiers within their datasets that cannot change

# however, it's not yet usable because there are actually three duplications:
# B and C are both actually A, and E is actually D
# assume we need the data to actually identify the duplications

# the task is finished when all three changes have been cascaded through the database

# looking at data1, we can see A and B are all the same person
# how do we go about correcting this?

# step 1 is adding entries in the pid_changes table

db$pid_changes <- list(
  list(
    old_id = "B",
    active_id = "A"
  )
) %>% bind_rows() %>% as.data.frame(stringsAsFactors = FALSE)

any(db$data1$pid %in% db$pid_changes$old_id) # TRUE

pid_updates <- flag_pid_changes(db$data1, db$pid_changes, name = "data1")
db$data1 <- update_pids(db$data1, "pid", pid_updates)

any(db$data1$pid %in% db$pid_changes$old_id) # FALSE!

# we've fixed the duplication in the data, but the people and visits tables both have duplicated entries
# here we can use the merge_pids function to combine entries

db$people <- merge_pids("B", "A", db$people, verbose = FALSE)

# notice that the vid from the entry that used to have pid B is now correctly linked to the corresponding visit in the visits table

# at this point, the pids and vids in data1 are "clean", in the sense that we can check:

all(db$data1$pid %in% db$people$pid)

# now lets turn our attention to data2

# our search for duplications turns up the following:
# first, we notice is that there is already a "retired" PID inside data2, "B"
# in the process of cleaning, we also notice that "C" is also another alias for A, which we want to update
# finally, we notice from the evidence here that "E" is a duplication of "D" in the people table (and should be retired)

# as always, the first thing is to update the pid_changes list

list(
  old_id = "C",
  active_id = "A"
) %>% bind_rows(db$pid_changes, .) -> db$pid_changes
list(
  old_id = "E",
  active_id = "D"
) %>% bind_rows(db$pid_changes, .) -> db$pid_changes

# as before, we apply pid_changes to our data frame

any(db$data2$pid %in% db$pid_changes$old_id) # TRUE

pid_updates <- flag_pid_changes(db$data2, db$pid_changes, name = "data2")
db$data2 <- update_pids(db$data2, "pid", pid_updates)

# what about on parent columns?? need to include in tutorial too
# its like any data column, need to be flexible about what its called, not necessarily "pid"!

db$people <- merge_pids("C", "A", db$people, verbose = FALSE)
db$people <- merge_pids("E", "D", db$people, verbose = FALSE)

# confirm they are good
all(db$data2$pid %in% db$people$pid)

# so the database is now actually usable!
# this depends critically on being able to make the link to pid_changes table

# tutorial 2: resolving collisions

# now we introduce a new kind of problem:
# one id is actually serving as the identifier for two people in several cases
# we have to retire collided ids, into the id_changes log
# update the data file(s) affected with those ids




# in the first case, we realize the collision inside the data file itself
# e.g. A is Alex and Amanda too

# in the second case, the collision is *between* tables, A is Amanda in people, Alex in the other

# and in the third case, the collision was created by previous duplication resolution on accident
# so now its in the id_changes as a mistake

db <- list()

db$people <- list(
  list(
    pid = "D",
    name = "Danielle",
    female = 1
  ),
  list(
    pid = "G",
    name = "Gabrielle",
    female = 1
  ),
  list(
    pid = "H",
    name = "Henriette",
    female = 1
  ),
  list(
    pid = "I",
    name = "Isabel",
    female = 1
  )
) %>% bind_rows() %>% as.data.frame(stringsAsFactors = FALSE)

# key point: do not change 'primary source' info like sex, age, and *name*
# in the data tables -> you lose the ability to identify problems later

db$data1 <- list(
  list(
    obs_id = 1,
    date = "2020-02-01",
    pid = "G",
    name = "Danielle",
    y = 2,
    notes = "Danielle is not pid G, she is in a collision"
  ),
  list(
    obs_id = 2,
    date = "2020-02-02",
    pid = "D",
    name = "Danielle",
    y = 2
  ),
  list(
    obs_id = 3,
    date = "2020-02-03",
    pid = "H",
    name = "Henriette",
    y = 4
  ),
  list(
    obs_id = 4,
    date = "2020-02-02",
    pid = "I",
    name = "Isabel",
    y = 4
  ),
  list(
    obs_id = 5,
    pid = "G",
    date = "2020-02-05",
    name = "Gabrielle",
    y = 10
  )
) %>% bind_rows() %>% as.data.frame(stringsAsFactors = FALSE)

db$data2 <- list(
  list(
    obs_id = 1,
    date = "2020-02-02",
    pid = "H",
    name = "Isabel",
    x = 45,
    notes = "this is a collision between Isabel and Henriette, but Henriette doesn't have an entry in data2; this is supposed to be visit II9"
  ),
  list(
    obs_id = 2,
    date = "2020-02-05",
    pid = "G",
    name = "Gabrielle",
    x = 45
  )
) %>% bind_rows() %>% as.data.frame(stringsAsFactors = FALSE)


# so here are our problems:
# 1. Danielle incorrectly has Gabrielle's pid in the data1 table
# 2. Isabel incorrectly has Henriette's pid in the data2 table

# we can detect data1 prpblems by just looking at it
# data2 collision can only be detected by comparing against the people table

# generally speaking, collision resolution is a massive pain
# probably better to just start over with a new key and leave many duplications
# but often we have no choice

# another problem is the nature of the collision
# if you are working with only one set of files, you can just put each pid back to where it is
# but when working on a large corpus of files, a typo might appear more than once
# if there's any chance a collided pid you found in one file can show up in another file, we *need* to flag this automatically. the correct solution in this more careful approach is to *retire* the collided pid, add it to the id_changes log, thus forcing us to figure out who is who (or identify when we simply cannot tell from the available evidence)

db$id_changes <- list(
  list(
    old_id = "G",
    active_id = "L",
    name = "Gabrielle"
  ),
  list(
    old_id = "G",
    active_id = "D",
    name = "Danielle"
  ),
  list(
    old_id = "H",
    active_id = "N",
    name = "Henriette"
  ),
  list(
    old_id = "H",
    active_id = "I",
    name = "Isabel"
  )
) %>% bind_rows() %>% as.data.frame(stringsAsFactors = FALSE)

# one method: we can use exact name matches
# provided there's no chance of a same-name collision

cdm_key <- paste(db$id_changes$old_id, db$id_changes$name)

data1_key <- paste(db$data1$pid, db$data1$name)
db$data1$pid[data1_key %in% cdm_key]  <- db$id_changes$active_id[match(data1_key[data1_key %in% cdm_key], cdm_key)]

data2_key <- paste(db$data2$pid, db$data2$name)
db$data2$pid[data2_key %in% cdm_key]  <- db$id_changes$active_id[match(data2_key[data2_key %in% cdm_key], cdm_key)]

# this is great, but it depends on an exact identification of individuals
# this isn't usually possible due to: (a) non-unique names, (b) variant spellings of the same name
# thats where fuzzy matching comes in, 

# the end goal is a set of matches for all ids in the old_id column in the dataset and a single entry in the id_changes table
# then we just need to apply all those simultaneously!

db$people <- update_pid("G", "L", db$people)
db$people <- update_pid("H", "N", db$people)

# now we have the ability to clean any table with this same typo relatively quickly

all(db$data1$pid %in% db$people$pid)
all(db$data2$pid %in% db$people$pid)


# should never happen
!any(db$id_changes$old_id %in% db$id_changes$active_id)

# should always happen (except vids!)
all(db$id_changes$active_id[db$id_changes$type == "pid"] %in% db$people$pid)



# tutorial 3: adding new observations, people, visits and data tables

# in the case there are already pids, they have to be checked for duplications and collisions as above
# if there are *no* pids, then we assign new ones, and try to resolve any duplications as in tutorial 1




# tutorial 4: doing everything at once, connecting across visits

its useful to have the ability to query similar data
this is hard b/c (a) dates are not exact, (b) the month-day reversal is often a problem
we can define a visit with a vid, and store that as a separate table
vid is tied to a pid tho, if we reassign the pid the vid should change?
honestly the rules arent super-clear at the moment...prob need a thought experiment for the 'true' vid too
we absolutely need an observation id, one that will not change inside a table once assigned



# we can build up a sequence of steps using the above worked examples

# 0. standard cleaning of data: variable names, bad characters, bad dates, etc.
# 1. (as above) in the data file, identify new pid duplications (e.g. two observations, same name, same metadata, different dates) and fix id_changes table
# 2. (as above) in the data file, identify new interal and external collisions and fix id_changes table
# 3. (as above) apply the id_changes files to the remaining pids not in the people table
# 4. assign/add all remaining pids to people table as new people
# 5. match vids against the visits table
# 6. assign/add new visits to the visits table

# here's a thought experiment: imagine that everyone has a 'true' pid which you cannot normally observe but can for testing purposes
# then you'd want to use the id_changes log to pull out the active_pid by matching on true pid, for every table all the time
# it would just be a one-line operation, basically
# the problem is that takes time

# we need a single updates object that links id_changes entries row by row for every row in the original data table
# this DEPENDS on the existence of a stable primary key in that table tho!!
# will work for both duplications and collisions, methinks...


# the end result is a patch log that updates all tables
# under certain circumstances, id_changes can be directly applied to people, to visits, and to the data file
#

























############

# BASIC OPERATIONS:

# DUPLICATION IDENTIFIED:
# we need to combine 2+ entries on the people table into one
# all old ids are added to the id_changes table
# then we just update the data according to the id_changes table

# COLLISION IDENTIFIED:
# if we decide to retire the pid, we create new entries in the id_changes table
# one for each person in the collision, including the 'real' holder of that retired pid
# then we just update the data according to the id_changes table

############

# we have to be able to uniquely address data entries
# that's been the fundamental problem will of this work, a lack of primary key for observations
# obsID - one per interview!

# input: data table to be cleaned, people, visits, id_changes

# 1. update database pids based on problems identified below
# append_id_change() applied to id_changes table
# update_pid() applied to people to change one person's pid
# merge_pids() applied to people for DUPLICATIONS
# add de-collided people to the pop reg like in step 5

# 2. detect pid collisions inside file
## uses detect_pid_collisions() to generate potential collision list

# 3. update old_ids from id_changes log
## involves scripted changes to the ids in the data file
## protocol:
## (a) check if any same-name collision pids are in data file, manually fix
## (b) match on exact name and auto-update pids from id_changes table
## (c) for remaining pids in the old_id column, adjudicate which id they should be
## (d) for non-adjudicated ids, auto-update the rest?

# we also have to clean up vids during step 3

# 4. detect pid collisions against the people table
## uses detect_pid_collisions() to generate potential collision list

# 5. assign pids and add new people to people table
## uses make_ids()

# 6. match vids to visits table
## uses match_vids()

# 7. assign vids and add new visits to visits table
## uses make_ids()

# output: cleaned id_changes, people, visits, and data files