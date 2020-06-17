
# basic scenario:
# there's a table of visits with various identifying info
# mostly names and dates and pids

library(longformer)

people <- list(

) %>% as.data.frame(stringsAsFactors = FALSE)

visits <- list(

) %>% as.data.frame(stringsAsFactors = FALSE)

id_changes <- list(

) %>% as.data.frame(stringsAsFactors = FALSE)

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