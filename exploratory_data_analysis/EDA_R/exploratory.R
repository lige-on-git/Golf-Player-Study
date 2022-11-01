# Exploratory data analysis: Lige
# Tables:

setwd("/media/lige/Samsung2TB/Projects of Lige/DataJam Competition")
source("./Golf-Player-Study/exploratory_data_analysis/EDA_R/EDA_config.R")

## golfer table
golfer <- fread(golfer_path, select=get_field_names("golfer"))
head(golfer)

# - many rows share the same <golflinkno>, but each row has an unique <golferid>
golfer[, .(number = .N), by=golflinkno][number>10]
golfer[golflinkno == 999091090]  # e.g. this <golflinkno> belongs to a single golfer who has many golferid
nrow(golfer[golflinkno == 999091090])
nrow(golfer)

## membership table
membership <- fread(membership_path, select=get_field_names("membership"))
head(membership)

## join two tables
golfer_and_membership <- membership[golfer, on = c("golferid")]
head(golfer_and_membership)

golfer_and_membership[golflinkno == 999091090, .(isvalidmember)]  # this <golflinkno> has null membership, can remove

# - split member and non-member
members <- golfer_and_membership[isvalidmember==TRUE]
non.members <- golfer_and_membership[isvalidmember==FALSE]
head(members)
head(non.members)

# - after splitting, those <golflinkno> with high repeats disappears
max(members[, .N, by=golflinkno])  # 15
max(non.members[, .N, by=golflinkno])  # 38

# - e.g. for this <golflinkno>, the repeating rows actually represent different golfers (from the same club)
# - but for other <golflinkno>, this is not the case
example_golfer <- non.members[, .N, by=golflinkno][N==38, golflinkno]
non.members[golflinkno==example_golfer]

# - select rows that represent non member and have repeating <golflinkno>
example_golfers <- non.members[, .N, by=golflinkno][N>5, golflinkno]  # their <golflinkno>
non.members[golflinkno==example_golfers[4]]
non.members[golflinkno==example_golfers[5]]

# - conclude that most repeating <golflinkno> share the same <golferid>
# - this means the same golfer (by golferid) can plan in different clubs by has the same handicap results
# - can assign aggregated values to those golfers (e.g. played in how many clubs), 
# - we can safely treat (<golflinkno>, <golferid>) pair so simply <golferid> as the unique identifier of a golfer
indices <- which(non.members[,golflinkno] %in% example_golfers)
unique(non.members[indices], by=c("golferid", "golflinkno"))[, .N, by=golflinkno][N>1]
non.members[golflinkno==5021020]
non.members[golflinkno==9011939919]  # only these two, a <golflinkno> has multiple <golferid>
