# Exploratory data analysis: Lige
# Tables:

setwd("/media/lige/Samsung2TB/Projects of Lige/DataJam Competition")
source("./Golf-Player-Study/exploratory_data_analysis/EDA_config.R")

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

# - split member and non-member
members <- golfer_and_membership[isvalidmember==TRUE]
non.members <- golfer_and_membership[isvalidmember==FALSE]
head(members)

# - after splitting, those <golflinkno> with high repeats disappears
max(members[, .N, by=golflinkno])  # 15
max(non.members[, .N, by=golflinkno])  # 38

# - e.g. for this <golflinkno>, the repeating rows actually represent different golfers (from the same club)
# - so might now be safe to treat golferid as the unique identifier of a golfer
example_golfer <- non.members[, .N, by=golflinkno][N==38, golflinkno]
non.members[golflinkno==example_golfer]
