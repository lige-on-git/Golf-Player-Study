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



## _____________________________________________________________________________________________
## Fit the aggregated handicap table and response

responses <- fread(isactive_response_path); nrow(responses)  # less rows as 2022 not included
golfers.agg <- fread(handicap_aggregated_1_path); nrow(golfers.agg)

responses[is_active==TRUE & year>2013]
responses[is_active==FALSE  & year>2013]

# - left join golfers.agg to responses so each (golferid, year) pair is associated with an response
full.xy.table <- golfers.agg[on=c('golferid', 'year'), responses]; nrow(full.xy.table)


# - is.na() can apply to a vector or a list (so work for data.table); is.finite() can only apply to a vector/column

full.xy.table[is.infinite(newhandicap_change_ratio)==TRUE]  # is.infinite() works for a single vector/column
full.xy.table[, is.infinite(newhandicap_change_ratio)]

full.xy.table[, lapply(.SD, FUN = is.infinite)]  # apply to all columns - return a data.table
is.na(full.xy.table)  # return a Boolean matrix if call is.na() outside (is.finite() and is.infinite() NOT WORK for a list/data.table)


# - check percentage of NA values in each column
(colSums(is.na(full.xy.table)) / nrow(full.xy.table)) * 100  # calculate column sum/mean without grouping-by
                                                             # using colSums(); similarly there's colMeans()

full.xy.table[, lapply(.SD, FUN = sum)]  # sum() and mean() also only apply to a single vector/column,
                                         # so use lapply to calculate sum/mean of each column without grouping-by
na.nums <- function(col){
  sum(is.na(col))
}
full.xy.table[, lapply(.SD, FUN=na.nums)]  # use lapply to trick to calculate number of NA values in each column


# - check how many infinite numbers
inf.nums <- function(col){
  sum(is.infinite(col))  # unlike is.na(), can't call is.infinite(.SD) because it doesn't work for R list/data.table
}                        # so wrap it in a function and use <lapply> to apply <on each column> of a list/data.table
full.xy.table[, lapply(.SD, inf.nums)]


# - remove 4 columns with too many NA's
full.xy.table[, ":=" (average_playinghandicap=NULL, sd_playinghandicap=NULL,
                      average_sloperating=NULL, sd_sloperating=NULL)]


# - change all Inf values to NA, and then remove all rows containing NA 

for(col.name in names(full.xy.table)){
    
  if (full.xy.table[, inf.nums(get(col.name))] > 0){  # select using get(string)
    full.xy.table[is.infinite(get(col.name)), (col.name) := NA]  # assign using (string)
  }
}

full.xy.table <- na.omit(full.xy.table)



## _____________________________________________________________________________________________
## train some models

# - split training and testing data
sample <- sample(c(TRUE, FALSE), nrow(full.xy.table), replace=TRUE, prob=c(0.7,0.3))
train.xy  <- full.xy.table[sample, ]; nrow(train.xy)
test.xy   <- full.xy.table[!sample, ]; nrow(test.xy)

# - fit model
names(test.xy)
train.bin <- glm(as.factor(is_active) ~ . -golferid -year, 
                 family=binomial, data=test.xy, na.action=na.exclude)

summary(train.bin)

