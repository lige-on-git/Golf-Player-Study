setwd("/media/lige/Samsung2TB/Projects of Lige/DataJam Competition")
source("./Golf-Player-Study/exploratory_data_analysis/EDA_R/EDA_config.R")

## Sampled 100k golferid from the handicap table
data.100k <- fread(handicap_100k_path)
head(data.100k)


## ________________________________________________________________
## Remove records with invalid score status
data.100k[, .N, by=scorestatus]
valid_status <-  c("O", "A", "N", "E", "D")  # only keep these value status
data.100k <- data.100k[scorestatus %in% valid_status]; gc()  # free memory of the old data.100k table
data.100k[, .N, by=oldhandicap]


## ________________________________________________________________
## Remove rows with both NA oldhandicap and newhandicap

# -display rows to keep
nrow(data.100k)
data.100k[is.na(oldhandicap) & is.na(newhandicap), .N]
data.100k[! (is.na(oldhandicap) & is.na(newhandicap)), .N]

# -shrink data
data.100k <- data.100k[! (is.na(oldhandicap) & is.na(newhandicap))]; gc()

# -this also works (but not as elegant)
# null.indices <- data.100k[is.na(oldhandicap) & is.na(newhandicap), handicaphistoryid]
# data.100k <- data.100k[handicaphistoryid %notin% null.indices]


## ________________________________________________________________
## Replace NA newhandicap with the current value of oldhandicap
data.100k[is.na(newhandicap)]
data.100k[is.na(newhandicap), .N]
data.100k[is.na(newhandicap), newhandicap := oldhandicap]  # in-place replacement
data.100k[handicaphistoryid %in% c(135634642, 152975210, 122171692)]  # sanity check

# -in data.table no need to identify rows using "indices"
# nullNew.indices <- data.100k[is.na(newhandicap), handicaphistoryid]
# data.100k[handicaphistoryid %in% nullNew.indices, newhandicap := oldhandicap]


## ________________________________________________________________
## Replace NA oldhandicap with the previous non-null value of newhandicap

# -some groups do not have rows ordered by time (dateadjusted), need to order it first
data.100k[golferid==792549356]

# -order by <dateadjusted> in descending order - within each group and among all groups!!
# -only choose the first 2 rows of each group
data.100k[order(-dateadjusted), .(head(handicaphistoryid,2), head(oldhandicap,2), head(newhandicap,2), head(dateadjusted,2)), by = golferid]

# -total number of null oldhandicap
data.100k[is.na(oldhandicap), .N]

# -each group how many null oldhandicap
data.100k[is.na(oldhandicap), .N, by = golferid]

# -store 1-row shifted newhandicap in a new column 
data.100k[order(dateadjusted), shifted := shift(newhandicap), by = golferid]

# -check: since called "shift" together with "by", it shifts rows within each group only
data.100k[order(dateadjusted), .(head(shifted,3), head(dateadjusted,3)), by = golferid]

# -define a function
replace.NaOld <- function(old, shifted){
  # if a row has null oldhandicap, replace it with the previous non-null newhandicap
  if(is.na(old)){
    return(shifted)
  }
  # else do not change the oldhandicap
  else{
    return(old)
  }
}

# -can't use this!!
# -will pass entire <oldhandicap> and <shifted> values per group (since called by=golferid; otherwise will pass entire values of the table)
# -this doesn't matter for functions like mean(oldhandicap) and (oldhandicap+shifted),
# -but isn't suitable for functions like is.na(oldhandicap) (since didn't wrapped within a data.table, is.na() will only check null value of the first element of <oldhandicap>)
# data.100k[order(dateadjusted), oldhandicap.filled:=replace.NaOld(oldhandicap, shifted), by=golferid]

# -can use mapply instead
# -will pass inputs <oldhandicap> and <shifted> row by row (will be slower but works as intended)
data.100k[order(dateadjusted), oldhandicap.filled := mapply(replace.NaOld, oldhandicap, shifted), by=golferid]
print(data.100k[order(dateadjusted), .(golferid, newhandicap, oldhandicap, oldhandicap.filled, shifted, dateadjusted)][golferid==791073520], nrow=200)  # sanity checks
print(data.100k[order(dateadjusted), .(golferid, newhandicap, oldhandicap, oldhandicap.filled, shifted, dateadjusted)][golferid==791279940], nrow=200)


## Export data
fwrite(data.100k, "./downloaded-data/cleaned_data/100K golferid cleaned.csv")
