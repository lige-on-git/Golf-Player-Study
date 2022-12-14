setwd("/media/lige/Samsung2TB/Projects of Lige/DataJam Competition")
source("./Golf-Player-Study/exploratory_data_analysis/EDA_R/EDA_config.R")

golfers <- fread(handicap_cleaned_path)
head(golfers)
unique(golfers[,scorestatus])

# DON'T USE THIS IN THE FUTURE - as.Date() function is way too slow!!
# https://stackoverflow.com/questions/12786335/why-is-as-date-slow-on-a-character-vector
extract.time <- function(time.stamp, date.type){
  # to extract year or month from a time stamp
  # <type>: "m" for month and "Y" for year
  time.format <- paste0("%",date.type)
  new.date <- as.Date(time.stamp, "%m/%d/%Y")
  as.integer(strftime(new.date, time.format))
}

golfers[, month := lapply(dateadjusted, FUN=extract.time, "m")]
golfers[, year := lapply(dateadjusted, FUN=extract.time, "Y")]

fwrite(golfers, "./temp_data/golfer_gap_temp.csv")  # add a checkpoint; need to restart R session to recover speed...
golfers <- fread("./temp_data/golfer_gap_temp.csv")
golfers[, .(golferid, month, year)]

# golfers[, ':=' (all_rounds=NULL, nonapproved_rounds=NULL, approved_rounds=NULL, approved_round=NULL, i.N=NULL)]
# head(golfers)


# calculate number of rounds
# here, we put the golfers table on the "left" to apply left join, so the joined table keeps all rows of the golfers table
# https://www.w3schools.com/sql/sql_join.asp
golfers <- golfers[scorestatus=="O", .N, by=c('golferid', 'year')][on=c('golferid', 'year'), golfers]
setnames(golfers, old='N', new='approved_rounds')
golfers[is.na(approved_rounds), approved_rounds := 0]  # using 0 to fill NA values caused by left join 
gc()
nrow(golfers)

golfers <- golfers[scorestatus!="O", .N, by=c('golferid', 'year')][on=c('golferid', 'year'), golfers]  # only select .N to join
setnames(golfers, old='N', new='nonapproved_rounds')
golfers[is.na(nonapproved_rounds), nonapproved_rounds := 0]
gc()
nrow(golfers)

golfers[, all_rounds := .(approved_rounds + nonapproved_rounds)]
head(golfers[,.(golferid, approved_rounds, nonapproved_rounds, all_rounds, month, year)])
nrow(unique(golfers[, .(golferid, year)]))  # check the number of unique (golferid, year) pairs


# find the longest gap - this even works if not grouping by <year> thanks to <month.adj>
golfers[, month.adj := .(month+12*year)]                        # don't call <order> when in-place creating a new column 
months.played <- golfers[order(year, month), .SD, by=golferid]  # because after order(), a new table is created
months.played[, gap := .(month.adj - shift(month.adj)), by=c('golferid', 'year')]

head(months.played)  # sanity checks
months.played[gap>3]
months.played[golferid %in% c(789979119,139632850) & year %in% c(2006, 2007), .SD, by=c('golferid', 'year')]
months.played[golferid==791424820 & year==2018][order(year)]

longest.gap <- function(DT){
  gaps <- DT[, gap]
  indices <- c(1, which(gaps > 3), length(gaps))  # must add the first and last indices!!
  max(indices - shift(indices, fill=0))
}
longest.round <- months.played[, .(longest_round = longest.gap(.SD)), by=c('golferid', 'year')]
longest.round[golferid==789608883]
months.played[golferid==789608883, .(year, month, all_rounds, gap)][order(year, month)]  # sanity checks
longest.round[golferid==789609155]
longest.round[golferid==789609155, sum(longest_round)]

nrow(longest.round); nrow(golfers)  # still left join longest.round table to golfers table

golfers <- longest.round[on=c('golferid', 'year'), golfers]; gc()
golfers[, month.adj := NULL]  # remove redundant column
head(golfers)
nrow(golfers)
nrow(unique(golfers[, .(golferid, year)]))  # check the number of unique (golferid, year) pairs

fwrite(golfers, "./downloaded-data/cleaned_data/100K golferid enriched.csv")


## _____________________________________________________________________________________________
## Create response variable
responses <- golfers[!year==2022, .(round_num = .N), by=c('golferid', 'year')]

is.active <- function(curr_year, DT, threshold=0){
  # check if the next year is active (round_num not empty in the input table, or greater than a threshold)
  next_year <- curr_year + 1
  is.played <- length(DT[year==next_year, round_num])  # check if the "next year" exists in the DT; use length() to check logical(0)
  if(is.played){  # same as Python: if(0) is same as if(FALSE) 
    return(TRUE)  # inactive
  }else{
    return(FALSE)   # active
  }
}
responses[, is_active := .(lapply(year, FUN=is.active, .SD, threshold=0)), by=golferid]
fwrite(responses, "./downloaded-data/cleaned_data/100K golferid responses.csv")


## _____________________________________________________________________________________________
## check if isninehole is an important feature to influence the response
responses <- fread(isactive_response_path)
golfers <- fread(handicap_enriched_path)
head(golfers)
names(golfers)
nrow(golfers)

responses_and_isninehole <- responses[unique(golfers[, .(isninehole), by=c('golferid', 'year')]), 
                                      on=c('golferid', '')][!is.na(is_active)]

base.model <- glm(is_active~isninehole, family=binomial, data=responses_and_isninehole)
summary(base.model)


## _____________________________________________________________________________________________
## Add more aggregated features
responses <- fread(isactive_response_path)
golfers <- fread(handicap_enriched_path)

# - first flatten golfers table so each row is a golfer
unique(golfers[, .(golferid, year)])  # 300090 unique (golferid, year) pair
golfers.agg <- unique(golfers[, .(approved_rounds, nonapproved_rounds, longest_round), by=c('golferid', 'year')])
golfers.agg[, approved_round_ratio := .(approved_rounds/(approved_rounds + nonapproved_rounds))]
golfers.agg[is.na(approved_round_ratio)]  # check invalid rows

# add number of years played
golfers.agg[order(year), year_from_beginner := .(year - head(year,1)), by=golferid]
golfers.agg[order(year), the_ith_year2play := 1:.N]  # implicitly create "index" within each group
head(golfers.agg,20)

# latest new_handicap score - the oldest score
# average and SD of new_handicap (GA Handicap)
# total number of rounds
# maximum new_handicap - minimum score for each golfer

# Average and SD nettscore
# Average and SD score 
# Average and SD par score for each golfer
# Number of distinct par score for each golfer
# Average and SD playinghandicap
# Average and SD scratch rating

# ratio of new_handicap score improvement ((new_handicap - old_handicap) / old_handicap)
# ratio of each scoretype

scoretype.ratio <- function(DT, score.type){
  # calculate ratio of each input score type
  # <score.type>: 1, 2, 3, or 4
  total.rounds <- nrow(DT)
  type.count <- nrow(DT[scoretype==score.type])
  if(total.rounds>0){
    return(round(type.count / total.rounds, 3))
  }else{
    return(0)
  }
}

# Number of change of par scores (per each year)
avg.par.changes <- function(DT){
  par.hist <- DT[, par]
  initial.par <- par.hist[1]
  length(which(! par.hist == shift(par.hist, fill=initial.par))) # num of changes
}
avg.par.changes(golfers[golferid==791053708])
golfers[golferid==791053708 & (!par==73), par]  # tests

newhandicap.change <- golfers[order(dateadjusted), 
                            .(average_newhandicap = mean(newhandicap, na.rm=TRUE),  # skip null values
                             sd_newhandicap = sd(newhandicap, na.rm=TRUE),
                             total_num_rounds = .N,
                             newhandicap_change = tail(newhandicap, 1) - head(newhandicap, 1),
                             oldest_newhandicap = head(newhandicap, 1),
                             newhandicap_amplitude = max(newhandicap) - min(newhandicap),
                             scoretype1_ratio = scoretype.ratio(.SD, 1),
                             scoretype2_ratio = scoretype.ratio(.SD, 2),
                             scoretype3_ratio = scoretype.ratio(.SD, 3),
                             scoretype4_ratio = scoretype.ratio(.SD, 4),
                             average_nettscore = mean(nettscore, na.rm=TRUE),  # skip null values
                             sd_nettscore = sd(nettscore, na.rm=TRUE),
                             average_score = mean(score, na.rm=TRUE),  # skip null values
                             sd_score = sd(score, na.rm=TRUE),
                             average_par = mean(par, na.rm=TRUE),  # skip null values
                             sd_par = sd(par, na.rm=TRUE),
                             distinct_par = length(unique(par)),
                             average_playinghandicap = mean(handicapscore, na.rm=TRUE),  # skip null values
                             sd_playinghandicap = sd(handicapscore, na.rm=TRUE),
                             average_scratchrating = mean(rating, na.rm=TRUE),  # skip null values
                             sd_scratchrating = sd(rating, na.rm=TRUE),
                             average_sloperating = mean(sloperating, na.rm=TRUE),  # skip null values
                             sd_sloperating = sd(sloperating, na.rm=TRUE),
                             par_change_num = avg.par.changes(.SD)),
                             by=c('golferid', 'year')]

newhandicap.change[, ':=' (newhandicap_change_ratio = newhandicap_change/oldest_newhandicap,
                          newhandicap_change_amp_ratio = abs(newhandicap_change/newhandicap_amplitude))]
newhandicap.change[is.na(newhandicap_change_amp_ratio) & newhandicap_amplitude==0, 
                   newhandicap_change_amp_ratio:=0]  # remove NA caused by dividing 0

head(newhandicap.change)  # sanity checks
newhandicap.change[golferid==792168569 & year==1981]
golfers[golferid==792168569 & year==1981]

newhandicap.change[golferid==789609155]
golfers[golferid==789609155 & year==2000]

newhandicap.change[golferid %in% c(792517184,792087835,789609155), head(.SD), by=golferid]

fwrite(newhandicap.change, "./temp_data/newhandicap_change_temp.csv")  # add a checkpoint
newhandicap.change <- fread("./temp_data/newhandicap_change_temp.csv")

## change sd columns with NA value (since only a single record found) to 0

feature.means <- c('average_newhandicap', 'average_nettscore', 'average_score', 'average_par', 
                   'average_playinghandicap', 'average_scratchrating', 'average_sloperating')
feature.sds <- c('sd_newhandicap', 'sd_nettscore', 'sd_score', 'sd_par', 'sd_playinghandicap',
                 'sd_scratchrating', 'sd_sloperating')

# pass a column name as string (https://stackoverflow.com/questions/12391950/select-assign-to-data-table-when-variable-names-are-stored-in-a-character-vect)

# - select using get() and mget() - no need to add .() when select using mget()
newhandicap.change[is.na(get(feature.sds[7])) & !(is.na(get(feature.means[7]))), mget(feature.means[1:2])]  

# - select using 'dot dot' (..) prefix
colname1 <- feature.sds[7]  # must pass as a single variable
newhandicap.change[is.na(get(feature.sds[7])) & !(is.na(get(feature.means[7]))), ..colname1]

# - in-place creating a new column or editing old values (can't use get() or mget())
colname2 <- 'new_column'
newhandicap.change[, (colname2) := 2]
newhandicap.change[, ..colname2]
newhandicap.change[, (colname2) := NULL]

# let's get back to work...
newhandicap.change <- fread("./temp_data/newhandicap_change_temp.csv")  # read again

for (i in 1:length(feature.means)){
  feature.mean <- feature.means[i]
  feature.sd <- feature.sds[i]  # if mean isn't null but sd is null (sd of a single value is NA is R)
  newhandicap.change[is.na(get(feature.sds[i])) & !(is.na(get(feature.means[i]))), (feature.sd) := 1]
}

newhandicap.change[, mget(c(feature.means, feature.sds))]  # sanity check


## Join two aggregated feature tables
newhandicap.change[, .(golferid, year)]
golfers.agg[, .(golferid, year)]  # check: they have the same number of rows
names(newhandicap.change)
names(golfers.agg)

golfers.agg <- golfers.agg[on=c('golferid', 'year'), newhandicap.change]
fwrite(golfers.agg, "./downloaded-data/cleaned_data/100K golferid aggregated part1.csv")


# Some checks: - most of the playinghandicap NA values are before 2013 
#              - so we may discard records before 2013 to only consider recent years
golfers.agg <- fread(handicap_aggregated_1_path)
golfers.agg[is.na(average_playinghandicap) & year>2013, .(year, average_playinghandicap)]
golfers.agg[is.na(average_playinghandicap) & year<=2013, .(year, average_playinghandicap)]
