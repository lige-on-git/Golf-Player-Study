setwd("/media/lige/Samsung2TB/Projects of Lige/DataJam Competition")
source("./Golf-Player-Study/exploratory_data_analysis/EDA_R/EDA_config.R")

golfers <- fread(handicap_cleaned_path)
head(golfers)
unique(golfers[,scorestatus])

# calculate number of rounds
golfers <- golfers[on='golferid', golfers[scorestatus=="O",.N,by=c('golferid')]]
setnames(golfers, old='N', new='approved_rounds')
gc()

golfers <- golfers[on='golferid', golfers[scorestatus!="O",.N,by=c('golferid')]]
setnames(golfers, old='N', new='nonapproved_rounds')
gc()

golfers[, all_rounds := .(approved_rounds + nonapproved_rounds)]
golfers[,.(golferid, approved_rounds, nonapproved_rounds, all_rounds)]

# DON'T USE THIS IN THE FUTURE: https://stackoverflow.com/questions/12786335/why-is-as-date-slow-on-a-character-vector
extract.time <- function(time.stamp, date.type){
  # to extract year or month from a time stamp
  # <type>: "m" for month and "Y" for year
  time.format <- paste0("%",date.type)
  new.date <- as.Date(time.stamp, "%m/%d/%Y")
  as.integer(strftime(new.date, time.format))
}

# find the longest gap
golfers[, month := lapply(dateadjusted, FUN=extract.time, "m")]
golfers[, year := lapply(dateadjusted, FUN=extract.time, "Y")]
golfers[, .(golferid, month, year)]

fwrite(golfers, "temp.csv")
golfers <- fread("temp.csv")

months.played <- golfers[, month.adj := .(month+12*year)]
months.played <- months.played[order(year, month), .SD, by=golferid]  # don't call <order> when in-place creating a new column 
months.played[, gap := .(month.adj - shift(month.adj)), by=golferid]

head(months.played)  # sanity checks
months.played[gap>3]
months.played[golferid %in% c(789979119,139632850) & year %in% c(2006, 2007), .SD, by=golferid]
months.played[golferid==791424820 & year==2018][order(year)]

longest.gap <- function(DT){
  gaps <- DT[, gap]
  indices <- c(1, which(gaps > 3), length(gaps))  # must add the first and last indices!!
  max(indices - shift(indices, fill=0))
}
longest.round <- months.played[, .(longest_round = longest.gap(.SD)), by=golferid]
longest.round[golferid==789608883]
longest.round[golferid==789609155]

golfers <- golfers[on='golferid', longest.round]; gc()
golfers[, ':=' (i.N = NULL, month.adj = NULL)]
head(golfers)

fwrite(golfers, "./downloaded-data/cleaned_data/100K golferid enriched.csv")


## _____________________________________________________________________________________________
## Create response variable
responses <- golfers[!year==2022, .(round_num = .N), by=c('golferid', 'year')]

is.active <- function(curr_year, DT, threshold=0){
  # check if the next year is active (round_num not empty in the input table, or greater than a threshold)
  next_year <- curr_year + 1
  is.played <- length(DT[year==next_year, round_num])
  if(is.played){
    return(TRUE)  # inactive
  }else{
    return(FALSE)   # active
  }
}
responses[, is_active := .(lapply(year, FUN=is.active, .SD, threshold=0)), by=golferid]
fwrite(responses, "./downloaded-data/cleaned_data/100K golferid responses.csv")


## _____________________________________________________________________________________________
## check if isninehole is an important feature to influence the response
responses <- fread("./downloaded-data/cleaned_data/100K golferid responses.csv")
golfers <- fread("./downloaded-data/cleaned_data/100K golferid enriched.csv")
head(golfers)
names(golfers)

responses_and_isninehole <- responses[unique(golfers[, .(isninehole), by=c('golferid', 'year')]), 
                                      on=c('golferid', '')][!is.na(is_active)]

base.model <- glm(is_active~isninehole, family=binomial, data=responses_and_isninehole)
summary(base.model)


## Add more aggregated features 
golfers.agg <- unique(golfers[, .(approved_rounds, nonapproved_rounds, longest_round), by=c('golferid', 'year')])
golfers.agg[, approved_round_ratio := .(approved_rounds/(approved_rounds + nonapproved_rounds))]
head(golfers.agg)

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
golfers[golferid==791053708 & (!par==73), par]

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

head(newhandicap.change)
newhandicap.change[golferid==792168569 & year==1981]
golfers[golferid==792168569 & year==1981]

newhandicap.change[golferid==789609155]
golfers[golferid==789609155 & year==2000]

newhandicap.change[golferid %in% c(792517184,792087835,789609155), head(.SD), by=golferid]

fwrite(newhandicap.change, "newhandicap_change_temp.csv")
newhandicap.change <- fread("newhandicap_change_temp.csv")

# change sd columns with NA value (since only a single record found) to 0
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
newhandicap.change <- fread("newhandicap_change_temp.csv")  # read again

for (i in 1:length(feature.means)){
  feature.mean <- feature.means[i]
  feature.sd <- feature.sds[i]  # if mean isn't null but sd is null (sd of a single value is NA is R)
  newhandicap.change[is.na(get(feature.sds[i])) & !(is.na(get(feature.means[i]))), (feature.sd) := 1]
}

newhandicap.change[, mget(c(feature.means, feature.sds))]  # sanity check








# playinghandicap seems to only appear after 2013 - so we discard records before 2013 to only consider recent years





# unique(golfers[,golferid])
# golfers[golferid==792168569, (par)]
# golfers[golferid==792168569, (year)]






#

