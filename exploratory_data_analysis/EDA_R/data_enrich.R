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

golfers[, month := lapply(dateadjusted, FUN=extract.time, "m")]
golfers[, year := lapply(dateadjusted, FUN=extract.time, "Y")]
golfers[, .(golferid, month, year)]

golfer <- fread("temp.csv")


# golfers[golferid==139632850, .(dateadjusted, month)]

# golfers[, .N, by=c("golferid", "month", "year")][order(year, month),.(month.adj = month+12*year, month, year),by=golferid][golferid==792168569]

months.played <- golfers[, .N, by=c("golferid", "month", "year")][, month.adj := month+12*year, by=golferid]
months.played <- months.played[order(year, month)]
months.played[, gap := .(month.adj - shift(month.adj)), by=golferid]

months.played[gap>3]
months.played[golferid %in% c(789979119,139632850, 216392850) & year %in% c(2006, 2007), .SD, by=golferid]

months.played[golferid==791424820 & year==2018][order(year)]

months.played[,.N,by=golferid]


# find the longest gap:

longest.gap <- function(DT){
  indices <- c(1, which(DT[, gap] > 3))
  max(indices - shift(indices, fill=0))
}

# indices <- c(1, which(months.played[golferid==792168569, gap] > 3))
# max(indices - shift(indices, fill=0))

longest.round <- months.played[, .(longest.round = longest.gap(.SD)), by=golferid] #[,.(longest.round), by=golferid]
longest.round[golferid==789608883]

golfers <- golfers[on='golferid', months.played[, .(longest.round = longest.gap(.SD)), by=golferid]]
gc()

fwrite(golfers, "./downloaded-data/cleaned_data/100K golferid enriched.csv")

