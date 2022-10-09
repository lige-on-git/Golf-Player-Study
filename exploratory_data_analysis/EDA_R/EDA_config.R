## constants and libraries used in the exploratory data analysis and direct supporting functions

library("data.table")

get_FKs <- function(table_name, type="FK"){
  # <table_name>: lowercase string - e.g. "golfer" for table <tblGolfer>
  # get foreign keys of a table as a vector
  list_name <- paste0("valid_", tolower(table_name), "_fields")
  list_object <- get(list_name)
  indices <- grep(type, names(list_object))
  entries <- list_object[indices]
  unlist(unname(entries))  # unpack list to get values
}

get_PKs <- function(table_name){
  # <table_name>: lowercase string - e.g. "golfer" for table <tblGolfer>
  # get primary keys of a table as a vector
  get_FKs(table_name, "PK")
}

get_field_names <- function(table_name, PK=T, FK=T){
  # <table_name>: lowercase string - e.g. "golfer" for table <tblGolfer>
  # <PK>, <FK>: Boolean, if TRUE then include relevant keys
  # get all field names of a table as a vector
  list_name <- paste0("valid_", tolower(table_name), "_fields")
  list_object <- get(list_name)
  names <- unlist(unname(list_object))
  if(!PK){
    PKs <- get_PKs(table_name)
    names <- names[! names %in% PKs]
  }
  if(!FK){
    FKs <- get_FKs(table_name)
    names <- names[! names %in% FKs]
  }
  return(names)
}

## not in
`%notin%` <- Negate(`%in%`)

## Golfer table
golfer_path = "./downloaded-data/tblgolfer.csv"
valid_golfer_fields = list(PK.1='golflinkno', PK.2='golferid', 
      FK.Association='associationid', FK.Address='addressid', FK.HandicapHistory='anchorhandicaphistoryid',
      Personal.Fields = c('sex', 'dob', 'isprofessional', 'lastupdated'),
      Handicap.Fields = c('exacthandicap', 'initialhandicap', 'handicapstatus', 'handicaplastupdated', 'anchorhandicap'))
# get_FKs("golfer")
# get_PKs("golfer")
# get_field_names("golfer", T, F)

## Membership table
membership_path = "./downloaded-data/tblmembership.csv"
valid_membership_fields = list(PK.1='membershipid', 
                          FK.Golfer='golferid', FK.Club='clubid', FK.Address='addressid',
                          Status.Fields = c('ishomeclub', 'memberno', 'isvalidmember', 'isvalidplayer', 'leavingreason'),
                          Date.Fields = c('datelastvalid', 'datejoined', 'validtill', 'dateended'))


## Handicap table (100K golferid groups)
handicap_100k_path = "./downloaded-data/cleaned_data/100K golferid groups.csv"

