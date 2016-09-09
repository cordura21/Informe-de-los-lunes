# get data -------------------------------------------------
data.path <- '/Volumes/RiskApps/data/'
data.raw.files <- list(
  pnl = 'pnl.RDS',
  bulk = 'bulk.RDS',
  familias = 'familias.RDS'
)

library(dplyr)
data.raw <- list(pnl = readRDS(
  file.path(data.path,data.raw.files$pnl)) %>% tbl_df(),
  bulk = readRDS(
  file.path(data.path,data.raw.files$bulk)) %>% tbl_df(),
  familias = readRDS(
    file.path(data.path,data.raw.files$familias)) %>% tbl_df()
  )
# tidy and clean -------------------------------------------------
names(data.raw$bulk)[which(names(data.raw$bulk) == 'Periodo')] <- 'Fecha'
my.data <- data.raw
# augment data (relational) -------------------------------------------------
risk.dict <- read.csv('dictionary_risk.csv', stringsAsFactors = FALSE) %>% tbl_df() %>%
  mutate(risk.type = paste(position,Risk.Type,sep = ' | '))

my.data$bulk <- merge(my.data$bulk,risk.dict,by = 'AT12') %>% tbl_df()

# Additional calculations -------------------------------------------------

# Dates
library(lubridate)
my.dates <- list(current =  as.Date(max(my.data$bulk$Fecha)))

my.dates$ytd <- my.dates$current
day(my.dates$ytd) <- 1
month(my.dates$ytd) <- 1

my.dates$mtd <- my.dates$current
day(my.dates$mtd) <- 1

my.dates$twelve.months <- my.dates$current
year(my.dates$twelve.months) <- as.numeric(year(my.dates$twelve.months))-1


# Fees
fees <- list(performance <- 14/100)


# summary -------------------------------------------------

# visual inspection -------------------------------------------------

# create dates -------------------------------------------------
