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
my.data <- data.raw
names(my.data$bulk)[which(names(my.data$bulk) == 'Periodo')] <- 'Fecha'
my.data$pnl$PorcRentDiaria <- my.data$pnl$PorcRentDiaria / 100


# augment data (relational) -------------------------------------------------
risk.dict <- read.csv('dictionary_risk.csv', stringsAsFactors = FALSE) %>% tbl_df() %>%
  mutate(risk.type = paste(position,Risk.Type,sep = ' | '))


# Additional calculations -------------------------------------------------

risk.calcs <- my.data$bulk %>%
  select(Fecha,Valuacion,Exposure,Margin,
         LongExposure,ShortExposure) %>%
  group_by(Fecha) %>%
  summarise_all(.funs = sum)

real.margin <- my.data$bulk %>%
  filter(AT12 == "Cash Margin Accounts") %>%
  select(Fecha,Valuacion) %>%
  group_by(Fecha) %>%
  summarise_all(.funs = sum)
names(real.margin) <- c('Fecha','Real.Margin')
risk.calcs <- merge(risk.calcs,real.margin, by = 'Fecha') %>% tbl_df() %>%
  mutate(leverage.ratio = Exposure / Valuacion,
         margin.ratio = Real.Margin / Valuacion)

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
fees <- list(performance = 14/100)

# summary -------------------------------------------------

add.summary.table <- function(x,familia, start.date, date.label) {
  require(dplyr)
  my.object <- tbl_df(x)
  
  
  my.object <- my.data$pnl %>%
    filter(NombreFamilia == familia) %>%
    filter(Fecha >= start.date) %>%
    group_by(NombreFamilia) %>%
    summarize(retorno = sum(exp(log(PorcRentDiaria+1))-1),
              patrimonio.inicial = first(PatFamiliaFinal),
              patrimonio.Final = last(PatFamiliaFinal),
              patrimonio.promedio = mean(PatFamiliaFinal),
              resultado = sum(RentDiaria),
              cashflows = sum(Suscripciones) - sum(Rescates)) %>%
    mutate(retorno.neto.est = ifelse(retorno > 0 ,
                                     retorno * (1-fees$performance), retorno),
           periodo = date.label)
  my.object <- cbind(my.object$periodo,my.object)
  my.object[,1] <-  my.object$periodo
  my.object$periodo <- NULL
  names(my.object)[1] <- 'periodo'
  
  return(my.object)
  
}

my.tables <- add.summary.table(x = my.data$pnl,familia = 'TOTAL T+ L',
                               start.date = my.dates$current,date.label = 'Daily')

my.tables <- rbind(my.tables,
                   add.summary.table(x = my.data$pnl,familia = 'TOTAL T+ L',
                                     start.date = my.dates$mtd,date.label = 'MTD'))
my.tables <- rbind(my.tables,
                   add.summary.table(x = my.data$pnl,familia = 'TOTAL T+ L',
                                     start.date = my.dates$ytd,date.label = 'YTD'))

my.tables <- rbind(my.tables,
                   add.summary.table(x = my.data$pnl,familia = 'TOTAL T+ L',
                                     start.date = my.dates$twelve.months,date.label = '12 meses'))


my.tables <- rbind(my.tables,
                   add.summary.table(x = my.data$pnl,familia = 'Alfa Total',
                                     start.date = my.dates$current,date.label = 'Daily'))
my.tables <- rbind(my.tables,
                   add.summary.table(x = my.data$pnl,familia = 'Alfa Total',
                                     start.date = my.dates$mtd,date.label = 'MTD'))
my.tables <- rbind(my.tables,
                   add.summary.table(x = my.data$pnl,familia = 'Alfa Total',
                                     start.date = my.dates$ytd,date.label = 'YTD'))
my.tables <- rbind(my.tables,
                   add.summary.table(x = my.data$pnl,familia = 'Alfa Total',
                                     start.date = my.dates$twelve.months,date.label = '12 meses'))

my.tables <- rbind(my.tables,
                   add.summary.table(x = my.data$pnl,familia = 'BETA',
                                     start.date = my.dates$current,date.label = 'Daily'))
my.tables <- rbind(my.tables,
                   add.summary.table(x = my.data$pnl,familia = 'BETA',
                                     start.date = my.dates$mtd,date.label = 'MTD'))
my.tables <- rbind(my.tables,
                   add.summary.table(x = my.data$pnl,familia = 'BETA',
                                     start.date = my.dates$ytd,date.label = 'YTD'))
my.tables <- rbind(my.tables,
                   add.summary.table(x = my.data$pnl,familia = 'BETA',
                                     start.date = my.dates$twelve.months,date.label = '12 meses'))

my.tables <- rbind(my.tables,
                   add.summary.table(x = my.data$pnl,familia = 'TESORERIA',
                                     start.date = my.dates$current,date.label = 'Daily'))
my.tables <- rbind(my.tables,
                   add.summary.table(x = my.data$pnl,familia = 'TESORERIA',
                                     start.date = my.dates$mtd,date.label = 'MTD'))
my.tables <- rbind(my.tables,
                   add.summary.table(x = my.data$pnl,familia = 'TESORERIA',
                                     start.date = my.dates$ytd,date.label = 'YTD'))
my.tables <- rbind(my.tables,
                   add.summary.table(x = my.data$pnl,familia = 'TESORERIA',
                                     start.date = my.dates$twelve.months,date.label = '12 meses'))

my.tables <- rbind(my.tables,
                   add.summary.table(x = my.data$pnl,familia = 'FUTUROS ADMINISTRADOS',
                                     start.date = my.dates$current,date.label = 'Daily'))
my.tables <- rbind(my.tables,
                   add.summary.table(x = my.data$pnl,familia = 'FUTUROS ADMINISTRADOS',
                                     start.date = my.dates$mtd,date.label = 'MTD'))
my.tables <- rbind(my.tables,
                   add.summary.table(x = my.data$pnl,familia = 'FUTUROS ADMINISTRADOS',
                                     start.date = my.dates$ytd,date.label = 'YTD'))
my.tables <- rbind(my.tables,
                   add.summary.table(x = my.data$pnl,familia = 'FUTUROS ADMINISTRADOS',
                                     start.date = my.dates$twelve.months,date.label = '12 meses'))


library(data.table)
all.tables <- rbindlist(my.tables)
all.tables <- cbind(all.tables$periodo,all.tables)
# visual inspection -------------------------------------------------

# create dates -------------------------------------------------
