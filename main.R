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
fees <- list(performance = 14/100)

# summary -------------------------------------------------
my.tables <- list()

# Portafolio Total

my.tables$mtd.portafolio <- my.data$pnl %>%
  filter(NombreFamilia == "TOTAL T+ L") %>%
  filter(Fecha >= my.dates$mtd) %>%
  group_by(NombreFamilia) %>%
  summarize(retorno = sum(log(PorcRentDiaria+1)),
            patrimonio.inicial = first(PatFamiliaFinal),
            patrimonio.Final = last(PatFamiliaFinal),
            resultado = sum(RentDiaria),
            cashflows = sum(Suscripciones) - sum(Rescates)) %>%
  mutate(retorno.neto.est = ifelse(retorno > 0 ,
                        retorno * (1-fees$performance), retorno),
         periodo = 'mtd')

my.tables$ytd.portafolio <- my.data$pnl %>%
  filter(NombreFamilia == "TOTAL T+ L") %>%
  filter(Fecha >= my.dates$ytd) %>%
  group_by(NombreFamilia) %>%
  summarize(retorno = sum(log(PorcRentDiaria+1)),
            patrimonio.inicial = first(PatFamiliaFinal),
            patrimonio.Final = last(PatFamiliaFinal),
            resultado = sum(RentDiaria),
            cashflows = sum(Suscripciones) - sum(Rescates)) %>%
  mutate(retorno.neto.est = ifelse(retorno > 0 ,
                                   retorno * (1-fees$performance), retorno),
         periodo = 'ytd')

my.tables$twelve.months.portafolio <- my.data$pnl %>%
  filter(NombreFamilia == "TOTAL T+ L") %>%
  filter(Fecha >= my.dates$twelve.months) %>%
  group_by(NombreFamilia) %>%
  summarize(retorno = sum(log(PorcRentDiaria+1)),
            patrimonio.inicial = first(PatFamiliaFinal),
            patrimonio.Final = last(PatFamiliaFinal),
            resultado = sum(RentDiaria),
            cashflows = sum(Suscripciones) - sum(Rescates)) %>%
  mutate(retorno.neto.est = ifelse(retorno > 0 ,
                                   retorno * (1-fees$performance), retorno),
         periodo = '12 meses')

# Futuros Administrados

my.tables$mtd.Alfa <- my.data$pnl %>%
  filter(NombreFamilia == "Alfa Total") %>%
  filter(Fecha >= my.dates$mtd) %>%
  group_by(NombreFamilia) %>%
  summarize(retorno = sum(log(PorcRentDiaria+1)),
            patrimonio.inicial = first(PatFamiliaFinal),
            patrimonio.Final = last(PatFamiliaFinal),
            resultado = sum(RentDiaria),
            cashflows = sum(Suscripciones) - sum(Rescates)) %>%
  mutate(retorno.neto.est = ifelse(retorno > 0 ,
                                   retorno * (1-fees$performance), retorno),
         periodo = 'mtd')

my.tables$ytd.Alfa <- my.data$pnl %>%
  filter(NombreFamilia == "Alfa Total") %>%
  filter(Fecha >= my.dates$ytd) %>%
  group_by(NombreFamilia) %>%
  summarize(retorno = sum(log(PorcRentDiaria+1)),
            patrimonio.inicial = first(PatFamiliaFinal),
            patrimonio.Final = last(PatFamiliaFinal),
            resultado = sum(RentDiaria),
            cashflows = sum(Suscripciones) - sum(Rescates)) %>%
  mutate(retorno.neto.est = ifelse(retorno > 0 ,
                                   retorno * (1-fees$performance), retorno),
         periodo = 'ytd')

my.tables$twelve.months.Alfa <- my.data$pnl %>%
  filter(NombreFamilia == "Alfa Total") %>%
  filter(Fecha >= my.dates$twelve.months) %>%
  group_by(NombreFamilia) %>%
  summarize(retorno = sum(log(PorcRentDiaria+1)),
            patrimonio.inicial = first(PatFamiliaFinal),
            patrimonio.Final = last(PatFamiliaFinal),
            resultado = sum(RentDiaria),
            cashflows = sum(Suscripciones) - sum(Rescates)) %>%
  mutate(retorno.neto.est = ifelse(retorno > 0 ,
                                   retorno * (1-fees$performance), retorno),
         periodo = '12 meses')

# Mercado

my.tables$mtd.mercado <- my.data$pnl %>%
  filter(NombreFamilia == "BETA") %>%
  filter(Fecha >= my.dates$mtd) %>%
  group_by(NombreFamilia) %>%
  summarize(retorno = sum(log(PorcRentDiaria+1)),
            patrimonio.inicial = first(PatFamiliaFinal),
            patrimonio.Final = last(PatFamiliaFinal),
            resultado = sum(RentDiaria),
            cashflows = sum(Suscripciones) - sum(Rescates)) %>%
  mutate(retorno.neto.est = ifelse(retorno > 0 ,
                                   retorno * (1-fees$performance), retorno),
         periodo = 'mtd')

my.tables$ytd.mercado <- my.data$pnl %>%
  filter(NombreFamilia == "BETA") %>%
  filter(Fecha >= my.dates$ytd) %>%
  group_by(NombreFamilia) %>%
  summarize(retorno = sum(log(PorcRentDiaria+1)),
            patrimonio.inicial = first(PatFamiliaFinal),
            patrimonio.Final = last(PatFamiliaFinal),
            resultado = sum(RentDiaria),
            cashflows = sum(Suscripciones) - sum(Rescates)) %>%
  mutate(retorno.neto.est = ifelse(retorno > 0 ,
                                   retorno * (1-fees$performance), retorno),
         periodo = 'ytd')

my.tables$twelve.months.mercado <- my.data$pnl %>%
  filter(NombreFamilia == "BETA") %>%
  filter(Fecha >= my.dates$twelve.months) %>%
  group_by(NombreFamilia) %>%
  summarize(retorno = sum(log(PorcRentDiaria+1)),
            patrimonio.inicial = first(PatFamiliaFinal),
            patrimonio.Final = last(PatFamiliaFinal),
            resultado = sum(RentDiaria),
            cashflows = sum(Suscripciones) - sum(Rescates)) %>%
  mutate(retorno.neto.est = ifelse(retorno > 0 ,
                                   retorno * (1-fees$performance), retorno),
         periodo = '12 meses')

library(data.table)
all.tables <- rbindlist(my.tables)
# visual inspection -------------------------------------------------

# create dates -------------------------------------------------
