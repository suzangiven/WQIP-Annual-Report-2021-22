#SWN
pathSWN_2022<-here("A.2 Outfall Assessments/DryWeatherLoadingCalcs/Input/ProvisionalFlow/SWN_2022_provisional")
pathSWN_2022

data_filespathSWN_2022 = list.files(path=pathSWN_2022, pattern = "*.csv")
data_filespathSWN_2022

df <- data_filespathSWN_2022 %>%
  map(function(x) {
    read.csv(paste0(pathSWN_2022, "/", x)) %>%
      mutate(SourceFile = x)
  }
  ) %>%
  reduce(rbind)

df

DailyQ_SWN_2022 <- df %>%
  filter(!is.na(Flow..cfs.)) 

i <- sapply(DailyQ_SWN_2022, is.factor)  #https://stackoverflow.com/questions/2851015/convert-data-frame-columns-from-factors-to-characters
DailyQ_SWN_2022[i] <- lapply(DailyQ_SWN_2022[i], as.character)

str(DailyQ_SWN_2022)

DailyQ_SWN_2022$Inst.Time = substr(DailyQ_SWN_2022$Inst.Time, 1, nchar(DailyQ_SWN_2022$`Inst.Time`)-4) 

DailyQ_SWN_2022 <- DailyQ_SWN_2022 %>%
  mutate(Station = SourceFile) %>%
  mutate(date = as.Date(Inst.Time, format = '%m/%d/%Y'))

DailyQ_SWN_2022$Station = sub("./", "", DailyQ_SWN_2022$Station)
DailyQ_SWN_2022$Station = sub(".csv", "", DailyQ_SWN_2022$Station)
DailyQ_SWN_2022$Station = sub("_all", "", DailyQ_SWN_2022$Station)


DailyQ_SWN_2022[!duplicated(DailyQ_SWN_2022[c(1,2)]), ]  

DailyQ_SWN_2022$Station[DailyQ_SWN_2022$Station == "J01-9005-1"] <- "J01-9005-1 (J03P05)"
DailyQ_SWN_2022$Station[DailyQ_SWN_2022$Station == "J01-9007-1"] <- "J01-9007-1 (J02P05)"
DailyQ_SWN_2022$Station[DailyQ_SWN_2022$Station == "J01-9008-1"] <- "J01-9008-1 (J01P30)"
DailyQ_SWN_2022$Station[DailyQ_SWN_2022$Station == "J01-9066-1"] <- "J01-9066-1 (J01P04)"
DailyQ_SWN_2022$Station[DailyQ_SWN_2022$Station == "J01-9066-2"] <- "J01-9066-2 (J01P03)"
DailyQ_SWN_2022$Station[DailyQ_SWN_2022$Station == "J01-9131-1"] <- "J01-9131-1 (J01P28)"
DailyQ_SWN_2022$Station[DailyQ_SWN_2022$Station == "J01-9144-1"] <- "J01-9144-1 (J01P23)"
DailyQ_SWN_2022$Station[DailyQ_SWN_2022$Station == "J01-9144-4"] <- "J01-9144-1 (J01P26)"
DailyQ_SWN_2022$Station[DailyQ_SWN_2022$Station == "J01-9224-1"] <- "J01-9224-1 (J01P24)"
DailyQ_SWN_2022$Station[DailyQ_SWN_2022$Station == "J01-9224-2"] <- "J01-9224-2 (J01P25)"
DailyQ_SWN_2022$Station[DailyQ_SWN_2022$Station == "J01-9264-1"] <- "J01-9264-1 (J01P06)"
DailyQ_SWN_2022$Station[DailyQ_SWN_2022$Station == "J01-9992-1"] <- "J01-9992-1 (J01P27)"
DailyQ_SWN_2022$Station[DailyQ_SWN_2022$Station == "J01-10004-1"] <- "J01-10004-1 (J01P01)"
DailyQ_SWN_2022$Station[DailyQ_SWN_2022$Station == "J01-10017-1"] <- "J01-10017-1 (J01TBN4)"
DailyQ_SWN_2022$Station[DailyQ_SWN_2022$Station == "J01-10019-1"] <- "J01-10019-1 (J01P33)"
DailyQ_SWN_2022$Station[DailyQ_SWN_2022$Station == "J03-9368-1"] <- "J03-9368-1 (J03TBN1)"
DailyQ_SWN_2022$Station[DailyQ_SWN_2022$Station == "J03-9368-2"] <- "J03-9368-1 (J03TBN2)"
DailyQ_SWN_2022$Station[DailyQ_SWN_2022$Station == "J06-9079-1"] <- "J06-9079-1 (J06P03)"
DailyQ_SWN_2022$Station[DailyQ_SWN_2022$Station == "J06-10011-1"] <- "J06-10011-1 (J06P01)"
DailyQ_SWN_2022$Station[DailyQ_SWN_2022$Station == "J07-9109-4"] <- "J07-9109-4 (J07P02)"


values <- list()  
values[['RainInf']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/RainInf.rds'

RainInf<-  readRDS(values[["RainInf"]])

str(RainInf)

DailyQ_SWN_2022_d <-   left_join(DailyQ_SWN_2022, RainInf, by = c('date'='Date', 'Station'='FACILITYID')) %>%   #dry days only, each flow measurment
  filter(wet_within_72==FALSE) 
 

str(DailyQ_SWN_2022_d)

library(tidyverse)
library(ggpubr)
library(rstatix)


wetseasonbf_SWN<- DailyQ_SWN_2022_d %>%
  filter(date> '2021-9-30' & date < '2022-4-1') %>%
  group_by(Station) %>%
   mutate(median_wet = median(Flow..cfs.)) %>%
  mutate(average_wet=mean(Flow..cfs.)) %>%
  mutate(sd_wet=sd(Flow..cfs.)) %>%
  select('Station', 'median_wet', 'average_wet', 'sd_wet') %>%
  unique()



dryseasonbf_SWN<- DailyQ_SWN_2022_d %>%
  filter(date > '2022-3-30' & date < '2022-10-1') %>%
  group_by(Station) %>%
  mutate(median_dry = median(Flow..cfs.)) %>%
  mutate(average_dry=mean(Flow..cfs.)) %>%
  mutate(sd_dry=sd(Flow..cfs.)) %>%
  select('Station', 'median_dry', 'average_dry', 'sd_dry') %>%
  unique()

allbf_SWN<- DailyQ_SWN_2022_d %>%
  group_by(Station) %>%
  mutate(median_all = median(Flow..cfs.)) %>%
  mutate(average_all=mean(Flow..cfs.)) %>%
  mutate(sd_all=sd(Flow..cfs.)) %>%
  select('Station', 'median_all', 'average_all', 'sd_all') %>%
  unique()
  

write_csv(wetseasonbf_SWN, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/wetseasonbf_SWN.csv')) 

write_csv(dryseasonbf_SWN, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/dryseasonbf_SWN.csv')) 

write_csv(allbf_SWN, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/allbf_SWN.csv')) 

wetseasonbf_SWNstat<- DailyQ_SWN_2022_d %>%
  filter(date> '2021-9-30' & date < '2022-4-1') %>%
  select('Station', 'Flow..cfs.')

names(wetseasonbf_SWNstat)[2] <- "Wet_season_cfs"

dryseasonbf_SWNstat<- DailyQ_SWN_2022_d %>%
  filter(date > '2022-3-30' & date < '2022-10-1') %>%
  select('Station', 'Flow..cfs.')

names(dryseasonbf_SWNstat)[2] <- "Dry_season_cfs"

merge(SWNstat, merge(dryseasonbf_SWNstat, wetseasonbf_SWNstat, by="Station", all.x=TRUE, all.y=TRUE), by = "Station", all.x = TRUE, all.y = TRUE)
#  Year Site3 Site1 Site2

