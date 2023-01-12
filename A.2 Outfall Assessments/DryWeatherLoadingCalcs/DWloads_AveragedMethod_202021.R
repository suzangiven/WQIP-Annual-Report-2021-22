#load libraries
library(readr)
library(dplyr)
library(tidyr)
library(data.table)
library(here)
library(tibble)
library(tidyverse)
library(readxl)
library(purrr)
library(rqdatatable)
library(lubridate)
library(readxl)
library(readr)
library(hms)
library(psyche)

library(arcgisbinding)
arc.check_product()


getwd()
setwd("C:/Users/givens/Documents/GitHub_Feb2022/WQIP-Annual-Report-2020-21")

#### Overview ####

# There is a lot of data (rain gage, continuous flow, instantaeous flow, and chemistry data)
# To help with memory and keeping things clean:
# (1) Use for loops to cycle through data for various gages and outfalls so not all data needs to be imported at once
# (2) Each Task should create temporary files, create the "end result", then delete temporary files

# Here is a summary of the Tasks:
## Task 1: Import and Format rain data 
#Set working directory (Sharepoint? Box? GitHub?)


#Option 1:  Load files from GitHub Clone

Rain_Daily <-"C:/Users/givens/Documents/GitHub_Feb2022/WQIP-Annual-Report-2020-21/A.2 Outfall Assessments/DryWeatherLoadingCalcs/Input/Rain/Rain_SOCWMA_COOP_2015_2021.CSV"

Rain_Daily<-read.csv(Rain_Daily) 


#Option 2:  Load files from SharePoint 

#Rain_Daily <-"C:/Users/givens/OneDrive - County of Orange/Shared with Everyone/DryWeatherLoadingCalcs/Input/Rain/Rain_SOCWMA_COOP_2015_2020.CSV"

#Rain_Daily<-read.csv(Rain_Daily)


#Option 3: Load files from Box
#authenticate to Box (needs access to the client_id and client_secret, perhaps by Box-admin team)
#library(boxr)
#box_auth(client_id="fuy0gdlu0ppqiplxg7fwjkch5y9hx5r9", client_secret="VjrlD5Q7Y130nxm1JEo8c4CVyKS2X1dD")

#load file from box by the file_id (could not get to work)
#file_id <- '8do3i6hm9un0716o43mygy3363jha1mh'
#values[['raw_dailyrainmidnight']]        <- box_read_csv('file_id') #daily data processed in data_import_rainfall_loads


#Format Rain data

rainmax <- Rain_Daily %>%  # 
  gather(key=rain_gage, value=rain_total, 'TRABUCO':'TUCKER') %>%
  filter(!is.na(rain_total)) %>%
  mutate(date = as.Date(Date, format = '%m/%d/%Y')) %>%
  group_by(date, rain_gage) %>%
  summarize(max_rain_total = max(rain_total)) %>%
  filter(max_rain_total >= 0.1) 

raindaily <- Rain_Daily %>%  #use this object for point rainfall data.  May not need the summarize if already pulled as daily data from Hydstra
  gather(key=rain_gage, value=rain_total, 'TRABUCO':'TUCKER') %>%
  filter(!is.na(rain_total)) %>%
  mutate(date = as.Date(Date, format = '%m/%d/%Y')) %>%
  group_by(date, rain_gage) 
saveRDS(rainmax, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/Rain_Max.rds'))
saveRDS(raindaily, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/Rain_Daily.rds'))

##* Task 2: Determine how many dry weather days at each gage
##  Function Definitions                                                    ####

# did_it_rain, check rainfall dataset for date range,
# expects rainfall dataset to have a column named date, and the rainfall dataset to be named "rainmax"


calc_prior_rain <- function(SD, day_step) {
  
  date_current <- as.Date(SD['date'])
  gage_current <- as.character(SD['rain_gage'])
  
  rained <- rainmax %>%
    
    filter(date >= (date_current - day_step), date <= date_current, rain_gage==gage_current) %>%
    
    summarise(n()) 
  ifelse(as.numeric(rained) > 0,
         return(TRUE),
         return(FALSE))
}


##  ............................................................................
##  Rainfall influenced days                                                  ####
values <- list()
values[['raindaily']]         <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/Rain_Daily.rds'
values[['rainmax']]         <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/Rain_Max.rds'


# import data from import data script
raindaily <- readRDS(values[["raindaily"]]) 
 
rainmax <- readRDS(values[["rainmax"]])  
  
str(raindaily)
str(rainmax)

rainmax<-as.data.frame(rainmax)
raindaily<-as.data.frame(raindaily)


#join datasets by date and gage name#  
Rain_join <- left_join(
  raindaily,
  rainmax,
  by = c('date' = 'date', 'rain_gage'='rain_gage')
) %>%
  as_tibble()


## Mark Rainfall within 72 hours (3 days) ##
#Dry Days Defined 72 hours after 0.1 rainfall event is wet, all else is dry#

dry_new <- data.table(Rain_join)
str(dry_new)

dry_new[, 'wet_within_72' := apply(.SD, 1, calc_prior_rain, 3), .SDcols = c('date', 'rain_gage')]

saveRDS(dry_new, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/dry_new.rds'))

#   ____________________________________________________________________________
#   Formatting and intermediate data products                               ####

#recreate file in ArcGIS so it is updated with any station changes (Use Near Table Geoprocessing)
stn_near <-"A.2 Outfall Assessments/DryWeatherLoadingCalcs/Input/Rain/swDischargePoint_Near_Coop.csv"

stn_near <- read.csv(stn_near)

values <- list()
values[['dry_new']]         <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/dry_new.rds'
dry_new <-  readRDS(values[["dry_new"]])

#Combine rain data and station data near co-op gage

library(here)

RainInf <- left_join(
  dry_new,
  stn_near,
  by=c('rain_gage'='Cooperative'))  
#select(-c(Distance, Angle)) 

RainInf <- unique(RainInf)  #remove duplicates

# save final product
saveRDS(RainInf, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/RainInf.rds'))


##  ............................................................................
##  Dry days by month ####


#Count number of dry days#

##use the format function to extract month, years##
dry_days_month<-data.frame(RainInf %>%
                       mutate(month = format(date, "%m"), year = format(date, "%Y")) %>%
                       group_by(month, year, rain_gage, FacilityID) %>%
                       summarise(dry_days = sum(wet_within_72==FALSE)))

dry_days_year<-data.frame(RainInf %>%
                             mutate(year = format(date, "%Y")) 
)
                            
                            
dry_days_year$MonitoringYear<- ifelse(dry_days_year$date>(as.Date("2020-09-30", origin="1900-01-01")) & dry_days_year$date<(as.Date("2021-10-01", origin="1900-01-01")), "MY2020-21",
                                                                  ifelse(dry_days_year$date>(as.Date("2019-09-30", origin="1900-01-01")) & dry_days_year$date<(as.Date("2020-10-01", origin="1900-01-01")), "MY2019-20",
                                                                    ifelse(dry_days_year$date>(as.Date("2018-09-30", origin="1900-01-01")) & dry_days_year$date<(as.Date("2019-10-01", origin="1900-01-01")), "MY2018-19",
                                                                           ifelse(dry_days_year$date>(as.Date("2017-09-30", origin="1900-01-01")) & dry_days_year$date<(as.Date("2018-10-01", origin="1900-01-01")), "MY2017-18",
                                                                                  ifelse(dry_days_year$date>(as.Date("2016-09-30", origin="1900-01-01")) & dry_days_year$date<(as.Date("2017-10-01", origin="1900-01-01")), "MY2016-17",
                                                                                         ifelse(dry_days_year$date>(as.Date("2015-09-30", origin="1900-01-01")) & dry_days_year$date<(as.Date("2016-10-01", origin="1900-01-01")), "MY2015-16",
                                                                                                ifelse(dry_days_year$date>(as.Date("2014-09-30", origin="1900-01-01")) & dry_days_year$date<(as.Date("2015-10-01", origin="1900-01-01")), "MY2014-15",  
                                                                                                       NA)))))))
                            
dry_days_year_group <- dry_days_year %>%                       
                    
                             group_by(MonitoringYear, rain_gage, FacilityID) %>%
                             summarise(dry_days = sum(wet_within_72==FALSE)) %>%
  as_tibble()

# save final product
library(here)
library(readr)
write_csv(dry_days_month, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DryDaysMonthly.csv'))
write_csv(dry_days_year, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DryDaysYear.csv'))
write_csv(dry_days_year_group, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DryDaysYearGroup.csv'))


# Save final dataset for other products
saveRDS(dry_days_month, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DryDaysMonthly.rds'))
saveRDS(dry_days_year, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DryDaysYear.rds'))
saveRDS(dry_days_year_group, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DryDaysYearGroup.rds'))


## Task 3: Import flow data
#S:\Environmental Res\Environmental Studies Unit\Projects\WQIP\data\Flow Data Analysis
#S:\Environmental Res\Environmental Studies Unit\Projects\WQIP\data\Hach flow data
#https://app.box.com/s/in2b5wsc0mugf2ifboibf4l7r68voree
#
#Historic flow (2016) readily available as medians
library(readxl)
library(dplyr)
HistoricQ_medians2016 <- "A.2 Outfall Assessments/DryWeatherLoadingCalcs/Input/ProvisionalFlow/Att2_summarytable_09162016_modified.xlsx"
HistoricQ_medians2016 <- read_excel(HistoricQ_medians2016, sheet = "MasterGISTable with Scoring") %>%
  mutate(DailyQcfMeds2016 = (MedianFlow)*60*60*24) %>%
  mutate(DailyQcfAvg2016 = (AvgFlow)*60*60*24) %>%
  as_tibble() %>%
  select(c("FACILITYID", "JURISDICTI", "MedianFlow", "AvgFlow", "DailyQcfMeds2016", "DailyQcfAvg2016")) %>%
  filter(!is.na(DailyQcfMeds2016))
#Use this value if there was not a flow meter installed for time period of flow calcs
#2018 reprioritization by outfalls  C:\Users\givens\Documents\GitHub\WQIP-Annual-Report-2019-20\DryWeatherLoadingCalcs\Input\Flow\Folder_1_from_2018_Outfall_Reprioritization
Reprioritization_medians2018 <- "A.2 Outfall Assessments/DryWeatherLoadingCalcs/Input/ProvisionalFlow/Prioritization_FlowScores_Jan2018_modified.xlsx"
Reprioritization_medians2018 <- read_excel(Reprioritization_medians2018, sheet = "New_score") %>%
  mutate(DailyQcfMeds2018 = (`Best Est Median Flow_2018`)*60*60*24) %>%
  mutate(DailyQcfMeds2016b = (`Best Est Median Flow_2016`)*60*60*24) %>%
   select(c("FACILITYID", "Best Est Median Flow_2018", "Best Est Median Flow_2016", "Continuos_2016", "Continuos_2018","DailyQcfMeds2016b","DailyQcfMeds2018", "Continuous_20181920")) 
  #filter(!is.na(DailyQcfMeds2018))

saveRDS(HistoricQ_medians2016, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/HistoricQ_medians2016.rds')) 
write_csv(HistoricQ_medians2016, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/HistoricQ_medians2016.csv'))
saveRDS(Reprioritization_medians2018, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/Reprioritization_medians2018.rds')) 
write_csv(Reprioritization_medians2018, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/Reprioritization_medians2018.csv'))


#Continuous flow data from 2018-20
#For calcs presented in 2019-20 Annual data report, three folders (2018 prioritization, 2019-20 OCFS, and 2020 Coto de Caza)

#use for loop to iterate analysis over a list of dataframes, then combine list of data frames
#https://datacarpentry.org/semester-biology/materials/for-loops-R/
# https://stackoverflow.com/questions/33203800/unzip-a-zip-file

##2018 Reprioritization

path<-here("A.2 Outfall Assessments/DryWeatherLoadingCalcs/Input/ProvisionalFlow/Folder_1_from_2018_Outfall_Reprioritization")
path

data_files = list.files(path=path, pattern = "*.csv")
data_files

df <- data_files %>%
  map(function(x) {
    read.csv(paste0(path, "/", x)) %>%
      mutate(SourceFile = x)
  }
  ) %>%
  reduce(rbind)

df

DailyQ_2018R <- df %>%
  filter(!is.na(Flow..cfs.)) 

i <- sapply(DailyQ_2018R, is.factor)  #https://stackoverflow.com/questions/2851015/convert-data-frame-columns-from-factors-to-characters
DailyQ_2018R[i] <- lapply(DailyQ_2018R[i], as.character)

str(DailyQ_2018R)

#DailyQ_2018p$Inst.Time = substr(DailyQ_2018p$Inst.Time, 1, nchar(DailyQ_2018p$`Inst.Time`)-4) 

DailyQ_2018R <- DailyQ_2018R %>%
  mutate(Station = SourceFile) %>%
  mutate(date = as.Date(Inst.Time, format = '%m/%d/%Y'))

DailyQ_2018R$Station = sub("./", "", DailyQ_2018R$Station)
DailyQ_2018R$Station = sub(".csv", "", DailyQ_2018R$Station)
DailyQ_2018R$Station = sub("_all", "", DailyQ_2018R$Station)


DailyQ_2018R$Station<-sub("J01-9008-1", "J01-9008-1 (J01P30)",DailyQ_2018R$Station)
DailyQ_2018R$Station<-sub("J01-9131-1", "J01-9131-1 (J01P28)",DailyQ_2018R$Station)
DailyQ_2018R$Station<-sub("J01-9992-1", "J01-9992-1 (J01P27)",DailyQ_2018R$Station)
DailyQ_2018R$Station<-sub("K01-12138-1", "K01-12138-1 (K01TBN1)",DailyQ_2018R$Station)
DailyQ_2018R$Station<-sub("L01-724-1", "L01-724-1 (L01S01)",DailyQ_2018R$Station)
DailyQ_2018R$Station<-sub("L01-727-1", "L01-727-1 (L01S04)",DailyQ_2018R$Station)
DailyQ_2018R$Station<-sub("L02-622-2", "L02-622-2 (L02P32)",DailyQ_2018R$Station)
DailyQ_2018R$Station<-sub("L03-693-1", "L03-693-1 (L03P11)",DailyQ_2018R$Station)


DailyQ_2018R <- DailyQ_2018R %>%
  select(-SourceFile) %>%
  #filter(DailyQ_201920Cont > 4) %>% #filter away days from when flow meter not working correctly for J01P27 and J01P28
  filter(Station != 'J01-9992-1 (J01P27)'|date != '2018-04-06') %>%
  filter(Station != 'J01-9992-1 (J01P27)'|date != '2018-04-08') %>%
  mutate(date = as.Date(Inst.Time, format = '%m/%d/%Y'))

DailyQ_2018R[!duplicated(DailyQ_2018R[c(1,2)]), ]   

str(DailyQ_2018R)
saveRDS(DailyQ_2018R, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DailyQ_2018R.rds')) 
write_csv(DailyQ_2018R, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DailyQ_2018R.csv'))


##2019-20 OFP##
library(tidyverse)
library(here)
path<-here("A.2 Outfall Assessments/DryWeatherLoadingCalcs/Input/ProvisionalFlow/Folder_2_from_2020_Outfall_Flow_Plots")
path

data_files = list.files(path=path, pattern = "*.csv")
data_files

df <- data_files %>%
  map(function(x) {
    read.csv(paste0(path, "/", x)) %>%
      mutate(SourceFile = x)
  }
    ) %>%
  reduce(rbind)

df

DailyQ_2020OFP <- df %>%
  filter(!is.na(Flow..cfs.)) 

i <- sapply(DailyQ_2020OFP, is.factor)  #https://stackoverflow.com/questions/2851015/convert-data-frame-columns-from-factors-to-characters
DailyQ_2020OFP[i] <- lapply(DailyQ_2020OFP[i], as.character)

str(DailyQ_2020OFP)

DailyQ_2020OFP$Inst.Time = substr(DailyQ_2020OFP$Inst.Time, 1, nchar(DailyQ_2020OFP$`Inst.Time`)-4) 

DailyQ_2020OFP <- DailyQ_2020OFP %>%
  mutate(Station = SourceFile) %>%
  mutate(date = as.Date(Inst.Time, format = '%m/%d/%Y'))

DailyQ_2020OFP$Station = sub("./", "", DailyQ_2020OFP$Station)
DailyQ_2020OFP$Station = sub(".csv", "", DailyQ_2020OFP$Station)
DailyQ_2020OFP$Station = sub("_all", "", DailyQ_2020OFP$Station)


DailyQ_2020OFP$Station<-sub("J01-9066-2", "J01-9066-2 (J01P03)",DailyQ_2020OFP$Station)
DailyQ_2020OFP$Station<-sub("J01-9131-1", "J01-9131-1 (J01P28)",DailyQ_2020OFP$Station)
DailyQ_2020OFP$Station<-sub("J01-9992-1", "J01-9992-1(J01P27)",DailyQ_2020OFP$Station)
DailyQ_2020OFP$Station<-sub("J03-9221-1", "J03-9221-1 (J03P02)",DailyQ_2020OFP$Station)
DailyQ_2020OFP$Station<-sub("J06-9079-1", "J06-9079-1 (J06P03)",DailyQ_2020OFP$Station)
DailyQ_2020OFP$Station<-sub("J06-10011-1", "J06-10011-1 (J06P01)",DailyQ_2020OFP$Station)
DailyQ_2020OFP$Station<-sub("K01-12126-1", "K01-12126-1 (K01S01)",DailyQ_2020OFP$Station)
DailyQ_2020OFP$Station<-sub("L01-728-3", "L01-728-3 (L01S02)",DailyQ_2020OFP$Station)
DailyQ_2020OFP$Station<-sub("L01-766-2", "L01-766-2 (L01S06)",DailyQ_2020OFP$Station)
DailyQ_2020OFP$Station<-sub("L03-214-2", "L03-214-2 (L03P18)",DailyQ_2020OFP$Station)
DailyQ_2020OFP$Station<-sub("L03-316-3", "L03-316-3 (L03P12)",DailyQ_2020OFP$Station)
DailyQ_2020OFP$Station<-sub("L03-074-1", "L03-074-1 (L03B01)",DailyQ_2020OFP$Station)
DailyQ_2020OFP$Station<-sub("M01-050-4", "M01-050-4 (M01@CGV)",DailyQ_2020OFP$Station)


DailyQ_2020OFP <- DailyQ_2020OFP %>%
  select(-SourceFile) %>%
  #filter(DailyQ_2020OFPCont > 4) %>% #filter away days from when flow meter not working correctly for J01P27 and J01P28
  filter(Station != 'J01-9992-1 (J01P27)'|date != '2018-04-06') %>%
  filter(Station != 'J01-9992-1 (J01P27)'|date != '2018-04-08') %>%
  mutate(date = as.Date(Inst.Time, format = '%m/%d/%Y'))

DailyQ_2020OFP[!duplicated(DailyQ_2020OFP[c(1,2)]), ]   

str(DailyQ_2020OFP)
saveRDS(DailyQ_2020OFP, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DailyQ_2020OFP.rds')) 
write_csv(DailyQ_2020OFP, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DailyQ_2020OFP.csv'))


#Continuous flow data OCFS, 2021
pathOCFS_2021<-here("A.2 Outfall Assessments/DryWeatherLoadingCalcs/Input/ProvisionalFlow/OCFS_2021_provisional")
pathOCFS_2021

data_filespathOCFS_2021 = list.files(path=pathOCFS_2021, pattern = "*.csv")
data_filespathOCFS_2021

df <- data_filespathOCFS_2021 %>%
  map(function(x) {
    read.csv(paste0(pathOCFS_2021, "/", x)) %>%
      mutate(SourceFile = x)
  }
  ) %>%
  reduce(rbind)

df

DailyQ_OCFS_2021 <- df %>%
  filter(!is.na(Flow..cfs.)) 

i <- sapply(DailyQ_OCFS_2021, is.factor)  #https://stackoverflow.com/questions/2851015/convert-data-frame-columns-from-factors-to-characters
DailyQ_OCFS_2021[i] <- lapply(DailyQ_OCFS_2021[i], as.character)

str(DailyQ_OCFS_2021)

DailyQ_OCFS_2021$Inst.Time = substr(DailyQ_OCFS_2021$Inst.Time, 1, nchar(DailyQ_OCFS_2021$`Inst.Time`)-4) 

DailyQ_OCFS_2021 <- DailyQ_OCFS_2021 %>%
  mutate(Station = SourceFile) %>%
  mutate(date = as.Date(Inst.Time, format = '%m/%d/%Y'))

DailyQ_OCFS_2021$Station = sub("./", "", DailyQ_OCFS_2021$Station)
DailyQ_OCFS_2021$Station = sub(".csv", "", DailyQ_OCFS_2021$Station)
DailyQ_OCFS_2021$Station = sub("_all", "", DailyQ_OCFS_2021$Station)


DailyQ_OCFS_2021[!duplicated(DailyQ_OCFS_2021[c(1,2)]), ]   

str(DailyQ_OCFS_2021)
saveRDS(DailyQ_OCFS_2021, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DailyQ_OCFS_2021.rds')) 
write_csv(DailyQ_OCFS_2021, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DailyQ_OCFS_2021.csv')) 

#SWN
pathSWN_2021<-here("A.2 Outfall Assessments/DryWeatherLoadingCalcs/Input/ProvisionalFlow/SWN_2021_provisional")
pathSWN_2021

data_filespathSWN_2021 = list.files(path=pathSWN_2021, pattern = "*.csv")
data_filespathSWN_2021

df <- data_filespathSWN_2021 %>%
  map(function(x) {
    read.csv(paste0(pathSWN_2021, "/", x)) %>%
      mutate(SourceFile = x)
  }
  ) %>%
  reduce(rbind)

df

DailyQ_SWN_2021 <- df %>%
  filter(!is.na(Flow..cfs.)) 

i <- sapply(DailyQ_SWN_2021, is.factor)  #https://stackoverflow.com/questions/2851015/convert-data-frame-columns-from-factors-to-characters
DailyQ_SWN_2021[i] <- lapply(DailyQ_SWN_2021[i], as.character)

str(DailyQ_SWN_2021)

DailyQ_SWN_2021$Inst.Time = substr(DailyQ_SWN_2021$Inst.Time, 1, nchar(DailyQ_SWN_2021$`Inst.Time`)-4) 

DailyQ_SWN_2021 <- DailyQ_SWN_2021 %>%
  mutate(Station = SourceFile) %>%
  mutate(date = as.Date(Inst.Time, format = '%m/%d/%Y'))

DailyQ_SWN_2021$Station = sub("./", "", DailyQ_SWN_2021$Station)
DailyQ_SWN_2021$Station = sub(".csv", "", DailyQ_SWN_2021$Station)
DailyQ_SWN_2021$Station = sub("_all", "", DailyQ_SWN_2021$Station)


DailyQ_SWN_2021[!duplicated(DailyQ_SWN_2021[c(1,2)]), ]   

str(DailyQ_SWN_2021)
saveRDS(DailyQ_SWN_2021, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DailyQ_SWN_2021.rds')) 
write_csv(DailyQ_SWN_2021, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DailyQ_SWN_2021.csv')) 

#Salt Creek
SaltCreek <- "A.2 Outfall Assessments/DryWeatherLoadingCalcs/Input/ProvisionalFlow/SALTCRK.CSV"
SaltCreek <- read_csv(SaltCreek) %>%
  mutate(Station='K01-12156-4')

i <- sapply(SaltCreek, is.factor)  #https://stackoverflow.com/questions/2851015/convert-data-frame-columns-from-factors-to-characters
SaltCreek[i] <- lapply(SaltCreek[i], as.character)

str(SaltCreek)

SaltCreek$`Inst Time` = substr(SaltCreek$`Inst Time`, 1, nchar(SaltCreek$`Inst Time`)-4) 

SaltCreek <- SaltCreek %>%
  #mutate(Station = SourceFile) %>%
  mutate(date = as.Date(`Inst Time`, format = '%m/%d/%Y'))

  #mutate(DailyQSaltCreek_cf = (`Flow (cfs)`)*60*60*24) #median selected already in daily cf

saveRDS(SaltCreek, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/SaltCreek.rds')) 
write_csv(SaltCreek, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/SaltCreek.csv')) 


str(SaltCreek)

#mutate(Qcf = Flow..cfs.*60*5) %>%
  #select(SourceFile, date, Qcf) %>%
  #group_by(date, SourceFile) %>%     
  #summarize(dailyQCont=sum(Qcf)) 
#median here would be consistent with prioritization
  
#get station names from SourceFile Column

##* Task 4.1: Filter out wet weather from continuous flow data
      #join to station and rain gauge#
   
values <- list()  
values[['RainInf']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/RainInf.rds'
      
    RainInf<-  readRDS(values[["RainInf"]]) 
    
    RainInf$FacilityID  <- as.character(RainInf$FacilityID)
    
    str(RainInf)
    values <- list()  
    values[['DailyQ_2018R']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DailyQ_2018R.rds'
    
    DailyQ_2018R <-  readRDS(values[["DailyQ_2018R"]])     
    
    DailyQ_2018R_d <-   left_join(DailyQ_2018R, RainInf, by = c('date'='date', 'Station'='FacilityID')) %>%   #dry days only, each flow measurment
      filter(wet_within_72==FALSE)
    
    saveRDS(DailyQ_2018R_d, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DailyQ_2018R_d.rds')) 
    write_csv(DailyQ_2018R_d, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DailyQ_2018R_d.csv'))     
      
    
      values <- list()  
      values[['DailyQ_2020OFP']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DailyQ_2020OFP.rds'
      DailyQ_2020OFP <-  readRDS(values[["DailyQ_2020OFP"]])     
    
      DailyQ_2020OFP_d <-   left_join(DailyQ_2020OFP, RainInf, by = c('date'='date', 'Station'='FacilityID')) %>%   #dry days only, each flow measurment
        filter(wet_within_72==FALSE)
   
      saveRDS(DailyQ_2020OFP_d, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DailyQ_2020OFP_d.rds')) 
      write_csv(DailyQ_2020OFP_d, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DailyQ_2020OFP_d.csv'))     
    
      values <- list()  
      values[['DailyQ_OCFS_2021']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DailyQ_OCFS_2021.rds'
      DailyQ_OCFS_2021 <-  readRDS(values[["DailyQ_OCFS_2021"]]) 
      DailyQ_OCFS_2021$Station[DailyQ_OCFS_2021$Station == "L03-693-1"] <- "L03-693-1 (L03P11)"
      DailyQ_OCFS_2021$Station[DailyQ_OCFS_2021$Station == "L02-166-3"] <- "L02-166-3 (L02P26)"
     
      values <- list()  
      values[['DailyQ_SWN_2021']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DailyQ_SWN_2021.rds'
      DailyQ_SWN_2021 <-  readRDS(values[["DailyQ_SWN_2021"]])
      
      DailyQ_SWN_2021$Station[DailyQ_SWN_2021$Station == "J01-9005-1"] <- "J01-9005-1 (J03P05)"
      DailyQ_SWN_2021$Station[DailyQ_SWN_2021$Station == "J01-9007-1"] <- "J01-9007-1 (J02P05)"
      DailyQ_SWN_2021$Station[DailyQ_SWN_2021$Station == "J01-9008-1"] <- "J01-9008-1 (J01P30)"
      DailyQ_SWN_2021$Station[DailyQ_SWN_2021$Station == "J01-9066-1"] <- "J01-9066-1 (J01P04)"
      DailyQ_SWN_2021$Station[DailyQ_SWN_2021$Station == "J01-9066-2"] <- "J01-9066-2 (J01P03)"
      DailyQ_SWN_2021$Station[DailyQ_SWN_2021$Station == "J01-9131-1"] <- "J01-9131-1 (J01P28)"
      DailyQ_SWN_2021$Station[DailyQ_SWN_2021$Station == "J01-9144-1"] <- "J01-9144-1 (J01P23)"
      DailyQ_SWN_2021$Station[DailyQ_SWN_2021$Station == "J01-9144-4"] <- "J01-9144-1 (J01P26)"
      DailyQ_SWN_2021$Station[DailyQ_SWN_2021$Station == "J01-9224-1"] <- "J01-9224-1 (J01P24)"
      DailyQ_SWN_2021$Station[DailyQ_SWN_2021$Station == "J01-9224-2"] <- "J01-9224-2 (J01P25)"
      DailyQ_SWN_2021$Station[DailyQ_SWN_2021$Station == "J01-9264-1"] <- "J01-9264-1 (J01P06)"
      DailyQ_SWN_2021$Station[DailyQ_SWN_2021$Station == "J01-9992-1"] <- "J01-9992-1 (J01P27)"
      DailyQ_SWN_2021$Station[DailyQ_SWN_2021$Station == "J01-10004-1"] <- "J01-10004-1 (J01P01)"
      DailyQ_SWN_2021$Station[DailyQ_SWN_2021$Station == "J01-10017-1"] <- "J01-10017-1 (J01TBN4)"
      DailyQ_SWN_2021$Station[DailyQ_SWN_2021$Station == "J01-10019-1"] <- "J01-10019-1 (J01P33)"
      DailyQ_SWN_2021$Station[DailyQ_SWN_2021$Station == "J03-9368-1"] <- "J03-9368-1 (J03TBN1)"
      DailyQ_SWN_2021$Station[DailyQ_SWN_2021$Station == "J03-9368-2"] <- "J03-9368-1 (J03TBN2)"
      DailyQ_SWN_2021$Station[DailyQ_SWN_2021$Station == "J06-9079-1"] <- "J06-9079-1 (J06P03)"
      DailyQ_SWN_2021$Station[DailyQ_SWN_2021$Station == "J06-10011-1"] <- "J06-10011-1 (J06P01)"
      DailyQ_SWN_2021$Station[DailyQ_SWN_2021$Station == "J07-9109-4"] <- "J07-9109-4 (J07P02)"
      
      values <- list()  
      values[['SaltCreek']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/SaltCreek.rds'
      SaltCreek <-  readRDS(values[["SaltCreek"]]) 
      
      DailyQ_OCFS_2021_d <-   left_join(DailyQ_OCFS_2021, RainInf, by = c('date'='date', 'Station'='FacilityID')) %>%   #dry days only, each flow measurment
        filter(wet_within_72==FALSE) 
      
      saveRDS(DailyQ_OCFS_2021_d, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DailyQ_OCFS_2021_d.rds')) 
      write_csv(DailyQ_OCFS_2021_d, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DailyQ_OCFS_2021_d.csv'))
      
      DailyQ_SWN_2021_d <-   left_join(DailyQ_SWN_2021, RainInf, by = c('date'='date', 'Station'='FacilityID')) %>%   #dry days only, each flow measurment
        filter(wet_within_72==FALSE) 
      
      saveRDS(DailyQ_SWN_2021_d, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DailyQ_SWN_2021_d.rds')) 
      write_csv(DailyQ_SWN_2021_d, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DailyQ_SWN_2021_d.csv'))
      
      
      SaltCreek_d <-   left_join(SaltCreek, RainInf, by = c('date'='date', 'Station'='FacilityID')) %>%   #dry days only, each flow measurment
        filter(wet_within_72==FALSE) 
      
      
      
      saveRDS(SaltCreek_d, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/SaltCreek_d.rds')) 
      write_csv(SaltCreek_d, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/SaltCreek_d.csv'))
    ## Task 5 Import outfall observations 
   
    FieldScreening <- arc.open('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Input/ProvisionalFlow/SPOCDSQL1205.sde/OCEnvRes.OCENVRESUSER.swDischargePoint_Obs') %>% 
      arc.select() %>% 
      tibble::as_tibble() %>% 
      full_join(
        .,
        stn_flow2<-arc.open('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Input/ProvisionalFlow/SPOCDSQL1205.sde/OCEnvRes.OCENVRESUSER.swDischargePoint') %>%
          arc.select() %>%
          tibble::as_tibble(),
        by = c('swDischargePointGUID' = 'GlobalID')
      ) %>%
      filter(MANAGEMENT =='SOUTH', INSPECTED =='1',is.na(OBSWEATHER)|OBSWEATHER == '1') %>%
      select('FACILITYID', 'INSPECTIONDATE', 'JURISDICTI', 'FACTYPE', 'SIZE1', 'GlobalID', 'swDischargePointGUID', 'FLOWCOND', 'FLOWWIDTH', 'FLOWDEPTH', 'FLOWVELOCITY', 'AVGFLOWWIDTH', 'AVGFLOWDEPTH', 'AVGFLOWVEL','AVGDISCHARGE','PERSISTENTFLOW', 'OCFS', 'DRYSAMPLED', 'FLOWMONITDEPLOY', 'FLOWMONITRETURN', 'FLOWCONNECTIVITY') %>%
    mutate(date = as.Date(INSPECTIONDATE, format = '%Y-%m-%d'))
    
    FieldScreening$FACILITYID<-sub("J03-9216-1", "J03-9216-1 (J03P01)",FieldScreening$FACILITYID) 
    
    str(FieldScreening)
    
    # Save processed dataset
    
    saveRDS(FieldScreening, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/FieldScreening.rds')) 
    write_csv(FieldScreening, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/FieldScreening.csv'))
    
    #IF Cannot bind to valid ArcGIS Installation - load dataset from non-GitHub Working Directory
    library(dplyr)
    FieldScreeningr <-"A.2 Outfall Assessments/DryWeatherLoadingCalcs/Input/Flow/OCEnvRes_July162021.csv"
    FieldScreeningr <- read.csv(FieldScreeningr) 
    str(FieldScreeningr)
    #date formatting
     
      i <- sapply(FieldScreeningr, is.factor)  #https://stackoverflow.com/questions/2851015/convert-data-frame-columns-from-factors-to-characters
      FieldScreeningr[i] <- lapply(FieldScreeningr[i], as.character)
      
      str(FieldScreeningr)
    
      #FieldScreeningr$INSPECTIONDATE = substr(FieldScreeningr$INSPECTIONDATE, 1, nchar(FieldScreeningr$`INSPECTIONDATE`)-10) 
      #FieldScreeningr <- FieldScreeningr %>%
       #mutate(date = as.Date(INSPECTIONDATE, format = '%Y-%m-%d'))
   
      #FieldScreeningr$Inspection.Date = substr(FieldScreeningr$Inspection.Date, 1, nchar(FieldScreeningr$`Inspection.Date`)-10) 
      FieldScreeningr <- FieldScreeningr %>%
      #mutate(date = as.Date(Inspection.Date, format = '%Y-%m-%d'))
        mutate(date = as.Date(Inspection.Date, format = '%m/%d/%Y'))
      FieldScreeningr$Facility.Identifier<-sub("J03-9216-1", "J03-9216-1 (J01P01)",Facility.Identifier$FACILITYID)
      saveRDS(FieldScreeningr, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/FieldScreeningr.rds')) 
      
      #Outfall list, automated pull from ArcGIS Pro  
      Trib<- arc.open('C:/Users/givens/OneDrive - County of Orange/Shared with Everyone/Outfall/OutfallFieldScreen/Results/SPOCDSQL1205.sde/OCEnvRes.OCENVRESUSER.swDischargePoint_Tributary') %>%
        arc.select() %>%
        tibble::as_tibble()
      #update station names
      Trib$FACILITYID<-sub("J01-9224-1", "J01-9224-1 (J01P24)",Trib$FACILITYID)
      Trib$FACILITYID<-sub("J01-9224-2", "J01-9224-2 (J01P25)",Trib$FACILITYID)
      Trib$FACILITYID<-sub("J01-9992-1", "J01-9992-1 (J01P27)",Trib$FACILITYID)
      Trib$FACILITYID<-sub("J06-9362-1", "J06-9362-1 (J06-03)",Trib$FACILITYID)
      Trib$FACILITYID<-sub("K01-12177-1", "K01-12177-1 (K01P07)",Trib$FACILITYID)
      Trib$FACILITYID<-sub("L01-731-1", "L01-731-1 (L08TBN2)",Trib$FACILITYID)
      Trib$FACILITYID<-sub("L01-766-2", "L01-766-2 (L01S06)",Trib$FACILITYID)
      Trib$FACILITYID<-sub("L02-166-3", "L02-166-3 (L02P26)",Trib$FACILITYID)
      Trib$FACILITYID<-sub("L02-366-1", "L02-366-1 (MVL02P14)",Trib$FACILITYID)
      Trib$FACILITYID<-sub("L02-622-2", "L02-622-2 (L02P32)",Trib$FACILITYID)
      Trib$FACILITYID<-sub("L03-316-3", "L03-316-3 (L03P12)",Trib$FACILITYID)
      Trib$FACILITYID<-sub("L03-662-3", "L03-662-3 (L03P16)",Trib$FACILITYID)
      Trib$FACILITYID<-sub("SC10-075-3", "SC10-075-3 (M00S01)",Trib$FACILITYID)
      Trib$FACILITYID<-sub("J01-10041-2", "J01-10041-2 (J03P13)",Trib$FACILITYID)
      Trib$FACILITYID<-sub("J01-9007-1", "J01-9007-1 (J02P05)",Trib$FACILITYID)
      Trib$FACILITYID<-sub("J01-10004-1", "J01-10004-1 (J01P01)",Trib$FACILITYID)
      Trib$FACILITYID<-sub("I01-11343-2", "I01-11343-2 (I02P18)",Trib$FACILITYID)
      Trib$FACILITYID<-sub("M01-050-4", "M01-050-4 (M01@CGV)",Trib$FACILITYID)
      Trib$FACILITYID<-sub("L01-728-4", "L01-728-4 (L01-DP)",Trib$FACILITYID)
      Trib$FACILITYID<-sub("J01-9131-1", "J01-9131-1 (J01P28)",Trib$FACILITYID)
      Trib$FACILITYID<-sub("J01-9264-1", "J01-9264-1 (J01P06)",Trib$FACILITYID)
      Trib$FACILITYID<-sub("I01-11216-1", "I01-11216-1 (I02P13)",Trib$FACILITYID)
      Trib$FACILITYID<-sub("J01-9066-1", "J01-9066-1 (J01P04)",Trib$FACILITYID)
      Trib$FACILITYID<-sub("J01-9364-3", "J01-9364-3 (J01P21)",Trib$FACILITYID) 
 
  
    DischargePointTrib <-   Trib %>%
                full_join(., DischargePoint <- arc.open('C:/Users/givens/OneDrive - County of Orange/Shared with Everyone/Outfall/OutfallFieldScreen/Results/SPOCDSQL1205.sde/OCEnvRes.OCENVRESUSER.swDischargePoint') %>%
                   arc.select() %>%
                   tibble::as_tibble(),        
                   by = c("swDischargePointGUID" = "GlobalID", "FACILITYID")) %>%
                  filter(MANAGEMENT =='SOUTH') %>%
                  filter(INSPECTED=='1')
          
          DischargePointTrib$FACILITYID<-sub("J03-9216-1", "J03-9216-1 (J03P01)",DischargePointTrib$FACILITYID) 
         
      saveRDS(DischargePointTrib, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DischargePointTrib.rds'))
      write_csv(DischargePointTrib, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DischargePointTrib.csv'))  
      

      values <- list()  
      values[['DischargePointTrib']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DischargePointTrib.rds'
      DischargePointTrib <-  readRDS(values[["DischargePointTrib"]])
      
      AnnualCF_consolidate_small1<- DischargePointTrib %>%
        filter(SIZE1 < 35 & area_acres > 49)
      Box1 <-DischargePointTrib %>% 
        filter(FACTYPE == '1') %>%
        filter(area_acres > 49)
      Box2 <-DischargePointTrib %>% 
        filter(FACTYPE == '1') %>%
        filter(is.na(area_acres))
      Box<-bind_rows(Box1,Box2)
      
      Culvert <-DischargePointTrib %>% 
        filter(FACTYPE == '2') %>%
        filter(SIZE1 > 35)
      
      Culvert2 <-DischargePointTrib %>% 
        filter(FACTYPE == '2') %>%
        filter(is.na(area_acres)) %>%
        filter(SIZE1 <36 |is.na(SIZE1))
      
      
      Culvert <-bind_rows(Culvert, Culvert2)
      
      Outfall <-DischargePointTrib %>% 
        filter(FACTYPE == '0') %>%
        filter(SIZE1 > 35)
      
      L00120941d <- DischargePointTrib %>% 
        filter(FACILITYID=='L00-12094-1d')
      
      MajorOutfalls_lc <- bind_rows(AnnualCF_consolidate_small1,Box, Culvert,Outfall, L00120941d) %>%
        filter(ACCESSIBILITY !=4) %>%
        filter(ACCESSIBILITY !=5 | AVGDISCHARGE >0) %>% #keep last flow measurements for inaccessible outfalls 
      select(FACILITYID,POINT_Y, POINT_X, JURISDICTI, INSPECTED, AVGDISCHARGE, area_acres, ACCESSIBILITY,DRYSAMPLED,OCFS, PERSISTENTFLOW, GlobalID)
      
      str(MajorOutfalls_lc)
      
      saveRDS(MajorOutfalls_lc, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/MajorOutfalls_lc.rds'))
      write_csv(MajorOutfalls_lc, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/MajorOutfalls_lc.csv')) 
      
      #jurisdiction and Outfall to join to continuous datasets
      values <- list()  
      values[['MajorOutfalls_lc']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/MajorOutfalls_lc.rds'
      MajorOutfalls_lc <-  readRDS(values[["MajorOutfalls_lc"]])    
      
      Outfall_juris <- MajorOutfalls_lc %>%
        select(FACILITYID, JURISDICTI)
      
      saveRDS(Outfall_juris, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/Outfall_juris.rds'))
      write_csv(Outfall_juris, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/Outfall_juris.csv'))
 
   #find total acres in Jurisdiction in R9
      RB <- arc.open('H:/ArcGIS/Projects/StormDrainOutfall/StormDRainOutfall.gdb/CityInRB') %>%
        arc.select() %>%
        tbl_df() 
      
      Cities <- arc.open('H:/ArcGIS/Projects/StormDrainOutfall/StormDRainOutfall.gdb/JURISDICTI_Summary') %>%
      arc.select() %>%
      tbl_df()  
      
      R9_Cities <-full_join(Cities, RB, by=c("Join_ID")) %>%
         as_tibble() %>%
        filter(rb=='9') %>%
        select('JURISDICTI', 'sum_Area_ACRES.x') %>%
        filter(JURISDICTI != 'NEWPORT BEACH'& JURISDICTI != 'IRVINE') 
      
        colnames(R9_Cities)[2] <- "AreaJ"
        
      saveRDS(R9_Cities, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/R9_Cities.rds'))
      write_csv(R9_Cities, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/R9_Cities.csv')) 
      
      
           
      file_url<-"A.2 Outfall Assessments/DryWeatherLoadingCalcs/Input/JURISinTRIBS.csv"
        JURISinTRIBS<-read.csv(file_url) 
      
        
        #udpate station names
        JURISinTRIBS$FACILITYID<-sub("J01-9224-1", "J01-9224-1 (J01P24)",JURISinTRIBS$FACILITYID)
        JURISinTRIBS$FACILITYID<-sub("J01-9224-2", "J01-9224-2 (J01P25)",JURISinTRIBS$FACILITYID)
        JURISinTRIBS$FACILITYID<-sub("J01-9992-1", "J01-9992-1 (J01P27)",JURISinTRIBS$FACILITYID)
        JURISinTRIBS$FACILITYID<-sub("J06-9362-1", "J06-9362-1 (J06-03)",JURISinTRIBS$FACILITYID)
        JURISinTRIBS$FACILITYID<-sub("K01-12177-1", "K01-12177-1 (K01P07)",JURISinTRIBS$FACILITYID)
        JURISinTRIBS$FACILITYID<-sub("L01-731-1", "L01-731-1 (L08TBN2)",JURISinTRIBS$FACILITYID)
        JURISinTRIBS$FACILITYID<-sub("L01-766-2", "L01-766-2 (L01S06)",JURISinTRIBS$FACILITYID)
        JURISinTRIBS$FACILITYID<-sub("L02-166-3", "L02-166-3 (L02P26)",JURISinTRIBS$FACILITYID)
        JURISinTRIBS$FACILITYID<-sub("L02-366-1 (L02-P14)", "L02-366-1 (MVL02P14)",JURISinTRIBS$FACILITYID)
        JURISinTRIBS$FACILITYID<-sub("L02-622-2", "L02-622-2 (L02P32)",JURISinTRIBS$FACILITYID)
        JURISinTRIBS$FACILITYID<-sub("L03-316-3", "L03-316-3 (L03P12)",JURISinTRIBS$FACILITYID)
        JURISinTRIBS$FACILITYID<-sub("L03-662-3", "L03-662-3 (L03P16)",JURISinTRIBS$FACILITYID)
        JURISinTRIBS$FACILITYID<-sub("SC10-075-3", "SC10-075-3 (M00S01)",JURISinTRIBS$FACILITYID)
        JURISinTRIBS$FACILITYID<-sub("J01-10041-2", "J01-10041-2 (J03P13)",JURISinTRIBS$FACILITYID)
        JURISinTRIBS$FACILITYID<-sub("J01-9007-1", "J01-9007-1 (J02P05)",JURISinTRIBS$FACILITYID)
        JURISinTRIBS$FACILITYID<-sub("J01-10004-1", "J01-10004-1 (J01P01)",JURISinTRIBS$FACILITYID)
        JURISinTRIBS$FACILITYID<-sub("I01-11343-2", "I01-11343-2 (I02P18)",JURISinTRIBS$FACILITYID)
        JURISinTRIBS$FACILITYID<-sub("J01-9131-1", "J01-9131-1 (J01P28)",JURISinTRIBS$FACILITYID)
        JURISinTRIBS$FACILITYID<-sub("J01-9264-1", "J01-9264-1 (J01P06)",JURISinTRIBS$FACILITYID)
        JURISinTRIBS$FACILITYID<-sub("I01-11216-1", "I01-11216-1 (I02P13)",JURISinTRIBS$FACILITYID)
        JURISinTRIBS$FACILITYID<-sub("J01-9066-1", "J01-9066-1 (J01P04)",JURISinTRIBS$FACILITYID)
        JURISinTRIBS$FACILITYID<-sub("J01-9364-3", "J01-9364-3 (J01P21)",JURISinTRIBS$FACILITYID)
        
        
           JURISinTRIBS_MO <-JURISinTRIBS %>%
           right_join(., MajorOutfalls_lc,
                   by = c('FACILITYID')) %>%
          filter(!is.na(JURISDICTI)) %>% #filter away non-major outfalls
        filter(FACILITYID != 'L03-214-2 (With Upper Oso)' & FACILITYID != 'L03-141-1 (With Upper Oso)'& FACILITYID != 'L03-073-3 (With Upper Oso)') 
        #select('FACILITYID', 'JURISDICTI', 'JURISDICTI2', 'PERCENTAGE',  'INSPECTED','POINT_Y', 'POINT_X', 'PERSISTENTFLOW', 'OCFS', 'DRYSAMPLED', 'ACCESSIBILITY', 'area_acres', 'AREA', 'AVGDISCHARGE') 
              
           
      saveRDS(JURISinTRIBS_MO, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/JURISinTRIBS_MO.rds'))
      write_csv(JURISinTRIBS_MO, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/JURISinTRIBS_MO.csv'))
      
      values <- list() 
      values[['R9_Cities']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/R9_Cities.rds'
      R9_Cities <-  readRDS(values[["R9_Cities"]])
      
      values <- list() 
      values[['DischargePointTrib']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DischargePointTrib.rds'
      DischargePointTrib <-  readRDS(values[["DischargePointTrib"]])
      
      
      JURISinTRIBS2 <- JURISinTRIBS_MO %>% 
        as_tibble() %>%
        #create a new column for jurisdiction (use JURISDICTI2, except for when no tributary is drawn, use jurisdiction of discharge point)
        mutate(JURISDICTI3 = JURISDICTI2) %>%
        mutate(JURISDICTI3=coalesce(JURISDICTI3, JURISDICTI)) %>%
        full_join(., R9_Cities, by=c('JURISDICTI3' = 'JURISDICTI')) %>%
        left_join(., DischargePointTrib, by=c('FACILITYID', 'JURISDICTI', 'INSPECTED', 'POINT_Y', 'POINT_X', 'PERSISTENTFLOW', 'OCFS', 'DRYSAMPLED', 'ACCESSIBILITY', 'area_acres', 'AVGDISCHARGE'))
      
      saveRDS(JURISinTRIBS2, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/JURISinTRIBS2.rds'))
      write_csv(JURISinTRIBS2, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/JURISinTRIBS2.csv'))
      
      JURISinTRIBS2b<-JURISinTRIBS2 %>%
        #left_join(., DischargePointTrib, by=c('FACILITYID', 'JURISDICTI')) %>%
        filter(FACILITYID != "J06-9362-1 (J06-P03)")  %>% #Dairy Fork
        filter(FACILITYID != "J01-9082-5 (J02P08)") %>% #Wood Canyon
        filter(FACILITYID != "L05-049-2" & FACILITYID != "L05-049-1" & FACILITYID != "L05-489-7" & FACILITYID !="L05-489-3" & FACILITYID !="L05-489-4") %>%  #Horno Basin
        filter(FACILITYID != "J03-9234-8" & FACILITYID != "J03-9234-6" & FACILITYID !="J03-9234-5" & FACILITYID !="K01-12032-2 (K01P11)") %>% #Niguel Storm Drain  
        filter(FACILITYID != "L03-141-1" & FACILITYID != "L03-141-3" & FACILITYID != "L03-141-2" & FACILITYID != "L03-172-2" & FACILITYID != "L03-172-3" & FACILITYID != "L03-073-3" & FACILITYID != "L03-073-4" & FACILITYID != "L03-073-5" & FACILITYID != "L03-074-2" & FACILITYID != "L03-074-1 (L03B01)") %>% #Oso Creek
        filter(FACILITYID != "L04-672-1" & FACILITYID != "L04-136-1u (L04P07)") %>%
        filter(FACILITYID != "J03-9199-2" & FACILITYID != "J03-9190-1" & FACILITYID != "J03-9199-1") %>%
        filter(FACILITYID != "K01-12156-6"   & FACILITYID != "K01-12156-4") %>% #Salt Creek
        filter(FACILITYID != "M02-052-3" & FACILITYID != "M02-052-4" & FACILITYID != "M02-032-1" & FACILITYID != "M02-085-1 (M02P06)" & FACILITYID != "M02-085-2" & FACILITYID != "M02-013-1" & FACILITYID != "M02-086-1" & FACILITYID != "M02-015-1" & FACILITYID != "M02-028-2 (M02P08)" & FACILITYID != 'M02-061-7' & FACILITYID != 'M02-102-1') %>%   #Segunda Deshecha Channel
        filter(FACILITYID != "M01-008-1" & FACILITYID != "M01-060-3" & FACILITYID != "M01-124-4") %>%
        filter(FACILITYID != "M00.1-070-6" & FACILITYID != "M00.1-070-4" & FACILITYID != "M00.1-070-3" & FACILITYID != "M00.1-070-2" & FACILITYID != 'M00.1-070-1' & FACILITYID !=  "M00.1-071-1 (M00S04)" & FACILITYID !=  "M00.1-071-4 (M00S04)" & FACILITYID !=  "M00.1-071-3 (M00S04)") %>% #coastal SC
        filter(FACILITYID != "I01-11503-3"  & FACILITYID != "I01-11503-4" & FACILITYID != "I01-11502-1" & FACILITYID != "I01-11216-3" & FACILITYID != "I01-11216-2 (I02P12)" & FACILITYID != "I01-11216-1 (I02P13)" & FACILITYID != "I01-11216-4 (I02P14)" & FACILITYID != "I01-11217-1") %>%  #Laguna Canyon Wash
        filter(FACILITYID != "L01-613-1" & FACILITYID != "L01-728-7 (L01S03)")
      
      JURISinTRIBS2b<- JURISinTRIBS2b %>%
        group_by(JURISDICTI3) %>%
        mutate(sumNA=sum(is.na(AREA))) %>%
        ungroup() %>%
        tibble() 
      
      #estimate acreage at tribs with delineations    
      JURISinTRIBS2c<- JURISinTRIBS2b %>%
        filter(!is.na(AREA)) %>%
        group_by(JURISDICTI3) %>%
        mutate(AcresJO=sum(AREA)) %>% 
        ungroup() %>%
        select('FACILITYID','AREA', 'area_acres', 'JURISDICTI3', 'AreaJ', 'AcresJO', 'sumNA', 'PERCENTAGE') %>%
        unique() %>% 
        filter(!is.na(AreaJ))
      
      AcresJ0 <- JURISinTRIBS2c %>%  #area of delineated tribs
        select('AcresJO', 'JURISDICTI3', 'sumNA') %>%
        unique()
      
      DischargePointTribAfilla <- JURISinTRIBS2b %>%  #outfalls without tribs delineated
        filter(is.na(AREA)) %>%
        left_join(., AcresJ0, by=c('JURISDICTI3', 'sumNA')) %>%
        mutate(PERCENTAGE=as.numeric(100)) %>%
        select('FACILITYID','AREA', 'area_acres', 'JURISDICTI3', 'AreaJ', 'AcresJO', 'sumNA', 'PERCENTAGE') 
      
      DischargePointTrib_alla <- bind_rows(DischargePointTribAfilla, JURISinTRIBS2c) %>% #areas with and without delineations
        right_join(., JURISinTRIBS2, by=c('FACILITYID', 'AREA', 'JURISDICTI3', 'AreaJ', 'area_acres', 'PERCENTAGE')) %>% #join to get all outfall, not just outfalls nearest receiving water
        replace_na(list(PERCENTAGE='100')) %>%
        mutate(AcresOb= ifelse(is.na(AREA), ((AreaJ-AcresJO)/sumNA), AREA)) %>%
        group_by(JURISDICTI3) %>%
        mutate(PERCENT_o =(100*(AcresOb/AreaJ)))
      
      saveRDS(DischargePointTrib_alla, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DischargePointTrib_all.rds'))
      write_csv(DischargePointTrib_alla, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DischargePointTrib_all.csv'))
      
      
    ##Task 5.1: Filter out wet weather from instantaneous flow data
    #use outfall field screen data from outafll with persistent flow (need to redefine persistent flow)
   
    values <- list()  
    values[['FieldScreening']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/FieldScreening.rds'
    FieldScreeningb <-  readRDS(values[["FieldScreening"]]) %>%
      mutate(Dischargecfs=(6*FLOWWIDTH*FLOWDEPTH)/FLOWVELOCITY) %>%
      mutate(QDailycfIns=Dischargecfs*60*60*24) %>%
      #mutate(DischargeAVG=(6*AVGFLOWWIDTH*AVGFLOWDEPTH)/AVGFLOWVEL) %>%
      mutate(FlowMeasurementMethod = ifelse(!is.na(Dischargecfs), 'FloatingLeaf', NA)) %>%
      #filter(PERSISTENTFLOW == "Yes") %>%  #update what persistent flow is  #
      select('date', 'FACILITYID','JURISDICTI', 'GlobalID', 'swDischargePointGUID','FLOWCOND', 'Dischargecfs', 'QDailycfIns','FlowMeasurementMethod', 'FLOWCONNECTIVITY', 'AVGDISCHARGE', 'PERSISTENTFLOW', 'DRYSAMPLED')
      #select(date, FACILITYID,SIZE1, FACTYPE, QDailycfIns, JURISDICTI, OCFS, DRYSAMPLED, FLOWCOND, PERSISTENTFLOW, Discharge, FLOWCONNECTIVITY)
    #assign flowing observations at L04-136-1  to L04-136-1u
  
    FieldScreeningb <- within(FieldScreeningb, FACILITYID[FACILITYID == 'L04-136-1 (L04P07)' & FLOWCOND == 'Flowing'] <- 'L04-136-1u (L04P07)')
    #FieldScreeningb$FACILITYID<-sub("J03-9216-1", "J03-9216-1 (J01P01)",FieldScreeningb$FACILITYID)
    saveRDS(FieldScreeningb, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/FieldScreenb.csv'))
    write_csv(FieldScreeningb, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/FieldScreenb.csv'))
    
  str(FieldScreeningb)
    
    values <- list()  
    values[['RainInf']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/RainInf.rds'
    RainInf <-  readRDS(values[["RainInf"]]) 
    
      
    FieldScreen_dry <- FieldScreeningb %>%
      left_join(., RainInf, by = c('FACILITYID' = 'FacilityID','date' = 'date')) %>%
            filter(wet_within_72 == "FALSE")  
      
      saveRDS(FieldScreen_dry, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/FieldScreen_dry.rds'))
      write_csv(FieldScreen_dry, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/FieldScreen_dry.csv'))
      
     
##Task 5.2: determine what method to use for each outfall (only cont, only inst, both, none)
      #monthly
      #compare monthly flow volumes
      #continuous
      #Monthly dry weather flow volume based on continuous flow measurements
     
      #instant
      #Monthly dry weather flow volume based on instantenous flow measurements
      
      #annual
      
      #Find medians first then determine annual flow volume
      #continuous  #change name from monthly to annual

      values <- list() 
      values[['DailyQ_2018R_d']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DailyQ_2018R_d.rds'
      DailyQ_2018R_d <-  readRDS(values[["DailyQ_2018R_d"]])
      
      values <- list()
      values[['DailyQ_2020OFP_d']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DailyQ_2020OFP_d.rds'
      DailyQ_2020OFP_d <-  readRDS(values[["DailyQ_2020OFP_d"]])
      
      values[['DailyQ_OCFS_2021_d']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DailyQ_OCFS_2021_d.rds'
      DailyQ_OCFS_2021_d <-  readRDS(values[["DailyQ_OCFS_2021_d"]])
      
      values[['DailyQ_SWN_2021_d']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DailyQ_SWN_2021_d.rds'
      DailyQ_SWN_2021_d <-  readRDS(values[["DailyQ_SWN_2021_d"]]) 
      
         
      
  AnnualQ201516R <- DailyQ_2018R_d %>%  
  filter(date > "2015-09-30" & date < "2016-10-01") %>%
  group_by(Station) %>%
  as_tibble() %>%
  group_by(Station) %>%
  #mutate(median201718 = median(Flow..cfs.)) %>%
  summarise(medianRcfs = median(Flow..cfs.)) %>%
  mutate(MY = 'MY2015-16') %>%
  ungroup()

AnnualQ201516OFP <- DailyQ_2020OFP_d %>%  
  filter(date > "2015-09-30" & date < "2016-10-01") %>%
  group_by(Station) %>%
  as_tibble() %>%
  group_by(Station) %>%
  #mutate(median201718 = median(Flow..cfs.)) %>%
  summarise(medianOFPcfs = median(Flow..cfs.)) %>%
  mutate(MY = 'MY2015-16') %>%
  ungroup()

AnnualQ201516<-full_join(AnnualQ201516R,AnnualQ201516OFP, by=c('Station','MY'))


AnnualQ201617R <- DailyQ_2018R_d %>%  
  filter(date > "2016-09-30" & date < "2017-10-01") %>%
  group_by(Station) %>%
  as_tibble() %>%
  group_by(Station) %>%
  #mutate(median201718 = median(Flow..cfs.)) %>%
  summarise(medianRcfs = median(Flow..cfs.))  %>%
  mutate(MY = 'MY2016-17') %>%
  ungroup()

AnnualQ201617OFP <- DailyQ_2020OFP_d %>%  
  filter(date > "2016-09-30" & date < "2017-10-01") %>%
  group_by(Station) %>%
  as_tibble() %>%
  group_by(Station) %>%
  #mutate(median201718 = median(Flow..cfs.)) %>%
  summarise(medianOFPcfs = median(Flow..cfs.))  %>%
  mutate(MY = 'MY2016-17') %>%
  ungroup()

AnnualQ201617<-full_join(AnnualQ201617R,AnnualQ201617OFP, by=c('Station','MY'))

AnnualQ201718R <- DailyQ_2018R_d %>%  
  filter(date > "2017-09-30" & date < "2018-10-01") %>%
  group_by(Station) %>%
  as_tibble() %>%
  group_by(Station) %>%
  #mutate(median201718 = median(Flow..cfs.)) %>%
  summarise(medianRcfs = median(Flow..cfs.)) %>%
  mutate(MY = 'MY2017-18') %>%
  ungroup()

AnnualQ201718OFP <- DailyQ_2020OFP_d %>%  
  filter(date > "2017-09-30" & date < "2018-10-01") %>%
  group_by(Station) %>%
  as_tibble() %>%
  group_by(Station) %>%
  #mutate(median201718 = median(Flow..cfs.)) %>%
  summarise(medianOFPcfs = median(Flow..cfs.)) %>%
  mutate(MY = 'MY2017-18') %>%
  ungroup()

AnnualQ201718<-full_join(AnnualQ201718R,AnnualQ201718OFP, by=c('Station','MY'))

AnnualQ201819R <- DailyQ_2018R_d %>%  
  filter(date > "2018-09-30" & date < "2019-10-01") %>%
  group_by(Station) %>%
  as_tibble() %>%
  group_by(Station) %>%
  #mutate(median201718 = median(Flow..cfs.)) %>%
  summarise(medianRcfs = median(Flow..cfs.)) %>%
  mutate(MY = 'MY2018-19') %>%
  ungroup()

AnnualQ201819OFP <- DailyQ_2020OFP_d %>%  
  filter(date > "2018-09-30" & date < "2019-10-01") %>%
  group_by(Station) %>%
  as_tibble() %>%
  group_by(Station) %>%
  #mutate(median201718 = median(Flow..cfs.)) %>%
  summarise(medianOFPcfs = median(Flow..cfs.)) %>%
  mutate(MY = 'MY2018-19') %>%
  ungroup()

AnnualQ201819<-full_join(AnnualQ201819R,AnnualQ201819OFP, by=c('Station','MY'))

AnnualQ201920OFP <- DailyQ_2020OFP_d %>%  
  filter(date > "2019-09-30" & date < "2020-10-01") %>%
  group_by(Station) %>%
  as_tibble() %>%
  group_by(Station) %>%
  #mutate(median201718 = median(Flow..cfs.)) %>%
  summarise(medianOFPcfs = median(Flow..cfs.)) %>%
  mutate(MY = 'MY2019-20') %>%
  ungroup()

AnnualQ201920<-AnnualQ201920OFP

AnnualQ20202021O <- DailyQ_OCFS_2021_d %>%  
  filter(date > "2020-09-30" & date < "2021-10-01") %>%
  group_by(Station) %>%
  as_tibble() %>%
  group_by(Station) %>%
  #mutate(median201718 = median(Flow..cfs.)) %>%
  summarise(medianOCFS2021_cfs = median(Flow..cfs.)) %>%
  mutate(MY = 'MY2020-21') %>%
  ungroup()

AnnualQ20202021S <- DailyQ_SWN_2021_d %>%  
  filter(date > "2020-09-30" & date < "2021-10-01") %>%
  group_by(Station) %>%
  as_tibble() %>%
  group_by(Station) %>%
  #mutate(median201718 = median(Flow..cfs.)) %>%
  summarise(medianSWN2021_cfs = median(Flow..cfs.)) %>%
  mutate(MY = 'MY2020-21') %>%
  ungroup()


AnnualQ20202021 <-full_join(AnnualQ20202021O,AnnualQ20202021S, by=c('Station','MY'))


MedianQ_cont<-bind_rows(AnnualQ201516,AnnualQ201617,AnnualQ201718,AnnualQ201819,AnnualQ201920,AnnualQ20202021) 
#update station names
MedianQ_cont$Station<-sub("K01-12177-1", "K01-12177-1 (K01P07)",MedianQ_cont$Station)
MedianQ_cont$Station<-sub("M01-050-4", "M01-050-4 (M01@CGV)",MedianQ_cont$Station)
 ##TO DO:  make MonitoringYear consistent colnames(MedianQ_cont)[colnames(MedianQ_cont) == "MY"] <- "MonitoringYear"

saveRDS(MedianQ_cont, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/MedianQ_cont.rds'))
write_csv(MedianQ_cont, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/MedianQ_cont.csv'))

#compute annual flow from continuous  (see line 161 - DryDaysYearGroup)
values <- list()  
values[['MedianQ_cont']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/MedianQ_cont.rds'
MedianQ_cont <-  readRDS(values[["MedianQ_cont"]])

values <- list() 
values[['HistoricQ_medians2016']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/HistoricQ_medians2016.rds'
HistoricQ_medians2016 <-  readRDS(values[["HistoricQ_medians2016"]])
values[['Outfall_juris']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/Outfall_juris.rds'
Outfall_juris <-  readRDS(values[["Outfall_juris"]])

#update station names 

values[['Reprioritization_medians2018']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/Reprioritization_medians2018.rds'
Reprioritization_medians2018 <-  readRDS(values[["Reprioritization_medians2018"]])

Reprioritization_medians2018$FACILITYID<-sub("L03-455-3", "L03-455-3 (L03P37)",Reprioritization_medians2018$FACILITYID)
Reprioritization_medians2018$FACILITYID<-sub("M02-062-2", "M02-052-2",Reprioritization_medians2018$FACILITYID)
Reprioritization_medians2018$FACILITYID<-sub("L01-728-5", "L01-728-5 (L01-DP)",Reprioritization_medians2018$FACILITYID)

Reprioritization_medians2018 <- Reprioritization_medians2018  %>%
  left_join(., Outfall_juris, by=c('FACILITYID')) %>%
  filter(!is.na(JURISDICTI))

GeosyntecPrioritization<-full_join(HistoricQ_medians2016, Reprioritization_medians2018, by=c("FACILITYID", "JURISDICTI"))

saveRDS(GeosyntecPrioritization, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/GeosyntecPrioritization.rds'))
write_csv(GeosyntecPrioritization, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/GeosyntecPrioritization.csv')) #annual flow instantenous flow


#MedianQ_2016AnnualP <- right_join(DryDaysYearGroup, HistoricQ_medians2016,  by=c('FacilityID'='FACILITYID')) %>%
  #mutate(QAnnualcf2016P=DailyQcfMeds2016*dry_days) %>%
  #filter(!is.na(QAnnualcf2016P))
#saveRDS(MedianQ_2016AnnualP, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/MedianQ_2016AnnualP.rds'))
#write_csv(MedianQ_2016AnnualP, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/MedianQ_2016AnnualP.csv')) #annual flow, 2016 Prioritization

#MedianQ_2018AnnualR <- right_join(DryDaysYearGroup, Reprioritization_medians2018,  by=c('FacilityID'='FACILITYID')) %>%
  #mutate(QAnnualcf2018R = `DailyQcfMeds2018`*dry_days) %>%
  #filter(!is.na(QAnnualcf2018R))
#saveRDS(MedianQ_2018AnnualR, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/MedianQ_2018AnnualR.rds'))
#write_csv(MedianQ_2018AnnualR, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/MedianQ_2018AnnualR.csv')) #annual flow, 2018 Prioritization

#instant
#from field screening data, find average for each monitoring year, and multiply by number of dry days
#values <- list()  
#values[['DryDaysYearGroup']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DryDaysYearGroup.rds'
#DryDaysYearGroup <-  readRDS(values[["DryDaysYearGroup"]])

values <- list() 
values[['FieldScreen_dry']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/FieldScreen_dry.rds'
InstQ <-  readRDS(values[["FieldScreen_dry"]]) #line 370

str(InstQ)

InstQ <- InstQ %>%  #flowing only
  select(c("date", "FACILITYID",  "QDailycfIns", "JURISDICTI", "Dischargecfs",  "rain_gage", "FLOWCOND", "FLOWCONNECTIVITY", "AVGDISCHARGE", "PERSISTENTFLOW", "DRYSAMPLED")) %>%
  #select(c("date", "Facility.Identifier", "Facility.Type", "Size.1",  "QDailycfIns", "Jurisdiction", "Dischargecfs", "rain_gage", "Flow.Condition")) %>%
  filter(!is.na(Dischargecfs))  #remove outfalls without flow measurements (put back in later with all flow measurements)

#Define Monitoring Year
InstQ$MonitoringYear<- ifelse(InstQ$date>(as.Date("2020-09-30", origin="1900-01-01")) & InstQ$date<(as.Date("2021-10-01", origin="1900-01-01")), "MY2020-21",
  ifelse(InstQ$date>(as.Date("2019-09-30", origin="1900-01-01")) & InstQ$date<(as.Date("2020-10-01", origin="1900-01-01")), "MY2019-20",
                                      ifelse(InstQ$date>(as.Date("2018-09-30", origin="1900-01-01")) & InstQ$date<(as.Date("2019-10-01", origin="1900-01-01")), "MY2018-19",
                                             ifelse(InstQ$date>(as.Date("2017-09-30", origin="1900-01-01")) & InstQ$date<(as.Date("2018-10-01", origin="1900-01-01")), "MY2017-18",
                                                    ifelse(InstQ$date>(as.Date("2016-09-30", origin="1900-01-01")) & InstQ$date<(as.Date("2017-10-01", origin="1900-01-01")), "MY2016-17",
                                                           ifelse(InstQ$date>(as.Date("2015-09-30", origin="1900-01-01")) & InstQ$date<(as.Date("2016-10-01", origin="1900-01-01")), "MY2015-16",
                                                                  ifelse(InstQ$date>(as.Date("2014-09-30", origin="1900-01-01")) & InstQ$date<(as.Date("2015-10-01", origin="1900-01-01")), "MY2014-15",  
                                                                         NA)))))))


saveRDS(InstQ, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/InstQ.rds'))
write_csv(InstQ, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/InstQ.csv')) #annual flow instantenous flow

## Task 6*: Determine site-specific instantaneous:continuous flow ratio
#Daily Results

#Monthly Results
values <- list()  
values[['Outfall_juris']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/Outfall_juris.rds'
Outfall_juris<-readRDS(values[["Outfall_juris"]])

values[['InstQ']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/InstQ.rds'
InstQ<-readRDS(values[["InstQ"]])

values <- list()  
values[['MajorOutfalls_lc']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/MajorOutfalls_lc.rds'
MajorOutfalls_lc <-  readRDS(values[["MajorOutfalls_lc"]]) 
MajorOutfalls_avQ <- MajorOutfalls_lc %>%
  select('FACILITYID', 'AVGDISCHARGE', 'DRYSAMPLED','PERSISTENTFLOW', 'INSPECTED', 'OCFS', 'POINT_Y', 'POINT_X', 'ACCESSIBILITY','JURISDICTI') %>%
  mutate(avgdailyQ=AVGDISCHARGE*24*60*60) %>%
  mutate(avgdailyQadj=avgdailyQ*.69)

saveRDS(MajorOutfalls_avQ, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/MajorOutfalls_avQ.rds'))
write_csv(MajorOutfalls_avQ, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/MajorOutfalls_avQ.csv')) #annual flow instantenous flow



## Task 7.1: (Only) continuous data
#list of outfalls with (only) continuous data
#There are not any outfalls with only continuous flow data
values <- list()  
values[['MedianQ_cont']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/MedianQ_cont.rds'

MedianQ_cont<-readRDS(values[["MedianQ_cont"]]) %>% 
  left_join(., MajorOutfalls_avQ, by=c('Station'='FACILITYID')) %>% #find daily average
mutate(QDailycfContRcf=medianRcfs*60*60*24) %>%
  mutate(QDailycfContOFPcf=medianOFPcfs*60*60*24) %>%
  mutate(QDailycfContOCFScf=medianOCFS2021_cfs*60*60*24) %>%
  mutate(QDailycfContSWNcf=medianSWN2021_cfs*60*60*24)

colnames(MedianQ_cont)[colnames(MedianQ_cont) == "Station"] <- "FACILITYID"

#colnames(MedianQ_cont)[colnames(MedianQ_cont) == "MY"] <- "MonitoringYear"

## Task 7.2: (Only) instantaneous data #add dry days later (10/28/2021)

values <- list()  
values[['InstQ']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/InstQ.rds'
InstQ<-readRDS(values[["InstQ"]]) %>%  
  left_join(., MajorOutfalls_avQ, by=c('FACILITYID', "JURISDICTI", 'AVGDISCHARGE', 'DRYSAMPLED', 'PERSISTENTFLOW')) %>% 
mutate(QInsDailyCFAdj=QDailycfIns*0.69) #0.69 adjustment for Inst flow

## Task 7.3*: Both continuous and instantaneous data (and flow prioritization files)
values <- list()  
values[['GeosyntecPrioritization']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/GeosyntecPrioritization.rds'
GeosyntecPrioritization<-readRDS(values[["GeosyntecPrioritization"]]) 
  #add_column(MonitoringYear=NA, .after= "Continuous_20181920")


DailyAvgCF <- left_join(InstQ, MedianQ_cont, by=c("FACILITYID", "JURISDICTI","AVGDISCHARGE", "DRYSAMPLED", 'PERSISTENTFLOW', "OCFS", "INSPECTED","POINT_Y", "POINT_X", "ACCESSIBILITY", "avgdailyQ", "avgdailyQadj")) %>%
  full_join(., GeosyntecPrioritization, by=c("FACILITYID", "JURISDICTI"))
  
 
saveRDS(DailyAvgCF, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DailyAvgCF.rds'))
write_csv(DailyAvgCF, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DailyAvgCF.csv')) 

#AnnualCF$QAnnualcfContOFPcf <- replace(AnnualCF$QAnnualcfContOFPcf,AnnualCF$QAnnualcfContOFPcf == 0.0, NA)  

## Task 7.4*: No flow measurements
#could be ponded or dry (should this include un-verified and not found outfalls?)

stn_near <-"A.2 Outfall Assessments/DryWeatherLoadingCalcs/Input/Rain/swDischargePoint_Near_Coop.csv"

stn_near <- read.csv(stn_near)

values[['MajorOutfalls_avQ']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/MajorOutfalls_avQ.rds'
MajorOutfalls_avQ <-  readRDS(values[["MajorOutfalls_avQ"]]) %>%
  select(FACILITYID, JURISDICTI, AVGDISCHARGE)

values <- list()  
values[['FieldScreen_dry']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/FieldScreen_dry.rds'
NoQ <-  readRDS(values[["FieldScreen_dry"]]) %>%
  filter(date > '2020-09-30' & date <'2021-10-01' ) %>%
  right_join(., MajorOutfalls_avQ) %>%
  left_join(., stn_near, by=c('FACILITYID'='FacilityID')) %>%
  filter(is.na(QDailycfIns)) %>%
  as_tibble() %>%
  #filter(FLOWCOND!=3) %>% #remove dry obs later down in script
  select(date, FACILITYID, JURISDICTI,  FLOWCOND, Dischargecfs, AVGDISCHARGE,FLOWCONNECTIVITY, PERSISTENTFLOW, DRYSAMPLED) 


NoQ$MonitoringYear<- 
  ifelse(ifelse(NoQ$date>(as.Date("2020-09-30", origin="1900-01-01")) & NoQ$date<(as.Date("2021-10-01", origin="1900-01-01")), "MY2020-21",
                NA)) %>%
  replace_na(list(MonitoringYear= 'MY2020-21')) 



NoQ$MonitoringYear <- as.character(NoQ$MonitoringYear)

str(NoQ)

#filter(FLOWCONNECTIVITY != "1")  #only connectivity should this be applied later in the calculations? 

saveRDS(NoQ, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/NoQ.rds'))
write_csv(NoQ, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/NoQ.csv'))
###Combine all daily flow files
#join in ponded outfalls

#first find average of previous measurements from outfall.  Then for remaining without any measurements, find jurisdictional average
values <- list()  
values[['MajorOutfalls_avQ']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/MajorOutfalls_avQ.rds'
MajorOutfalls_avQ <-  readRDS(values[["MajorOutfalls_avQ"]]) %>%
  select(FACILITYID, AVGDISCHARGE)

values <- list()  
values[['NoQ']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/NoQ.rds'
NoQ_f <-  readRDS(values[["NoQ"]]) %>%
  #select(date, FACILITYID, JURISDICTI,JURISDICTI2, Dischargecfs, FLOWCONNECTIITY) %>%
 
  # filter(FACILITYID != "DP04-12015-1"& FACILITYID !="J01-10004-2" & FACILITYID !="J01-10006-3" & FACILITYID !="J01-9005-3" & FACILITYID !="J01-9040-1" & FACILITYID !="J01-9046-2" & FACILITYID !="J01-9066-1 (J01P04)" & FACILITYID !="J01-9082-5 (J02P08)" & FACILITYID !="J01-9131-1 (J01P28)" & FACILITYID !="J01-9273-1" & FACILITYID !="J01-9349-1" & FACILITYID !="J01-9377-1" & FACILITYID !="J01-9785-1"	& FACILITYID !="J03-9221-1 (J03P02)")		
  #right_join(., MajorOutfalls_avQ, by=c('FACILITYID'='FACILITYID', 'JURISDICTI', 'AVGDISCHARGE')) %>%
  replace_na(list(Dischargecfs=-0.99)) %>%
  mutate(avgdailyQ=AVGDISCHARGE*24*60*60) %>%  #estimate outfalls with no measurements with average
  mutate(avgdailyQadj=(avgdailyQ*.69)) %>%
  filter(!is.na(FACILITYID)) %>%
  unique()


NoQ_f$MonitoringYear<- 
  ifelse(NoQ_f$date>(as.Date("2019-09-30", origin="1900-01-01")) & NoQ_f$date<(as.Date("2020-10-01", origin="1900-01-01")), "MY2019-20",
                              ifelse(NoQ_f$date>(as.Date("2018-09-30", origin="1900-01-01")) & NoQ_f$date<(as.Date("2019-10-01", origin="1900-01-01")), "MY2018-19",
                                     ifelse(NoQ_f$date>(as.Date("2020-09-30", origin="1900-01-01")) & NoQ_f$date<(as.Date("2021-10-01", origin="1900-01-01")), "MY2020-21",     
                                     NA)))

colnames(NoQ_f)[colnames(NoQ_f) == "Dischargecfs"] <- "DischargeP"  #includes ponded AND flowing outfalls -sometimes flow measurements were not recorded

NoQ_f <-  NoQ_f %>%
 
  filter(MonitoringYear=='MY2020-21'|is.na(MonitoringYear)) %>% 
  replace_na(list(MonitoringYear= 'MY2020-21')) 
 
  

saveRDS(NoQ_f, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/NoQ_f.rds'))
write_csv(NoQ_f, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/NoQ_f.csv'))

## Task 8*: Compute flow volume results from four methods (Task 6.1-6.4) #
#Combine Daily flow file and no flow files to estimate flow at outfalls without flow measurements
values <- list()  

values[['DailyAvgCF']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DailyAvgCF.rds'
DailyAvgCF <-  readRDS(values[["DailyAvgCF"]])
values[['NoQ_f']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/NoQ_f.rds'
NoQ_f <-  readRDS(values[["NoQ_f"]])
values[['DailyAvgCF']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DailyAvgCF.rds'
DailyAvgCF <-  readRDS(values[["DailyAvgCF"]]) %>%
  filter(MonitoringYear == 'MY2020-21'|is.na(MonitoringYear))

values[['MajorOutfalls_avQ']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/MajorOutfalls_avQ.rds'
MajorOutfalls1 <-  readRDS(values[["MajorOutfalls_avQ"]]) %>%
  select(FACILITYID)

values[['MajorOutfalls_avQ']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/MajorOutfalls_avQ.rds'
MajorOutfalls_avQ <-  readRDS(values[["MajorOutfalls_avQ"]]) %>%
  select(FACILITYID, JURISDICTI, AVGDISCHARGE)

#DailyCFandPond <- NoQ_f %>%   
  #select(-c(date)) %>%
  #right_join(., MajorOutfalls_avQ, by=c('FACILITYID', 'AVGDISCHARGE', 'JURISDICTI'))

AnnualCF <- full_join(DailyAvgCF,NoQ_f, by=c('date', 'FACILITYID', 'JURISDICTI', 'MonitoringYear', 'AVGDISCHARGE', 'FLOWCOND', 'FLOWCONNECTIVITY', 'PERSISTENTFLOW', 'DRYSAMPLED', 'avgdailyQ', 'avgdailyQadj'))  %>% 
  #right_join(., MajorOutfalls_avQ, by=c('FACILITYID', 'JURISDICTI', 'AVGDISCHARGE', 'POINT_Y', 'POINT_X', 'DRYSAMPLED', 'OCFS', 'ACCESSIBILITY','INSPECTED', 'avgdailyQ', 'avgdailyQadj')) %>%
  #filter(MonitoringYear == 'MY2020-21'|is.na(MonitoringYear)) %>%
  group_by(FACILITYID, MonitoringYear) %>%  #Find average annual flow for each outfall
  mutate(Q_DailyavgCF= ifelse(!is.na(QInsDailyCFAdj),mean(c(QDailycfContRcf,QDailycfContOFPcf, QDailycfContOCFScf, QDailycfContSWNcf, DailyQcfMeds2018, DailyQcfMeds2016b,DailyQcfMeds2016, QInsDailyCFAdj), na.rm=TRUE), avgdailyQadj)) %>%
  ungroup() %>%
  unique() %>%
select(-c(MY, rain_gage)) 
# filter(FLOWCOND ==1|FLOWCOND ==2|FLOWCOND ==4|is.na(FLOWCOND)) filter for dry days later
 
saveRDS(AnnualCF, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/AnnualCF.rds'))
write_csv(AnnualCF, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/AnnualCF.csv'))

getwd()
#Estimate flow at outfalls without measurements
values <- list()  
values[['JURISinTRIBS2']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/JURISinTRIBS2.rds'
JURISinTRIBS2 <-  readRDS(values[["JURISinTRIBS2"]]) %>%
  select(FACILITYID, JURISDICTI3, JURISDICTI, PERCENTAGE, area_acres, AREA)

values[['AnnualCF']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/AnnualCF.rds'
AnnualCF <-  readRDS(values[["AnnualCF"]])

values[['MajorOutfalls_avQ']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/MajorOutfalls_avQ.rds'
MajorOutfalls_avQ <-  readRDS(values[["MajorOutfalls_avQ"]]) %>%
  as.data.frame() %>%
  select(FACILITYID, JURISDICTI, AVGDISCHARGE, avgdailyQadj)

DailyCFandPond_trib <- full_join(AnnualCF, JURISinTRIBS2,  by=c('FACILITYID', 'JURISDICTI')) %>%
  filter(!is.na(JURISDICTI3)) %>% #filters out non major outfalls
  filter(!is.na(MonitoringYear))
       
saveRDS(DailyCFandPond_trib, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DailyCFandPond_trib.rds'))
write_csv(DailyCFandPond_trib, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DailyCFandPond_trib.csv'))

#install.packages("psych")          # Install psych package
library("psych")
values <- list()  
values[['DailyCFandPond_trib']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DailyCFandPond_trib.rds'
DailyCFandPond_trib <-  readRDS(values[["DailyCFandPond_trib"]]) 

values[['MajorOutfalls_avQ']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/MajorOutfalls_avQ.rds'
MajorOutfalls_avQ <-  readRDS(values[["MajorOutfalls_avQ"]])  %>% 
  select(FACILITYID)


DailyCFandPond_trib2a <-  DailyCFandPond_trib %>%    #estimate flow at ponded outfalls and outfalls not visited from average daily (instead of annual)
  filter(FLOWCOND!='3'|is.na(FLOWCOND)) %>% 
  replace_na(list(PERCENTAGE=100)) %>%
  filter(!is.na(MonitoringYear)) %>%
  #select(FACILITYID, JURISDICTI, JURISDICTI3,date,FLOWCOND, DRYSAMPLED,  QAnnualcfContOFPcf, QAnnualcf2016P, QAnnualcf2018R,QInsAnnualCF, QInsAnnualCFAdj, DischargeP, QavgCF, MonitoringYear, PERCENTAGE, AVGDISCHARGE, FLOWCONNECTIVITY, area_acres, Acres) %>%
  select(FACILITYID, JURISDICTI, JURISDICTI3,date,FLOWCOND, PERSISTENTFLOW, DRYSAMPLED, DischargeP, Q_DailyavgCF, MonitoringYear, PERCENTAGE, AVGDISCHARGE, avgdailyQ,avgdailyQadj, FLOWCONNECTIVITY, area_acres, AREA) %>%
  group_by(FACILITYID, MonitoringYear) %>%
  mutate(DischargePCF = ifelse(DischargeP < 0 & is.na(Q_DailyavgCF), mean(Q_DailyavgCF), Q_DailyavgCF)) %>% #estimates flow for outfalls without flow measurements from prior measurements
  #estimate discharge at ponded outfalls - need to group by JURIS3
  mutate(DischargePCF2= ifelse(!is.na(PERCENTAGE), PERCENTAGE*DischargePCF/100, DischargePCF)) %>%
  group_by(JURISDICTI3, MonitoringYear) %>% #use geomean instead of log mean (July 27, 2021; 2019-20 Annual Report used arithmetic average)
  mutate(DischargePCF3 = ifelse(DischargeP < 0 & is.na(DischargePCF2) , geometric.mean(DischargePCF2, na.rm=TRUE), DischargePCF2)) %>%   #only calculates for outfalls with no flow measurements, but column includes daily flow for outfalls with data
  ungroup() 
# filter(!is.na(DischargePCF3)) 

DailyCFandPond_trib2b <-  DailyCFandPond_trib %>%  #marks dry observations as 0, but keeps in record
  select(FACILITYID, JURISDICTI, JURISDICTI3,date,FLOWCOND, DRYSAMPLED, PERSISTENTFLOW, DischargeP, Q_DailyavgCF, MonitoringYear, PERCENTAGE, AVGDISCHARGE, avgdailyQ, avgdailyQadj, FLOWCONNECTIVITY, area_acres, AREA) %>%
  filter(FLOWCOND =='3') %>% 
  mutate(DischargePCF = as.numeric(0)) %>%
  mutate(DischargePCF2 = as.numeric(0)) %>%
  mutate(DischargePCF3 = as.numeric(0)) 

DailyCFandPond_trib2 <-rbind(DailyCFandPond_trib2a, DailyCFandPond_trib2b) 

saveRDS(DailyCFandPond_trib2, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DailyCFandPond_trib2.rds'))
write_csv(DailyCFandPond_trib2, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DailyCFandPond_trib2.csv'))

#Annual Flow volume - join in dry_days_file
values <- list()  
values[['DryDaysYearGroup']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DryDaysYearGroup.rds'
DryDaysYearGroup <-  readRDS(values[["DryDaysYearGroup"]]) %>%
  filter(!is.na(FacilityID)) %>%
  filter(MonitoringYear=='MY2020-21')

#DryDaysYearGroup$FacilityID<-sub("J01-9224-2", "J01-9224-2 (J01P25)",DryDaysYearGroup$FacilityID)
#DryDaysYearGroup$FacilityID<-sub("J03-9216-2", "J03-9216-2 (J03P01)",DryDaysYearGroup$FacilityID)
AnnualCF2b <- DailyCFandPond_trib2 %>%
  left_join(.,DryDaysYearGroup, by=c('FACILITYID'='FacilityID', 'MonitoringYear')) %>%
  filter(MonitoringYear=='MY2020-21') %>%
  group_by(FACILITYID, JURISDICTI3) %>%
  mutate(AnnualCFb=mean(DischargePCF3)) %>%
  mutate(AnnualCF = dry_days*AnnualCFb)

saveRDS(AnnualCF2b, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/AnnualCF2b.rds'))
write_csv(AnnualCF2b, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/AnnualCF2b.csv'))


#Find annual flow volume by jurisdiction
#Remove outfalls after volumes determined
AnnualCF2 <- AnnualCF2b %>%  #remove outfalls upstream of most downstream outfall closest to receiving water
  filter(FACILITYID != "J06-9362-1 (J06-P03)")  %>% #Dairy Fork
  filter(FACILITYID != "J01-9082-5 (J02P08)") %>% #Wood Canyon
  filter(FACILITYID != "L05-049-2" & FACILITYID != "L05-049-1" & FACILITYID != "L05-489-7" & FACILITYID !="L05-489-3" & FACILITYID !="L05-489-4") %>%  #Horno Basin
  filter(FACILITYID != "J03-9234-8" & FACILITYID != "J03-9234-6" & FACILITYID !="J03-9234-5" & FACILITYID !="K01-12032-2 (K01P11)") %>% #Niguel Storm Drain  
  filter(FACILITYID != "L03-141-1" & FACILITYID != "L03-141-3" & FACILITYID != "L03-141-2" & FACILITYID != "L03-172-2" & FACILITYID != "L03-172-3" & FACILITYID != "L03-073-3" & FACILITYID != "L03-073-4" & FACILITYID != "L03-073-5" & FACILITYID != "L03-074-2" & FACILITYID != "L03-074-1 (L03B01)") %>% #Oso Creek
  filter(FACILITYID != "L04-672-1" & FACILITYID != "L04-136-1u (L04P07)") %>%
  filter(FACILITYID != "J03-9199-2" & FACILITYID != "J03-9190-1" & FACILITYID != "J03-9199-1") %>%
  filter(FACILITYID != "K01-12156-6"   & FACILITYID != "K01-12156-4") %>% #Salt Creek
  filter(FACILITYID != "M02-052-3" & FACILITYID != "M02-052-4" & FACILITYID != "M02-032-1" & FACILITYID != "M02-085-1 (M02P06)" & FACILITYID != "M02-085-2" & FACILITYID != "M02-013-1" & FACILITYID != "M02-086-1" & FACILITYID != "M02-015-1" & FACILITYID != "M02-028-2 (M02P08)" & FACILITYID != 'M02-061-7' & FACILITYID != 'M02-102-1') %>%   #Segunda Deshecha Channel
  filter(FACILITYID != "M01-008-1" & FACILITYID != "M01-060-3" & FACILITYID != "M01-124-4") %>%
  filter(FACILITYID != "M00.1-070-6" & FACILITYID != "M00.1-070-4" & FACILITYID != "M00.1-070-3" & FACILITYID != "M00.1-070-2" & FACILITYID != 'M00.1-070-1' & FACILITYID !=  "M00.1-071-1 (M00S04)" & FACILITYID !=  "M00.1-071-4 (M00S04)" & FACILITYID !=  "M00.1-071-3 (M00S04)") %>% #coastal SC
  filter(FACILITYID != "I01-11503-3"  & FACILITYID != "I01-11503-4" & FACILITYID != "I01-11502-1" & FACILITYID != "I01-11216-3" & FACILITYID != "I01-11216-2 (I02P12)" & FACILITYID != "I01-11216-1 (I02P13)" & FACILITYID != "I01-11216-4 (I02P14)" & FACILITYID != "I01-11217-1") %>%  #Laguna Canyon Wash
  filter(FACILITYID != "L01-613-1" & FACILITYID != "L01-728-7 (L01S03)")

saveRDS(AnnualCF2, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/AnnualCF2.rds'))
write_csv(AnnualCF2, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/AnnualCF2.csv'))


#For tribs with no area:  find total area accounted for, subtract from total jurisdictional area, divide the remainig acreage equally amongst other jurisdicitons 
values <- list()  
values[['DischargePointTrib']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DischargePointTrib.rds'
DischargePointTrib <-  readRDS(values[["DischargePointTrib"]]) #includes all

values <- list() 
values[['JURISinTRIBS2']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/JURISinTRIBS2.rds'
JURISinTRIBS2 <-  readRDS(values[["JURISinTRIBS2"]])

values[['R9_Cities']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/R9_Cities.rds'
R9_Cities <-  readRDS(values[["R9_Cities"]])
  

DischargePointTrib_p <- DischargePointTrib %>%  #includes all major outfalls
  select('FACILITYID', 'area_acres', 'JURISDICTI') %>%
  full_join(JURISinTRIBS2, by=c('FACILITYID', 'JURISDICTI', 'area_acres')) %>%
mutate(JURISDICTI3 = JURISDICTI2) %>%
  mutate(JURISDICTI3=coalesce(JURISDICTI3, JURISDICTI)) %>%
  select('FACILITYID', 'JURISDICTI', 'JURISDICTI3', 'area_acres','AREA') %>%
  right_join(., MajorOutfalls_avQ) %>%
  full_join(., R9_Cities, by=c('JURISDICTI3' = 'JURISDICTI')) %>%
  unique() %>%  #remove outfalls upstream of most downstream outfall closest to receiving water
  filter(FACILITYID != "J06-9362-1 (J06-P03)")  %>% #Dairy Fork
  filter(FACILITYID != "J01-9082-5 (J02P08)") %>% #Wood Canyon
  filter(FACILITYID != "L05-049-2" & FACILITYID != "L05-049-1" & FACILITYID != "L05-489-7" & FACILITYID !="L05-489-3" & FACILITYID !="L05-489-4") %>%  #Horno Basin
  filter(FACILITYID != "J03-9234-8" & FACILITYID != "J03-9234-6" & FACILITYID !="J03-9234-5" & FACILITYID !="K01-12032-2 (K01P11)") %>% #Niguel Storm Drain  
  filter(FACILITYID != "L03-141-1" & FACILITYID != "L03-141-3" & FACILITYID != "L03-141-2" & FACILITYID != "L03-172-2" & FACILITYID != "L03-172-3" & FACILITYID != "L03-073-3" & FACILITYID != "L03-073-4" & FACILITYID != "L03-073-5" & FACILITYID != "L03-074-2" & FACILITYID != "L03-074-1 (L03B01)") %>% #Oso Creek
  filter(FACILITYID != "L04-672-1" & FACILITYID != "L04-136-1u (L04P07)") %>%
  filter(FACILITYID != "J03-9199-2" & FACILITYID != "J03-9190-1" & FACILITYID != "J03-9199-1") %>%
  filter(FACILITYID != "K01-12156-6"   & FACILITYID != "K01-12156-4") %>% #Salt Creek
  filter(FACILITYID != "M02-052-3" & FACILITYID != "M02-052-4" & FACILITYID != "M02-032-1" & FACILITYID != "M02-085-1 (M02P06)" & FACILITYID != "M02-085-2" & FACILITYID != "M02-013-1" & FACILITYID != "M02-086-1" & FACILITYID != "M02-015-1" & FACILITYID != "M02-028-2 (M02P08)" & FACILITYID != 'M02-061-7' & FACILITYID != 'M02-102-1') %>%   #Segunda Deshecha Channel
  filter(FACILITYID != "M01-008-1" & FACILITYID != "M01-060-3" & FACILITYID != "M01-124-4") %>%
  filter(FACILITYID != "M00.1-070-6" & FACILITYID != "M00.1-070-4" & FACILITYID != "M00.1-070-3" & FACILITYID != "M00.1-070-2" & FACILITYID != 'M00.1-070-1' & FACILITYID !=  "M00.1-071-1 (M00S04)" & FACILITYID !=  "M00.1-071-4 (M00S04)" & FACILITYID !=  "M00.1-071-3 (M00S04)") %>% #coastal SC
  filter(FACILITYID != "I01-11503-3"  & FACILITYID != "I01-11503-4" & FACILITYID != "I01-11502-1" & FACILITYID != "I01-11216-3" & FACILITYID != "I01-11216-2 (I02P12)" & FACILITYID != "I01-11216-1 (I02P13)" & FACILITYID != "I01-11216-4 (I02P14)" & FACILITYID != "I01-11217-1") %>%  #Laguna Canyon Wash
  filter(FACILITYID != "L01-613-1" & FACILITYID != "L01-728-7 (L01S03)") %>%

  group_by(JURISDICTI3) %>%
  mutate(sumNA=sum(is.na(AREA))) %>%
  ungroup() %>%
  tibble() 

saveRDS(DischargePointTrib_p, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DischargePointTrib_p.rds'))
write_csv(DischargePointTrib_p, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DischargePointTrib_p.csv'))

DischargePointTrib_q <- DischargePointTrib_p %>%   #outfalls with tributaries delineated - total area, including outfalls without de-lineated tributaries (e.g. DP01-12022-1), taken into account in calculation, even though they are filtered away
  filter(!is.na(AREA)) %>%
  group_by(JURISDICTI3) %>%
  mutate(AcresJO=sum(AREA)) %>%
  ungroup() %>%
  select('FACILITYID','AREA', 'area_acres', 'JURISDICTI3', 'AreaJ', 'AcresJO', 'sumNA') %>%
  unique() 

AcresJ0 <- DischargePointTrib_q %>%
  select('AcresJO', 'JURISDICTI3', 'sumNA') %>%
  unique()

DischargePointTribAfill <- DischargePointTrib_p %>%  #outfalls without tribs delineated
  filter(is.na(AREA)) %>%
  full_join(., AcresJ0, by=c('JURISDICTI3', 'sumNA')) %>%
  select('FACILITYID','AREA', 'area_acres', 'JURISDICTI3', 'AreaJ', 'AcresJO', 'sumNA') 
     
DischargePointTrib_all <- bind_rows(DischargePointTribAfill, DischargePointTrib_q) %>% #areas with and without delineations
 mutate(AcresOb= ifelse(is.na(AREA), ((AreaJ-AcresJO)/sumNA), AREA))

saveRDS(DischargePointTrib_all, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DischargePointTrib_all.rds'))
write_csv(DischargePointTrib_all, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DischargePointTrib_all.csv'))

#make unique values for JURISDICTI3 and ANNUAL CF. 
values <- list()  
values[['DischargePointTrib_all']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DischargePointTrib_all.rds'
DischargePointTrib_all <-  readRDS(values[["DischargePointTrib_all"]]) 
values[['AnnualCF2']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/AnnualCF2.rds'
AnnualCF2 <-  readRDS(values[["AnnualCF2"]]) 
values[['MajorOutfalls_avQ']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/MajorOutfalls_avQ.rds'
MajorOutfalls_avQ <-  readRDS(values[["MajorOutfalls_avQ"]]) %>%
  select(c(FACILITYID, JURISDICTI))

AnnualCF2<-left_join(AnnualCF2, MajorOutfalls_avQ)

DischargePointTrib_flow <- full_join(AnnualCF2, DischargePointTrib_all, by=c('FACILITYID', 'JURISDICTI3', 'AREA', 'area_acres')) %>% 
  #select(c(-INSPECTED, -ACCESSIBILITY, -DRYSAMPLED)) %>%
  filter(FACILITYID != 'L01-727-1 (L01S04)' | MonitoringYear != 'MY2020-21') %>%
  right_join(., MajorOutfalls_avQ)

saveRDS(DischargePointTrib_flow, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DischargePointTrib_flow.rds'))
write_csv(DischargePointTrib_flow, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DischargePointTrib_flow.csv'))

AnnualCF_J_QC  <- DischargePointTrib_flow %>%
  filter(!is.na(AreaJ)) %>%
  #remove dry outfalls
  select('JURISDICTI3', 'MonitoringYear', 'AnnualCF', 'AreaJ') %>%
  unique() 

saveRDS(AnnualCF_J_QC, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/AnnualCF_J_QC.rds'))
write_csv(AnnualCF_J_QC, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/AnnualCF_J_QC.csv'))

AnnualCF_J  <- DischargePointTrib_flow %>%
  select('JURISDICTI3', 'MonitoringYear', 'AnnualCF', 'AreaJ') %>%
  unique() %>%
  group_by(JURISDICTI3, MonitoringYear) %>%
  mutate(DischargeJ_cf=sum(AnnualCF)) %>%
 mutate(DischargeJ_af=DischargeJ_cf/43560) %>%  #convert to acre-ft; 1 acre-feet = 43560 cf
  select('JURISDICTI3', 'MonitoringYear', 'DischargeJ_cf', 'DischargeJ_af',  'AreaJ') %>%
  mutate(QperArea_cfperacre=DischargeJ_cf/AreaJ) %>%
  unique() %>%
  ungroup() %>%
  filter(!is.na(MonitoringYear)) %>%
  filter(!is.na(AreaJ))
 #discharge per cubic acre without connectivity adjustment 
saveRDS(AnnualCF_J, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/AnnualCF_J.rds'))
write_csv(AnnualCF_J, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/AnnualCF_J.csv'))


#Find discharge per area

#Determine LSPC land-use percentage in each jurisdiction (need to make sure ArcGIS combines all unincorporated parcels together)
  LSPCjur <-"C:/Users/givens/Documents/GitHub/WQIP-Annual-Report-2019-20/A.2 Outfall Assessments/DryWeatherLoadingCalcs/Input/LSPC_in_JURIS_TableToExcel.csv"
  LSPCjur <- read.csv(LSPCjur)  
  
  LSPCjur$JURISDICTI<-as.character(LSPCjur$JURISDICTI) 
  
  LSPCjur <- LSPCjur %>%
    select(JURISDICTI,Summarized.Area.in.ACRES.1) %>%
    unique() %>%
    group_by(JURISDICTI) %>%
    mutate(sumAcresTotal=sum(Summarized.Area.in.ACRES.1)) %>%
    select(-c(Summarized.Area.in.ACRES.1)) %>%
    right_join(LSPCjur, LSPCjur, by=c('JURISDICTI')) %>%
    group_by(JURISDICTI, LSPC_LU_CODE) %>%
    mutate(sumAcresLU=sum(Summarized.Area.in.ACRES)) %>%
    mutate(PercentLU = (sumAcresLU/sumAcresTotal)*100) 
  
  str(LSPCjur)  #file is a result of doing a summarize within with jurisdiciton and land-use, and then joining tables.
  
  LU_codes <-"C:/Users/givens/Documents/GitHub/WQIP-Annual-Report-2019-20/A.2 Outfall Assessments/DryWeatherLoadingCalcs/Input/sbpat_landuse_RCandEMC.csv"
  LU_codes <- read.csv(LU_codes)
  str(LU_codes)
  
  LSPCcodeJuris<-left_join(LSPCjur, LU_codes, by=c('LSPC_LU_CODE' = 'LSPC_LU_CODE')) %>%
    select(JURISDICTI, LSPC_LU_CODE, LSPC_LU_DESC, Description, sumAcresTotal, sumAcresLU,PercentLU,Summarized.Area.in.ACRES, Percent.of.area, Acres, Summarized.Area.in.ACRES.1) 
    
    
    #filter(!is.na(LSPC_LU_CODE))
    
  saveRDS( LSPCcodeJuris, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/LSPCcodeJuris.rds'))
  write_csv( LSPCcodeJuris, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/LSPCcodeJuris.csv'))   
  
  
  ## Task 9*: Determine connectivity adjustment for each outfall
#use field screening dataset, select flow condition and connectivity and Facility, determine monitoring year, and find average for each outfall and monitoring year

  values <- list()  
  values[['FieldScreen_dry']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/FieldScreen_dry.rds'
  
values[['MajorOutfalls_lc']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/MajorOutfalls_lc.rds'
MajorOutfalls_lc1 <-  readRDS(values[["MajorOutfalls_lc"]]) %>%
  select(FACILITYID)

Connectivity1 <-  readRDS(values[["FieldScreen_dry"]]) %>%
  select(date, FACILITYID, JURISDICTI, FLOWCOND, PERSISTENTFLOW, FLOWCONNECTIVITY) %>%
  #filter(PERSISTENTFLOW =="Yes") %>%
  mutate(Connectivity = ifelse(FLOWCONNECTIVITY==1,"0",ifelse(FLOWCONNECTIVITY==3,"1", ifelse(FLOWCONNECTIVITY==2,"0.5", ifelse(FLOWCONNECTIVITY=='NA',"0.77",ifelse(is.na(FLOWCONNECTIVITY), "0.77", "0.77")))))) %>%   #if direct connection assign 1; if no connection assign 0; otherwise assign 0.77
  as_tibble() %>%
  replace_na(list(Connectivity=0.77)) %>%
  mutate(Date = as.Date(date, format = '%Y-%m-%d')) %>%
  filter(!is.na(Date)) %>%
  filter(Date>"2016-10-01" & Date <"2021-10-01") %>%
  mutate(Connectivity=as.numeric(Connectivity)) %>%
  select(FACILITYID, JURISDICTI, PERSISTENTFLOW, Connectivity, Date) 

str(Connectivity)

#Find average connectivity for each outfall by monitoring year; add a column for monitoring year and summarize average connectivity per MY for each outfall
Connectivity1$MonitoringYear<- ifelse(Connectivity1$Date>(as.Date("2020-09-30", origin="1900-01-01")) & Connectivity1$Date<(as.Date("2021-10-01", origin="1900-01-01")), "MY2020-21",
  ifelse(Connectivity1$Date>(as.Date("2019-09-30", origin="1900-01-01")) & Connectivity1$Date<(as.Date("2020-10-01", origin="1900-01-01")), "MY2019-20",
                              ifelse(Connectivity1$Date>(as.Date("2018-09-30", origin="1900-01-01")) & Connectivity1$Date<(as.Date("2019-10-01", origin="1900-01-01")), "MY2018-19",
                                     ifelse(Connectivity1$Date>(as.Date("2017-09-30", origin="1900-01-01")) & Connectivity1$Date<(as.Date("2018-10-01", origin="1900-01-01")), "MY2017-18",
                                            ifelse(Connectivity1$Date>(as.Date("2016-09-30", origin="1900-01-01")) & Connectivity1$Date<(as.Date("2017-10-01", origin="1900-01-01")), "MY2016-17",
                                                                 NA)))))
Connectivity1 <- Connectivity1 %>%
  group_by(FACILITYID, MonitoringYear) %>%
  mutate(avgCnx=(mean(Connectivity))) %>%
  ungroup() %>%
  group_by(FACILITYID) %>%
  mutate(avgCnxAll=(mean(Connectivity))) 


Connectivityall <- Connectivity1 %>%
  select(FACILITYID, avgCnxAll) %>%
  unique() %>%
  right_join(., MajorOutfalls_lc1)

Connectivity202021<-Connectivity1 %>%
  filter(MonitoringYear=='MY2020-21') %>%
  right_join(.,MajorOutfalls_lc1) %>%
  right_join(.,Connectivityall) %>%
  select(FACILITYID, MonitoringYear, avgCnx, avgCnxAll) %>%
  unique() %>%
  replace_na(list(MonitoringYear='MY2020-21')) %>% #use all connectivity if no connectivity obs for monitoring year
  replace_na(list(avgCnxAll=0.77)) %>%
  mutate(avgCnxAll2=coalesce(avgCnx,avgCnxAll)) %>%
  select(FACILITYID,MonitoringYear,avgCnxAll2) %>%
  unique()
                      
names(Connectivity202021)[names(Connectivity202021)=='avgCnxAll2'] <- 'avgCnx'               

saveRDS(Connectivity202021, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/Connectivity.rds'))
write_csv(Connectivity202021, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/Connectivity.csv'))


## Task 10*: Multiply annual flow volume results (Task 7) with connectivity adjustment (Task 8)
#join datasets
values <- list()  
values[['DischargePointTrib_flow']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DischargePointTrib_flow.rds'
DischargePointTrib_flow <-  readRDS(values[["DischargePointTrib_flow"]]) %>%  
  filter(MonitoringYear=='MY2020-21')  
  #includes all outfalls, including those without delineated tributaries
 
  #select(JURISDICTI3, FACIILTYID, AVGDISCHARGE,  )
  #filter(INSPECTED=='1') %>%
  #filter(Persistent.Flow =='Yes') %>%
  #filter(!is.nan(DischargeJ_cf)) %>%  #no flow measurements for Laguna Hills
  #select(FACILITYID,JURISDICTI.x,  POINT_Y, POINT_X, DRYSAMPLED, MonitoringYear, PERSISTENTFLOW, QAnnualcfCont, QAnnualcf2016, QAnnualcf2018P, QInsAnnualCF, DischargeP, QInsAnnualCFAdj, QavgCF,DischargePCF, DischargeJ_cf, DischargeJ_af)
 
values <- list()  
values[['Connectivity']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/Connectivity.rds'
Connectivity <-  readRDS(values[["Connectivity"]]) %>%  #includes all outfalls, including those without delineated tributaries
filter(MonitoringYear=='MY2020-21')


str(AnnualFlow)
#Calculate total annual flow by jurisdiction and monitoring year - should only be for those with a result in DischargePCF3
AnnualFlow <- left_join(DischargePointTrib_flow, Connectivity, by=c('FACILITYID', 'MonitoringYear' = 'MonitoringYear')) %>%
  unique() %>%
  left_join(., DryDaysYearGroup, by=c('FACILITYID'='FacilityID', 'MonitoringYear', 'dry_days', 'rain_gage')) %>%
   mutate(Qadj_Qall=avgCnx*AnnualCF) %>%
  filter(MonitoringYear=='MY2020-21') %>%
  ungroup()
saveRDS(AnnualFlow, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/AnnualFlow.rds'))  
write_csv(AnnualFlow, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/AnnualFlow.csv'))

AnnualFlow_QC<-AnnualFlow %>%
select(FACILITYID, JURISDICTI, JURISDICTI3, AVGDISCHARGE, PERSISTENTFLOW,AVGDISCHARGE, avgdailyQ, avgdailyQadj, MonitoringYear, AnnualCF, avgCnx, Qadj_Qall) %>%
  unique()

write_csv(AnnualFlow_QC, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/AnnualFlow__QC.csv'))


AnnualFlow_Juriscon<-AnnualFlow %>%
  select(JURISDICTI3, MonitoringYear, Qadj_Qall) %>%
  unique() %>%
  group_by(JURISDICTI3, MonitoringYear) %>%
  mutate(QTot=sum(Qadj_Qall)) %>% 
  select(JURISDICTI3, MonitoringYear,QTot) %>%
  unique() %>%
  mutate(DischargeJ_af = QTot/43560) %>%
  ungroup() %>%
  filter(!is.na(QTot))

saveRDS(AnnualFlow_Juriscon, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/AnnualFlow_Juriscon.rds'))  
write_csv(AnnualFlow_Juriscon, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/AnnualFlow_Juriscon.csv'))


#select(JURISDICTI.x,FACILITYID,FACTYPE, SIZE1, PERSISTENTFLOW, DischargePCF, DischargeJ_cf, DischargeJ_af, Qadj_Qall, QJur, MonitoringYear, DRYSAMPLED)


#Determine Annual flow by jurisdiction for unsampled outfalls (not in Appendix M, and also ponded outfalls in Appendix M)
values <- list()  
values[['Connectivity']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/Connectivity.rds'
Connectivity <-  readRDS(values[["Connectivity"]])
values[['DischargePointTrib_flow']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DischargePointTrib_flow.rds'
DischargePointTrib_flow <-  readRDS(values[["DischargePointTrib_flow"]])
DischargePointTrib_flow$DRYSAMPLED[is.na(DischargePointTrib_flow$DRYSAMPLED)] <- 'No'
#find outfalls with flow measurements but not sampled
AnnualFlow_USa <- full_join(DischargePointTrib_flow, Connectivity, by=c('FACILITYID' = 'FACILITYID', 'MonitoringYear' = 'MonitoringYear')) %>%
  filter(DRYSAMPLED =='No'|is.na(DRYSAMPLED)) %>%
  select(FACILITYID, JURISDICTI, JURISDICTI3, MonitoringYear, AnnualCF,DischargePCF3, avgCnx) %>%
  filter(MonitoringYear=='MY2020-21') %>%
  filter(!is.na(AnnualCF)) %>% #removes ponded outfalls and other outfalls without flow measurements
  mutate(Qadj_Qall=avgCnx*AnnualCF) %>%
  unique() 

  saveRDS(AnnualFlow_USa, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/AnnualFlow_USa.rds'))
write_csv(AnnualFlow_USa, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/AnnualFlow_USa.csv'))  #put in connectivity by hand in csv#save csv as AnnualFlow_USa; use same connectivity as previous years if no data; if no observation, use 0.77


#add in connectivity by hand for NAs

#AnnualFlow_USa1 <-"C:/Users/givens/Documents/GitHub/WQIP-Annual-Report-2020-21/A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/AnnualFlow_USa1.csv"
#AnnualFlow_USa2 <- read.csv(AnnualFlow_USa1) %>% 
  
AnnualFlow_USa <-AnnualFlow_USa %>%
 
  select(FACILITYID, JURISDICTI3, Qadj_Qall) %>%
  unique() 
 
AnnualFlow_USja <-AnnualFlow_USa %>%
  group_by(JURISDICTI3) %>%
  unique() %>%
 # ungroup() %>%
  mutate(QJurUSa=sum(Qadj_Qall)) %>% 
  unique() %>%
  ungroup() 
  
#outfalls sampled but without any flow measurements         
AnnualFlow_USb1 <- left_join(DischargePointTrib_flow, Connectivity, by=c('FACILITYID' = 'FACILITYID', 'MonitoringYear' = 'MonitoringYear')) %>%
  filter(DRYSAMPLED =='Yes' & (DischargeP=='-0.99'& is.na(Q_DailyavgCF))) %>%
  select(FACILITYID, JURISDICTI3, MonitoringYear, AnnualCF, avgCnx) %>%
  filter(MonitoringYear=='MY2020-21') %>%
  mutate(Qadj_Qall=avgCnx*AnnualCF) %>%
  select(FACILITYID, JURISDICTI3, Qadj_Qall) %>%
  unique() 

AnnualFlow_USjb <-AnnualFlow_USb1 %>%
  group_by(JURISDICTI3) %>%
  unique() %>%
  # ungroup() %>%
  mutate(QJurUSb=sum(Qadj_Qall)) %>% 
  unique() %>%
  ungroup() 

AnnualFlow_US <- bind_rows(AnnualFlow_USja, AnnualFlow_USjb) %>%  #flow from unsampled outfalls, both measured flow and estimates at ponded outfalls
mutate(QJur_US=coalesce(QJurUSb, QJurUSa)) 
  
AnnualFlow_USj <- AnnualFlow_US %>%
  select(JURISDICTI3, QJur_US) %>%
  unique() %>%
  group_by(JURISDICTI3) %>%
mutate(QJur_US=sum(QJur_US)) %>%
  unique() 

AnnualFlow_US <-AnnualFlow_US %>%
  select(-QJur_US) %>%
  left_join(.,AnnualFlow_USj)

saveRDS(AnnualFlow_US, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/AnnualFlow_US.rds'))
write_csv(AnnualFlow_US, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/AnnualFlow_US.csv'))

 
#by jurisdiction
values <- list()  
values[['AnnualFlow_US']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/AnnualFlow_US.rds'

AnnualFlow_US_j <- AnnualFlow_US %>%
  select(JURISDICTI3, QJur_US) %>%
  unique() %>%
  ungroup() 

saveRDS(AnnualFlow_US_j, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/AnnualFlow_US_j.rds'))
write_csv(AnnualFlow_US_j, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/AnnualFlow_US_j.csv'))

#select(MonitoringYear.x, JURISDICTI3.x,FACILITYID, AVGDISCHARGE.x, Dischargecfs.x, QDailycfIns.x, avgdailyQ.x,avgdailyQadj.x,  DischargePCF, DischargeJ_cf, DischargeJ_af, Qadj_Qall, QJur_US, MonitoringYear, DRYSAMPLED, dry_days.x, AnnualCF.x)

#Dataset with flow from sampled outfalls with flow
AnnualFlow_s <- left_join(DischargePointTrib_flow, Connectivity, by=c('FACILITYID' = 'FACILITYID', 'MonitoringYear' = 'MonitoringYear')) %>%
  unique() %>%
  filter(MonitoringYear=='MY2020-21') %>%
  group_by(JURISDICTI3) %>%
  filter(DRYSAMPLED =='Yes'& !is.na(Q_DailyavgCF)) %>%
  mutate(Qadj_Qall=avgCnx*AnnualCF) %>%
  select(FACILITYID, JURISDICTI3, Qadj_Qall) %>%
  group_by(JURISDICTI3) %>%
  unique() %>%
  mutate(QJur_S=sum(Qadj_Qall)) %>%
  ungroup() %>%
  unique()
  
saveRDS(AnnualFlow_s, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/AnnualFlow_s.rds'))
write_csv(AnnualFlow_s, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/AnnualFlow_s.csv'))

#combine sampled and unsampled for all outfalls and total jurisdictional flow volume
values <- list()  
values[['AnnualFlow_s']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/AnnualFlow_s.rds'
AnnualFlow_s <-  readRDS(values[["AnnualFlow_s"]]) %>%
  #select(c(FACILITYID, JURISDICTI, JURISDICTI3, MonitoringYear, AnnualCF, avgCnx, Qadj_Qall, QJur_S))

values[['AnnualFlow_US_j']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/AnnualFlow_US_j.rds'
AnnualFlow_US_j <-  readRDS(values[["AnnualFlow_US_j"]]) 

values[['AnnualFlow_US']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/AnnualFlow_US.rds'
AnnualFlow_US <-  readRDS(values[["AnnualFlow_US"]]) #%>%
  #select(c(FACILITYID, JURISDICTI, JURISDICTI3, MonitoringYear, AnnualCF, avgCnx, Qadj_Qall, QJur_US))

AnnualFlow_all <- bind_rows(AnnualFlow_s, AnnualFlow_US)
saveRDS(AnnualFlow_all, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/AnnualFlow_all.rds'))
write_csv(AnnualFlow_all, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/AnnualFlow_all.csv'))


#by jurisdiction
AnnualFlow_S_j <-  readRDS(values[["AnnualFlow_s"]]) %>%
  select(c(JURISDICTI3, QJur_S)) %>%
  unique()

AnnualFlow_J2<-full_join(AnnualFlow_S_j, AnnualFlow_US_j, by=c('JURISDICTI3')) %>%
  mutate(DischargeJ_cf = (QJur_S + QJur_US)) %>%
  mutate(DischargeJ_af = DischargeJ_cf/43560) %>%   #convert to acre-ft; 1 acre-feet = 43560 cf
unique()

saveRDS(AnnualFlow_J2, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/AnnualFlow_J2.rds'))
write_csv(AnnualFlow_J2, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/AnnualFlow_J2.csv'))

#for print output (Jurisdictions, cnx)

values <- list()  
values[['Connectivity']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/Connectivity.rds'
Connectivity <-  readRDS(values[["Connectivity"]])
values[['MajorOutfalls_lc']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/MajorOutfalls_lc.rds'
MajorOutfalls_lc1 <-  readRDS(values[["MajorOutfalls_lc"]]) %>%
  select(FACILITYID, JURISDICTI)

values[['AnnualFlow_all']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/AnnualFlow_all.rds'
AnnualFlow_all <-  readRDS(values[["AnnualFlow_all"]]) 

values[['DischargePointTrib_flow']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DischargePointTrib_flow.rds'
DischargePointTrib_flow_p <-  readRDS(values[["DischargePointTrib_flow"]]) %>%
  select(FACILITYID, JURISDICTI, JURISDICTI3, MonitoringYear, AnnualCF) %>%
  unique()

AnnualFlow_all_print <- AnnualFlow_all %>%
  full_join(., Connectivity, by=c('FACILITYID')) %>%
  full_join(., MajorOutfalls_lc1) %>%
  full_join(., DischargePointTrib_flow_p, by=c('FACILITYID', 'JURISDICTI', 'JURISDICTI3'))  %>%
  select(JURISDICTI, FACILITYID, JURISDICTI3, AnnualCF, avgCnx, Qadj_Qall, QJur_S, QJur_US) %>%
  filter(!is.na(JURISDICTI)) %>%
  filter(!is.na(Qadj_Qall))

write_csv(AnnualFlow_all_print, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/AnnualFlow_all_print.csv'))


## Task 11*: Import chemistry data


values <- list()  
values[['JURISinTRIBS2']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/JURISinTRIBS2.rds'
JURISinTRIBS2 <-readRDS(values[["JURISinTRIBS2"]]) 


#2020-21
Chemdata2021R1and2 <- "A.2 Outfall Assessments/DryWeatherLoadingCalcs/Input/WQ/DryWeatherDataNALsAssessment2021.xlsx"

library(readxl)
library(dplyr)
library(lubridate)
library(tidyr)
library(here)
library(readr)

getwd()

Chemdata2021R1and2 <- read_excel(Chemdata2021R1and2, skip = 1, 'Horizon Data units') 
#format date
Chemdata2021R1and2 <- Chemdata2021R1and2 %>%
  separate('Collect Date', into=c("date", "time"), sep = " ") %>%
  mutate(date=ymd(date), time=hms(time)) %>%
  select(-Station, -'Sample ID', -time, -pH, -Temperature, -'Field Dissolved Oxygen', -'Field pH', -'Field Specific Conductivity', -'Field Temperature', -'Field Turbidity - NTU')


names(Chemdata2021R1and2)[names(Chemdata2021R1and2)=='Column1'] <- 'Station'


#use 1/2 the detection limit
OutfallChem2021 <- Chemdata2021R1and2 %>%
  filter(!is.na(Station)) %>%
  filter('Total Coliforms - CFU/100 mL'!= "NA") %>%
  #filter(`Sample Type` == 'Total') %>%
  gather(Parameter, Value, `d10-Acenaphthene - ng/L`:`Total Coliforms - CFU/100 mL`) %>% 
  separate(Parameter, c('Parameter', 'Units'), sep = ' - ') %>%
  separate(Value, c('Qualifier', 'Result'), "(?<=[<|>|>=]) ?(?=[0-9])") %>%
  mutate(
    Result = as.numeric(ifelse(is.na(Result), Qualifier, Result)),
    Qualifier = ifelse(grepl('>|<=|>=|<', Qualifier), Qualifier, NA),
    Result = ifelse(!(Parameter %in% c('Fecal coliforms', 'Total Coliforms', 'Enterococcus')),
                    ifelse(grepl('<', Qualifier),
                           Result / 2,
                           Result),
                    Result)
  ) %>%
  filter(!is.na(Result))
  

saveRDS(OutfallChem2021, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/OutfallChem2021.rds'))
write_csv(OutfallChem2021, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/OutfallChem2021.csv'))

values <- list()  
values[['OutfallChem2021']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/OutfallChem2021.rds'
OutfallChem2021 <-readRDS(values[["OutfallChem2021"]]) 


OutfallChem2021_avg <- OutfallChem2021 %>%  
  group_by(Station, `Sample Type`, Parameter) %>%
  #filter(!is.na(Result)) %>%
  mutate(ResultAvg=mean(Result, na.rm=TRUE)) %>%  #find average for each jurisdiction for each monitoring year
  ungroup() %>%
  unique() 
  

OutfallChem2021_samp <- OutfallChem2021_avg %>%
  select(Station, `Sample Type`, Parameter, Qualifier, ResultAvg, Units) %>%
  ungroup() %>%
  unique() 

saveRDS(OutfallChem2021_avg, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/OutfallChem2021_avg.rds'))
write_csv(OutfallChem2021_avg, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/OutfallChem2021_avg.csv'))

saveRDS(OutfallChem2021_samp, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/OutfallChem2021_samp.rds'))
write_csv(OutfallChem2021_samp, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/OutfallChem2021_samp.csv'))


#Find average conc by jurisdiction (use in loading calcs for unsampled outfalls)

#join jurisdiciton

values <- list()  
values[['JURISinTRIBS2']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/JURISinTRIBS2.rds'
JURISinTRIBS2 <-readRDS(values[["JURISinTRIBS2"]]) 

values[['OutfallChem2021_samp']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/OutfallChem2021_samp.rds'
OutfallChem2021_j <-  readRDS(values[["OutfallChem2021_samp"]]) %>%
  
  full_join(., JURISinTRIBS2, by=c('Station' = 'FACILITYID')) %>%
  select(Station,`Sample Type` , Parameter, Units, Qualifier, ResultAvg, JURISDICTI, JURISDICTI3) %>%
  group_by(JURISDICTI3, Parameter) %>%
  mutate(ResultJuris=mean(ResultAvg)) %>%
  ungroup() %>%
  summarise(JURISDICTI3, JURISDICTI,`Sample Type` , Parameter, ResultJuris, Units) %>%
  select(-JURISDICTI) %>%
  unique() %>%
  filter(!is.na(JURISDICTI3))

saveRDS(OutfallChem2021_j, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/OutfallChem2021_j.rds'))
write_csv(OutfallChem2021_j, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/OutfallChem2021_j.csv'))

## Task 12*: Multiply adjusted annual flow volume results (Task 9) with chemistry data (Task 10)
library(dplyr)
library(here)
library(readr)
library(tidyverse)
library(tidyr)
library(data.table)

values <- list()  

values[['OutfallChem2021_samp']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/OutfallChem2021_samp.rds'
OutfallChem2021_samp <-  readRDS(values[["OutfallChem2021_samp"]])
values[['OutfallChem2021_j']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/OutfallChem2021_j.rds'
OutfallChem2021_j <-  readRDS(values[["OutfallChem2021_j"]])


values[['AnnualFlow_US']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/AnnualFlow_US.rds'
AnnualFlow_US <-  readRDS(values[["AnnualFlow_US"]]) 

values[['AnnualFlow_s']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/AnnualFlow_s.rds'
AnnualFlow_s <-  readRDS(values[["AnnualFlow_s"]]) 
 
  # select(FACILITYID, JURISDICTI3, ResultAvg, Qadj_Qall)
  
values[['AnnualFlow_US_j']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/AnnualFlow_US_j.rds'
AnnualFlow_US_j <-  readRDS(values[["AnnualFlow_US_j"]])


#sum by jurisdiction

#2021 sampled outfalls
Loads2021_samp <-left_join(OutfallChem2021_samp, AnnualFlow_s, by=c('Station'='FACILITYID')) %>%  #Unit Converion:  453592 mg in a pound, 1 cubic foot is 62.43 pounds, 28.3168 Liters in one cubic-ft
  filter(!is.na(ResultAvg)) %>% 
  #filter(MonitoringYear=='MY2020-21') %>%
  mutate(LoadQadj_Qall=ResultAvg*Qadj_Qall) %>%
  mutate(LoadsPoundsQS = ifelse(Parameter %in% c('Fecal coliforms', 'Total Coliforms', 'Enterococcus', 'E. coli'),
                                (LoadQadj_Qall)  * (28.3168) * 10,
                                ifelse(Units == 'mg/L',
                                       (LoadQadj_Qall) * (1/453592)  * (28.3168),
                                       ifelse(Units == 'ug/L',
                                              (LoadQadj_Qall) * (1/10^3) * (1/453592)  * (28.3168),
                                              ifelse(Units == 'ng/L',
                                                     (LoadQadj_Qall) * (1/10^6) * (1/453592) * (28.3168),
                                                     NA)
                                       )
                                )))

saveRDS(Loads2021_samp, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/Loads2021_samp.rds'))
write_csv(Loads2021_samp, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/Loads2021_samp.csv'))

#sum by jurisdiction
values <- list()  
values[['Loads2021_samp']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/Loads2021_samp.rds'
Loads2021_j <-  readRDS(values[["Loads2021_samp"]]) %>%
  filter(!is.na(JURISDICTI3)) %>%
  filter(Parameter!="SpecificConductivity") %>%
  filter(Parameter!="Turbidity") %>%
  group_by(JURISDICTI3, `Sample Type` ,Parameter) %>%
  mutate(LoadsJurs=sum(LoadsPoundsQS, na.rm=TRUE)) %>%
  ungroup()

saveRDS(Loads2021_j, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/Loads2021_j.rds'))
write_csv(Loads2021_j, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/Loads2021_j.csv'))

#2021 Unsampled Outfalls
values <- list()  

values[['OutfallChem2021_j']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/OutfallChem2021_j.rds'
OutfallChem2021_j <-  readRDS(values[["OutfallChem2021_j"]]) 


values[['AnnualFlow_US']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/AnnualFlow_US.rds'
AnnualFlow_US <-  readRDS(values[["AnnualFlow_US"]]) 
  
Loads2021Uns<-left_join(AnnualFlow_US, OutfallChem2021_j,  by=c('JURISDICTI3'='JURISDICTI3')) %>%  #Unit Converion:  453592 mg in a pound, 1 cubic foot is 62.43 pounds, 28.3168 Liters in one cubic-ft
  mutate(LoadQUns=ResultJuris*QJur_US) %>% #flow with connectivity adjustment
  mutate(LoadsPoundsQUns = ifelse(Parameter %in% c('Fecal coliforms', 'Total Coliforms', 'Enterococcus', 'E. coli'),
                                  (LoadQUns)  * (28.3168) * 10,
                                  ifelse(Units == 'mg/L',
                                         (LoadQUns) * (1/453592)  * (28.3168),
                                         ifelse(Units == 'ug/L',
                                                (LoadQUns) * (1/10^3) * (1/453592)  * (28.3168),
                                                ifelse(Units == 'ng/L',
                                                       (LoadQUns) * (1/10^6) * (1/453592) * (28.3168),
                                                       NA)
                                         )
                                  )))
saveRDS(Loads2021Uns, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/Loads2021Uns.rds'))
write_csv(Loads2021Uns, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/Loads2021Uns.csv'))

Loads2021Uns_j <-  Loads2021Uns %>%
  filter(!is.na(JURISDICTI3)) %>%
  filter(Parameter!="SpecificConductivity") %>%
  filter(Parameter!="Turbidity") %>%
  filter(Parameter!="Chloride") %>%
  unique() %>%
  select(JURISDICTI3,`Sample Type` , Parameter, Units, LoadsPoundsQUns) %>%
  unique() %>%
  group_by(JURISDICTI3,`Sample Type` , Parameter) %>%
  mutate(LoadQUns_j=sum(LoadsPoundsQUns)) %>%

  ungroup()

saveRDS(Loads2021Uns_j, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/Loads2021Uns_j.rds'))
write_csv(Loads2021Uns_j, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/Loads2021Uns_j.csv'))

#Combine loads from sampled and unsampled outfalls

#2021
values <- list() 
values[['Loads2021_j']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/Loads2021_j.rds'
Loads2021_j <-  readRDS(values[["Loads2021_j"]]) %>%
  select(JURISDICTI3,`Sample Type`, Parameter, Units,  QJur_S, LoadsJurs) %>%
  group_by(JURISDICTI3) %>%
  distinct() %>%
  filter(!is.na(JURISDICTI3))

values[['Loads2021Uns_j']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/Loads2021Uns_j.rds'
Loads2021Uns_j <-  readRDS(values[["Loads2021Uns_j"]]) %>%
  select(c('JURISDICTI3',`Sample Type` , 'Parameter', 'Units',  'LoadsPoundsQUns', 'LoadQUns_j')) %>%
  filter(Parameter!='SpecificConductivity'|Parameter!='Turbidity') %>%
  filter(!is.na(LoadQUns_j))

JurisLoads2021<-full_join(Loads2021_j, Loads2021Uns_j, by=c('JURISDICTI3','Sample Type', 'Parameter', 'Units')) 
JurisLoads2021$LoadsPoundsQUns[is.na(JurisLoads2021$LoadsPoundsQUns)] <- 0 #necessary to ensure Laguna Woods is included

JurisLoads2021 <- JurisLoads2021 %>%  
  group_by(JURISDICTI3, Parameter,`Sample Type`) %>%
  mutate(TotalLoadPounds=LoadsPoundsQUns+LoadsJurs) %>%
  filter(Parameter!="SpecificConductivity"|Parameter!="Turbidity") %>%
  ungroup() %>%
  unique()
  
saveRDS(JurisLoads2021, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/JurisLoads2021.rds'))
write_csv(JurisLoads2021, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/JurisLoads2021.csv'))

#NALS Parameters
values <- list() 
values[['JurisLoads2021']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/JurisLoads2021.rds'
JurisLoads2021 <-  readRDS(values[["JurisLoads2021"]])


#2021
JurisLoads2021NALs <- JurisLoads2021 %>%
  filter(Parameter=='Fecal coliforms'|Parameter=='Enterococcus'|Parameter=='Nitrate + Nitrite as N'|Parameter=='Nitrogen, Total Kjeldahl'|Parameter=='Phosphorus as PO4'|Parameter=='TSS'|Parameter=='MBAS'|Parameter=='Iron, Total'|Parameter=='Manganese, Total'|Parameter=='Cadmium, Total'|Parameter=='Chromium, Total'|Parameter=='Copper, Total'|Parameter=='Lead, Total'|Parameter=='Nickel, Total'|Parameter=='Silver, Total'|Parameter=='Zinc, Total') %>%
  select(c('JURISDICTI3', `Sample Type` ,'Parameter', 'Units','LoadsJurs' , 'LoadQUns_j','TotalLoadPounds')) %>%
  unique() 

saveRDS(JurisLoads2021NALs, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/JurisLoads2021NALsg.rds'))
write_csv(JurisLoads2021NALs, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/JurisLoads2021NALsg.csv'))



#### Task 1: Import rain data ####
# 1. download QC'd data from Hydstra as daily totals from 00:00 to 00:00 from 10/01/2015 to present for all cooperative gages (Laguna Beach, Sulphur Creek Dam, Palisades Reservoir, El Toro, Tucker Wildlife, Trabuco Forestry, Dana Point) listed in the table of closest rain gages
# 2. create folder to store all individual csvs for each gage
# 3. very important: label the files CONSISTENTLY with the gage names that match the table of closest rain gages
## end results: a folder with all rain gage flow data since MY2015-16, labeled consistently 


#### Task 2: Determine how many dry weather days at each gage ####
# 1. use a loop to load in gage data one by one from Task 1 end result
# 2. use "Dry Days Calculation" from method document: for each gage, calculate for each row/day if it is wet or dry
# 3. for each gage, separate into monitoring years, and sum dry days by month and by year
## end result: one table per monitoring year since MY2015-16, each with 14 columns: (1) gage name (2) # of dry days in the whole year and (3-14) # of dry days in each month
#**********************Question: what do we do when there's missing data?
#use end result from DryDayCount_Monthly.R (in Files)

#### Task 3: Import flow data ####
# 1. for continuous flow data, check where most QC'd data is: Hydstra or FSData
# 2. create folder to store all individual csvs for each continuous flow data collection period for each outfall
# 3. very important: label the files CONSISTENTLY with the outfall names and with start date of the collection period (for example "J01-9131-1_20180403")
# 4. for instantaneous flow data, import field screening observations for all monitoring years (past monitoring years are needed for Task 6 below)
# 5. filter field screening observations to only have 6 columns: (1) outfall name (2) observation date (3) Flow velocity (4) Flow Width (5) Flow Depth (6) Connectivity to RW
## end results: (1) a folder with all continuous flow data, labeled consistently and (2) the 6 column table from step 5 above.

#### Task 4.1: Filter out wet weather from continuous flow data ####
# 1. create a loop that will edit the files in the Task 3 end result cont data folder to append a "wet/dry" column
# 2A. in the loop: import one cont flow data file at a time
# 2B. in the loop: use the table of closest rain gages to determine which gage to reference
# 2C. in the loop: use daily rain data mutate another column labeled "wet/dry" (or replace data in) with wet/dry determination using the "Dry Days Calculation" from method document
# 2D. in the loop: replace the file with this new "wet/dry" column into the cont data folder
## end result: folder with all continuous flow data, labeled consistently and with a "wet/dry" determination
#***********when to filter this out?

#### Task 4.2: Filter out wet weather from instantaneuous flow data ####
# 1. Append a column with a "wet/dry" determination onto Task 3 end result's 6 column table 
# 2. fill that new column using Task 2's end result rain gage information, based on the outfall (use the table of closest rain gages to determine which gage)
# 3. filter out wet weather and delete the "wet/dry" column
## end result: a 6 column table from Task 3 end result with only the dry weather inst flow

#### Task 5: Import outfall observations and determine what method to use for each outfall (only cont, only inst, both, none) ####
# 1. import list of major MS4 outfalls with persistent flows this monitoring year
# 2. create list of outfalls that have continuous data by pulling file names from the folder with all continuous flow data and deleting last 9 characters on each file name
# 3. create list of outfalls that have instantaneous data (filter for non-blanks in all 3 of those field screening columns - flow velocity, width, & depth)
# 4. on list from step 1, cross reference lists from steps 2 and 3 to determine if there is only cont, only inst, both, or none
## end result: table with two columns: (1) major MS4 outfalls with persistent flows this monitoring year and (2) what method to use (only cont, only inst, both, or none)

#### Task 6: Determine site-specific instantaneous:continuous flow ratio ####
# 1. 

#### Task 7.1: Only continuous data ####
# ~~~~use "Volumes from Continuous Flow Monitoring Equation" from method document~~~~
# 1. Filter Task 5 end result table to create a list of "only cont" outfalls
# 2. create a loop that looks for one outfall at a time from the "only cont" list
# 3. in the loop, temporarily import appropriate data: make it search through the Task 4.1 end result continuous flow data folder for names that contain the outfall name and import the flow data
# 4. in the loop, write logic that says if there is more than one file for the outfall, use the file name whose last 8 digits are the largest number (AKA the most recent file)
# 5A. in the loop, use the method equation to calculate an annual volume for that outfall
# 5B. use the table of closest rain gages to determine which gage to reference
# 5C. use the file name last 8 digits to determine the monitoring year for dry day determination (for example if last 8 digits are between 20190801 and 20200930, use the MY1920 dry day table from Task 2 end result)
# 6. in the loop, have that annual volume fill a list with the outfall name and the annual flow volume (this is your end result from this Task 7.1)
# 7. at the end of the loop, delete from the R environment the temporary imported cont flow data files
## end result: table with two columns: (1) major MS4 outfalls with persistent flows this monitoring year that used this "only cont" method and (2) annual volume

#### Task 7.2: Only instantaneous data ####
# ~~~~use "Volumes from Instantaneous Flow Observations Equations" from method document~~~~
# 1. Filter Task 5 end result table to create a list of "only inst" outfalls
# 2. Split that list into two lists: outfalls with 1 field screening, and outfalls with more than 1
# 3. For outfalls with just 1 field screening with inst data, use "Volumes from Instantaneous Flow Observations Equations > Outfall = 1 field screening" from method document 
# 4. For outfalls with more than 1 field screening with inst data, use "Outfall >1 field screening" from method document 
#    Note for steps 3 and 4: Task 2 end product gives you annual and monthly dry days; decide which gage to use with the nearest gage table for each outfall
## end result: two tables (for 1 obs and for >1 obs) with two columns each: (1) major MS4 outfalls with persistent flows this monitoring year that used this "only inst" method and (2) annual volume


#### Task 7.3: Both continuous and instantaneous data ####
# 1. use similar loop from Task 7.1 to only import cont data for one outfall at a time
# 2. in the loop, create a second loop:
# 3. in the second loop, loop through each month of the monitoring year and calculate an average daily flow from the continuous data
# 1. Create table with 4 columns:
#   (1) every month in the monitoring year
#   (2) average daily flow volume for each month (left blank if there's no data), 
#       *Use if/then logic to fill this out (for example if outfall is )
#   (3) #Dry Days in that month, and 
#   (4) estimated monthly flow volume (multiply columns 2 and 3, months without data will be blank)

#### Task 7.4: No flow data ####

#### Task 8: combining annual flow volume results from four methods ####

#### Task 9: determine connectivity adjustment for each outfall ####

#### Task 10: multiply annual flow volume results (Task 6) with connectivity adjustment (Task 7) ####
#### Task 11: Import chemistry data ####
#### Task 12: Multiply adjusted annual flow volume results (Task 9) with chemistry data (Task 10) ####
