source('A.2 Outfall Assessments/DryWeatherLoadingCalcs/1projectsetup.R')


## Task 3: Import flow data
#S:\Environmental Res\Environmental Studies Unit\Projects\WQIP\data\Flow Data Analysis
#S:\Environmental Res\Environmental Studies Unit\Projects\WQIP\data\Hach flow data
#https://app.box.com/s/in2b5wsc0mugf2ifboibf4l7r68voree
#


#Continuous flow data from 2018-20
#For calcs presented in 2019-20 Annual data report, three folders (2018 prioritization, 2019-20 OCFS, and 2020 Coto de Caza)

#use for loop to iterate analysis over a list of dataframes, then combine list of data frames
#https://datacarpentry.org/semester-biology/materials/for-loops-R/
# https://stackoverflow.com/questions/33203800/unzip-a-zip-file

#TM flow data

path<-here("A.2 Outfall Assessments/DryWeatherLoadingCalcs/Input/ProvisionalFlow/Detailed_Flow_Monitoring_Data_Summer_Dry_Clean")
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

DailyQ_TM <- df %>%
  filter(!is.na(Flow_cfs)) 

i <- sapply(DailyQ_TM, is.factor)  #https://stackoverflow.com/questions/2851015/convert-data-frame-columns-from-factors-to-characters
DailyQ_TM[i] <- lapply(DailyQ_TM[i], as.character)

str(DailyQ_TM)

#DailyQ_2018p$Inst.Time = substr(DailyQ_2018p$Inst.Time, 1, nchar(DailyQ_2018p$`Inst.Time`)-4) 

DailyQ_TM <- DailyQ_TM %>%
  mutate(Station = SourceFile) %>%
  mutate(date = as.Date(Datetime, format = '%Y-%m-%d')) %>%
  select('Station', 'date', 'Flow_cfs') %>% #find mean daily discharge
  group_by(Station, date) %>% 
  mutate(Flow_cfs_md=mean(Flow_cfs))

DailyQ_TM$Station = sub("./", "", DailyQ_TM$Station)
DailyQ_TM$Station = sub("_Dry_Summer_Clean.csv", "", DailyQ_TM$Station)
DailyQ_TM$Station = sub("_Dry_Clean.csv", "", DailyQ_TM$Station)
DailyQ_TM$Station = sub("_all", "", DailyQ_TM$Station)

DailyQ_TM$Station<-sub("K01-12032-2", "K01-12032-2 (K01P11)",DailyQ_TM$Station)
DailyQ_TM$Station<-sub("L01-242-1", "L01-242-1 (L07P16)",DailyQ_TM$Station)
DailyQ_TM$Station<-sub("L01-724-1", "L01-724-1 (L01S01)",DailyQ_TM$Station)
DailyQ_TM$Station<-sub("L01-727-1", "L01-727-1 (L01S04)",DailyQ_TM$Station)
DailyQ_TM$Station<-sub("L01-728-7", "L01-728-7 (L01S03)",DailyQ_TM$Station)
DailyQ_TM$Station<-sub("L02-166-3", "L02-166-3 (L02P26)",DailyQ_TM$Station)
DailyQ_TM$Station<-sub("L02-246-1", "L02-246-1 (L11P01)",DailyQ_TM$Station)
DailyQ_TM$Station<-sub("L02-374-1", "L02-374-1 (L02P50)",DailyQ_TM$Station)
DailyQ_TM$Station<-sub("L02-622-2", "L02-622-2 (L02P32)",DailyQ_TM$Station)
DailyQ_TM$Station<-sub("L02-640-1", "L02-640-1 (L11P02)",DailyQ_TM$Station)
DailyQ_TM$Station<-sub("L03-662-3", "L03-662-3 (L03P16)",DailyQ_TM$Station)
DailyQ_TM$Station<-sub("L03-691-1", "L03-691-1 (L03P09)",DailyQ_TM$Station)
DailyQ_TM$Station<-sub("L03-708-11", "L03-708-11 (L03P05)",DailyQ_TM$Station)
DailyQ_TM$Station<-sub("L04-136-1u", "L04-136-1u (L04P07)",DailyQ_TM$Station)
DailyQ_TM$Station<-sub("M00.1-071-3", "M00.1-071-3 (M00S04)",DailyQ_TM$Station)
DailyQ_TM$Station<-sub("M01-042-1", "M01-042-1 (M01S01)",DailyQ_TM$Station)
DailyQ_TM$Station<-sub("M02-085-1", "M02-085-1 (M02P06)",DailyQ_TM$Station)

DailyQ_TM <- DailyQ_TM %>%
  select('Station', 'date', 'Flow_cfs_md') %>%
  unique()

saveRDS(DailyQ_TM, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DailyQ_TM.rds')) 
write_csv(DailyQ_TM, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DailyQ_TM.csv'))

##2018 Reprioritization

getwd()

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
  mutate(date = as.Date(Inst.Time, format = '%m/%d/%Y')) %>%
  group_by(Station, date) %>% #find mean daily discharge
  mutate(Flow_cfs_md=mean(Flow..cfs.)) %>%
  select('Station', 'date', 'Flow_cfs_md') %>%
  unique()

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
  #filter(DailyQ_201920Cont > 4) %>% #filter away days from when flow meter not working correctly for J01P27 and J01P28
  filter(Station != 'J01-9992-1 (J01P27)'|date != '2018-04-06') %>%
  filter(Station != 'J01-9992-1 (J01P27)'|date != '2018-04-08') 


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
  mutate(date = as.Date(Inst.Time, format = '%m/%d/%Y')) %>%
  group_by(Station, date) %>% #find mean daily discharge
  mutate(Flow_cfs_md=mean(Flow..cfs.)) %>%
  select('Station', 'date', 'Flow_cfs_md') %>%
  unique()

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
DailyQ_2020OFP$Station<-sub("K01-12177-1", "K01-12177-1 (K01P07)",DailyQ_2020OFP$Station)


DailyQ_2020OFP <- DailyQ_2020OFP %>%
  #select(-SourceFile) %>%
  #filter(DailyQ_2020OFPCont > 4) %>% #filter away days from when flow meter not working correctly for J01P27 and J01P28
  filter(Station != 'J01-9992-1 (J01P27)'|date != '2018-04-06') %>%
  filter(Station != 'J01-9992-1 (J01P27)'|date != '2018-04-08') 

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

DailyQ_OCFS_2021$Inst.Time = substr(DailyQ_OCFS_2021$Inst.Time, 1, nchar(DailyQ_OCFS_2021$`Inst.Time`)-5) 

DailyQ_OCFS_2021 <- DailyQ_OCFS_2021 %>%
  mutate(Station = SourceFile) %>%
  mutate(date = as.Date(Inst.Time, format = '%m/%d/%Y'))  %>%
  group_by(Station, date) %>% #find mean daily discharge
  mutate(Flow_cfs_md=mean(Flow..cfs.)) %>%
  select('Station', 'date', 'Flow_cfs_md') %>%
  unique()
  

DailyQ_OCFS_2021$Station = sub("./", "", DailyQ_OCFS_2021$Station)
DailyQ_OCFS_2021$Station = sub(".csv", "", DailyQ_OCFS_2021$Station)



DailyQ_OCFS_2021[!duplicated(DailyQ_OCFS_2021[c(1,2)]), ]   

str(DailyQ_OCFS_2021)
saveRDS(DailyQ_OCFS_2021, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DailyQ_OCFS_2021.rds')) 
write_csv(DailyQ_OCFS_2021, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DailyQ_OCFS_2021.csv')) 

#SWN (mean daily dishcarge from Hydstra)
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
  mutate(date = as.Date(Inst.Time, format = '%m/%d/%Y')) %>%
  select('Station', 'date', 'Flow..cfs.')

DailyQ_SWN_2022$Station = sub("./", "", DailyQ_SWN_2022$Station)
DailyQ_SWN_2022$Station = sub(".csv", "", DailyQ_SWN_2022$Station)


DailyQ_SWN_2022[!duplicated(DailyQ_SWN_2022[c(1,2)]), ]   

DailyQ_SWN_2022 <- rename(DailyQ_SWN_2022, Flow_cfs_md = Flow..cfs.)

str(DailyQ_SWN_2022)
saveRDS(DailyQ_SWN_2022, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DailyQ_SWN_2022.rds')) 
write_csv(DailyQ_SWN_2022, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DailyQ_SWN_2022.csv')) 


#DFM 2022
pathDFM_2022<-here("A.2 Outfall Assessments/DryWeatherLoadingCalcs/Input/ProvisionalFlow/DFM_2022_provisional")
pathDFM_2022

data_filespathDFM_2022 = list.files(path=pathDFM_2022, pattern = "*.csv")
data_filespathDFM_2022

df <- data_filespathDFM_2022 %>%
  map(function(x) {
    read.csv(paste0(pathDFM_2022, "/", x)) %>%
      mutate(SourceFile = x)
  }
  ) %>%
  reduce(rbind)

df

DailyQ_DFM_2022 <- df %>%
  filter(!is.na(Flow..cfs.)) 

i <- sapply(DailyQ_DFM_2022, is.factor)  #https://stackoverflow.com/questions/2851015/convert-data-frame-columns-from-factors-to-characters
DailyQ_DFM_2022[i] <- lapply(DailyQ_DFM_2022[i], as.character)

str(DailyQ_DFM_2022)

DailyQ_DFM_2022$Inst.Time = substr(DailyQ_DFM_2022$Inst.Time, 1, nchar(DailyQ_DFM_2022$`Inst.Time`)-4) 

DailyQ_DFM_2022 <- DailyQ_DFM_2022 %>%
  mutate(Station = SourceFile) %>%
  mutate(date = as.Date(Inst.Time, format = '%m/%d/%Y')) %>%
select('Station', 'date', 'Flow..cfs.')

DailyQ_DFM_2022$Station = sub("./", "", DailyQ_DFM_2022$Station)
DailyQ_DFM_2022$Station = sub(".csv", "", DailyQ_DFM_2022$Station)

DailyQ_DFM_2022[!duplicated(DailyQ_DFM_2022[c(1,2)]), ] 

DailyQ_DFM_2022 <- rename(DailyQ_DFM_2022, Flow_cfs_md = Flow..cfs.)

DailyQ_DFM_2022$Station<-sub("K01-12058-1", "K01-12058-1 (K01P08)",DailyQ_DFM_2022$Station)
DailyQ_DFM_2022$Station<-sub("K01-12058-2", "K01-12058-1 (K01P09)",DailyQ_DFM_2022$Station)

str(DailyQ_DFM_2022)
saveRDS(DailyQ_DFM_2022, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DailyQ_DFM_2022.rds')) 
write_csv(DailyQ_DFM_2022, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DailyQ_DFM_2022.csv')) 


##* Task 4.1: Filter out wet weather from continuous flow data
#join to station and rain gauge#

values <- list()  
values[['RainInf']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/RainInf.rds'

RainInf<-  readRDS(values[["RainInf"]]) %>%
  select('Date', 'FACILITYID', 'wet_within_72')

str(RainInf)

values <- list()  
values[['DailyQ_TM']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DailyQ_TM.rds'
DailyQ_TM <-  readRDS(values[["DailyQ_TM"]])     

DailyQ_TM_d <-   left_join(DailyQ_TM, RainInf, by = c('date'='Date', 'Station'='FACILITYID')) %>%   #dry days only, each flow measurment
  filter(wet_within_72==FALSE)

saveRDS(DailyQ_TM_d, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DailyQ_TM_d.rds')) 
write_csv(DailyQ_TM_d, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DailyQ_TM_d.csv'))  


values <- list()  
values[['DailyQ_2018R']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DailyQ_2018R.rds'
DailyQ_2018R <-  readRDS(values[["DailyQ_2018R"]])     

DailyQ_2018R_d <-   left_join(DailyQ_2018R, RainInf, by = c('date'='Date', 'Station'='FACILITYID')) %>%   #dry days only, each flow measurment
  filter(wet_within_72==FALSE)

saveRDS(DailyQ_2018R_d, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DailyQ_2018R_d.rds')) 
write_csv(DailyQ_2018R_d, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DailyQ_2018R_d.csv'))     


values <- list()  
values[['DailyQ_2020OFP']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DailyQ_2020OFP.rds'
DailyQ_2020OFP <-  readRDS(values[["DailyQ_2020OFP"]])     

DailyQ_2020OFP_d <-   left_join(DailyQ_2020OFP, RainInf, by = c('date'='Date', 'Station'='FACILITYID')) %>%   #dry days only, each flow measurment
  filter(wet_within_72==FALSE)

saveRDS(DailyQ_2020OFP_d, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DailyQ_2020OFP_d.rds')) 
write_csv(DailyQ_2020OFP_d, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DailyQ_2020OFP_d.csv'))     

values <- list()  
values[['DailyQ_OCFS_2021']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DailyQ_OCFS_2021.rds'
DailyQ_OCFS_2021 <-  readRDS(values[["DailyQ_OCFS_2021"]]) 
DailyQ_OCFS_2021$Station[DailyQ_OCFS_2021$Station == "L03-693-1"] <- "L03-693-1 (L03P11)"
DailyQ_OCFS_2021$Station[DailyQ_OCFS_2021$Station == "L02-166-3"] <- "L02-166-3 (L02P26)"

values <- list()  
values[['DailyQ_SWN_2022']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DailyQ_SWN_2022.rds'
DailyQ_SWN_2022 <-  readRDS(values[["DailyQ_SWN_2022"]])

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

DailyQ_OCFS_2021_d <-   left_join(DailyQ_OCFS_2021, RainInf, by = c('date'='Date', 'Station'='FACILITYID')) %>%   #dry days only, each flow measurment
  filter(wet_within_72==FALSE) 

saveRDS(DailyQ_OCFS_2021_d, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DailyQ_OCFS_2021_d.rds')) 
write_csv(DailyQ_OCFS_2021_d, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DailyQ_OCFS_2021_d.csv'))

DailyQ_SWN_2022_d <-   left_join(DailyQ_SWN_2022, RainInf, by = c('date'='Date', 'Station'='FACILITYID')) %>%   #dry days only, each flow measurment
  filter(wet_within_72==FALSE) 

saveRDS(DailyQ_SWN_2022_d, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DailyQ_SWN_2022_d.rds')) 
write_csv(DailyQ_SWN_2022_d, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DailyQ_SWN_2022_d.csv'))

DailyQ_DFM_2022_d <-   left_join(DailyQ_DFM_2022, RainInf, by = c('date'='Date', 'Station'='FACILITYID')) %>%   #dry days only, each flow measurment
  filter(wet_within_72==FALSE) 


saveRDS(DailyQ_DFM_2022_d, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DailyQ_DFM_2022_d.rds')) 
write_csv(DailyQ_DFM_2022_d, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DailyQ_DFM_2022_d.csv'))

