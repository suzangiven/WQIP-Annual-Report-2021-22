## Task 5 Import outfall observations 

source('A.2 Outfall Assessments/DryWeatherLoadingCalcs/1projectsetup.R')

getwd()

FieldScreening <-"A.2 Outfall Assessments/DryWeatherLoadingCalcs/Input/ProvisionalFlow/FieldScreening_2022.csv"

FieldScreening<-read.csv(FieldScreening) %>%
  filter(!is.na(PointX)) %>%
  mutate(Date = as.Date(ObsRecordDate, format = '%m/%d/%Y')) 
  
str(FieldScreening)

#date formatting


# Save processed dataset

saveRDS(FieldScreening, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/FieldScreening.rds')) 
write_csv(FieldScreening, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/FieldScreening.csv'))

library(stringr)


##Task 5.1: Filter out wet weather from instantaneous flow data
#use outfall field screen data from outafll with persistent flow (need to redefine persistent flow)

values <- list()  
values[['FieldScreening']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/FieldScreening.rds'
FieldScreeningb <-  readRDS(values[["FieldScreening"]]) 
  
FieldScreeningb <- within(FieldScreeningb, FacilityID[FacilityID == 'L04-136-1 (L04P07)' & FlowCondition == 'Flowing'] <- 'L04-136-1u (L04P07)')

str(FieldScreeningb)


FieldScreeningFL<-FieldScreeningb %>%
  select('FacilityID', 'FloatDischargeCFS', 'Date') %>%
  group_by(FacilityID, Date) %>%
  mutate(FloatDischargeCFS=mean(FloatDischargeCFS)) %>%
  unique()

FieldScreeningV<-FieldScreeningb %>%
  select('FacilityID', 'VolumeDischargeCFS', 'Date') %>%
  unique() %>%
  filter(!is.na(VolumeDischargeCFS))


FieldScreeningc <- left_join(FieldScreeningFL,FieldScreeningV, by=c('FacilityID', 'Date'))

FieldScreeningFC<-FieldScreeningb %>%
  select('FacilityID', 'Date', 'FlowCondition', 'PersistentFlow', 'SampleDry') %>%
  unique() %>%
  right_join(., FieldScreeningc, by=c('FacilityID', 'Date'))



FieldScreeningC<-FieldScreeningb %>%
  select('FacilityID','Date', 'FlowConnectivity') %>%
  unique() 

FieldScreeningC<-FieldScreeningC %>%
  mutate(across(where(is.character), ~na_if(.,""))) %>%
  filter(!is.na(FlowConnectivity)) %>%
  right_join(., FieldScreeningFC, by=c('FacilityID', 'Date'))


values <- list()  
values[['RainInf']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/RainInf.rds'
RainInf <-  readRDS(values[["RainInf"]]) 


FieldScreen_dry <- FieldScreeningC %>%
  left_join(., RainInf, by = c('FacilityID' = 'FACILITYID','Date' = 'Date')) %>%
  filter(wet_within_72 == "FALSE") %>%
  mutate(FloatDischargeCFS = ifelse(FlowCondition=='Dry', 0,FloatDischargeCFS)) %>%
  mutate(VolumeDischargeCFS = ifelse(FlowCondition=='Dry', 0,VolumeDischargeCFS)) %>%
  group_by(FacilityID, Date) %>%
  mutate(QInscfs = mean(c(FloatDischargeCFS, VolumeDischargeCFS))) %>%
  mutate(QInscfs= coalesce(QInscfs, FloatDischargeCFS)) %>%
  mutate(QInscfs= coalesce(QInscfs, VolumeDischargeCFS))

 
saveRDS(FieldScreen_dry, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/FieldScreen_dry.rds'))
write_csv(FieldScreen_dry, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/FieldScreen_dry.csv'))




