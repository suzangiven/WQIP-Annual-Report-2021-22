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

values <- list()  
values[['RainInf']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/RainInf.rds'
RainInf <-  readRDS(values[["RainInf"]]) 


FieldScreen_dry <- FieldScreeningb %>%
  left_join(., RainInf, by = c('FacilityID' = 'FACILITYID','Date' = 'Date')) %>%
  filter(wet_within_72 == "FALSE") %>%
  select('Jurisdiction', 'FacilityID', 'FlowCondition', 'FloatDischargeCFS','VolumeDischargeCFS', 'SampleDry', 'FlowConnectivity',  'Date')

saveRDS(FieldScreen_dry, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/FieldScreen_dry.rds'))
write_csv(FieldScreen_dry, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/FieldScreen_dry.csv'))




