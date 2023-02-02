## Task 11*: Import chemistry data
library(readxl)
library(dplyr)
library(lubridate)
library(tidyr)
library(here)
library(readr)

getwd()
getwd(
  
)

values <- list()  
values[['JURISinTRIBS2']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/JURISinTRIBS2.rds'
JURISinTRIBS2 <-readRDS(values[["JURISinTRIBS2"]])  

setwd("C:/Users/givens/Box/WQIP-Annual-Report-2021-22")
#2021-22
Chemdata2022R1and2 <- "C:/Users/givens/Box/OC Watersheds/Environmental Monitoring/WQIP/Monitoring and Assessment Program Implementation/MS4 Outfall Monitoring/Outfall Field Screening Follow-ups/DryWeatherDataNALsAssessment2022_draft.xlsx"

Chemdata2022R1and2 <- read_excel(Chemdata2022R1and2, skip = 1, 'Horizon Data') 
#format date
Chemdata2022R1and2 <- Chemdata2022R1and2 %>%
  separate('Collect Date', into=c("date", "time"), sep = " ") %>%
  mutate(Date = as.Date(date, format = '%m/%d/%Y')) %>%
  select(-`Entry.Set`, -Station, -`Sample ID`, -time, -`Field pH`, -`Field Temperature`, -`Field Dissolved Oxygen`, -`Field Turbidity - NTU`, -`Turbidity - NTU`, -'Temperature', `Field Specific Conductivity`)

str(Chemdata2022R1and2)

names(Chemdata2022R1and2)[names(Chemdata2022R1and2)=='Column1'] <- 'Station'


#use 1/2 the detection limit
OutfallChem2022 <- Chemdata2022R1and2 %>%
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


saveRDS(OutfallChem2022, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/OutfallChem2022.rds'))
write_csv(OutfallChem2022, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/OutfallChem2022.csv'))

values <- list()  
values[['OutfallChem2022']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/OutfallChem2022.rds'
OutfallChem2021 <-readRDS(values[["OutfallChem2022"]]) 


OutfallChem2022_avg <- OutfallChem2022 %>%  
  group_by(Station, `Sample Type`, Parameter) %>%
  #filter(!is.na(Result)) %>%
  mutate(ResultAvg=mean(Result, na.rm=TRUE)) %>%  #find average for each jurisdiction for each monitoring year
  ungroup() %>%
  unique() 


OutfallChem2022_samp <- OutfallChem2022_avg %>%
  select(Station, `Sample Type`, Parameter, Qualifier, ResultAvg, Units) %>%
  ungroup() %>%
  unique() 

saveRDS(OutfallChem2022_avg, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/OutfallChem2022_avg.rds'))
write_csv(OutfallChem2022_avg, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/OutfallChem2022_avg.csv'))

saveRDS(OutfallChem2022_samp, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/OutfallChem2022_samp.rds'))
write_csv(OutfallChem2022_samp, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/OutfallChem2022_samp.csv'))


#Find average conc by jurisdiction (use in loading calcs for unsampled outfalls)

#join jurisdiciton

values <- list()  
values[['JURISinTRIBS2']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/JURISinTRIBS2.rds'
JURISinTRIBS2 <-readRDS(values[["JURISinTRIBS2"]]) 

values[['OutfallChem2022_samp']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/OutfallChem2022_samp.rds'
OutfallChem2022_j <-  readRDS(values[["OutfallChem2022_samp"]]) %>%
  
  full_join(., JURISinTRIBS2, by=c('Station' = 'FACILITYID')) %>%
  select(Station,`Sample Type` , Parameter, Units, Qualifier, ResultAvg, JURISDICTI, JURISDICTI3) %>%
  group_by(JURISDICTI3, Parameter) %>%
  mutate(ResultJuris=mean(ResultAvg)) %>%
  ungroup() %>%
  summarise(JURISDICTI3, JURISDICTI,`Sample Type` , Parameter, ResultJuris, Units) %>%
  select(-JURISDICTI) %>%
  unique() %>%
  filter(!is.na(JURISDICTI3))

saveRDS(OutfallChem2022_j, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/OutfallChem2022_j.rds'))
write_csv(OutfallChem2022_j, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/OutfallChem2022_j.csv'))

## Task 12*: Multiply adjusted annual flow volume results (Task 9) with chemistry data (Task 10)
library(dplyr)
library(here)
library(readr)
library(tidyverse)
library(tidyr)
library(data.table)

values <- list()  

values[['OutfallChem2022_samp']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/OutfallChem2022_samp.rds'
OutfallChem2021_samp <-  readRDS(values[["OutfallChem2022_samp"]])
values[['OutfallChem2022_j']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/OutfallChem2022_j.rds'
OutfallChem2022_j <-  readRDS(values[["OutfallChem2022_j"]])


values[['AnnualFlow_US']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/AnnualFlow_US.rds'
AnnualFlow_US <-  readRDS(values[["AnnualFlow_US"]]) 

values[['AnnualFlow_s']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/AnnualFlow_s.rds'
AnnualFlow_s <-  readRDS(values[["AnnualFlow_s"]]) 

# select(FACILITYID, JURISDICTI3, ResultAvg, Qadj_Qall)

values[['AnnualFlow_US_j']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/AnnualFlow_US_j.rds'
AnnualFlow_US_j <-  readRDS(values[["AnnualFlow_US_j"]])


#sum by jurisdiction

#2022 sampled outfalls
Loads2022_samp <-left_join(OutfallChem2022_samp, AnnualFlow_s, by=c('Station'='FacilityID')) %>%  #Unit Converion:  453592 mg in a pound, 1 cubic foot is 62.43 pounds, 28.3168 Liters in one cubic-ft
  filter(!is.na(ResultAvg)) %>% 
  #filter(MonitoringYear=='MY2021-22') %>%
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

saveRDS(Loads2022_samp, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/Loads2022_samp.rds'))
write_csv(Loads2022_samp, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/Loads2022_samp.csv'))

#sum by jurisdiction
values <- list()  
values[['Loads2022_samp']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/Loads2022_samp.rds'
Loads2022_j <-  readRDS(values[["Loads2022_samp"]]) %>%
  filter(!is.na(JURISDICTI3)) %>%
  filter(Parameter!="SpecificConductivity") %>%
  filter(Parameter!="Turbidity") %>%
  group_by(JURISDICTI3, `Sample Type` ,Parameter) %>%
  mutate(LoadsJurs=sum(LoadsPoundsQS, na.rm=TRUE)) %>%
  ungroup()

saveRDS(Loads2022_j, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/Loads2022_j.rds'))
write_csv(Loads2022_j, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/Loads2022_j.csv'))

#2022 Unsampled Outfalls
values <- list() 
values[['AnnualFlow_US_j']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/AnnualFlow_US_j.rds'
AnnualFlow_US_j <-  readRDS(values[["AnnualFlow_US_j"]]) %>%
  select(JURISDICTI3, QJur_US) %>%
  unique()
  
values <- list()
values[['OutfallChem2022_j']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/OutfallChem2022_j.rds'
OutfallChem2022_j <-  readRDS(values[["OutfallChem2022_j"]]) %>%
  as.tibble()%>%
  as.data.frame()




values[['AnnualFlow_US']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/AnnualFlow_US.rds'
AnnualFlow_US <-  readRDS(values[["AnnualFlow_US"]]) 
  

Loads2022Uns<-full_join(AnnualFlow_US_j, OutfallChem2022_j,  by=c('JURISDICTI3'='JURISDICTI3')) %>%  #Unit Converion:  453592 mg in a pound, 1 cubic foot is 62.43 pounds, 28.3168 Liters in one cubic-ft
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
saveRDS(Loads2022Uns, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/Loads2022Uns.rds'))
write_csv(Loads2022Uns, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/Loads2022Uns.csv'))

Loads2022Uns_j <-  Loads2022Uns %>%
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

saveRDS(Loads2022Uns_j, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/Loads2022Uns_j.rds'))
write_csv(Loads2022Uns_j, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/Loads2022Uns_j.csv'))

#Combine loads from sampled and unsampled outfalls

#2022
values <- list() 
values[['Loads2022_j']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/Loads2022_j.rds'
Loads2022_j <-  readRDS(values[["Loads2022_j"]]) %>%
  select(JURISDICTI3,`Sample Type`, Parameter, Units, LoadsJurs, LoadsPoundsQS) %>%
  group_by(JURISDICTI3) %>%
  distinct() %>%
  filter(!is.na(JURISDICTI3))

values <- list()
values[['Loads2022Uns_j']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/Loads2022Uns_j.rds'
Loads2022Uns_j <-  readRDS(values[["Loads2022Uns_j"]]) %>%
  select(c('JURISDICTI3',`Sample Type` , 'Parameter', 'Units',  'LoadsPoundsQUns', 'LoadQUns_j')) %>%
  filter(Parameter!='SpecificConductivity'|Parameter!='Turbidity') %>%
  filter(!is.na(LoadQUns_j))

JurisLoads2022<-full_join(Loads2022_j, Loads2022Uns_j, by=c('JURISDICTI3','Sample Type', 'Parameter', 'Units')) 
JurisLoads2022$LoadsPoundsQUns[is.na(JurisLoads2022$LoadsPoundsQUns)] <- 0 #necessary to ensure Laguna Woods is included

JurisLoads2022 <- JurisLoads2022 %>%  
  group_by(JURISDICTI3, Parameter,`Sample Type`) %>%
  mutate(TotalLoadPounds=LoadsPoundsQUns+LoadsJurs) %>%
  filter(Parameter!="SpecificConductivity"|Parameter!="Turbidity") %>%
  ungroup() %>%
  unique() %>%
  select(JURISDICTI3, `Sample Type`, Parameter, LoadsJurs, LoadsPoundsQUns, TotalLoadPounds) %>%
  unique()

saveRDS(JurisLoads2022, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/JurisLoads2022.rds'))
write_csv(JurisLoads2022, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/JurisLoads2022.csv'))

#NALS Parameters
values <- list() 
values[['JurisLoads2022']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/JurisLoads2022.rds'
JurisLoads2022 <-  readRDS(values[["JurisLoads2022"]])


#2022
JurisLoads2022NALs <- JurisLoads2022 %>%
  filter(Parameter=='Fecal coliforms'|Parameter=='Enterococcus'|Parameter=='Nitrate + Nitrite as N'|Parameter=='Nitrogen, Total Kjeldahl'|Parameter=='Phosphorus as PO4'|Parameter=='TSS'|Parameter=='MBAS'|Parameter=='Iron, Total'|Parameter=='Manganese, Total'|Parameter=='Cadmium, Total'|Parameter=='Chromium, Total'|Parameter=='Copper, Total'|Parameter=='Lead, Total'|Parameter=='Nickel, Total'|Parameter=='Silver, Total'|Parameter=='Zinc, Total') %>%
  select(c('JURISDICTI3', `Sample Type` ,'Parameter','LoadsJurs' , 'LoadsPoundsQUns','TotalLoadPounds')) %>%
  unique() 

saveRDS(JurisLoads2022NALs, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/JurisLoads2022NALsg.rds'))
write_csv(JurisLoads2022NALs, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/JurisLoads2022NALsg.csv'))


