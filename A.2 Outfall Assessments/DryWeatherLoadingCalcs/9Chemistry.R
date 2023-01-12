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
