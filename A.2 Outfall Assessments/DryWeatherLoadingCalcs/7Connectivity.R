## Task 9*: Determine connectivity adjustment for each outfall
#use field screening dataset, select flow condition and connectivity and Facility, determine monitoring year, and find average for each outfall and monitoring year

values <- list()  
values[['FieldScreen_dry']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/FieldScreen_dry.rds' 
  FieldScreen_dry <-  readRDS(values[["FieldScreen_dry"]]) %>%
  
  select(FacilityID, FlowConnectivity)

values[['MO_']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/MO_.rds'
MO_ <-  readRDS(values[["MO_"]]) %>%
  select(FACILITYID)

Connectivity1a <- left_join(MO_, FieldScreen_dry, by=c('FACILITYID'='FacilityID'))

saveRDS(Connectivity1a, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/Connectivity1a.rds'))
write_csv(Connectivity1a, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/Connectivity1a.csv'))

#Flow conditions:  0:nothing selected;  1: Flowing; 2:Pooled or Ponded;  3: Dry;  4:  Other - See Comments; NA: 'No Value' selected
Connectivity1$FlowConnectivity <-sub("Direct Connection", "2",Connectivity1$FlowConnectivity)
Connectivity1$FlowConnectivity<-sub("None - Flow Infiltrates or Outfall is Dry", "1",Connectivity1$FlowConnectivity)
Connectivity1$FlowConnectivity<-sub("Partial - Significant Distance", "2", Connectivity1$FlowConnectivity)
#Connectivity1$FlowConnectivity<-sub("4", "Undetermined",Connectivity1$FlowConnectivity)





Connectivity1 <-  Connectivity1a %>%
  mutate(Connectivity = ifelse(FlowConnectivity == 'None - Flow Infiltrates or Outfall is Dry',"0",ifelse(FlowConnectivity == 'Direct Connection',"1", ifelse(FlowConnectivity== 'Partial - Significant Distance',"0.5", ifelse(FlowConnectivity=='NA',"0.77",ifelse(is.na(FlowConnectivity), "0.77", "0.77")))))) %>%   #if direct connection assign 1; if no connection assign 0; otherwise assign 0.77
  as_tibble() %>%
  replace_na(list(Connectivity='0.77')) %>%
  #mutate(Date = as.Date(date, format = '%Y-%m-%d')) %>%
  #filter(!is.na(Date)) %>%
  #filter(Date>"2016-10-01" & Date <"2021-10-01") %>%
  mutate(Connectivity=as.numeric(Connectivity)) 
as_tibble()
  #select(FACILITYID, JURISDICTI, PERSISTENTFLOW, Connectivity, Date) 

str(Connectivity1)

Connectivity1 <- Connectivity1 %>%
  select(-Date) %>%
  group_by(FACILITYID) %>%
  mutate(avgCnx=(mean(Connectivity))) %>%
  ungroup() %>%
  select(FACILITYID,avgCnx) %>%
  unique()
#group_by(FACILITYID) %>%
#mutate(avgCnxAll=(mean(Connectivity)))



saveRDS(Connectivity1, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/Connectivity.rds'))
write_csv(Connectivity1, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/Connectivity.csv'))

#Find average connectivity for each outfall by monitoring year; add a column for monitoring year and summarize average connectivity per MY for each outfall
Connectivity1$MonitoringYear<- ifelse(Connectivity1$Date>(as.Date("2020-09-30", origin="1900-01-01")) & Connectivity1$Date<(as.Date("2021-10-01", origin="1900-01-01")), "MY2020-21",
                                      ifelse(Connectivity1$Date>(as.Date("2019-09-30", origin="1900-01-01")) & Connectivity1$Date<(as.Date("2020-10-01", origin="1900-01-01")), "MY2019-20",
                                             ifelse(Connectivity1$Date>(as.Date("2018-09-30", origin="1900-01-01")) & Connectivity1$Date<(as.Date("2019-10-01", origin="1900-01-01")), "MY2018-19",
                                                    ifelse(Connectivity1$Date>(as.Date("2017-09-30", origin="1900-01-01")) & Connectivity1$Date<(as.Date("2018-10-01", origin="1900-01-01")), "MY2017-18",
                                                           ifelse(Connectivity1$Date>(as.Date("2016-09-30", origin="1900-01-01")) & Connectivity1$Date<(as.Date("2017-10-01", origin="1900-01-01")), "MY2016-17",
                                                                  NA)))))



Connectivity1 <- Connectivity1 %>%
  select(-Date) %>%
  group_by(FACILITYID) %>%
  mutate(avgCnx=(mean(Connectivity))) %>%
  ungroup() %>%
  select(FACILITYID,avgCnx) %>%
  unique()
  #group_by(FACILITYID) %>%
  #mutate(avgCnxAll=(mean(Connectivity))) 

saveRDS(Connectivity1, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/Connectivity.rds'))
write_csv(Connectivity1, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/Connectivity.csv'))


#Connectivityall <- Connectivity1 %>%
  #select(FACILITYID, avgCnxAll) %>%
  #unique() %>%
  #right_join(., MajorOutfalls_lc1)

Connectivity202122<-Connectivity1 %>%
  filter(MonitoringYear=='MY2021-22') %>%
  right_join(.,MajorOutfalls_lc1) %>%
  right_join(.,Connectivityall) %>%
  select(FACILITYID, MonitoringYear, avgCnx, avgCnxAll) %>%
  unique() %>%
  replace_na(list(MonitoringYear='MY2021-22')) %>% #use all connectivity if no connectivity obs for monitoring year
  replace_na(list(avgCnxAll=0.77)) %>%
  mutate(avgCnxAll2=coalesce(avgCnx,avgCnxAll)) %>%
  select(FACILITYID,MonitoringYear,avgCnxAll2) %>%
  unique()

names(Connectivity202122)[names(Connectivity202021)=='avgCnxAll2'] <- 'avgCnx'               

saveRDS(Connectivity1, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/Connectivity.rds'))
write_csv(Connectivity1, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/Connectivity.csv'))
