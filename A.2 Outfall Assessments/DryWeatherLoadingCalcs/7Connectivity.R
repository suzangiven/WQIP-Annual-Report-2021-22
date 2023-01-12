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
