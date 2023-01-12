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
