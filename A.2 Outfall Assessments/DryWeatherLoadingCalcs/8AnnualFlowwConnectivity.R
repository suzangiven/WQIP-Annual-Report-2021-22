## Task 10*: Multiply annual flow volume results (Task 7) with connectivity adjustment (Task 8)
#join datasets
values <- list()  
values[['DailyCFandPond_trib2']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DailyCFandPond_trib2.rds'
DailyCFandPond_trib2 <-  readRDS(values[["DailyCFandPond_trib2"]])  
 
#includes all outfalls, including those without delineated tributaries

values <- list()  
values[['DryDaysYearGroup']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DryDaysYearGroup.rds'
DryDaysYearGroup <-  readRDS(values[["DryDaysYearGroup"]]) %>%
  filter(!is.na(FACILITYID)) %>%
  filter(MonitoringYear=='MY2021-22')  %>%
  select(-c(MonitoringYear, rain_gage)) %>%
  data.table()

DryDaysYearGroup[DryDaysYearGroup == ""] <- NA

DryDaysYearGroup <- DryDaysYearGroup %>%
  filter(!is.na(FACILITYID))

values <- list()  
values[['Connectivity']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/Connectivity.rds'
Connectivity <-  readRDS(values[["Connectivity"]])   #includes all outfalls, including those without delineated tributaries
  

values[['MO_']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/MO_.rds'
MO_ <-  readRDS(values[["MO_"]]) %>%
  select(FACILITYID, )


str(AnnualFlow)
#Calculate total annual flow by jurisdiction and monitoring year - should only be for those with a result in DischargePCF3
AnnualFlow <- full_join(DailyCFandPond_trib2, Connectivity, by=c('FacilityID'= 'FACILITYID')) %>%
  unique() %>%
  left_join(., DryDaysYearGroup, by=c('FacilityID'='FACILITYID')) %>%
  group_by(FacilityID, JURISDICTI3) %>%
  mutate(Qadj_Qall=avgCnx*dry_days*DischargePCF3) 

saveRDS(AnnualFlow, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/AnnualFlow.rds'))  
write_csv(AnnualFlow, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/AnnualFlow.csv'))


#Find annual flow volume by jurisdiction
#Remove outfalls after volumes determined
AnnualFlow_Juriscon<-AnnualFlow %>% #remove outfalls upstream of most downstream outfall closest to receiving water
  filter(FacilityID != "J06-9362-1 (J06-P03)")  %>% #Dairy Fork
  filter(FacilityID != "J01-9082-5 (J02P08)") %>% #Wood Canyon
  filter(FacilityID != "L05-049-2" & FacilityID != "L05-049-1" & FacilityID != "L05-489-7" & FacilityID !="L05-489-3" & FacilityID !="L05-489-4") %>%  #Horno Basin
  filter(FacilityID != "J03-9234-8" & FacilityID != "J03-9234-6" & FacilityID !="J03-9234-5" & FacilityID !="K01-12032-2 (K01P11)") %>% #Niguel Storm Drain  
  filter(FacilityID != "L03-141-1" & FacilityID != "L03-141-3" & FacilityID != "L03-141-2" & FacilityID != "L03-172-2" & FacilityID != "L03-172-3" & FacilityID != "L03-073-3" & FacilityID != "L03-073-4" & FacilityID != "L03-073-5" & FacilityID != "L03-074-2" & FacilityID != "L03-074-1 (L03B01)") %>% #Oso Creek
  filter(FacilityID != "L04-136-1u (L04P07)") %>%
  filter(FacilityID != "J03-9199-2" & FacilityID != "J03-9190-1" & FacilityID != "J03-9199-1") %>%
  filter(FacilityID != "K01-12156-6"   & FacilityID != "K01-12156-4") %>% #Salt Creek
  filter(FacilityID != "M02-052-3" & FacilityID != "M02-052-4" & FacilityID != "M02-032-1" & FacilityID != "M02-085-1 (M02P06)" & FacilityID != "M02-085-2" & FacilityID != "M02-013-1" & FacilityID != "M02-086-1" & FacilityID != "M02-015-1" & FacilityID != "M02-028-2 (M02P08)" & FacilityID != 'M02-061-7' & FacilityID != 'M02-102-1') %>%   #Segunda Deshecha Channel
  filter(FacilityID != "M01-008-1" & FacilityID != "M01-060-3" & FacilityID != "M01-124-4") %>%
  filter(FacilityID != "M00.1-070-6" & FacilityID != "M00.1-070-4" & FacilityID != "M00.1-070-3" & FacilityID != "M00.1-070-2" & FacilityID != 'M00.1-070-1' & FacilityID !=  "M00.1-071-1 (M00S04)" & FacilityID !=  "M00.1-071-4 (M00S04)" & FacilityID !=  "M00.1-071-3 (M00S04)") %>% #coastal SC
  filter(FacilityID != "I01-11503-3"  & FacilityID != "I01-11503-4" & FacilityID != "I01-11502-1" & FacilityID != "I01-11216-3" & FacilityID != "I01-11216-2 (I02P12)" & FacilityID != "I01-11216-1 (I02P13)" & FacilityID != "I01-11216-4 (I02P14)" & FacilityID != "I01-11217-1") %>%  #Laguna Canyon Wash
  filter(FacilityID != "L01-613-1" & FacilityID != "L01-728-7 (L01S03)")  %>%

  select(JURISDICTI3, Qadj_Qall) %>%
  unique() %>%
  group_by(JURISDICTI3) %>%
  mutate(QTot=sum(Qadj_Qall)) %>% 
  select(JURISDICTI3, QTot) %>%
  unique() %>%
  mutate(DischargeJ_af = QTot/43560) %>%
  ungroup() %>%
  filter(!is.na(QTot))

saveRDS(AnnualFlow_Juriscon, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/AnnualFlow_Juriscon.rds'))  
write_csv(AnnualFlow_Juriscon, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/AnnualFlow_Juriscon.csv'))

test <- AnnualFlow %>%
 #remove outfalls upstream of most downstream outfall closest to receiving water
  filter(FacilityID != "J06-9362-1 (J06-P03)")  %>% #Dairy Fork
  filter(FacilityID != "J01-9082-5 (J02P08)") %>% #Wood Canyon
  filter(FacilityID != "L05-049-2" & FacilityID != "L05-049-1" & FacilityID != "L05-489-7" & FacilityID !="L05-489-3" & FacilityID !="L05-489-4") %>%  #Horno Basin
  filter(FacilityID != "J03-9234-8" & FacilityID != "J03-9234-6" & FacilityID !="J03-9234-5" & FacilityID !="K01-12032-2 (K01P11)") %>% #Niguel Storm Drain  
  filter(FacilityID != "L03-141-1" & FacilityID != "L03-141-3" & FacilityID != "L03-141-2" & FacilityID != "L03-172-2" & FacilityID != "L03-172-3" & FacilityID != "L03-073-3" & FacilityID != "L03-073-4" & FacilityID != "L03-073-5" & FacilityID != "L03-074-2" & FacilityID != "L03-074-1 (L03B01)") %>% #Oso Creek
  filter(FacilityID != "L04-136-1u (L04P07)") %>%
  filter(FacilityID != "J03-9199-2" & FacilityID != "J03-9190-1" & FacilityID != "J03-9199-1") %>%
  filter(FacilityID != "K01-12156-6"   & FacilityID != "K01-12156-4") %>% #Salt Creek
  filter(FacilityID != "M02-052-3" & FacilityID != "M02-052-4" & FacilityID != "M02-032-1" & FacilityID != "M02-085-1 (M02P06)" & FacilityID != "M02-085-2" & FacilityID != "M02-013-1" & FacilityID != "M02-086-1" & FacilityID != "M02-015-1" & FacilityID != "M02-028-2 (M02P08)" & FacilityID != 'M02-061-7' & FacilityID != 'M02-102-1') %>%   #Segunda Deshecha Channel
  filter(FacilityID != "M01-008-1" & FacilityID != "M01-060-3" & FacilityID != "M01-124-4") %>%
  filter(FacilityID != "M00.1-070-6" & FacilityID != "M00.1-070-4" & FacilityID != "M00.1-070-3" & FacilityID != "M00.1-070-2" & FacilityID != 'M00.1-070-1' & FacilityID !=  "M00.1-071-1 (M00S04)" & FacilityID !=  "M00.1-071-4 (M00S04)" & FacilityID !=  "M00.1-071-3 (M00S04)") %>% #coastal SC
  filter(FacilityID != "I01-11503-3"  & FacilityID != "I01-11503-4" & FacilityID != "I01-11502-1" & FacilityID != "I01-11216-3" & FacilityID != "I01-11216-2 (I02P12)" & FacilityID != "I01-11216-1 (I02P13)" & FacilityID != "I01-11216-4 (I02P14)" & FacilityID != "I01-11217-1") %>%  #Laguna Canyon Wash
  filter(FacilityID != "L01-613-1" & FacilityID != "L01-728-7 (L01S03)") 
write_csv(test, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/test.csv'))

#Determine Annual flow by jurisdiction for unsampled outfalls (not in Appendix M, and also ponded outfalls in Appendix M)

#find outfalls with flow measurements but not sampled
AnnualFlow_USa <- AnnualFlow %>%
  filter(is.na(SAMPLEDRY)) %>%
  #filter(Q_DailyavgCF >'-0.99') %>%
  select(FacilityID, JURISDICTI3, Qadj_Qall) 
   
saveRDS(AnnualFlow_USa, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/AnnualFlow_USa.rds'))
write_csv(AnnualFlow_USa, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/AnnualFlow_USa.csv'))  #put in connectivity by hand in csv#save csv as AnnualFlow_USa; use same connectivity as previous years if no data; if no observation, use 0.77


#outfalls sampled but without any flow measurements         
AnnualFlow_USb <- AnnualFlow %>%
  filter(SAMPLEDRY =='1' & (Q_DailyavgCF=='-0.99')) %>%
  select(FacilityID, JURISDICTI3, Qadj_Qall) %>%   
  unique() 


AnnualFlow_US <- bind_rows(AnnualFlow_USa, AnnualFlow_USb)   #flow from unsampled outfalls, both measured flow and estimates at ponded outfalls
  

#AnnualFlow_USj <- AnnualFlow_US %>%
  #select(JURISDICTI3, QJur_US) %>%
  #unique() %>%
  #group_by(JURISDICTI3) %>%
  #mutate(QJur_US=sum(QJur_US)) %>%
  #unique() 

#AnnualFlow_US <-AnnualFlow_US %>%
  #select(-QJur_US) %>%
  #left_join(.,AnnualFlow_USj)

saveRDS(AnnualFlow_US, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/AnnualFlow_US.rds'))
write_csv(AnnualFlow_US, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/AnnualFlow_US.csv'))


#by jurisdiction
values <- list()  
values[['AnnualFlow_US']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/AnnualFlow_US.rds'

AnnualFlow_US_j <- AnnualFlow_US %>%
  filter(FacilityID != "M01-008-1" & FacilityID != "M01-060-3" & FacilityID != "M01-124-4") %>%
  filter(FacilityID != "M00.1-070-6" & FacilityID != "M00.1-070-4" & FacilityID != "M00.1-070-3" & FacilityID != "M00.1-070-2" & FacilityID != 'M00.1-070-1' & FacilityID !=  "M00.1-071-1 (M00S04)" & FacilityID !=  "M00.1-071-4 (M00S04)" & FacilityID !=  "M00.1-071-3 (M00S04)") %>% #coastal SC
  filter(FacilityID != "I01-11503-3"  & FacilityID != "I01-11503-4" & FacilityID != "I01-11502-1" & FacilityID != "I01-11216-3" & FacilityID != "I01-11216-2 (I02P12)" & FacilityID != "I01-11216-1 (I02P13)" & FacilityID != "I01-11216-4 (I02P14)" & FacilityID != "I01-11217-1") %>%  #Laguna Canyon Wash
  filter(FacilityID != "L01-613-1" & FacilityID != "L01-728-7 (L01S03)")  %>%
  filter(FacilityID != "J06-9362-1 (J06-P03)")  %>% #Dairy Fork
  filter(FacilityID != "J01-9082-5 (J02P08)") %>% #Wood Canyon
  filter(FacilityID != "L05-049-2" & FacilityID != "L05-489-7" & FacilityID !="L05-489-4" & FacilityID !="L05-489-3") %>%  #Horno Basin
  filter(FacilityID != "J03-9234-8" & FacilityID != "J03-9234-6" & FacilityID !="J03-9234-5" & FacilityID !="K01-12032-2 (K01P11)") %>% #Niguel Storm Drain  
  filter(FacilityID != "L03-141-1" & FacilityID != "L03-141-3" & FacilityID != "L03-141-2" & FacilityID != "L03-172-2" & FacilityID != "L03-172-3" & FacilityID != "L03-073-3" & FacilityID != "L03-073-4" & FacilityID != "L03-073-5" & FacilityID != "L03-074-2" & FacilityID != "L03-074-1 (L03B01)") %>% #Oso Creek
  filter(FacilityID != "L04-136-1u (L04P07)") %>%
  filter(FacilityID != "J03-9199-2" & FacilityID != "J03-9190-1" & FacilityID != "J03-9199-1") %>%
  filter(FacilityID != "K01-12156-6"   & FacilityID != "K01-12156-4") %>% #Salt Creek
  filter(FacilityID != "M02-052-3" & FacilityID != "M02-052-4" & FacilityID != "M02-013-1" & FacilityID != "M02-086-1" & FacilityID != "M02-015-1" & FacilityID != "M02-028-2 (M02P08)" & FacilityID != 'M02-061-7' & FacilityID != 'M02-102-1') %>% #egunda Deshecha Channel

unique() %>%
  group_by(JURISDICTI3) %>%
  
  mutate(QJur_US=sum(Qadj_Qall)) %>%
  
  
  
unique() 

  saveRDS(AnnualFlow_US_j, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/AnnualFlow_US_j.rds'))
write_csv(AnnualFlow_US_j, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/AnnualFlow_US_j.csv'))

#Dataset with flow from sampled outfalls with flow
AnnualFlow_s <- AnnualFlow %>%
  filter(SAMPLEDRY =='1'& Q_DailyavgCF != '-0.99') %>%
  select(FacilityID, JURISDICTI3, Qadj_Qall) %>%
  unique() 
  

saveRDS(AnnualFlow_s, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/AnnualFlow_s.rds'))
write_csv(AnnualFlow_s, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/AnnualFlow_s.csv'))

#combine sampled and unsampled for all outfalls and total jurisdictional flow volume

AnnualFlow_all <- bind_rows(AnnualFlow_s, AnnualFlow_US)
saveRDS(AnnualFlow_all, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/AnnualFlow_all.rds'))
write_csv(AnnualFlow_all, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/AnnualFlow_all.csv'))


#by jurisdiction

values <- list()  
values[['AnnualFlow_s']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/AnnualFlow_s.rds'
AnnualFlow_S_j <-  readRDS(values[["AnnualFlow_s"]]) %>%
  filter(FacilityID != "L05-049-1" & FacilityID != "M02-032-1" & FacilityID != "M02-085-1 (M02P06)" & FacilityID != "M02-085-2"
         & FacilityID !="L05-489-3") %>% 
  select(c(JURISDICTI3, Qadj_Qall)) %>%
  group_by(JURISDICTI3) %>%
  mutate(QJur_S=sum(Qadj_Qall)) %>%
  ungroup() %>%
  unique() %>%
  select(c(FacilityID, JURISDICTI3, Qadj_Qall, QJur_S))

values <- list() 
values[['AnnualFlow_US_j']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/AnnualFlow_US_j.rds'
AnnualFlow_US_j <-  readRDS(values[["AnnualFlow_US_j"]]) %>%
  
  select(c(FacilityID, JURISDICTI3, Qadj_Qall, QJur_US))

AnnualFlow_J2<-full_join(AnnualFlow_S_j, AnnualFlow_US_j, by=c('JURISDICTI3', 'FacilityID', 'Qadj_Qall')) %>%
  select(FacilityID, JURISDICTI3, Qadj_Qall, QJur_US,QJur_S) %>%
  unique()

saveRDS(AnnualFlow_J2, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/AnnualFlow_J2.rds'))
write_csv(AnnualFlow_J2, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/AnnualFlow_J2.csv'))

AnnualFlow_J2short<-full_join(AnnualFlow_S_j, AnnualFlow_US_j, by=c('JURISDICTI3', 'FacilityID', 'Qadj_Qall')) %>%
  rowwise() %>%
  mutate(DischargeJ_cf = sum(QJur_S, QJur_US, na.rm=TRUE)) %>%
  mutate(DischargeJ_af = DischargeJ_cf/43560) %>%   #convert to acre-ft; 1 acre-feet = 43560 cf
  unique() %>%
  select(JURISDICTI3, DischargeJ_cf, DischargeJ_af) %>%
unique() %>%
  group_by(JURISDICTI3) %>%
  mutate(DischargeJ_cf = sum(DischargeJ_cf)) %>%
  mutate(DischargeJ_af = sum(DischargeJ_af)) %>%
  unique()


saveRDS(AnnualFlow_J2short, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/AnnualFlow_J2short.rds'))
write_csv(AnnualFlow_J2short, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/AnnualFlow_J2short.csv'))

#for print output (Jurisdictions, cnx)

values <- list()  
values[['Connectivity']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/Connectivity.rds'
Connectivity <-  readRDS(values[["Connectivity"]])


values[['AnnualFlow_all']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/AnnualFlow_all.rds'
AnnualFlow_all <-  readRDS(values[["AnnualFlow_all"]]) 

values[['AnnualCF2b']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/AnnualCF2b.rds'
AnnualCF2b <-  readRDS(values[["AnnualCF2b"]]) %>%
  select(FacilityID, JURISDICTI, JURISDICTI3, AnnualCF) %>%
  unique()

values <- list() 
values[['MO_']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/MO_.rds'
MO_ <-  readRDS(values[["MO_"]]) %>%
  select(FACILITYID, JURISDICTI)

values <- list() 
values[['AnnualFlow_J2']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/AnnualFlow_J2.rds'
AnnualFlow_J2 <-  readRDS(values[["AnnualFlow_J2"]]) %>%
  select(FacilityID, JURISDICTI3, QJur_US, QJur_S)



AnnualFlow_all_print <- AnnualFlow_all %>%
  full_join(., Connectivity, by=c('FacilityID' = 'FACILITYID')) %>%
  full_join(., AnnualFlow_J2short) %>%
  full_join(., MO_, by=c('FacilityID' = 'FACILITYID')) %>%
  full_join(., AnnualCF2b, by=c('FacilityID', 'JURISDICTI', 'JURISDICTI3')) %>%
  full_join(., AnnualFlow_J2, by=c('FacilityID', 'JURISDICTI3'))
  
 

write_csv(AnnualFlow_all_print, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/AnnualFlow_all_print.csv'))
