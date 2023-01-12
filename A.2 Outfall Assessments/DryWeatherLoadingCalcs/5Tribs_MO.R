#Outfall list, automated pull from ArcGIS Pro  
Trib<- arc.open('C:/Users/givens/OneDrive - County of Orange/Shared with Everyone/Outfall/OutfallFieldScreen/Results/SPOCDSQL1205.sde/OCEnvRes.OCENVRESUSER.swDischargePoint_Tributary') %>%
  arc.select() %>%
  tibble::as_tibble() %>%
  select('FACILITYID','area_acres')
#update station names
Trib$FACILITYID<-sub("J01-9224-1", "J01-9224-1 (J01P24)",Trib$FACILITYID)
Trib$FACILITYID<-sub("J01-9224-2", "J01-9224-2 (J01P25)",Trib$FACILITYID)
Trib$FACILITYID<-sub("J06-9362-1", "J06-9362-1 (J06-03)",Trib$FACILITYID)
Trib$FACILITYID<-sub("K01-12177-1", "K01-12177-1 (K01P07)",Trib$FACILITYID)
Trib$FACILITYID<-sub("L01-766-2", "L01-766-2 (L01S06)",Trib$FACILITYID)
Trib$FACILITYID<-sub("L02-166-3", "L02-166-3 (L02P26)",Trib$FACILITYID)
Trib$FACILITYID<-sub("L02-622-2", "L02-622-2 (L02P32)",Trib$FACILITYID)
Trib$FACILITYID<-sub("SC10-075-3", "SC10-075-3 (M00S01)",Trib$FACILITYID)
Trib$FACILITYID<-sub("J01-10041-2", "J01-10041-2 (J03P13)",Trib$FACILITYID)
Trib$FACILITYID<-sub("J01-9007-1", "J01-9007-1 (J02P05)",Trib$FACILITYID)
Trib$FACILITYID<-sub("J01-10004-1", "J01-10004-1 (J01P01)",Trib$FACILITYID)
Trib$FACILITYID<-sub("I01-11343-2", "I01-11343-2 (I02P18)",Trib$FACILITYID)
Trib$FACILITYID<-sub("L01-728-4", "L01-728-4 (L01-DP)",Trib$FACILITYID)
Trib$FACILITYID<-sub("J01-9264-1", "J01-9264-1 (J01P06)",Trib$FACILITYID)
Trib$FACILITYID<-sub("I01-11216-1", "I01-11216-1 (I02P13)",Trib$FACILITYID)
Trib$FACILITYID<-sub("J01-9066-1", "J01-9066-1 (J01P04)",Trib$FACILITYID)
Trib$FACILITYID<-sub("J01-9364-3", "J01-9364-3 (J01P21)",Trib$FACILITYID) 


MO <-"A.2 Outfall Assessments/DryWeatherLoadingCalcs/Input/MO2022.csv"

MO<-read.csv(MO) %>%
  select('Facility.Identifier')

swDischarge<- arc.open('C:/Users/givens/OneDrive - County of Orange/Shared with Everyone/Outfall/OutfallFieldScreen/Results/SPOCDSQL1205.sde/OCEnvRes.OCENVRESUSER.swDischargePoint') %>%
  arc.select() %>%
  tibble::as_tibble() %>% 
  select(FACILITYID, JURISDICTI, AVGDISCHARGE, SAMPLEDRY,PERSISTENTFLOW)

DischargePointTrib <-   Trib %>%
  right_join(., MO,        
            by = c("FACILITYID" = "Facility.Identifier")) %>%
  left_join(., swDischarge, by='FACILITYID')

saveRDS(DischargePointTrib, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DischargePointTrib.rds'))
write_csv(DischargePointTrib, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DischargePointTrib.csv'))  


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
  right_join(., MO,
             by = c('FACILITYID'='Facility.Identifier')) 

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

