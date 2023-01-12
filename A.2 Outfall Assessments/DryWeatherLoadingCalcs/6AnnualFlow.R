#Find medians first then determine annual flow volume
#continuous  #change name from monthly to annual

values <- list() 
values[['DailyQ_2018R_d']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DailyQ_2018R_d.rds'
DailyQ_2018R_d <-  readRDS(values[["DailyQ_2018R_d"]])

values <- list()
values[['DailyQ_2020OFP_d']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DailyQ_2020OFP_d.rds'
DailyQ_2020OFP_d <-  readRDS(values[["DailyQ_2020OFP_d"]])

values[['DailyQ_OCFS_2021_d']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DailyQ_OCFS_2021_d.rds'
DailyQ_OCFS_2021_d <-  readRDS(values[["DailyQ_OCFS_2021_d"]])

values[['DailyQ_SWN_2021_d']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DailyQ_SWN_2021_d.rds'
DailyQ_SWN_2021_d <-  readRDS(values[["DailyQ_SWN_2021_d"]]) 



AnnualQ201516R <- DailyQ_2018R_d %>%  
  filter(date > "2015-09-30" & date < "2016-10-01") %>%
  group_by(Station) %>%
  as_tibble() %>%
  group_by(Station) %>%
  #mutate(median201718 = median(Flow..cfs.)) %>%
  summarise(medianRcfs = median(Flow..cfs.)) %>%
  mutate(MY = 'MY2015-16') %>%
  ungroup()

AnnualQ201516OFP <- DailyQ_2020OFP_d %>%  
  filter(date > "2015-09-30" & date < "2016-10-01") %>%
  group_by(Station) %>%
  as_tibble() %>%
  group_by(Station) %>%
  #mutate(median201718 = median(Flow..cfs.)) %>%
  summarise(medianOFPcfs = median(Flow..cfs.)) %>%
  mutate(MY = 'MY2015-16') %>%
  ungroup()

AnnualQ201516<-full_join(AnnualQ201516R,AnnualQ201516OFP, by=c('Station','MY'))


AnnualQ201617R <- DailyQ_2018R_d %>%  
  filter(date > "2016-09-30" & date < "2017-10-01") %>%
  group_by(Station) %>%
  as_tibble() %>%
  group_by(Station) %>%
  #mutate(median201718 = median(Flow..cfs.)) %>%
  summarise(medianRcfs = median(Flow..cfs.))  %>%
  mutate(MY = 'MY2016-17') %>%
  ungroup()

AnnualQ201617OFP <- DailyQ_2020OFP_d %>%  
  filter(date > "2016-09-30" & date < "2017-10-01") %>%
  group_by(Station) %>%
  as_tibble() %>%
  group_by(Station) %>%
  #mutate(median201718 = median(Flow..cfs.)) %>%
  summarise(medianOFPcfs = median(Flow..cfs.))  %>%
  mutate(MY = 'MY2016-17') %>%
  ungroup()

AnnualQ201617<-full_join(AnnualQ201617R,AnnualQ201617OFP, by=c('Station','MY'))

AnnualQ201718R <- DailyQ_2018R_d %>%  
  filter(date > "2017-09-30" & date < "2018-10-01") %>%
  group_by(Station) %>%
  as_tibble() %>%
  group_by(Station) %>%
  #mutate(median201718 = median(Flow..cfs.)) %>%
  summarise(medianRcfs = median(Flow..cfs.)) %>%
  mutate(MY = 'MY2017-18') %>%
  ungroup()

AnnualQ201718OFP <- DailyQ_2020OFP_d %>%  
  filter(date > "2017-09-30" & date < "2018-10-01") %>%
  group_by(Station) %>%
  as_tibble() %>%
  group_by(Station) %>%
  #mutate(median201718 = median(Flow..cfs.)) %>%
  summarise(medianOFPcfs = median(Flow..cfs.)) %>%
  mutate(MY = 'MY2017-18') %>%
  ungroup()

AnnualQ201718<-full_join(AnnualQ201718R,AnnualQ201718OFP, by=c('Station','MY'))

AnnualQ201819R <- DailyQ_2018R_d %>%  
  filter(date > "2018-09-30" & date < "2019-10-01") %>%
  group_by(Station) %>%
  as_tibble() %>%
  group_by(Station) %>%
  #mutate(median201718 = median(Flow..cfs.)) %>%
  summarise(medianRcfs = median(Flow..cfs.)) %>%
  mutate(MY = 'MY2018-19') %>%
  ungroup()

AnnualQ201819OFP <- DailyQ_2020OFP_d %>%  
  filter(date > "2018-09-30" & date < "2019-10-01") %>%
  group_by(Station) %>%
  as_tibble() %>%
  group_by(Station) %>%
  #mutate(median201718 = median(Flow..cfs.)) %>%
  summarise(medianOFPcfs = median(Flow..cfs.)) %>%
  mutate(MY = 'MY2018-19') %>%
  ungroup()

AnnualQ201819<-full_join(AnnualQ201819R,AnnualQ201819OFP, by=c('Station','MY'))

AnnualQ201920OFP <- DailyQ_2020OFP_d %>%  
  filter(date > "2019-09-30" & date < "2020-10-01") %>%
  group_by(Station) %>%
  as_tibble() %>%
  group_by(Station) %>%
  #mutate(median201718 = median(Flow..cfs.)) %>%
  summarise(medianOFPcfs = median(Flow..cfs.)) %>%
  mutate(MY = 'MY2019-20') %>%
  ungroup()

AnnualQ201920<-AnnualQ201920OFP

AnnualQ20202021O <- DailyQ_OCFS_2021_d %>%  
  filter(date > "2020-09-30" & date < "2021-10-01") %>%
  group_by(Station) %>%
  as_tibble() %>%
  group_by(Station) %>%
  #mutate(median201718 = median(Flow..cfs.)) %>%
  summarise(medianOCFS2021_cfs = median(Flow..cfs.)) %>%
  mutate(MY = 'MY2020-21') %>%
  ungroup()

AnnualQ20202021S <- DailyQ_SWN_2021_d %>%  
  filter(date > "2020-09-30" & date < "2021-10-01") %>%
  group_by(Station) %>%
  as_tibble() %>%
  group_by(Station) %>%
  #mutate(median201718 = median(Flow..cfs.)) %>%
  summarise(medianSWN2021_cfs = median(Flow..cfs.)) %>%
  mutate(MY = 'MY2020-21') %>%
  ungroup()


AnnualQ20202021 <-full_join(AnnualQ20202021O,AnnualQ20202021S, by=c('Station','MY'))


MedianQ_cont<-bind_rows(AnnualQ201516,AnnualQ201617,AnnualQ201718,AnnualQ201819,AnnualQ201920,AnnualQ20202021) 
#update station names
MedianQ_cont$Station<-sub("K01-12177-1", "K01-12177-1 (K01P07)",MedianQ_cont$Station)
MedianQ_cont$Station<-sub("M01-050-4", "M01-050-4 (M01@CGV)",MedianQ_cont$Station)
##TO DO:  make MonitoringYear consistent colnames(MedianQ_cont)[colnames(MedianQ_cont) == "MY"] <- "MonitoringYear"

saveRDS(MedianQ_cont, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/MedianQ_cont.rds'))
write_csv(MedianQ_cont, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/MedianQ_cont.csv'))

#compute annual flow from continuous  (see line 161 - DryDaysYearGroup)
values <- list()  
values[['MedianQ_cont']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/MedianQ_cont.rds'
MedianQ_cont <-  readRDS(values[["MedianQ_cont"]])

values <- list() 
values[['HistoricQ_medians2016']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/HistoricQ_medians2016.rds'
HistoricQ_medians2016 <-  readRDS(values[["HistoricQ_medians2016"]])
values[['Outfall_juris']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/Outfall_juris.rds'
Outfall_juris <-  readRDS(values[["Outfall_juris"]])

#update station names 

values[['Reprioritization_medians2018']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/Reprioritization_medians2018.rds'
Reprioritization_medians2018 <-  readRDS(values[["Reprioritization_medians2018"]])

Reprioritization_medians2018$FACILITYID<-sub("L03-455-3", "L03-455-3 (L03P37)",Reprioritization_medians2018$FACILITYID)
Reprioritization_medians2018$FACILITYID<-sub("M02-062-2", "M02-052-2",Reprioritization_medians2018$FACILITYID)
Reprioritization_medians2018$FACILITYID<-sub("L01-728-5", "L01-728-5 (L01-DP)",Reprioritization_medians2018$FACILITYID)

Reprioritization_medians2018 <- Reprioritization_medians2018  %>%
  left_join(., Outfall_juris, by=c('FACILITYID')) %>%
  filter(!is.na(JURISDICTI))

GeosyntecPrioritization<-full_join(HistoricQ_medians2016, Reprioritization_medians2018, by=c("FACILITYID", "JURISDICTI"))

saveRDS(GeosyntecPrioritization, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/GeosyntecPrioritization.rds'))
write_csv(GeosyntecPrioritization, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/GeosyntecPrioritization.csv')) #annual flow instantenous flow


#MedianQ_2016AnnualP <- right_join(DryDaysYearGroup, HistoricQ_medians2016,  by=c('FacilityID'='FACILITYID')) %>%
#mutate(QAnnualcf2016P=DailyQcfMeds2016*dry_days) %>%
#filter(!is.na(QAnnualcf2016P))
#saveRDS(MedianQ_2016AnnualP, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/MedianQ_2016AnnualP.rds'))
#write_csv(MedianQ_2016AnnualP, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/MedianQ_2016AnnualP.csv')) #annual flow, 2016 Prioritization

#MedianQ_2018AnnualR <- right_join(DryDaysYearGroup, Reprioritization_medians2018,  by=c('FacilityID'='FACILITYID')) %>%
#mutate(QAnnualcf2018R = `DailyQcfMeds2018`*dry_days) %>%
#filter(!is.na(QAnnualcf2018R))
#saveRDS(MedianQ_2018AnnualR, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/MedianQ_2018AnnualR.rds'))
#write_csv(MedianQ_2018AnnualR, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/MedianQ_2018AnnualR.csv')) #annual flow, 2018 Prioritization

#instant
#from field screening data, find average for each monitoring year, and multiply by number of dry days
#values <- list()  
#values[['DryDaysYearGroup']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DryDaysYearGroup.rds'
#DryDaysYearGroup <-  readRDS(values[["DryDaysYearGroup"]])

values <- list() 
values[['FieldScreen_dry']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/FieldScreen_dry.rds'
InstQ <-  readRDS(values[["FieldScreen_dry"]]) #line 370

str(InstQ)

InstQ <- InstQ %>%  #flowing only
  select(c("date", "FACILITYID",  "QDailycfIns", "JURISDICTI", "Dischargecfs",  "rain_gage", "FLOWCOND", "FLOWCONNECTIVITY", "AVGDISCHARGE", "PERSISTENTFLOW", "DRYSAMPLED")) %>%
  #select(c("date", "Facility.Identifier", "Facility.Type", "Size.1",  "QDailycfIns", "Jurisdiction", "Dischargecfs", "rain_gage", "Flow.Condition")) %>%
  filter(!is.na(Dischargecfs))  #remove outfalls without flow measurements (put back in later with all flow measurements)

#Define Monitoring Year
InstQ$MonitoringYear<- ifelse(InstQ$date>(as.Date("2020-09-30", origin="1900-01-01")) & InstQ$date<(as.Date("2021-10-01", origin="1900-01-01")), "MY2020-21",
                              ifelse(InstQ$date>(as.Date("2019-09-30", origin="1900-01-01")) & InstQ$date<(as.Date("2020-10-01", origin="1900-01-01")), "MY2019-20",
                                     ifelse(InstQ$date>(as.Date("2018-09-30", origin="1900-01-01")) & InstQ$date<(as.Date("2019-10-01", origin="1900-01-01")), "MY2018-19",
                                            ifelse(InstQ$date>(as.Date("2017-09-30", origin="1900-01-01")) & InstQ$date<(as.Date("2018-10-01", origin="1900-01-01")), "MY2017-18",
                                                   ifelse(InstQ$date>(as.Date("2016-09-30", origin="1900-01-01")) & InstQ$date<(as.Date("2017-10-01", origin="1900-01-01")), "MY2016-17",
                                                          ifelse(InstQ$date>(as.Date("2015-09-30", origin="1900-01-01")) & InstQ$date<(as.Date("2016-10-01", origin="1900-01-01")), "MY2015-16",
                                                                 ifelse(InstQ$date>(as.Date("2014-09-30", origin="1900-01-01")) & InstQ$date<(as.Date("2015-10-01", origin="1900-01-01")), "MY2014-15",  
                                                                        NA)))))))


saveRDS(InstQ, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/InstQ.rds'))
write_csv(InstQ, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/InstQ.csv')) #annual flow instantenous flow

## Task 6*: Determine site-specific instantaneous:continuous flow ratio
#Daily Results

#Monthly Results
values <- list()  
values[['Outfall_juris']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/Outfall_juris.rds'
Outfall_juris<-readRDS(values[["Outfall_juris"]])

values[['InstQ']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/InstQ.rds'
InstQ<-readRDS(values[["InstQ"]])

values <- list()  
values[['MajorOutfalls_lc']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/MajorOutfalls_lc.rds'
MajorOutfalls_lc <-  readRDS(values[["MajorOutfalls_lc"]]) 
MajorOutfalls_avQ <- MajorOutfalls_lc %>%
  select('FACILITYID', 'AVGDISCHARGE', 'DRYSAMPLED','PERSISTENTFLOW', 'INSPECTED', 'OCFS', 'POINT_Y', 'POINT_X', 'ACCESSIBILITY','JURISDICTI') %>%
  mutate(avgdailyQ=AVGDISCHARGE*24*60*60) %>%
  mutate(avgdailyQadj=avgdailyQ*.69)

saveRDS(MajorOutfalls_avQ, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/MajorOutfalls_avQ.rds'))
write_csv(MajorOutfalls_avQ, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/MajorOutfalls_avQ.csv')) #annual flow instantenous flow



## Task 7.1: (Only) continuous data
#list of outfalls with (only) continuous data
#There are not any outfalls with only continuous flow data
values <- list()  
values[['MedianQ_cont']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/MedianQ_cont.rds'

MedianQ_cont<-readRDS(values[["MedianQ_cont"]]) %>% 
  left_join(., MajorOutfalls_avQ, by=c('Station'='FACILITYID')) %>% #find daily average
  mutate(QDailycfContRcf=medianRcfs*60*60*24) %>%
  mutate(QDailycfContOFPcf=medianOFPcfs*60*60*24) %>%
  mutate(QDailycfContOCFScf=medianOCFS2021_cfs*60*60*24) %>%
  mutate(QDailycfContSWNcf=medianSWN2021_cfs*60*60*24)

colnames(MedianQ_cont)[colnames(MedianQ_cont) == "Station"] <- "FACILITYID"

#colnames(MedianQ_cont)[colnames(MedianQ_cont) == "MY"] <- "MonitoringYear"

## Task 7.2: (Only) instantaneous data #add dry days later (10/28/2021)

values <- list()  
values[['InstQ']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/InstQ.rds'
InstQ<-readRDS(values[["InstQ"]]) %>%  
  left_join(., MajorOutfalls_avQ, by=c('FACILITYID', "JURISDICTI", 'AVGDISCHARGE', 'DRYSAMPLED', 'PERSISTENTFLOW')) %>% 
  mutate(QInsDailyCFAdj=QDailycfIns*0.69) #0.69 adjustment for Inst flow

## Task 7.3*: Both continuous and instantaneous data (and flow prioritization files)
values <- list()  
values[['GeosyntecPrioritization']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/GeosyntecPrioritization.rds'
GeosyntecPrioritization<-readRDS(values[["GeosyntecPrioritization"]]) 
#add_column(MonitoringYear=NA, .after= "Continuous_20181920")


DailyAvgCF <- left_join(InstQ, MedianQ_cont, by=c("FACILITYID", "JURISDICTI","AVGDISCHARGE", "DRYSAMPLED", 'PERSISTENTFLOW', "OCFS", "INSPECTED","POINT_Y", "POINT_X", "ACCESSIBILITY", "avgdailyQ", "avgdailyQadj")) %>%
  full_join(., GeosyntecPrioritization, by=c("FACILITYID", "JURISDICTI"))


saveRDS(DailyAvgCF, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DailyAvgCF.rds'))
write_csv(DailyAvgCF, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DailyAvgCF.csv')) 

#AnnualCF$QAnnualcfContOFPcf <- replace(AnnualCF$QAnnualcfContOFPcf,AnnualCF$QAnnualcfContOFPcf == 0.0, NA)  

## Task 7.4*: No flow measurements
#could be ponded or dry (should this include un-verified and not found outfalls?)

stn_near <-"A.2 Outfall Assessments/DryWeatherLoadingCalcs/Input/Rain/swDischargePoint_Near_Coop.csv"

stn_near <- read.csv(stn_near)

values[['MajorOutfalls_avQ']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/MajorOutfalls_avQ.rds'
MajorOutfalls_avQ <-  readRDS(values[["MajorOutfalls_avQ"]]) %>%
  select(FACILITYID, JURISDICTI, AVGDISCHARGE)

values <- list()  
values[['FieldScreen_dry']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/FieldScreen_dry.rds'
NoQ <-  readRDS(values[["FieldScreen_dry"]]) %>%
  filter(date > '2020-09-30' & date <'2021-10-01' ) %>%
  right_join(., MajorOutfalls_avQ) %>%
  left_join(., stn_near, by=c('FACILITYID'='FacilityID')) %>%
  filter(is.na(QDailycfIns)) %>%
  as_tibble() %>%
  #filter(FLOWCOND!=3) %>% #remove dry obs later down in script
  select(date, FACILITYID, JURISDICTI,  FLOWCOND, Dischargecfs, AVGDISCHARGE,FLOWCONNECTIVITY, PERSISTENTFLOW, DRYSAMPLED) 


NoQ$MonitoringYear<- 
  ifelse(ifelse(NoQ$date>(as.Date("2020-09-30", origin="1900-01-01")) & NoQ$date<(as.Date("2021-10-01", origin="1900-01-01")), "MY2020-21",
                NA)) %>%
  replace_na(list(MonitoringYear= 'MY2020-21')) 



NoQ$MonitoringYear <- as.character(NoQ$MonitoringYear)

str(NoQ)

#filter(FLOWCONNECTIVITY != "1")  #only connectivity should this be applied later in the calculations? 

saveRDS(NoQ, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/NoQ.rds'))
write_csv(NoQ, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/NoQ.csv'))
###Combine all daily flow files
#join in ponded outfalls

#first find average of previous measurements from outfall.  Then for remaining without any measurements, find jurisdictional average
values <- list()  
values[['MajorOutfalls_avQ']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/MajorOutfalls_avQ.rds'
MajorOutfalls_avQ <-  readRDS(values[["MajorOutfalls_avQ"]]) %>%
  select(FACILITYID, AVGDISCHARGE)

values <- list()  
values[['NoQ']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/NoQ.rds'
NoQ_f <-  readRDS(values[["NoQ"]]) %>%
  #select(date, FACILITYID, JURISDICTI,JURISDICTI2, Dischargecfs, FLOWCONNECTIITY) %>%
  
  # filter(FACILITYID != "DP04-12015-1"& FACILITYID !="J01-10004-2" & FACILITYID !="J01-10006-3" & FACILITYID !="J01-9005-3" & FACILITYID !="J01-9040-1" & FACILITYID !="J01-9046-2" & FACILITYID !="J01-9066-1 (J01P04)" & FACILITYID !="J01-9082-5 (J02P08)" & FACILITYID !="J01-9131-1 (J01P28)" & FACILITYID !="J01-9273-1" & FACILITYID !="J01-9349-1" & FACILITYID !="J01-9377-1" & FACILITYID !="J01-9785-1"	& FACILITYID !="J03-9221-1 (J03P02)")		
  #right_join(., MajorOutfalls_avQ, by=c('FACILITYID'='FACILITYID', 'JURISDICTI', 'AVGDISCHARGE')) %>%
  replace_na(list(Dischargecfs=-0.99)) %>%
  mutate(avgdailyQ=AVGDISCHARGE*24*60*60) %>%  #estimate outfalls with no measurements with average
  mutate(avgdailyQadj=(avgdailyQ*.69)) %>%
  filter(!is.na(FACILITYID)) %>%
  unique()


NoQ_f$MonitoringYear<- 
  ifelse(NoQ_f$date>(as.Date("2019-09-30", origin="1900-01-01")) & NoQ_f$date<(as.Date("2020-10-01", origin="1900-01-01")), "MY2019-20",
         ifelse(NoQ_f$date>(as.Date("2018-09-30", origin="1900-01-01")) & NoQ_f$date<(as.Date("2019-10-01", origin="1900-01-01")), "MY2018-19",
                ifelse(NoQ_f$date>(as.Date("2020-09-30", origin="1900-01-01")) & NoQ_f$date<(as.Date("2021-10-01", origin="1900-01-01")), "MY2020-21",     
                       NA)))

colnames(NoQ_f)[colnames(NoQ_f) == "Dischargecfs"] <- "DischargeP"  #includes ponded AND flowing outfalls -sometimes flow measurements were not recorded

NoQ_f <-  NoQ_f %>%
  
  filter(MonitoringYear=='MY2020-21'|is.na(MonitoringYear)) %>% 
  replace_na(list(MonitoringYear= 'MY2020-21')) 



saveRDS(NoQ_f, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/NoQ_f.rds'))
write_csv(NoQ_f, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/NoQ_f.csv'))

## Task 8*: Compute flow volume results from four methods (Task 6.1-6.4) #
#Combine Daily flow file and no flow files to estimate flow at outfalls without flow measurements
values <- list()  

values[['DailyAvgCF']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DailyAvgCF.rds'
DailyAvgCF <-  readRDS(values[["DailyAvgCF"]])
values[['NoQ_f']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/NoQ_f.rds'
NoQ_f <-  readRDS(values[["NoQ_f"]])
values[['DailyAvgCF']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DailyAvgCF.rds'
DailyAvgCF <-  readRDS(values[["DailyAvgCF"]]) %>%
  filter(MonitoringYear == 'MY2020-21'|is.na(MonitoringYear))

values[['MajorOutfalls_avQ']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/MajorOutfalls_avQ.rds'
MajorOutfalls1 <-  readRDS(values[["MajorOutfalls_avQ"]]) %>%
  select(FACILITYID)

values[['MajorOutfalls_avQ']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/MajorOutfalls_avQ.rds'
MajorOutfalls_avQ <-  readRDS(values[["MajorOutfalls_avQ"]]) %>%
  select(FACILITYID, JURISDICTI, AVGDISCHARGE)

#DailyCFandPond <- NoQ_f %>%   
#select(-c(date)) %>%
#right_join(., MajorOutfalls_avQ, by=c('FACILITYID', 'AVGDISCHARGE', 'JURISDICTI'))

AnnualCF <- full_join(DailyAvgCF,NoQ_f, by=c('date', 'FACILITYID', 'JURISDICTI', 'MonitoringYear', 'AVGDISCHARGE', 'FLOWCOND', 'FLOWCONNECTIVITY', 'PERSISTENTFLOW', 'DRYSAMPLED', 'avgdailyQ', 'avgdailyQadj'))  %>% 
  #right_join(., MajorOutfalls_avQ, by=c('FACILITYID', 'JURISDICTI', 'AVGDISCHARGE', 'POINT_Y', 'POINT_X', 'DRYSAMPLED', 'OCFS', 'ACCESSIBILITY','INSPECTED', 'avgdailyQ', 'avgdailyQadj')) %>%
  #filter(MonitoringYear == 'MY2020-21'|is.na(MonitoringYear)) %>%
  group_by(FACILITYID, MonitoringYear) %>%  #Find average annual flow for each outfall
  mutate(Q_DailyavgCF= ifelse(!is.na(QInsDailyCFAdj),mean(c(QDailycfContRcf,QDailycfContOFPcf, QDailycfContOCFScf, QDailycfContSWNcf, DailyQcfMeds2018, DailyQcfMeds2016b,DailyQcfMeds2016, QInsDailyCFAdj), na.rm=TRUE), avgdailyQadj)) %>%
  ungroup() %>%
  unique() %>%
  select(-c(MY, rain_gage)) 
# filter(FLOWCOND ==1|FLOWCOND ==2|FLOWCOND ==4|is.na(FLOWCOND)) filter for dry days later

saveRDS(AnnualCF, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/AnnualCF.rds'))
write_csv(AnnualCF, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/AnnualCF.csv'))

getwd()
#Estimate flow at outfalls without measurements
values <- list()  
values[['JURISinTRIBS2']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/JURISinTRIBS2.rds'
JURISinTRIBS2 <-  readRDS(values[["JURISinTRIBS2"]]) %>%
  select(FACILITYID, JURISDICTI3, JURISDICTI, PERCENTAGE, area_acres, AREA)

values[['AnnualCF']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/AnnualCF.rds'
AnnualCF <-  readRDS(values[["AnnualCF"]])

values[['MajorOutfalls_avQ']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/MajorOutfalls_avQ.rds'
MajorOutfalls_avQ <-  readRDS(values[["MajorOutfalls_avQ"]]) %>%
  as.data.frame() %>%
  select(FACILITYID, JURISDICTI, AVGDISCHARGE, avgdailyQadj)

DailyCFandPond_trib <- full_join(AnnualCF, JURISinTRIBS2,  by=c('FACILITYID', 'JURISDICTI')) %>%
  filter(!is.na(JURISDICTI3)) %>% #filters out non major outfalls
  filter(!is.na(MonitoringYear))

saveRDS(DailyCFandPond_trib, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DailyCFandPond_trib.rds'))
write_csv(DailyCFandPond_trib, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DailyCFandPond_trib.csv'))

#install.packages("psych")          # Install psych package
library("psych")
values <- list()  
values[['DailyCFandPond_trib']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DailyCFandPond_trib.rds'
DailyCFandPond_trib <-  readRDS(values[["DailyCFandPond_trib"]]) 

values[['MajorOutfalls_avQ']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/MajorOutfalls_avQ.rds'
MajorOutfalls_avQ <-  readRDS(values[["MajorOutfalls_avQ"]])  %>% 
  select(FACILITYID)


DailyCFandPond_trib2a <-  DailyCFandPond_trib %>%    #estimate flow at ponded outfalls and outfalls not visited from average daily (instead of annual)
  filter(FLOWCOND!='3'|is.na(FLOWCOND)) %>% 
  replace_na(list(PERCENTAGE=100)) %>%
  filter(!is.na(MonitoringYear)) %>%
  #select(FACILITYID, JURISDICTI, JURISDICTI3,date,FLOWCOND, DRYSAMPLED,  QAnnualcfContOFPcf, QAnnualcf2016P, QAnnualcf2018R,QInsAnnualCF, QInsAnnualCFAdj, DischargeP, QavgCF, MonitoringYear, PERCENTAGE, AVGDISCHARGE, FLOWCONNECTIVITY, area_acres, Acres) %>%
  select(FACILITYID, JURISDICTI, JURISDICTI3,date,FLOWCOND, PERSISTENTFLOW, DRYSAMPLED, DischargeP, Q_DailyavgCF, MonitoringYear, PERCENTAGE, AVGDISCHARGE, avgdailyQ,avgdailyQadj, FLOWCONNECTIVITY, area_acres, AREA) %>%
  group_by(FACILITYID, MonitoringYear) %>%
  mutate(DischargePCF = ifelse(DischargeP < 0 & is.na(Q_DailyavgCF), mean(Q_DailyavgCF), Q_DailyavgCF)) %>% #estimates flow for outfalls without flow measurements from prior measurements
  #estimate discharge at ponded outfalls - need to group by JURIS3
  mutate(DischargePCF2= ifelse(!is.na(PERCENTAGE), PERCENTAGE*DischargePCF/100, DischargePCF)) %>%
  group_by(JURISDICTI3, MonitoringYear) %>% #use geomean instead of log mean (July 27, 2021; 2019-20 Annual Report used arithmetic average)
  mutate(DischargePCF3 = ifelse(DischargeP < 0 & is.na(DischargePCF2) , geometric.mean(DischargePCF2, na.rm=TRUE), DischargePCF2)) %>%   #only calculates for outfalls with no flow measurements, but column includes daily flow for outfalls with data
  ungroup() 
# filter(!is.na(DischargePCF3)) 

DailyCFandPond_trib2b <-  DailyCFandPond_trib %>%  #marks dry observations as 0, but keeps in record
  select(FACILITYID, JURISDICTI, JURISDICTI3,date,FLOWCOND, DRYSAMPLED, PERSISTENTFLOW, DischargeP, Q_DailyavgCF, MonitoringYear, PERCENTAGE, AVGDISCHARGE, avgdailyQ, avgdailyQadj, FLOWCONNECTIVITY, area_acres, AREA) %>%
  filter(FLOWCOND =='3') %>% 
  mutate(DischargePCF = as.numeric(0)) %>%
  mutate(DischargePCF2 = as.numeric(0)) %>%
  mutate(DischargePCF3 = as.numeric(0)) 

DailyCFandPond_trib2 <-rbind(DailyCFandPond_trib2a, DailyCFandPond_trib2b) 

saveRDS(DailyCFandPond_trib2, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DailyCFandPond_trib2.rds'))
write_csv(DailyCFandPond_trib2, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DailyCFandPond_trib2.csv'))

#Annual Flow volume - join in dry_days_file
values <- list()  
values[['DryDaysYearGroup']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DryDaysYearGroup.rds'
DryDaysYearGroup <-  readRDS(values[["DryDaysYearGroup"]]) %>%
  filter(!is.na(FacilityID)) %>%
  filter(MonitoringYear=='MY2020-21')

#DryDaysYearGroup$FacilityID<-sub("J01-9224-2", "J01-9224-2 (J01P25)",DryDaysYearGroup$FacilityID)
#DryDaysYearGroup$FacilityID<-sub("J03-9216-2", "J03-9216-2 (J03P01)",DryDaysYearGroup$FacilityID)
AnnualCF2b <- DailyCFandPond_trib2 %>%
  left_join(.,DryDaysYearGroup, by=c('FACILITYID'='FacilityID', 'MonitoringYear')) %>%
  filter(MonitoringYear=='MY2020-21') %>%
  group_by(FACILITYID, JURISDICTI3) %>%
  mutate(AnnualCFb=mean(DischargePCF3)) %>%
  mutate(AnnualCF = dry_days*AnnualCFb)

saveRDS(AnnualCF2b, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/AnnualCF2b.rds'))
write_csv(AnnualCF2b, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/AnnualCF2b.csv'))


#Find annual flow volume by jurisdiction
#Remove outfalls after volumes determined
AnnualCF2 <- AnnualCF2b %>%  #remove outfalls upstream of most downstream outfall closest to receiving water
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

saveRDS(AnnualCF2, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/AnnualCF2.rds'))
write_csv(AnnualCF2, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/AnnualCF2.csv'))


#For tribs with no area:  find total area accounted for, subtract from total jurisdictional area, divide the remainig acreage equally amongst other jurisdicitons 
values <- list()  
values[['DischargePointTrib']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DischargePointTrib.rds'
DischargePointTrib <-  readRDS(values[["DischargePointTrib"]]) #includes all

values <- list() 
values[['JURISinTRIBS2']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/JURISinTRIBS2.rds'
JURISinTRIBS2 <-  readRDS(values[["JURISinTRIBS2"]])

values[['R9_Cities']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/R9_Cities.rds'
R9_Cities <-  readRDS(values[["R9_Cities"]])


DischargePointTrib_p <- DischargePointTrib %>%  #includes all major outfalls
  select('FACILITYID', 'area_acres', 'JURISDICTI') %>%
  full_join(JURISinTRIBS2, by=c('FACILITYID', 'JURISDICTI', 'area_acres')) %>%
  mutate(JURISDICTI3 = JURISDICTI2) %>%
  mutate(JURISDICTI3=coalesce(JURISDICTI3, JURISDICTI)) %>%
  select('FACILITYID', 'JURISDICTI', 'JURISDICTI3', 'area_acres','AREA') %>%
  right_join(., MajorOutfalls_avQ) %>%
  full_join(., R9_Cities, by=c('JURISDICTI3' = 'JURISDICTI')) %>%
  unique() %>%  #remove outfalls upstream of most downstream outfall closest to receiving water
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
  filter(FACILITYID != "L01-613-1" & FACILITYID != "L01-728-7 (L01S03)") %>%
  
  group_by(JURISDICTI3) %>%
  mutate(sumNA=sum(is.na(AREA))) %>%
  ungroup() %>%
  tibble() 

saveRDS(DischargePointTrib_p, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DischargePointTrib_p.rds'))
write_csv(DischargePointTrib_p, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DischargePointTrib_p.csv'))

DischargePointTrib_q <- DischargePointTrib_p %>%   #outfalls with tributaries delineated - total area, including outfalls without de-lineated tributaries (e.g. DP01-12022-1), taken into account in calculation, even though they are filtered away
  filter(!is.na(AREA)) %>%
  group_by(JURISDICTI3) %>%
  mutate(AcresJO=sum(AREA)) %>%
  ungroup() %>%
  select('FACILITYID','AREA', 'area_acres', 'JURISDICTI3', 'AreaJ', 'AcresJO', 'sumNA') %>%
  unique() 

AcresJ0 <- DischargePointTrib_q %>%
  select('AcresJO', 'JURISDICTI3', 'sumNA') %>%
  unique()

DischargePointTribAfill <- DischargePointTrib_p %>%  #outfalls without tribs delineated
  filter(is.na(AREA)) %>%
  full_join(., AcresJ0, by=c('JURISDICTI3', 'sumNA')) %>%
  select('FACILITYID','AREA', 'area_acres', 'JURISDICTI3', 'AreaJ', 'AcresJO', 'sumNA') 

DischargePointTrib_all <- bind_rows(DischargePointTribAfill, DischargePointTrib_q) %>% #areas with and without delineations
  mutate(AcresOb= ifelse(is.na(AREA), ((AreaJ-AcresJO)/sumNA), AREA))

saveRDS(DischargePointTrib_all, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DischargePointTrib_all.rds'))
write_csv(DischargePointTrib_all, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DischargePointTrib_all.csv'))

#make unique values for JURISDICTI3 and ANNUAL CF. 
values <- list()  
values[['DischargePointTrib_all']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DischargePointTrib_all.rds'
DischargePointTrib_all <-  readRDS(values[["DischargePointTrib_all"]]) 
values[['AnnualCF2']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/AnnualCF2.rds'
AnnualCF2 <-  readRDS(values[["AnnualCF2"]]) 
values[['MajorOutfalls_avQ']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/MajorOutfalls_avQ.rds'
MajorOutfalls_avQ <-  readRDS(values[["MajorOutfalls_avQ"]]) %>%
  select(c(FACILITYID, JURISDICTI))

AnnualCF2<-left_join(AnnualCF2, MajorOutfalls_avQ)

DischargePointTrib_flow <- full_join(AnnualCF2, DischargePointTrib_all, by=c('FACILITYID', 'JURISDICTI3', 'AREA', 'area_acres')) %>% 
  #select(c(-INSPECTED, -ACCESSIBILITY, -DRYSAMPLED)) %>%
  filter(FACILITYID != 'L01-727-1 (L01S04)' | MonitoringYear != 'MY2020-21') %>%
  right_join(., MajorOutfalls_avQ)

saveRDS(DischargePointTrib_flow, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DischargePointTrib_flow.rds'))
write_csv(DischargePointTrib_flow, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DischargePointTrib_flow.csv'))

AnnualCF_J_QC  <- DischargePointTrib_flow %>%
  filter(!is.na(AreaJ)) %>%
  #remove dry outfalls
  select('JURISDICTI3', 'MonitoringYear', 'AnnualCF', 'AreaJ') %>%
  unique() 

saveRDS(AnnualCF_J_QC, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/AnnualCF_J_QC.rds'))
write_csv(AnnualCF_J_QC, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/AnnualCF_J_QC.csv'))

AnnualCF_J  <- DischargePointTrib_flow %>%
  select('JURISDICTI3', 'MonitoringYear', 'AnnualCF', 'AreaJ') %>%
  unique() %>%
  group_by(JURISDICTI3, MonitoringYear) %>%
  mutate(DischargeJ_cf=sum(AnnualCF)) %>%
  mutate(DischargeJ_af=DischargeJ_cf/43560) %>%  #convert to acre-ft; 1 acre-feet = 43560 cf
  select('JURISDICTI3', 'MonitoringYear', 'DischargeJ_cf', 'DischargeJ_af',  'AreaJ') %>%
  mutate(QperArea_cfperacre=DischargeJ_cf/AreaJ) %>%
  unique() %>%
  ungroup() %>%
  filter(!is.na(MonitoringYear)) %>%
  filter(!is.na(AreaJ))
#discharge per cubic acre without connectivity adjustment 
saveRDS(AnnualCF_J, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/AnnualCF_J.rds'))
write_csv(AnnualCF_J, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/AnnualCF_J.csv'))

