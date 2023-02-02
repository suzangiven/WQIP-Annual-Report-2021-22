#Find medians first then determine annual flow volume
#continuous  #change name from monthly to annual

values <- list() 
values[['DailyQ_TM_d']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DailyQ_TM_d.rds'
DailyQ_TM_d <-  readRDS(values[["DailyQ_TM_d"]])

values <- list() 
values[['DailyQ_2018R_d']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DailyQ_2018R_d.rds'
DailyQ_2018R_d <-  readRDS(values[["DailyQ_2018R_d"]])

values <- list()
values[['DailyQ_2020OFP_d']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DailyQ_2020OFP_d.rds'
DailyQ_2020OFP_d <-  readRDS(values[["DailyQ_2020OFP_d"]])

values[['DailyQ_OCFS_2021_d']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DailyQ_OCFS_2021_d.rds'
DailyQ_OCFS_2021_d <-  readRDS(values[["DailyQ_OCFS_2021_d"]])

values[['DailyQ_SWN_2022_d']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DailyQ_SWN_2022_d.rds'
DailyQ_SWN_2022_d <-  readRDS(values[["DailyQ_SWN_2022_d"]]) 

values[['DailyQ_DFM_2022_d']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DailyQ_DFM_2022_d.rds'
DailyQ_DFM_2022_d <-  readRDS(values[["DailyQ_DFM_2022_d"]]) 


#2015-16
AnnualQ201516TM <- DailyQ_TM_d %>%  
  filter(date > "2015-09-30" & date < "2016-10-01") %>%
  group_by(Station) %>%
  as_tibble() %>%
  group_by(Station) %>%
  summarise(medianTMcfs = median(Flow_cfs_md)) %>%
  mutate(MY = 'MY2015-16') %>%
  ungroup()


AnnualQ201516R <- DailyQ_2018R_d %>%  
  filter(date > "2015-09-30" & date < "2016-10-01") %>%
  group_by(Station) %>%
  as_tibble() %>%
  group_by(Station) %>%
  summarise(medianRcfs = median(Flow_cfs_md)) %>%
  mutate(MY = 'MY2015-16') %>%
  ungroup()

AnnualQ201516OFP <- DailyQ_2020OFP_d %>%  
  filter(date > "2015-09-30" & date < "2016-10-01") %>%
  group_by(Station) %>%
  as_tibble() %>%
  group_by(Station) %>%
  summarise(medianOFPcfs = median(Flow_cfs_md)) %>%
  mutate(MY = 'MY2015-16') %>%
  ungroup()

AnnualQ201516<-full_join(AnnualQ201516TM, AnnualQ201516R,by=c('Station','MY')) %>%
  full_join(.,AnnualQ201516OFP, by=c('Station','MY'))

#2016-17
AnnualQ201617TM <- DailyQ_TM_d %>%  
  filter(date > "2016-09-30" & date < "2017-10-01") %>%
  group_by(Station) %>%
  as_tibble() %>%
  group_by(Station) %>%
  summarise(medianTMcfs = median(Flow_cfs_md)) %>%
  mutate(MY = 'MY2016-17') %>%
  ungroup()


AnnualQ201617R <- DailyQ_2018R_d %>%  
  filter(date > "2016-09-30" & date < "2017-10-01") %>%
  group_by(Station) %>%
  as_tibble() %>%
  group_by(Station) %>%
  summarise(medianRcfs = median(Flow_cfs_md))  %>%
  mutate(MY = 'MY2016-17') %>%
  ungroup()

AnnualQ201617OFP <- DailyQ_2020OFP_d %>%  
  filter(date > "2016-09-30" & date < "2017-10-01") %>%
  group_by(Station) %>%
  as_tibble() %>%
  group_by(Station) %>%
  summarise(medianOFPcfs = median(Flow_cfs_md))  %>%
  mutate(MY = 'MY2016-17') %>%
  ungroup()

AnnualQ201617<-full_join(AnnualQ201617TM, AnnualQ201617R, by=c('Station','MY')) %>%
full_join(.,AnnualQ201617OFP, by=c('Station','MY'))

#2017-18
AnnualQ201718TM <- DailyQ_TM_d %>%  
  filter(date > "2017-09-30" & date < "2018-10-01") %>%
  group_by(Station) %>%
  as_tibble() %>%
  group_by(Station) %>%
  summarise(medianTMcfs = median(Flow_cfs_md)) %>%
  mutate(MY = 'MY2017-18') %>%
  ungroup()

AnnualQ201718R <- DailyQ_2018R_d %>%  
  filter(date > "2017-09-30" & date < "2018-10-01") %>%
  group_by(Station) %>%
  as_tibble() %>%
  group_by(Station) %>%
  summarise(medianRcfs = median(Flow_cfs_md)) %>%
  mutate(MY = 'MY2017-18') %>%
  ungroup()

AnnualQ201718OFP <- DailyQ_2020OFP_d %>%  
  filter(date > "2017-09-30" & date < "2018-10-01") %>%
  group_by(Station) %>%
  as_tibble() %>%
  group_by(Station) %>%
  summarise(medianOFPcfs = median(Flow_cfs_md)) %>%
  mutate(MY = 'MY2017-18') %>%
  ungroup()

AnnualQ201718<-full_join(AnnualQ201718TM, AnnualQ201718R, by=c('Station','MY')) %>%
full_join(., AnnualQ201718OFP, by=c('Station','MY'))

#2018-19

AnnualQ201819TM <- DailyQ_TM_d %>%  
  filter(date > "2018-09-30" & date < "2019-10-01") %>%
  group_by(Station) %>%
  as_tibble() %>%
  group_by(Station) %>%
  summarise(medianTM = median(Flow_cfs_md)) %>%
  mutate(MY = 'MY2018-19') %>%
  ungroup()

AnnualQ201819R <- DailyQ_2018R_d %>%  
  filter(date > "2018-09-30" & date < "2019-10-01") %>%
  group_by(Station) %>%
  as_tibble() %>%
  group_by(Station) %>%
  summarise(medianRcfs = median(Flow_cfs_md)) %>%
  mutate(MY = 'MY2018-19') %>%
  ungroup()

AnnualQ201819OFP <- DailyQ_2020OFP_d %>%  
  filter(date > "2018-09-30" & date < "2019-10-01") %>%
  group_by(Station) %>%
  as_tibble() %>%
  group_by(Station) %>%
  summarise(medianOFPcfs = median(Flow_cfs_md)) %>%
  mutate(MY = 'MY2018-19') %>%
  ungroup()

AnnualQ201819<-full_join(AnnualQ201819TM, AnnualQ201819R, by=c('Station','MY')) %>%
full_join(.,AnnualQ201819OFP, by=c('Station','MY'))

#2019-20

AnnualQ201920OFP <- DailyQ_2020OFP_d %>%  
  filter(date > "2019-09-30" & date < "2020-10-01") %>%
  group_by(Station) %>%
  as_tibble() %>%
  group_by(Station) %>%
  summarise(medianOFPcfs = median(Flow_cfs_md)) %>%
  mutate(MY = 'MY2019-20') %>%
  ungroup()

AnnualQ201920<-AnnualQ201920OFP

#2020-21

AnnualQ20202021O <- DailyQ_OCFS_2021_d %>%  
  filter(date > "2020-09-30" & date < "2021-10-01") %>%
  group_by(Station) %>%
  as_tibble() %>%
  group_by(Station) %>%
  summarise(medianOCFS2021cfs = median(Flow_cfs_md)) %>%
  mutate(MY = 'MY2020-21') %>%
  ungroup()

AnnualQ202021<-AnnualQ20202021O

#2021-22

AnnualQ20212022S <- DailyQ_SWN_2022_d %>%  
  filter(date > "2021-09-30" & date < "2022-10-01") %>%
  group_by(Station) %>%
  as_tibble() %>%
  group_by(Station) %>%
  summarise(medianSWN2022cfs = median(Flow_cfs_md)) %>%
  mutate(MY = 'MY2021-22') %>%
  ungroup()

AnnualQ20212022dfm <- DailyQ_DFM_2022_d %>%  
  filter(date > "2021-09-30" & date < "2022-10-01") %>%
  group_by(Station) %>%
  as_tibble() %>%
  group_by(Station) %>%
  summarise(medianDFM2022cfs = median(Flow_cfs_md)) %>%
  mutate(MY = 'MY2021-22') %>%
  ungroup()


AnnualQ202122 <-full_join(AnnualQ20212022dfm,AnnualQ20212022S, by=c('Station','MY'))


MedianQ_cont<-bind_rows(AnnualQ201516,AnnualQ201617,AnnualQ201718,AnnualQ201819,AnnualQ201920,AnnualQ202021, AnnualQ202122) 


##TO DO:  make MonitoringYear consistent colnames(MedianQ_cont)[colnames(MedianQ_cont) == "MY"] <- "MonitoringYear"

saveRDS(MedianQ_cont, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/MedianQ_cont.rds'))
write_csv(MedianQ_cont, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/MedianQ_cont.csv'))

#compute annual flow from continuous  (see line 161 - DryDaysYearGroup)
values <- list()  
values[['MedianQ_cont']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/MedianQ_cont.rds'
MedianQ_cont <-  readRDS(values[["MedianQ_cont"]])


#update station names 


values <- list() 
values[['FieldScreen_dry']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/FieldScreen_dry.rds'
InstQ <-  readRDS(values[["FieldScreen_dry"]]) #line 370

str(InstQ)

InstQ <- InstQ %>%  #flowing only
  
  filter(!is.na(QInscfs))  #remove outfalls without flow measurements (put back in later with all flow measurements)

#Define Monitoring Year
InstQ$MonitoringYear<- ifelse(InstQ$Date>(as.Date("2021-09-30", origin="1900-01-01")) & InstQ$Date<(as.Date("2022-10-01", origin="1900-01-01")), "MY2021-22",
  ifelse(InstQ$Date>(as.Date("2020-09-30", origin="1900-01-01")) & InstQ$Date<(as.Date("2021-10-01", origin="1900-01-01")), "MY2020-21",
                              ifelse(InstQ$Date>(as.Date("2019-09-30", origin="1900-01-01")) & InstQ$Date<(as.Date("2020-10-01", origin="1900-01-01")), "MY2019-20",
                                     ifelse(InstQ$Date>(as.Date("2018-09-30", origin="1900-01-01")) & InstQ$Date<(as.Date("2019-10-01", origin="1900-01-01")), "MY2018-19",
                                            ifelse(InstQ$Date>(as.Date("2017-09-30", origin="1900-01-01")) & InstQ$Date<(as.Date("2018-10-01", origin="1900-01-01")), "MY2017-18",
                                                   ifelse(InstQ$Date>(as.Date("2016-09-30", origin="1900-01-01")) & InstQ$Date<(as.Date("2017-10-01", origin="1900-01-01")), "MY2016-17",
                                                          ifelse(InstQ$Date>(as.Date("2015-09-30", origin="1900-01-01")) & InstQ$Date<(as.Date("2016-10-01", origin="1900-01-01")), "MY2015-16",
                                                                 ifelse(InstQ$Date>(as.Date("2014-09-30", origin="1900-01-01")) & InstQ$Date<(as.Date("2015-10-01", origin="1900-01-01")), "MY2014-15",  
                                                                        NA))))))))



InstQ <- InstQ %>%

mutate(QInscfsAdj=QInscfs*0.69) %>%
  mutate(QInsCFAdj=QInscfsAdj*60*60*24)

colnames(InstQ)[colnames(InstQ) == "FacilityID"] <- "FACILITYID"

saveRDS(InstQ, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/InstQ.rds'))
write_csv(InstQ, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/InstQ.csv')) #annual flow instantenous flow



## Task 7.1: (Only) continuous data
#list of outfalls with (only) continuous data
#There are not any outfalls with only continuous flow data
values <- list()  
values[['MedianQ_cont']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/MedianQ_cont.rds'

MedianQ_cont<-readRDS(values[["MedianQ_cont"]]) %>% 
  mutate(QDailycfContTMcf=medianTMcfs*60*60*24) %>%
  mutate(QDailycfContRcf=medianRcfs*60*60*24) %>%
  mutate(QDailycfContOFPcf=medianOFPcfs*60*60*24) %>%
  mutate(QDailycfContOCFScf=medianOCFS2021cfs*60*60*24) %>%
  mutate(QDailycfContSWNcf=medianSWN2022cfs*60*60*24)

colnames(MedianQ_cont)[colnames(MedianQ_cont) == "Station"] <- "FACILITYID"

colnames(MedianQ_cont)[colnames(MedianQ_cont) == "MY"] <- "MonitoringYear"



## Both continuous and instantaneous data (and flow prioritization files)


DailyAvgCF <- full_join(InstQ, MedianQ_cont, by=c('FacilityID'="FACILITYID", "MonitoringYear")) 
  


saveRDS(DailyAvgCF, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DailyAvgCF.rds'))
write_csv(DailyAvgCF, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DailyAvgCF.csv')) 

#AnnualCF$QAnnualcfContOFPcf <- replace(AnnualCF$QAnnualcfContOFPcf,AnnualCF$QAnnualcfContOFPcf == 0.0, NA)  

## Task 7.4*: No flow measurements
#ponded  (should this include un-verified and not found outfalls?)


values <- list()  
values[['FieldScreen_dry']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/FieldScreen_dry.rds'
NoQ <-  readRDS(values[["FieldScreen_dry"]]) %>%
  filter(Date > '2021-09-30' & Date <'2022-10-01' ) %>%
  filter(is.na(QInscfs)) %>%
  as_tibble() 

NoQ$MonitoringYear<- 
  ifelse(NoQ$Date>(as.Date("2021-09-30", origin="1900-01-01")) & NoQ$Date<(as.Date("2022-10-01", origin="1900-01-01")), "MY2021-22",
                NA)
 

NoQ$MonitoringYear<- 
  ifelse(NoQ_f$Date>(as.Date("2019-09-30", origin="1900-01-01")) & NoQ_f$Date<(as.Date("2020-10-01", origin="1900-01-01")), "MY2019-20",
         ifelse(NoQ_f$Date>(as.Date("2018-09-30", origin="1900-01-01")) & NoQ_f$Date<(as.Date("2019-10-01", origin="1900-01-01")), "MY2018-19",
                ifelse(NoQ_f$Date>(as.Date("2020-09-30", origin="1900-01-01")) & NoQ_f$Date<(as.Date("2021-10-01", origin="1900-01-01")), "MY2020-21",  
                       ifelse(NoQ_f$Date>(as.Date("2021-09-30", origin="1900-01-01")) & NoQ_f$Date<(as.Date("2022-10-01", origin="1900-01-01")), "MY2021-22",     
                              NA))))


NoQ$MonitoringYear <- as.character(NoQ$MonitoringYear)

str(NoQ)



saveRDS(NoQ, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/NoQ.rds'))
write_csv(NoQ, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/NoQ.csv'))
###Combine all daily flow files
#join in ponded outfalls

#first find average of previous measurements from outfall.  Then for remaining without any measurements, find jurisdictional average

values <- list()  
values[['MO_']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/MO_.rds'
MO_ <-  readRDS(values[["MO_"]]) 


values <- list()  
values[['NoQ']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/NoQ.rds'
NoQ_f <-  readRDS(values[["NoQ"]]) 

NoQ_fa <- NoQ_f %>%
  left_join(., MO_, by=c('FacilityID'='FACILITYID')) %>%
  filter(FlowCondition == 'Pooled or Ponded' & AVGDISCHARGE == 0) 

NoQ_fa['QInscfsAdj'] <-NA
NoQ_fa['QInsCFAdj'] <-NA

NoQ_fb <- NoQ_f %>%
  left_join(., MO_, by=c('FacilityID'='FACILITYID')) %>%
  filter(FlowCondition != 'Pooled or Ponded' & AVGDISCHARGE > 0) %>% 
  mutate(QInscfsAdj=AVGDISCHARGE*0.69) %>%
  mutate(QInsCFAdj=QInscfsAdj*60*60*24)

NoQ_f<-(bind_rows(NoQ_fa, NoQ_fb)) %>%
unique()

  #replace_na(list(Dischargecfs=-0.99)) %>%
  
  

colnames(NoQ_f)[colnames(NoQ_f) == "Dischargecfs"] <- "DischargeP"  #includes ponded AND flowing outfalls -sometimes flow measurements were not recorded

NoQ_f <-  NoQ_f %>%
  
  filter(MonitoringYear=='MY2020-21'|is.na(MonitoringYear)) %>% 
  replace_na(list(MonitoringYear= 'MY2020-21')) 



saveRDS(NoQ_f, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/NoQ_f.rds'))
write_csv(NoQ_f, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/NoQ_f.csv'))

## Combine Daily flow file and no flow files to estimate flow at outfalls without flow measurements
values <- list()  

values[['NoQ_f']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/NoQ_f.rds'
NoQ_f <-  readRDS(values[["NoQ_f"]]) %>%
  select(FacilityID, QInscfsAdj, QInsCFAdj) %>%
  unique()

str(DailyAvgCF)

values <- list() 
values[['DailyAvgCF']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DailyAvgCF.rds'
DailyAvgCF <-  readRDS(values[["DailyAvgCF"]]) %>%
  filter(MonitoringYear == 'MY2021-22'|is.na(MonitoringYear)) %>%
 ungroup() %>%
  select(FacilityID,QDailycfContTMcf, QDailycfContRcf,QDailycfContOFPcf, QDailycfContOCFScf, QDailycfContSWNcf, QInscfs, QInscfsAdj, QInsCFAdj) %>%
  group_by(FacilityID) %>%
  mutate(Q_DailyavgCF=mean(c(QDailycfContTMcf, QDailycfContRcf,QDailycfContOFPcf, QDailycfContOCFScf, QDailycfContSWNcf, QInsCFAdj), na.rm=TRUE)) %>%
  unique() %>%
  select(FacilityID, Q_DailyavgCF) %>%
  unique()
  
AnnualCF <-full_join(NoQ_f, DailyAvgCF, by=c('FacilityID'='FacilityID')) %>%
  right_join(., MO_, by=c('FacilityID'='FACILITYID')) %>%
   mutate(Q_DailyavgCF = ifelse(AVGDISCHARGE > 0 & is.na(Q_DailyavgCF), AVGDISCHARGE*60*60*24, Q_DailyavgCF))


saveRDS(AnnualCF, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/AnnualCF.rds'))
write_csv(AnnualCF, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/AnnualCF.csv'))

getwd()
#Estimate flow at outfalls without any measurements
values <- list()  
values[['DischargePointTrib_all']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DischargePointTrib_all.rds'
DischargePointTrib_all <-  readRDS(values[["DischargePointTrib_all"]]) %>%
  select(FACILITYID, JURISDICTI3, JURISDICTI, PERCENTAGE, area_acres, AREA) %>%
  unique()

DailyCFandPond_trib <- full_join(AnnualCF, DischargePointTrib_all,  by=c('FacilityID'='FACILITYID', 'JURISDICTI')) %>%
  filter(!is.na(JURISDICTI3))  #filters out non major outfalls


saveRDS(DailyCFandPond_trib, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DailyCFandPond_trib.rds'))
write_csv(DailyCFandPond_trib, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DailyCFandPond_trib.csv'))

#mutate(Q_DailyavgCF = ifelse(AVGDISCHARGE > 0 & is.na(Q_DailyavgCF), AVGDISCHARGE*60*60*24, Q_DailyavgCF))

#install.packages("psych")          # Install psych package
library("psych")
values <- list()  
values[['DailyCFandPond_trib']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DailyCFandPond_trib.rds'
DailyCFandPond_trib <-  readRDS(values[["DailyCFandPond_trib"]]) 


DailyCFandPond_trib2a <-  DailyCFandPond_trib %>%    #estimate flow at ponded outfalls and outfalls not visited from average daily (instead of annual)
  filter(JURISDICTI3 != "IRVINE") %>%
  filter(JURISDICTI3 != 'NEWPORT BEACH') %>%
  mutate(DischargePCF2= PERCENTAGE*Q_DailyavgCF/100, Q_DailyavgCF) %>%
  replace_na(list(Q_DailyavgCF=-0.99)) %>% 
  group_by(JURISDICTI3) %>%  #use geomean instead of log mean (July 27, 2021; 2019-20 Annual Report used arithmetic average)
  mutate(DischargePCF3 = ifelse(Q_DailyavgCF < 0 & is.na(DischargePCF2) , geometric.mean(DischargePCF2, na.rm=TRUE), DischargePCF2)) %>%   #only calculates for outfalls with no flow measurements, but column includes daily flow for outfalls with data
  #mutate(Q_DailyavgCF = ifelse(Q_DailyavgCF < 0, geometric.mean(Q_DailyavgCF, na.rm=TRUE), Q_DailyavgCF)) %>% 
  ungroup() %>%
# filter(!is.na(DischargePCF3)) 
  

#DailyCFandPond_trib2b <-  DailyCFandPond_trib %>%  #marks dry observations as 0, but keeps in record
  #select(FACILITYID, JURISDICTI, JURISDICTI3,date,FLOWCOND, DRYSAMPLED, PERSISTENTFLOW, DischargeP, Q_DailyavgCF, MonitoringYear, PERCENTAGE, AVGDISCHARGE, avgdailyQ, avgdailyQadj, FLOWCONNECTIVITY, area_acres, AREA) %>%
  #filter(FLOWCOND =='3') %>% 
  #mutate(DischargePCF = as.numeric(0)) %>%
  #mutate(DischargePCF2 = as.numeric(0)) %>%
  #mutate(DischargePCF3 = as.numeric(0)) 

#DailyCFandPond_trib2 <-rbind(DailyCFandPond_trib2a, DailyCFandPond_trib2b) 

saveRDS(DailyCFandPond_trib2a, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DailyCFandPond_trib2.rds'))
write_csv(DailyCFandPond_trib2a, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DailyCFandPond_trib2.csv'))

#Annual Flow volume - join in dry_days_file
values <- list()  
values[['DryDaysYearGroup']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DryDaysYearGroup.rds'
DryDaysYearGroup <-  readRDS(values[["DryDaysYearGroup"]]) %>%
  filter(!is.na(FACILITYID)) %>%
  filter(MonitoringYear=='MY2021-22')

values <- list()  
values[['DailyCFandPond_trib2']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DailyCFandPond_trib2.rds'
DailyCFandPond_trib2 <-  readRDS(values[["DailyCFandPond_trib2"]])


AnnualCF2b <- DailyCFandPond_trib2 %>%
  left_join(.,DryDaysYearGroup, by=c('FacilityID'='FACILITYID')) %>%  
  #filter(MonitoringYear=='MY2021-22') %>%
  group_by(FacilityID, JURISDICTI3) %>%
  mutate(AnnualCFb=mean(DischargePCF3)) %>%
  mutate(AnnualCF = dry_days*AnnualCFb)

saveRDS(AnnualCF2b, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/AnnualCF2b.rds'))
write_csv(AnnualCF2b, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/AnnualCF2b.csv'))


#Find annual flow volume by jurisdiction
#Remove outfalls after volumes determined
AnnualCF2 <- AnnualCF2b %>%  #remove outfalls upstream of most downstream outfall closest to receiving water
  filter(FacilityID != "J06-9362-1 (J06-P03)")  %>% #Dairy Fork
  filter(FacilityID != "J01-9082-5 (J02P08)") %>% #Wood Canyon
  filter(FacilityID != "L05-049-2" & FacilityID != "L05-049-1" & FacilityID != "L05-489-7" & FacilityID !="L05-489-3" & FacilityID !="L05-489-4") %>%  #Horno Basin
  filter(FacilityID != "J03-9234-8" & FacilityID != "J03-9234-6" & FacilityID !="J03-9234-5" & FacilityID !="K01-12032-2 (K01P11)") %>% #Niguel Storm Drain  
  filter(FacilityID != "L03-141-1" & FacilityID != "L03-141-3" & FacilityID != "L03-141-2" & FacilityID != "L03-172-2" & FacilityID != "L03-172-3" & FacilityID != "L03-073-3" & FacilityID != "L03-073-4" & FacilityID != "L03-073-5" & FacilityID != "L03-074-2" & FacilityID != "L03-074-1 (L03B01)") %>% #Oso Creek
  filter(FacilityID != "L04-672-1" & FacilityID != "L04-136-1u (L04P07)") %>%
  filter(FacilityID != "J03-9199-2" & FacilityID != "J03-9190-1" & FacilityID != "J03-9199-1") %>%
  filter(FacilityID != "K01-12156-6"   & FacilityID != "K01-12156-4") %>% #Salt Creek
  filter(FacilityID != "M02-052-3" & FacilityID != "M02-052-4" & FacilityID != "M02-032-1" & FacilityID != "M02-085-1 (M02P06)" & FacilityID != "M02-085-2" & FacilityID != "M02-013-1" & FacilityID != "M02-086-1" & FacilityID != "M02-015-1" & FacilityID != "M02-028-2 (M02P08)" & FacilityID != 'M02-061-7' & FacilityID != 'M02-102-1') %>%   #Segunda Deshecha Channel
  filter(FacilityID != "M01-008-1" & FacilityID != "M01-060-3" & FacilityID != "M01-124-4") %>%
  filter(FacilityID != "M00.1-070-6" & FacilityID != "M00.1-070-4" & FacilityID != "M00.1-070-3" & FacilityID != "M00.1-070-2" & FacilityID != 'M00.1-070-1' & FacilityID !=  "M00.1-071-1 (M00S04)" & FacilityID !=  "M00.1-071-4 (M00S04)" & FacilityID !=  "M00.1-071-3 (M00S04)") %>% #coastal SC
  filter(FacilityID != "I01-11503-3"  & FacilityID != "I01-11503-4" & FacilityID != "I01-11502-1" & FacilityID != "I01-11216-3" & FacilityID != "I01-11216-2 (I02P12)" & FacilityID != "I01-11216-1 (I02P13)" & FacilityID != "I01-11216-4 (I02P14)" & FacilityID != "I01-11217-1") %>%  #Laguna Canyon Wash
  filter(FacilityID != "L01-613-1" & FacilityID != "L01-728-7 (L01S03)")

saveRDS(AnnualCF2, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/AnnualCF2.rds'))
write_csv(AnnualCF2, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/AnnualCF2.csv'))

#make unique values for JURISDICTI3 and ANNUAL CF. 



values <- list()  
values[['AnnualCF2']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/AnnualCF2.rds'
AnnualCF2 <-  readRDS(values[["AnnualCF2"]])

values <- list()  
values[['SOCWMA_Cities']] <- 'A.2 Outfall Assessments/DryWeatherLoadingCalcs/Input/SOCWMA_Cities.rds'
SOCWMA_Cities <-  readRDS(values[["SOCWMA_Cities"]]) 



AnnualCF_J  <- AnnualCF2 %>%
  select('JURISDICTI3', 'AnnualCF') %>%
  full_join(., SOCWMA_Cities, by=c('JURISDICTI3'='jurisdicti')) %>%
  unique() %>%
  group_by(JURISDICTI3) %>%
  mutate(DischargeJ_cf=sum(AnnualCF)) %>%
  mutate(DischargeJ_af=DischargeJ_cf/43560) %>%  #convert to acre-ft; 1 acre-feet = 43560 cf
  select('JURISDICTI3','DischargeJ_cf', 'DischargeJ_af',  'acres_juris_soc') %>%
  mutate(QperArea_cfperacre=DischargeJ_cf/acres_juris_soc) %>%
  unique() %>%
  ungroup() 
  
  
  
#discharge per cubic acre without connectivity adjustment 
saveRDS(AnnualCF_J, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/AnnualCF_J.rds'))
write_csv(AnnualCF_J, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/AnnualCF_J.csv'))




AnnualCF_J_QC  <- DischargePointTrib_flow %>%
  filter(!is.na(AreaJ)) %>%
  #remove dry outfalls
  select('JURISDICTI3', 'MonitoringYear', 'AnnualCF', 'AreaJ') %>%
  unique() 

saveRDS(AnnualCF_J_QC, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/AnnualCF_J_QC.rds'))
write_csv(AnnualCF_J_QC, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/AnnualCF_J_QC.csv'))
