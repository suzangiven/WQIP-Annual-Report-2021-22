library(readr)

#https://rpubs.com/lokraj/github_csv

DailyRain = "https://raw.githubusercontent.com/South-OC-WMA/WQIP-Annual-Report-2019-20/3c4aa75386f2d95dbdfbd82b8f088133b3c8dc76/Input/Rain_SOCWMA_COOP_2015_2020.CSV?token=AELS54XF572XUQDRKKXKIE27X3QN4"

DailyRain2<-"https://raw.githubusercontent.com/South-OC-WMA/WQIP-Annual-Report-2019-20/3c4aa75386f2d95dbdfbd82b8f088133b3c8dc76/Input/Rain_SOCWMA_COOP_2015_2020.CSV"

DailyRain3<-read_csv(url(DailyRain2))
