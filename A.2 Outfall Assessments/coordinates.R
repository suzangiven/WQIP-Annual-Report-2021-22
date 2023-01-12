library(dplyr)
library(tidyr)
library(readxl)
library(geojsonio)
library(tibble)

library(arcgisbinding)
arc.check_product()
#install.packages("arcgisbinding", repos="https://r.esri.com", type="win.binary")


  
#Working files?  C:\Users\givens\Documents\R    C:\Program Files\R\R-3.6.2    C:\Users\givens\Documents\R\R-3.6.2     H:\R\R-3.6.2
getwd()
setwd("H:/Outfall2020")
setwd("C:/Users/givens/Documents")
 
#https://data-ocpw.opendata.arcgis.com/datasets/outfall-locations   #https://www.rdocumentation.org/packages/geojsonio/versions/0.7.0/topics/geojson_read
 
url_outfalls<-"https://opendata.arcgis.com/datasets/780b8317d3e144b4bfaeed3f2accad6e_0.geojson"

url_Outfall_Inspect<-"C:/Users/givens/OneDrive - County of Orange/Shared with Everyone/FieldScreeningObs_MY201920.xlsx"

url_Outfall_Coord<-"C:/Users/givens/OneDrive - County of Orange/Shared with Everyone/SOCWMA_outfall coordinates_2020.xlsx"
Outfall_Coord<-read_excel(url_Outfall_Coord, 'coordinates')

Outfall_Coord<-Outfall_Coord %>%
  select(-c(Accessibility, Accessibility3, 'Watershed Management Area', 'Flow Measurement Count')) %>%
  distinct()

Outfall_Coord_AV<-Outfall_Coord %>%
  filter(Jurisdiction == 'Aliso Viejo') %>% 
  select(-Jurisdiction)
write.csv(Outfall_Inspect_AV, "H:/Outfall2020/Outfall_Inspect_AV.csv", na="")


Outfall_Coord_DP<-Outfall_Coord %>%
  filter(Jurisdiction == 'Dana Point') %>%
  select(-Jurisdiction)
write.csv(Outfall_Coord_DP, "H:/Outfall2020/Outfall_Coord_DP.csv", na="")

Outfall_Coord_LB<-Outfall_Coord %>%
  filter(Jurisdiction == 'Laguna Beach')%>%
  select(-Jurisdiction)
write.csv(Outfall_Coord_LB, "H:/Outfall2020/Outfall_Coord_LB.csv", na="")

Outfall_Coord_LH<-Outfall_Coord %>%
  filter(Jurisdiction == 'Laguna Hills') %>%
  select(-Jurisdiction)
write.csv(Outfall_Coord_LH, "H:/Outfall2020/Outfall_Coord_LH.csv", na="")

Outfall_Coord_LN<-Outfall_Coord %>%
  filter(Jurisdiction == 'Laguna Niguel') %>%
  select(-Jurisdiction)
write.csv(Outfall_Coord_LN, "H:/Outfall2020/Outfall_Coord_LN.csv", na="")

Outfall_Coord_LW<-Outfall_Coord %>%
  filter(Jurisdiction == 'Laguna Woods') %>%
  select(-Jurisdiction)
write.csv(Outfall_Coord_LW, "H:/Outfall2020/Outfall_Coord_LW.csv", na="")

Outfall_Coord_LF<-Outfall_Coord %>%
  filter(Jurisdiction == 'Lake Forest') %>%
  select(-Jurisdiction)
write.csv(Outfall_Coord_LF, "H:/Outfall2020/Outfall_Coord_LF.csv", na="")

Outfall_Coord_MV<-Outfall_Coord %>%
  filter(Jurisdiction == 'Mission Viejo') %>%
  select(-Jurisdiction)
write.csv(Outfall_Coord_MV, "H:/Outfall2020/Outfall_Coord_MV.csv", na="")

Outfall_Coord_OC<-Outfall_Coord %>%
  filter(Jurisdiction == 'Orange Co') %>%
  select(-Jurisdiction)
write.csv(Outfall_Coord_OC, "H:/Outfall2020/Outfall_Coord_OC.csv", na="")

Outfall_Coord_RSM<-Outfall_Coord %>%
  filter(Jurisdiction == 'Rancho Santa Margarita') %>%
  select(-Jurisdiction)
write.csv(Outfall_Coord_RSM, "H:/Outfall2020/Outfall_Coord_RSM.csv", na="")

Outfall_Coord_SJC<-Outfall_Coord %>%
  filter(Jurisdiction == 'San Juan Capistrano') %>%
  select(-Jurisdiction)
write.csv(Outfall_Coord_SJC, "H:/Outfall2020/Outfall_Coord_SJC.csv", na="")

Outfall_Coord_SC<-Outfall_Coord %>%
  filter(Jurisdiction == 'San Clemente') %>%
  select(-Jurisdiction)
write.csv(Outfall_Coord_SC, "H:/Outfall2020/Outfall_Coord_SC.csv", na="")




Outfall_Inspect<-read_excel(url_Outfall_Inspect, 'Sheet1', skip=2)


Outfall_Inspect_c<-add_column(Outfall_Inspect, "NALExceedance (sampled outfall)", .after="Jurisdiction")
Outfall_Inspect_c<-add_column(Outfall_Inspect_c, "Date Jurisdictional Manager Contacted", .after="COMMENTS")
Outfall_Inspect_c<-Outfall_Inspect_c[order(as.Date(Outfall_Inspect_c$'Inspection Date', format="%Y-%m-%d")), ]



Outfall_Inspect_AV<-Outfall_Inspect_c %>%
  filter(Jurisdiction == 'Aliso Viejo') %>% 
  select(-Jurisdiction)
  write.csv(Outfall_Inspect_AV, "H:/Outfall2020/Outfall_Inspect_AV.csv", na="")
  

  
Outfall_Inspect_DP<-Outfall_Inspect_c %>%
    filter(Jurisdiction == 'Dana Point') %>%
    select(-Jurisdiction)
    write.csv(Outfall_Inspect_DP, "H:/Outfall2020/Outfall_Inspect_DP.csv", na="")
  
Outfall_Inspect_LB<-Outfall_Inspect_c %>%
    filter(Jurisdiction == 'Laguna Beach')%>%
    select(-Jurisdiction)
    write.csv(Outfall_Inspect_LB, "H:/Outfall2020/Outfall_Inspect_LB.csv", na="")
    
Outfall_Inspect_LH<-Outfall_Inspect_c %>%
      filter(Jurisdiction == 'Laguna Hills') %>%
      select(-Jurisdiction)
      write.csv(Outfall_Inspect_LH, "H:/Outfall2020/Outfall_Inspect_LH.csv", na="")
      
Outfall_Inspect_LN<-Outfall_Inspect_c %>%
        filter(Jurisdiction == 'Laguna Niguel') %>%
        select(-Jurisdiction)
        write.csv(Outfall_Inspect_LN, "H:/Outfall2020/Outfall_Inspect_LN.csv", na="")
        
Outfall_Inspect_LW<-Outfall_Inspect_c %>%
        filter(Jurisdiction == 'Laguna Woods') %>%
        select(-Jurisdiction)
        write.csv(Outfall_Inspect_LW, "H:/Outfall2020/Outfall_Inspect_LW.csv", na="")
        
Outfall_Inspect_LF<-Outfall_Inspect_c %>%
        filter(Jurisdiction == 'Lake Forest') %>%
        select(-Jurisdiction)
        write.csv(Outfall_Inspect_LF, "H:/Outfall2020/Outfall_Inspect_LF.csv", na="")
        
Outfall_Inspect_MV<-Outfall_Inspect_c %>%
        filter(Jurisdiction == 'Mission Viejo') %>%
        select(-Jurisdiction)
        write.csv(Outfall_Inspect_MV, "H:/Outfall2020/Outfall_Inspect_MV.csv", na="")
        
Outfall_Inspect_OC<-Outfall_Inspect_c %>%
        filter(Jurisdiction == 'Orange Co') %>%
        select(-Jurisdiction)
        write.csv(Outfall_Inspect_OC, "H:/Outfall2020/Outfall_Inspect_OC.csv", na="")
        
Outfall_Inspect_RSM<-Outfall_Inspect_c %>%
        filter(Jurisdiction == 'Rancho Santa Margarita') %>%
       select(-Jurisdiction)
        write.csv(Outfall_Inspect_RSM, "H:/Outfall2020/Outfall_Inspect_RSM.csv", na="")
        
Outfall_Inspect_SJC<-Outfall_Inspect_c %>%
        filter(Jurisdiction == 'San Juan Capistrano') %>%
       select(-Jurisdiction)
        write.csv(Outfall_Inspect_SJC, "H:/Outfall2020/Outfall_Inspect_SJC.csv", na="")
        
Outfall_Inspect_SC<-Outfall_Inspect_c %>%
        filter(Jurisdiction == 'San Clemente') %>%
        select(-Jurisdiction)
        write.csv(Outfall_Inspect_SC, "H:/Outfall2020/Outfall_Inspect_SC.csv", na="")


  
Outfalls_SOCWMA<-arc.open('S:/Environmental Res/Environmental Studies Unit/Projects/Project Templates/SPOCDSQL1205.sde/OCEnvRes.OCENVRESUSER.swDischargePoint') %>%
          arc.select() %>%
          tibble::as_tibble()



Outfalls_SOCWMA<-geojson_read(url_outfalls, what="sp") %>% #Outfall domains are numeric - prefer to have descriptions
  as.data.frame() %>%
  filter(MANAGEMENT == "SOUTH") 

Outfalls_AV<-Outfalls_SOCWMA %>%
  filter(JURISDICTI == 'ALISO VIEJO') %>%
  select(-c('OBJECTID', 'GlobalID'))

write.csv(Outfalls_AV, "H:/Outfall2020/Outfalls_AV2020.csv")  
Outfalls_DP<-Outfalls_SOCWMA %>%
  filter(JURISDICTI == 'DANA POINT') %>%
  select(-c('OBJECTID', 'GlobalID'))

write.csv(Outfalls_DP, "H:/Outfall2020/Outfalls_DP2020.csv")




https://opendata.arcgis.com/datasets/780b8317d3e144b4bfaeed3f2accad6e_0.geojson

file_url <- "C:/Users/givens/OneDrive - County of Orange/Shared with Everyone/SOCWMA_outfall coordinates_2020.xlsx"

coordinates<-read_excel(file_url, 'coordinates')

coordinates_distinct<-coordinates %>%
  select(c(-"Accessibility")) %>%
  distinct()

coordinates_AV<-coordinates_distinct %>%
  filter(Jurisdiction=="Aliso Viejo")

write.csv(coordinates_AV, "H:/Outfall2020/coordinates_AV.csv")

#  https://stackoverflow.com/questions/7813141/how-to-create-a-kml-file-using-r
# https://dmbeskow.github.io/html/geo4.html

library(ggmap)
library(maptools)
library(rgdal)
library(raster)
library(plotKML)
library(rworldmap)

coordinates(coordinates_AV)<-c("POINT_X", "POINT_Y")
proj4string(coordinates_AV)<-CRS("+proj=longlat +datum=WGS84")   #https://stackoverflow.com/questions/7813141/how-to-create-a-kml-file-using-r

writeOGR(coordinates_AV, dsn="coordinates_AV.kml", layer="Facility Identifier", driver="KML")
 #doesn't include ID in the kmz file (appears as 'no name')

coordinates_AV_ll <- spTransform(coordinates_AV, CRS("+proj=longlat +datum=WGS84"))
writeOGR(coordinates_AV_ll["Facility Identifier"], dsn="coordinates_AV_ll.kml", layer="Facility Identifier", driver="KML")


library(mapview)
##https://www.color-hex.com/   https://rgbcolorcode.com/
plotKML::kml(coordinates_AV,
             file.name    = "coordinates_AV.kml",
             points_names = coordinates_AV$'Facility Identifier',
             colour    = "#FFFF00",
             alpha     = 0.6,
             size      = .7,
             shape     = "http://maps.google.com/mapfiles/kml/pal2/icon18.png")


mapView(coordinates_AV)

#  https://rdrr.io/cran/plotKML/src/R/attributes.R





library(leaflet)

test<-leaflet(coordinates_distinct) %>%
  addTiles() %>%
  addCircleMarkers(lng = ~POINT_X, lat = ~POINT_Y)


r_birthplace_map <- leaflet() %>%
  addTiles() %>%  # use the default base map which is OpenStreetMap tiles
  addMarkers(lng=174.768, lat=-36.852,
             popup="The birthplace of R")
r_birthplace_map
