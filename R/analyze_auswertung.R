###Point Data###

library("tidyverse")
library("xml2")
library("stringr")

Auswertung <- readRDS("Auswertung.rds")

Auswertung$xml <- Auswertung$kml %>% 
  map(function(x) read_xml(x, as_html = TRUE, options = "NOBLANKS"))

temporary <- Auswertung$xml %>% 
  map(function(x) xml_find_all(x, "//coordinates"))

Auswertung$coordinates <- temporary %>% 
  map(function(x) xml_text(x, trim = FALSE)) 
      

Auswertung<-Auswertung%>%unnest(coordinates)
Auswertung$coords<- trimws(Auswertung$coordinates, which = c("both"))



Auswertung$point <- word(Auswertung$coords, 1:2)
Auswertung<-Auswertung %>% separate(point, into = c("lon", "lat"), sep = ",")
AuswertungII<-Auswertung%>%filter(!is.na(lon))
AuswertungII<- st_as_sf(AuswertungII, coords = c("lon","lat"), crs = 4326)
AuswertungIII<-AuswertungII[,c(5:8)]      
