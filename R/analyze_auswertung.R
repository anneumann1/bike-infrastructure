library("tidyverse")
library("xml2")

filename <- file.choose("Auswertung.rds")
auswertung <- readRDS(filename)

auswertung$xml <- auswertung$kml %>% 
  map(function(x) read_xml(x, as_html = TRUE, options = "NOBLANKS"))

temporary <- auswertung$xml %>% 
  map(function(x) xml_find_all(x, "//coordinates"))

auswertung$coordinates <- temporary %>% 
  map(function(x) xml_text(x, trim = FALSE)) 
      

Auswertung<-Auswertung%>%unnest(coordinates)
Auswertung$coords<- trimws(Auswertung$coordinates, which = c("both"))

library(stringr)

Auswertung$point <- word(Auswertung$coords, 1:2)
Auswertung<-Auswertung %>% separate(point, into = c("lon", "lat"), sep = ",")
AuswertungII<-Auswertung%>%filter(!is.na(lon))
AuswertungII<- st_as_sf(AuswertungII, coords = c("lon","lat"), crs = 4326)
AuswertungIII<-AuswertungII[,c(5:8)]      
