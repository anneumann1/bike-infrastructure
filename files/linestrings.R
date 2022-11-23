library(xml2)
library(tidyverse)
library(sf)
library(rjson)

Auswertung <- readRDS("auswertung.rds")
Auswertung$xml <- Auswertung$kml %>% 
  map(function(x) read_xml(x, as_html = TRUE, options = "NOBLANKS"))

temporaryIII <- Auswertung$xml %>% map(function(x) xml_find_all(x, "//multigeometry"))                               ##Man nehme Multigeometry
Auswertung$line <- temporaryIII %>% map(function(x) xml_text(x, trim = FALSE))

Auswertung$line<-gsub('[\t\n]', '', Auswertung$line)                                                          
Auswertung$line<-sub('relativeToGround', '', Auswertung$line)                                                        
Auswertung$num<-length(grep("relativeToGround", Auswertung))                                                        ##zählen wie viele Linestrings sich in in der Spalte xml befinden: Es befinden sich max.2 Linestrings
Auswertung<-separate(data = Auswertung, col = line, into = c("eins", "zwei"), sep = "relativeToGround")             ##da mehrere Linestrings in der xml-Spalte:nochmals eine Spalte mit den zweiten Linestrings erstellen
Auswertung<-Auswertung%>%filter(!eins=="character(0)")                                                              ##2 abgeschlossene Projekte filtern

###Erste Linestrings###
tt<-Auswertung%>%select("eins","title")%>%as.data.frame()                                                           ##select Spalte eins->lat
at<-tt %>%                                                                                                          ##1 lange Spalte mit allen latitudes/longitudes
  mutate(lat = strsplit(as.character(tt$eins), ",")) %>%                                            
  unnest(lat)

at<-at%>%select("title","lat")%>%as.data.frame(matrix(at$lat, ncol = 2, byrow = TRUE))                             
at$lat<-sub("^0+", "", at$lat) 
at$lat<-as.numeric(at$lat)
at<-as.data.frame(at)
at<-at%>%dplyr::group_by(title)%>%dplyr::filter(!is.na(lat))                       
at<-at %>%                                                                                                          ##longitude in einer extra Spalte verlegen (benannt: lat2)
  dplyr::group_by(grp = str_c('lat', rep(1:2, length.out = n()))) %>%
  dplyr::mutate(rn = row_number()) %>%
  ungroup %>%
  pivot_wider(names_from = grp, values_from = lat) %>%
  select(-rn)


at<-at%>%sf::st_as_sf(coords = c("lat1","lat2"))%>%                                                                 ##Projiziere Latitude (lat1) und longitude (lat2)
  sf::st_set_crs(4326)

ka<-at%>%group_by(title)%>%dplyr::summarise(do_union=FALSE)%>%st_cast("LINESTRING")%>%                              ##kreire Linestrings->do_union=FALSE fürs sortieren
  sf::st_set_crs(4326)

mapview::mapview(ka)