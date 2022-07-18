##FixMyBerlin API##

library("rjson")
json_file <- "https://fixmyberlin.de/api/v1/projects"
json_data <- fromJSON(paste(readLines(json_file), collapse=""))

json_data<-as.vector(json_data)

jj<-rbind(json_data)%>%as.data.frame()

jj<-jj%>%unnest(results)

jj<-jj%>%unnest_wider(results)

jj<-jj%>%unnest_wider(geometry)

jj<-jj%>%rowwise%>%unnest_wider(center,names_repair = "unique")

jj<-jj%>%unnest(coordinates...18)

jj<-jj%>%unnest(photos)%>%unnest(photos)

jj<-jj%>%unnest(coordinates...20)


jj<-jj %>%
  dplyr::group_by(grp = str_c('coordinates', rep(1:2, length.out = n()))) %>%
  dplyr::mutate(rn = row_number()) %>%
  ungroup %>%
  pivot_wider(names_from = grp, values_from = coordinates...20) %>%
  select(-rn)

jj<-st_as_sf(x = jj,coords = c("coordinates1","coordinates2"),crs = 4326)

##Infravelo API##

library(xml2)
json_file <- "https://www.infravelo.de/api/v1/projects/"
Auswertung <- fromJSON(paste(readLines(json_file), collapse=""))

Auswertung<-as.vector(Auswertung)

Auswertung<-rbind(Auswertung)%>%as.data.frame()%>%unnest(results)%>%unnest_wider(results)

###InfraVelo API###
library("rjson")
library("data.table")
library("xml2")
library(stringr)

##load file unnest columns##
json_file <- "https://www.infravelo.de/api/v1/projects/"
Auswertung <- fromJSON(paste(readLines(json_file), collapse="", na.strings=c("","NA")))
Auswertung<-as.data.table(Auswertung)%>%unnest_wider(results)
Auswertung<-Auswertung%>%
  unnest(types)%>%
  unnest_wider(types)%>%
  unnest(metrics)%>%
  unnest_wider(metrics, names_repair = "unique")%>%
  unnest(districts)%>%
  unnest_wider(districts)%>%
  unnest(categories)%>%
  unnest(categories)%>%
  unnest(milestones)%>%
  unnest_wider(milestones, names_repair = "unique")%>%
  unnest(imagesBefore)%>%
  unnest_wider(imagesBefore)%>%
  unnest_wider(image, names_repair = "unique")
  
##unnest xml column##
Auswertung$xml <- Auswertung$kml %>% 
  map(function(x) read_xml(x, as_html = TRUE, options = "NOBLANKS"))

temporary <- Auswertung$xml %>% 
  map(function(x) xml_find_all(x, "//coordinates"))

Auswertung$coordinates <- temporary %>% 
  map(function(x) xml_text(x, trim = FALSE))

Auswertung<-Auswertung%>%unnest(coordinates)
Auswertung$coords<- trimws(Auswertung$coordinates, which = c("both"))

##isolate longitude/latitude data from xml column##
Auswertung<-Auswertung %>%
  mutate(koordinaten = strsplit(as.character(coords), ",")) %>% 
  unnest(koordinaten)

Auswertung$koord<-str_replace(Auswertung$koordinaten, "^0+" ,"")
#Auswertung$koord<-as.numeric(Auswertung$koord)

library(tidyr)

Auswertung<-Auswertung %>% mutate_all(na_if,"")
Auswertung<-Auswertung%>%drop_na(koord)


aa<-data.frame(title= Auswertung$title[c(FALSE, TRUE)],
               coord1 = Auswertung$koord[c(TRUE, FALSE)], 
               coord2 = Auswertung$koord[c(FALSE, TRUE)])


at<-aa%>%dplyr::arrange(coord1,coord2)%>%group_by(title)%>%sf::st_as_sf(coords = c("coord1","coord2"))%>%
  sf::st_set_crs(4326)


