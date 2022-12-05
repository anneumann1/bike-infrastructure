library(xml2)
library(tidyverse)
library(sf)
library(rjson)

v1 <- "https://www.infravelo.de/api/v1/projects/"
Auswertung <- fromJSON(paste(readLines(v1), collapse="", na.strings=c("","NA")))
Auswertung <-as.data.table(Auswertung)%>%
  unnest_wider(results)%>%
  unnest(districts)%>%
  unnest(districts)

Auswertung$xml <- Auswertung$kml %>% 
  map(function(x) read_xml(x, as_html = TRUE, options = "NOBLANKS"))

temporaryIII <- Auswertung$xml %>% map(function(x) xml_find_all(x, "//multigeometry"))                               
Auswertung$line <- temporaryIII %>% map(function(x) xml_text(x, trim = FALSE))

Auswertung$line<-gsub('[\t\n]', '', Auswertung$line)                                                          
Auswertung$line<-sub('relativeToGround', '', Auswertung$line)                                                        
Auswertung$num<-length(grep("relativeToGround", Auswertung))                                                         
Auswertung<-separate(data = Auswertung, col = line, into = c("eins", "zwei"), sep = "relativeToGround")              
Auswertung<-Auswertung%>%filter(!eins=="character(0)")                                                               

###Erste Linestrings###
tt<-Auswertung%>%select("eins","districts","subtitle","description","status","categories","title")%>%as.data.frame()                                                           
at<-tt %>%                                                                                                          
  mutate(lat = strsplit(as.character(tt$eins), ",")) %>%                                            
  unnest(lat)

at<-at%>%select("title","subtitle","districts","description","status","categories","lat")%>%as.data.frame(matrix(at$lat, ncol = 2, byrow = TRUE))                             
at$lat<-sub("^0+", "", at$lat) 
at$lat<-as.numeric(at$lat)
at<-as.data.frame(at)
at<-at%>%dplyr::group_by(title)%>%dplyr::filter(!is.na(lat))                       
at<-at %>%                                                                                                          
  dplyr::group_by(grp = str_c('lat', rep(1:2, length.out = n()))) %>%
  dplyr::mutate(rn = row_number()) %>%
  ungroup %>%
  pivot_wider(names_from = grp, values_from = lat) %>%
  select(-rn)


at<-at%>%sf::st_as_sf(coords = c("lat1","lat2"))%>%                                                                 
  sf::st_set_crs(4326)

ka<-at%>%group_by(title,districts,subtitle,description,status,categories)%>%
  dplyr::summarise(do_union=FALSE)%>%st_cast("LINESTRING")%>%                              
  sf::st_set_crs(4326)

#ka<-ka%>%  unnest(categories)%>%
#  unnest(categories)


mapview::mapview(ka["title"])

###2.Linestrings###

tt_2<-Auswertung%>%select("zwei","districts","subtitle","description","status","categories","title")%>%as.data.frame()                                                           
at_2<-tt_2 %>%                                                                                                          
  mutate(lat = strsplit(as.character(tt_2$zwei), ",")) %>%                                            
  unnest(lat)

at_2<-at_2%>%select("title","subtitle","districts","description","status","categories","lat")%>%as.data.frame(matrix(at_2$lat, ncol = 2, byrow = TRUE))                             
at_2$lat<-sub("^0+", "", at_2$lat) 
at_2$lat<-as.numeric(at_2$lat)
at_2<-as.data.frame(at_2)
at_2<-at_2%>%dplyr::group_by(title)%>%dplyr::filter(!is.na(lat))                       
at_2<-at_2 %>%                                                                                                          
  dplyr::group_by(grp = str_c('lat', rep(1:2, length.out = n()))) %>%
  dplyr::mutate(rn = row_number()) %>%
  ungroup %>%
  pivot_wider(names_from = grp, values_from = lat) %>%
  select(-rn)


at_2<-at_2%>%sf::st_as_sf(coords = c("lat1","lat2"))%>%                                                                 
  sf::st_set_crs(4326)

ka_2<-at_2%>%group_by(title,districts,subtitle,description,status,categories)%>%
  dplyr::summarise(do_union=FALSE)%>%st_cast("LINESTRING")%>%                              
  sf::st_set_crs(4326)

mapview::mapview(ka_2["title"])

at<-rbind(ka, ka_2)

################################################################################################

##50 Projekte##

fifty <- "https://www.infravelo.de/api/v1/projects/50/50/"
Auswertung50 <- fromJSON(paste(readLines(fifty), collapse="", na.strings=c("","NA")))
Auswertung50 <-as.data.table(Auswertung50)%>%
  unnest_wider(results)%>%
  unnest(districts)%>%
  unnest(districts)


Auswertung50$xml <- Auswertung50$kml %>% 
  map(function(x) read_xml(x, as_html = TRUE, options = "NOBLANKS"))

temporary50 <- Auswertung50$xml %>% map(function(x) xml_find_all(x, "//multigeometry"))                               
Auswertung50$line <- temporary50 %>% map(function(x) xml_text(x, trim = FALSE))

Auswertung50$line<-gsub('[\t\n]', '', Auswertung50$line)                                                          
Auswertung50$line<-sub('relativeToGround', '', Auswertung50$line)                                                        
Auswertung50$num<-length(grep("relativeToGround", Auswertung50))                                                         
Auswertung50<-separate(data = Auswertung50, col = line, into = c("eins", "zwei"), sep = "relativeToGround")              
Auswertung50<-Auswertung50%>%filter(!eins=="character(0)")                                                               

###Erste Linestrings###
tt50<-Auswertung50%>%select("eins","districts","subtitle","description","status","categories","title")%>%as.data.frame()                                                           
at50<-tt50 %>%                                                                                                          
  mutate(lat = strsplit(as.character(tt50$eins), ",")) %>%                                            
  unnest(lat)

at50<-at50%>%select("title","subtitle","districts","description","status","categories","lat")%>%as.data.frame(matrix(at50$lat, ncol = 2, byrow = TRUE))                             
at50$lat<-sub("^0+", "", at50$lat) 
at50$lat<-as.numeric(at50$lat)
at50<-as.data.frame(at50)
at50<-at50%>%dplyr::group_by(title)%>%dplyr::filter(!is.na(lat))                       
at50<-at50 %>%                                                                                                          
  dplyr::group_by(grp = str_c('lat', rep(1:2, length.out = n()))) %>%
  dplyr::mutate(rn = row_number()) %>%
  ungroup %>%
  pivot_wider(names_from = grp, values_from = lat) %>%
  select(-rn)


at50<-at50%>%sf::st_as_sf(coords = c("lat1","lat2"))%>%                                                                 
  sf::st_set_crs(4326)

ka50<-at50%>%group_by(title,districts,subtitle,description,status,categories)%>%dplyr::summarise(do_union=FALSE)%>%st_cast("LINESTRING")%>%                              
  sf::st_set_crs(4326)

mapview::mapview(ka50["title"])

##2.Linestrings##

tt50_2<-Auswertung50%>%select("zwei","districts","subtitle","description","status","categories","title")%>%as.data.frame()                                                           
at50_2<-tt50_2 %>%                                                                                                          
  mutate(lat = strsplit(as.character(tt50_2$zwei), ",")) %>%                                            
  unnest(lat)

at50_2<-at50_2%>%select("title","subtitle","districts","description","status","categories","lat")%>%as.data.frame(matrix(at50_2$lat, ncol = 2, byrow = TRUE))                             
at50_2$lat<-sub("^0+", "", at50_2$lat) 
at50_2$lat<-as.numeric(at50_2$lat)
at50_2<-as.data.frame(at50_2)
at50_2<-at50_2%>%dplyr::group_by(title)%>%dplyr::filter(!is.na(lat))                       
at50_2<-at50_2 %>%                                                                                                          
  dplyr::group_by(grp = str_c('lat', rep(1:2, length.out = n()))) %>%
  dplyr::mutate(rn = row_number()) %>%
  ungroup %>%
  pivot_wider(names_from = grp, values_from = lat) %>%
  select(-rn)


at50_2<-at50_2%>%sf::st_as_sf(coords = c("lat1","lat2"))%>%                                                                 
  sf::st_set_crs(4326)

ka50_2<-at50_2%>%group_by(title,districts,subtitle,description,status,categories)%>%
  dplyr::summarise(do_union=FALSE)%>%st_cast("LINESTRING")%>%                              
  sf::st_set_crs(4326)

mapview::mapview(ka50_2["title"])

at50<-rbind(ka50, ka50_2)

###################################

##100 Projekte##

hundred <- "https://www.infravelo.de/api/v1/projects/100/50/"
Auswertung100 <- fromJSON(paste(readLines(hundred), collapse="", na.strings=c("","NA")))
Auswertung100 <-as.data.table(Auswertung100)%>%
  unnest_wider(results)%>%
  unnest(districts)%>%
  unnest(districts)

Auswertung100<-Auswertung100[!(is.na(Auswertung100$kml) | Auswertung100$kml==""), ]

Auswertung100$xml <- Auswertung100$kml %>% 
  map(function(x) read_xml(x, as_html = TRUE, options = "NOBLANKS"))

temporary100 <- Auswertung100$xml %>% map(function(x) xml_find_all(x, "//multigeometry"))                               
Auswertung100$line <- temporary100 %>% map(function(x) xml_text(x, trim = FALSE))

Auswertung100$line<-gsub('[\t\n]', '', Auswertung100$line)                                                          
Auswertung100$line<-sub('relativeToGround', '', Auswertung100$line)                                                        
Auswertung100$num<-length(grep("relativeToGround", Auswertung100))                                                         
Auswertung100<-separate(data = Auswertung100, col = line, into = c("eins", "zwei"), sep = "relativeToGround")              
Auswertung100<-Auswertung100%>%filter(!eins=="character(0)")                                                               

###Erste Linestrings###
tt100<-Auswertung100%>%select("eins","districts","subtitle","description","status","categories","title")%>%as.data.frame()                                                           
at100<-tt100 %>%                                                                                                          
  mutate(lat = strsplit(as.character(tt100$eins), ",")) %>%                                            
  unnest(lat)

at100<-at100%>%select("title","subtitle","districts","description","status","categories","lat")%>%as.data.frame(matrix(at100$lat, ncol = 2, byrow = TRUE))                             
at100$lat<-sub("^0+", "", at100$lat) 
at100$lat<-as.numeric(at100$lat)
at100<-as.data.frame(at100)
at100<-at100%>%dplyr::group_by(title)%>%dplyr::filter(!is.na(lat))                       
at100<-at100 %>%                                                                                                          
  dplyr::group_by(grp = str_c('lat', rep(1:2, length.out = n()))) %>%
  dplyr::mutate(rn = row_number()) %>%
  ungroup %>%
  pivot_wider(names_from = grp, values_from = lat) %>%
  select(-rn)


at100<-at100%>%sf::st_as_sf(coords = c("lat1","lat2"))%>%                                                                 
  sf::st_set_crs(4326)

ka100<-at100%>%group_by(title,districts,subtitle,description,status,categories)%>%dplyr::summarise(do_union=FALSE)%>%st_cast("LINESTRING")%>%                              
  sf::st_set_crs(4326)

mapview::mapview(ka100["title"])

##2.Linestrings##

tt100_2<-Auswertung100%>%select("zwei","districts","subtitle","description","status","categories","title")%>%as.data.frame()                                                           
at100_2<-tt100_2 %>%                                                                                                          
  mutate(lat = strsplit(as.character(tt100_2$zwei), ",")) %>%                                            
  unnest(lat)

at100_2<-at100_2%>%select("title","subtitle","districts","description","status","categories","lat")%>%as.data.frame(matrix(at100_2$lat, ncol = 2, byrow = TRUE))                             
at100_2$lat<-sub("^0+", "", at100_2$lat) 
at100_2$lat<-as.numeric(at100_2$lat)
at100_2<-as.data.frame(at100_2)
at100_2<-at100_2%>%dplyr::group_by(title)%>%dplyr::filter(!is.na(lat))                       
at100_2<-at100_2 %>%                                                                                                          
  dplyr::group_by(grp = str_c('lat', rep(1:2, length.out = n()))) %>%
  dplyr::mutate(rn = row_number()) %>%
  ungroup %>%
  pivot_wider(names_from = grp, values_from = lat) %>%
  select(-rn)


at100_2<-at100_2%>%sf::st_as_sf(coords = c("lat1","lat2"))%>%                                                                 
  sf::st_set_crs(4326)

ka100_2<-at100_2%>%group_by(title,districts,subtitle,description,status,categories)%>%
  dplyr::summarise(do_union=FALSE)%>%st_cast("LINESTRING")%>%                              
  sf::st_set_crs(4326)

mapview::mapview(ka100_2["title"])

at100<-rbind(ka100, ka100_2)


##################################

##150 Projekte##

hfifty <- "https://www.infravelo.de/api/v1/projects/150/50/"
Auswertung150 <- fromJSON(paste(readLines(hfifty), collapse="", na.strings=c("","NA")))
Auswertung150 <-as.data.table(Auswertung150)%>%
  unnest_wider(results)%>%
  unnest(districts)%>%
  unnest(districts)


Auswertung150$xml <- Auswertung150$kml %>% 
  map(function(x) read_xml(x, as_html = TRUE, options = "NOBLANKS"))

temporary150 <- Auswertung150$xml %>% map(function(x) xml_find_all(x, "//multigeometry"))                               
Auswertung150$line <- temporary150 %>% map(function(x) xml_text(x, trim = FALSE))

Auswertung150$line<-gsub('[\t\n]', '', Auswertung150$line)                                                          
Auswertung150$line<-sub('relativeToGround', '', Auswertung150$line)                                                        
Auswertung150$num<-length(grep("relativeToGround", Auswertung150))                                                         
Auswertung150<-separate(data = Auswertung150, col = line, into = c("eins", "zwei"), sep = "relativeToGround")              
Auswertung150<-Auswertung150%>%filter(!eins=="character(0)")                                                               

###Erste Linestrings###
tt150<-Auswertung150%>%select("eins","districts","subtitle","description","status","categories","title")%>%as.data.frame()                                                           
at150<-tt150 %>%                                                                                                          
  mutate(lat = strsplit(as.character(tt150$eins), ",")) %>%                                            
  unnest(lat)

at150<-at150%>%select("title","subtitle","districts","description","status","categories","lat")%>%as.data.frame(matrix(at150$lat, ncol = 2, byrow = TRUE))                             
at150$lat<-sub("^0+", "", at150$lat) 
at150$lat<-as.numeric(at150$lat)
at150<-as.data.frame(at150)
at150<-at150%>%dplyr::group_by(title)%>%dplyr::filter(!is.na(lat))                       
at150<-at150 %>%                                                                                                          
  dplyr::group_by(grp = str_c('lat', rep(1:2, length.out = n()))) %>%
  dplyr::mutate(rn = row_number()) %>%
  ungroup %>%
  pivot_wider(names_from = grp, values_from = lat) %>%
  select(-rn)


at150<-at150%>%sf::st_as_sf(coords = c("lat1","lat2"))%>%                                                                 
  sf::st_set_crs(4326)

ka150<-at150%>%group_by(title,districts,subtitle,description,status,categories)%>%dplyr::summarise(do_union=FALSE)%>%st_cast("LINESTRING")%>%                              
  sf::st_set_crs(4326)

mapview::mapview(ka150["title"])

##2.Linestrings##

tt150_2<-Auswertung150%>%select("zwei","districts","subtitle","description","status","categories","title")%>%as.data.frame()                                                           
at150_2<-tt150_2 %>%                                                                                                          
  mutate(lat = strsplit(as.character(tt150_2$zwei), ",")) %>%                                            
  unnest(lat)

at150_2<-at150_2%>%select("title","subtitle","districts","description","status","categories","lat")%>%as.data.frame(matrix(at150_2$lat, ncol = 2, byrow = TRUE))                             
at150_2$lat<-sub("^0+", "", at150_2$lat) 
at150_2$lat<-as.numeric(at150_2$lat)
at150_2<-as.data.frame(at150_2)
at150_2<-at150_2%>%dplyr::group_by(title)%>%dplyr::filter(!is.na(lat))                       
at150_2<-at150_2 %>%                                                                                                          
  dplyr::group_by(grp = str_c('lat', rep(1:2, length.out = n()))) %>%
  dplyr::mutate(rn = row_number()) %>%
  ungroup %>%
  pivot_wider(names_from = grp, values_from = lat) %>%
  select(-rn)


at150_2<-at150_2%>%sf::st_as_sf(coords = c("lat1","lat2"))%>%                                                                 
  sf::st_set_crs(4326)

ka150_2<-at150_2%>%group_by(title,districts,subtitle,description,status,categories)%>%
  dplyr::summarise(do_union=FALSE)%>%st_cast("LINESTRING")%>%                              
  sf::st_set_crs(4326)

mapview::mapview(ka150_2["title"])

at150<-rbind(ka150, ka150_2)

##############################################################################
##200 Projekte##

twoh <- "https://www.infravelo.de/api/v1/projects/200/50/"
Auswertung200 <- fromJSON(paste(readLines(twoh), collapse="", na.strings=c("","NA")))
Auswertung200 <-as.data.table(Auswertung200)%>%
  unnest_wider(results)%>%
  unnest(districts)%>%
  unnest(districts)


Auswertung200$xml <- Auswertung200$kml %>% 
  map(function(x) read_xml(x, as_html = TRUE, options = "NOBLANKS"))

temporary200 <- Auswertung200$xml %>% map(function(x) xml_find_all(x, "//multigeometry"))                               
Auswertung200$line <- temporary200 %>% map(function(x) xml_text(x, trim = FALSE))

Auswertung200$line<-gsub('[\t\n]', '', Auswertung200$line)                                                          
Auswertung200$line<-sub('relativeToGround', '', Auswertung200$line)                                                        
Auswertung200$num<-length(grep("relativeToGround", Auswertung200))                                                         
Auswertung200<-separate(data = Auswertung200, col = line, into = c("eins", "zwei"), sep = "relativeToGround")              
Auswertung200<-Auswertung200%>%filter(!eins=="character(0)")                                                               

###Erste Linestrings###
tt200<-Auswertung200%>%select("eins","districts","subtitle","description","status","categories","title")%>%as.data.frame()                                                           
at200<-tt200 %>%                                                                                                          
  mutate(lat = strsplit(as.character(tt200$eins), ",")) %>%                                            
  unnest(lat)

at200<-at200%>%select("title","subtitle","districts","description","status","categories","lat")%>%as.data.frame(matrix(at200$lat, ncol = 2, byrow = TRUE))                             
at200$lat<-sub("^0+", "", at200$lat) 
at200$lat<-as.numeric(at200$lat)
at200<-as.data.frame(at200)
at200<-at200%>%dplyr::group_by(title)%>%dplyr::filter(!is.na(lat))                       
at200<-at200 %>%                                                                                                          
  dplyr::group_by(grp = str_c('lat', rep(1:2, length.out = n()))) %>%
  dplyr::mutate(rn = row_number()) %>%
  ungroup %>%
  pivot_wider(names_from = grp, values_from = lat) %>%
  select(-rn)


at200<-at200%>%sf::st_as_sf(coords = c("lat1","lat2"))%>%                                                                 
  sf::st_set_crs(4326)

ka200<-at200%>%group_by(title,districts,subtitle,description,status,categories)%>%dplyr::summarise(do_union=FALSE)%>%st_cast("LINESTRING")%>%                              
  sf::st_set_crs(4326)

mapview::mapview(ka200["title"])

###2.Linestrings###

tt200_2<-Auswertung200%>%select("zwei","districts","subtitle","description","status","categories","title")%>%as.data.frame()                                                           
at200_2<-tt200_2 %>%                                                                                                          
  mutate(lat = strsplit(as.character(tt200_2$zwei), ",")) %>%                                            
  unnest(lat)

at200_2<-at200_2%>%select("title","subtitle","districts","description","status","categories","lat")%>%as.data.frame(matrix(at200_2$lat, ncol = 2, byrow = TRUE))                             
at200_2$lat<-sub("^0+", "", at200_2$lat) 
at200_2$lat<-as.numeric(at200_2$lat)
at200_2<-as.data.frame(at200_2)
at200_2<-at200_2%>%dplyr::group_by(title)%>%dplyr::filter(!is.na(lat))                       
at200_2<-at200_2 %>%                                                                                                          
  dplyr::group_by(grp = str_c('lat', rep(1:2, length.out = n()))) %>%
  dplyr::mutate(rn = row_number()) %>%
  ungroup %>%
  pivot_wider(names_from = grp, values_from = lat) %>%
  select(-rn)


at200_2<-at200_2%>%sf::st_as_sf(coords = c("lat1","lat2"))%>%                                                                 
  sf::st_set_crs(4326)

ka200_2<-at200_2%>%group_by(title,districts,subtitle,description,status,categories)%>%
  dplyr::summarise(do_union=FALSE)%>%st_cast("LINESTRING")%>%                              
  sf::st_set_crs(4326)

mapview::mapview(ka200_2["title"])

at200<-rbind(ka200, ka200_2)



############################################################
##250 Projekte

twohf <- "https://www.infravelo.de/api/v1/projects/250/50/"
Auswertung250 <- fromJSON(paste(readLines(twohf), collapse="", na.strings=c("","NA")))
Auswertung250 <-as.data.table(Auswertung250)%>%
  unnest_wider(results)%>%
  unnest(districts)%>%
  unnest(districts)


Auswertung250$xml <- Auswertung250$kml %>% 
  map(function(x) read_xml(x, as_html = TRUE, options = "NOBLANKS"))

temporary250 <- Auswertung250$xml %>% map(function(x) xml_find_all(x, "//multigeometry"))                               
Auswertung250$line <- temporary250 %>% map(function(x) xml_text(x, trim = FALSE))

Auswertung250$line<-gsub('[\t\n]', '', Auswertung250$line)                                                          
Auswertung250$line<-sub('relativeToGround', '', Auswertung250$line)                                                        
Auswertung250$num<-length(grep("relativeToGround", Auswertung250))                                                         
Auswertung250<-separate(data = Auswertung250, col = line, into = c("eins", "zwei"), sep = "relativeToGround")              
Auswertung250<-Auswertung250%>%filter(!eins=="character(0)")                                                               

###Erste Linestrings###
tt250<-Auswertung250%>%select("eins","districts","subtitle","description","status","categories","title")%>%as.data.frame()                                                           
at250<-tt250 %>%                                                                                                          
  mutate(lat = strsplit(as.character(tt250$eins), ",")) %>%                                            
  unnest(lat)

at250<-at250%>%select("title","subtitle","districts","description","status","categories","lat")%>%as.data.frame(matrix(at250$lat, ncol = 2, byrow = TRUE))                             
at250$lat<-sub("^0+", "", at250$lat) 
at250$lat<-as.numeric(at250$lat)
at250<-as.data.frame(at250)
at250<-at250%>%dplyr::group_by(title)%>%dplyr::filter(!is.na(lat))                       
at250<-at250 %>%                                                                                                          
  dplyr::group_by(grp = str_c('lat', rep(1:2, length.out = n()))) %>%
  dplyr::mutate(rn = row_number()) %>%
  ungroup %>%
  pivot_wider(names_from = grp, values_from = lat) %>%
  select(-rn)


at250<-at250%>%sf::st_as_sf(coords = c("lat1","lat2"))%>%                                                                 
  sf::st_set_crs(4326)

ka250<-at250%>%group_by(title,districts,subtitle,description,status,categories)%>%dplyr::summarise(do_union=FALSE)%>%st_cast("LINESTRING")%>%                              
  sf::st_set_crs(4326)

mapview::mapview(ka250["title"])

###2.Linestrings##

tt250_2<-Auswertung250%>%select("zwei","districts","subtitle","description","status","categories","title")%>%as.data.frame()                                                           
at250_2<-tt250_2 %>%                                                                                                          
  mutate(lat = strsplit(as.character(tt250_2$zwei), ",")) %>%                                            
  unnest(lat)

at250_2<-at250_2%>%select("title","subtitle","districts","description","status","categories","lat")%>%as.data.frame(matrix(at250_2$lat, ncol = 2, byrow = TRUE))                             
at250_2$lat<-sub("^0+", "", at250_2$lat) 
at250_2$lat<-as.numeric(at250_2$lat)
at250_2<-as.data.frame(at250_2)
at250_2<-at250_2%>%dplyr::group_by(title)%>%dplyr::filter(!is.na(lat))                       
at250_2<-at250_2 %>%                                                                                                          
  dplyr::group_by(grp = str_c('lat', rep(1:2, length.out = n()))) %>%
  dplyr::mutate(rn = row_number()) %>%
  ungroup %>%
  pivot_wider(names_from = grp, values_from = lat) %>%
  select(-rn)


at250_2<-at250_2%>%sf::st_as_sf(coords = c("lat1","lat2"))%>%                                                                 
  sf::st_set_crs(4326)

ka250_2<-at250_2%>%group_by(title,districts,subtitle,description,status,categories)%>%
  dplyr::summarise(do_union=FALSE)%>%st_cast("LINESTRING")%>%                              
  sf::st_set_crs(4326)

mapview::mapview(ka250_2["title"])

at250<-rbind(ka250, ka250_2)

####################################################################

##300 Projekte

three <- "https://www.infravelo.de/api/v1/projects/300/50/"
Auswertung300 <- fromJSON(paste(readLines(three), collapse="", na.strings=c("","NA")))
Auswertung300 <-as.data.table(Auswertung300)%>%
  unnest_wider(results)%>%
  unnest(districts)%>%
  unnest(districts)

Auswertung300<-Auswertung300[!(is.na(Auswertung300$kml) | Auswertung300$kml==""), ]

Auswertung300$xml <- Auswertung300$kml %>% 
  map(function(x) read_xml(x, as_html = TRUE, options = "NOBLANKS"))

temporary300 <- Auswertung300$xml %>% map(function(x) xml_find_all(x, "//multigeometry"))                               
Auswertung300$line <- temporary300 %>% map(function(x) xml_text(x, trim = FALSE))

Auswertung300$line<-gsub('[\t\n]', '', Auswertung300$line)                                                          
Auswertung300$line<-sub('relativeToGround', '', Auswertung300$line)                                                        
Auswertung300$num<-length(grep("relativeToGround", Auswertung300))                                                         
Auswertung300<-separate(data = Auswertung300, col = line, into = c("eins", "zwei"), sep = "relativeToGround")              
Auswertung300<-Auswertung300%>%filter(!eins=="character(0)")                                                               

###Erste Linestrings###
tt300<-Auswertung300%>%select("eins","districts","subtitle","description","status","categories","title")%>%as.data.frame()                                                           
at300<-tt300 %>%                                                                                                          
  mutate(lat = strsplit(as.character(tt300$eins), ",")) %>%                                            
  unnest(lat)

at300<-at300%>%select("title","subtitle","districts","description","status","categories","lat")%>%as.data.frame(matrix(at300$lat, ncol = 2, byrow = TRUE))                             
at300$lat<-sub("^0+", "", at300$lat) 
at300$lat<-as.numeric(at300$lat)
at300<-as.data.frame(at300)
at300<-at300%>%dplyr::group_by(title)%>%dplyr::filter(!is.na(lat))                       
at300<-at300 %>%                                                                                                          
  dplyr::group_by(grp = str_c('lat', rep(1:2, length.out = n()))) %>%
  dplyr::mutate(rn = row_number()) %>%
  ungroup %>%
  pivot_wider(names_from = grp, values_from = lat) %>%
  select(-rn)


at300<-at300%>%sf::st_as_sf(coords = c("lat1","lat2"))%>%                                                                 
  sf::st_set_crs(4326)

ka300<-at300%>%group_by(title,districts,subtitle,description,status,categories)%>%dplyr::summarise(do_union=FALSE)%>%st_cast("LINESTRING")%>%                              
  sf::st_set_crs(4326)

mapview::mapview(ka300["title"])

###2.Linestrings###

tt300_2<-Auswertung300%>%select("zwei","districts","subtitle","description","status","categories","title")%>%as.data.frame()                                                           
at300_2<-tt300_2 %>%                                                                                                          
  mutate(lat = strsplit(as.character(tt300_2$zwei), ",")) %>%                                            
  unnest(lat)

at300_2<-at300_2%>%select("title","subtitle","districts","description","status","categories","lat")%>%as.data.frame(matrix(a300_2$lat, ncol = 2, byrow = TRUE))                             
at300_2$lat<-sub("^0+", "", at300_2$lat) 
at300_2$lat<-as.numeric(at300_2$lat)
at300_2<-as.data.frame(at300_2)
at300_2<-at300_2%>%dplyr::group_by(title)%>%dplyr::filter(!is.na(lat))                       
at300_2<-at300_2 %>%                                                                                                          
  dplyr::group_by(grp = str_c('lat', rep(1:2, length.out = n()))) %>%
  dplyr::mutate(rn = row_number()) %>%
  ungroup %>%
  pivot_wider(names_from = grp, values_from = lat) %>%
  select(-rn)


at300_2<-at300_2%>%sf::st_as_sf(coords = c("lat1","lat2"))%>%                                                                 
  sf::st_set_crs(4326)

ka300_2<-at300_2%>%group_by(title,districts,subtitle,description,status,categories)%>%
  dplyr::summarise(do_union=FALSE)%>%st_cast("LINESTRING")%>%                              
  sf::st_set_crs(4326)

at300<-rbind(ka300, ka300_2)

gesamt<-rbind(ka,ka50,ka100,ka150,ka200,ka250,ka300)

gesamt_buffer<-st_transform(gesamt, CRS("+init=epsg:3068 +datum=WGS84 +units=m"))
gesamt_buffer<-st_buffer(gesamt_buffer,12)  

####load fbinter data###
fbinter_2020<-read_sf("D:\\Eigene Dateien\\Favorites\\Downloads\\fbinter_radverkehrsanlagen.geojson")

fbinter_2020<-st_transform(fbinter_2020, CRS("+init=epsg:3068 +datum=WGS84 +units=m"))

intersections <- st_join(fbinter_2020,gesamt_buffer)

