############################################################################
#ersten 50 Projekte#

top50 <- "https://www.infravelo.de/api/v1/projects/50/50/"
top_50 <- fromJSON(paste(readLines(top50), collapse="", na.strings=c("","NA")))
top_50<-as.data.table(top_50)%>%
  unnest_wider(results)%>%
  unnest(categories)%>%
  unnest(categories)%>%
  unnest(types)%>%
  unnest(types)%>%
  unnest(districts)%>%unnest(districts)

top_50$xml <- top_50$kml %>% 
  map(function(x) read_xml(x, as_html = TRUE, options = "NOBLANKS"))

temporary_top50 <- top_50$xml %>% 
  map(function(x) xml_find_all(x, "//coordinates"))

top_50$coordinates <- temporary_top50 %>% 
  map(function(x) xml_text(x, trim = FALSE))

top_50<-top_50%>%unnest(coordinates)
top_50$coords<- trimws(top_50$coordinates, which = c("both"))

##isolate longitude/latitude data from xml column##
top_50<-top_50 %>%
  mutate(koordinaten = strsplit(as.character(coords), ",")) %>% 
  unnest(koordinaten)

top_50$koord<-str_replace(top_50$koordinaten, "^0+" ,"")
top_50$koord<-as.numeric(top_50$koord)

library(tidyr)

top_50<-top_50 %>% mutate_all(na_if,"")
top_50<-top_50%>%drop_na(koord)

aa_top50<-top_50 %>%
  dplyr::group_by(grp = str_c('koord', rep(1:2, length.out = n()))) %>%
  dplyr::mutate(rn = row_number()) %>%
  ungroup %>%
  pivot_wider(names_from = grp, values_from = koord) %>%
  select(-rn)

test<-transform(aa_top50, koord2 = c(koord2[-1], NA))

test <- subset(test, !is.na(koord1)) 

unique_top50<-distinct(test, koord1, .keep_all = TRUE)

at_top50<- unique_top50 %>%
  dplyr::arrange(koord2)%>%
  tail(-2)%>%
  # slice(which(row_number() %% 4 == 2))%>%                                   #select every 3 rd geopoint to reduce density 
  sf::st_as_sf(coords = c("koord1","koord2"))%>%
  sf::st_set_crs(4326)

#####################################################################################################

#100 Projekte

top100<- "https://www.infravelo.de/api/v1/projects/100/50/"
top_100 <- fromJSON(paste(readLines(top100), collapse="", na.strings=c("","NA")))
top_100<-as.data.table(top_100)%>%
  unnest_wider(results)%>%
  unnest(categories)%>%
  unnest(categories)%>%
  unnest(types)%>%
  unnest(types)%>%
  unnest(districts)%>%unnest(districts)

#-1 row#
top_100<-top_100[!(is.na(top_100$kml) | top_100$kml==""), ]

top_100$xml <- top_100$kml %>% 
  map(function(x) read_xml(x, as_html = TRUE, options = "NOBLANKS"))

temporary_top100 <- top_100$xml %>% 
  map(function(x) xml_find_all(x, "//coordinates"))

top_100$coordinates <- temporary_top100 %>% 
  map(function(x) xml_text(x, trim = FALSE))

top_100<-top_100%>%unnest(coordinates)
top_100$coords<- trimws(top_100$coordinates, which = c("both"))

##isolate longitude/latitude data from xml column##
top_100<-top_100 %>%
  mutate(koordinaten = strsplit(as.character(coords), ",")) %>% 
  unnest(koordinaten)

top_100$koord<-str_replace(top_100$koordinaten, "^0+" ,"")
top_100$koord<-as.numeric(top_100$koord)

top_100<-top_100 %>% mutate_all(na_if,"")
top_100<-top_100%>%drop_na(koord)

aa_top100<-top_100 %>%
  dplyr::group_by(grp = str_c('koord', rep(1:2, length.out = n()))) %>%
  dplyr::mutate(rn = row_number()) %>%
  ungroup %>%
  pivot_wider(names_from = grp, values_from = koord) %>%
  select(-rn)

aa_top100<-transform(aa_top100, koord2 = c(koord2[-1], NA))

aa_top100 <- subset(aa_top100, !is.na(koord1)) 

unique_top100<-distinct(aa_top100, koord1, .keep_all = TRUE)

at_top100<- unique_top100 %>%
  dplyr::arrange(koord2)%>%
  tail(-2)%>%
  #  slice(which(row_number() %% 4 == 2))%>%                                  
  sf::st_as_sf(coords = c("koord1","koord2"))%>%
  sf::st_set_crs(4326)

##################################################################################

#150 Projekte

top150<-"https://www.infravelo.de/api/v1/projects/150/50/"
top_150 <- fromJSON(paste(readLines(top150), collapse="", na.strings=c("","NA")))

top_150<-as.data.table(top_150)%>%
  unnest_wider(results)%>%
  unnest(categories)%>%
  unnest(categories)%>%
  unnest(types)%>%
  unnest(types)%>%
  unnest(districts)%>%unnest(districts)

top_150$xml <- top_150$kml %>% 
  map(function(x) read_xml(x, as_html = TRUE, options = "NOBLANKS"))

temporary_top150 <- top_150$xml %>% 
  map(function(x) xml_find_all(x, "//coordinates"))

top_150$coordinates <- temporary_top150 %>% 
  map(function(x) xml_text(x, trim = FALSE))

top_150<-top_150%>%unnest(coordinates)
top_150$coords<- trimws(top_150$coordinates, which = c("both"))

##isolate longitude/latitude data from xml column##
top_150<-top_150 %>%
  mutate(koordinaten = strsplit(as.character(coords), ",")) %>% 
  unnest(koordinaten)

top_150$koord<-str_replace(top_150$koordinaten, "^0+" ,"")
top_150$koord<-as.numeric(top_150$koord)

top_150<-top_150 %>% mutate_all(na_if,"")
top_150<-top_150%>%drop_na(koord)

aa_top150<-top_150 %>%
  dplyr::group_by(grp = str_c('koord', rep(1:2, length.out = n()))) %>%
  dplyr::mutate(rn = row_number()) %>%
  ungroup %>%
  pivot_wider(names_from = grp, values_from = koord) %>%
  select(-rn)

aa_top150<-transform(aa_top150, koord2 = c(koord2[-1], NA))

aa_top150 <- subset(aa_top150, !is.na(koord1)) 

unique_top150<-distinct(aa_top150, koord1, .keep_all = TRUE)

at_top150<- unique_top150 %>%
  dplyr::arrange(koord2)%>%
  tail(-2)%>%
  #  slice(which(row_number() %% 4 == 2))%>%                                  
  sf::st_as_sf(coords = c("koord1","koord2"))%>%
  sf::st_set_crs(4326)

##################################################################################

##200 Projekte

top200<- "https://www.infravelo.de/api/v1/projects/200/50/"
top_200 <- fromJSON(paste(readLines(top200), collapse="", na.strings=c("","NA")))


###Metricsunnesten!!!###
top_200<-as.data.table(top_200)%>%
  unnest_wider(results)%>%
  unnest(categories)%>%
  unnest(categories)%>%
  unnest(types)%>%
  unnest(types)%>%
  unnest(districts)%>%unnest(districts)

top_200$xml <- top_200$kml %>% 
  map(function(x) read_xml(x, as_html = TRUE, options = "NOBLANKS"))

temporary_top200 <- top_200$xml %>% 
  map(function(x) xml_find_all(x, "//coordinates"))

top_200$coordinates <- temporary_top200 %>% 
  map(function(x) xml_text(x, trim = FALSE))

top_200<-top_200%>%unnest(coordinates)
top_200$coords<- trimws(top_200$coordinates, which = c("both"))

##isolate longitude/latitude data from xml column##
top_200<-top_200 %>%
  mutate(koordinaten = strsplit(as.character(coords), ",")) %>% 
  unnest(koordinaten)

top_200$koord<-str_replace(top_200$koordinaten, "^0+" ,"")
top_200$koord<-as.numeric(top_200$koord)

top_200<-top_200 %>% mutate_all(na_if,"")
top_200<-top_200%>%drop_na(koord)


aa_top200<-top_200 %>%
  dplyr::group_by(grp = str_c('koord', rep(1:2, length.out = n()))) %>%
  dplyr::mutate(rn = row_number()) %>%
  ungroup %>%
  pivot_wider(names_from = grp, values_from = koord) %>%
  select(-rn)

aa_top200<-transform(aa_top200, koord2 = c(koord2[-1], NA))

aa_top200 <- subset(aa_top200, !is.na(koord1)) 

unique_top200<-distinct(aa_top200, koord1, .keep_all = TRUE)

at_top200<- unique_top200 %>%
  dplyr::arrange(koord2)%>%
  tail(-2)%>%
  #  slice(which(row_number() %% 4 == 2))%>%                                  
  sf::st_as_sf(coords = c("koord1","koord2"))%>%
  sf::st_set_crs(4326)

##############################################################################################

## 250 Projekte

top250<- "https://www.infravelo.de/api/v1/projects/250/50/"
top_250 <- fromJSON(paste(readLines(top250), collapse="", na.strings=c("","NA")))

top_250<-as.data.table(top_250)%>%
  unnest_wider(results)%>%
  unnest(categories)%>%
  unnest(categories)%>%
  unnest(types)%>%
  unnest(types)%>%
  unnest(districts)%>%unnest(districts)

top_250$xml <- top_250$kml %>% 
  map(function(x) read_xml(x, as_html = TRUE, options = "NOBLANKS"))

temporary_top250 <- top_250$xml %>% 
  map(function(x) xml_find_all(x, "//coordinates"))

top_250$coordinates <- temporary_top250 %>% 
  map(function(x) xml_text(x, trim = FALSE))

top_250<-top_250%>%unnest(coordinates)
top_250$coords<- trimws(top_250$coordinates, which = c("both"))

##isolate longitude/latitude data from xml column##
top_250<-top_250 %>%
  mutate(koordinaten = strsplit(as.character(coords), ",")) %>% 
  unnest(koordinaten)

top_250$koord<-str_replace(top_250$koordinaten, "^0+" ,"")
top_250$koord<-as.numeric(top_250$koord)

library(tidyr)

top_250<-top_250 %>% mutate_all(na_if,"")
top_250<-top_250%>%drop_na(koord)

aa_top250<-top_250 %>%
  dplyr::group_by(grp = str_c('koord', rep(1:2, length.out = n()))) %>%
  dplyr::mutate(rn = row_number()) %>%
  ungroup %>%
  pivot_wider(names_from = grp, values_from = koord) %>%
  select(-rn)

aa_top250<-transform(aa_top250, koord2 = c(koord2[-1], NA))

aa_top250 <- subset(aa_top250, !is.na(koord1)) 

unique_top250<-distinct(aa_top250, koord1, .keep_all = TRUE)

at_top250<- unique_top250 %>%
  dplyr::arrange(koord2)%>%
  tail(-2)%>%
  # slice(which(row_number() %% 4 == 2))%>%                                  
  sf::st_as_sf(coords = c("koord1","koord2"))%>%
  sf::st_set_crs(4326)

########################################################################

##Top 300

top300<- "https://www.infravelo.de/api/v1/projects/300/50/"
top_300 <- fromJSON(paste(readLines(top300), collapse="", na.strings=c("","NA")))

top_300<-as.data.table(top_300)%>%
  unnest_wider(results)%>%
  unnest(categories)%>%
  unnest(categories)%>%
  unnest(types)%>%
  unnest(types)%>%
  unnest(districts)%>%unnest(districts)


top_300$xml <- top_300$kml %>% 
  map(function(x) read_xml(x, as_html = TRUE, options = "NOBLANKS"))

temporary_top300 <- top_300$xml %>% 
  map(function(x) xml_find_all(x, "//coordinates"))

top_300$coordinates <- temporary_top300 %>% 
  map(function(x) xml_text(x, trim = FALSE))

top_300<-top_300%>%unnest(coordinates)
top_300$coords<- trimws(top_300$coordinates, which = c("both"))

##isolate longitude/latitude data from xml column##
top_300<-top_300 %>%
  mutate(koordinaten = strsplit(as.character(coords), ",")) %>% 
  unnest(koordinaten)

top_300$koord<-str_replace(top_300$koordinaten, "^0+" ,"")
top_300$koord<-as.numeric(top_300$koord)

library(tidyr)

top_300<-top_300 %>% mutate_all(na_if,"")
top_300<-top_300%>%drop_na(koord)


aa_top300<-top_300 %>%
  dplyr::group_by(grp = str_c('koord', rep(1:2, length.out = n()))) %>%
  dplyr::mutate(rn = row_number()) %>%
  ungroup %>%
  pivot_wider(names_from = grp, values_from = koord) %>%
  select(-rn)

aa_top300<-transform(aa_top300, koord2 = c(koord2[-1], NA))

aa_top300 <- subset(aa_top300, !is.na(koord1)) 

unique_top300<-distinct(aa_top300, koord1, .keep_all = TRUE)

at_top300<- unique_top300 %>%
  dplyr::arrange(koord2)%>%
  tail(-2)%>%
  #  slice(which(row_number() %% 4 == 2))%>%                                  
  sf::st_as_sf(coords = c("koord1","koord2"))%>%
  sf::st_set_crs(4326)

##################################################################################
##Assemble Infravelo dataset

at_top50$additionalHtmlContent=NULL
at_top100$additionalHtmlContent=NULL
at_top150$additionalHtmlContent=NULL
at_top200$additionalHtmlContent=NULL
at_top250$additionalHtmlContent=NULL
at_top300$additionalHtmlContent=NULL

gesamt_beschreibung<-bind_rows(at_top50,at_top100,at_top150,at_top200,at_top250,at_top300)

gesamt_beschreibung<-gesamt_beschreibung%>%
  dplyr::filter(!title %like% "Mitte - Tegel - Spandau")%>%
  dplyr::filter(!title %like% "Reinicken")%>%
  dplyr::filter(!title %like% "Spree-Rad")%>%                   #Spree-Rad- und Wanderweg -> nicht vergessen
  dplyr::filter(!title %like% "Ost-Route")

gesamt_beschreibung<-st_transform(gesamt_beschreibung, CRS("+init=epsg:3068 +datum=WGS84 +units=m"))
gesamt_beschreibung_buffer<-st_buffer(gesamt_beschreibung,7)                                             #create a 7 meter buffer around Infravelo Geopoint


###West-Route gesondert betrachten!!!###
############################################################################################

##Load fbinter data as fbinter_2020##

fbinter_2020<-
  #kreuzung<-fbinter_2020%>%filter(sorvt_typ=="Radfahrerfurt Z 340") 
  #fbinter_2020<-fbinter_2020%>%filter(!sorvt_typ=="Radfahrerfurt Z 340")                                              #Kreuzungen rausfiltern
  fbinter_2020<-st_transform(fbinter_2020, CRS("+init=epsg:3068 +datum=WGS84 +units=m"))

intersections <- st_join(fbinter_2020,gesamt_beschreibung_buffer)

vergleich<- intersections%>%select("stst_str","title","sorvt_typ","types")