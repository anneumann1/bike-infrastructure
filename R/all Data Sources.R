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


##Berlin API##

url <- "https://api.viz.berlin.de/daten/baustellen_sperrungen.json"
downloader::download(url = url, destfile = "baustellen_sperrungen.json")
baustelle <- read_sf("baustellen_sperrungen.json")

baustelle<-baustelle %>% st_transform(4326) 

baustelle%>%
  #  filter(!content=="wieder frei")%>%
  #  filter(!str_detect(content, "geräumt")) %>%
  filter(!str_detect(street, "A")) %>%
  ggplot()+geom_sf()
