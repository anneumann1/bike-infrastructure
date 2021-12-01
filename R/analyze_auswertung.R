library("tidyverse")
library("xml2")

filename <- file.choose("Auswertung.rds")
auswertung <- readRDS(filename)

auswertung <-  auswertung %>% drop_na(kml)

auswertung$xml <- auswertung$kml %>% 
  map(function(x) read_xml(x, as_html = TRUE, options = "NOBLANKS"))

auswertung$coordinates <- auswertung$xml %>% 
  map(function(x) xml_text(x, "//coordinates"))
