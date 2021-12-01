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