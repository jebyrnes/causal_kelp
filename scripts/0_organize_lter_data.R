# Load Libraries ####
library(tidyverse)
library(readr)
library(visdat)

# Load Data ####
kelp <- read_csv("../raw_data/Giant_Kelp_All_Years_20171201.csv")
quads <- read_csv("../raw_data/quad_swath_all_years_20171201.csv")
cover <- read_csv("../raw_data/Cover_All_Years_20171201.csv", na = "")

# Aggregate Data ####
# get kelp stipe density per transect/year/site
kelp_ag <- kelp %>%
  mutate(FRONDS = ifelse(FRONDS == -99999, 0, FRONDS)) %>%
  group_by(YEAR, SITE, TRANSECT) %>%
  summarize(KELP = sum(FRONDS, na.rm=T)) 
  

# get subcanopy kelp density per transect/year/site
quads_ag <- quads %>%
  filter(GROUP == "ALGAE")%>%
  filter(SURVEY == "SWATH") %>%
  filter(!str_detect(SP_CODE, "CYOS")) %>%
  mutate(COUNT = ifelse(COUNT == -99999, 0, COUNT)) %>%
  group_by(YEAR, SITE, TRANSECT) %>%
  summarize(SUBCANOPY_ALGAE = sum(COUNT, na.rm=T)) 

# get algal and invert cover per transect/year/site
cover_ag <- cover %>%
  filter(GROUP %in%  c("ALGAE", "INVERT"))%>%
  filter(!(SP_CODE %in%  c("MPJ", "BLD", "EC", "MH", "CYOS")))%>%
  filter(TAXON_ORDER != "Corallinales") %>%
  mutate(COUNT = ifelse(PERCENT_COVER == -99999,0, PERCENT_COVER)) %>%
  group_by(YEAR, SITE, TRANSECT, GROUP) %>%
  summarize(COVER = sum(COUNT, na.rm=T)) %>%
  spread(GROUP, COVER)

# get urchins
urchins <- quads %>%
  filter(TAXON_GENUS %in% c("Strongylocentrotus", "Mesocentrotus"))%>%
  filter(SURVEY == "QUAD") %>%
  filter(str_detect(SP_CODE, "L")) %>%
  mutate(URCHINS = ifelse(COUNT == -99999, 0, COUNT)) %>%
  group_by(YEAR, SITE, TRANSECT) %>%
  summarize(URCHINS = sum(URCHINS, na.rm=T)) 
  
  
# Combine Data ####
combined_data <- reduce(list(kelp_ag, quads_ag, cover_ag, urchins), left_join) #%>%
  #replace_na(list(SUBCANOPY_ALGAE = 0, ALGAE = 0, URCHINS = 0, INVERT = 0))


# Save Derived Data ####
saveRDS(combined_data, file = "../derived_data/combined_lter_aggregated.Rds")
