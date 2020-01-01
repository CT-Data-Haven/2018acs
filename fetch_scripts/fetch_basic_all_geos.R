library(tidyverse)
library(tidycensus)
library(camiller)
library(cwi)

##################### VARIABLES ############
year <- 2018
############################################

regions_short <- cwi::regions[c("Greater New Haven", "Greater Waterbury", "Greater Bridgeport", "Lower Naugatuck Valley")]


fetch <- cwi::basic_table_nums %>%
  map(~multi_geo_acs(table = ., year = year, towns = "all", regions = regions_short))

BRRR::skrrrahh()

saveRDS(fetch, str_glue("./output/acs_basic_{year}_fetch.rds"))

