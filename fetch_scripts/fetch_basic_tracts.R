library(tidyverse)
library(tidycensus)
library(camiller)
library(cwi)

##################### VARIABLES ############
year <- 2018
############################################

# this gets all the data for neighborhood profiles for New Haven, Hartford/West Hartford, Bridgeport, and Stamford. 
# Hierarchy:
# State, Greater New Haven, Fairfield County, cities, all tracts in cities
# separately, do all block groups in New Haven for tables where available
regions_short <- cwi::regions[c("Greater New Haven", "Greater Hartford")]
cities <- c("New Haven", "Bridgeport", "Hartford", "West Hartford", "Stamford")

fetch <- cwi::basic_table_nums %>%
  imap(function(num, nm) {
    print(paste(num, nm))
    multi_geo_acs(table = num, year = year,
                  towns = cities,
                  regions = regions_short,
                  counties = "all", 
                  tracts = "all")
  })

saveRDS(fetch, str_glue("./output/acs_basic_{year}_fetch_tracts.rds"))

############ block groups, only New Haven County

fetch_bg <- cwi::basic_table_nums %>%
  imap(function(num, nm) {
    print(paste(num, nm))
    get_acs("block group", table = num, 
            year = year, 
            state = "09", 
            county = "New Haven")
  })

saveRDS(fetch_bg, str_glue("./output/acs_basic_{year}_fetch_bgrps.rds"))
