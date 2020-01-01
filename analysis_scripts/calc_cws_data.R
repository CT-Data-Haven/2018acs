library(tidyverse)
library(camiller)
library(cwi)

# need these same indicators, but for 2018 cws
# satisfied with area
# safe to walk at night
# excellent/very good health
# obesity
# smoking
# food insecurity
# housing insecurity
# transportation insecurity
# no bank account
# underemployment
indics <- c("satisfied_w_area", "safe_walking_at_night", "self_rated_health", "obesity", "smoking", "food_insecurity", "housing_insecurity", "transportation_insecure", "no_bank_account", "underemployed")
prof16 <- read_csv("https://raw.githubusercontent.com/CT-Data-Haven/WebsiteIndicators/master/TownProfiles/5year2016town_profile_expanded_CWS.csv", col_types = cols(.default = "c")) %>% 
  select(1, 5:15) %>% 
  filter(!is.na(`% of adults, Satisfied with area where he/she lives`)) %>%
  mutate(name = str_remove(Town, ",.+$"))



housing <- readRDS("input_data/cws_2018_housing_insecurity.rds") %>%
  mutate(indicator = "housing_insecurity") %>%
  rename(name = region)
comm <- read_csv("input_data/cws_community_cohesion_2018.csv")
finance <- read_csv("input_data/cws_financial_insecurity_2018.csv")
health <- read_csv("input_data/cws_health_risk_factors_2018.csv") %>%
  select(-proper_name)
econ <- read_csv("input_data/cws_econ_opportunity_2018.csv")

# for now, keeping all areas cws data is available for; might filter to match 2015 data if Mark wants
cws_df <- bind_rows(housing, comm, finance, health, econ) %>%
  mutate(name = clean_titles(name, cap_all = TRUE) %>% recode(`Valley` = "Lower Naugatuck Valley")) %>%
  filter(category == "Total", name != "5ct") %>%
  select(indicator, name, value) %>%
  filter(indicator %in% indics) %>%
  mutate(indicator = as_factor(indicator) %>% fct_relevel(indics)) %>%
  arrange(name, indicator)

write_csv(cws_df, "input_data/cws_2018_all_town_data.csv")
