library(tidyverse)
library(tidycensus)
library(camiller)
library(cwi)

################# VARIABLES ##################
acs_year <- 2018
short_yr <- str_sub(acs_year, -2)
##############################################

town_county <- xwalk %>%
  mutate(county_code = str_sub(town_fips, 3, 5)) %>%
  distinct(town, county_code) %>%
  inner_join(fips_codes %>% filter(state == "CT"), by = "county_code") %>%
  select(town, county) %>%
  mutate(county = str_remove(county, " County"))
  

fetch <- readRDS(str_glue("./output/acs_basic_{acs_year}_fetch.rds")) %>%
  map(~filter(., str_detect(level, "(state|counties|regions|towns)"))) %>%
  map(label_acs, acs_year) %>%
  map(~mutate(., NAME = ifelse(str_detect(level, "tracts"), GEOID, NAME))) %>%
  map(janitor::clean_names) %>%
  map(group_by, level, name, year) %>%
  map(~replace_na(., list(moe = 0)))
out <- list()


# TOTAL POPULATION
out$total_pop <- fetch$total_pop %>%
  mutate(group = "total_pop") %>%
  select(level, name, year, group, estimate, moe)


# SEX & AGE
# population under 18, 65+, male, female
# age
out$age <- fetch$sex_by_age %>%
  separate(label, into = c("total", "sex", "age"), sep = "!!") %>%
  filter(!is.na(age)) %>%
  add_grps(list(total_pop = 1:23, ages0_17 = 1:4, ages65plus = 18:23), group = age, moe = moe) %>%
  calc_shares(group = age, moe = moe) %>%
  rename(group = age)

# sex
out$sex <- fetch$sex_by_age %>%
  separate(label, into = c("total", "sex", "age"), sep = "!!") %>%
  filter(is.na(age)) %>%
  replace_na(list(sex = "total_pop")) %>%
  select(level, name, year, sex, estimate, moe) %>%
  calc_shares(group = sex, moe = moe) %>%
  mutate(sex = str_to_lower(sex)) %>%
  rename(group = sex)


# RACE / HISPANIC
# hispanic, white non-hispanic, black non-hispanic, other non-hispanic
out$race <- fetch$race %>%
  add_grps(list(total_pop = 1, hispanic = 12, white = 3, black = 4, other_race = 5:9), group = label, moe = moe) %>%
  calc_shares(group = label, moe = moe) %>%
  rename(group = label)


# FOREIGN-BORN
out$foreign_born <- fetch$foreign_born %>%
  add_grps(list(total_pop = 1, foreign_born = 5:6), group = label, moe = moe) %>%
  calc_shares(group = label, moe = moe) %>%
  rename(group = label)


# TENURE
# owner-occupied households
out$tenure <- fetch$tenure %>%
  add_grps(list(total_households = 1, owner_occupied = 2), group = label, moe = moe) %>%
  calc_shares(group = label, denom = "total_households", moe = moe) %>%
  rename(group = label)


# HOUSING COST
# cost-burdened, not by tenure
out$housing_cost <- fetch$housing_cost %>%
  separate(label, into = c("total", "tenure", "income", "group"), sep = "!!") %>%
  filter(!is.na(group)) %>%
  add_grps(list(total_households = 1:3, cost_burden = 3), moe = moe) %>%
  calc_shares(denom = "total_households", moe = moe)


# VEHICLES
# households with at least 1 car
out$vehicles <- fetch$vehicles %>%
  add_grps(list(total_households = 1, has_vehicle = 3:6), group = label, moe = moe) %>%
  calc_shares(group = label, denom = "total_households", moe = moe) %>%
  rename(group = label)


# EDUCATIONAL ATTAINMENT
# ages 25+; share with less than high school, bachelors+
out$education <- fetch$education %>%
  add_grps(list(ages25plus = 1, less_than_high_school = 2, bachelors_plus = 5:6), group = label, moe = moe) %>%
  calc_shares(group = label, denom = "ages25plus", moe = moe) %>%
  rename(group = label)


# MEDIAN HOUSEHOLD INCOME
# drop regions
out$median_household_income <- fetch$median_income %>%
  filter(!str_detect(level, "regions")) %>%
  mutate(group = "median_household_income") %>%
  select(level, name, year, group, estimate, moe)


# POVERTY & LOW-INCOME
# poverty determined; below 1x fpl, below 2x fpl
out$poverty <- fetch$poverty %>%
  add_grps(list(poverty_status_determined = 1, poverty = 2:3, low_income = 2:7), group = label, moe = moe) %>%
  calc_shares(group = label, denom = "poverty_status_determined", moe = moe) %>%
  rename(group = label)


# POVERTY & LOW-INCOME BY AGE
# ages 0-17, ages 65+
out$pov_age <- fetch$pov_age %>%
  separate(label, into = c("total", "age", "ratio"), sep = "!!") %>%
  filter(!is.na(ratio)) %>%
  mutate_at(vars(age, ratio), as_factor) %>%
  group_by(ratio, add = TRUE) %>%
  add_grps(list(ages0_17 = 1:3, ages65plus = 9:10), group = age, moe = moe) %>%
  group_by(level, name, year, age) %>%
  add_grps(list(poverty_status_determined = 1:12, poverty = 1:3, low_income = 1:8), group = ratio, moe = moe) %>%
  calc_shares(group = ratio, denom = "poverty_status_determined", moe = moe) %>%
  unite(group, age, ratio)


# BIND ALL TOGETHER
# use old copy for its names
prof16 <- read_csv("https://raw.githubusercontent.com/CT-Data-Haven/WebsiteIndicators/master/TownProfiles/5year2016town_profile_expanded_CWS.csv", col_types = cols(.default = "c"))

out_df <- out %>%
  bind_rows(.id = "indicator")

out_wide <- out_df %>%
  ungroup() %>%
  mutate_at(vars(group, name), as_factor) %>%
  mutate(level = fct_relevel(level, "1_state", "3_regions", "2_counties", "4_towns")) %>%
  arrange(level, name) %>% 
  select(-indicator, -level, -year) %>% 
  distinct(name, group, .keep_all = TRUE) %>%
  make_wide(estimate:sharemoe, group = group) %>%
  mutate(name = as.character(name) %>%
           str_replace("(?<= County)$", ", Connecticut")) %>%
  left_join(town_county, by = c("name" = "town")) %>%
  select(Town = name, County = county, everything())


cws_df <- read_csv("input_data/cws_2018_all_town_data.csv") %>%
  mutate(indicator = as_factor(indicator) %>%
           fct_relabel(~paste(., "cws", sep = "_")),
         name = str_replace(name, "(?<= County)$", ", Connecticut")) %>%
  pivot_wider(names_from = indicator)

meta <- read_csv("input_data/website_meta.csv")

prof_done <- out_wide %>%
  left_join(cws_df, by = c("Town" = "name")) %>%
  left_join(meta, by = "Town") %>% 
  select(Town, County, `Key Facts`, `Wellbeing, Population 18 years and over`,
         # cws indicators
         satisfied_w_area_cws:underemployed_cws, max_moe,
         `Demographic, Total Population`,
         total_pop_estimate:female_sharemoe,
         `Race and Ethnicity, Total Population`,
         hispanic_estimate:other_race_sharemoe,
         `Place of Birth, Total Population`,
         foreign_born_estimate:foreign_born_sharemoe,
         Households,
         total_households_estimate:has_vehicle_sharemoe,
         `Educational Attainment, Population 25 years and over`,
         ages25plus_estimate:bachelors_plus_sharemoe,
         `Median Income`,
         median_household_income_estimate:median_household_income_moe,
         `Poverty and Low-Income, Total Population`,
         poverty_status_determined_estimate:low_income_sharemoe,
         `Poverty and Low-Income, Population 0 to 17 years`,
         ages0_17_poverty_status_determined_estimate:ages0_17_low_income_sharemoe,
         `Poverty and Low-Income, Population 65 years and over`,
         ages65plus_poverty_status_determined_estimate:ages65plus_low_income_sharemoe,
         Source:`Demographic Characteristics`
         ) %>%
  mutate_at(vars(matches("share(moe)?$")), scales::percent, accuracy = 0.1) %>%
  mutate_at(vars(matches("cws$")), scales::percent, accuracy = 1) %>%
  set_names(names(prof16)) %>%
  rename_all(str_replace_all, "his/her", "their") %>%
  rename_all(str_replace_all, "he/she lives", "they live")

write_csv(prof_done, str_glue("output/5year{acs_year}town_profile_expanded_CWS.csv"), na = "")

saveRDS(out_df, str_glue("output/acs_town_basic_profile_{acs_year}.rds"))

write_csv(out_df, str_glue("output/acs_town_basic_profile_{acs_year}.csv"))
