library(tidyverse)
library(tidycensus)
library(camiller)
library(cwi)

################# VARIABLES ###############
acs_year <- 2018
###########################################

source("./_utils/weight_regions.R")

# will do new haven in separately, since it involves combining block groups & tracts depending on table
params <- list(
  bridgeport = list(
    region = "Fairfield County", town = "Bridgeport", df = bridgeport_tracts
  ),
  hartford = list(
    region = "Greater Hartford", town = c("Hartford", "West Hartford"), df = hartford_tracts
  ),
  stamford = list(
    region = "Fairfield County", town = "Stamford", df = stamford_tracts
  )
)

filters <- c(
  "Connecticut",
  params %>% map(pluck, "town") %>% flatten_chr(),
  params %>% map(pluck, "region") %>% flatten_chr()
) %>% unique()

tracts_df <- params %>%
  map_dfr(pluck, "df", .id = "city") %>%
  mutate(town = coalesce(town, city) %>% str_to_title()) %>%
  select(-tract)

fetch_tract <- readRDS(str_glue("./output/acs_basic_{acs_year}_fetch_tracts.rds")) %>%
  map(select, -NAME, -moe) %>%
  map(janitor::clean_names) %>%
  map(~weight_regions(., tracts_df, city, town, name, variable, join_cols = c("geoid"))) %>%
  map(label_acs, year = acs_year) %>%
  map(mutate, level = factor("5_tracts"))

fetch_town <- readRDS(str_glue("./output/acs_basic_{acs_year}_fetch.rds")) %>%
  map(select, level, name = NAME, variable, estimate) %>%
  map(filter, name %in% filters) %>%
  map(label_acs, year = acs_year)

fetch <- map2(fetch_town, fetch_tract, bind_rows) %>%
  map(mutate, level = as.factor(level)) %>%
  map(group_by, level, city, town, name)

out <- list()

# TOTAL POPULATION
# out$total_pop <- fetch$total_pop %>%
#   mutate(indicator = "total_pop") %>%
#   select(level, city, town, name, indicator, estimate)


# AGE
# population under 18, 18+, 65+
out$age <- fetch$sex_by_age %>%
  separate(label, into = c("total", "sex", "group"), sep = "!!") %>%
  filter(!is.na(group)) %>%
  add_grps(list(total_pop = 1:23, ages0_17 = 1:4, ages18plus = 5:23, ages65plus = 18:23)) %>%
  calc_shares(digits = 2)


# RACE / HISPANIC
# hispanic, white non-hispanic, black non-hispanic, other non-hispanic
out$race <- fetch$race %>%
  add_grps(list(total_pop = 1, hispanic = 12, white = 3, black = 4, other_race = 5:9), group = label) %>%
  calc_shares(digits = 2, group = label) %>%
  rename(group = label)


# FOREIGN-BORN
out$immigration <- fetch$foreign_born %>%
  separate(label, into = c("total", "group"), sep = "!!") %>%
  filter(!is.na(group)) %>%
  add_grps(list(total_pop = 1:5, foreign_born = 4:5)) %>%
  calc_shares(digits = 2)


# TENURE
# owner-occupied households
tenure <- fetch$tenure %>%
  add_grps(list(total_households = 1, owner_occupied = 2), group = label) %>%
  calc_shares(digits = 2, group = label, denom = "total_households") %>%
  rename(group = label)


# HOUSING COST
# cost-burdened, not by tenure
housing_cost <- fetch$housing_cost %>%
  separate(label, into = c("total", "tenure", "income", "group"), sep = "!!") %>%
  filter(!is.na(group)) %>%
  add_grps(list(total_households = 1:3, cost_burden = 3)) %>%
  calc_shares(digits = 2, denom = "total_households") %>%
  filter(group != "total_households")

out$housing <- bind_rows(tenure, housing_cost)


# POVERTY & LOW-INCOME
# poverty determined; below 1x fpl, below 2x fpl
out$income <- fetch$poverty %>%
  add_grps(list(poverty_status_determined = 1, poverty = 2:3, low_income = 2:7), group = label) %>%
  calc_shares(digits = 2, group = label, denom = "poverty_status_determined") %>%
  rename(group = label)


# POVERTY & LOW-INCOME BY AGE
# ages 0-17, ages 65+
pov_age <- fetch$pov_age %>%
  separate(label, into = c("total", "age", "ratio"), sep = "!!") %>%
  filter(!is.na(ratio)) %>%
  mutate_at(vars(age, ratio), as_factor) %>%
  group_by(ratio, add = TRUE) %>%
  add_grps(list(ages0_17 = 1:3, ages65plus = 9:10), group = age) %>%
  group_by(level, city, town, name, age) %>%
  add_grps(list(poverty_status_determined = 1:12, poverty = 1:3, low_income = 1:8), group = ratio) %>%
  calc_shares(digits = 2, group = ratio, denom = "poverty_status_determined") %>%
  ungroup() %>%
  unite(group, age, ratio)
out$income_children <- pov_age %>% filter(str_detect(group, "^ages0_17"))
out$income_seniors <- pov_age %>% filter(str_detect(group, "^ages65plus"))


# BIND EVERYTHING

headings <- read_csv("./_utils/indicator_headings.txt") %>%
  mutate(topic = as_factor(topic))

out_df <- suppressWarnings(bind_rows(out, .id = "topic")) %>%
  ungroup() %>%
  mutate_at(vars(topic, group), as_factor) %>%
  pivot_longer(estimate:share, names_to = "type") %>%
  unite(indicator, type, group, sep = " ") %>%
  filter(!is.na(value)) %>%
  left_join(headings, by = c("topic", "indicator")) %>%
  mutate(level = fct_relabel(level, str_replace, "tracts", "neighborhoods") %>%
           fct_collapse("2_regions" = c("2_counties", "3_regions")))

# move json stuff to separate file
# just output csvs
out_by_city <- params %>%
  imap(function(p, cty) {
    fltr <- c(unlist(p[c("region", "town")]), "Connecticut")
    out_df %>%
      filter(name %in% fltr | city == cty) %>%
      mutate(city = clean_titles(cty, cap_all = TRUE))
  })


saveRDS(out_by_city, str_glue("../nhood_profiles_2018/input_data/acs_to_prep_for_viz_{acs_year}.rds"))

# only hartford actually needs town column
out_by_city %>%
  map(distinct, level, city, town, name, indicator, .keep_all = TRUE) %>%
  map(pivot_wider, id_cols = c(level, city, town, name), names_from = display) %>%
  map(function(df) {
    if ("town" %in% names(df) && n_distinct(df$town, na.rm = TRUE) < 2) {
      df %>% select(-town)
    } else {
      df
    }
  }) %>%
  iwalk(~write_csv(.x, str_glue("./to_distro/{.y}_acs_basic_neighborhood_{acs_year}.csv"), na = ""))
