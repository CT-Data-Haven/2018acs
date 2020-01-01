library(tidyverse)

year <- 2018
# get data profile table numbers
dps <- jsonlite::fromJSON(str_glue("https://api.census.gov/data/{year}/acs/acs5/profile/variables.json"), simplifyVector = TRUE, flatten = TRUE) %>%
  `[[`("variables") %>%
  map_dfr(as_tibble, .id = "variable") %>% 
  distinct(group, concept) %>%
  filter(str_detect(group, "^DP\\d{2}$")) %>%
  mutate(concept = str_to_title(concept) %>%
           str_remove("^Selected") %>%
           str_remove("In The United States") %>%
           str_trim() %>%
           recode(`Acs Demographic And Housing Estimates` = "Demographic Characteristics")) %>%
  arrange(group)

# pull out text-only columns, take distinct to get 1 row
# geo codes from census xlsx file; keep state, county, county subdivision
geos <- openxlsx::read.xlsx("https://www2.census.gov/programs-surveys/popest/geographies/2018/all-geocodes-v2018.xlsx", startRow = 5) %>%
  as_tibble() %>%
  rename_all(~str_to_lower(.) %>% 
               str_remove_all("\\.\\(.+$") %>% 
               str_replace_all("\\.", "_") %>%
               str_remove("_code")) %>%
  filter(state == "09", summary_level %in% c("040", "050", "061")) %>%
  mutate(area_name = str_remove(area_name, " town") %>%
           str_replace(" County", " County, Connecticut"),
         summary_level = str_sub(summary_level, 1, 2) %>% str_pad(7, "right", "0")) %>%
  select(summary_level:county_subdivision, name = area_name) %>%
  mutate(us = "US") %>%
  select(summary_level, us, everything()) %>%
  pivot_longer(-name, names_to = "variable") %>%
  filter(!str_detect(value, "^0+$")) %>%
  group_by(name) %>%
  summarise(code = paste0(value, collapse = ""))


base_url <- "https://data.census.gov/cedsci/table"
# add g = geo code, table = DP02, tid ACSDP5Y2018.DP03
base_q <- list(vintage = year, d = "ACS 5-Year Estimates Data Profiles")

urls <- geos %>%
  mutate(dp = list(dps)) %>%
  unnest(dp) %>%
  mutate(tid = str_glue("ACSDP5Y{year}.{group}")) %>%
  mutate(q = pmap(list(table = group, g = code, tid = tid), c, base_q),
         url = map_chr(q, ~httr::modify_url(base_url, query = .))) %>%
  select(name, concept, url) %>%
  pivot_wider(id_cols = name, names_from = concept, values_from = url)


moes <- read_csv("input_data/cws_max_moes.csv") %>%
  mutate(name = name %>%
           str_replace("(?<= County)$", ", Connecticut") %>%
           recode(Valley = "Lower Naugatuck Valley")) %>%
  mutate(max_moe = sprintf("%.1f%%", max_moe)) 

prof16 <- read_csv("https://raw.githubusercontent.com/CT-Data-Haven/WebsiteIndicators/master/TownProfiles/5year2016town_profile_expanded_CWS.csv", col_types = cols(.default = "c"))

has_digits <- function(x) all((str_detect(x, "^\\d")), na.rm = TRUE)
not_digits <- function(x) !has_digits(x)

meta_text <- prof16 %>%
  select(-Town:-County, -ends_with("Characteristics")) %>%
  select_if(not_digits) %>%
  mutate(Source = Source %>% 
           str_replace_all("2015", "2018") %>%
           str_replace_all("1, 3, and 5-year", "1 and 5-year")) %>%
  distinct()

meta_out <- prof16 %>%
  select(Town) %>%
  mutate(meta = list(meta_text)) %>%
  unnest(meta) %>%
  left_join(urls, by = c("Town" = "name")) %>%
  left_join(moes, by = c("Town" = "name")) %>%
  select(Town, `Key Facts`, `Wellbeing, Population 18 years and over`, max_moe, everything())


write_csv(meta_out, "input_data/website_meta.csv")