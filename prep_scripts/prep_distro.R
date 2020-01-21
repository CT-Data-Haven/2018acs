library(tidyverse)
library(data.world)

url <- "camille86/neighborhoods18"
hdrs <- read_csv("_utils/indicator_headings.txt") %>%
  distinct(indicator, display) %>%
  mutate(indicator = str_replace(indicator, "^(estimate|share)\\s(.+)$", "\\2_\\1")) %>%
  bind_rows(read_csv("_utils/headings_extra.txt"))


# neighborhood profiles, individual files (?)
nhoods <- list.files("to_distro", pattern = "neighborhood_2018.csv", full.names = TRUE) %>%
  set_names(str_extract, "\\w+_neighborhood_2018.csv")

# town, all one file
town <- read_csv("output/acs_town_basic_profile_2018.csv") %>%
  select(level, name, group, estimate, share) %>%
  # mutate_at(vars(level, group), as_factor) %>%
  distinct(name, group, .keep_all = TRUE) %>%
  pivot_longer(estimate:share, names_to = "type") %>%
  filter(!is.na(value)) %>%
  unite(indicator, group, type) %>%
  left_join(hdrs, by = "indicator") %>%
  select(-indicator) %>%
  pivot_wider(names_from = display)

write_csv(town, "to_distro/town_acs_basic_2018.csv")

# all_files <- c(list(town_acs_basic_2018.csv = town), nhoods) 
# 
# all_files %>%
#   imap(~dwapi::upload_data_frame(dataset = url, data_frame = .x, file_name = .y))

# add license: cc sharealike
dwapi::update_dataset(url, dataset_update_request(license_string = "CC-BY-SA"))

# add descriptions
# nhood_desc <- names(nhoods) %>%
#   str_extract("^\\w+(?=_acs)") %>%
#   camiller::clean_titles(cap_all = TRUE) %>%
#   paste("ACS basic indicators by neighborhood, 2018 5yr estimates,", .)
# 
# nhood_req <- map2(names(nhoods), nhood_desc, function(file, desc) {
#   file_create_or_update_request(file_name = file, description = desc, labels = list("clean data"))
# })
# 
# walk(nhood_req, ~update_dataset(url, dataset_update_request(files = list(.))))
