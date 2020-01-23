library(tidyverse)
library(data.world)

url <- "camille86/neighborhoods18"
hdrs <- read_csv("_utils/indicator_headings.txt") %>%
  distinct(indicator, display) %>%
  mutate(indicator = str_replace(indicator, "^(estimate|share)\\s(.+)$", "\\2_\\1")) %>%
  bind_rows(read_csv("_utils/headings_extra.txt"))

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


# nhoods go on dw from gh; towns can just go up from this file
# neighborhood profiles, individual files (?)
nhood_req <- list.files("to_distro", pattern = "neighborhood_2018.csv") %>%
  map(function(path) {
    city <- path %>%
      str_extract("\\w+_neighborhood_2018.csv") %>%
      camiller::clean_titles(cap_all = TRUE)
    desc <- paste("ACS basic indicators by neighborhood, 2018 5yr estimates,", city)
    url <- paste0("https://github.com/CT-Data-Haven/2018acs/blob/master/to_distro/", path)
    file_create_or_update_request(file_name = path, description = desc, labels = list("clean data"), url = url)
  })

walk(nhood_req, ~update_dataset(url, dataset_update_request(files = list(.))))

dwapi::upload_file(url, "to_distro/town_acs_basic_2018.csv", "town_acs_basic_2018.csv")
town_req <- file_create_or_update_request(file_name = "town_acs_basic_2018.csv", 
                                          description = "ACS basic indicators by town and region, 2018 5yr estimates", 
                                          labels = list("clean data"))
update_dataset(url, dataset_update_request(files = list(town_req)))

# add license: cc sharealike
dwapi::update_dataset(url, dataset_update_request(license_string = "CC-BY-SA"))
