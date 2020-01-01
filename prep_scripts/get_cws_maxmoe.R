library(tidyverse)
library(readxl)
library(xml2)

survey_year <- 2018

paths18 <- list.files(file.path("input_data", "crosstabs"), pattern = str_glue("^DataHaven{survey_year}.+\\.xlsx?$"), full.names = T) %>%
  set_names(~str_extract(., "(?<=DataHaven\\d{4} ).+$") %>%
              str_remove("( Crosstabs Pub| Crosstabs)?\\.xlsx$") %>%
              str_replace_all("^CCF$", "Greater Waterbury") %>%
              str_replace_all("^CRCOG$", "Greater Hartford") %>%
              str_remove_all("(Statewide|CCF|CRCOG|Region|Towns)") %>%
              str_replace_all("Cty", "County") %>%
              str_replace_all("(?<=[a-z])\\B(?=[A-Z])", " ") %>%
              # str_trim() %>%
              str_remove_all("\\s{2,}") %>%
              str_replace("(Inner Ring|Outer Ring)([\\w\\s]+$)", "\\2 \\1") %>%
              str_remove("Greater (?=[\\w\\s]+Ring)") %>%
              str_trim()
  )
# max MOE is only contained in the header of each excel file, wtf
# and apparently not all the headers come through right, so I have to unzip each file, then extract from xml
# https://gist.github.com/schaunwheeler/5825002
get_header <- function(path) {
  temp_dir <- file.path(tempdir(check = TRUE))
  suppressWarnings(dir.create(temp_dir))
  file.copy(path, temp_dir)
  new_file <- list.files(temp_dir, full.names = TRUE, pattern = basename(path))
  unzip(new_file, exdir = temp_dir)

  xl <- file.path(temp_dir, "xl", "worksheets", "sheet1.xml")

  read_xml(xl) %>%
    xml_child("d1:headerFooter") %>%
    as_list() %>%
    flatten() %>%
    str_subset("MOE") %>%
    str_match("(?<=MOE )\\+\\/\\-\\s?([\\d.]+)") %>%
    .[2] %>%
    as.numeric()
}

moes <- paths18 %>%
  map(get_header) %>%
  map_dfr(enframe, name = "num", value = "max_moe", .id = "name") %>%
  select(-num)

write_csv(moes, "input_data/cws_max_moes.csv")