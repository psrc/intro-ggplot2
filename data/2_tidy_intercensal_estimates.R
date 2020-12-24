library(tidyverse)
library(readxl)
library(here)

inter_censal_files <- "ofm_april1_intercensal_estimates_2000-2010.xlsx"

clean_intercensal_sheet <- function(filename, sheetname) {
  # munge intercensal data from a single sheet
  # one sheet covers one attribute

  lookup <- read_excel(here("data", "lookup.xlsx")) %>%
    drop_na(inter_county_name) %>% 
    mutate(across("Filter", as.numeric))
  
  rdf <- read_excel(here("data", filename), sheet = sheetname) %>% 
    drop_na(Filter)
  
  est_cols <- str_subset(colnames(rdf), "^\\d+")
  id_cols <- c("Filter", "County Name", "City Name", "Jurisdiction")
  id_cols2 <- c("Filter", "County", "Jurisdiction")
  counties <- c("King", "Kitsap", "Pierce", "Snohomish")
  
  df <- rdf %>% inner_join(lookup, by = c("Filter", "County Name" = "inter_county_name", 
                                          "Jurisdiction" = "inter_jurisdiction", 
                                          "City Name" = "inter_city_name")) %>% 
    select(!all_of(id_cols[2:4])) %>%
    rename(Jurisdiction = post_jurisdiction, County = post_county) %>% 
    filter(County %in% counties) %>% 
    select(all_of(c(id_cols2, est_cols))) %>% 
    pivot_longer(cols = all_of(est_cols)) %>% 
    extract(name, into = c("year"), "(^\\d+)") %>% 
    mutate(across(value, as.numeric), attr = sheetname)
}

compile_intercensal_data <- function(filename) {
  # compile select attributes of intercensal data across multiple sheets
  # pivot to long format
  sheets <- c("Total Population", "Household Population", "GQ Population", "Total Housing", "Occupied Housing")
  
  clean_data <- partial(clean_intercensal_sheet, filename = filename)
  all_data <- map(sheets, clean_data) %>% reduce(bind_rows)
}

df <- compile_intercensal_data(inter_censal_files)