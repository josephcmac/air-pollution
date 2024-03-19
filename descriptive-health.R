library(tidyverse)

read_file <- function(filename) {
  read.csv(paste0("../../datasets/IHME_GHDx/Incidence/", filename,"/", filename, ".csv")) %>%
	  select(location, sex, age, year, val) %>%
	  rename(incidence = val)
}

# age group n ranges from 5n to 5n+4
fix_age_single <- function(x) {
  switch(x, 
    "<5 years"="0",
    "5-9 years"="1",
    "10-14 years"="2",
    "15-19 years"="3",
    "20-24 years"="4",
    "25-29 years"="5",
    "30-34 years"="6",
    "35-39 years"="7",
    "40-44 years"="8",
    "45-49 years"="9",
    "50-54 years"="10",
    "55-59 years"="11",
    "60-64 years"="12",
    "65-69 years"="13",
    "70-74 years"="14",
    "75-79 years"="15",
    "80-84"="16",
    "85-89"="17",
    "90-94"="18",
    x
  )
}

fix_age <- function(x) {
  sapply(x, function(y) fix_age_single(y))
}

read_files <- function(filenames) {
map_df(filenames, ~ read_file(.x)) %>%
  filter( !(age %in% c("All ages", "Age-standardized") ) ) %>%
  mutate(age = age %>% fix_age %>% as.integer, sex = as.factor(sex), location = as.factor(location))
}


filenames <- c("IHME-GBD_2019_DATA-5fcc42dd-1", "IHME-GBD_2019_DATA-28e7f8dc-1", "IHME-GBD_2019_DATA-77ebed24-1", "IHME-GBD_2019_DATA-610d44b5-1", "IHME-GBD_2019_DATA-a0e15397-1", "IHME-GBD_2019_DATA-c8d94d50-1", "IHME-GBD_2019_DATA-d0cb166f-1")


df <- read_files(filenames)

df %>% glimpse



