library(tidyverse)

read_file_standarized <- function(filename) {
  read.csv(paste0("../../datasets/IHME_GHDx/Incidence/", filename,"/", filename, ".csv")) %>%
	  select(location, sex, age, year, val) %>%
	  rename(incidence = val) %>% 
	  filter(age =="Age-standardized") %>%
	  select(-age) %>%
	  mutate(sex = as.factor(sex), location = as.factor(location))
}


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

exp_label <- function(x) {
  parse(text = sprintf("10^%s", as.character(log10(x))))
}

male_female_separation <- function(df) {
  df_Male <- df %>% filter(sex=="Male") %>% select(-sex) %>% rename(Male = incidence)
  df_Female <- df %>% filter(sex=="Female") %>% select(-sex) %>% rename(Female = incidence)
  return(merge(df_Male, df_Female))
}


filenames <- c("IHME-GBD_2019_DATA-5fcc42dd-1", "IHME-GBD_2019_DATA-28e7f8dc-1", "IHME-GBD_2019_DATA-77ebed24-1", "IHME-GBD_2019_DATA-610d44b5-1", "IHME-GBD_2019_DATA-a0e15397-1", "IHME-GBD_2019_DATA-c8d94d50-1", "IHME-GBD_2019_DATA-d0cb166f-1")

df_all <- read_files(filenames) %>% male_female_separation() %>% select(-Male)

x_min <- df_all$Female %>% min(na.rm=T)
x_max <- df_all$Female %>% max(na.rm=T)

df_sd <- read_file_standarized("IHME-GBD_2019_DATA-5fcc42dd-1") %>% male_female_separation() %>% select(-Male)
df_all <- reshape(df_all, idvar=c("year","location"), timevar="age", direction="wide")


df <- merge(df_all,df_sd, by=c("year", "location"))
rm(df_all, df_sd)


head(df)

png("images/age_comparison.png")
ggplot(df) +
  geom_abline(slope=1, intercept=0, color = "gray", linewidth=2) +
  geom_smooth(aes(Female, Female.0), method=MASS::rlm, formula=y~x, color="black") + 
  geom_smooth(aes(Female, Female.1), method=MASS::rlm, formula=y~x, color="black") + 
  geom_smooth(aes(Female, Female.2), method=MASS::rlm, formula=y~x, color="black") + 
  geom_smooth(aes(Female, Female.3), method=MASS::rlm, formula=y~x, color="black") + 
  geom_smooth(aes(Female, Female.4), method=MASS::rlm, formula=y~x, color="black") + 
  geom_smooth(aes(Female, Female.5), method=MASS::rlm, formula=y~x, color="black") + 
  geom_smooth(aes(Female, Female.6), method=MASS::rlm, formula=y~x, color="black") + 
  geom_smooth(aes(Female, Female.7), method=MASS::rlm, formula=y~x, color="black") +
  geom_smooth(aes(Female, Female.8), method=MASS::rlm, formula=y~x, color="black") + 
  geom_smooth(aes(Female, Female.9), method=MASS::rlm, formula=y~x, color="black") + 
  geom_smooth(aes(Female, Female.10), method=MASS::rlm, formula=y~x, color="black") + 
  geom_smooth(aes(Female, Female.11), method=MASS::rlm, formula=y~x, color="black") + 
  geom_smooth(aes(Female, Female.12), method=MASS::rlm, formula=y~x, color="black") + 
  geom_smooth(aes(Female, Female.13), method=MASS::rlm, formula=y~x, color="black") + 
  geom_smooth(aes(Female, Female.14), method=MASS::rlm, formula=y~x, color="black") + 
  geom_smooth(aes(Female, Female.15), method=MASS::rlm, formula=y~x, color="black") +
  geom_smooth(aes(Female, Female.16), method=MASS::rlm, formula=y~x, color="black") + 
  geom_smooth(aes(Female, Female.17), method=MASS::rlm, formula=y~x, color="black") + 
  geom_smooth(aes(Female, Female.18), method=MASS::rlm, formula=y~x, color="black") +
  coord_fixed(ratio=1, xlim=c(x_min, x_max), ylim=c(x_min, x_max)) +
  labs(title="Comparaison entre l'âge standardisé et l'âge réele",
       caption = "Source : IHME, Global Burden of Disease (2019)") +
  xlab("Taux d'incidence chez les femmes (standarisée par âge)") +
  ylab("Taux d'incidence chez les femmes") +
  theme_classic()
dev.off()


