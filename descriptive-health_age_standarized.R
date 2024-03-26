library(tidyverse)

read_file <- function(filename) {
  read.csv(paste0("../../datasets/IHME_GHDx/Incidence/", filename,"/", filename, ".csv")) %>%
	  select(location, sex, age, year, val) %>%
	  rename(incidence = val) %>% 
	  filter(age =="Age-standardized") %>%
	  select(-age) %>%
	  mutate(sex = as.factor(sex), location = as.factor(location))
}

exp_label <- function(x) {
  parse(text = sprintf("10^%s", as.character(log10(x))))
}

male_female_separation <- function(df) {
  df_Male <- df %>% filter(sex=="Male") %>% select(-sex) %>% rename(Male = incidence)
  df_Female <- df %>% filter(sex=="Female") %>% select(-sex) %>% rename(Female = incidence)
  return(merge(df_Male, df_Female))
}

df <- read_file("IHME-GBD_2019_DATA-5fcc42dd-1")

df %>% glimpse

df %>% summary

df_sex <- male_female_separation(df)
rm(df)

x_min <- min(c(df_sex$Male, df_sex$Female))
x_max <- max(c(df_sex$Male, df_sex$Female))

png("images/male_female_age_standarized.png")
df_sex %>% 
  ggplot(aes(x = Male, y = Female)) + 
  geom_point(alpha = 0.5, color="gray") +
  geom_smooth(method=MASS::rlm, formula = y ~ x, color = "black") +
  geom_abline(intercept = 0, slope = 1, linetype="dashed", color = "black", linewidth = 2) + 
  scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x), labels = exp_label) +
  scale_y_log10(breaks = scales::trans_breaks("log10", function(y) 10^y), labels = exp_label) +
  labs(title = "Taux d'incidence des maladies respiratoires chroniques",
       subtitle = "Standardisé par âge",
       x = "Hommes", 
       y = "Femmes",
       caption = "Source : IHME, Global Burden of Disease (2019)") +
  theme_classic() + 
  coord_fixed(ratio=1, xlim=c(x_min, x_max), ylim=c(x_min, x_max))
dev.off()


png("images/incidence_year_age_standarized.png", width=600, height=1000)
df_sex %>% 
  ggplot(aes(x=year, y = Female)) + 
  geom_line() + 
  scale_y_log10(breaks = scales::trans_breaks("log10", function(y) 10^y), labels = exp_label) +
  labs(title = "Taux d'incidence des maladies respiratoires chroniques",
       subtitle = "Standardisé par âge",
       x = "Année", 
       y = "Femmes",
       caption = "Source : IHME, Global Burden of Disease (2019)") +
  theme_classic() +
  facet_wrap(~location, ncol=6, scales = "fixed") +
  theme(panel.spacing = unit(1, "lines"), axis.text.x = element_text(size = 6)) 
dev.off()


