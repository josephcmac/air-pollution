library(tidyverse)

read_year <- function(year, parameter, sample_duration) {
        read.csv(paste0("../../datasets/daily_HAPS/daily_HAPS_",year,".csv")) %>%
                filter(Parameter.Name == parameter, Sample.Duration == sample_duration) %>%
                filter(!(State.Name %in% c("Country Of Mexico", "Puerto Rico", "Virgin Islands"))) %>%
                mutate(State.Name = ifelse(State.Name == "District Of Columbia",
                        "District of Columbia", State.Name)) %>%
                select(Date.Local, State.Name, X1st.Max.Value) %>%
                mutate(Date.Local = as.Date(Date.Local)) %>%
                group_by(Date.Local, State.Name) %>%
                summarise(value = ifelse(length(X1st.Max.Value) > 0, max(X1st.Max.Value), NA), .groups = "drop") %>%
                mutate(value = sapply(value, function(x) max(x,0))) %>%
                rename(date = Date.Local, location = State.Name)
}

min_nonzero <- function(x) {
  x_nonzero <- x[x > 0]
  if (length(x_nonzero) == 0) {
    return(NA)
  }
  min(x_nonzero)
}

geom_mean_nonzero <- function(x) {
  x_nonzero <- x[x > 0]
  if (length(x_nonzero) == 0) {
    return(NA)
  }
  x_nonzero %>% log %>% mean %>% exp
}

max_nonzero <- function(x) {
  x_nonzero <- x[x > 0]
  if (length(x_nonzero) == 0) {
    return(NA) 
  }
  max(x_nonzero) 
}

exp_label <- function(x) {
  parse(text = sprintf("10^%s", as.character(log10(x))))
}

read_file_health <- function(filename) {
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

read_combination<- function(latency) {
  df_env <- map_df(1988:(2019-latency), ~ read_year(.x, parameter="Arsenic PM2.5 LC", sample_duration="24 HOUR")) %>% 
        mutate(location = as.factor(location))

  df_env_yearly <- df_env %>%
    mutate(year = floor_date(date, "year") %>% year) %>% 
    group_by(year, location) %>%
    summarise(positive_proportion=mean(value>0), 
            min_nonzero=min_nonzero(value), 
            geom_mean_nonzero=geom_mean_nonzero(value), 
            max_nonzero=max_nonzero(value), .groups = "drop") 
  rm(df_env)
  gc()

  df_env_yearly$year <- df_env_yearly$year + latency 

  df_health <- read_file_health("IHME-GBD_2019_DATA-5fcc42dd-1")

  df_health <- male_female_separation(df_health)

  df <- merge(df_env_yearly, df_health)

  rm(df_env_yearly, df_health)
  gc()

  return(df)
}

logit <- function(x) {
  log(x/(1-x))
}

comp_p <- function(df, year0) {
  df_year <- df %>% filter(year == year0) %>% select(-year) %>% na.omit()
  cor.test(x=df_year$geom_mean_nonzero, y=df_year$Female)$p.value
}

compute_p <- function(df) {
  years <- df$year %>% unique %>% sort
  data.frame(year = years, p_value = sapply(years, function(year) comp_p(df, year)))
}

df <- read_combination(3)

p_values <- compute_p(df)

ggplot(p_values, aes(x=year, y=logit(p_value))) +
  geom_point(color="gray") +
  geom_smooth(color="black", method = lm, formula=y~splines::bs(x, 3)) +
  geom_hline(yintercept = log(0.05/(0.95)), linetype = "dashed", color="lightgray") +
  theme_classic()


visualize_median <- function(df, p_values, column_name) {
  col_sym <- rlang::ensym(column_name)
  df_median <- df %>% 
    na.omit() %>%
    group_by(year) %>%
    summarise(value = median(!!col_sym))

  df_median <- merge(df_median, p_values)

  ggplot(df_median, aes(x=logit(value), y=logit(p_value))) +
    geom_point(color="gray") +
    geom_smooth(color="black", method = MASS::rlm, formula=y~x) +
    geom_hline(yintercept = log(0.05/(0.95)), linetype = "dashed", color="black") +
    theme_classic()
}

visualize_median(df, p_values, "positive_proportion")
visualize_median(df, p_values, "min_nonzero")
visualize_median(df, p_values, "max_nonzero")
visualize_median(df, p_values, "geom_mean_nonzero")



