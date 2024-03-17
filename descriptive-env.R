library(tidyverse)
library(scales)

# Preliminaries
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


# Reading
df <- map_df(1988:2023, ~ read_year(.x, parameter="Arsenic PM2.5 LC", sample_duration="24 HOUR")) %>% 
	mutate(location = as.factor(location))

df_yearly <- df %>%
  mutate(year = floor_date(date, "year") %>% year) %>% 
  group_by(year, location) %>%
  summarise(positive_proportion=mean(value>0), 
	    min_nonzero=min_nonzero(value), 
	    geom_mean_nonzero=geom_mean_nonzero(value), 
	    max_nonzero=max_nonzero(value), .groups = "drop") 

# Overview
glimpse(df)

nrow(df)

length(unique(df$location))

length(unique(df$date))

summary(df)

(mean( df$value > 0 )*100) %>% round(0)

summary(df_yearly)

# Longitudinal study (microscopic)
df %>% 
	mutate(year = floor_date(date, "year") %>% year %>% sapply(function(x) 10*round(x/10))) %>%
	filter(value > 0) %>%
	ggplot()  +
	geom_boxplot(aes(x=as.factor(year), y=value)) +
	scale_y_log10(breaks = trans_breaks("log10", function(y) 10^y), labels = exp_label) +
	labs(title = "Diagrammes de moustache pour une durée de 10 ans",
	     x = "Année",
	     y = "Valeurs positives") +
	theme_classic() +
	facet_wrap(~location, ncol=6)


# Longitudinal study (macroscopic)

ggplot(df_yearly, aes(year, positive_proportion)) + 
	geom_point(color="gray") +
       	geom_line(color ="black") +
	labs(title = "Série chronologique de proportion de valeurs positives",
	     x = "Année",
	     y = "Proportion de valeurs positives") +
	theme_classic() +
	facet_wrap(~location, ncol = 6)

ggplot(df_yearly %>% na.omit) + 
	geom_point(color="gray", aes(year, max_nonzero)) +
	geom_point(color="gray", aes(year, min_nonzero)) +
       	geom_line(color ="black", aes(year, max_nonzero)) +
       	geom_line(color ="black", aes(year, min_nonzero)) +
	scale_y_log10(breaks = trans_breaks("log10", function(y) 10^y), labels = exp_label) +
	labs(title = "Série chronologique de valeur positive maximale/minimale",
		x = "Année",
		y = "Valeur positive maximale/minimale") +
	theme_classic() +
	facet_wrap(~location, ncol = 6)

ggplot(df_yearly %>% na.omit) + 
	geom_point(color="gray", aes(year, geom_mean_nonzero)) +
       	geom_line(color ="black", aes(year, geom_mean_nonzero)) +
	scale_y_log10(breaks = trans_breaks("log10", function(y) 10^y), labels = exp_label) +
	labs(title = "Série chronologique moyenne géométrique",
		x = "Année",
		y = "Moyenne géométrique") +
	theme_classic() +
	facet_wrap(~location, ncol = 6)




