library(tidyverse)
library(scales)

# Reading
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

median_nonzero <- function(x) {
  x_nonzero <- x[x > 0]
  if (length(x_nonzero) == 0) {
    return(NA) 
  }
  median(x_nonzero) 
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


df <- map_df(1988:2019, ~ read_year(.x, parameter="Arsenic PM2.5 LC", sample_duration="24 HOUR")) %>% 
	mutate(location = as.factor(location))

df_yearly <- df %>%
  mutate(year = floor_date(date, "year") %>% year) %>% 
  group_by(year, location) %>%
  summarise(positive_proportion=mean(value>0), min_nonzero=min_nonzero(value), median_nonzero=median_nonzero(value), max_nonzero=max_nonzero(value), .groups = "drop") 

# Overview
# Overview (daily)
glimpse(df)

nrow(df)

length(unique(df$location))

length(unique(df$date))

summary(df)

(mean( df$value > 0 )*100) %>% round(0)

png("arsenic_pm25_lc_density.png", width = 1800, height = 1350, res = 300)
ggplot(df %>% filter(value > 0), aes(x = value)) +
        geom_density(fill = "gray", alpha = 0.5) +
        scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = exp_label) +
        labs(title = "Densité des valeurs non nulles pour la concentration 
	     quotidienne d'arsenic PM2,5 LC",
             subtitle = "Échelle logarithmique",
             x = "Concentration d'arsenic (µg/m³) - Valeurs non nulles",
             y = "Densité") +
        theme_classic() +
        theme(plot.title = element_text(hjust = 0.5),
              plot.subtitle = element_text(hjust = 0.5))
dev.off()


# Overview (yearly)

# Longitudinal
# Longitudinal (daily)

png("arsenic_pm25_lc_region.png", width = 2500, height = 3000, res = 300)
ggplot(df, aes(x=date, y=value)) +
  geom_point(pch=20) +
  labs(title = "Série chronologique de la concentration en arsenic PM2,5 LC par région",
       y = "Concentration d'arsenic (µg/m³)",
       x = "Date (jour)") +
  theme_classic() +
 facet_wrap(~location, ncol = 5)
dev.off()


png("arsenic_pm25_lc_region_nonzero.png", width = 2500, height = 3000, res = 300)
ggplot(df %>% filter(value > 0), aes(x=date, y=value)) +
	geom_point(pch=20, alpha=0.01) +
	scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = exp_label) +
	labs(title = "Série chronologique de la concentration non nulle en arsenic PM2,5 LC par région",
     		subtitle = "Échelle logarithmique, opacité 1 %",
		y = "Concentration d'arsenic (µg/m³)",
		x = "Date (jour)") +
	theme_classic() +
	facet_wrap(~location, ncol = 5)
dev.off()


# Longitudinal (yearly)
ggplot(df_yearly, aes(year, positive_proportion)) + 
	geom_point(color="gray") +
       	geom_smooth(color="black") +	
	theme_classic() +
	facet_wrap(~location)

ggplot(df_yearly %>% filter(!is.na(max_nonzero))) + 
	geom_point(color="gray", aes(year, max_nonzero)) +
	geom_point(color="gray", aes(year, median_nonzero)) +
       	geom_point(color="gray", aes(year, min_nonzero)) +
       	geom_smooth(color="black", method="loess", formula = "y ~ x", aes(year, max_nonzero)) +
	geom_smooth(color="black", method="loess", formula = "y ~ x", aes(year, median_nonzero)) +
       	geom_smooth(color="black", method="loess", formula = "y ~ x", aes(year, min_nonzero)) +	
	scale_y_log10() +	
	theme_classic() +
	ylab("value") + 
	facet_wrap(~location)

# Cross-sectional
ggplot(df %>% filter(year(date) == 2000)) +
	geom_boxplot(aes(location, value)) +
	coord_flip() +
	theme_classic()

ggplot(df %>% filter(value > 0, year(date) == 2000)) +
	geom_boxplot(aes(location, value)) +
	scale_y_log10() +
	coord_flip() +
	theme_classic()

ggplot(df_yearly) +
       geom_density(aes(positive_proportion)) +
       theme_classic() +
       facet_wrap(~year)

ggplot(df_yearly %>% filter(!is.na(min_nonzero))) +
       geom_density(aes(min_nonzero)) +
       scale_x_log10() +
       theme_classic() +
       facet_wrap(~year)

ggplot(df_yearly %>% filter(!is.na(max_nonzero))) +
       geom_density(aes(max_nonzero)) +
       scale_x_log10() +
       theme_classic() +
       facet_wrap(~year)


