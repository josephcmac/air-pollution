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
png("arsenic_pm25_lc_region_positive_proportion.png", width = 3000, height = 3000, res = 300)
ggplot(df_yearly, aes(year, positive_proportion)) + 
	geom_point(color="gray") +
       	geom_smooth(color="black", method = "loess", formula = "y ~ x") +
	labs(title = "Série chronologique de proportion de valeurs positives",
	     x = "Année",
	     y = "Proportion de valeurs positives") +
	theme_classic() +
	facet_wrap(~location, ncol = 5)
dev.off()



# Cross-sectional
png("arsenic_pm25_lc_region_nonzero_density_1990.png", width = 3000, height = 3000, res = 300)
ggplot(df %>% filter(value > 0, year(date) == 1990), aes(x=value)) +
	geom_density() +
	scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = exp_label) +
	labs(	title = "Densité des valeurs non nulles pour la concentration quotidienne d'arsenic PM2,5 LC",
		subtitle = "Année 1990",
		x = "Concentration d'arsenic (µg/m³) - Valeurs non nulles",
		y = "Densité") +
	theme_classic() +
	facet_wrap(~location, ncol = 5)
dev.off()

png("arsenic_pm25_lc_region_nonzero_density_1995.png", width = 3000, height = 3000, res = 300)
ggplot(df %>% filter(value > 0, year(date) == 1995), aes(x=value)) +
	geom_density() +
	scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = exp_label) +
	labs(	title = "Densité des valeurs non nulles pour la concentration quotidienne d'arsenic PM2,5 LC",
		subtitle = "Année 1995",
		x = "Concentration d'arsenic (µg/m³) - Valeurs non nulles",
		y = "Densité") +
	theme_classic() +
	facet_wrap(~location, ncol = 5)
dev.off()

png("arsenic_pm25_lc_region_nonzero_density_2000.png", width = 3000, height = 3000, res = 300)
ggplot(df %>% filter(value > 0, year(date) == 2000), aes(x=value)) +
	geom_density() +
	scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = exp_label) +
	labs(	title = "Densité des valeurs non nulles pour la concentration quotidienne d'arsenic PM2,5 LC",
		subtitle = "Année 2000",
		x = "Concentration d'arsenic (µg/m³) - Valeurs non nulles",
		y = "Densité") +
	theme_classic() +
	facet_wrap(~location, ncol = 5)
dev.off()


png("arsenic_pm25_lc_region_nonzero_density_2005.png", width = 3000, height = 3000, res = 300)
ggplot(df %>% filter(value > 0, year(date) == 2005), aes(x=value)) +
	geom_density() +
	scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = exp_label) +
	labs(	title = "Densité des valeurs non nulles pour la concentration quotidienne d'arsenic PM2,5 LC",
		subtitle = "Année 2005",
		x = "Concentration d'arsenic (µg/m³) - Valeurs non nulles",
		y = "Densité") +
	theme_classic() +
	facet_wrap(~location, ncol = 5)
dev.off()


png("arsenic_pm25_lc_region_nonzero_density_2010.png", width = 3000, height = 3000, res = 300)
ggplot(df %>% filter(value > 0, year(date) == 2010), aes(x=value)) +
	geom_density() +
	scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = exp_label) +
	labs(	title = "Densité des valeurs non nulles pour la concentration quotidienne d'arsenic PM2,5 LC",
		subtitle = "Année 2010",
		x = "Concentration d'arsenic (µg/m³) - Valeurs non nulles",
		y = "Densité") +
	theme_classic() +
	facet_wrap(~location, ncol = 5)
dev.off()

png("arsenic_pm25_lc_region_nonzero_density_2015.png", width = 3000, height = 3000, res = 300)
ggplot(df %>% filter(value > 0, year(date) == 2015), aes(x=value)) +
	geom_density() +
	scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = exp_label) +
	labs(	title = "Densité des valeurs non nulles pour la concentration quotidienne d'arsenic PM2,5 LC",
		subtitle = "Année 2015",
		x = "Concentration d'arsenic (µg/m³) - Valeurs non nulles",
		y = "Densité") +
	theme_classic() +
	facet_wrap(~location, ncol = 5)
dev.off()


png("arsenic_pm25_lc_year_positive.png", width = 3000, height = 3000, res = 300)
ggplot(df_yearly) +
       geom_density(aes(positive_proportion)) + 
	labs(	title = "Densité des proportions des valeurs positives de la concentration d'arsenic PM2,5 LC",
		subtitle = "Stratification par année",
		x = "Proportion des valeurs positives",
		y = "Densité") +
       theme_classic() +
       facet_wrap(~year)
dev.off()


png("arsenic_pm25_lc_min_nonzero.png", width = 3000, height = 4000, res = 300)
ggplot(df_yearly %>% filter(! is.na(min_nonzero)  )) +
geom_density(aes(min_nonzero)) +
scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = exp_label) +
labs(	title = "Densité des minimums des valeurs positifs annuels de la concentration d'arsenic PM2,5 LC",
	subtitle = "Stratification par année",
	x = "Minimums des valeurs positives annuels",
	y = "Densité") +
theme_classic() +
facet_wrap(~year, ncol = 4)
dev.off()


png("arsenic_pm25_lc_max_nonzero.png", width = 3000, height = 4000, res = 300)
ggplot(df_yearly %>% filter(! is.na(max_nonzero)  )) +
geom_density(aes(max_nonzero)) +
scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = exp_label) +
labs(	title = "Densité des maximums des valeurs positives  annuels de la concentration d'arsenic PM2,5 LC",
	subtitle = "Stratification par année",
	x = "Maximums des valeurs positives annuels",
	y = "Densité") +
theme_classic() +
facet_wrap(~year, ncol=4)
dev.off()


