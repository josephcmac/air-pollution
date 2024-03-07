library(tidyverse)

read_year <- function(parameter, year) {
	read.csv(paste0("../../datasets/daily_HAPS/daily_HAPS_",year,".csv")) %>%
		filter(Parameter.Name == parameter) %>%
		mutate(Parameter.Name = ifelse(Parameter.Name == "District Of Columbia",
            		"District of Columbia", Parameter.Name)) %>%
		filter(!(State.Name %in% c("Country Of Mexico", "Puerto Rico", "Virgin Islands"))) %>%
	    	select(Date.Local, State.Name, Arithmetic.Mean) %>%
    		mutate(Date.Local = as.Date(Date.Local)) %>%
    		group_by(Date.Local, State.Name) %>%
    		summarise(value = median(Arithmetic.Mean), .groups = "drop") %>%
		rename(date = Date.Local, location = State.Name)
}

df <- map_df(1980:2019, ~ read_year("Arsenic PM2.5 LC", .x)) %>% 
	mutate(location = as.factor(location))

glimpse(df)


# daily data

# all data
png("date-value.png")
ggplot(df, aes(x=date,y=value)) +
	geom_point() +
	theme_classic() +
	theme(legend.position = "none") +
	facet_wrap(~location)
dev.off()

# only zero/non-zero
png("date-zero-nonzero-value.png")
ggplot(df, aes(x=date,y=as.numeric(value > 0))) +
	geom_point() +
	theme_classic() +
	theme(legend.position = "none") +
	facet_wrap(~location)
dev.off()

# non-zero values
png("date-nonzero-value.png")
ggplot(df %>% filter(value > 0), aes(x=date,y=value)) +
	geom_point() +
	scale_y_log10() +
	theme_classic() +
	theme(legend.position = "none") +
	facet_wrap(~location)
dev.off()

# yearly data

min_nonzero <- function(x) {
  x_nonzero <- x[x > 0]
  if (length(x_nonzero) == 0) {
    return(0) 
  }
  min(x_nonzero, na.rm = TRUE) 
}

logit <- function(p) {
	log(p/(1-p))
}

df_yearly <- df %>%
  mutate(year = floor_date(date, "year")) %>% 
  group_by(year, location) %>%
  summarise(alpha = logit(mean(value > 0)), beta = log(min_nonzero(value)),  gamma = log(max(value)), N = length(value), N0 = sum(value > 0), .groups = "drop") 

head(df_yearly)


png("location-alpha.png")
ggplot(df_yearly, aes(x=year,y=alpha)) +
	geom_point() +
	geom_line() +
	theme_classic() +
	theme(legend.position = "none") +
	facet_wrap(~location)
dev.off()

png("location-beta.png")
ggplot(df_yearly, aes(x=year,y=beta)) +
	geom_point() +
	geom_line() +
	theme_classic() +
	theme(legend.position = "none") +
	facet_wrap(~location)
dev.off()

png("location-gamma.png")
ggplot(df_yearly, aes(x=year,y=gamma)) +
	geom_point() +
	geom_line() +	
	theme_classic() +
	theme(legend.position = "none") +
	facet_wrap(~location)
dev.off()




