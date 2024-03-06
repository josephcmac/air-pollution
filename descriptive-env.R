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
ggplot(df, aes(x=date,y=value)) +
	geom_point() +
	theme_classic() +
	theme(legend.position = "none") +
	facet_wrap(~location)

# only zero/non-zero
ggplot(df, aes(x=date,y=as.numeric(value > 0))) +
	geom_point() +
	theme_classic() +
	theme(legend.position = "none") +
	facet_wrap(~location)

# non-zero values
ggplot(df %>% filter(value > 0), aes(x=date,y=value)) +
	geom_point() +
	scale_y_log10() +
	theme_classic() +
	theme(legend.position = "none") +
	facet_wrap(~location)


# yearly data

min_nonzero <- function(x) {
  x_nonzero <- x[x > 0]
  if (length(x_nonzero) == 0) {
    return(0) 
  }
  min(x_nonzero, na.rm = TRUE) 
}


df_yearly <- df %>%
  mutate(year = floor_date(date, "year")) %>% 
  group_by(year, location) %>%
  summarise(p = mean(value > 0),  M = max(value), m = min_nonzero(value), N = length(value), N0 = sum(value > 0), .groups = "drop") %>% 
  filter(p > 0, M > 0, m > 0)

df_yearly$M <- df_yearly$M**(1/df_yearly$N)
df_yearly$m <- df_yearly$m**(1/df_yearly$N0) 

head(df_yearly)



# proportion of non-zero values
ggplot(df_yearly, aes(x=year,y=p)) +
	geom_point() +
	geom_line() +
	scale_y_log10() +
	theme_classic() +
	theme(legend.position = "none") +
	facet_wrap(~location)

# max values
ggplot(df_yearly, aes(x=year,y=M)) +
	geom_point() +
	geom_line() +
	theme_classic() +
	theme(legend.position = "none") +
	facet_wrap(~location)

# min nonzero values
ggplot(df_yearly, aes(x=year,y=m)) +
	geom_point() +
	geom_line() +	
	theme_classic() +
	theme(legend.position = "none") +
	facet_wrap(~location)





