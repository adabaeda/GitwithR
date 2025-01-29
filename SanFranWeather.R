

#====================================================================
#                     Session 7: CA-SF Data
#====================================================================


library(lubridate)
library(tidyverse)
library(maps)

setwd("~/R/R_Practice")






sfweather <- read_csv("3690530.csv", col_types="ccDdddddddddd") %>%
  select(DATE, starts_with("T"), PRCP, SNOW, SNWD) %>%
  rename("date" = "DATE",
         "t_max_f" = "TMAX",
         "t_min_f" = "TMIN",
         "t_obs_f" = "TOBS",
         "total_precip_mm" = "PRCP",
         "snow_fall_mm" = "SNOW",
         "snow_depth_mm" = "SNWD")
 

sfweather
sfweather %>% ggplot(aes(x=t_min_f)) + geom_histogram()
sfweather %>% ggplot(aes(x=t_max_f)) + geom_histogram()
sfweather %>% ggplot(aes(x=t_obs_f)) + geom_histogram()


sfweather %>%
  mutate(t_obs_c = ifelse(t_obs_f > 110, NA, t_obs_f),
         t_obs_c = ifelse(t_obs_f < -25, NA, t_obs_f)) %>%
  ggplot(aes(x=t_obs_f)) +
  geom_histogram()

#==============================================================

sfweather %>%
  ggplot(aes(total_precip_mm)) + geom_histogram()

sfweather %>%
  ggplot(aes(x=t_obs_c)) +
  geom_histogram()

sfweather %>%
  ggplot(aes(x=date, y=total_precip_mm)) +
  geom_line()

# dry dry dry dry dry dry dry dry dry dry dry


sfweather <- read_csv("3690530.csv", col_types="ccDdddddddddd") %>%
  select(DATE, starts_with("T"), PRCP, SNOW, SNWD) %>%
  rename("date" = "DATE",
         "t_max_f" = "TMAX",
         "t_min_f" = "TMIN",
         "t_obs_f" = "TOBS",
         "total_precip_mm" = "PRCP",
         "snow_fall_mm" = "SNOW",
         "snow_depth_mm" = "SNWD")

colnames(sfweather)
sfweather %>%
  ggplot(aes(x=date, y=t_max_f)) +
  geom_line()

#working with dates using lubridate
head(sfweather)

#dates are in yyyy-mm-dd
ada_birthday <- "1999-02-08"
year(ada_birthday)
month(ada_birthday)
day(ada_birthday)
week(ada_birthday)
wday(ada_birthday)
week(ada_birthday)

annual_precipitation <- sfweather %>%
  mutate(year = year(date))%>%
  group_by(year) %>%
  filter(year != 1925) %>%
  summarize(annual_precip = sum(total_precip_mm, na.rm= TRUE))

annual_precipitation %>%
  ggplot(aes(x=year, y=annual_precip)) +
  geom_line() +
  geom_smooth()


# adding month and day columns, grouping those columns together, summarizing the temperature
# by looking at the median, and setting confidence intervals

daily_temps <- sfweather %>%
  mutate(month = month(date),
         day = day(date)) %>%
  group_by(month, day) %>%
  summarize(t_med_f = median(t_max_f),
            t_lci_f = quantile(t_max_f, prob=0.025, na.rm = TRUE),
            t_uci_f = quantile(t_max_f, prob=0.975, na.rm = TRUE),
            n = n()) %>%
  ungroup()

daily_temps %>%
  mutate(date = ymd(paste(2020, month, day, sep='-'))) %>%
  ggplot(aes(x=date, y=t_med_f, ymin=t_lci_f, ymax=t_uci_f)) +
  geom_errorbar(color="grey") +
  geom_line(color="black") 

#4. Make a line plot of the average daily high temperature

sfweather %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  filter(year != 1925 & year != 2024) %>%
  summarise(avg_high_temp = mean(t_max_f, na.rm = TRUE))%>%
  ggplot(aes(x=year, y=avg_high_temp)) +
  geom_line()+
  geom_smooth()

# 5. Make a scatter plot of the total yearly precipitation on the y-axis
# and the average annual high temperature on the x-axis. Plot the year using
# the color aesthetic.

colnames(sfweather)
sfweather %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  filter(year != 1925 & year != 2024) %>%
  summarise(yearly_precip = sum(total_precip_mm, na.rm = TRUE),
            t_mean_f = mean(t_max_f, na.rm = TRUE)) %>%
  ggplot(aes(x=t_mean_f, y=yearly_precip, color=year))+
  geom_point()

