#### Assignment 01 ####

# Clearing the environment.
rm(list=ls())

# Loading packages.
library(tidyverse)
library(lubridate)
library(zoo)

# Loading data.
df_lower_trop  <- read_table2("http://vortex.nsstc.uah.edu/data/msu/v6.0/tlt/uahncdc_lt_6.0.txt")
df_mid_trop    <- read_table2("http://vortex.nsstc.uah.edu/data/msu/v6.0/tmt/uahncdc_mt_6.0.txt")
df_trop        <- read_table2("http://vortex.nsstc.uah.edu/data/msu/v6.0/ttp/uahncdc_tp_6.0.txt")
df_lower_strat <- read_table2("http://vortex.nsstc.uah.edu/data/msu/v6.0/tls/uahncdc_ls_6.0.txt")

# Removing the notes at the bottom of the data frames.
df_lower_trop  <- df_lower_trop [1:which (df_lower_trop$Year %in% "Year")-1, ]
df_mid_trop    <- df_mid_trop   [1:which   (df_mid_trop$Year %in% "Year")-1, ]
df_trop        <- df_trop       [1:which       (df_trop$Year %in% "Year")-1, ]
df_lower_strat <- df_lower_strat[1:which(df_lower_strat$Year %in% "Year")-1, ]

# Creating two new variables, named "location" and "date" in the data frames to separate them after the merge.
# Selecting only the "globe", "location" and "date" variables.
df_lower_trop <- df_lower_trop %>%
  mutate(Location = "Lower Troposphere") %>%
  mutate(Date = ymd(paste(df_lower_trop$Year, df_lower_trop$Mo, 1, sep = "-"))) %>%
  select(Date, Globe, Location)

df_mid_trop <- df_mid_trop %>%
  mutate(Location = "Mid Troposphere") %>%
  mutate(Date = ymd(paste(df_mid_trop$Year, df_mid_trop$Mo, 1, sep = "-"))) %>%
  select(Date, Globe, Location)

df_trop <- df_trop %>%
  mutate(Location = "Tropopause") %>%
  mutate(Date = ymd(paste(df_trop$Year, df_trop$Mo, 1, sep = "-"))) %>%
  select(Date, Globe, Location)

df_lower_strat <- df_lower_strat %>%
  mutate(Location = "Lower Stratosphere") %>%
  mutate(Date = ymd(paste(df_lower_strat$Year, df_lower_strat$Mo, 1, sep = "-"))) %>%
  select(Date, Globe, Location)

# Merging the four data frames into one.
df_merged <- rbind(df_lower_trop, df_mid_trop, df_trop, df_lower_strat)

# Transforming character into numeric.
df_merged <- df_merged %>%
  mutate(Globe = as.numeric(Globe))

# Creating a new data frame containing the average "globe" temperature.
df_average <- df_merged %>%
  group_by(Date) %>%
  summarise(Globe = mean(Globe)) %>%
  mutate(Location = "Average")

# Merging the "average" whit the "merged" to create the finished data frame.
df <- rbind(df_merged, df_average)

# Calculating the 12-month(right aligned) moving average.
# Filtering out data from before 1980.
df <- df %>%
  group_by(Location) %>%
  mutate(Rollmean = rollmean(Globe, 12, fill = NA, align = c("right"))) %>%
  filter(Date >= "1980-01-01")

# Plotting the graph, with the "average" highlighted in a thicker line.
df %>%
  ggplot(aes(x = Date, y = Rollmean, color = Location)) +
  geom_line(aes(), alpha = 0.6, size = 1) +
  geom_line(data = filter(df, Location == "Average"), size = 1.2) +
  labs(x = "Year", y = "Temprature (deg. C)", 
       title = "12-months average global temprature over time",
       subtitle = "How does the average global temp. from different layers in the atmosphere change over time?",
       color = "Atmosphere layer") +
  theme_bw()