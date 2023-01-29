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
df_lower_trop  <- df_lower_trop[1:529, ]
df_mid_trop    <- df_mid_trop[1:529, ]
df_trop        <- df_trop[1:529, ]
df_lower_strat <- df_lower_strat[1:529, ]

# Selecting only "Year", "Mo" and "Globe" temperature from each data frame.
df_lower_trop  <- df_lower_trop %>%
  select("Year", "Mo", "Globe")

df_mid_trop    <- df_mid_trop %>%
  select("Year", "Mo", "Globe")
  
df_trop        <- df_trop %>%
  select("Year", "Mo", "Globe")

df_lower_strat <- df_lower_strat %>%
  select("Year", "Mo", "Globe")

# Transforming character into numeric.
df_lower_trop <- df_lower_trop %>% 
  mutate_at(vars(Globe), ~as.numeric(.))

df_mid_trop <- df_mid_trop %>% 
  mutate_at(vars(Globe), ~as.numeric(.))

df_trop <- df_trop %>% 
  mutate_at(vars(Globe), ~as.numeric(.))

df_lower_strat <- df_lower_strat %>% 
  mutate_at(vars(Globe), ~as.numeric(.))

# Calculating the 12-month (right-aligned) moving average.
df_lower_trop  <- df_lower_trop %>%
  mutate(rollmean = zoo::rollmean(Globe, 12, fill = NA, align = c("right")))

df_mid_trop  <- df_mid_trop %>%
  mutate(rollmean = zoo::rollmean(Globe, 12, fill = NA, align = c("right")))

df_trop  <- df_trop %>%
  mutate(rollmean = zoo::rollmean(Globe, 12, fill = NA, align = c("right")))

df_lower_strat  <- df_lower_strat %>%
  mutate(rollmean = zoo::rollmean(Globe, 12, fill = NA, align = c("right")))

# Calculating the average of the four 12-month moving averages.


# Filtering out data before "Year" 1980.
df_lower_trop  <- df_lower_trop %>%
  filter(Year >= 1980)

df_mid_trop    <- df_mid_trop %>%
  filter(Year >= 1980)

df_trop        <- df_trop %>%
  filter(Year >= 1980)

df_lower_strat <- df_lower_strat %>%
  filter(Year >= 1980)
  







