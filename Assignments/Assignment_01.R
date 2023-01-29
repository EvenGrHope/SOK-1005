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
# Selecting only "Year", "Mo" and "Globe" temperature from each data frame.
df_lower_trop  <- df_lower_trop[1:529, 1:3]
df_mid_trop    <- df_mid_trop[1:529, 1:3]
df_trop        <- df_trop[1:529, 1:3]
df_lower_strat <- df_lower_strat[1:529, 1:3]

# Transforming character into numeric.
# Calculating the 12-month (right-aligned) moving average.
# Filtering out data before "Year" 1980.
df_lower_trop <- df_lower_trop %>% 
  mutate_at(vars(Globe), ~as.numeric(.)) %>%
  mutate(rollmean = zoo::rollmean(Globe, 12, fill = NA, 
                                  align = c("right"))) %>%
  filter(Year >= 1980)

df_mid_trop <- df_mid_trop %>% 
  mutate_at(vars(Globe), ~as.numeric(.)) %>%
  mutate(rollmean = zoo::rollmean(Globe, 12, fill = NA, 
                                  align = c("right"))) %>%
  filter(Year >= 1980)

df_trop <- df_trop %>% 
  mutate_at(vars(Globe), ~as.numeric(.)) %>%
  mutate(rollmean = zoo::rollmean(Globe, 12, fill = NA, 
                                  align = c("right"))) %>%
  filter(Year >= 1980)

df_lower_strat <- df_lower_strat %>% 
  mutate_at(vars(Globe), ~as.numeric(.)) %>%
  mutate(rollmean = zoo::rollmean(Globe, 12, fill = NA, 
                                  align = c("right"))) %>%
  filter(Year >= 1980)

# Calculating the average of the four 12-month moving averages.
df_lower_trop_av <- df_lower_trop %>%
  group_by(Year) %>%
  summarise(average = (mean(Globe)))

df_lower_strat_av <- df_lower_strat %>%
  group_by(Year) %>%
  summarise(average = (mean(Globe)))

df_trop_av <- df_trop %>%
  group_by(Year) %>%
  summarise(average = (mean(Globe)))

df_mid_trop_av <- df_mid_trop %>%
  group_by(Year) %>%
  summarise(average = (mean(Globe)))

# Plotting the graph.
ggplot() +
  geom_line(data = df_lower_trop_av,  
            aes(x = Year, y = average, group = 1), 
            linewidth = 1, color = "red",    alpha = 0.6) +
  geom_line(data = df_lower_strat_av, 
            aes(x = Year, y = average, group = 1), 
            linewidth = 1, color = "blue",   alpha = 0.6) +
  geom_line(data = df_trop_av,        
            aes(x = Year, y = average, group = 1), 
            linewidth = 1, color = "green",  alpha = 0.6) +
  geom_line(data = df_mid_trop_av,    
            aes(x = Year, y = average, group = 1), 
            linewidth = 1, color = "orange", alpha = 0.6) +
  geom_text(aes(x = 8, y = 0.6,      label = "Lower stratosphere"), color = "blue",   alpha = 0.6) +
  geom_text(aes(x = 10, y = -0.4,    label = "Lower troposphere"),  color = "red",    alpha = 0.6) +
  geom_text(aes(x = 17.4, y = -0.25, label = "Mid-troposphere"),    color = "orange", alpha = 1) +
  geom_text(aes(x = 8, y = 0.2,      label = "Troposphere"),        color = "green",  alpha = 1) +
  labs(x = "Year", y = "Average temperature (Deg. C)") +
  scale_x_discrete(guide = guide_axis(n.dodge=2)) +
  theme_bw()

