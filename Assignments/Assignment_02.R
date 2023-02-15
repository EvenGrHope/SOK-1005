#### Assignment 02 ####

# Clearing the environment.
rm(list=ls())

# Loading packages.
library(tidyverse)
library(rvest)
library(janitor)

# URL of the data
url <- "https://www.motor.no/aktuelt/motors-store-vintertest-av-rekkevidde-pa-elbiler/217132"

# Selecting the table i want and storing the data in a dataframe.
df_car <- url %>%
  read_html() %>%
  html_nodes("table") %>%
  html_table() %>% .[[1]]

# Setting row 1 as the title of the columns.
# Removing car models without values.
df_car <- df_car %>%
  row_to_names(row_number = 1) %>%
  subset(STOPP != "x")

# Seperating caharacters form the values i want to easily remove them.
df_car <- df_car %>%
  separate("WLTP-tall", into = c("WLTP-tall", "delete01"), sep = "km") %>%
  separate("STOPP", into = c("STOPP", "delete02"), sep = "km")

# Deleting the new columns.
df_car <- df_car %>%
  select(-c(delete01, delete02))

# Transforming values into numeric.
df_car <- df_car %>%
  mutate(`WLTP-tall` = as.numeric(`WLTP-tall`)) %>%
  mutate(STOPP = as.numeric(STOPP))

# Creating the figur.
fig <- df_car %>%
  ggplot(aes(x = `WLTP-tall`, y = STOPP)) +
  geom_point() +
  geom_smooth(method = lm) +
  xlim(200,620) +
  ylim(200, 620) +
  geom_abline(intercept = 0, slope = +1, color = "red", size = 1) +
  labs(title = "EL-bil rekkevidde", 
       subtitle = "I figuren kan vi se oppgitt rekkevidde fra bilprodusentene på x-aksen, og faktisk rekkevidde på y-aksen på vinteren. 
Den røde linjen representerer forventet rekkevidde.
Den blå linjen representerer hvordan rekkevidde ville sett ut med bruk av dataen vi har tilgjengelig (lineær regeresjon).",
       x = "Produsentens rekkevidde",
       y = "Faktisk rekkevidde")+
  theme_bw()

# Showing the figur.
fig

# Using lm function to find where the actuall expected milage should be. 
lm(STOPP ~ `WLTP-tall`, data = df_car)

# The "lm" function tells us where the line would "intercept" with the y-axis and
# the rate of increase (WLTP-tall). 
# The function in this example would be: -26.6450 + 0.8671x
  