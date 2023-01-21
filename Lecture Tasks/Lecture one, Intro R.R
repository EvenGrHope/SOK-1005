
##############################
#'  Forelesning 1 
##############################
#' `Objective`: learn/review basic commands from `dplyr`
#' 
#' R makes use of the # or #' signs to add comments, 
#' so that you and others can understand what the R code 
#' is about.

##########################################################
##########################################################
######
#' `dplyr- Basic data manipulation`
#' browseURL("https://r4ds.had.co.nz/transform.html")
#' `filter()` - Pick observations (rows) by their values   
#' `select()` -  Pick variables (columns) by their names  
#' `mutate()` - Create new variables (columns) with functions of existing variables  
#' `arrange()`-  Reorder rows using  
#' `summarise()` - Collapse many values down to a single summary 
#' `rename()`  - Rename columns (variables):
#' `group_by()` - Group rows by columns (variables)
#' 
##########################################################
#############################################################
########


rm (list = ls())
library(tidyverse)
# Use the gapminder data-set from the gapminder package
library(gapminder) 
data("gapminder")


# Some Questions:

#' Q0. From the gapminder dataset, select country, year & pop 

gapminder_01 <- gapminder %>%
  select(country, year, pop)


#' Q1: From the gapminder dataset, filter out the data from Norway before and including 1977.

gapminder_02 <- gapminder %>%
  filter(country == "Norway", year > 1977)


#' Q2: Filter the data from Norway or Sweden before and including 1970.

gapminder_03 <- gapminder %>%
  filter(country %in% c("Norway", "Sweden")) %>%
  filter(year > 1970)


#' Q3. Filter the data from Norway, Sweden, or Denmark before and including 1970.

gapminder_04 <- gapminder %>%
  filter(country %in% c("Norway", "Sweden", "Denmark")) %>%
  filter(year > 1970)


#' Q4. Following Q3. Let us say you do not like long variable names such as "gdpPercap".  
#' Rename "gdpPercap" by gdp_pc        

gapminder_04 <- gapminder_04 %>%
  rename("gdp_pc" = "gdpPercap")


#' Q5. Following Q4. Arrange rows according to ascending order of "gdp_pc"

gapminder_04 <- gapminder_04 %>%
  arrange(gdp_pc)


#' Q6. Following Q5. Arrange rows according to descending order of "gdp_pc"

gapminder_04 <- gapminder_04 %>%
  arrange(desc(gdp_pc))


#' Q7. Arrange rows according to ascending order of "gdp_pc", within each year. 

gapminder_04 <- gapminder_04 %>%
  arrange(year, gdp_pc)


#'Q8.Data from Norway.   
#' Create a new variable that is GDP  from gdpPercap & pop

norway_01 <- gapminder %>%
  filter(country == "Norway") %>%
  mutate(GDP = gdpPercap * pop)


#' Q9. Data from Norway. 
#' Create a new variable called "gdpNOK" that is GDP per per billion NOK (1 000 000 000 NOK) 
#' (1 USD=9 NOK)

norway_02 <- norway_01 %>%
  mutate(gdpNOK = (GDP * 9)/1e9)


#' Q10.Use mutate and ifelse to Categorise "gdpNOK" into 3 categories,
#'  (i.e., less than or equal to 999, between 1000 and 1999, and greater than 2000).

norway_03 <- norway_02 %>%
  mutate(categorie = ifelse(gdpNOK <= 999, "Less then a billion",
                            ifelse(gdpNOK > 1000 & gdpNOK <= 1999, "Between a billion and two billion",
                            ifelse(gdpNOK >= 2000, "Larger than two billion", NA))))


#' Q11. Calculate the average lifExp of all three Nordic countries
#' (i.e., Norway, Sweden, Denmark)

gapminder_05 <- gapminder_04 %>%
  summarise(mean_lifeexp = mean(lifeExp))


#' Q12. Calculate the average lifExp of the three countries,per country. 

gapminder_06 <- gapminder_04 %>%
  group_by(country) %>%
  summarise(mean_lifeexp = mean(lifeExp))


#' Q13. Calculate mean life expectancy per country 
#' (i.e., for every country in the gapminder data set)

life_exp_country <- gapminder %>%
  group_by(country) %>%
  summarise(mean_lifeexp = mean(lifeExp))


#' Q14. Calculate mean life expectancy per continent.

life_exp_continent <- gapminder %>%
  group_by(continent) %>%
  summarise(mean_lifeexp = mean(lifeExp))


#' Q15. calculate mean life expectancy by continent & country

life_exp_continent_and_country <- gapminder %>%
  group_by(continent, country) %>%
  summarise(mean_lifeexp = mean(lifeExp), .groups = ("drop"))


#' Q16. Calculate mean life expectancy by continent & add min and max lifeExp

life_exp_continent_min_max <- gapminder %>%
  group_by(continent) %>%
  summarise(max = max(lifeExp),
            min = min(lifeExp),
            mean = mean(lifeExp))

# Q17. Scatter plot of gdpPercap vs lifeExp 

gapminder %>%
  ggplot(aes(x = gdpPercap, y = lifeExp)) + 
  geom_point(alpha=0.4) +
  ggtitle("gdpPercap vs life Expectancy")


# Q18. Scatter plot of gdpPercap vs lifeExp by Continent

gapminder %>%
  ggplot(aes(x = gdpPercap, y = lifeExp, color=continent)) + 
  geom_point(alpha=0.4) +
  ggtitle("Life Expectancy and GDP by Continent")


# Q19. Scatter plot of gdpPercap vs lifeExp by Continent. Use different shapes per continent

gapminder %>%
  ggplot(aes(x = gdpPercap, y = lifeExp, color=continent, shape = continent)) + 
  geom_point(alpha=0.4) +
  ggtitle("Life Expectancy and GDP by Continent")

