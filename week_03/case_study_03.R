#install gapminder
install.packages("gapminder")

#load packages
library("ggplot2")
library("gapminder")
library("dplyr")

glimpse(gapminder)


#remove "Kuwait"
gapminder_filtered <- gapminder %>% 
  filter(country != "Kuwait")

# Create the plot
ggplot(gapminder_filtered, aes(x = gdpPercap, y = lifeExp, color = continent, size = pop / 100000)) +
  geom_point() +
  facet_wrap(~year, nrow = 1) +
  scale_y_continuous(trans = "sqrt") +
  theme_bw() +
  labs(
    x = "GDP per Capita",
    y = "Life Expectancy",
    size = "Population (100k)",
    color = "Continent",
    title = "Wealth and life expectancy through time(Excluding Kuwait)"
  )

#prepare the data for the second plot
gapminder_continent <-  gapminder_filtered %>%
  group_by(year, continent) %>%
  summarise(gdpPercapweighted = weighted.mean(x = gdpPercap, w = pop),
            pop = sum(as.numeric(pop)))

#Create the 2nd plot
ggplot() +
  geom_line(data = gapminder_filtered, aes(x = year, y = gdpPercap, group = country, color = continent)) + 
  geom_point(data = gapminder_filtered, aes(x = year, y = gdpPercap, group = country, color = continent)) +
  geom_line(data = gapminder_continent, aes(x = year, y = gdpPercapweighted)) + 
  geom_point(data = gapminder_continent, aes(x = year, y = gdpPercapweighted)) + 
  facet_wrap(~continent, nrow = 1) + 
  theme_bw() + 
  labs(
    x = "year",
    y = "gdpPercap",
    size = "Population(100k)",
    color = "Continent"
  )