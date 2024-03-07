#Problem Set 1
#Bern DySart
#January 22, 2024
#SIS 750 with Austin Hart

#load packages
install.packages("gapminder")
library(gapminder)
library(tidyverse)
library(stargazer)
library(haven)
library(knitr)
str(gapminder_unfiltered)

#Question 1---------
gapminder = gapminder_unfiltered
#gapminder_unfilterd contains data from years 1950 - 2007. 
#There are 3,313 observations in the dataset. There are 6 variables in the dataset. 
#Every country is covered in the data set for each year. 

#Question 2--------
plot(lifeExp~year, data = gapminder,
     xlab = 'Year',
     ylab = 'Life Expectancy',
     main = 'Life Expectancy Trends')
model1 =
  lm(lifeExp ~ year, data = gapminder)
abline(model1,col = 'green', lw = 8)

#Question 3--------
outcome =
  count(gapminder, continent)
kable(outcome)

#Question 4--------
p <- ggplot(gapminder,
            aes(x = gdpPercap, y = lifeExp)) # just initializes
p <- p + scale_x_log10() # log the x axis the right way
p + geom_point(alpha = (1/3), size = 3) + facet_wrap(~ continent) +
  geom_smooth(lwd = 1.5, se = FALSE)

#Question 5--------
flights |>
  filter(dest=="IAH") |>
  group_by(year,month,day) |>
  summarize(
    n=n(), 
    delay=mean(arr_delay, na.rm=TRUE)
  ) |>
  filter(n>10)

flights |> 
  filter(carrier=="UA", 
         dest%in%c("IAH", "HOU"), 
         sched_dep_time > 0900,sched_arr_time<2000) |>
  group_by(flight) |>
  summarize(
    delay=mean(arr_delay,na.rm=TRUE),
    cancelled=sum(is.na(arr_delay)),n=n()
  ) |>
  filter(n>10)