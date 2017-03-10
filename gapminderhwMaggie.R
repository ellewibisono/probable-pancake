#Maggie's R script
library(ggplot2)
library(plyr)
library(dplyr)
library(curl)

# Load the data
gapminder_location = curl(url = "https://raw.githubusercontent.com/resbaz/r-novice-gapminder-files/master/data/gapminder-FiveYearData.csv")
gapminder = read.csv(gapminder_location, stringsAsFactors = FALSE)

# Create a variable expressing population in millions.
gapminder$pop_mil = gapminder$pop/1000000

# Functions
# Calculate standard error of the mean.
se = function(x){
  std.error = sd(x)/sqrt(length(x))
  return(std.error)
}

## Variables in relation to time and continent

# Split data by continent and year, then calculate the mean and standard error of the mean for life expectancy, population, and gdpPercap.
gap1 = ddply(gapminder, c('continent','year'), function(df)c(mean(df$lifeExp),
                                                             se(df$lifeExp),
                                                             mean(df$pop_mil),
                                                             se(df$pop_mil),
                                                             mean(df$gdpPercap),
                                                             se(df$gdpPercap)))
colnames(gap1)[3:8] = c('Mean.LifeExp',
                        'SE.LifeExp',
                        'Mean.PopMil',
                        'SE.PopMil',
                        'Mean.gdpPercap',
                        'SE.gdpPercap')

# Life Expectancy and Continent 

ggplot(gap1, aes(x = continent, y = Mean.LifeExp)) +
  geom_boxplot(fill = "skyblue2", colour = "dimgray") +
  scale_x_discrete() + xlab("Continent") +
  ylab("Mean Life Expectancy")

#One-Way Analysis of variance conducted to assess if life expectancies for all years significantly differed by continent

aov1 = lm(gap1$Mean.LifeExp ~ gap1$continent)
summary(aov1)

#Results show omnibus significance with medium to large effect size of continent on life expectancy

