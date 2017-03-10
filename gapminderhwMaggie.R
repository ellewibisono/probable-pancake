#Maggie R script

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

# Population and Life Expectancy 
ggplot(gap1, aes(log(Mean.PopMil), Mean.LifeExp)) +
  geom_point(aes(color=continent)) +
  geom_smooth (method=lm, level=0.95) +
  xlab('Mean Population in Millions (log)')+
  ylab('Mean Life expectancy')

#Life expectancy as a function of population 

LC1 = lm(gap1$Mean.LifeExp ~ log(gap1$Mean.PopMil))
summary(LC1)
#No significance between IV Population in Millions and DV Life Expectancy, p=0.389

# Life Expectancy and Continent 
ggplot(gap1, aes(x = continent, y = Mean.LifeExp)) +
  geom_boxplot(fill = "skyblue2", colour = "dimgray") +
  scale_x_discrete() + xlab("Continent") +
  ylab("Mean Life Expectancy")

#One-Way Analysis of variance conducted to assess if life expectancies for all years significantly differed by continent

aov1 = aov(gap1$Mean.LifeExp ~ gap1$continent)
summary(aov1)

#Results show omnibus significance p<.001 with medium to large effect size of continent on life expectancy

#Post-hoc test multiple comparisons with Tukey HSD, alpha = 0.05
TukeyHSD(aov1)

#Non-significant contrasts: Asia-Americas, Europe-Americas, Oceania-Europe, alpha = 0.05
