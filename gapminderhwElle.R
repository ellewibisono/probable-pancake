library(ggplot2)
library(plyr)
library(dplyr)
library(curl)
library(bbmle)
library(HH)
library(lattice)

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
```

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


fit1 = lm(gapminder$lifeExp ~ log(gapminder$gdpPercap))
summary(fit1)

fit2 = lm(gapminder$lifeExp ~ log(gapminder$gdpPercap) + gapminder$continent)
summary(fit2)

fit3 = lm(gapminder$lifeExp ~ log(gapminder$gdpPercap) + as.factor(gapminder$year))
summary(fit3)

fit4 = lm(gapminder$lifeExp ~ log(gapminder$gdpPercap) * as.factor(gapminder$year))
summary(fit4)

AICtab(fit1, fit2, fit3, fit4)

a1 <- coef(fit3)[1]
b <- coef(fit3)[2]

fit3anc <- predict(fit3)
ggplot(data=cbind(gapminder, fit3anc),
      aes(gapminder$lifeExp, log(gapminder$gdpPercap), color=year))+geom_point()+
      facet_grid(.~year) + geom_smooth(method='lm') 
  


#xyplot(gapminder$lifeExp~gapminder$gdpPercap | gapminder$year, data=gapminder)

