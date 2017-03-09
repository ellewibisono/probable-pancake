library(ggplot2)
library(plyr)
library(dplyr)
library(curl)
library(bbmle)
library(HH)

# Load the data
gapminder_location = curl(url = "https://raw.githubusercontent.com/resbaz/r-novice-gapminder-files/master/data/gapminder-FiveYearData.csv")
gapminder = read.csv(gapminder_location, stringsAsFactors = FALSE)

# Create a variable expressing population in millions.
gapminder$pop_mil = gapminder$pop/1000000

# Log transform the gdpPercap
lgdpPercap <- log(gapminder$gdpPercap)

# Add to lgdpPercap to data frame 
gap2 <- cbind(gapminder,lgdpPercap)


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
#yo, I might be crazy but would fit3 be ancova because it is continuous ~ continuous + categorical? 
fit3anc= ancova(gap2$lifeExp ~ gap2$lgdpPercap + gap2$year, data.in= gap2)


