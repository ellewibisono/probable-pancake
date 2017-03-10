<<<<<< HEAD
=======
---
title: "Gapminder Assignment"
author: "Elle Wibisono, Maggie Tsai, Paul Carvalho, Xintong Guan"
date: "March 10, 2017"
output: pdf_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, fig.width=6, fig.height=3.5, warning=FALSE, $
```

## Analysis of gapminder dataset

Gapminder data includes population, life expectancy, and GDP per captita for 14$
``{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, fig.width=6, fig.height=3.5, warning=FALSE, $
```

## Analysis of gapminder dataset

Gapminder data includes population, life expectancy, and GDP per captita for 14$

```{r gapminder, include=FALSE}
library(ggplot2)
library(plyr)
library(dplyr)
library(curl)
library(bbmle)

# Load the data
gapminder_location = curl(url = "https://raw.githubusercontent.com/resbaz/r-nov$
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

```{r life expectancy, echo=FALSE}
# Split data by continent and year, then calculate the mean and standard error $
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


# Plot mean life expectency (including standard error of the mean) for each con$
ggplot(gap1, aes(year, Mean.LifeExp)) +
       geom_point(aes(color=continent)) +
       geom_line(aes(color=continent)) +
       geom_linerange(aes(ymin=Mean.LifeExp - SE.LifeExp,
                         ymax=Mean.LifeExp + SE.LifeExp,
                         color = continent)) +
       xlab("Year") +
       ylab("Life expectancy") +
       theme(legend.title=element_blank())+ggtitle("Life Expectancy")

#run regression to see p value and significance
ggplot(data=gapminder, aes(x=year, y=lifeExp))+geom_point()+geom_smooth(method=lm)
summary(lm(lifeExp~year))


```

Figure 1. Mean life expectancy by continent over time (years 1952-2007). Bars i$

```{r population, echo=FALSE}
# Plot mean population in millions (including standard error of the mean) for e$

ggplot(gap1, aes(year, Mean.PopMil)) +
  geom_point(aes(color=continent)) +
  geom_line(aes(color=continent)) +
  geom_linerange(aes(ymin=Mean.PopMil - SE.PopMil,
                     ymax=Mean.PopMil + SE.PopMil,
                     color = continent)) +
  xlab("Year") +
  ylab("Population (millions)") +
  theme(legend.title=element_blank())+ggtitle("Figure1 Mean Life Expextancy by $


#run regression to see p value and significance
ggplot(data=gapminder, aes(x=year, y=pop))+geom_point()+geom_smooth(method=lm)
summary(lm(pop~year))

```

Figure 2. Mean population by continent over time (years 1952-2007). Bars indica$

```{r gdp per cap, echo=FALSE}
# Plot mean gdp per capita (including standard error of the mean) for each cont$
ggplot(gap1, aes(year, Mean.gdpPercap)) +
  geom_point(aes(color=continent)) +
  geom_line(aes(color=continent)) +
 geom_linerange(aes(ymin=Mean.gdpPercap - SE.gdpPercap,
                     ymax=Mean.gdpPercap + SE.gdpPercap,
                     color = continent)) +
  xlab("Year") +
  ylab("GDP per capita") +
  theme(legend.title=element_blank())+ggtitle("Figure2 Mean Population by Conti$
```

Figure 3. Mean GDP per capita by continent over time (years 1952-2007). Bars in$

# Life expectancy and GDP per capita

```{r lifeExp as a function of gdpPercap, echo=FALSE}
ggplot(gapminder, aes(log(gdpPercap), lifeExp)) +
       geom_point() +
       geom_smooth (method=lm, level=0.95) +
       xlab('GDP per capita (log)')+
       ylab('Life expectancy')+ggtitle("Figure3 Mean GDP per capita")
```

Figure 4. Life expectancy as a function of GDP per cap for all countries and ye$

Linear regression: lifeExp = [8.4 * log(gdpPercap)] - 9.1
R-squared: 0.65

```{r lifeExp as a function of gdpPercap models, echo=FALSE, include=FALSE}
# Linear Models
fit1 = lm(gapminder$lifeExp ~ log(gapminder$gdpPercap))
summary(fit1)

fit2 = lm(gapminder$lifeExp ~ log(gapminder$gdpPercap) + gapminder$continent)
summary(fit2)

fit3 = lm(gapminder$lifeExp ~ log(gapminder$gdpPercap) + as.factor(gapminder$ye$
summary(fit3)

fit4 = lm(gapminder$lifeExp ~ log(gapminder$gdpPercap) * as.factor(gapminder$ye$
summary(fit4)

AICtab(fit1, fit2, fit3, fit4)
```

We created four different linear models and assessed fit with the Akaike inform$

Figure 5.
