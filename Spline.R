library(tidyverse)
library(dplyr)
library(ggcorrplot)
library(splines)


dat <- read.csv("DelhiHousing.csv", header = TRUE)
str(dat)

dat <- table(dat)
View(dat)
dat_Mode<-function(x){
  return(names(which.max(table(x))))
}


dat1<-filter(dat, dat$Area == "") %>% mutate(Type = dat_Mode(dat$Area))
dat <-filter(dat,dat$Area != "")
dat <- rbind(dat, dat1)


length(dat$Area)
view(dat)

range(dat$Parking)

plot(dat$Price~dat$Area)

install.packages("splines2")


y<-dat$Price[dat$Area<10000]
x<-dat$Area[dat$Area<10000]


#Spline Regression (choosing knots manually)
splinemodel <- lm(y~bs(x, knots = c(2000,4000,6000,8000)), data = dat)
summary(splinemodel)

# Let's store the R-squared valu in a variable 
Rsqr.spline1 =  0.7752

#Smooth Splines 

y.spline<-smooth.spline(x,y, df = 10)
summary(y.spline)
str(y.spline)


length(y.spline$y)


y.fitted = predict(y.spline, x)
length(y.fitted$y)


plot(x,y)

lines(y.spline, col = 'red', lwd = 3)

# SSE and R-squared of Smooth splines

diff = y.fitted$y-y

diffsq = diff^2

SSE = sum(diffsq)

SST = sum((y-mean(y))^2)

SST

Rsqr.spline2 = 1-(SSE/SST)

# Comparing the two models

compare = c(Rsqr.spline1, Rsqr.spline2)
compare

## Both models are close enough in goodness of fit, first one being slightly better


