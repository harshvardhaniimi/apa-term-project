library(tidyverse)

dat <- read.csv("MagicBricks.csv", header = TRUE)
str(dat)

View(dat)
dat_Mode<-function(x){
  return(names(which.max(table(x))))
}

dat[is.na(dat$Parking),]$Parking = dat_Mode(dat$Parking)
dat[is.na(dat$Bathroom),]$Bathroom = dat_Mode(dat$Bathroom)
dat[dat$Furnishing == "",]$Furnishing = dat_Mode(dat$Furnishing)
dat[dat$Type == "",]$Type = dat_Mode(dat$Type)

write.csv(dat, file = "DelhiHousingClean.csv", row.names = F)




#



dat1<-filter(dat, is.na(Bathroom)) %>% mutate(Bathroom = dat_Mode(Bathroom))
dat <-filter(dat,is.na(Bathroom) == FALSE)
dat <- rbind(dat, dat1)


dat1<-filter(dat, Furnishing == "") %>% mutate(Furnishing = dat_Mode(Furnishing))
dat <-filter(dat,Furnishing != "")
dat <- rbind(dat, dat1)

dat1<-filter(dat, is.na(Parking)) %>% mutate(Parking = dat_Mode(Parking))
dat <-filter(dat,is.na(Parking) == FALSE)
dat <- rbind(dat, dat1)

dat1<-filter(dat, Type == "") %>% mutate(Type = dat_Mode(Type))
dat <-filter(dat,Type != "")
dat <- rbind(dat, dat1)

write.csv(dat, file = "DelhiHousingClean.csv", row.names = F)

