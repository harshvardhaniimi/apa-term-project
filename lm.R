#R will automatically encode char variables as factors
#recoding int BHK 
BHK <- as.factor(BHK)

#splitting into training and test data
dat.split <- resample_partition(dat, c(test = 0.3, train = 0.7))

train <- as_tibble(dat.split$train)
test <- as_tibble(dat.split$test)

#linear regression
dat.lm <- lm(Price ~ Area + BHK + Bathroom + Furnishing  + Parking + Status + Transaction + Type, data = train)
summary(dat.lm)

#removing 'Type' 
dat.lm2 <- lm(Price ~ Area + BHK + Bathroom + Furnishing  + Parking + Status + Transaction, data = train)
summary(dat.lm2)

#removing 'Furnishing'
dat.lm3 <- lm(Price ~ Area + BHK + Bathroom + Parking + Status + Transaction, data = train)
summary(dat.lm3)

#removing 'BHK'
dat.lm4 <- lm(Price ~ Area + Bathroom + Parking + Status + Transaction, data = train)
summary(dat.lm4)

#selecting model 3 as it has the highest adjusted R-squared

#predictions
test <- test %>% add_predictions(dat.lm3)
ggplot(test, aes(test$Price, test$pred)) +
  geom_point() +
  geom_abline()

