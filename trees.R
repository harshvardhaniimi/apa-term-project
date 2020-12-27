#----------
# This aim of this project is to predict the house prices in New Delhi, using whatever 
# information is available. The data was sourced from Kaggle, which is essentially from
# listings available on MagicBricks, a leading real estate broker.
#----------


##### Exploratary Data Analysis

#----------
# Loading Data
#----------

data = read.csv("DelhiHousingClean.csv", head = T, stringsAsFactors = T)
head(data)

str(data)

# Most variables are categorical in nature; only area and price are continious. Further, 'Locality' apparently has 365 levels. This might not be entirely accurate many addresses are repeated. For example, "Rohini Sector 24" and "Rohini Sector 24 carpet area 650 sqft status..." are in the same locality. But because of poor web scrapping, errors have popped up.

# While recognising that there is need for cleaning this variable, we will skip that part for the paucity of time.

# So, assuming that each entry is unique, let's proceed to explore.

# The variable Per_Sqft is mostly Price/Area. Therefore, we exclude it to avoid multicollinearity problems.

data = dplyr::select(data, -Per_Sqft, -Locality)

area = data$Area
bhk = data$BHK
bath = data$Bathroom
furnish = data$Furnishing
park = data$Parking
price = data$Price
status = data$Status
trans = data$Transaction
type = data$Type

#---------
# Visualisation
#---------

# Area vs Price
plot(area, price)

# Most are concentrated in the lower are limit and lower price limit. So, let's reset the limits.

plot(area, price, xlim = c(0,6000), ylim = c(0,1.5e8))

# The general trend is that as area increases, the price increases.

# BHK
hist(bhk)

# Bathroom
hist(bath)

# Parking
hist(park)

# to eliminate outliers as they are skewing the histogram
trim <- function(x){
  x[(x > mean(x, na.rm = T)-1.5*IQR(x, na.rm = T)) & (x < mean(x, na.rm = T)+1.5*IQR(x, na.rm = T))]
}
hist(trim(park))

# Price
boxplot(price, log = "y")

# Status
barplot(table(status))

# Transaction
barplot(table(trans))

# Type
barplot(table(type))

# Furnishing
barplot(table(furnish))

# Price by Furnishing and Trans - log scaled
boxplot(price~furnish+trans, col = 2:8, drop = T, boxwex = 0.6, log = "y", na.rm = T)
abline(h = median(price), col = "red", lwd = 3, lty = 2)

#### Splitting data into training and test

set.seed(12345)
l = sample(nrow(data), size = 0.7*nrow(data))
train = data[l,]
test = data[-l,]

#### Fitting Trees - CART and Random Forrest

#-------
# Fitting CART via bagging
#-------

library(rpart)
library(ipred)

# Training model using rpart library
fit0 = rpart(Price~., data = train, method="anova", control=rpart.control(minsplit=30,cp=0.001))
# Multiple values of minsplit were tried and then 30 was chosen. Complexity parameter tells how much should the R squared increase by at least to consider an iteration. It is for pruning the needless branches.

# yhat_training
yhat_train = predict(fit0)
plot(train$Price, yhat_train)
rsq_train_rpart = 1 - mean(yhat_train - train$Price)^2/var(train$Price)
rsq_train_rpart

# Clearly there is heteroskedasticity problem as the error is blowing up as price increases. But still, the prediction R squared is 100%!

# Let's see the testing R-squared
yhat_test = predict(fit0, newdata = test)
plot(test$Price, yhat_test)
rsq_test_rpart = 1 - mean(yhat_test - test$Price)^2/var(test$Price)
rsq_test_rpart

# 99%! - impressive


# Training model using ipred library
fit1 = bagging(Price~., data = train,nbagg=100,control=rpart.control(minsplit=30,cp=0))
# choosing 100 bootstrap replications. Higher will give better estimates but will make the machine very slow.

yhat_train = predict(fit1)
plot(train$Price, yhat_train)
rsq_train_ipred = 1 - mean(yhat_train - train$Price)^2/var(train$Price)
rsq_train_ipred

# 99% - impressive again

# Testing ipred model
yhat_test = predict(fit1, newdata = test)
plot(test$Price, yhat_test)
rsq_test_ipred = 1 - mean(yhat_test - test$Price)^2/var(test$Price)
rsq_test_ipred

# Both CART algorithms give excellent predictive abilities, explaining 99% of variability.

print(rbind(rsq_train_rpart, rsq_test_rpart, rsq_train_ipred, rsq_test_ipred))

#--------
# Random Forrest
#--------

# Now let's try Random Forrest as that gives less biased predictive capability.

library(randomForest)

fit_rf = randomForest(Price~.,data=train,ntree=500)

# training accuracy
yhat_train = predict(fit_rf)
plot(train$Price, yhat_train)
rsq_train_randfor = 1 - mean(yhat_train - train$Price)^2/var(train$Price)
rsq_train_randfor

# 99% - impressive again

# Testing ipred model
yhat_test = predict(fit_rf, newdata = test)
plot(test$Price, yhat_test)
rsq_test_randfor = 1 - mean(yhat_test - test$Price)^2/var(test$Price)
rsq_test_randfor

# 99% - impressive again
