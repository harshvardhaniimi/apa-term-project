#### Splitting data into training and test
set.seed(12345)
l = sample(nrow(data), size = 0.7*nrow(data))
train = data[l,]
test = data[-l,]

#### Fitting ANN models
# ---- ANN using nnet R package----------
library(nnet) # allows only one hidden layer

# Training model
Y = train$Price
n = length(Y)

train_price_scaled = (Y-min(Y))/(max(Y)-min(Y))
fit2 = nnet(train_price_scaled ~ Area + BHK + Bathroom + Parking, data=train,size=1,
            trace=F,decay=0.001)
print(fit2$wts) # parameter estimates

# yhat_training
yhat_train = fit2$fitted.values
plot(train_price_scaled, yhat_train)
rsq_train = 1 - sum((train_price_scaled - yhat_train)^2)/(var(train_price_scaled)*(n-1))
print(rsq_train)

#70% -- that's decent

# Testing model
Y = test$Price
n = length(Y)

test_price_scaled = (Y-min(Y))/(max(Y)-min(Y))
yhat_test = predict(fit2, newdata = test)
plot(test_price_scaled, yhat_test)
rsq_test = 1 - sum((test_price_scaled - yhat_test)^2)/(var(test_price_scaled)*(n-1))
print(rsq_test)

#63% -- that's still ok

#But we see a possibly somewhat significant difference in R^2 train vs R^2 test. 
#It might help to resample and try. 

#Apart from that, we can change "size" in our our model to get better R^2.
#By trial & error on sizes=1,2,3,4, we find that k=2 hidden nodes works best.

#=========================================
#=========================================
# Now let's try fitting using neuralnet R package which offers possibility of having >1 hidden layers.
# ---- ANN using neuralnet R package----------
library(neuralnet)

#=========================================
###--------1 hidden layer - 3 hidden variables--------
#=========================================
## Training model
Y = train$Price
n = length(Y)
train_price_scaled = (Y-min(Y))/(max(Y)-min(Y))

fit2 = neuralnet(train_price_scaled ~ Area + BHK + Bathroom + Parking, 
                 data=train, hidden=c(3,2), threshold = 0.01, linear.output=FALSE)
#--- network plot ----
plot(fit2)

# yhat_training
yhat_train = c((fit2$net.result)[[1]])
plot(train_price_scaled, yhat_train)

#There seems to be an error in the package functioning as required. So, drop all operations based on this package.