library(forecast)
library(fpp3)
library(zoo)
library(dplyr)

Aussietour.df <- read.csv("AustralianTourism.csv")

Aussietour.df$Quarter <- as.yearqtr(Aussietour.df$Quarter, format='%m/%d/%y')

Aussietour.ts <- ts(Aussietour.df$Victoria, start = c(1998, 1), end = c(2017, 4), freq = 4)
plot(Aussietour.ts,  ylab = "Revenue ($ millions)", xlab = "Time", bty = "l", 
     xlim = c(1998,2018), main = "")

nValid <- 12
nTrain <- length(Aussietour.ts) - nValid
train.ts <- window(Aussietour.ts, start = c(1998, 1), end = c(1998, nTrain))
valid.ts <- window(Aussietour.ts, start = c(1998, nTrain + 1), end = c(1998, nTrain + 
                                                                         nValid))
#QUESTION1

fit.lm <- tslm(train.ts ~ trend + season)
summary(fit.lm)

fit.lm.pred <- forecast::forecast(fit.lm, h = nValid)
forecast::accuracy(fit.lm.pred, valid.ts)

plot(train.ts, xlim = c(1998,2017), ylim = c(3500,7000))
lines(fit.lm.pred$fitted, lwd = 2, col = "blue")
lines(fit.lm.pred$mean, lwd = 2, col = "blue", lty = 3)

plot(fit.lm.pred,
     ylab = "Tourism", xlab = "Time", bty = "l", 
     xlim = c(1998,2017), main = "")

#QUESTION 2

plot(train.ts)

ndiffs(train.ts)
#0 d=0

nsdiffs(train.ts)
#1 D=1


diff1.train.ts <- diff(train.ts, lag = 1)
tsdisplay(diff1.train.ts)

diff2.train.ts <- diff(train.ts, lag = 2)
tsdisplay(diff2.train.ts)

diff4.train.ts <- diff(train.ts, lag = 4)
tsdisplay(diff4.train.ts)

par(mfrow=c(2,2))
plot(train.ts)
plot(diff1.train.ts)
plot(diff2.train.ts)
plot(diff4.train.ts)
par(mfrow=c(1,1))

#I would have say that you only need 1 level of differencing since 
#ndiffs = 0 and nsdiffs = 1 and when you plot train.ts and difference it like I 
#did above, level one difference smooths out the time series 


tsdisplay(train.ts)
tsdisplay(diff4.train.ts)

#Looking at my ARIMA models below I would say that you need 1 MA and AR term and
#1 season MA and season AR term. This achieves the lowest AIC 


# We are going to fit a SARIMA (p,d,q) (P,D,Q) model with
# p = nos. of AR terms
# d = level of differencing to remove trends
# q = nos. of MA terms
# P = nos. of seasonal AR terms (i.e. multiples of frequency)
# D = level of seasonal differencing to remove seasonal trends
# Q = nos. of seasonal MA terms (i.e. multiples of frequency)

ARIMA1.fit <- Arima(train.ts, order = c(1,0,1), seasonal=c(1,1,0))
summary(ARIMA1.fit)

ARIMA2.fit <- Arima(train.ts, order = c(1,0,1), seasonal=c(1,1,1))
summary(ARIMA2.fit)

ARIMA3.fit <- Arima(train.ts, order = c(1,0,1), seasonal=c(0,1,1))
summary(ARIMA3.fit)

ARIMA4.fit <- Arima(train.ts, order = c(1,0,1), seasonal=c(0,1,0))
summary(ARIMA4.fit)

ARIMA5.fit <- Arima(train.ts, order = c(1,0,0), seasonal=c(0,1,1))
summary(ARIMA5.fit)

ARIMA6.fit <- Arima(train.ts, order = c(1,1,1), seasonal=c(1,1,1))
summary(ARIMA6.fit)



ARIMA6.pred <- forecast::forecast(ARIMA6.fit, h = nValid)


# plot forecasts and actuals in the training and validation sets
plot(train.ts, ylab = "Aussie Tourism", xlab = "Time", bty = "l", 
     xlim = c(1998,2017), main = "")
lines(ARIMA6.pred$fitted, lwd = 2, col = "blue", lty = 3)
lines(ARIMA6.pred$mean, lwd = 2, col = "blue", lty = 3)

plot(ARIMA6.pred, ylab = "Aussie Tourism", xlab = "Time", bty = "l", 
     xlim = c(1998,2017), main = "")


#QUESTION3

auto.ARIMA.fit <- auto.arima(train.ts)
summary(auto.ARIMA.fit)

auto.ARIMA.pred <- forecast::forecast(auto.ARIMA.fit, h = nValid)

plot(train.ts, ylab = "Aussie Tourism", xlab = "Time", bty = "l", 
     xlim = c(1998,2017), main = "")
lines(auto.ARIMA.pred$fitted, lwd = 2, col = "blue", lty = 3)
lines(auto.ARIMA.pred$mean, lwd = 2, col = "blue", lty = 3)

plot(auto.ARIMA.pred, ylab = "Aussie Tourism", xlab = "Time", bty = "l", 
     xlim = c(1998,2017), main = "")

#QUESTION 4 
#Yes there is a constant trend with additive seasonality 
# My recommendation is ETS model 7

plot(train.ts, xlab = "Time", ylab = "Aussie Tourism", bty = "l")

seasonplot(train.ts, ylab="Aussie Tourism", 
           xlab="Year", main="Seasonal Plot", year.labels=TRUE)

monthplot(train.ts, ylab="Aussie Tourism", 
          xlab="Year", main="Seasonal Deviation Plot")


my.ets1 <- ets(train.ts, model = "AMA", restrict=FALSE)
my.ets1

my.ets2 <- ets(train.ts, model = "ANA", restrict=FALSE)
my.ets2

my.ets3 <- ets(train.ts, model = "AAM", restrict=FALSE)
my.ets3

my.ets4 <- ets(train.ts, model = "ANM", restrict=FALSE)
my.ets4

my.ets5 <- ets(train.ts, model = "AMM", restrict=FALSE)
my.ets5

my.ets6 <- ets(train.ts, model = "MAM", restrict=FALSE)
my.ets6

my.ets7 <- ets(train.ts, model = "MNA", restrict=FALSE)
my.ets7

ETS.model7.pred <- forecast::forecast(my.ets7, h = nValid)

plot(train.ts, ylim = c(3500, 7000),  ylab = "Aussie Tourism", xlab = "Time", bty = "l", 
     xaxt = "n", xlim = c(1998,2017), main = "")
axis(1, at = seq(1998, 2017, 1), labels = format(seq(1998, 2017, 1)))
lines(ETS.model7.pred$fitted, lwd = 2, col = "blue")
lines(ETS.model7.pred$mean, lwd = 2, col = "blue", lty = 3)


plot(ETS.model7.pred, ylim = c(3500, 7000),  ylab = "Aussie Tourism", xlab = "Time", bty = "l", 
     xaxt = "n", xlim = c(1998,2017), main = "")
axis(1, at = seq(1998, 2017, 1), labels = format(seq(1998, 2017, 1)))




#Question 5
auto.ets <- ets(train.ts, restrict = FALSE)
auto.ets

auto.ets.pred <- forecast::forecast(auto.ets, h = nValid)

plot(train.ts, ylim = c(3500, 7000),  ylab = "Aussie Tourism", xlab = "Time", bty = "l", 
     xaxt = "n", xlim = c(1998,2017), main = "")
axis(1, at = seq(1998, 2017, 1), labels = format(seq(1998, 2017, 1)))
lines(auto.ets.pred$fitted, lwd = 2, col = "blue")
lines(auto.ets.pred$mean, lwd = 2, col = "blue", lty = 3)


plot(auto.ets.pred, ylim = c(3500, 7000),  ylab = "Aussie Tourism", xlab = "Time", bty = "l", 
     xaxt = "n", xlim = c(1998,2017), main = "")
axis(1, at = seq(1998, 2017, 1), labels = format(seq(1998, 2017, 1)))


#QUESTION 6

forecast::accuracy(fit.lm.pred, valid.ts)
forecast::accuracy(ARIMA6.pred, valid.ts)
forecast::accuracy(auto.ARIMA.pred, valid.ts)
forecast::accuracy(ETS.model7.pred, valid.ts)
forecast::accuracy(auto.ets.pred, valid.ts)

#QUESTION 7 

# My choice is the ARIMA that I created ARIMA6.pred due to the best MAPE score 
# and RMSE 

plot(train.ts, ylab = "Aussie Tourism", xlab = "Time", bty = "l", 
     xlim = c(1998,2017), main = "")
lines(ARIMA6.pred$fitted, lwd = 2, col = "purple", lty = 3)
lines(ARIMA6.pred$mean, lwd = 2, col = "blue", lty = 3)
lines(valid.ts, col = "grey20", lty = 3)

plot(ARIMA6.pred)

ARIMAforecast.fit <- Arima(Aussietour.ts, order = c(1,1,1), seasonal=c(1,1,1))
summary(ARIMAforecast.fit)

ARIMAforecastfit.pred <- forecast::forecast(ARIMAforecast.fit, h = 12)

plot(ARIMAforecastfit.pred)

plot(Aussietour.ts, ylab = "Aussie Tourism", xlab = "Time", bty = "l", 
     xlim = c(2015,2020), main = "")
lines(ARIMAforecastfit.pred$fitted, lwd = 2, col = "purple", lty = 3)
lines(ARIMAforecastfit.pred$mean, lwd = 2, col = "blue", lty = 3)

installed.packages()
library(bupaR)
