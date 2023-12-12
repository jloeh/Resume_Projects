library(forecast)
library(fpp3)
library(zoo)


visits.df <- read.csv("pmrproject.csv")
visits.df$Dates <- as.yearmon(visits.df$Dates, format='%m/%d/%Y')
visits.ts <- ts(visits.df$NPV, 
                start = c(2019, 1), end = c(2023, 4), freq = 12)
plot(visits.ts,  ylab = "Patients", xlab = "Time",
     xlim = c(2019,2023.5))
## partitioning into training (60%) and validation (40%)
# randomly sample 60% of the row IDs for training; the remaining 40% serve as
# validation


nValid <- 21
nTrain <- length(visits.ts) - nValid
train.ts <- window(visits.ts, start = c(2019, 1), 
                   end = c(2019, nTrain))
valid.ts <- window(visits.ts, start = c(2019, nTrain + 1), 
                   end = c(2019, nTrain + nValid))
#########################################################

naive.fit <- naive(train.ts, h = nValid)
snaive.fit <- snaive(train.ts, h = nValid)

naive.pred <- forecast::forecast(naive.fit, h = nValid)
snaive.pred <- forecast::forecast(snaive.fit, h = nValid)

forecast::accuracy(naive.pred, valid.ts)
forecast::accuracy(snaive.pred, valid.ts)

###############################################################


fit.lm <- tslm(train.ts ~ trend + season)
summary(fit.lm)

fit.lm.pred <- forecast::forecast(fit.lm, h = nValid)
forecast::accuracy(fit.lm.pred, valid.ts)

train.lm.expo.trend <- tslm(train.ts ~ trend, lambda = 0)
train.lm.expo.trend.pred <- forecast::forecast(train.lm.expo.trend, h = nValid, level = 0)
forecast::accuracy(train.lm.expo.trend.pred, valid.ts)


train.lm.linear.trend <- tslm(train.ts ~ trend, lambda = 1)
train.lm.linear.trend.pred <- forecast::forecast(train.lm.linear.trend, h = nValid, level = 0)
forecast::accuracy(train.lm.linear.trend.pred, valid.ts)

train.lm.poly.trend <- tslm(train.ts ~ trend + I(trend^2))
summary(train.lm.poly.trend)
train.lm.poly.trend.pred <- forecast::forecast(train.lm.poly.trend, h = nValid, level = 0)

forecast::accuracy(train.lm.poly.trend.pred, valid.ts)

train.lm.season <- tslm(train.ts ~ season)
summary(train.lm.season)
train.lm.season.pred <- forecast::forecast(train.lm.season, h = nValid, level = 0)
forecast::accuracy(train.lm.season.pred, valid.ts)

train.lm.trend.season <- tslm(train.ts ~ trend + I(trend^2) + season)
summary(train.lm.trend.season)
train.lm.trend.season.pred <- forecast::forecast(train.lm.trend.season, h = nValid, level = 0)

forecast::accuracy(naive.pred, valid.ts)
forecast::accuracy(snaive.pred, valid.ts)
forecast::accuracy(train.lm.poly.trend.pred, valid.ts)
forecast::accuracy(train.lm.season.pred, valid.ts)
forecast::accuracy(train.lm.trend.season.pred, valid.ts)


plot(train.ts, xlim = c(2019,2023.5), ylim = c(0,120))
lines(train.lm.season.pred$fitted, lwd = 2, col = "blue")
lines(train.lm.season.pred$mean, lwd = 2, col = "blue", lty = 3)
lines(valid.ts, col = "grey20", lty = 3)

forecast::accuracy(train.lm.season.pred, valid.ts)


#####################################################

ses.pred <- ses(train.ts, h = nValid)

hwin <- HoltWinters(train.ts)

hwin.pred <- forecast::forecast(hwin, h = nValid)

holt.pred <- holt(train.ts, h = nValid)

forecast::accuracy(ses.pred, valid.ts)
forecast::accuracy(holt.pred, valid.ts)
forecast::accuracy(hwin.pred, valid.ts)

#######################################################
#ETS(M,N,N)
plot(visits.ts, xlim = c(2019,2023.5), ylim = c(0,120))

my.ets1 <- ets(train.ts, model = "MMN", restrict=FALSE)
summary(my.ets1)
my.ets1.pred <- forecast::forecast(my.ets1, h = nValid)


my.ets2 <- ets(train.ts, model = "MMM", restrict=FALSE)
summary(my.ets2)
my.ets2.pred <- forecast::forecast(my.ets2, h = nValid)


my.ets3 <- ets(train.ts, model = "MNM", restrict=FALSE)
summary(my.ets3)
my.ets3.pred <- forecast::forecast(my.ets3, h = nValid)


auto.ets <- ets(train.ts, restrict = FALSE)
summary(auto.ets)

autoets.pred <- forecast::forecast(auto.ets, h = nValid)

forecast::accuracy(autoets.pred, valid.ts)
forecast::accuracy(my.ets2.pred, valid.ts)
forecast::accuracy(my.ets3.pred, valid.ts)


plot(train.ts, xlim = c(2019,2023.5), ylim = c(0,120))
lines(my.ets3.pred$fitted, lwd = 2, col = "blue")
lines(my.ets3.pred$mean, lwd = 2, col = "blue", lty = 3)
lines(valid.ts, col = "grey20", lty = 3)

forecast::accuracy(my.ets3.pred, valid.ts)
##############################################################


plot(train.ts, xlim = c(2019,2023.5), ylim = c(0,120))
lines(AA.pred$fitted, lwd = 2, col = "blue")
lines(AA.pred$mean, lwd = 2, col = "blue", lty = 3)
lines(valid.ts, col = "grey20", lty = 3)




ndiffs(train.ts)
#0 d=1

nsdiffs(train.ts)
#1 D=0


 

diff1.train.ts <- diff(train.ts, 12)
tsdisplay(diff1.train.ts)

diff2.train.ts <- diff(train.ts, 1)
tsdisplay(diff1.train.ts)

diff3.train.ts <- diff(train.ts, 2)
tsdisplay(diff3.train.ts)

diff4.train.ts <- diff(diff(train.ts, 1),12)
tsdisplay(diff4.train.ts)


par(mfrow=c(2,3))
plot(train.ts)
plot(diff1.train.ts)
plot(diff2.train.ts)
plot(diff3.train.ts)
plot(diff4.train.ts)
par(mfrow=c(1,1))

diff12.train.ts <- diff(train.ts, lag = 12)
tsdisplay(diff12.train.ts)



# We are going to fit a SARIMA (p,d,q) (P,D,Q) model with
# p = nos. of AR terms
# d = level of differencing to remove trends
# q = nos. of MA terms
# P = nos. of seasonal AR terms (i.e. multiples of frequency)
# D = level of seasonal differencing to remove seasonal trends
# Q = nos. of seasonal MA terms (i.e. multiples of frequency)
#(p1q) (P0Q)


AA2.fit <- auto.arima(train.ts)
summary(AA2.fit) 

AA2.pred <- forecast::forecast(AA2.fit, h = nValid)

forecast::accuracy(AA2.pred, valid.ts)

AA.fit <- auto.arima(train.ts)
summary(AA.fit)
AA.pred <- forecast::forecast(AA.fit, h = nValid)

forecast::accuracy(AA.pred, valid.ts)

##AUTOARIMA(0,1,0) 262 AIC

#(0,1,1) (1,1,0)

ARIMA1.fit <- Arima(train.ts, order = c(0,1,0), seasonal=c(1,1,0)) 
summary(ARIMA1.fit)

ARIMA2.fit <- Arima(train.ts, order = c(0,1,1), seasonal=c(1,1,0)) 
summary(ARIMA2.fit)



ARIMA1.pred = forecast::forecast(ARIMA1.fit, h=nValid)
ARIMA2.pred = forecast::forecast(ARIMA2.fit, h=nValid)

forecast::accuracy(ARIMA2.pred, valid.ts)
forecast::accuracy(ARIMA1.pred, valid.ts)
forecast::accuracy(AA.pred, valid.ts)

plot(train.ts, xlim = c(2019,2024), ylim = c(0,120))
lines(AA.pred$fitted, lwd = 2, col = "blue")
lines(AA.pred$mean, lwd = 2, col = "blue", lty = 3)
lines(valid.ts, col = "grey20", lty = 3)


##################################################


forecast::accuracy(AA.pred, valid.ts)
forecast::accuracy(my.ets3.pred, valid.ts)
forecast::accuracy(ses.pred, valid.ts)


forecast::accuracy(train.lm.season.pred, valid.ts)

plot(train.ts, xlim = c(2019,2024), ylim = c(0,120))
lines(train.lm.season.pred$fitted, lwd = 2, col = "blue")
lines(train.lm.season.pred$mean, lwd = 2, col = "blue", lty = 3)
lines(valid.ts, col = "grey20", lty = 3)


train.lm.season1 <- tslm(visits.ts ~ season)
summary(train.lm.season1)
train.lm.season.pred <- forecast::forecast(train.lm.season, h = nValid, level = 0)
forecast::accuracy(train.lm.season.pred, valid.ts)

train.lm.season1 <- tslm(visits.ts ~ season)
summary(train.lm.season1)
train.lm.season.pred1 <- forecast::forecast (train.lm.season1, h = 12, level = 0)



plot(visits.ts, xlim = c(2019,2024), ylim = c(0,120))
lines(train.lm.season.pred1$fitted, lwd = 2, col = "blue")
lines(train.lm.season.pred1$mean, lwd = 2, col = "blue", lty = 3)
lines(valid.ts, col = "grey20", lty = 3)

train.lm.season.pred1
