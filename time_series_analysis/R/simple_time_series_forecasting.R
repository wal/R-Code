# Simple Time Series forecasting
library(tidyverse)
library(forecast)
library(gridExtra)
library(ggfortify)




# AirPassengers 
class(AirPassengers) # ts uses a regularized index computed using the start and frequency arguments
summary(AirPassengers)

horizon = 30
confidence_level = 95

grid.arrange(
  autoplot(AirPassengers),
  autoplot(meanf(AirPassengers, h=horizon, level = confidence_level)),
  autoplot(naive(AirPassengers, h=horizon, level = confidence_level)),
  autoplot(snaive(AirPassengers, h=horizon, level = confidence_level)),
  autoplot(rwf(AirPassengers, h=horizon, level = confidence_level)),
  autoplot(hw(AirPassengers, h=horizon, level = confidence_level)),
  autoplot(forecast(auto.arima(AirPassengers), h=horizon, level = confidence_level)),
  autoplot(forecast(ets(AirPassengers), h=horizon, level = confidence_level)),
  ncol=1)
