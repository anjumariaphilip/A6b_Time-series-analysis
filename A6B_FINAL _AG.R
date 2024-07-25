if (!require(tseries)) install.packages("tseries", dependencies = TRUE)
if (!require(forecast)) install.packages("forecast", dependencies = TRUE)
if (!require(rugarch)) install.packages("rugarch", dependencies = TRUE)
if (!require(FinTS)) install.packages("FinTS", dependencies = TRUE)

library(tseries)
library(forecast)
library(rugarch)
library(FinTS)

setwd("C:\\Users\\HP\\Documents\\ipl")
data <- read.csv("BERGEPAINT.NS.csv")

str(data)

log_returns <- diff(log(data$Adj.Close))

log_returns_df <- data.frame(LogReturns = log_returns)

plot(log_returns_df$LogReturns, type = 'l', main = "Log Returns of BERGEPAINT.NS", ylab = "Log Returns", xlab = "Time")

arch_test <- ArchTest(log_returns_df$LogReturns, lags = 12)
print(arch_test)

garch_spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                         mean.model = list(armaOrder = c(0, 0)),
                         distribution.model = "norm")

garch_fit <- ugarchfit(spec = garch_spec, data = log_returns_df$LogReturns)
print(garch_fit)

arch_spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 0)),
                        mean.model = list(armaOrder = c(0, 0)),
                        distribution.model = "norm")

arch_fit <- ugarchfit(spec = arch_spec, data = log_returns_df$LogReturns)
print(arch_fit)

garch_forecast <- ugarchforecast(garch_fit, n.ahead = 63)
print(garch_forecast)

arch_forecast <- ugarchforecast(arch_fit, n.ahead = 63)
print(arch_forecast)

plot(garch_forecast, which = 1, main = "Forecasted Volatility (GARCH)")

plot(arch_forecast, which = 1, main = "Forecasted Volatility (ARCH)")
