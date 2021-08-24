# import library
library(tseries)
library(tidyr)
library(forecast)

# Nhập dữ liệu
df = read.csv("./data/day_wise.csv", header = TRUE, row.names = "Date")
df = na.omit(df)
y = df$Confirmed

# Phân tích sơ bộ
plot(y, type="l")

# Kiểm tra tính dừng
adf.test(y)

y_log = log(y)
adf.test(y_log)

# Xác định p bằng graph
pacf(y_log)

#Xây dựng model AR
AR_model = arima(y_log, order =c(1,0,0))

#Visualize
plot(y_log, type="l",col="blue")
pre = y_log - residuals(AR_model)
lines(pre, type="l",col="red")

#Xây dựng model ARMA
ARMA_model = arima(y_log, order =c(1,0,3))

#Visualize
plot(y_log, type="l",col="blue")
pre = y_log - residuals(ARMA_model)
lines(pre, type="l",col="red")

# Tính p,q,d tự động
auto.arima(y_log,trace=TRUE)

#Xây dựng model ARIMA aka ARMA
ARIMA_model = arima(y_log,order = c(2,2,0))

#Visualize
plot(y_log,type = "l",col="blue")
pre2 = y_log - residuals(ARIMA_model)
lines(pre2,type = "l",col="red")

plot(forecast(ARIMA_model,h = 12),col="green")
