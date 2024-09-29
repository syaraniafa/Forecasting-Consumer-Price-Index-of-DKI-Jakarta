library(forecast)
library(AICcmodavg)

df <- read.csv("C:/Users/syarani/OneDrive - Bina Nusantara/tugas kuliah/Semester 6/Time Series Analysis/AOL/Data IHK Jakarta.csv")

dt = ts(df$IHK, start = 2009, frequency = 12)
ts.plot(dt)

train = ts(dt[1:144], start=c(2009,1), frequency=12, end=c(2020,12))
test = ts(dt[145:180], start=c(2021, 1), frequency=12, end=c(2023,12))
length(test)
plot(decompose(dt))
decompose(dt)

calculate_metrics <- function(actual, predicted, residuals, k, n) {
  # Calculate Mean Absolute Error (MAE)
  mae <- mean(abs(actual - predicted))
  
  # Calculate Root Mean Squared Error (RMSE)
  rmse <- sqrt(mean((actual - predicted)^2))
  
  # Calculate Mean Absolute Percentage Error (MAPE)
  mape <- mean(abs((actual - predicted) / actual)) * 100
  
  sse <- sum(residuals^2)
  # Calculate AIC using the formula provided
  estimated_variance <- sum(residuals^2) / n
  
  # Compute AIC
  aic <- 2*k + (n*log(2*pi)) + (n*log(sse/n)) + n
  
  # Compute AICc
  aicc <- aic + ((2*k^2) + (2*k)) / (n - k - 1)
  
  
  log_likelihood <- -0.5 * n * (log(2 * pi * estimated_variance) + 1)
  
  # Calculate BIC
  bic <- log(n) * k - 2 * log_likelihood
  
  # Print the results
  cat("MAE: ", mae, "\n")
  cat("RMSE: ", rmse, "\n")
  cat("MAPE: ", mape, "\n")
  cat("AICc: ", aicc, "\n")
  cat("BIC: ", bic, "\n")
}

# Holt-Winters (a = 0.9436852, b = 0.008545873, g = 1)

# Train
model_holtwinters_train = HoltWinters(train)
summary(model_holtwinters_train)
er_holtwinters_train = resid(model_holtwinters_train)
rmse_holtwinters_train = sqrt(mean(er_holtwinters_train^2))

# Test
forecast_holtwinters=forecast(model_holtwinters_train, h=36, level=21)
yhat_holtwinters = forecast_holtwinters$upper
rmse_forecast_holtwinters=sqrt(mean((test - yhat_holtwinters)^2))

# Plot
plot(dt, ylab="CPI", xlim=c(2009, 2026), main='CPI Prediction with Best Holt-Winters Model', xlab='Year')
lines(model_holtwinters_train$fitted[,1], lty=1, col="blue")
lines(yhat_holtwinters, lty=1, col='red')
legend("topright", legend=c("Training", "Testing"), col=c("blue", "red"), lty=1)


resid(model_holtwinters_train)
resid_holtwinters_test = test-yhat_holtwinters
# Jumlah parameter = 17 (alpha, beta, gamma, a, b, and seasonal factor)
calculate_metrics(train[13:144], model_holtwinters_train$fitted[,1], resid(model_holtwinters_train), 17, 
                  length(train))
calculate_metrics(test, yhat_holtwinters, resid_holtwinters_test, 17, 
                  length(test))


# Naive
model_naive_train = snaive(train, h=36, level=21)
yhat_naive_train = model_naive_train$mean
summary(model_naive_train)

plot(dt, ylab="IHK", xlim=c(2009, 2028), main='CPI Prediction with Naive')
lines(model_naive_train$fitted, col='blue', lty=1)
lines(model_naive_train$upper, col='red', lty=1)
legend("topright", legend=c("Training", "Testing"), col=c("blue", "red"), lty=1)
resid_naive_test=test-model_naive_train$upper

calculate_metrics(train[13:144], model_naive_train$fitted[13:144], resid(model_naive_train)[13:144], 0, 
                  length(train[13:144]))
calculate_metrics(test, model_naive_train$upper, resid_naive_test, 0, 
                  length(test))

# SARIMA
library(tseries)
library(caret)
library(car)
library(nortest)
library(lmtest)
# Model ARIMA SEASONAL
plot.ts(dt)

# Uji stasioneritas
summary(powerTransform(dt)) # Gagal tolak H0. Stasioner terhadap varians
adf.test(dt) # Belum stasioner terhadap mean.

d1=diff(train)
adf.test(d1)
d2=diff(train, differences=2)
adf.test(d2)

acf(d1, lag.max = 72) 
pacf(d1, lag.max = 72) # Tidak ada yang melewati garis
acf(d2, lag.max = 72) # cut-off (yang bisa dicoba 0 dan 1)
pacf(d2, lag.max=72) # cut-off (yang bisa dicoba 1, 2, 3, 4)

# SARIMA(1, 2, 0)(2, 2, 0)[12]
sarima3 <- arima(train, order = c(1, 2, 0), seasonal = c(1, 0, 1, 12))
summary(sarima3)
coeftest(sarima3)
# Signifikan
err3=resid(sarima3)
Box.test(err3, type='Ljung-Box')
lillie.test(err3)

yhat_sarima3 = fitted.values(sarima3)
forecast3 = forecast(sarima3, h=36)
plot(dt, ylab="IHK", xlim=c(2009, 2023), main='Prediksi IHK dengan SARIMA(1, 2, 0)(1, 0, 1)[12]', ylim=c(90, 145))
lines(yhat_sarima3, col='blue', lty=2)
lines(forecast3$mean, col='red', lty=2)

calculate_metrics(train, yhat_sarima3, resid(sarima3), 3, 
                  length(train))
calculate_metrics(test, forecast3$mean, (test-forecast3$mean), 3, 
                  length(test))

# SARIMA(2, 2, 0)(2, 2, 0)[12]
sarima7 <- arima(train, order = c(2, 2, 0), seasonal = c(1, 0, 1, 12))
summary(sarima7)
coeftest(sarima7)
err7=resid(sarima7)
Box.test(err7, type='Ljung-Box')
lillie.test(err7)
# Signifikan

yhat_sarima7 = fitted.values(sarima7)
forecast7 = forecast(sarima7, h=36)
plot(dt, ylab="IHK", xlim=c(2009, 2023), main='Prediksi IHK dengan SARIMA(2, 2, 0)(1, 0, 1)[12]', ylim=c(90, 145))
lines(yhat_sarima7, col='blue', lty=2)
lines(forecast7$mean, col='red', lty=2)

calculate_metrics(train, yhat_sarima7, resid(sarima7), 3, 
                  length(train))
calculate_metrics(test, forecast7$mean, (test-forecast7$mean), 3, 
                  length(test))

# Regression
library(caret)
dummy = dummyVars("~.", data=df)
hasil = data.frame(predict(dummy, newdata=df))
head(hasil)
nrow(hasil)

hasil$t = seq(1:length(dt))

head(hasil)
tail(hasil)

dataset = hasil[, -c(4, 14)]
head(dataset)
dataset_train=dataset[1:144, 1:13]
dataset_test=dataset[145:180, 1:13]

# Kalo t tidak signifikan, tidak usah masukkan t.
# Kalau semua dummy tidak signifikan, maka kita salah menentukan ordo s
# Kalau ada salah satu yang tidak signifikan (p-value), maka cari tahu kenapa.

# Kita jadikan januari sebagai kontrol.
# Lihat p-value, mana yang tidak signifikan.

# Lihat di decompose. Karena yang jadi kontrol adalah januari, maka karena 
# februari dan maret nilainya mirip dengan januari, maka dia tidak signifikan.

model_tsr1 = lm(IHK~., data=dataset_train)
summary(model_tsr1) # yang signifikan hanya intercept
yhat_fitted = ts(model_tsr1$fitted.values, start=2009, end=2020)
er = resid(model_tsr1)
accuracy(model_tsr1)

predictions <- ts(predict(model_tsr1, newdata = dataset_test), start=2020, end=2023)

plot(dt, ylab="IHK", xlim=c(2009, 2023), main='Prediksi IHK dengan Regresi')
lines(yhat_fitted, col='blue', lty=2)
lines(predictions, col='red', lty=2)

predictions <- predict(model_tsr1, newdata = dataset_test)
resid_tsr1_test = test-predictions

calculate_metrics(train, model_tsr1$fitted.values, resid(model_tsr1), 13, length(train))
calculate_metrics(test, predictions, resid(model_tsr1), 13, length(test))
AICc(model_tsr1)
BIC(model_tsr1)

bptest(model_tsr1) # Heteroskedastisitas
dwtest(model_tsr1) # Error berautokorelasi
lillie.test(er)
model_tsr2 = lm(IHK~BulanJanuari+BulanFebruari+BulanMaret+BulanApril+BulanMei+BulanJuni
                +BulanJuli+BulanAgustus+BulanSeptember+BulanOktober+BulanNovember, data=dataset_train)
summary(model_tsr2) # yang signifikan hanya intercept
er = resid(model_tsr2)

# Regresi y-1
x = dataset_train[2:144,]
xt1 = dataset_train$IHK[1:143]
lag1 = data.frame(x, xt1)
head(lag1)
model_tsr2 = lm(IHK~BulanJanuari+BulanFebruari+BulanMaret+BulanApril+BulanMei+BulanJuni
                +BulanJuli+BulanAgustus+BulanSeptember+BulanOktober+BulanNovember+xt1, data=lag1)
summary(model_tsr2) # hanya januari dan xt-1 yang signifikan
er2 = resid(model_tsr2)
bptest(model_tsr2) # Heteroskedastisitas
dwtest(model_tsr2) # Error berautokorelasi
lillie.test(er2)


x = dataset_test[2:36,]
xt1 = dataset_test$IHK[1:35]
lag1 = data.frame(x, xt1)

yhat_fitted2 = ts(model_tsr2$fitted.values, start=2009, end=2020)
prediction2 <- ts(predict(model_tsr2, newdata = lag1), start=2020, end=2023)

plot(dt, ylab="IHK", xlim=c(2009, 2023), main='Prediksi IHK dengan Regresi')
lines(yhat_fitted2, col='blue', lty=2)
lines(prediction2, col='red', lty=2)

prediction2 <- predict(model_tsr2, newdata = lag1)
resid_tsr2_test = test[2:36]-length(prediction2)

calculate_metrics(lag1, model_tsr2$fitted.values, resid(model_tsr2), 13, length(lag1))
calculate_metrics(test, prediction, resid(model_tsr1), 13, length(test))
AICc(model_tsr1)
BIC(model_tsr1)

# Model NN
library(forecast)

# Define the function
run_model_until_target_rmse <- function(train_data, target_rmse, size = 6, P = 1, p = 12, h = 36) {
  rmse <- Inf
  iteration <- 0
  model_nn <- NULL
  yhat_nn_test <- NULL
  
  while (rmse >= target_rmse) {
    iteration <- iteration + 1
    cat("Iteration:", iteration, "\n")
    
    # Fit the model
    model_nn <- nnetar(train_data, size = size, P = P, p = p, repeats=20)
    
    # Generate forecast
    yhat_nn_test <- forecast(model_nn, h = h)
    
    # Calculate RMSE
    rmse <- sqrt(mean((yhat_nn_test$mean - test)^2))
    
    cat("Current RMSE:", rmse, "\n")
  }
  
  cat("Final RMSE:", rmse, "\n")
  
  return(list(model = model_nn, forecast = yhat_nn_test$mean))
}

# Example usage:
pacf(train)


result1 <- run_model_until_target_rmse(p=3, size=10, P=1, train, target_rmse = 1.5)
autoplot(forecast(result1$model, h=36))

nn_forecast1 = forecast(result1$model, h=36)
plot(dt, ylab="CPI", xlim=c(2009, 2026), main='CPI Prediction with NNAR(p=3, P=1, size=10)[12]', xlab='Year')
lines(result1$model$fitted, lty=1, col="blue")
lines(nn_forecast1$mean, lty=1, col='red')
legend("topright", legend=c("Training", "Testing"), col=c("blue", "red"), lty=1)
calculate_metrics(train[13:144], result1$model$fitted[13:144], result1$model$residuals[13:144], 49, length(train[13:144]))
calculate_metrics(test, result1$forecast, test-result1$forecast, 49, length(test))


result2 <- run_model_until_target_rmse(p=3, size=12, P=1, train, target_rmse = 1.5)
autoplot(forecast(result2$model, h=36))
nn_forecast2 = forecast(result2$model, h=36)
result2$model$model
plot(dt, ylab="CPI", xlim=c(2009, 2026), main='CPI Prediction with NNAR(p=3, P=1, size=12)[12]', xlab='Year')
lines(result2$model$fitted, lty=1, col="blue")
lines(nn_forecast2$mean, lty=1, col='red')
legend("topright", legend=c("Training", "Testing"), col=c("blue", "red"), lty=1)
calculate_metrics(train[13:144], result2$model$fitted[13:144], result2$model$residuals[13:144], 73, length(train[13:144]))
calculate_metrics(test, result2$forecast, test-result2$forecast, 73, length(test))

result3 <- run_model_until_target_rmse(p=3, size=8, P=1, train, target_rmse = 1.5)
autoplot(forecast(result3$model, h=36))
nn_forecast3 = forecast(result3$model, h=36)
result3$model$model
plot(dt, ylab="CPI", xlim=c(2009, 2026), main='CPI Prediction with NNAR(p=3, P=1, size=8)[12]', xlab='Year')
lines(nn_forecast3$model$fitted, lty=1, col="blue")
lines(nn_forecast3$mean, lty=1, col='red')
legend("topright", legend=c("Training", "Testing"), col=c("blue", "red"), lty=1)
calculate_metrics(train[13:144], result3$model$fitted[13:144], result3$model$residuals[13:144], 73, length(train[13:144]))
calculate_metrics(test, result3$forecast, test-result3$forecast, 73, length(test))

result4 <- run_model_until_target_rmse(p=2, size=10, P=1, train, target_rmse = 1.5)
autoplot(forecast(result4$model, h=36))
nn_forecast4 = forecast(result4$model, h=36)
result4$model$model
plot(dt, ylab="CPI", xlim=c(2009, 2026), main='CPI Prediction with NNAR(p=2, P=1, size=10)[12]', xlab='Year')
lines(result4$model$fitted, lty=1, col="blue")
lines(nn_forecast4$mean, lty=1, col='red')
legend("topright", legend=c("Training", "Testing"), col=c("blue", "red"), lty=1)
calculate_metrics(train[13:144], result4$model$fitted[13:144], result4$model$residuals[13:144], 51, length(train[13:144]))
calculate_metrics(test, result4$forecast, test-result4$forecast, 51, length(test))


result5 <- run_model_until_target_rmse(p=2, size=12, P=1, train, target_rmse = 1.5)
autoplot(forecast(result5$model, h=36))
nn_forecast5 = forecast(result5$model, h=36)
result5$model$model
plot(dt, ylab="CPI", xlim=c(2009, 2026), main='CPI Prediction with NNAR(p=2, P=1, size=12)[12]', xlab='Year')
lines(result5$model$fitted, lty=1, col="blue")
lines(nn_forecast5$mean, lty=1, col='red')
legend("topright", legend=c("Training", "Testing"), col=c("blue", "red"), lty=1)
calculate_metrics(train[13:144], result5$model$fitted[13:144], result5$model$residuals[13:144], 61, length(train[13:144]))
calculate_metrics(test, result5$forecast, test-result5$forecast, 61, length(test))

result6 <- run_model_until_target_rmse(p=2, size=8, P=1, train, target_rmse = 1.5)
autoplot(forecast(result6$model, h=36))
nn_forecast6 = forecast(result6$model, h=36)
result6$model$model
plot(dt, ylab="CPI", xlim=c(2009, 2026), main='CPI Prediction with NNAR(p=2, P=1, size=8)[12]', xlab='Year')
lines(result6$model$fitted, lty=1, col="blue")
lines(nn_forecast6$mean, lty=1, col='red')
legend("topright", legend=c("Training", "Testing"), col=c("blue", "red"), lty=1)
calculate_metrics(train[13:144], result6$model$fitted[13:144], result6$model$residuals[13:144], 61, length(train[13:144]))
calculate_metrics(test, result6$forecast, test-result6$forecast, 61, length(test))