library(tseries)
library(forecast)
library(lmtest)
library(readxl)


#Read data
data = read_xlsx("C:/Users/RINI/Documents/SEMESTER 6/ANALISIS DERET WAKTU I/data fix kali.xlsx")
dataemas = ts(data$Harga)
dataemas

#Training dan Testing
train = head(dataemas, 140)
train
test = tail(dataemas, 15)
test

#1. Deskripsi data
summary(dataemas)

#2. Data Asli
plot.ts(dataemas, main="Plot Harga Emas di Amerika Serikat", ylab="Harga", xlab="Hari")

#3. Data train dan test
par(mfrow=c(1,2))
plot.ts(train, main="Plot Data Training", ylab="Harga", xlab="Hari")
plot.ts(test, main="Plot Data Testing", ylab="Harga", xlab="Hari")

#4. uji Stationer terhadap varians
library(forecast)
FitAR::BoxCox(train)

#5. uji stationer terhadap mean 
adf.test(train)

#6. Plot acf dan paf
acf(train, lag.max = 100, main="Plot ACF Data Train")
pacf(train, lag.max = 100, main="Plot PACF Data Train")

#7. Differencing d=1
emasdiff1 = diff(train, differences=1)
emasdiff1
plot.ts(emasdiff1, main="Plot Diferencing d=1")

#8. Plot ACF dan PACF
acf(emasdiff1, lag.max = 100, main="Plot ACF Diff1")
pacf(emasdiff1, lag.max = 100, main="Plot PACF Diff1")

#9. Differencing d=2
emasdiff2 = diff(emasdiff1, differences=1)
emasdiff2
plot.ts(emasdiff2, main="Plot Diferencing d=2")

#10. Plot ACF dan PACF
acf(emasdiff2, lag.max = 100, main="Plot ACF Diff2")
pacf(emasdiff2, , lag.max = 100, main="Plot ACF Diff2")

#11. Uji Stasioneritas
adf.test(emasdiff1)
adf.test(emasdiff2)


#Model ARIMA 
#12. Signifikan
emasarima1 = arima(train, order=c(1,1,1), include.mean = TRUE, method = "ML")
emasarima1
coeftest(emasarima1)
Box.test(emasarima1$residuals, type = "Ljung")
shapiro.test(emasarima1$residuals)

#13. Tidak signifikan
emasarima2 = arima(train, order=c(1,2,1),include.mean = TRUE, method = "ML")
emasarima2
coeftest(emasarima2)
Box.test(emasarima2$residuals, type = "Ljung")
shapiro.test(emasarima2$residuals)

#Model IMA
#14. Signifikan
emasima1 = arima(train, order=c(0,2,1), include.mean = TRUE, method = "ML")
emasima1
coeftest(emasima1)
Box.test(emasima1$residuals, type = "Ljung")
shapiro.test(emasima1$residuals)

#15. Tidak signifikan
emasima2 = arima(train, order=c(0,2,2), include.mean = TRUE, method = "ML")
emasima2
coeftest(emasima2)
Box.test(emasima2$residuals, type = "Ljung")
shapiro.test(emasima2$residuals)

#Model AR
#16. Tidak signifikan
emasari1 = arima(train, order=c(1,2,0), include.mean = TRUE, method = "ML")
emasari1
coeftest(emasari1)
Box.test(emasari1$residuals, type = "Ljung")
shapiro.test(emasari1$residuals)

#17. Signifikan
emasari2 = arima(train, order=c(2,2,0), include.mean = TRUE, method = "ML")
emasari2
coeftest(emasari2)
Box.test(emasari2$residuals, type = "Ljung")
shapiro.test(emasari2$residuals)

#18. Forecast

model1 = Arima(test, model=emasarima1)
plot(test, main="Peramalan ARIMA(1,1,1)",ylab="Harga Emas")
lines(window(fitted(model1)), col="red")
forecast1 = fitted(model1)
forecast1
accuracy(model1)
AIC(model1)

model2 = Arima(test, model=emasima1)
plot(test, main="Peramalan ARIMA(0,2,1)",ylab="Harga Emas")
lines(window(fitted(model2)), col="red")
forecast2 = fitted(model2)
forecast2
accuracy(model2)
AIC(model2)

model3 = Arima(test, model=emasari2)
plot(test, main="Peramalan ARIMA(2,2,0)",ylab="Harga Emas")
lines(window(fitted(model3)), col="red")
forecast3 = fitted(model3)
forecast3
accuracy(model3)
AIC(model3)


accuracy(emasarima1)
accuracy(emasima1)
accuracy(emasari2)
AIC(emasarima1)
AIC(emasima1)
AIC(emasari2)

accuracy(model1)
accuracy(model2)
accuracy(model3)
AIC(model1)
AIC(model2)
AIC(model3)


#Forecasting tanggal 3 April
frcst=forecast(model2,h=1)
frcst

