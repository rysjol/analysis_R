#avocado.csv
library(ggplot2)
library(forecast)
library(dplyr)

#avocado <- read.csv('../avocado.csv')
source('avocado_cleanup.R')

pl1 = ggplot(avocado, aes(x=Date, y=AveragePrice)) +
  geom_point(color="blue") + theme_bw()
pl1

time_series_raw = ts(avocado %>% filter( region == 'Albany' & type == 'organic') %>% select(AveragePrice))
plot(time_series_raw)

#find periods of time_series and put it here (you can use acf and diff)
acf(time_series_raw, lag.max = 350)
# Funckja autokorelacji nie wskazuje powtarzalności w czasie poszczególnych wyników

# Z tego powodu wektor period jest do zakomentowania

#Załóżmy jednak że występuje minimalna okresowość co 40 tygodni, jak wtedy wygląda funkcja autokorelacji?
po_diff = diff (time_series_raw, lag = 60)
acf(po_diff, 200)
po_2diff = diff (po_diff, lag = 24)
acf (po_2diff, 200)
po_3diff = diff(po_2diff, lag = 40)
acf(po_3diff, 100)

#Załóżmy że znaleźliśmy wszystkie występujące okresowości w danych, dlatego tworzymy wektor 'periods'
periods = c(40, 24, 60)

#convert time_series to Multi-Seasonal Time Series (msts), include periods
time_series = msts(time_series_raw, seasonal.periods= periods)
plot(time_series)



#arima

#choose number of hamornics for each period, experiment with different values
# nie używamy bo dajemy seasonal = FALSE
K = c(3,3)

#fit a model, use auto.arima (it may take a while) or choose orders by hand
#we don't want SARIMA model, thus seasonal=TRUE, season is handled by fourier
model_auto = auto.arima(time_series_raw, seasonal=FALSE)

#warości p,d,q dobrane na podstawie porównania wszystkich możliwych wartości pod kątem parametrów AIC, AICC, BIC
#ktoś to zrobił na zajęciach i wyszło, że 4,0,2 są najlepsze
model = Arima(time_series_raw, order=c(4,0,2))  
summary(model_auto)
summary(model)

#próba przwidzenia 10 warości i porównania modelu auto.arima oraz modelu arima z wartościami p,d,q dobranymi w czasie zajęć
forecast_length = 10
forecasted_series_auto = forecast(model_auto, h=forecast_length)
forecasted_series = forecast(model, h=forecast_length)
plotted_previous_length = 200
plot(forecasted_series, plotted_previous_length)
plot(forecasted_series_auto, plotted_previous_length)
