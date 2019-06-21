#avocado.csv
library(ggplot2)
library(splines)
library(knitr)

source("avocado_cleanup.R")

#avocado <- read.csv('../avocado.csv')
conven <- subset(avocado, type == conventiona)

#Wykres zależności ceny awokado od całkowitej ilości zamówionych sztuk 
pl1 = ggplot(avocado, aes(x=Total.Volume, y=AveragePrice)) +
  geom_point(color="blue") + theme_bw()

#Regresja liniowa dla ceny awokado w zależnosći od ilośći sprzedanych sztuk
fit1=lm(AveragePrice~Total.Volume,data=avocado)
pl1 <- pl1 +
  geom_abline(slope=fit1$coefficients[2],
              intercept=fit1$coefficients[1], colour="red")
pl1

#Wyświetlenie summary oraz współczynników wraz z p-value
summary(fit1)
A <- data.frame(summary(fit1)$coef)
A[,4] <- format.pval(summary(fit1)$coeff[,4],
                     eps=0.001, digits=2)
kable(A, digits=10, col.names=c('współczynnik', 'SE',
                               't', 'p-value'))


#Regresja liniowa dla większej liczby współczynników wraz z wyświetleniem współczynników
fit2=lm(AveragePrice~Total.Bags + Total.Volume + Large.Bags + type,data=avocado)
summary(fit2)
A <- data.frame(summary(fit2)$coef)
A[,4] <- format.pval(summary(fit2)$coeff[,4],
                     eps=0.001, digits=2)
row.names(A) <- c('Intercept', 'type (organic)')
kable(A, digits=10, col.names=c('współczynnik', 'SE',
                                't', 'p-value'))

#Regresja liniowa dla ceny w zależności od całkowitej liczby siatek (?) wraz ze spline'ami
#Wartości 'knots' dobierane eksperymentalnie i 'na oko' po wyświetleniu zależności AveragePrice ~ Total.Bags w ggplot
model = lm(AveragePrice ~ bs(Total.Bags, knots=c(1e+06, 4.5e+06, 7.5e+06, 1e+07)), data = avocado)
pl = ggplot(avocado, aes(x = Total.Bags, y=AveragePrice)) +
  geom_point(color="blue") + theme_bw() + stat_smooth(method = "lm",
  formula = "y ~ bs(x, knots=c(1e+06, 4.5e+06, 7.5e+06, 1e+07))")
pl
summary(model)

#Regresja liniowa dla ceny w zależności od całkowitej liczby siatek (?) wraz ze spline'ami
#Wartości 'knots' dobierane automatycznie przez funkcję bs
model2 = lm(AveragePrice ~ bs(Total.Bags, df=7, degree=3), data = avocado)
pl2 = ggplot(avocado, aes(x = Total.Bags, y=AveragePrice)) +
  geom_point(color="blue") + theme_bw() + stat_smooth(method = "lm",
  formula = "y ~ bs(x, df=7, degree=3)")
pl2
summary(model2)

#Predykcja ceny awokado przy dużej ilości zamówionych siatek dla obu modeli
pred1 = predict(model, list(Total.Bags = c(1.7e+07, 1.8e+07, 1.85e+07)))
pred1
pred2 =predict(model2, list(Total.Bags = c(1.7e+07, 1.8e+07, 1.85e+07)))
pred2
