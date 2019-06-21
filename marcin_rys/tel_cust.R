#Telecom_customer_churn.csv
library(ggplot2)
library(knitr)

source("tel_cust_cleanup.R")
#tel_cust <- read.csv('Telecom_customer_churn.csv')


pl1 = ggplot(tel_cust, aes(x=mou_Mean, y=churn)) +
  geom_point(color="blue") + theme_bw()
pl1

fit1=lm(churn~mou_Mean,data=tel_cust)
pl1 <- pl1 +
  geom_abline(slope=fit1$coefficients[2],
              intercept=fit1$coefficients[1], colour="red")
pl1



A <- data.frame(summary(fit1)$coef)
A[,4] <-format.pval(summary(fit1)$coeff[,4], eps=0.001,
                    digits=2)
kable(A, digits=2, col.names=c('coef', 'SE', 't', 'p'))


#Regresja logistyczna dla churn w zależności od pozostałych parametrów numerycznych
mod = glm(churn ~ ., data = train_reduced_num, family = binomial)
summary(mod)

#Predykcja dla zbioru testowego
prediction <- predict(mod, newdata = test_reduced_num, type = 'response')
summary(prediction)
pred_df <- data.frame(CustomerID=names(prediction), Predicted_Churn=prediction, row.names=NULL)
ggplot(pred_df, aes(CustomerID, Predicted_Churn)) + geom_point(color="blue") + theme_bw() + geom_abline(slope = 0, intercept = 0.5048, colour = 'red')

#Ponieważ churn jest wartością logiczną, a wyniki otrzymane z predykcji mają wartości rzeczywiste z zakresu (0,1)
# przyjmijmy, że wartość predykcji powyżej mediany (0.5048, czerwona linia w ggplot) traktujemy jako 1
pred_df$Predicted_Churn <- ifelse(pred_df$Predicted_Churn > 0.4756,1,0)
ggplot(pred_df, aes(CustomerID, Predicted_Churn)) + geom_point(color="blue") + theme_bw()

#Liczymy błąd klsyfikacji (wynik około 0.59) wg przykładu tu: https://www.r-bloggers.com/how-to-perform-a-logistic-regression-in-r/
classification_error <- mean(pred_df$Predicted_Churn != test_reduced_num$churn)
paste('Classification accuracy: ',1-classification_error)

summary(tel_cust)
head(tel_cust)
str(tel_cust)