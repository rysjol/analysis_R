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
pred_df$Predicted_Churn <- ifelse(pred_df$Predicted_Churn > 0.5048,1,0)
ggplot(pred_df, aes(CustomerID, Predicted_Churn)) + geom_point(color="blue") + theme_bw()

#Liczymy błąd klsyfikacji (wynik około 0.59) wg przykładu tu: https://www.r-bloggers.com/how-to-perform-a-logistic-regression-in-r/
classification_error <- mean(pred_df$Predicted_Churn != test_reduced_num$churn)
paste('Classification accuracy: ',1-classification_error)