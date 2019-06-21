library(e1071)
source("tel_cust_cleanup.R")


#zmiana wartości 'chur' z (1,0) na (TRUE, FALSE)
train_reduced_num$churn <- as.factor(train_reduced_num$churn)


df <- data.frame(train_reduced_num$churn, train_reduced_num$rev_Mean, train_reduced_num$mou_Mean)
names(df) <- c("churn", "rev_Mean", "mou_Mean")

# kernel = linear
# Dobieram eksperymentalnie c = 5
lin_fit = svm(churn ~ ., data = df, kernel = "linear", cost = 5, scale = FALSE)
plot(lin_fit, df)

# Próbuję znaleźć najlepszy model automatycznie
tune.out <- tune(svm, churn ~ ., data = df, kernel = "linear",
                 ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
# extract the best model
best_lin_mod <- tune.out$best.model
plot(best_lin_mod, df)

# Predykcja i ocena błędu klasyfikacji dla obu modeli
#c dobrane przeze mnie
pred_lin <- predict(lin_fit, test_reduced_num)
(misclass_lin <- table(predict = pred_lin, truth = test_reduced_num$churn))

#c z best.model
pred_best <- predict(best_lin_mod, test_reduced_num)
(misclass_best <- table(predict = pred_best, truth = test_reduced_num$churn))


# kernel = polynomial
pol_fit = svm(churn ~ ., data = df, kernel = "polynomial", cost = 5, scale = FALSE)
plot(pol_fit, df)

# Predykcja
pred_pol <- predict(pol_fit, test_reduced_num)
(misclass_pol <- table(predict = pred_pol, truth = test_reduced_num$churn))

