library(rpart)
library(rpart.plot)
library(randomForest)
library(gbm)
source("tel_cust_cleanup.R")


#create tree
churn3 = rpart(churn ~ ., data=train_reduced_num, method = "class")
summary(churn3)

#plot tree with names
plot(churn3)
text(churn3, pretty=T)

rpart.plot(churn3)

#make a prediction based on tree
pr <- predict(churn3, newdata = test_reduced_num, type = "class")
mean(pr==test_reduced_num$churn)
misclass_tree <- table(predict = pr, truth = test_reduced_num$churn)
accuracy_Test <- sum(diag(misclass_tree)) / sum(misclass_tree)
accuracy_Test



#random-forest
churnRF = randomForest(churn ~ ., data = train_reduced_num)
plot(churnRF)

#predykcja
pred = predict(churnRF, test_reduced_num)

#wartość powyżej mediany przyjmujemy jako 1
summary(pred)
df <- data.frame(pred)
df$pred <- ifelse(df$pred > 0.4826,1,0)
mean(df$pred == test_reduced_num$churn)



#boosting
train_reduced_num$refurb_new <- as.factor(train_reduced_num$refurb_new)
train_reduced_num$rv <- as.factor(train_reduced_num$rv)
train_reduced_num$asl_flag <- as.factor(train_reduced_num$asl_flag)
boost.churn = gbm(churn ~ ., data = train_reduced_num, distribution = "gaussian",
                   n.trees = 10000)
summary(boost.churn)

#predykcja
pred_boost = predict(boost.churn, newdata = test_reduced_num, n.trees = 10000)
#wartość powyżej mediany przyjmujemy jako 1
summary(pred_boost)
df_boost <- data.frame(pred_boost)
df_boost$pred_boost <- ifelse(df_boost$pred_boost > 0.4581,1,0)
mean(df_boost$pred_boost == test_reduced_num$churn)