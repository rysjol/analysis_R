# reading files

library(data.table)
library(ggplot2)
library(reshape2)
library(dplyr)
library(tidyr)
library(xtable)

a = read.csv('avocado.csv')
summary(a)
sapply(a, class)
plot(a[,1:3])


b = read.csv('Telecom_customer_churn.csv')
summary(b)
sapply(b, class)
boxplot(b[,7])
