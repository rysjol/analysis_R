install.packages("dplyr")
install.packages("https://cran.r-project.org/src/contrib/Archive/RcppArmadillo/RcppArmadillo_0.6.100.0.0.tar.gz", repos=NULL, type="source")
install.packages("imputeTS")
install.packages("corrplot")
install.packages("ggplot2")
install.packages("gridExtra")
install.packages("caret")
library(caret)
library(ggplot2)
library(imputeTS)
library(gridExtra)
library(grid)
library(dplyr)
library(corrplot)
train_test_split <- function(x,s,data,p){
  # x should be a column from dataset without NaN values
  library(caret)
  set.seed(s)
  trainIndex <- createDataPartition(x, 
                                    p = p, 
                                    list = FALSE, 
                                    times = 1)
  
  dataTrain <- data[ trainIndex,]
  dataTest  <- data[-trainIndex,]
  return(list("train" = dataTrain, "test" = dataTest))
}
# The target variable here is churn which explains whether the customer will churn or not.
# 

# load dataset
telco <- read.csv("~/ED-2018/Telecom_customer_churn.csv")
telco_analysis <- telco %>%
  select(rev_Mean, mou_Mean, totmrc_Mean, Customer_ID, uniqsubs, 
         totcalls, refurb_new, recv_sms_Mean, avgmou, avgqty, avgrev, avg6qty,
         avg6mou, avg6rev, avg3qty, avg3mou, avg3rev, iwylis_vce_Mean, mouiwylisv_Mean, 
         peak_vce_Mean, vceovr_Mean, months, mou_opkd_Mean, adjmou, adjrev, adjqty,
         dualband, churn, new_cell, recv_vce_Mean, unan_vce_Mean, roam_Mean, blck_vce_Mean, 
         models, inonemin_Mean, comp_vce_Mean, plcd_vce_Mean, threeway_Mean, owylis_vce_Mean,
         mou_cdat_Mean, opk_vce_Mean, mou_opkv_Mean, area, rv, asl_flag,
         ownrent, lor, income, eqpdays, totmou, complete_Mean, actvsubs, totrev,
         prizm_social_one, HHstatin, hnd_webcap, truck, creditcd, da_Mean, ovrmou_Mean,
         hnd_price, marital, adults)
telco_analysis[telco_analysis == ""] <- NA
telco_analysis$uniqsubs <- as.numeric(telco_analysis$uniqsubs)
telco_analysis$uniqsubs <- ifelse(telco_analysis$uniqsubs>20, median(telco_analysis$uniqsubs) , telco_analysis$uniqsubs)
telco_analysis$totcalls <- as.numeric(telco_analysis$totcalls)
levels(telco_analysis$refurb_new) <- c("NA", "FALSE", "TRUE")
telco_analysis$refurb_new <- as.logical(telco_analysis$refurb_new)
telco_analysis <- subset(telco_analysis, !is.na(avg6qty))
telco_analysis <- subset(telco_analysis, !is.na(rev_Mean))
telco_analysis <- subset(telco_analysis, !is.na(mou_Mean))
telco_analysis <- subset(telco_analysis, !is.na(totmrc_Mean))
telco_analysis <- subset(telco_analysis, !is.na(avg6mou))
telco_analysis <- subset(telco_analysis, !is.na(avg6rev))
telco_analysis <- subset(telco_analysis, !is.na(vceovr_Mean))
telco_analysis$churn <- as.logical(telco_analysis$churn)
telco_analysis$models <- as.integer(telco_analysis$models)
telco_analysis <- subset(telco_analysis, !is.na(roam_Mean))
telco_analysis <- subset(telco_analysis, !is.na(blck_vce_Mean))
telco_analysis <- subset(telco_analysis, dualband != "U")
telco_analysis$rv[is.na(telco_analysis$rv)] = 0
levels(telco_analysis$asl_flag) <- c(0,1)
telco_analysis$asl_flag <- as.integer(telco_analysis$asl_flag)
telco_analysis$asl_flag <- as.logical(telco_analysis$asl_flag)
telco_analysis$rv <- as.logical(telco_analysis$rv)
telco_analysis$ownrent = factor(telco_analysis$ownrent, levels=c(levels(telco_analysis$ownrent), 'U'))
telco_analysis$ownrent[is.na(telco_analysis$ownrent)] = 'U'
telco_analysis$lor[is.na(telco_analysis$lor)] = summary(telco_analysis$lor)[3]
telco_analysis$income[is.na(telco_analysis$income)] = summary(telco_analysis$income)[3]
telco_analysis$income = as.factor(telco_analysis$income)
telco_analysis <- subset(telco_analysis, !is.na(eqpdays))
telco_analysis$actvsubs <- as.integer(telco_analysis$actvsubs)
telco_analysis$actvsubs <- ifelse(telco_analysis$actvsubs>15, median(telco_analysis$actvsubs) , telco_analysis$actvsubs)
telco_analysis$avg6qty <- log(telco_analysis$avg6qty)
telco_analysis <- subset(telco_analysis, !is.na(truck))
telco_analysis <- subset(telco_analysis, !is.na(creditcd))
v1 = telco_analysis$da_Mean 
v2 = telco_analysis$ovrmou_Mean
v3 = telco_analysis$hnd_price 
v1[v1 > 0.66 * max(v1, rm.na = T)] = NA
v2[v2 > 0.66 * max(v2, rm.na = T)] = NA
v1[is.na(v1)] <- median(v1, na.rm = T)
v2[is.na(v2)] <- median(v2, na.rm = T)
v3[is.na(v3)] <- sample(v3[!is.na(v3)], sum(is.na(v3)))
telco_analysis$da_Mean <- v1
telco_analysis$ovrmou_Mean <- v2
telco_analysis$hnd_price <- v3
telco_analysis$marital[which(telco_analysis$marital == "A" | telco_analysis$marital == "B" | telco_analysis$marital == "U")] <- NA 
telco_analysis <- subset(telco_analysis, !is.na(marital))
telco_analysis$adults[is.na(telco_analysis$adults )] <- 7
telco_analysis$adults <- as.factor(telco_analysis$adults)
# removing NAs - correction
telco_analysis <- subset(telco_analysis, !is.na(prizm_social_one))
telco_analysis <- subset(telco_analysis, !is.na(HHstatin))
telco_analysis <- subset(telco_analysis, !is.na(hnd_webcap))


nrow(telco_analysis)
# train test split
seed = 7
ratio = 0.8
z <- train_test_split(telco_analysis$Customer_ID, seed, telco_analysis, ratio)
train <- z$train
test <- z$test

train_reduced <- z$train[seq(from = 1, to =  nrow(z$train), by = 70),]
test_reduced <- z$test[seq(from = 1, to =  nrow(z$test), by = 19),]
