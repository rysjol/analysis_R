#install.packages("RcppArmadillo")
#install.packages("uroot")
#install.packages("data.table")
#install.packages("dplyr")
##install.packages("https://cran.r-project.org/src/contrib/Archive/RcppArmadillo/RcppArmadillo_0.6.100.0.0.tar.gz", repos=NULL, type="source")
#install.packages("imputeTS")
#install.packages("corrplot")
#install.packages("ggplot2")
#install.packages("gridExtra")
#install.packages("caret")
library(dplyr)
#library(caret)
library(ggplot2)
#library(imputeTS)
#library(gridExtra)
#library(grid)
#library(corrplot)

# read 'avocado.csv'
avocado <- read.csv('../avocado.csv')

# drop column with ID
avocado <- avocado[-1]

# delete each row containing 'NA'
for (i in 1:ncol(avocado)) {
  avocado <- subset(avocado, !is.na(avocado[i]))
}

# drop each non-numeric (and non-integer) column and save to 'avocado_num'
drop.cols <- c()
for (i in 1:ncol(avocado)) {
  colname <- colnames(avocado[i])
  if(sapply(avocado[i], class) == "numeric" || sapply(avocado[i], class) == "integer"){
    next
  } else {
    drop.cols <- c(drop.cols, colname)
  }
}
avocado_num <- avocado %>% select(-one_of(drop.cols))