# libraries 
library(ggplot2)
library(dplyr)
library(lubridate)

# reading the data
av = read.csv('../avocado.csv', colClasses = c('NULL', 'Date', 'numeric', 'numeric', 'numeric', 
                                            'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric',
                                            'factor','numeric','factor'))

# calculate mean volume for each region and sort set
av %>% group_by(region) %>% 
  summarise(volume = mean(Total.Volume)) -> vol4reg
vol4reg <- vol4reg[order(vol4reg$volume),]
# extracting regions names
regList <- lapply(vol4reg$region, as.character)
len <- length(regList)

# Number of regions to analyze
n <- 3

#extract first 'n' regions
for (i in 1:(n)){
  if(i == 1 ){
    low_reg <- regList[i]
  } else {
    low_reg[i] <- regList[i]
  }
}

#extract 'n' regions in the middle of the set. if 'n' is even, then extract n+1 regions
for (i in ceiling(len/2-1/2*n):floor(len/2+1/2*n)){
  if(i == ceiling(len/2-1/2*n)){
    med_reg <- regList[i]
    print(i)
  } else {
    med_reg[i + 1 - ceiling(len/2-1/2*n)] <- regList[i]
    print(i)
  }
}

#extract 'n' regions from the end of the set
for (i in (len-n+1):(len)){
  if(i == (len-n+1)){
    high_reg <- regList[i]
  } else {
    high_reg[i + 1 - (len-n+1)] <- regList[i]
  }
}

# creating subsets for each extracted set of data
av_reg_low <- av[av$region %in% low_reg,]
av_reg_med <- av[av$region %in% med_reg,]
av_reg_high <- av[av$region %in% high_reg,]

# extracting data for the plots
av_reg_low %>% group_by(month = floor_date(Date, "month"), region) %>%
  summarize(volume = mean(Total.Volume)) -> avAg_low
av_reg_med %>% group_by(month = floor_date(Date, "month"), region) %>%
  summarize(volume = mean(Total.Volume)) -> avAg_med
av_reg_high %>% group_by(month = floor_date(Date, "month"), region) %>%
  summarize(volume = mean(Total.Volume)) -> avAg_high

# plot data
ggplot(data = avAg_low, aes(x = month, y = volume, col = region)) + geom_point() + geom_line(size = 0.1)
ggplot(data = avAg_med, aes(x = month, y = volume, col = region)) + geom_point() + geom_line(size = 0.1)
ggplot(data = avAg_high, aes(x = month, y = volume, col = region)) + geom_point() + geom_line(size = 0.1)

 