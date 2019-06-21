# Laboratorium ED 2018 Lucjan Janowski

## 1. Czyszczenie i wczytywanie danych

Skrypt wczytujący 'avocado.csv' znajduje się w pliku 'avocado_cleanup.R'.

Wczytywanie pliku i usunięcie pierwszej kolumy, która zawiera numer rekordu:
```{r}
# read 'avocado.csv'
avocado <- read.csv('../avocado.csv')

# drop column with ID
avocado <- avocado[-1]
```

Usunięcie wartości "NA":
```{r}
# delete each row containing 'NA'
for (i in 1:ncol(avocado)) {
  avocado <- subset(avocado, !is.na(avocado[i]))
}
```

Usunięcie wszystkich kolumn, które zawierają dane innego typy niż "numeric" bądź "integer" (przyda nam się do PCA):
```{r}
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
```

Skrypt wczytujący 'Telecom_customer_churn.csv' znajduje się w pliku 'tel_cust_cleanup.R'.
Jest on zmodyfikowaną wersją skryptu wycinanie.R. Zbiór danych przygotowany przez wycinanie.R zostaje oczyszczony z wartości nienumerycznych i niecałkowitoliczbowych (jest takie słowo?), a następnie zostają usuniete z niego rekordy, które zawierają wartości "inf" bądź "na". Wynikowy zbiór danych ma 94 243 rekordów (oryginalny 100 000, po przejściu przez wycinanie 95 010) oraz 48 kolumn (oryginalny 100, po przejściu przez wyciannie 63).

```{r}
# drop each non-numeric (and non-integer) column and save to 'tel_cust_num'
drop.cols <- c()
for (i in 1:ncol(telco_analysis)) {
  colname <- colnames(telco_analysis[i])
  if(sapply(telco_analysis[i], class) == "numeric" || sapply(telco_analysis[i], class) == "integer"){
    next
  } else {
    drop.cols <- c(drop.cols, colname)
  }
}
tel_cust_num <- telco_analysis %>% select(-one_of(drop.cols))

# delete each row containing 'Inf', '-Inf' and 'NA'
for (i in 1:ncol(tel_cust_num)) {
  #change each 'Inf' to 'NA' first 
  is.na(tel_cust_num[i]) <- sapply(tel_cust_num[i], is.infinite)
  tel_cust_num <- subset(tel_cust_num, !is.na(tel_cust_num[i]))
}
```


## 2. Wizualizacja danych

Skrypt wizualizujący łączą ilość sprzedanych avocado w:
- n regionach, gdzie sprzedaż była największa,
- n regionach, w okolicy mediany łącznej sprzedaży,
- n regionach, gdzie sprzedaż była najmniejsza.

Dla każdego przypadku wygenerowany zostaje osobny wykres.
Skrypt został zbudowany na bazie pliku dra Janowskiego.

```{r}
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

 
```

## 3. Regresja liniowa

...

## 4. Regresja logistyczna

...

## 5. FARIMA - szeregi czasowe

...

## 6. Sieci neuronowe

...

## 7. SVM

...

## 8. Drzewa decyzyjne

...

## 9. PCA

...

## 10. Klasteryzacja

...
