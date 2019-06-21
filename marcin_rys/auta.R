require(ggplot2)

auta <- read.csv('dane.csv', header = T, colClasses = c("character", "numeric", "numeric", "numeric"))
auta$cena <- abs(auta$cena)
summary(auta)

ggplot(auta, aes(auta$wiek, auta$cena, colour = auta$model)) + geom_point()
ggplot(auta, aes(auta$przebieg, auta$cena, colour = auta$model)) + geom_point()


