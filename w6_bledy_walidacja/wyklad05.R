#
#Sekcja pierwsza
#
rm(list = ls())
library(ggplot2)
calcPoly <- function(x, fit){
  y <- fit$coefficients[1]+fit$coefficients[2]*x
  if (length(fit$coefficients) > 2){
    for (coeff in (3:length(fit$coefficients))){
      y <- y + fit$coefficients[coeff]*x^(coeff-1)
    }
  }
  return(y)
}


x = seq(-3,1.7, length.out = 11)
y = 3.5*x+2
exam1 <- data.frame(x, y)
rm("x", "y")
ggplot(data = exam1) + geom_point(aes(x = x, y = y), col = "green", size = 5)
exam2 = exam1
exam2$y[5] = 0
ggplot(data = exam2) + geom_point(aes(x = x, y = y), col = "red", size = 5)

fit11 <- lm(y ~ poly(x, 1, raw = TRUE), data=exam1)
fit12 <- lm(y ~ poly(x, 1, raw = TRUE), data=exam2)
x = seq(-3,1.7,0.01)
y1 = calcPoly(x,fit11)
y2 = calcPoly(x, fit12)
ggplot(data.frame(x, y1)) + 
  geom_line(aes(x = x, y = y1), col = "green") + 
  geom_point(data = exam1, aes(x = x, y = y), col = "green", size = 5)
ggplot(data.frame(x, y2)) + geom_line(aes(x = x, y = y2), col = "red") + 
  geom_point(data = exam2, aes(x = x, y = y), col = "red", size = 5) +
  annotate("text", x = -1.4, y = 5, label = sprintf("(M1 - M2)^2: %.3f",sum((y1-y2)^2)), color = "red", size = 10)
ggplot(data.frame(x, y1)) + 
  geom_line(aes(x = x, y = y1), col = "green") + 
  geom_line(data = data.frame(x, y2), aes(x = x, y = y2), col = "red")

fit21 <- lm(y ~ poly(x, 5, raw = TRUE), data=exam1)
fit22 <- lm(y ~ poly(x, 5, raw = TRUE), data=exam2)
x = seq(-3,1.7,0.01)
y1 = calcPoly(x,fit21)
y2 = calcPoly(x, fit22)
ggplot(data.frame(x, y1)) + 
  geom_line(aes(x = x, y = y1), col = "green") + 
  geom_point(data = exam1, aes(x = x, y = y), col = "green", size = 5)
ggplot(data.frame(x, y2)) + geom_line(aes(x = x, y = y2), col = "red") + 
  geom_point(data = exam2, aes(x = x, y = y), col = "red", size = 5) +
  annotate("text", x = -1.4, y = 5, label = sprintf("(M1 - M2)^2: %.3f",sum((y1-y2)^2)), color = "red", size = 10)
ggplot(data.frame(x, y1)) + 
  geom_line(aes(x = x, y = y1), col = "green") + 
  geom_line(data = data.frame(x, y2), aes(x = x, y = y2), col = "red")


fit31 <- lm(y ~ poly(x, 10, raw = TRUE), data=exam1)
fit32 <- lm(y ~ poly(x, 10, raw = TRUE), data=exam2)
x = seq(-3,1.7,0.01)
y1 = calcPoly(x,fit31)
y2 = calcPoly(x, fit32)
ggplot(data.frame(x, y1)) + 
  geom_line(aes(x = x, y = y1), col = "green") + 
  geom_point(data = exam1, aes(x = x, y = y), col = "green", size = 5)
ggplot(data.frame(x, y2)) + geom_line(aes(x = x, y = y2), col = "red") + 
  geom_point(data = exam2, aes(x = x, y = y), col = "red", size = 5) +
  annotate("text", x = -1.4, y = 5, label = sprintf("(M1 - M2)^2: %.3f",sum((y1-y2)^2)), color = "red", size = 10)
ggplot(data.frame(x, y1)) + 
  geom_line(aes(x = x, y = y1), col = "green") + 
  geom_line(data = data.frame(x, y2), aes(x = x, y = y2), col = "red")


#
# Sekcja 2
#

rm(list = ls())
library(ggplot2)
calcPoly <- function(x, fit){
  y <- fit$coefficients[1]+fit$coefficients[2]*x
  if (length(fit$coefficients) > 2){
    for (coeff in (3:length(fit$coefficients))){
      y <- y + fit$coefficients[coeff]*x^(coeff-1)
    }
  }
  return(y)
}

n=50
x = runif(n,-3,1.7)
y = x^2 + 3.5*x + 2 + rnorm(n,0,0.1)
exam <- data.frame(x, y)
rm("x", "y")
p <- ggplot(data = exam) + geom_point(aes(x = x, y = y), col = "green", size = 4, shape = 1)
p
fit1 <- lm(y ~ poly(x, 1, raw = TRUE), data=exam)
fit2 <- lm(y ~ poly(x, 2, raw = TRUE), data=exam)
x = seq(-3,1.7,0.01)
y1 = calcPoly(x, fit1)
y2 = calcPoly(x, fit2)
p <- p + geom_line(data = data.frame(x = x, y = y1), aes(x = x, y = y1), col = "blue")
p
p <- p + geom_line(data = data.frame(x = x, y = y2), aes(x = x, y = y2), col = "red")
p
p <- p + 
  annotate("text", x = -2, y = 8, label = sprintf("Lin R^2: %.3f",summary(fit1)$r.squared), col = "blue", size = 10) + 
  annotate("text", x = -2, y = 6, label = sprintf("Sqr R^2: %.3f",summary(fit2)$r.squared), col = "red", size = 10) 
p

#
# Sekcja 3
#

rm(list = ls())
library(ggplot2)
calcPoly <- function(x, fit){
  y <- fit$coefficients[1]+fit$coefficients[2]*x
  if (length(fit$coefficients) > 2){
    for (coeff in (3:length(fit$coefficients))){
      y <- y + fit$coefficients[coeff]*x^(coeff-1)
    }
  }
  return(y)
}

#Model
x = seq(-3,1.7,0.01)
y = x^5+3.5*x^4-2.5*x^3-12.5*x^2+3.5*x+9
reality <- data.frame(x, y)
rm("x", "y")
plot(reality$x, reality$y, type = "l", col = "green")

#Deane bez szumu
par(mfrow=c(2,3))
plot(reality$x, reality$y, col="green")
fit1 <- lm(y ~ poly(x, 1, raw = TRUE), data=reality)
ytmp = calcPoly(reality$x, fit1)
lines(reality$x, ytmp, type = "l", col="red")
mtext(sprintf("R^2: %.3f",summary(fit1)$r.squared), line=-1.4,at=-2)

plot(reality$x, reality$y, type="l", col="green")
fit1 <- lm(y ~ poly(x, 2, raw = TRUE), data=reality)
ytmp = calcPoly(reality$x, fit1)
lines(reality$x, ytmp, type = "l", col="red")
mtext(sprintf("R^2: %.3f",summary(fit1)$r.squared), line=-1.4,at=-2)

plot(reality$x, reality$y, type="l", col="green")
fit1 <- lm(y ~ poly(x, 3, raw = TRUE), data=reality)
ytmp = calcPoly(reality$x, fit1)
lines(reality$x, ytmp, type = "l", col="red")
mtext(sprintf("R^2: %.3f",summary(fit1)$r.squared), line=-1.4,at=-2)

plot(reality$x, reality$y, type="l", col="green")
fit1 <- lm(y ~ poly(x, 4, raw = TRUE), data=reality)
ytmp = calcPoly(reality$x, fit1)
lines(reality$x, ytmp, type = "l", col="red")
mtext(sprintf("R^2: %.3f",summary(fit1)$r.squared), line=-1.4,at=-2)

plot(reality$x, reality$y, type="l", col="green")
fit1 <- lm(y ~ poly(x, 5, raw = TRUE), data=reality)
ytmp = calcPoly(reality$x, fit1)
lines(reality$x, ytmp, type = "l", col="red")
mtext(sprintf("R^2: %.3f",summary(fit1)$r.squared), line=-1.4,at=-2)

plot(reality$x, reality$y, type="l", col="green")
fit1 <- lm(y ~ poly(x, 10, raw = TRUE), data=reality)
ytmp = calcPoly(reality$x, fit1)
lines(reality$x, ytmp, type = "l", col="red")
mtext(sprintf("R^2: %.3f",summary(fit1)$r.squared), line=-1.4,at=-2)

#Mały szum
par(mfrow=c(1,1))
x = runif(1000,-3,1.7)
y = x^5+3.5*x^4-2.5*x^3-12.5*x^2+3.5*x+9 + rnorm(length(x), 0, 1)
smallN <- data.frame(x, y)
rm("x", "y")
plot(smallN$x, smallN$y, col = "green")

par(mfrow=c(2,3))
plot(smallN$x, smallN$y, col="green")
fit1 <- lm(y ~ poly(x, 1, raw = TRUE), data=smallN)
ytmp = calcPoly(reality$x, fit1)
lines(reality$x, ytmp, type = "l", col="red")
mtext(sprintf("R^2 (d): %.3f",summary(fit1)$r.squared), line=-1.4,at=-2)
mtext(sprintf("R^2 (m): %.3f",1-sum((ytmp - reality$y)^2)/sum((reality$y-mean(reality$y))^2)), line=-2.5,at=-2)

plot(smallN$x, smallN$y, col="green")
fit1 <- lm(y ~ poly(x, 2, raw = TRUE), data=smallN)
ytmp = calcPoly(reality$x, fit1)
lines(reality$x, ytmp, type = "l", col="red")
mtext(sprintf("R^2 (d): %.3f",summary(fit1)$r.squared), line=-1.4,at=-2)
mtext(sprintf("R^2 (m): %.3f",1-sum((ytmp - reality$y)^2)/sum((reality$y-mean(reality$y))^2)), line=-2.5,at=-2)

plot(smallN$x, smallN$y, col="green")
fit1 <- lm(y ~ poly(x, 3, raw = TRUE), data=smallN)
ytmp = calcPoly(reality$x, fit1)
lines(reality$x, ytmp, type = "l", col="red")
mtext(sprintf("R^2 (d): %.3f",summary(fit1)$r.squared), line=-1.4,at=-2)
mtext(sprintf("R^2 (m): %.3f",1-sum((ytmp - reality$y)^2)/sum((reality$y-mean(reality$y))^2)), line=-2.5,at=-2)

plot(smallN$x, smallN$y, col="green")
fit1 <- lm(y ~ poly(x, 4, raw = TRUE), data=smallN)
ytmp = calcPoly(reality$x, fit1)
lines(reality$x, ytmp, type = "l", col="red")
mtext(sprintf("R^2 (d): %.3f",summary(fit1)$r.squared), line=-1.4,at=-2)
mtext(sprintf("R^2 (m): %.3f",1-sum((ytmp - reality$y)^2)/sum((reality$y-mean(reality$y))^2)), line=-2.5,at=-2)

plot(smallN$x, smallN$y, col="green")
fit1 <- lm(y ~ poly(x, 5, raw = TRUE), data=smallN)
ytmp = calcPoly(reality$x, fit1)
lines(reality$x, ytmp, type = "l", col="red")
mtext(sprintf("R^2 (d): %.3f",summary(fit1)$r.squared), line=-1.4,at=-2)
mtext(sprintf("R^2 (m): %.3f",1-sum((ytmp - reality$y)^2)/sum((reality$y-mean(reality$y))^2)), line=-2.5,at=-2)

plot(smallN$x, smallN$y, col="green")
fit1 <- lm(y ~ poly(x, 10, raw = TRUE), data=smallN)
ytmp = calcPoly(reality$x, fit1)
lines(reality$x, ytmp, type = "l", col="red")
mtext(sprintf("R^2 (d): %.3f",summary(fit1)$r.squared), line=-1.4,at=-2)
mtext(sprintf("R^2 (m): %.3f",1-sum((ytmp - reality$y)^2)/sum((reality$y-mean(reality$y))^2)), line=-2.5,at=-2)

#Duży szum
par(mfrow=c(1,1))
x = runif(1000,-3,1.7)
y = x^5+3.5*x^4-2.5*x^3-12.5*x^2+3.5*x+9 + rnorm(length(x), 0, 10)
largeN <- data.frame(x, y)
rm("x", "y")
plot(largeN$x, largeN$y, col = "green")

par(mfrow=c(2,3))
plot(largeN$x, largeN$y, col="green")
fit1 <- lm(y ~ poly(x, 1, raw = TRUE), data=largeN)
ytmp = calcPoly(reality$x, fit1)
lines(reality$x, ytmp, type = "l", col="red")
mtext(sprintf("R^2 (d): %.3f",summary(fit1)$r.squared), line=-1.4,at=-2)
mtext(sprintf("R^2 (m): %.3f",1-sum((ytmp - reality$y)^2)/sum((reality$y-mean(reality$y))^2)), line=-2.5,at=-2)

plot(largeN$x, largeN$y, col="green")
fit1 <- lm(y ~ poly(x, 2, raw = TRUE), data=largeN)
ytmp = calcPoly(reality$x, fit1)
lines(reality$x, ytmp, type = "l", col="red")
mtext(sprintf("R^2 (d): %.3f",summary(fit1)$r.squared), line=-1.4,at=-2)
mtext(sprintf("R^2 (m): %.3f",1-sum((ytmp - reality$y)^2)/sum((reality$y-mean(reality$y))^2)), line=-2.5,at=-2)

plot(largeN$x, largeN$y, col="green")
fit1 <- lm(y ~ poly(x, 3, raw = TRUE), data=largeN)
ytmp = calcPoly(reality$x, fit1)
lines(reality$x, ytmp, type = "l", col="red")
mtext(sprintf("R^2 (d): %.3f",summary(fit1)$r.squared), line=-1.4,at=-2)
mtext(sprintf("R^2 (m): %.3f",1-sum((ytmp - reality$y)^2)/sum((reality$y-mean(reality$y))^2)), line=-2.5,at=-2)

plot(largeN$x, largeN$y, col="green")
fit1 <- lm(y ~ poly(x, 4, raw = TRUE), data=largeN)
ytmp = calcPoly(reality$x, fit1)
lines(reality$x, ytmp, type = "l", col="red")
mtext(sprintf("R^2 (d): %.3f",summary(fit1)$r.squared), line=-1.4,at=-2)
mtext(sprintf("R^2 (m): %.3f",1-sum((ytmp - reality$y)^2)/sum((reality$y-mean(reality$y))^2)), line=-2.5,at=-2)

plot(largeN$x, largeN$y, col="green")
fit1 <- lm(y ~ poly(x, 5, raw = TRUE), data=largeN)
ytmp = calcPoly(reality$x, fit1)
lines(reality$x, ytmp, type = "l", col="red")
mtext(sprintf("R^2 (d): %.3f",summary(fit1)$r.squared), line=-1.4,at=-2)
mtext(sprintf("R^2 (m): %.3f",1-sum((ytmp - reality$y)^2)/sum((reality$y-mean(reality$y))^2)), line=-2.5,at=-2)

plot(largeN$x, largeN$y, col="green")
fit1 <- lm(y ~ poly(x, 10, raw = TRUE), data=largeN)
ytmp = calcPoly(reality$x, fit1)
lines(reality$x, ytmp, type = "l", col="red")
mtext(sprintf("R^2 (d): %.3f",summary(fit1)$r.squared), line=-1.4,at=-2)
mtext(sprintf("R^2 (m): %.3f",1-sum((ytmp - reality$y)^2)/sum((reality$y-mean(reality$y))^2)), line=-2.5,at=-2)

#Duży szum, dużo danych
par(mfrow=c(1,1))
x = runif(10000,-3,1.7)
y = x^5+3.5*x^4-2.5*x^3-12.5*x^2+3.5*x+9 + rnorm(length(x), 0, 10)
largeN <- data.frame(x, y)
rm("x", "y")
plot(largeN$x, largeN$y, col = "green")

par(mfrow=c(2,3))
plot(largeN$x, largeN$y, col="green")
fit1 <- lm(y ~ poly(x, 1, raw = TRUE), data=largeN)
ytmp = calcPoly(reality$x, fit1)
lines(reality$x, ytmp, type = "l", col="red")
mtext(sprintf("R^2 (d): %.3f",summary(fit1)$r.squared), line=-1.4,at=-2)
mtext(sprintf("R^2 (m): %.3f",1-sum((ytmp - reality$y)^2)/sum((reality$y-mean(reality$y))^2)), line=-2.5,at=-2)

plot(largeN$x, largeN$y, col="green")
fit1 <- lm(y ~ poly(x, 2, raw = TRUE), data=largeN)
ytmp = calcPoly(reality$x, fit1)
lines(reality$x, ytmp, type = "l", col="red")
mtext(sprintf("R^2 (d): %.3f",summary(fit1)$r.squared), line=-1.4,at=-2)
mtext(sprintf("R^2 (m): %.3f",1-sum((ytmp - reality$y)^2)/sum((reality$y-mean(reality$y))^2)), line=-2.5,at=-2)

plot(largeN$x, largeN$y, col="green")
fit1 <- lm(y ~ poly(x, 3, raw = TRUE), data=largeN)
ytmp = calcPoly(reality$x, fit1)
lines(reality$x, ytmp, type = "l", col="red")
mtext(sprintf("R^2 (d): %.3f",summary(fit1)$r.squared), line=-1.4,at=-2)
mtext(sprintf("R^2 (m): %.3f",1-sum((ytmp - reality$y)^2)/sum((reality$y-mean(reality$y))^2)), line=-2.5,at=-2)

plot(largeN$x, largeN$y, col="green")
fit1 <- lm(y ~ poly(x, 4, raw = TRUE), data=largeN)
ytmp = calcPoly(reality$x, fit1)
lines(reality$x, ytmp, type = "l", col="red")
mtext(sprintf("R^2 (d): %.3f",summary(fit1)$r.squared), line=-1.4,at=-2)
mtext(sprintf("R^2 (m): %.3f",1-sum((ytmp - reality$y)^2)/sum((reality$y-mean(reality$y))^2)), line=-2.5,at=-2)

plot(largeN$x, largeN$y, col="green")
fit1 <- lm(y ~ poly(x, 5, raw = TRUE), data=largeN)
ytmp = calcPoly(reality$x, fit1)
lines(reality$x, ytmp, type = "l", col="red")
mtext(sprintf("R^2 (d): %.3f",summary(fit1)$r.squared), line=-1.4,at=-2)
mtext(sprintf("R^2 (m): %.3f",1-sum((ytmp - reality$y)^2)/sum((reality$y-mean(reality$y))^2)), line=-2.5,at=-2)

plot(largeN$x, largeN$y, col="green")
fit1 <- lm(y ~ poly(x, 10, raw = TRUE), data=largeN)
ytmp = calcPoly(reality$x, fit1)
lines(reality$x, ytmp, type = "l", col="red")
mtext(sprintf("R^2 (d): %.3f",summary(fit1)$r.squared), line=-1.4,at=-2)
mtext(sprintf("R^2 (m): %.3f",1-sum((ytmp - reality$y)^2)/sum((reality$y-mean(reality$y))^2)), line=-2.5,at=-2)

#Mało danych
par(mfrow=c(1,1))
x = seq(-3,1.7, length.out = 11)
y = x^5+3.5*x^4-2.5*x^3-12.5*x^2+3.5*x+9
malo <- data.frame(x, y)
rm("x", "y")
plot(malo$x, malo$y, type = "l", col = "green")

par(mfrow=c(2,3))
plot(reality$x, reality$y, type="l", col="green")
fit1 <- lm(y ~ poly(x, 1, raw = TRUE), data=malo)
ytmp = calcPoly(reality$x, fit1)
lines(reality$x, ytmp, type = "l", col="red")
mtext(sprintf("R^2 (d): %.3f",summary(fit1)$r.squared), line=-1.4,at=-2)
mtext(sprintf("R^2 (m): %.3f",1-sum((ytmp - reality$y)^2)/sum((reality$y-mean(reality$y))^2)), line=-2.5,at=-2)

plot(reality$x, reality$y, type="l", col="green")
fit1 <- lm(y ~ poly(x, 2, raw = TRUE), data=malo)
ytmp = calcPoly(reality$x, fit1)
lines(reality$x, ytmp, type = "l", col="red")
mtext(sprintf("R^2 (d): %.3f",summary(fit1)$r.squared), line=-1.4,at=-2)
mtext(sprintf("R^2 (m): %.3f",1-sum((ytmp - reality$y)^2)/sum((reality$y-mean(reality$y))^2)), line=-2.5,at=-2)

plot(reality$x, reality$y, type="l", col="green")
fit1 <- lm(y ~ poly(x, 3, raw = TRUE), data=malo)
ytmp = calcPoly(reality$x, fit1)
lines(reality$x, ytmp, type = "l", col="red")
mtext(sprintf("R^2 (d): %.3f",summary(fit1)$r.squared), line=-1.4,at=-2)
mtext(sprintf("R^2 (m): %.3f",1-sum((ytmp - reality$y)^2)/sum((reality$y-mean(reality$y))^2)), line=-2.5,at=-2)

plot(reality$x, reality$y, type="l", col="green")
fit1 <- lm(y ~ poly(x, 4, raw = TRUE), data=malo)
ytmp = calcPoly(reality$x, fit1)
lines(reality$x, ytmp, type = "l", col="red")
mtext(sprintf("R^2 (d): %.3f",summary(fit1)$r.squared), line=-1.4,at=-2)
mtext(sprintf("R^2 (m): %.3f",1-sum((ytmp - reality$y)^2)/sum((reality$y-mean(reality$y))^2)), line=-2.5,at=-2)

plot(reality$x, reality$y, type="l", col="green")
fit1 <- lm(y ~ poly(x, 5, raw = TRUE), data=malo)
ytmp = calcPoly(reality$x, fit1)
lines(reality$x, ytmp, type = "l", col="red")
mtext(sprintf("R^2 (d): %.3f",summary(fit1)$r.squared), line=-1.4,at=-2)
mtext(sprintf("R^2 (m): %.3f",1-sum((ytmp - reality$y)^2)/sum((reality$y-mean(reality$y))^2)), line=-2.5,at=-2)

plot(reality$x, reality$y, type="l", col="green")
fit1 <- lm(y ~ poly(x, 10, raw = TRUE), data=malo)
ytmp = calcPoly(reality$x, fit1)
lines(reality$x, ytmp, type = "l", col="red")
mtext(sprintf("R^2 (d): %.3f",summary(fit1)$r.squared), line=-1.4,at=-2)
mtext(sprintf("R^2 (m): %.3f",1-sum((ytmp - reality$y)^2)/sum((reality$y-mean(reality$y))^2)), line=-2.5,at=-2)

#Mały szum
par(mfrow=c(1,1))
x = runif(12,-3,1.7)
y = x^5+3.5*x^4-2.5*x^3-12.5*x^2+3.5*x+9 + rnorm(length(x), 0, 1)
smallN <- data.frame(x, y)
rm("x", "y")
plot(smallN$x, smallN$y, col = "green")

par(mfrow=c(2,3))
plot(smallN$x, smallN$y, col="green")
fit1 <- lm(y ~ poly(x, 1, raw = TRUE), data=smallN)
ytmp = calcPoly(reality$x, fit1)
lines(reality$x, ytmp, type = "l", col="red")
mtext(sprintf("R^2 (d): %.3f",summary(fit1)$r.squared), line=-1.4,at=-2)
mtext(sprintf("R^2 (m): %.3f",1-sum((ytmp - reality$y)^2)/sum((reality$y-mean(reality$y))^2)), line=-2.5,at=-2)

plot(smallN$x, smallN$y, col="green")
fit1 <- lm(y ~ poly(x, 2, raw = TRUE), data=smallN)
ytmp = calcPoly(reality$x, fit1)
lines(reality$x, ytmp, type = "l", col="red")
mtext(sprintf("R^2 (d): %.3f",summary(fit1)$r.squared), line=-1.4,at=-2)
mtext(sprintf("R^2 (m): %.3f",1-sum((ytmp - reality$y)^2)/sum((reality$y-mean(reality$y))^2)), line=-2.5,at=-2)

plot(smallN$x, smallN$y, col="green")
fit1 <- lm(y ~ poly(x, 3, raw = TRUE), data=smallN)
ytmp = calcPoly(reality$x, fit1)
lines(reality$x, ytmp, type = "l", col="red")
mtext(sprintf("R^2 (d): %.3f",summary(fit1)$r.squared), line=-1.4,at=-2)
mtext(sprintf("R^2 (m): %.3f",1-sum((ytmp - reality$y)^2)/sum((reality$y-mean(reality$y))^2)), line=-2.5,at=-2)

plot(smallN$x, smallN$y, col="green")
fit1 <- lm(y ~ poly(x, 4, raw = TRUE), data=smallN)
ytmp = calcPoly(reality$x, fit1)
lines(reality$x, ytmp, type = "l", col="red")
mtext(sprintf("R^2 (d): %.3f",summary(fit1)$r.squared), line=-1.4,at=-2)
mtext(sprintf("R^2 (m): %.3f",1-sum((ytmp - reality$y)^2)/sum((reality$y-mean(reality$y))^2)), line=-2.5,at=-2)

plot(smallN$x, smallN$y, col="green")
fit1 <- lm(y ~ poly(x, 5, raw = TRUE), data=smallN)
ytmp = calcPoly(reality$x, fit1)
lines(reality$x, ytmp, type = "l", col="red")
mtext(sprintf("R^2 (d): %.3f",summary(fit1)$r.squared), line=-1.4,at=-2)
mtext(sprintf("R^2 (m): %.3f",1-sum((ytmp - reality$y)^2)/sum((reality$y-mean(reality$y))^2)), line=-2.5,at=-2)

plot(smallN$x, smallN$y, col="green")
fit1 <- lm(y ~ poly(x, 10, raw = TRUE), data=smallN)
ytmp = calcPoly(reality$x, fit1)
lines(reality$x, ytmp, type = "l", col="red")
mtext(sprintf("R^2 (d): %.3f",summary(fit1)$r.squared), line=-1.4,at=-2)
mtext(sprintf("R^2 (m): %.3f",1-sum((ytmp - reality$y)^2)/sum((reality$y-mean(reality$y))^2)), line=-2.5,at=-2)

#Duży szum
par(mfrow=c(1,1))
x = runif(12,-3,1.7)
y = x^5+3.5*x^4-2.5*x^3-12.5*x^2+3.5*x+9 + rnorm(length(x), 0, 10)
largeN <- data.frame(x, y)
rm("x", "y")
plot(largeN$x, largeN$y, col = "green")

par(mfrow=c(2,3))
plot(largeN$x, largeN$y, col="green")
fit1 <- lm(y ~ poly(x, 1, raw = TRUE), data=largeN)
ytmp = calcPoly(reality$x, fit1)
lines(reality$x, ytmp, type = "l", col="red")
mtext(sprintf("R^2 (d): %.3f",summary(fit1)$r.squared), line=-1.4,at=-2)
mtext(sprintf("R^2 (m): %.3f",1-sum((ytmp - reality$y)^2)/sum((reality$y-mean(reality$y))^2)), line=-2.5,at=-2)

plot(largeN$x, largeN$y, col="green")
fit1 <- lm(y ~ poly(x, 2, raw = TRUE), data=largeN)
ytmp = calcPoly(reality$x, fit1)
lines(reality$x, ytmp, type = "l", col="red")
mtext(sprintf("R^2 (d): %.3f",summary(fit1)$r.squared), line=-1.4,at=-2)
mtext(sprintf("R^2 (m): %.3f",1-sum((ytmp - reality$y)^2)/sum((reality$y-mean(reality$y))^2)), line=-2.5,at=-2)

plot(largeN$x, largeN$y, col="green")
fit1 <- lm(y ~ poly(x, 3, raw = TRUE), data=largeN)
ytmp = calcPoly(reality$x, fit1)
lines(reality$x, ytmp, type = "l", col="red")
mtext(sprintf("R^2 (d): %.3f",summary(fit1)$r.squared), line=-1.4,at=-2)
mtext(sprintf("R^2 (m): %.3f",1-sum((ytmp - reality$y)^2)/sum((reality$y-mean(reality$y))^2)), line=-2.5,at=-2)

plot(largeN$x, largeN$y, col="green")
fit1 <- lm(y ~ poly(x, 4, raw = TRUE), data=largeN)
ytmp = calcPoly(reality$x, fit1)
lines(reality$x, ytmp, type = "l", col="red")
mtext(sprintf("R^2 (d): %.3f",summary(fit1)$r.squared), line=-1.4,at=-2)
mtext(sprintf("R^2 (m): %.3f",1-sum((ytmp - reality$y)^2)/sum((reality$y-mean(reality$y))^2)), line=-2.5,at=-2)

plot(largeN$x, largeN$y, col="green")
fit1 <- lm(y ~ poly(x, 5, raw = TRUE), data=largeN)
ytmp = calcPoly(reality$x, fit1)
lines(reality$x, ytmp, type = "l", col="red")
mtext(sprintf("R^2 (d): %.3f",summary(fit1)$r.squared), line=-1.4,at=-2)
mtext(sprintf("R^2 (m): %.3f",1-sum((ytmp - reality$y)^2)/sum((reality$y-mean(reality$y))^2)), line=-2.5,at=-2)

plot(largeN$x, largeN$y, col="green")
fit1 <- lm(y ~ poly(x, 10, raw = TRUE), data=largeN)
ytmp = calcPoly(reality$x, fit1)
lines(reality$x, ytmp, type = "l", col="red")
mtext(sprintf("R^2 (d): %.3f",summary(fit1)$r.squared), line=-1.4,at=-2)
mtext(sprintf("R^2 (m): %.3f",1-sum((ytmp - reality$y)^2)/sum((reality$y-mean(reality$y))^2)), line=-2.5,at=-2)

#
#funkcja liniowa
#

#Model
par(mfrow=c(1,1))
x = seq(-3,1.7,0.01)
y = 3.5*x+9
reality <- data.frame(x, y)
rm("x", "y")
plot(reality$x, reality$y, type = "l", col = "green")

#Deane bez szumu
par(mfrow=c(2,3))
plot(reality$x, reality$y, type="l", col="green")
fit1 <- lm(y ~ poly(x, 1, raw = TRUE), data=reality)
ytmp = calcPoly(reality$x, fit1)
lines(reality$x, ytmp, type = "l", col="red")
mtext(sprintf("R^2: %.3f",summary(fit1)$r.squared), line=-1.4,at=-2)

plot(reality$x, reality$y, type="l", col="green")
fit1 <- lm(y ~ poly(x, 2, raw = TRUE), data=reality)
ytmp = calcPoly(reality$x, fit1)
lines(reality$x, ytmp, type = "l", col="red")
mtext(sprintf("R^2: %.3f",summary(fit1)$r.squared), line=-1.4,at=-2)

plot(reality$x, reality$y, type="l", col="green")
fit1 <- lm(y ~ poly(x, 3, raw = TRUE), data=reality)
ytmp = calcPoly(reality$x, fit1)
lines(reality$x, ytmp, type = "l", col="red")
mtext(sprintf("R^2: %.3f",summary(fit1)$r.squared), line=-1.4,at=-2)

plot(reality$x, reality$y, type="l", col="green")
fit1 <- lm(y ~ poly(x, 4, raw = TRUE), data=reality)
ytmp = calcPoly(reality$x, fit1)
lines(reality$x, ytmp, type = "l", col="red")
mtext(sprintf("R^2: %.3f",summary(fit1)$r.squared), line=-1.4,at=-2)

plot(reality$x, reality$y, type="l", col="green")
fit1 <- lm(y ~ poly(x, 5, raw = TRUE), data=reality)
ytmp = calcPoly(reality$x, fit1)
lines(reality$x, ytmp, type = "l", col="red")
mtext(sprintf("R^2: %.3f",summary(fit1)$r.squared), line=-1.4,at=-2)

plot(reality$x, reality$y, type="l", col="green")
fit1 <- lm(y ~ poly(x, 10, raw = TRUE), data=reality)
ytmp = calcPoly(reality$x, fit1)
lines(reality$x, ytmp, type = "l", col="red")
mtext(sprintf("R^2: %.3f",summary(fit1)$r.squared), line=-1.4,at=-2)

#Mały szum
par(mfrow=c(1,1))
x = runif(1000,-3,1.7)
y = 3.5*x+9 + rnorm(length(x), 0, 1)
smallN <- data.frame(x, y)
rm("x", "y")
plot(smallN$x, smallN$y, col = "green")

par(mfrow=c(2,3))
plot(smallN$x, smallN$y, col="green")
fit1 <- lm(y ~ poly(x, 1, raw = TRUE), data=smallN)
ytmp = calcPoly(reality$x, fit1)
lines(reality$x, ytmp, type = "l", col="red")
mtext(sprintf("R^2 (d): %.3f",summary(fit1)$r.squared), line=-1.4,at=-2)
mtext(sprintf("R^2 (m): %.3f",1-sum((ytmp - reality$y)^2)/sum((reality$y-mean(reality$y))^2)), line=-2.5,at=-2)

plot(smallN$x, smallN$y, col="green")
fit1 <- lm(y ~ poly(x, 2, raw = TRUE), data=smallN)
ytmp = calcPoly(reality$x, fit1)
lines(reality$x, ytmp, type = "l", col="red")
mtext(sprintf("R^2 (d): %.3f",summary(fit1)$r.squared), line=-1.4,at=-2)
mtext(sprintf("R^2 (m): %.3f",1-sum((ytmp - reality$y)^2)/sum((reality$y-mean(reality$y))^2)), line=-2.5,at=-2)

plot(smallN$x, smallN$y, col="green")
fit1 <- lm(y ~ poly(x, 3, raw = TRUE), data=smallN)
ytmp = calcPoly(reality$x, fit1)
lines(reality$x, ytmp, type = "l", col="red")
mtext(sprintf("R^2 (d): %.3f",summary(fit1)$r.squared), line=-1.4,at=-2)
mtext(sprintf("R^2 (m): %.3f",1-sum((ytmp - reality$y)^2)/sum((reality$y-mean(reality$y))^2)), line=-2.5,at=-2)

plot(smallN$x, smallN$y, col="green")
fit1 <- lm(y ~ poly(x, 4, raw = TRUE), data=smallN)
ytmp = calcPoly(reality$x, fit1)
lines(reality$x, ytmp, type = "l", col="red")
mtext(sprintf("R^2 (d): %.3f",summary(fit1)$r.squared), line=-1.4,at=-2)
mtext(sprintf("R^2 (m): %.3f",1-sum((ytmp - reality$y)^2)/sum((reality$y-mean(reality$y))^2)), line=-2.5,at=-2)

plot(smallN$x, smallN$y, col="green")
fit1 <- lm(y ~ poly(x, 5, raw = TRUE), data=smallN)
ytmp = calcPoly(reality$x, fit1)
lines(reality$x, ytmp, type = "l", col="red")
mtext(sprintf("R^2 (d): %.3f",summary(fit1)$r.squared), line=-1.4,at=-2)
mtext(sprintf("R^2 (m): %.3f",1-sum((ytmp - reality$y)^2)/sum((reality$y-mean(reality$y))^2)), line=-2.5,at=-2)

plot(smallN$x, smallN$y, col="green")
fit1 <- lm(y ~ poly(x, 10, raw = TRUE), data=smallN)
ytmp = calcPoly(reality$x, fit1)
lines(reality$x, ytmp, type = "l", col="red")
mtext(sprintf("R^2 (d): %.3f",summary(fit1)$r.squared), line=-1.4,at=-2)
mtext(sprintf("R^2 (m): %.3f",1-sum((ytmp - reality$y)^2)/sum((reality$y-mean(reality$y))^2)), line=-2.5,at=-2)

#Duży szum
par(mfrow=c(1,1))
x = runif(1000,-3,1.7)
y = 3.5*x+9 + rnorm(length(x), 0, 10)
largeN <- data.frame(x, y)
rm("x", "y")
plot(largeN$x, largeN$y, col = "green")

par(mfrow=c(2,3))
plot(largeN$x, largeN$y, col="green")
fit1 <- lm(y ~ poly(x, 1, raw = TRUE), data=largeN)
ytmp = calcPoly(reality$x, fit1)
lines(reality$x, ytmp, type = "l", col="red")
mtext(sprintf("R^2 (d): %.3f",summary(fit1)$r.squared), line=-1.4,at=-2)
mtext(sprintf("R^2 (m): %.3f",1-sum((ytmp - reality$y)^2)/sum((reality$y-mean(reality$y))^2)), line=-2.5,at=-2)

plot(largeN$x, largeN$y, col="green")
fit1 <- lm(y ~ poly(x, 2, raw = TRUE), data=largeN)
ytmp = calcPoly(reality$x, fit1)
lines(reality$x, ytmp, type = "l", col="red")
mtext(sprintf("R^2 (d): %.3f",summary(fit1)$r.squared), line=-1.4,at=-2)
mtext(sprintf("R^2 (m): %.3f",1-sum((ytmp - reality$y)^2)/sum((reality$y-mean(reality$y))^2)), line=-2.5,at=-2)

plot(largeN$x, largeN$y, col="green")
fit1 <- lm(y ~ poly(x, 3, raw = TRUE), data=largeN)
ytmp = calcPoly(reality$x, fit1)
lines(reality$x, ytmp, type = "l", col="red")
mtext(sprintf("R^2 (d): %.3f",summary(fit1)$r.squared), line=-1.4,at=-2)
mtext(sprintf("R^2 (m): %.3f",1-sum((ytmp - reality$y)^2)/sum((reality$y-mean(reality$y))^2)), line=-2.5,at=-2)

plot(largeN$x, largeN$y, col="green")
fit1 <- lm(y ~ poly(x, 4, raw = TRUE), data=largeN)
ytmp = calcPoly(reality$x, fit1)
lines(reality$x, ytmp, type = "l", col="red")
mtext(sprintf("R^2 (d): %.3f",summary(fit1)$r.squared), line=-1.4,at=-2)
mtext(sprintf("R^2 (m): %.3f",1-sum((ytmp - reality$y)^2)/sum((reality$y-mean(reality$y))^2)), line=-2.5,at=-2)

plot(largeN$x, largeN$y, col="green")
fit1 <- lm(y ~ poly(x, 5, raw = TRUE), data=largeN)
ytmp = calcPoly(reality$x, fit1)
lines(reality$x, ytmp, type = "l", col="red")
mtext(sprintf("R^2 (d): %.3f",summary(fit1)$r.squared), line=-1.4,at=-2)
mtext(sprintf("R^2 (m): %.3f",1-sum((ytmp - reality$y)^2)/sum((reality$y-mean(reality$y))^2)), line=-2.5,at=-2)

plot(largeN$x, largeN$y, col="green")
fit1 <- lm(y ~ poly(x, 10, raw = TRUE), data=largeN)
ytmp = calcPoly(reality$x, fit1)
lines(reality$x, ytmp, type = "l", col="red")
mtext(sprintf("R^2 (d): %.3f",summary(fit1)$r.squared), line=-1.4,at=-2)
mtext(sprintf("R^2 (m): %.3f",1-sum((ytmp - reality$y)^2)/sum((reality$y-mean(reality$y))^2)), line=-2.5,at=-2)

#Duży szum, dużo danych
par(mfrow=c(1,1))
x = runif(10000,-3,1.7)
y = 3.5*x+9 + rnorm(length(x), 0, 10)
largeN <- data.frame(x, y)
rm("x", "y")
plot(largeN$x, largeN$y, col = "green")

par(mfrow=c(2,3))
plot(largeN$x, largeN$y, col="green")
fit1 <- lm(y ~ poly(x, 1, raw = TRUE), data=largeN)
ytmp = calcPoly(reality$x, fit1)
lines(reality$x, ytmp, type = "l", col="red")
mtext(sprintf("R^2 (d): %.3f",summary(fit1)$r.squared), line=-1.4,at=-2)
mtext(sprintf("R^2 (m): %.3f",1-sum((ytmp - reality$y)^2)/sum((reality$y-mean(reality$y))^2)), line=-2.5,at=-2)

plot(largeN$x, largeN$y, col="green")
fit1 <- lm(y ~ poly(x, 2, raw = TRUE), data=largeN)
ytmp = calcPoly(reality$x, fit1)
lines(reality$x, ytmp, type = "l", col="red")
mtext(sprintf("R^2 (d): %.3f",summary(fit1)$r.squared), line=-1.4,at=-2)
mtext(sprintf("R^2 (m): %.3f",1-sum((ytmp - reality$y)^2)/sum((reality$y-mean(reality$y))^2)), line=-2.5,at=-2)

plot(largeN$x, largeN$y, col="green")
fit1 <- lm(y ~ poly(x, 3, raw = TRUE), data=largeN)
ytmp = calcPoly(reality$x, fit1)
lines(reality$x, ytmp, type = "l", col="red")
mtext(sprintf("R^2 (d): %.3f",summary(fit1)$r.squared), line=-1.4,at=-2)
mtext(sprintf("R^2 (m): %.3f",1-sum((ytmp - reality$y)^2)/sum((reality$y-mean(reality$y))^2)), line=-2.5,at=-2)

plot(largeN$x, largeN$y, col="green")
fit1 <- lm(y ~ poly(x, 4, raw = TRUE), data=largeN)
ytmp = calcPoly(reality$x, fit1)
lines(reality$x, ytmp, type = "l", col="red")
mtext(sprintf("R^2 (d): %.3f",summary(fit1)$r.squared), line=-1.4,at=-2)
mtext(sprintf("R^2 (m): %.3f",1-sum((ytmp - reality$y)^2)/sum((reality$y-mean(reality$y))^2)), line=-2.5,at=-2)

plot(largeN$x, largeN$y, col="green")
fit1 <- lm(y ~ poly(x, 5, raw = TRUE), data=largeN)
ytmp = calcPoly(reality$x, fit1)
lines(reality$x, ytmp, type = "l", col="red")
mtext(sprintf("R^2 (d): %.3f",summary(fit1)$r.squared), line=-1.4,at=-2)
mtext(sprintf("R^2 (m): %.3f",1-sum((ytmp - reality$y)^2)/sum((reality$y-mean(reality$y))^2)), line=-2.5,at=-2)

plot(largeN$x, largeN$y, col="green")
fit1 <- lm(y ~ poly(x, 10, raw = TRUE), data=largeN)
ytmp = calcPoly(reality$x, fit1)
lines(reality$x, ytmp, type = "l", col="red")
mtext(sprintf("R^2 (d): %.3f",summary(fit1)$r.squared), line=-1.4,at=-2)
mtext(sprintf("R^2 (m): %.3f",1-sum((ytmp - reality$y)^2)/sum((reality$y-mean(reality$y))^2)), line=-2.5,at=-2)

#Mało danych
par(mfrow=c(1,1))
x = seq(-3,1.7, length.out = 11)
y = 3.5*x+9
malo <- data.frame(x, y)
rm("x", "y")
plot(malo$x, malo$y, type = "l", col = "green")

par(mfrow=c(2,3))
plot(reality$x, reality$y, type="l", col="green")
fit1 <- lm(y ~ poly(x, 1, raw = TRUE), data=malo)
ytmp = calcPoly(reality$x, fit1)
lines(reality$x, ytmp, type = "l", col="red")
mtext(sprintf("R^2 (d): %.3f",summary(fit1)$r.squared), line=-1.4,at=-2)
mtext(sprintf("R^2 (m): %.3f",1-sum((ytmp - reality$y)^2)/sum((reality$y-mean(reality$y))^2)), line=-2.5,at=-2)

plot(reality$x, reality$y, type="l", col="green")
fit1 <- lm(y ~ poly(x, 2, raw = TRUE), data=malo)
ytmp = calcPoly(reality$x, fit1)
lines(reality$x, ytmp, type = "l", col="red")
mtext(sprintf("R^2 (d): %.3f",summary(fit1)$r.squared), line=-1.4,at=-2)
mtext(sprintf("R^2 (m): %.3f",1-sum((ytmp - reality$y)^2)/sum((reality$y-mean(reality$y))^2)), line=-2.5,at=-2)

plot(reality$x, reality$y, type="l", col="green")
fit1 <- lm(y ~ poly(x, 3, raw = TRUE), data=malo)
ytmp = calcPoly(reality$x, fit1)
lines(reality$x, ytmp, type = "l", col="red")
mtext(sprintf("R^2 (d): %.3f",summary(fit1)$r.squared), line=-1.4,at=-2)
mtext(sprintf("R^2 (m): %.3f",1-sum((ytmp - reality$y)^2)/sum((reality$y-mean(reality$y))^2)), line=-2.5,at=-2)

plot(reality$x, reality$y, type="l", col="green")
fit1 <- lm(y ~ poly(x, 4, raw = TRUE), data=malo)
ytmp = calcPoly(reality$x, fit1)
lines(reality$x, ytmp, type = "l", col="red")
mtext(sprintf("R^2 (d): %.3f",summary(fit1)$r.squared), line=-1.4,at=-2)
mtext(sprintf("R^2 (m): %.3f",1-sum((ytmp - reality$y)^2)/sum((reality$y-mean(reality$y))^2)), line=-2.5,at=-2)

plot(reality$x, reality$y, type="l", col="green")
fit1 <- lm(y ~ poly(x, 5, raw = TRUE), data=malo)
ytmp = calcPoly(reality$x, fit1)
lines(reality$x, ytmp, type = "l", col="red")
mtext(sprintf("R^2 (d): %.3f",summary(fit1)$r.squared), line=-1.4,at=-2)
mtext(sprintf("R^2 (m): %.3f",1-sum((ytmp - reality$y)^2)/sum((reality$y-mean(reality$y))^2)), line=-2.5,at=-2)

plot(reality$x, reality$y, type="l", col="green")
fit1 <- lm(y ~ poly(x, 10, raw = TRUE), data=malo)
ytmp = calcPoly(reality$x, fit1)
lines(reality$x, ytmp, type = "l", col="red")
mtext(sprintf("R^2 (d): %.3f",summary(fit1)$r.squared), line=-1.4,at=-2)
mtext(sprintf("R^2 (m): %.3f",1-sum((ytmp - reality$y)^2)/sum((reality$y-mean(reality$y))^2)), line=-2.5,at=-2)

#Mały szum
par(mfrow=c(1,1))
x = runif(12,-3,1.7)
y = 3.5*x+9 + rnorm(length(x), 0, 1)
smallN <- data.frame(x, y)
rm("x", "y")
plot(smallN$x, smallN$y, col = "green")

par(mfrow=c(2,3))
plot(smallN$x, smallN$y, col="green")
fit1 <- lm(y ~ poly(x, 1, raw = TRUE), data=smallN)
ytmp = calcPoly(reality$x, fit1)
lines(reality$x, ytmp, type = "l", col="red")
mtext(sprintf("R^2 (d): %.3f",summary(fit1)$r.squared), line=-1.4,at=-2)
mtext(sprintf("R^2 (m): %.3f",1-sum((ytmp - reality$y)^2)/sum((reality$y-mean(reality$y))^2)), line=-2.5,at=-2)

plot(smallN$x, smallN$y, col="green")
fit1 <- lm(y ~ poly(x, 2, raw = TRUE), data=smallN)
ytmp = calcPoly(reality$x, fit1)
lines(reality$x, ytmp, type = "l", col="red")
mtext(sprintf("R^2 (d): %.3f",summary(fit1)$r.squared), line=-1.4,at=-2)
mtext(sprintf("R^2 (m): %.3f",1-sum((ytmp - reality$y)^2)/sum((reality$y-mean(reality$y))^2)), line=-2.5,at=-2)

plot(smallN$x, smallN$y, col="green")
fit1 <- lm(y ~ poly(x, 3, raw = TRUE), data=smallN)
ytmp = calcPoly(reality$x, fit1)
lines(reality$x, ytmp, type = "l", col="red")
mtext(sprintf("R^2 (d): %.3f",summary(fit1)$r.squared), line=-1.4,at=-2)
mtext(sprintf("R^2 (m): %.3f",1-sum((ytmp - reality$y)^2)/sum((reality$y-mean(reality$y))^2)), line=-2.5,at=-2)

plot(smallN$x, smallN$y, col="green")
fit1 <- lm(y ~ poly(x, 4, raw = TRUE), data=smallN)
ytmp = calcPoly(reality$x, fit1)
lines(reality$x, ytmp, type = "l", col="red")
mtext(sprintf("R^2 (d): %.3f",summary(fit1)$r.squared), line=-1.4,at=-2)
mtext(sprintf("R^2 (m): %.3f",1-sum((ytmp - reality$y)^2)/sum((reality$y-mean(reality$y))^2)), line=-2.5,at=-2)

plot(smallN$x, smallN$y, col="green")
fit1 <- lm(y ~ poly(x, 5, raw = TRUE), data=smallN)
ytmp = calcPoly(reality$x, fit1)
lines(reality$x, ytmp, type = "l", col="red")
mtext(sprintf("R^2 (d): %.3f",summary(fit1)$r.squared), line=-1.4,at=-2)
mtext(sprintf("R^2 (m): %.3f",1-sum((ytmp - reality$y)^2)/sum((reality$y-mean(reality$y))^2)), line=-2.5,at=-2)

plot(smallN$x, smallN$y, col="green")
fit1 <- lm(y ~ poly(x, 10, raw = TRUE), data=smallN)
ytmp = calcPoly(reality$x, fit1)
lines(reality$x, ytmp, type = "l", col="red")
mtext(sprintf("R^2 (d): %.3f",summary(fit1)$r.squared), line=-1.4,at=-2)
mtext(sprintf("R^2 (m): %.3f",1-sum((ytmp - reality$y)^2)/sum((reality$y-mean(reality$y))^2)), line=-2.5,at=-2)

#Duży szum
par(mfrow=c(1,1))
x = runif(12,-3,1.7)
y = 3.5*x+9 + rnorm(length(x), 0, 10)
largeN <- data.frame(x, y)
rm("x", "y")
plot(largeN$x, largeN$y, col = "green")

par(mfrow=c(2,3))
plot(largeN$x, largeN$y, col="green")
fit1 <- lm(y ~ poly(x, 1, raw = TRUE), data=largeN)
ytmp = calcPoly(reality$x, fit1)
lines(reality$x, ytmp, type = "l", col="red")
mtext(sprintf("R^2 (d): %.3f",summary(fit1)$r.squared), line=-1.4,at=-2)
mtext(sprintf("R^2 (m): %.3f",1-sum((ytmp - reality$y)^2)/sum((reality$y-mean(reality$y))^2)), line=-2.5,at=-2)

plot(largeN$x, largeN$y, col="green")
fit1 <- lm(y ~ poly(x, 2, raw = TRUE), data=largeN)
ytmp = calcPoly(reality$x, fit1)
lines(reality$x, ytmp, type = "l", col="red")
mtext(sprintf("R^2 (d): %.3f",summary(fit1)$r.squared), line=-1.4,at=-2)
mtext(sprintf("R^2 (m): %.3f",1-sum((ytmp - reality$y)^2)/sum((reality$y-mean(reality$y))^2)), line=-2.5,at=-2)

plot(largeN$x, largeN$y, col="green")
fit1 <- lm(y ~ poly(x, 3, raw = TRUE), data=largeN)
ytmp = calcPoly(reality$x, fit1)
lines(reality$x, ytmp, type = "l", col="red")
mtext(sprintf("R^2 (d): %.3f",summary(fit1)$r.squared), line=-1.4,at=-2)
mtext(sprintf("R^2 (m): %.3f",1-sum((ytmp - reality$y)^2)/sum((reality$y-mean(reality$y))^2)), line=-2.5,at=-2)

plot(largeN$x, largeN$y, col="green")
fit1 <- lm(y ~ poly(x, 4, raw = TRUE), data=largeN)
ytmp = calcPoly(reality$x, fit1)
lines(reality$x, ytmp, type = "l", col="red")
mtext(sprintf("R^2 (d): %.3f",summary(fit1)$r.squared), line=-1.4,at=-2)
mtext(sprintf("R^2 (m): %.3f",1-sum((ytmp - reality$y)^2)/sum((reality$y-mean(reality$y))^2)), line=-2.5,at=-2)

plot(largeN$x, largeN$y, col="green")
fit1 <- lm(y ~ poly(x, 5, raw = TRUE), data=largeN)
ytmp = calcPoly(reality$x, fit1)
lines(reality$x, ytmp, type = "l", col="red")
mtext(sprintf("R^2 (d): %.3f",summary(fit1)$r.squared), line=-1.4,at=-2)
mtext(sprintf("R^2 (m): %.3f",1-sum((ytmp - reality$y)^2)/sum((reality$y-mean(reality$y))^2)), line=-2.5,at=-2)

plot(largeN$x, largeN$y, col="green")
fit1 <- lm(y ~ poly(x, 10, raw = TRUE), data=largeN)
ytmp = calcPoly(reality$x, fit1)
lines(reality$x, ytmp, type = "l", col="red")
mtext(sprintf("R^2 (d): %.3f",summary(fit1)$r.squared), line=-1.4,at=-2)
mtext(sprintf("R^2 (m): %.3f",1-sum((ytmp - reality$y)^2)/sum((reality$y-mean(reality$y))^2)), line=-2.5,at=-2)


#
# Sekcja 4
#


rm(list=ls())
par(mfrow=c(1,1))
calcPoly <- function(x, fit){
  y <- fit$coefficients[1]+fit$coefficients[2]*x
  if (length(fit$coefficients) > 2){
    for (coeff in (3:length(fit$coefficients))){
      y <- y + fit$coefficients[coeff]*x^(coeff-1)
    }
  }
  return(y)
}

k=3
n=k*10
x = runif(n,-1,2)
y = x^3 + 3*x^2 + rnorm(n,0,0.2)
input = data.frame(x = x, y = y)
rm("x", "y")

test = sample(length(input$y))
ErrorPlot = 1:12
for (deg in 1:length(ErrorPlot)){
  plot(input$x, input$y)
  title(sprintf("Wielomian stopnia %d", deg))
  MSE = 0;
  for (i in 1:((n/k)-1)){
    fit = lm(y ~ poly(x,deg, raw = TRUE), data = input[-test[(i*k+1):((i+1)*k)],])
    MSE = MSE + sum((input$y[test[(i*k+1):((i+1)*k)]] - calcPoly(input$x[test[(i*k+1):((i+1)*k)]], fit))^2)
    
  }
  ErrorPlot[deg] = MSE
  lines(seq(-1,2,0.1), calcPoly(seq(-1,2,0.1), fit), col = "green")  
  
  cat ("Press [enter] to continue")
  line <- readline()
}

plot(ErrorPlot, type = "l")


#
# Sekcja 5
#


rm(list=ls())
par(mfrow=c(1,1))
calcPoly <- function(x, fit){
  y <- fit$coefficients[1]+fit$coefficients[2]*x
  if (length(fit$coefficients) > 2){
    for (coeff in (3:length(fit$coefficients))){
      y <- y + fit$coefficients[coeff]*x^(coeff-1)
    }
  }
  return(y)
}
k=5
n=k*10
x = runif(n,-1,2)
y = x^3 + 3*x^2 + rnorm(n,0,0.1)
input = data.frame(x = x, y = y)
rm("x", "y")

test = sample(length(input$y))
ErrorPlotLimit = 1:10
ErrorPlot = ErrorPlotLimit
for (deg in 1:length(ErrorPlot)){
  plot(input$x, input$y, ylim = c(-5,14))
  title(sprintf("Wielomian stopnia %d", deg))
  MSE = 0
  for (i in 1:((n/k)-1)){
    fit = lm(y ~ poly(x,deg, raw = TRUE), data = input[-c((1:n)[input$x<0], test[(i*k+1):((i+1)*k)]),])
    MSE = MSE + sum((input$y[unique(c((1:n)[input$x<0], test[(i*k+1):((i+1)*k)]))] - calcPoly(input$x[unique(c((1:n)[input$x<0], test[(i*k+1):((i+1)*k)]))], fit))^2)
  }
  ErrorPlotLimit[deg] = MSE
  lines(seq(-1,2,0.1), calcPoly(seq(-1,2,0.1), fit), col = "red")

  MSE = 0
  for (i in 1:((n/k)-1)){
    fit = lm(y ~ poly(x,deg, raw = TRUE), data = input[-test[(i*k+1):((i+1)*k)],])
    MSE = MSE + sum((input$y[test[(i*k+1):((i+1)*k)]] - calcPoly(input$x[test[(i*k+1):((i+1)*k)]], fit))^2)
  }
  ErrorPlot[deg] = MSE
  lines(seq(-1,2,0.1), calcPoly(seq(-1,2,0.1), fit), col = "green")  
  
  cat ("Press [enter] to continue")
  line <- readline()
}

 
plot(1:length(ErrorPlot), ErrorPlot, type="l", col = "green", ylim = c(0, 2000))
lines(1:length(ErrorPlot), ErrorPlotLimit, type="l", col = "red")












