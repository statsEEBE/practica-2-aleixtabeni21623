#Codigo para problema 2
mis_dades <- iris
mis_dades
x <- mis_dades$Petal.Length
y <- mis_dades$Sepal.Length
x
y
plot(x,y)

x_bar <- mean(x)
y_bar <- mean(y)

m <- sum((x-x_bar)*(y-y_bar))/sum((x-x_bar)^2)
b <- y_bar-m*x_bar
b
m
m*1.5+b
x_pred <- x
y_pred <- m*x_pred+b
plot (x,y)
lines(x_pred, y_pred)
#coef de determinación
Rsq <- (sum((y_pred-y_bar)^2))/(sum((y-y_bar)^2))
Rsq
# coef correlación
cor <- sqrt(Rsq)
cor
mod <- lm(y~x)
mod
summary(mod)
cor.test(x,y)

y_pred2 <- predict(mod, data.frame(x=x))
y_pred2
