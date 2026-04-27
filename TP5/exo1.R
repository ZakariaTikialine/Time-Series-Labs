set.seed(123)
n <- 50
sigma <- 2

par(mfrow = c(3,2))

X1 <- arima.sim(model = list(ar = 0.7), n = n, sd = sigma)
plot(X1, type="l", main="Model 1: AR(1)")

X2 <- arima.sim(model = list(ma = c(-0.3, 0.4)), n = n, sd = sigma)
plot(X2, type="l", main="Model 2: MA(2)")

X3 <- arima.sim(model = list(ar = 0.6, ma = -0.2), n = n, sd = sigma)
mu <- -2 / (1 - 0.6)
X3 <- X3 + mu
plot(X3, type="l", main="Model 3: ARMA(1,1) + mean")

base4 <- arima.sim(model = list(ma = 0.9), n = n, sd = sigma)
trend4 <- 1 + 2 * (1:n)
X4 <- base4 + trend4
plot(X4, type="l", main="Model 4: Trend + MA(1)")

base5 <- arima.sim(model = list(ar = 0.8), n = n, sd = sigma)
trend5 <- 0.5 + 0.5 * (1:n)
s <- c(1.5, 2.6, 0.4, -1, -3.5)
season5 <- rep(s, length.out = n)
X5 <- base5 + trend5 + season5
plot(X5, type="l", main="Model 5: AR + Trend + Seasonality")