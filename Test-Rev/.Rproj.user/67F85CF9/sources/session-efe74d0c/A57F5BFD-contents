lissage.exponentiel.simple <- function(serie, a) {
  n <- length(serie)
  lissage <- numeric(n)
  
  # Initialisation
  lissage[1] <- serie[1]
  
  # Formule de mise à jour : X̂_t = (1-α)X_t + α·X̂_{t-1}
  for (t in 2:n) {
    lissage[t] <- (1 - a) * serie[t] + a * lissage[t - 1]
  }
  
  return(ts(lissage))
}

lissage.exponentiel.double <- function(serie, a) {
  n <- length(serie)
  
  S1 <- numeric(n)   # lissage simple
  S2 <- numeric(n)   # lissage double
  a_hat <- numeric(n)
  b_hat <- numeric(n)
  x_prediction <- numeric(n)
  
  # Initialisation
  S1[1] <- serie[1]
  S2[1] <- serie[1]
  
  # Calcul des lissages
  for (t in 2:n) {
    S1[t] <- (1 - a) * serie[t] + a * S1[t - 1]
    S2[t] <- (1 - a) * S1[t]   + a * S2[t - 1]
  }
  
  # Paramètres â et b̂
  for (t in 1:n) {
    a_hat[t] <- 2 * S1[t] - S2[t]
    b_hat[t] <- ((1 - a) / a) * (S1[t] - S2[t])
  }
  
  # Prévision à horizon h=1 : X̂_t(1) = â_t + b̂_t
  for (t in 1:n) {
    x_prediction[t] <- a_hat[t] + b_hat[t]
  }
  
  return(list(
    x_prediction = ts(x_prediction),
    S1    = S1,
    S2    = S2,
    a_hat = a_hat,
    b_hat = b_hat
  ))
}

# Définir les deux séries
X <- ts(c(23.1,25.6,27.2,24.5,25.1,26.6,23.2,24.9,
          24.8,23.6,26.2,24.5,25.4,27.3,26.5,23.5,25.1))

Y <- ts(c(12.9,13.5,14.2,14.3,14.7,14.8,16.2,15.1,
          15.8,17.2,15.9,16.3,17.3,17.9,18.9,18.2,18.5))

# Tracer les deux séries
par(mfrow = c(2, 1), mar = c(3, 4, 3, 1))

plot.ts(X,
        main = "Série X (n=17)",
        col  = "blue", lwd = 2,
        ylab = "X_t", xlab = "Temps")
abline(lm(X ~ time(X)), col = "red", lty = 2)  # droite de tendance

plot.ts(Y,
        main = "Série Y (n=17)",
        col  = "darkgreen", lwd = 2,
        ylab = "Y_t", xlab = "Temps")
abline(lm(Y ~ time(Y)), col = "red", lty = 2)

par(mfrow = c(1, 1))

# Vérifier visuellement la tendance
cat("--- Série X ---\n")
cat("Moyenne :", mean(X), "\n")
cat("Corrélation X ~ t :", cor(X, 1:17), "\n")

cat("\n--- Série Y ---\n")
cat("Moyenne :", mean(Y), "\n")
cat("Corrélation Y ~ t :", cor(Y, 1:17), "\n")