# Exo 1
saison_add <- function(X) {
  n <- length(X)
  p <- frequency(X)
  
  MM <- filter(X, rep(1/p,p), sides = 2)
  
  diff <- X - MM
  
  mat <- matrix(diff, ncol = p, byrow = TRUE)
  
  s_prime <- colMeans(mat, na.rm = TRUE)
  
  S <- s_prime - mean(s_prime)
  
  return(S)
}

saison_multi <- function(X) {
  n <- length(X)
  p <- frequency(X)
  
  MM <- filter(X, rep(1/p,p), sides = 2)
  
  diff <- X / MM
  
  mat <- matrix(diff, ncol = p, byrow = TRUE)
  
  s_prime <- colMeans(mat, na.rm = TRUE)
  
  S <- s_prime / mean(s_prime)
  
  return(S)
}

X0 <- ts(data3$X0, frequency = 4)   # trimestriel
X1 <- ts(data3$X1, frequency = 4)

# Calculer les coefficients
s0 <- saison_add(X0)
s1 <- saison_multi(X1)

# Afficher
s0
s1

# Vérifications
sum(s0)          # doit être ≈ 0  (additif)
sum(s1)          # doit être ≈ p  (multiplicatif)

# Exo 2
Z <- ts(c(118.2, 129, 138.9, 157.1,
          148.6, 154.5, 163,  184,
          163.3, 175.3, 189.1, 217.9,
          186.3, 198.8, 211.4, 247.5),
        frequency = 4, start = c(2015, 1))

plot.ts(Z,
        main = "Indices trimestriels de vente",
        xlab = "Temps", ylab = "Indice",
        col  = "blue", lwd = 2)

# Réorganiser en matrice 4 années × 4 trimestres
mat <- matrix(Z, nrow = 4, ncol = 4, byrow = TRUE)
rownames(mat) <- 2015:2018
colnames(mat) <- paste("T", 1:4)

# Tracer les profils (une courbe par année)
matplot(t(mat),
        type = "b", pch = 16,
        col  = c("blue","red","green","purple"),
        main = "Méthode des profils",
        xlab = "Trimestre", ylab = "Indice",
        xaxt = "n")
axis(1, at = 1:4, labels = paste("T", 1:4))
legend("topleft",
       legend = 2015:2018,
       col    = c("blue","red","green","purple"),
       lty = 1, pch = 16)

x_bar <- apply(mat, 1, mean)   # moyenne par ligne (année)
sigma  <- apply(mat, 1, sd)    # écart-type par ligne

# Afficher
data.frame(Année  = 2015:2018,
           Moyenne = round(x_bar, 4),
           EcartType = round(sigma, 4))

# Régression sigma_i ~ x_bar_i
reg <- lm(sigma ~ x_bar)
summary(reg)

# Si pente ≈ 0 → additif, sinon → multiplicatif
cat("Pente a =", round(coef(reg)[2], 6), "\n")

MM4 <- filter(Z, rep(1/4, 4), sides = 2)

# Afficher
round(MM4, 4)

plot.ts(Z,
        main = "Série X et moyennes mobiles",
        col  = "blue", lwd = 1.5,
        ylab = "Indice", xlab = "Temps")
lines(MM4, col = "red", lwd = 2)
legend("topleft",
       legend = c("Série X", "MM d'ordre 4"),
       col    = c("blue", "red"), lty = 1)

s <- saison_add(Z)   # réutiliser la fonction de l'Ex1
names(s) <- paste("T", 1:4)
round(s, 4)
# Vérification : sum(s) ≈ 0

# Z_t = X_t - s_j  (modèle additif)
s_rep <- rep(s, length.out = length(Z))  # répéter les 4 coefficients
W <- Z - s_rep
plot.ts(W, main = "Série corrigée Z", col = "darkgreen")

t <- 1:length(X)

# Régression linéaire sur Z
reg <- lm(Z ~ t)
a <- coef(reg)[2]
b <- coef(reg)[1]

cat("a =", round(a, 4), "\n")
cat("b =", round(b, 4), "\n")
cat("T_t =", round(a,4), "* t +", round(b,4), "\n")

# Valeurs ajustées de la tendance
T_t <- ts(a * t + b, frequency = 4, start = c(2015,1))

# Modèle complet
cat("Modèle : X_t =", round(a,4), "* t +", round(b,4), "+ S_j\n")

# Prévisions : X̂_t = T_t + s_j
X_hat <- T_t + s_rep

plot.ts(X,
        main = "Série X et prévisions du modèle",
        col  = "blue", lwd = 1.5)
lines(X_hat, col = "red", lwd = 2, lty = 2)
legend("topleft",
       legend = c("Série X", "Prévisions X̂"),
       col    = c("blue","red"), lty = c(1,2))

# Erreurs
e <- X - X_hat

# Visualiser
par(mfrow = c(1, 2))

plot.ts(e,
        main = "Erreurs de prévision",
        col  = "purple", ylab = "e_t")
abline(h = 0, col = "red", lty = 2)

hist(e,
     main = "Distribution des erreurs",
     col  = "plum", xlab = "Erreurs")
abline(v = 0, col = "red", lty = 2)

par(mfrow = c(1, 1))

# Statistiques
cat("Moyenne des erreurs :", round(mean(e), 6), "\n")
cat("Variance des erreurs :", round(var(e), 4), "\n")