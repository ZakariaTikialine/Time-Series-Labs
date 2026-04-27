# Exo 1

serie1 <- data$série1
serie2 <- data$série2
serie3 <- data$série3

serie1
serie2
serie3

# Série 1
plot.ts(serie1, 
        main = "Évolution Série 1",
        xlab = "Temps", 
        ylab = "Observations", 
        col  = "blue")
"La série 1 présente :
— une tendance à la hausse à long terme,
— pas de saisonnalité visible avec une période d'environ [p] observations,
— une amplitude des oscillations constante → additif,
— des variations accidentelles faibles.
→ Le modèle retenu est le modèle additif."

# Série 2
plot.ts(serie2,
        main = "Évolution Série 2",
        xlab = "Temps",
        ylab = "Observations",
        col  = "red")
"La série 2 présente :
— une tendance à la hausse à long terme,
— pas de saisonnalité visible avec une période d'environ [p] observations,
— une amplitude des oscillations constante → additif,
— des variations accidentelles faibles.
→ Le modèle retenu est le modèle additif."

# Série 3
plot.ts(serie3,
        main = "Évolution Série 3",
        xlab = "Temps",
        ylab = "Observations",
        col  = "darkgreen")
"La série [n] présente :
— une tendancestable à long terme,
— pas de saisonnalité visible avec une période d'environ [p] observations,
— une amplitude des oscillations croissante → multiplicatif,
— des variations accidentelles importantes.
→ Le modèle retenu est le modèle multiplicatif."

# Exo 2

t <- 1:n
z <- log(0.55 * t + 2)

# Tracer Y d'abord
plot.ts(Y,
        main = "Série Y et tendance z(t) = log(0.55t + 2)",
        xlab = "Temps",
        ylab = "Valeurs",
        col  = "blue",
        ylim = range(c(Y, z)))  # adapter l'axe Y aux deux courbes

# Ajouter z(t) par dessus
lines(t, z, col = "red", lwd = 2)

# Ajouter une légende
legend("topleft",
       legend = c("Série Y", "z(t) = log(0.55t+2)"),
       col    = c("blue", "red"),
       lty    = 1,
       lwd    = c(1, 2))

# Calculer les résidus
e <- Y - log(0.55 * t + 2)

# Tracer les résidus
plot.ts(e, main="Résidus e_t", col="purple")
abline(h=0, col="red", lty=2) # ligne y=0

# Exo 3
# Centré réduit = moyenne 0, écart-type 1
e1 <- ts(rnorm(100, mean = 0, sd = 1))

# Tracer le graphe
plot.ts(e1,
        main = "Bruit blanc e1 (centré réduit, n=100)",
        xlab = "Temps",
        ylab = "Valeurs",
        col  = "blue")
abline(h = 0, col = "red", lty = 2)  # ligne de moyenne

hist(e1,
     main = "Histogramme de e1",
     xlab = "Valeurs",
     col  = "lightblue")


# variance = 2.5 → écart-type = sqrt(2.5)
e2 <- ts(rnorm(200, mean = 0, sd = sqrt(2.5)))

# Tracer
plot.ts(e2,
        main = "Bruit blanc e2 (n=200, σ²=2.5)",
        xlab = "Temps",
        ylab = "Valeurs",
        col  = "darkgreen")
abline(h = 0, col = "red", lty = 2)

# Comparer les deux histogrammes côte à côte
par(mfrow = c(1,2))
hist(e1, main = "e1 (σ²=1)",   col = "lightblue",  xlim = c(-4,4))
hist(e2, main = "e2 (σ²=2.5)", col = "lightgreen", xlim = c(-4,4))
par(mfrow = c(1,1))

# Exo 4
# Paramètres
n <- 150
t <- 1:n

# Les 3 composantes
T_t <- 0.75 * t + 1.5                    # Tendance linéaire
S_t <- 10 * sin(2 * pi * t / 25)         # Saisonnalité période 25
e_t <- ts(rnorm(n, mean = 0, sd = 1))    # Bruit blanc gaussien variance 1

# Série complète
X_t <- ts(T_t + S_t + e_t)

# Tracer toutes les composantes
par(mfrow = c(4,1), mar = c(2,4,2,1))

plot.ts(T_t, main = "Tendance T_t = 0.75t + 1.5",
        col = "red",    ylab = "T_t")

plot.ts(S_t, main = "Saisonnalité S_t = 10sin(2πt/25)",
        col = "blue",   ylab = "S_t")

plot.ts(e_t, main = "Bruit blanc e_t ~ N(0,1)",
        col = "gray",   ylab = "e_t")

plot.ts(X_t, main = "Série complète X_t = T_t + S_t + e_t",
        col = "black",  ylab = "X_t")

par(mfrow = c(1,1))

# Params
n <- 150
t <- 1:n

T_t <- (1/20) * t^2 + 1                  # Tendance quadratique
S_t <- 50 * cos(2 * pi * t / 25)         # Saisonnalité période 25
e_t <- runif(n, min = 1, max = 5)        # Loi uniforme sur [1,5]

X_t <- ts(T_t * S_t * e_t)

par(mfrow = c(4,1), mar = c(2,4,2,1))

plot.ts(ts(T_t), main = "Tendance T_t = t²/20 + 1",
        col = "red",    ylab = "T_t")

plot.ts(ts(S_t), main = "Saisonnalité S_t = 50cos(2πt/25)",
        col = "blue",   ylab = "S_t")

plot.ts(ts(e_t), main = "Erreur e_t ~ Uniforme[1,5]",
        col = "gray",   ylab = "e_t")

plot.ts(X_t,     main = "Série complète X_t = T_t × S_t × e_t",
        col = "black",  ylab = "X_t")

par(mfrow = c(1,1))