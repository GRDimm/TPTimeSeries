# Exercice : AR(3) avec un seul coefficient non nul

# Définir le paramètre alpha et la taille de la série
alpha <- 0.1 # Assurez-vous que |alpha| < 1
n <- 10000 # Nombre de points dans la série temporelle

# Initialiser la série avec des zéros
X <- rep(0, n)

# Générer le bruit blanc avec une variance de 1
epsilon <- rnorm(n, mean = 0, sd = 1)

# Générer le processus AR(3)
for (i in 4:n) {
  X[i] <- alpha * X[i-3] + epsilon[i]
}

# Tracer la série temporelle
plot(X, type = "l", main = "Processus AR(3)")

# Tracer la fonction d'autocorrélation
acf(X, main = "Autocorrélation du processus AR(3)")
