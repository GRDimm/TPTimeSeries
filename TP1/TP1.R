# Exercice 1

epsilon = runif(101, -3, 3)
X = epsilon[2:101] - 2*epsilon[1:100]
epsilon = epsilon[1:100]

# Correlogramme de epsilon
acf(epsilon, main="Correlogramme de epsilon")

# Correlogramme de X
acf(X, main="Correlogramme de X")

# On voit que le lag 2 de X est en permanence significatif et négatif. Epsilon est un bruit blanc donc on ne voit rien dans les lags.


# Exercice 2

# Taille de la matrice
n <- 1000

# Créer une matrice avec des 5 sur la diagonale
matrice <- diag(5, n, n)

# Modifier les diagonales supérieure et inférieure
for (i in 1:(n-1)) {
  matrice[i, i+1] <- -2  # Diagonale supérieure
  matrice[i+1, i] <- -2  # Diagonale inférieure
}

Z = runif(n)
C = chol(matrice)

X = C%*%Z

acf(X)

# Quand n augmente, le lag 2 de X converge vers -0.4

nmc = 10000
n = 200

MC <- list()
for (i in 1:nmc) {
  # Créer une matrice avec des 5 sur la diagonale
  matrice <- diag(5, n, n)
  
  # Modifier les diagonales supérieure et inférieure
  for (j in 1:(n-1)) {
    matrice[j, j+1] <- -2  # Diagonale supérieure
    matrice[j+1, j] <- -2  # Diagonale inférieure
  }
  
  Z = runif(n)
  C = chol(matrice)
  
  X = C %*% Z
  
  MC[[i]] = sqrt(n) * (acf(X, plot = FALSE)$acf[2] + 0.4)  # Utiliser des doubles crochets
  print(i)
}

# Convertir la liste en vecteur pour l'histogramme
MC_vecteur <- unlist(MC)

hist(MC_vecteur, breaks = 30, main = "Histogramme des valeurs d'Autocorrélation", xlab = "Autocorrélation au décalage 1")

print(mean(MC_vecteur))
print(sd(MC_vecteur))


# On a un ecart-type d'environ 0.79 


