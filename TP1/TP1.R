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