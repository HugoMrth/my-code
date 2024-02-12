#Package BINDA
library(binda)



# X la variable à seuiller
# Doit être sous forme de matrice
X <- matrix(var_a_seuiller, ncol = 1)

# Y la variable discriminante sous forme de vecteur
Y <- var_discriminante



# Calcul du seuil par la méthode BINDA
(th_glob <- binda::optimizeThreshold(X, Y))

# Dichotomisation de la variable X selon le nouveau seuil
table(Y, dichotomize(X, th_glob))