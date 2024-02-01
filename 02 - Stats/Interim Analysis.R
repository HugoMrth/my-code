library(ldbounds)
library(PwrGSD)
library(rpact)
library(gsDesign)




# I. Calcul des timings ####

# _a. Une analyse intermediaire ####

n_interim <- 2 # Nb analyses inter
duree <- 30 # Duree de l'etude

# Fonction de proposition de différents timings
(timings <- commonbounds(n_interim, t = (1:n_interim)/n_interim, t2 = (1:n_interim)/n_interim,
                         iuse = "OF", alpha = 0.05, sides = 2))

# Une seule option possible pour K = 2

(t_interim <- duree*timings$time)
(time <- timings$time)





# _b. Deux analyses intermediaires ####

n_interim <- 3 # Nb analyses inter
duree <- 30 # Duree de l'etude

# Fonction de proposition de différents timings
(timings <- commonbounds(n_interim, t = (1:n_interim)/n_interim, t2 = t,
             iuse = "OF", alpha = 0.05, sides = 2))

# Deux options possibles pour K = 2

(t_interim <- duree*timings$time2)
(time <- timings$time2)


(t_interim <- duree*timings$time)
(time <- timings$time)









# II. Calcul des alpha intermédiaires ####

# Les timings sont exprimes en proportions de la durre complete de l'etude
# Le dernier timing correspon a la fin de l'etude (analyse finale)
# time est donc un vecteur de taille K + 1 (K le nombre d'analyses intermediaires)
# Et le dernier element de time = 1

time


# Les exemples sont ici tous donnes pour une fonctions de cout de O'Brien Fleming,
# Et selon l'approche de Lan-Demets


# _a. Cas bilateral ####

# Les lpha retenus dsont dans la colonnes "Exit.pr"
summary(ldbounds::ldBounds(
  time, # timings
  iuse = 1, # Methode utilisee : 1 = O'Brien Fleming
  alpha = 0.05,
  sides = 2 # 1 = unilateral, 2 = bilateral
))

# Deuxieme package, memes resultat
# Ici, les alphas sont les Output : Cumulative alpha spending
rpact::getDesignGroupSequential(
  sided = 2,
  alpha = 0.05,
  informationRates = time,
  typeOfDesign = "asOF" # methode : "asOF" = O'Brien Fleming
)





# _b. Cas unilateral ####

# Meme chose avec sides = 1

summary(ldbounds::ldBounds(
  time,
  iuse = 1,
  alpha = 0.025,
  sides = 1
))

rpact::getDesignGroupSequential(
  sided = 1,
  alpha = 0.025,
  informationRates = time,
  typeOfDesign = "asOF"
)





# _c. Cas bilateral asymetrique ####

# Le cas bilateral aysetrique correspond a l'etablissement des deux hypotheses de superiorite et de futilite
# Cela revient aussi au meme que d'aditionner deux cas unilateraux avec des alphas differents



# Ici, seul le pkg ldbounds peut gerer le melange d'hypotheses
# Les deux emeples ci dessous fournissent les memes resultats
ldbounds::ldBounds(
  time,
  iuse = 1,
  alpha = c(0.01, 0.04),
  sides = 2
)$exit.pr


ldbounds::ldBounds(
  time,
  iuse = 1,
  alpha = 0.01,
  sides = 1
)$exit.pr +
ldbounds::ldBounds(
  time,
  iuse = 1,
  alpha = 0.04,
  sides = 1
)$exit.pr
