
# Utilisation du package Icarus
library(icarus)
library(dplyr)
library(magrittr)

# Taille de la population totale
N <- 230 ## population total


## Fonction du package Icarus
weightedMean(data_employees$movies, data_employees$weight, N)
# Fonction prenant l'argument na.rm = TRUE si besoin
weighted.mean(data_employees$movies, data_employees$weight)

## Enter calibration margins:
margins <- newMarginMatrix() %>%
  addMargin("category", c(0.35, 0.40, 0.25)) %>%
  addMargin("sex", c(0.6, 0.4)) %>%
  addMargin("department", c(0.45, 0.55)) %>%
  addMargin("salary", 470000)

margins

# Variables facteurs codees en numerique dans le jeu de donnes
# Modalites dans l'ordre des répartition de pourcentages des marges
# Les noms de variables doivent correspondre entre la matrice de marges et le jeu de donnees
head(data_employees)

## Compute calibrated weights with raking ratio method
# Ressort un vecteir de poids de taille nrow(data)
# colWeight est obligatoire
  # Si pas de poids, ajouter une colonne constante
  # data_employees$weight <- 1
wCal <- calibration(data = data_employees, marginMatrix = margins, 
                    colWeights = "weight", # requis
                    method = "raking", 
                    pct = TRUE, # requis si marges en pourcentage
                    description = FALSE, 
                    popTotal = N)

wCal

# sum(wCal) == N ???
(c(sum(wCal), N))


## Calibrated estimate: 2.471917
weightedMean(data_employees$movies, wCal, N)
