#### Parametres ####
N_BOX <- 5 # Nombre de boites possibles
N_ITER <- 10000 # Nombre de recherches
N_JOUR_MAX <- 10 # Nombre de jour maxial pour le calcul de l'ensemble des trajectoires

#### Calcul des trajectoires ####
# Je calcule d'abord toutes les trajectoires existantes 
# Puis je selectionne seulement les valides

system.time({
  ALL_TRAJECTORIES <- matrix(1:N_BOX, ncol = 1) # Initialisation au jour 1
  for (i in 2:N_JOUR_MAX) { # Boucle pas jour
    # Copier colle des trajectoires 5 fois
    # Avec ajout des nouvelles position
    ALL_TRAJECTORIES <- rbind( 
      cbind(ALL_TRAJECTORIES, 1),
      cbind(ALL_TRAJECTORIES, 2),
      cbind(ALL_TRAJECTORIES, 3),
      cbind(ALL_TRAJECTORIES, 4),
      cbind(ALL_TRAJECTORIES, 5)
    )
    # Selectin des trajectoires valides
    ALL_TRAJECTORIES <- ALL_TRAJECTORIES[apply(ALL_TRAJECTORIES, 1, function(x) {all(diff(x) %in% c(-1, 1))}),]
  }
})

# #### Recherche du chat ####
# system.time({
#   # Initialisation
#   N_TRIES <- rep(NA, N_ITER)
#   
#   # Boucle du processus
#   # Repetition de l'algo de recherche du chat N_ITER fois
#   for (i in 1:N_ITER) {
#     # Initialisation
#     TRUE_BOX <- sample.int(N_BOX, 1) # Vrai position du chat
#     PRED_BOX <- sample.int(c(2, 3, 4), 1) # Premiere prediction
#     j <- 1 # Compteur du nombre d'essais
# 
#     # Preselection des trajectoires restantes (premiere position du chat differente du premier choix)
#     TRAJECTORIES <- ALL_TRAJECTORIES[ALL_TRAJECTORIES[, 1] != PRED_BOX, ]
#     
#     # Boucle tant que la prediction est differente de la realite
#     # Simulation des jours de recherche
#     while (TRUE_BOX != PRED_BOX) { 
#       # Definition du mouvement du chat
#       TRUE_BOX <- TRUE_BOX + ifelse(TRUE_BOX == 1, 1, # Si tout a gauche, obligatoirement a droite
#                                     ifelse(TRUE_BOX == 5, -1, # Si tout a droite, obligatoirement  gauche
#                                            sample(c(-1, 1), 1))) # Sinon, au hasard
# 
#       
#       if (j != N_JOUR_MAX) {
#         # Selection des trajectoires restantes possible apres le choix du jour d'avant
#         if (j != 1) TRAJECTORIES <- TRAJECTORIES[TRAJECTORIES[, j] != PRED_BOX, ]
#         # Mise a plat des choix possible au jour donne
#         CHOICES <- table(unique.matrix(TRAJECTORIES[, 1:(j+1)], MARGIN = 1)[, j+1])
#         CHOICES <- CHOICES[CHOICES == max(CHOICES)]
#         
#         if (length(CHOICES) == 1) {
#           PRED_BOX <- as.numeric(names(CHOICES))
#         } else {
#           PRED_BOX <- sample(x = as.numeric(names(CHOICES)), size = 1)
#         }
#         
#         j <- j + 1
#       } else {
#         PRED_BOX <- TRUE_BOX
#         j <- j + 1
#       }
#     }
#     N_TRIES[i] <- j
#   }
# })
# 
# mean(N_TRIES)
# table(N_TRIES)
# hist(N_TRIES)






#if (j != 1) TRAJECTORIES <- unique.matrix(TRAJECTORIES[apply(TRAJECTORIES[, 1:j], 1, function(x) all(x != ALL_PRED)), ], MARGIN = 1)



# if (length(CHOICES) == 1) {
#   PRED_BOX <- as.numeric(names(CHOICES))
# } else {
#   PRED_BOX <- sample(x = as.numeric(names(CHOICES)), size = 1, prob = CHOICES)
# }


# MOVE <- ifelse(TRUE_BOX == 1, "right", # Si tout a gauche, obligatoirement a droite
#                ifelse(TRUE_BOX == 5, "left", # Si tout a droite, obligatoirement  gauche
#                       sample(c("right", "left"), 1))) # Sinon, au hasard
# 
# if (MOVE == "right") { # Selon le cote vers lequel le chat doit aller
#   TRUE_BOX <- TRUE_BOX + 1
# } else {
#   TRUE_BOX <- TRUE_BOX - 1
# }

#### Recherche du chat ####
#### Recherche du chat ####

  
  # Boucle du processus
  # Repetition de l'algo de recherche du chat N_ITER fois
  find_cat <- function() {
    # Initialisation
    TRUE_BOX <- sample.int(N_BOX, 1) # Vrai position du chat
    PRED_BOX <- sample.int(c(2, 3, 4), 1) # Premiere prediction
    j <- 1 # Compteur du nombre d'essais
    
    # Preselection des trajectoires restantes (premiere position du chat differente du premier choix)
    TRAJECTORIES <- ALL_TRAJECTORIES[ALL_TRAJECTORIES[, 1] != PRED_BOX, ]
    
    # Boucle tant que la prediction est differente de la realite
    # Simulation des jours de recherche
    while (TRUE_BOX != PRED_BOX) { 
      # Definition du mouvement du chat
      TRUE_BOX <- TRUE_BOX + ifelse(TRUE_BOX == 1, 1, # Si tout a gauche, obligatoirement a droite
                                    ifelse(TRUE_BOX == 5, -1, # Si tout a droite, obligatoirement  gauche
                                           sample(c(-1, 1), 1))) # Sinon, au hasard
      
      
      if (j != N_JOUR_MAX) {
        # Selection des trajectoires restantes possible apres le choix du jour d'avant
        if (j != 1) TRAJECTORIES <- TRAJECTORIES[TRAJECTORIES[, j] != PRED_BOX, ]
        # Mise a plat des choix possible au jour donne
        CHOICES <- table(unique.matrix(TRAJECTORIES[, 1:(j+1)], MARGIN = 1)[, j+1])
        CHOICES <- CHOICES[CHOICES == max(CHOICES)]
        
        if (length(CHOICES) == 1) {
          PRED_BOX <- as.numeric(names(CHOICES))
        } else {
          PRED_BOX <- sample(x = as.numeric(names(CHOICES)), size = 1)
        }
        
        j <- j + 1
      } else {
        PRED_BOX <- TRUE_BOX
        j <- j + 1
      }
    }
    j
  }

system.time({
  TRIES <- replicate(N_ITER, find_cat())
})

mean(TRIES)
table(TRIES)
hist(TRIES)
