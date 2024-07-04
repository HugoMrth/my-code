rm(list = ls())

# Import ------------------------------------------------------------------

library(FactoMineR) # Fonctions ACM et ACP
library(factoextra) # Graphiques ACM et ACP
library(cluster)

library(dendextend) # Graphiques pour la CAH
library(ggdendro)

library(ggplot2) # Autres fonctions graphiques
library(ggpubr)
library(gridExtra)
library(corrplot)
library(plotly)

library(gtsummary) # Tableaux et manipulation de donnees
library(GGally)
library(dplyr)


load("C:/Users/h.marthinet/Documents/32 - R/Codes & Scripts/Rdata/MCA.RData")




# I. Mise en place des donnees pour l'ACM ------------------------------------


# _a. Definition des variables ####
vars <- 9:39
vars_sup <- 1:8

# Selection des variables
DATA_ACM <- DATA[, c(vars_sup, vars)]
DATA_ACM <- as.data.frame(apply(DATA_ACM, 2, as.factor))

# _b. Gestion des valeurs manquantes ####
# Verification du nombre de valeurs manquantes par lignes
table(rowSums(is.na(DATA_ACM[, -c(1:length(vars_sup))])))
# Suppression des lignes contenant trop de valeurs manquantes si besoin
# DATA_ACM <- DATA_ACM[rowSums(is.na(DATA_ACM[, -c(1:length(vars_sup))])) < 2, ]


# Verification du nombre de valeurs manquantes par colonnes
colSums(is.na(DATA_ACM[, -c(1:length(vars_sup))]))
# Recodage des modalites manquantes si besoin
# Ou suppression de variable dans le jeu de donnees

# Matrice des Chi-2
ggcorrplot::ggcorrplot(DATA_ACM[, -c(1:length(vars_sup))], show.diag = FALSE, type = "upper", tl.cex = 12, legend.title = "p-val Chi-2") + 
  scale_fill_gradientn(breaks = c(0, 0.05, 0.1, 0.25, 0.5, 1), limit = c(0, 1), 
                       guide = "legend", colors = RColorBrewer::brewer.pal(6, "RdBu"), space = 'p-value')


# II. ACM ----------------------------------------------------------------

### _a. Creation de l'ACM ####

# J le nombre de modalites total
# 1 dans le cas d'une variable quanti (FAMD)
J <- 0
for (i in 1:ncol(DATA_ACM)) {
  J <- J + ifelse(is.numeric(DATA_ACM[, i]), 1, nlevels(as.factor(DATA_ACM[, i]))-1)
}

# Premiere ACM 
# Toutes les dimensions sont retenues
ACM <- MCA(DATA_ACM,
           quali.sup = 1:length(vars_sup), 
           graph = FALSE,
           ncp = J)

# Deuxieme ACM
# En en retenant que les dimensions portant plus d'information que la moyenne
ACM <- MCA(DATA_ACM,
           quali.sup = 1:length(vars_sup), 
           graph = FALSE,
           ncp = max(which(ACM$eig[, 2] > 100/J)))
# En ne retenant que le nombre de dimensions requises pour avoir 80 % d'info
ACM <- MCA(DATA_ACM,
           quali.sup = 1:length(vars_sup), 
           graph = FALSE,
           ncp = min(which(ACM$eig[, 3] > 80)))



### Valeurs propres
head(ACM$eig)
sum(ACM$eig[, 1])
fviz_screeplot(ACM, choice = "variance")

### Modalites et variables
fviz_mca_biplot(ACM)
fviz_mca_biplot(ACM, axes = c(3, 4))







### _b. Graphiques des individus ####
# Que le nuage de points des individus
fviz_mca_ind(ACM)
# Contributions des individus aux axes
fviz_mca_ind(ACM, col.ind = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))
grid.arrange(
  fviz_contrib(ACM, choice = "ind", axes = 1, top = 25),
  fviz_contrib(ACM, choice = "ind", axes = 2, top = 25),
  fviz_contrib(ACM, choice = "ind", axes = 1:2, top = 25),
  nrow = 3
)
# Qualites de representation des individus sur les axes
fviz_mca_ind(ACM, col.ind = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))
grid.arrange(
  fviz_cos2(ACM, choice = "ind", axes = 1, top = 25),
  fviz_cos2(ACM, choice = "ind", axes = 2, top = 25),
  fviz_cos2(ACM, choice = "ind", axes = 1:2, top = 25),
  nrow = 3
)





### _c. Graphiques des variables ####

# ACM
  # Modalites des variables
  fviz_mca_var(ACM)
  fviz_mca_var(ACM, repel = TRUE)
  # Correlations des variables aux axes
  fviz_mca_var(ACM, choice = "mca.cor")
  fviz_mca_var(ACM, choice = "mca.cor", axes = c(3,4))
# FAMD
  # Partie Quali
  fviz_famd_var(FAMD, choice = "quali.var", repel = TRUE,
                col.var = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))
  # Partie Quanti
  fviz_famd_var(FAMD, choice = "quanti.var", 
                col.var = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))
  

# Qualites de representation des variables sur les axes
fviz_mca_var(ACM, col.var = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))
grid.arrange(
  fviz_cos2(ACM, choice = "var", axes = 1, top = 25),
  fviz_cos2(ACM, choice = "var", axes = 2, top = 25),
  fviz_cos2(ACM, choice = "var", axes = 1:2, top = 25),
  nrow = 3
)
corrplot(ACM$var$cos2, is.corr = FALSE)
# Contributions des variables aux axes
fviz_mca_var(ACM, col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))
grid.arrange(
  fviz_contrib(ACM, choice = "var", axes = 1, top = 25),
  fviz_contrib(ACM, choice = "var", axes = 2, top = 25),
  fviz_contrib(ACM, choice = "var", axes = 1:2, top = 25),
  nrow = 3
)
corrplot(apply(ACM$var$contrib, 2, function(x) {x/sum(x)}), is.corr = FALSE)
# Matrice de distances
fviz_dist(dist(ACM$var$coord), order = TRUE, show_labels = TRUE)


### Representation du graphiques des individus par facteurs
# Selon toutes les variables utilisees pour construire l'ACM
for (i in 0:(ncol(ACM$call$X) %/% 9)) {
  print(plotellipses(ACM, keepvar = 1:9 + 9*i, means = FALSE))
}
# Meme chose pour les variables supplementaires
plotellipses(ACM, keepvar = "quali.sup", means = FALSE)




# III. CAH sur les variables ---------------------------------------------------
# Pour mieux interpreter les resultats
# Permet une nouvelles visualisation des variable pour mieux identifer les comportements

### _a. Dendrogramme ####
# Creation de la matrice des distances
# Ici depuis les resultats de l'ACM
md <- dist(ACM$var$coord)

# Creation de l'arbre
arbre <- hclust(md, method = "ward.D2")

plot(arbre, labels = FALSE, main = "Dendrogramme")
ggdendrogram(arbre, labels = FALSE)


### _b. Decoupage ####
# Inertie de l'arbre
# Pour prise de decision sur le nombre de classes
# Notion de coude comme pour les valeurs propres de l'ACM
inertie <- sort(arbre$height, decreasing = TRUE)
plot(inertie[1:20], type = "s", xlab = "Nombre de classes", ylab = "Inertie")

# Ici plutot 3 ou 4
plot(inertie[1:20], type = "s", xlab = "Nombre de classes", ylab = "Inertie")
points(c(3, 4), inertie[c(3, 4)], col = c("green3", "red3"), cex = 2, lwd = 3)

# Visualisation des decoupagess possibles sur l'arbre
plot(arbre, labels = FALSE, main = "Partition en 3 ou 4 classes", xlab = "", ylab = "", sub = "", axes = FALSE, hang = -1)
rect.hclust(arbre, 3, border = "green3")
rect.hclust(arbre, 4, border = "red3")

# Meilleure visualisation pour une seul decoupage
# Ici choix d'une classification en 3 classes
fviz_dend(arbre, k = 3, show_labels = FALSE, rect = TRUE)

# _c. Creation des classes ####
classes_var <- cutree(arbre, 3)
freq(classes_var)

# Listing 
by(names(classes_var), classes_var, as.vector)

# _d. Visualisation dans l'ACM ####

fviz_mca_var(ACM, habillage = as.factor(classes_var), addEllipses = TRUE)
fviz_mca_var(ACM, habillage = as.factor(classes_var), addEllipses = TRUE, repel = TRUE)





# IV. CAH sur les individus ---------------------------------------------------------------------
# Classificationn des individus depuis les nouvelles coordonnees
# Obtenues depuis les resultats de l'ACP ou de l'ACM



### _a. Dendrogramme ####
# Creation de la matrice des distances
# Ici depuis les resultats de l'ACM
md <- dist(ACM$ind$coord)

# Creation de l'arbre
arbre <- hclust(md, method = "ward.D2")

plot(arbre, labels = FALSE, main = "Dendrogramme")
ggdendrogram(arbre, labels = FALSE)



###  Inertie 


# Inertie de l'arbre
# Pour prise de decision sur le nombre de classes
# Notion de coude comme pour les valeurs propres de l'ACM
inertie <- sort(arbre$height, decreasing = TRUE)
plot(inertie[1:20], type = "s", xlab = "Nombre de classes", ylab = "Inertie")

# Ici plutot 3 ou 4
plot(inertie[1:20], type = "s", xlab = "Nombre de classes", ylab = "Inertie")
points(c(3, 5, 7), inertie[c(3, 5, 7)], col = c("green3", "red3", "blue3"), cex = 2, lwd = 3)

# Visualisation des decoupagess possibles sur l'arbre
plot(arbre, labels = FALSE, main = "Partition en 3 ou 4 classes", xlab = "", ylab = "", sub = "", axes = FALSE, hang = -1)
rect.hclust(arbre, 3, border = "green3")
rect.hclust(arbre, 5, border = "red3")
rect.hclust(arbre, 7, border = "blue3")


# Toujours avec l'inertie, choix par minimisation du ratio d'inertie en Q et Q+1 clusters
ratios_inerties <- (-diff(inertie[1:10]))/(-diff(inertie[2:11]))
names(ratios_inerties) <- 1:9
sort(ratios_inerties)

fviz_dend(arbre, k = 3, show_labels = FALSE)





# _b. Criteres de validite ####


# 30 criteres calcules sur 1 a 15 clusters
# ATTENTION, prend un moment a tourner
# Infos sur la meilleur nombre de cluster par indicateurs
nbclus <- NbClust::NbClust(ACM$ind$coord, method = "ward.D2")
nbclus$All.index
nbclus$Best.nc

# Les connus :
  # CH = Calinski -> a maximiser
  # Silhouette -> a maximiser
  # Dunn -> a maximiser
  # Beale -> a maximiser







# _c. Procedure criteres de similarite ####
# Basee sur la procedure presentee par Ben-Hur (2002)
# Boucle de classif sur 80% des donnees

Jaccard <- function(a, b){
  intersection <- length(intersect(a, b))
  union <- length(a) + length(b) - intersection
  return(intersection/union)
}

# Definition des parametres
prop_f <- 0.8 # Sampling ratio
k_max <- 10 # Nombre max de cluster a construire
n_subsample <- 1000 # Nombre d'iteration de la proc
method <- "average" # Methode pour la CAH
data <- ACM$ind$coord

#Initialisation des matrices d'indice de similarite
M_FM <- matrix(NA, ncol = k_max-1, nrow = n_subsample)
# M_aRand <- matrix(NA, ncol = k_max-1, nrow = n_subsample)
# M_jaccard <- matrix(NA, ncol = k_max-1, nrow = n_subsample)

# Proc
for (i in 2:k_max) { # Boucle sur les cluster
  #Initialisation des vecteurs d'indice de similarite
  FM <- rep(NA, n_subsample)
  # aRand <- rep(NA, n_subsample)
  # jaccard <- rep(NA, n_subsample)
  
  for (j in 1:n_subsample) { # Boucle
    # Definition des deux subset -> application du samplig ratio sur data
    subset1 <- data[sample.int(nrow(data), floor(nrow(data)*prop_f)), ]
    subset2 <- data[sample.int(nrow(data), floor(nrow(data)*prop_f)), ]
    
    # Calcul des partitions en i cluster par CAH, sur les deux subset
    part1 <- cutree(hclust(dist(subset1), method = method), i)
    part2 <- cutree(hclust(dist(subset2), method = method), i)
    
    # Indices de similarite des partitions
    FM[j] <- FM_index(part1, part2)
    # aRand[j] <- pdfCluster::adj.rand.index(part1, part2)
    # jaccard[j] <- Jaccard(part1, part2)
  }
  
  M_FM[, i-1] <- FM
  # M_aRand[, i-1] <- aRand
  # M_jaccard[, i-1] <- jaccard
}

# Histogramme de similarite par nombre de clusters
# Selection du k apr?s decrochage de la concentration de similarite a 1
par(mfrow = c(3, 3))
for (i in 1:ncol(M_FM)) {
  hist(M_FM[, i], 
       breaks = seq(min(M_FM), max(M_FM), length.out = 100),
       main = paste0("k=", i+1), 
       xlab = "Fowlkes-Mallows Index", ylab = "")
}







# _d. Creation des classes ####

# Decoupe de l'arbre apr?s choix
classes <- cutree(arbre, 3)
table(classes)


# Decoupe par k-means en gardant le meme nombre de clusters
classes_kmeans <- kmeans(ACM$ind$coord, 3)$cluster
table(classes_kmeans)

# Consolidation par k-means en renseiignant les centres de la CAH comme point de d?part
by(ACM$ind$coord, classes, colMeans)
classes_conso <- kmeans(ACM$ind$coord, matrix(unlist(by(ACM$ind$coord, classes, colMeans)), nrow = 3, byrow = TRUE))$cluster
table(classes_conso)



# _e. Visualisation dans l'ACM ####

# Uniquement les individus
  # Version ellipses
fviz_mca_ind(ACM, geom = "point", habillage = as.factor(classes), addEllipses = TRUE)
fviz_mca_ind(ACM, geom = "point", habillage = as.factor(classes_kmeans), addEllipses = TRUE)
fviz_mca_ind(ACM, geom = "point", habillage = as.factor(classes_conso), addEllipses = TRUE)
  # Version polygone au plus pret
fviz_cluster(HCPC(ACM, graph = F, min = 3, max = 5), 
             geom = "point", main = "Factor map", ggtheme = theme_minimal()) +
  scale_colour_manual(values = c("lightblue3", "greenyellow", "tomato")) +
  scale_fill_manual(values = c("lightblue3", "greenyellow", "tomato"))

# Avec les variables par dessus), addEllipses = TRUE, col.var = "black", repel = TRUE)




# Graphique en 3D
mat <- ACM$ind$coord
p <- plot_ly() %>%
  add_trace(type = "scatter3d", mode = "markers",
            x = mat[,1], y = mat[,2], z = mat[,3],
            #color = classes, colors = c("red3", "green3", "blue3"),
            color = factor(classes), #name = "Groupes",
            size = 10)
for (i in 1:nrow(mat)) {
  p <- p %>%
    add_trace(
      type = "scatter3d", mode = "lines",
      x = c(mat[i, 1], mat[i, 1]),
      y = c(mat[i, 2], mat[i, 2]),
      z = c(mat[i, 3], min(mat[, 3])),
      color = factor(classes)[i]
    )
}
p <- p %>% config(p, displayModeBar = FALSE) %>%
  layout(p, scene = list(xaxis = list(title = paste0(colnames(mat)[1], " (", round(ACM$eig[1, 2], 2), "%)")),
                         yaxis = list(title = paste0(colnames(mat)[2], " (", round(ACM$eig[2, 2], 2), "%)")),
                         zaxis = list(title = paste0(colnames(mat)[3], " (", round(ACM$eig[3, 2], 2), "%)")),
                         aspectmode = "data"),
         margin = list(l = 0, r = 0, b = 0, t = 0))
p




# V. Description des populations ---------------------------------------------

DATA_ACM$classes <- factor(classes)

# Bivarie sur les variables supplementaires de l'ACM
DATA_ACM %>%
  dplyr::select(1:length(vars_sup)) %>%
  mutate(classes = classes) %>%
  tbl_summary(by = classes)






