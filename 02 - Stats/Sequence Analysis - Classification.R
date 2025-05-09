library(TraMineR)
library(cluster)
library(seqhandbook)
library(questionr)

### LINK
# https://larmarange.github.io/analyse-R/analyse-de-sequences.html
# https://larmarange.github.io/analyse-R/analyse-de-sequences.html
# https://larmarange.github.io/analyse-R/analyse-de-sequences.html
# https://larmarange.github.io/analyse-R/analyse-de-sequences.html
# https://larmarange.github.io/analyse-R/analyse-de-sequences.html
# https://larmarange.github.io/analyse-R/analyse-de-sequences.html

#### Data ####

get_script_file_path <- function() {
  dp <- rstudioapi::getSourceEditorContext()$path
  vec_slash <- gregexpr("/", dp)[[1]]
  return(substring(dp, 1, vec_slash[length(vec_slash)] - 1))
}

load(paste0(get_script_file_path(), "/Sequences Analysis.Rdata"))
data$generation <- factor(data$generation, labels = c("1930-38", "1939-45", "1946-50"))
str(data)

# State creation
labels <- c("agric", "acce", "cadr", "pint", "empl", "ouvr", "etud", "inact", "smil")
seq <- seqdef(data[, 1:37], states = labels)



#### Optimal matching & clustering ####


# Exemple with fixed costs equals to 2
couts <- seqsubm(seq, method = "CONSTANT", cval = 2)
# Distance matrix
# Insertion Deletion costst also constan and equal to 1
seq.om <- seqdist(seq, method = "OM", indel = 1, sm = couts)
seqdistmc(seq)

# sequences HC
seq.dist <- hclust(as.dist(seq.om), method = "ward.D2")
plot(as.dendrogram(seq.dist), leaflab = "none")

# inertia plot
plot(sort(seq.dist$height, decreasing = TRUE)[1:20], type = "s", xlab = "nb de classes", ylab = "inertie")

# 5 classes clustering
nbcl <- 5
seq.part <- cutree(seq.dist, nbcl)
seq.part <- factor(seq.part, labels = paste("classe", 1:nbcl, sep = "."))


#### Plots ####

# chronogrammes / state distribution plots) 
# Transversal cuts serie
# proportion of people in each state at each age
seqdplot(seq, group = seq.part, xtlab = 14:50, border = NA)

# Tapis / Index plots
# Every sequences of the different groups
seqIplot(seq, xtlab = 14:50, space = 0, border = NA, yaxis = FALSE)
seqIplot(seq, group = seq.part, xtlab = 14:50, space = 0, border = NA, yaxis = FALSE)

# Sorted index plots (multidimensionnal scaling)
ordre <- cmdscale(as.dist(seq.om), k = 1)
seqIplot(seq, group = seq.part, sortv = ordre, xtlab = 14:50, space = 0, border = NA, yaxis = FALSE)


# Index plot of every sequences ordered according t the dendrogram
seq_heatmap(seq, seq.dist, labCol = 14:50)



#### Additionnal plots ####

# average distance of the sequcens from the center of the group
aggregate(disscenter(as.dist(seq.om), group = seq.part), list(seq.part), mean)

# Most common sequences in each group
seqfplot(seq, group = seq.part)

# Most represented state at each age for each group
seqmsplot(seq, group = seq.part, xtlab = 14:50, main = "classe")

# Average time spent in each state
seqmtplot(seq, group = seq.part)

# Some reprensentative sequences in each group
seqrplot(seq, group = seq.part, dist.matrix = seq.om, criterion = "dist")

# Group entropy through time
#the lower the most similar the group at a given time
seqHtplot(seq, group = seq.part, xtlab = 14:50)



#### Typology ####

# Transition rate matrix
round(seqtrate(seq), 2)

# Transversal state distribution
seqstatd(seq)

# TOtal time spent in each state
# aka table
seqistatd(seq[1:10, ])


# Entropy
seqient(seq)
# Turbulence
seqST(seq)

# Groups description
freq(seq.part)


# Description per generation
cprop(table(seq.part, data$generation))
chisq.test(table(seq.part, data$generation))















#### Case Study ####


# Creation de la table de trajectoire pour les analyses
# Depuis la table brutes extraites du SNDS



#### __Trajectoires de deces ####
# Je commence par censurer les individus decedes
# Non taguer dans la table de trajectoire, ils restent en "PSTT"
# Jointure avec la table patient pour les passer en "DC"

# Identifiants unique des décédés 
iter <- unique(BDD_TRAJ_EXPOSES$BEN_IDT_ANO[BDD_TRAJ_EXPOSES$BEN_IDT_ANO %in%
                                       data$BEN_IDT_ANO[data$deces_suivi_ON == "Oui"]])
for (i in iter) { # Boucle sur les ids
  # Selection de la dernière lignede sequence
  rowSelect <- BDD_TRAJ_EXPOSES$BEN_IDT_ANO == i & # pour le ieme deces
    BDD_TRAJ_EXPOSES$period_num == max(BDD_TRAJ_EXPOSES$period_num[BDD_TRAJ_EXPOSES$BEN_IDT_ANO == i]) # derniere periode
  
  # Si date de deces egale à la date de debut de periode
  if(as.Date(data[data$BEN_IDT_ANO == i, "date_dc"][!is.na(data[data$BEN_IDT_ANO == i, "date_dc"])]) == 
     pull(BDD_TRAJ_EXPOSES[rowSelect, "date_debut_epid"] - 1)) {
    # update de l'etat
    BDD_TRAJ_EXPOSES[rowSelect, "groupe_n1"] <- "DC"
  } else {
    # Remplacement des valeurs
    # backup date de fin avant remplacement (pas toujours la meme si cas ou temoin)
    dtFin <- BDD_TRAJ_EXPOSES[rowSelect, "date_fin_epid"]
    # fin de periode  = date de deces moins un jour (pour debut de la prochaine à la date de deces)
    dtFinNew <- data[data$BEN_IDT_ANO == i, "date_dc"][!is.na(data[data$BEN_IDT_ANO == i, "date_dc"])]
    # update du nombre de jours et date de fin
    BDD_TRAJ_EXPOSES[rowSelect, "date_fin_epid"] <- dtFinNew - 1
    BDD_TRAJ_EXPOSES[rowSelect, "nb_jours"] <- as.numeric(BDD_TRAJ_EXPOSES[rowSelect, "date_fin_epid"] - BDD_TRAJ_EXPOSES[rowSelect, "date_debut_epid"]) + 1
    
    # Creation de la periodede deces
    # dans le df des nouvelles lignes
    BDD_TRAJ_EXPOSES <- rbind(BDD_TRAJ_EXPOSES,
                              data.frame(BEN_IDT_ANO = i, # même id
                                         date_debut_epid = dtFinNew, # date de deces
                                         date_fin_epid = pull(dtFin), # recup date de fin de periode
                                         nb_jours = max(as.numeric(pull(dtFin) - dtFinNew + 1), 1),
                                         groupe_n1 = "DC",
                                         period_num = BDD_TRAJ_EXPOSES[rowSelect, "period_num"] + 1
                              )) 
  }
}
BDD_TRAJ_EXPOSES <- BDD_TRAJ_EXPOSES[BDD_TRAJ_EXPOSES$nb_jours != 0,]





#### __Trajectoire uniques par semaine ####

# Aggregation des donnes journalieres en donnees habdo
# Autrement objet trop gros pour etre traite

dataTrajExp <- BDD_TRAJ_EXPOSES %>%
  dplyr::select(BEN_IDT_ANO, period_num, nb_jours, groupe_n1) %>%
  # mutate(nb_jours = round(nb_jours/7, 0)) %>%
  pivot_wider(
    id_cols = BEN_IDT_ANO,
    names_from = period_num,
    values_from = c(nb_jours, groupe_n1)) %>%
  dplyr::select(-BEN_IDT_ANO) %>%
  group_by_all() %>%
  summarise(count = n())

dataTrajExp <- t(apply(dataTrajExp, 1, function(x) {
  c(x[23], rep(x[12:22][!is.na(x[12:22])], x[1:11][!is.na(x[1:11])]))
  }))





#### __Classification ####

# Pour cette partie
# Requis prealable des table de trajectoires de soin mises en page

# Ici, fait une seule fois pour un des groupes d'expossition
# Même analyse à faire une deuxieme fois pour 'lautre groupe
# Cf le tapis en commun ou les deux classification de sequences sont requises

sum(dataTapisExp[, 1])
# Etats au debut de chaque mois
dataTapisExp <- dataTapisExp[, c(1, seq(1, 365, 30)+1)]

# Aggregation des effectifs par mois
dataTapisExp <- dataTapisExp %>%
  group_by_all() %>%
  summarise(V1 = sum(V1))
sum(dataTapisExp[, 1])

#Creation de l'objet de sequences
seqExp <- seqdef(data = dataTapisExp[, 2:14],
                 weights = pull(dataTapisExp[, 1]),
                 cpal = c("lightgray", "lightcoral" ,"cornflowerblue", "mediumseagreen"),
                 cnames = 0:12)


# Exemple with fixed costs equals to 2
couts <- seqsubm(seqExp, method = "CONSTANT", cval = 2)
# Distance matrix
# Insertion Deletion costst also constan and equal to 1
seq.om <- seqdist(seqExp, method = "OM", indel = 1, sm = couts)

# sequences HC
seq.dist <- hclust(as.dist(seq.om), method = "ward.D2")
#plot(as.dendrogram(seq.dist), leaflab = "none")

# inertia plot
plot(sort(seq.dist$height, decreasing = TRUE)[1:10], type = "s", xlab = "nb de classes", ylab = "inertie")

# 4 classes clustering
nbcl <- 4
seq.part <- cutree(seq.dist, nbcl)
seq.part <- factor(seq.part, labels = c("Décès", "Arrêt", 
                                        #"Reprise", 
                                        "Substitution", "Référence"))
seq_cl_exp_4cl <- seq.part
# Graphs pour la classification des sequences de soin 
seqIplot(seqExp, group = seq.part, space = 0, border = NA, yaxis = FALSE,
         ylab = paste("N =", table(rep(seq.part, pull(dataTapisExp[, 1])))), 
         sortv = "from.start")
seqdplot(seqExp, border = NA,
         main = "Répartition des états au cours du temps (exposés)")
seqIplot(seqExp,space=0,border=NA, yaxis=F, cex.legend = 0.58, ncol =3, legend.prop = 0.20, sortv = "from.start")







#### __Tapis deux deux groupes d'exposisiton ####

seq_cl_exp_4cl <- factor(as.character(seq_cl_exp_4cl),
                         levels = levels(seq_cl_tem_4cl),
                         labels = levels(seq_cl_tem_4cl))
# Besoin des deux objets de sequences de soin
levels(seq_cl_tem_4cl) <- paste0(levels(seq_cl_tem_4cl), " (N=",table(rep(seq_cl_tem_4cl, pull(dataTapisTem[, 1]))), ")")
levels(seq_cl_exp_4cl) <- paste0(levels(seq_cl_exp_4cl), " (N=",table(rep(seq_cl_exp_4cl, pull(dataTapisExp[, 1]))), ")")



ggarrange(ggseqiplot(seqTem, # Facet selon la classe du clustering
                     group = seq_cl_tem_4cl,
                     facet_ncol = 1,
                     no.n = TRUE) +
          # Taille des panel proportionnelle aux effectifs
            force_panelsizes(rows = table(rep(seq_cl_tem_4cl, pull(dataTapisTem[, 1])))) +
            theme(
              panel.spacing = unit(0.1, "lines"),
              axis.text.y = element_text(colour = "white"),
              axis.ticks.y = element_line(colour = "white")) +
            labs(y = ""),
          # Deuxieme graph
          ggseqiplot(seqExp, 
                     group = seq_cl_exp_4cl,
                     facet_ncol = 1,
                     no.n = TRUE) +
            force_panelsizes(rows = table(rep(seq_cl_exp_4cl, pull(dataTapisExp[, 1])))) +
            theme(
              panel.spacing = unit(0.1, "lines"),
              axis.text.y = element_text(colour = "white"),
              axis.ticks.y = element_line(colour = "white")) +
            labs(y = ""),
          ncol = 2, nrow = 1, 
          # Legende en commun pour les deux graphs
          common.legend = T, legend = "bottom")


