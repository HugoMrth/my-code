###########START###############

library(tidyverse, quietly = TRUE)
library(tidyr)
library(labelled)
library(viridis)
library(survival)
library(TraMineR, quietly = TRUE)
library(WeightedCluster, quietly = TRUE)
library(factoextra)
library(gtsummary)
library(GGally)
library(nnet)
library(ggeffects)
library(lcmm)



### LINK

# https://larmarange.github.io/analyse-R/trajectoires-de-soins.html
# https://larmarange.github.io/analyse-R/trajectoires-de-soins.html
# https://larmarange.github.io/analyse-R/trajectoires-de-soins.html
# https://larmarange.github.io/analyse-R/trajectoires-de-soins.html
# https://larmarange.github.io/analyse-R/trajectoires-de-soins.html
# https://larmarange.github.io/analyse-R/trajectoires-de-soins.html
# https://larmarange.github.io/analyse-R/trajectoires-de-soins.html


#### I. Data ####

get_script_file_path <- function() {
  dp <- rstudioapi::getSourceEditorContext()$path
  vec_slash <- gregexpr("/", dp)[[1]]
  return(substring(dp, 1, vec_slash[length(vec_slash)] - 1))
}

load(paste0(get_script_file_path(), "/Care Trajectories Analysis.Rdata"))

care_trajectories <- as_tibble(care_trajectories)
head(care_trajectories)

# Codebook
look_for(care_trajectories)

# Labelling
var_label(care_trajectories$sex) <- "Sexe"
val_labels(care_trajectories$sex) <- c(homme = 0, femme = 1)
var_label(care_trajectories$age) <- "Âge"
var_label(care_trajectories$education) <- "Education"
val_labels(care_trajectories$education) <- c(
  primaire = 1,
  secondaire = 2,
  supérieur = 3
)


#### II. Exploration ####

#### __a. Plots ####

# Not every patients has the same sequence length
# They're not all diagnosed at the same time
# There should not be any holes in the sequence

# Number of diagnosis per month
ggplot(care_trajectories) +
  aes(x = month) +
  geom_bar()

# Same with color pet state
n <- care_trajectories %>%
  filter(month %in% (0:8*6)) %>%
  group_by(month) %>%
  count() %>%
  pluck("n")
etiquettes <- paste0("M", 0:8*6, "\n(n=", n, ")")
val_labels(care_trajectories$care_status) <- c(
  "diagnostiqué, mais pas suivi" = "D",
  "suivi, mais pas sous traitement" = "C",
  "sous traitement, mais infection non contrôlée" = "T",
  "sous traitement et infection contrôlée" = "S"
)
ggplot(care_trajectories) +
  aes(x = month, fill = to_factor(care_status)) +
  geom_bar(color = "gray50", width = 1) +
  scale_x_continuous(breaks = 0:8*6, labels = etiquettes) +
  ggtitle("Distribution du statut dans les soins chaque mois") +
  xlab("") + ylab("") +
  theme_light() +
  theme(legend.position = "bottom") +
  labs(fill = "Statut dans les soins") + 
  scale_fill_viridis(discrete = TRUE, direction = -1) +
  guides(fill = guide_legend(nrow = 2))



# Cascade de soins
# Same plot with proportions instead of frequencies
ggplot(care_trajectories) +
  aes(x = month, fill = to_factor(care_status)) +
  geom_bar(color = "gray50", width = 1, position = "fill") +
  scale_x_continuous(breaks = 0:8*6, labels = etiquettes) +
  scale_y_continuous(labels = scales::percent) +
  ggtitle("Cascade des soins observée, selon le temps depuis le diagnostic") +
  xlab("") + ylab("") +
  theme_light() +
  theme(legend.position = "bottom") +
  labs(fill = "Statut dans les soins") + 
  scale_fill_viridis(discrete = TRUE, direction = -1) +
  guides(fill = guide_legend(nrow = 2))


# Truncated version because of the lack of N after month no 36
casc_obs <- ggplot(care_trajectories %>% filter(month <= 36)) +
  aes(x = month, fill = to_factor(care_status)) +
  geom_bar(color = "gray50", width = 1, position = "fill") +
  scale_x_continuous(breaks = 0:8*6, labels = etiquettes) +
  scale_y_continuous(labels = scales::percent) +
  ggtitle("Cascade des soins observée, selon le temps depuis le diagnostic") +
  xlab("") + ylab("") +
  theme_light() +
  theme(legend.position = "bottom") +
  labs(fill = "Statut dans les soins") + 
  scale_fill_viridis(discrete = TRUE, direction = -1) +
  guides(fill = guide_legend(nrow = 2))
casc_obs



#### __b. Survival Analysis ####

# We first focus on the first entry in care, first initiation of treatment and first infection control
# Then calculate the dates of those events
ind <- care_trajectories %>% filter(month == 0)
ind$diagnostic <- 0
ind <- ind %>%
  left_join(
    care_trajectories %>%
      filter(care_status %in% c("C", "T", "S")) %>%
      group_by(id) %>%
      dplyr::summarise(entree_soins = min(month)),
    by = "id"
  ) %>%
  left_join(
    care_trajectories %>%
      filter(care_status %in% c("T", "S")) %>%
      group_by(id) %>%
      dplyr::summarise(initiation_tt = min(month)),
    by = "id"
  ) %>%
  left_join(
    care_trajectories %>%
      filter(care_status == "S") %>%
      group_by(id) %>%
      dplyr::summarise(controle = min(month)),
    by = "id"
  )


# Time variable
ind <- ind %>%
  left_join(
    care_trajectories %>%
      group_by(id) %>%
      dplyr::summarise(suivi = max(month)),
    by = "id"
  )



# Kaplan Meier Curve computation
# In this estimation, we do not consider it a possibility
# That patient can progress in reverse from the second state to the first one


# Given an origin and an event
km <- function(date_origine, date_evenement, nom) {
  # ne garder que les observations avec date d'origine
  tmp <- ind[!is.na(ind[[date_origine]]), ]
  
  # pre-remplir la variable time avec duree de suivi
  # depuis date d'origine
  tmp$time <- tmp$suivi - tmp[[date_origine]]
  # et considérer que l'événement n'a pas été vécu
  tmp$event <- FALSE
  
  # si date_evement documentée, événement vécu
  tmp[!is.na(tmp[[date_evenement]]), ]$event <- TRUE
  tmp[tmp$event == TRUE, ]$time <- 
    tmp[tmp$event == TRUE, ][[date_evenement]] -
    tmp[tmp$event == TRUE, ][[date_origine]]
  
  kaplan <- survfit(Surv(time, event) ~ 1, data = tmp)
  res <- broom::tidy(kaplan, conf.int = TRUE)
  res$nom <- nom
  res
}



# Time from diagnosis
depuis_diag <- dplyr::bind_rows(
  km("diagnostic", "entree_soins", "entrée en soins"),
  km("diagnostic", "initiation_tt", "initiation du traitement"),
  km("diagnostic", "controle", "contrôle de l'infection")
)

g_diag <- ggplot(data = depuis_diag) +
  aes(x = time, y = 1 - estimate, 
      color = as_factor(nom), fill = as_factor(nom),
      ymin = 1 - conf.high, ymax = 1 - conf.low) +
  geom_ribbon(alpha = .25, mapping = aes(color = NULL)) +
  geom_line(size = 1) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    panel.grid.major.y = element_line(colour = "grey")
  ) +
  scale_x_continuous(breaks = 0:6*6, limits = c(0, 36)) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  xlab("mois depuis le diagnostic") + 
  ylab("") + labs(color = "", fill = "")
g_diag


# Time from previous state
depuis_prec <- dplyr::bind_rows(
  km("diagnostic", "entree_soins", "entrée en soins"),
  km("entree_soins", "initiation_tt", "initiation du traitement"),
  km("initiation_tt", "controle", "contrôle de l'infection")
)

g_prec <- ggplot(data = depuis_prec) +
  aes(x = time, y = 1 - estimate, 
      color = as_factor(nom), fill = as_factor(nom),
      ymin = 1 - conf.high, ymax = 1 - conf.low) +
  geom_ribbon(alpha = .25, mapping = aes(color = NULL)) +
  geom_line(size = 1) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    panel.grid.major.y = element_line(colour = "grey")
  ) +
  scale_x_continuous(breaks = 0:6*6, limits = c(0, 36)) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  xlab("mois depuis l'étape précédente") + 
  ylab("") + labs(color = "", fill = "")

g_prec


# Time from previous event per event
g_prec + facet_grid(~ as_factor(nom))






#### III. Sequence analysis ####

#### __a. Full time series ####

# long to wide
large <- care_trajectories %>%
  dplyr::select(id, m = month, care_status) %>%
  pivot_wider(names_from = m, values_from = care_status, names_prefix = "m") 
head(large)

# sequences creation
seq_all <- seqdef(
  large %>% dplyr::select(m0:m50),
  id = large$id,
  alphabet = c("D", "C", "T", "S"), # states order
  states = c("diagnostiqué", "en soins", "sous traitement", "inf. contrôlée"), # labels
  cpal = viridis(4, direction = -1)
)


# proportion of each state per month (same as before)
seqdplot(seq_all, legend.prop = .25)


# costs
# Costs should not be constant anymore
# Indel costs shuold be half of the maximum substitution costs
(couts <- seqcost(seq_all, method = "CONSTANT"))
couts$sm[1, ] <- c(0, 1, 2, 3)
couts$sm[2, ] <- c(1, 0, 1, 2)
couts$sm[3, ] <- c(2, 1, 0, 1)
couts$sm[4, ] <- c(3, 2, 1, 0)
couts$indel <- max(couts$sm) / 2
couts

# Distances
dist_all <- seqdist(seq_all, method = "OM", sm = couts$sm, indel = couts$indel)


# Tapis / Index plot w/ dendrogram
arbre_all <- hclust(as.dist(dist_all), method = "ward.D2")
library(seqhandbook, quietly = TRUE)
seq_heatmap(seq_all, arbre_all)


# Sequences groups are moslty separated given their length
# Lots of patient are lost after month 18





#### __b. Restricted time period ####

# basically the same analysis 
# caped at 18 month so most sequence are full
large$seq_length <- seqlength(seq_all)
large_m18 <- large %>%
  dplyr::filter(seq_length >= 19) %>%
  dplyr::select(id:m18)

seq_m18 <- seqdef(
  large_m18 %>% dplyr::select(m0:m18),
  id = large_m18$id,
  alphabet = c("D", "C", "T", "S"),
  states = c("diagnostiqué", "en soins", "sous traitement", "inf. contrôlée"),
  cpal = viridis(4, direction = -1)
)
dist_m18 <- seqdist(seq_m18, method = "OM", sm = couts$sm, indel = couts$indel)
arbre_m18 <- hclust(as.dist(dist_m18), method = "ward.D2")
seq_heatmap(seq_m18, arbre_m18)


#### __c. Clustering ####

# NEXT TWO LINES RECQUIRE Rgraphviz TO RUN
# INSTALL BELOW
    # if (!requireNamespace("BiocManager", quietly = TRUE))
    #   install.packages("BiocManager")
    # BiocManager::install("Rgraphviz")
seqtree_m18 <- as.seqtree(arbre_m18, seqdata = seq_m18, diss = dist_m18, ncluster = 7)
seqtreedisplay(seqtree_m18, type="I", border=NA, show.depth=TRUE)





# Cut the tree
large_m18$typo_cah <- cutree(arbre_m18, 4)
# Choose the number of clusters
nc <- as.clustrange(arbre_m18, dist_m18)
summary(nc, max.rank = 5)
plot(nc, norm = "zscore")




# Try and improve the selected partition using de KMedoids
# PAM - Partitioning Around Medoids
pam_m18 <- wcKMedoids(dist_m18, k = 4, initialclust = arbre_m18)
large_m18$typo_pam <- pam_m18$clustering
# Changse from one version to another
large_m18 %>% 
  tbl_cross(row = typo_cah, col = typo_pam)




# Groups of sequnces depeding on the chosen partition
large_m18$ordre_cmd <- cmdscale(as.dist(dist_m18), k = 1)
seqIplot(seq_m18, group = large_m18$typo_cah, sortv = large_m18$ordre_cmd)
seqIplot(seq_m18, group = large_m18$typo_pam, sortv = large_m18$ordre_cmd)


# Comparison of judging criteria between he two partition
# as they're visually very similar
# Criteria should be as high as possible
tab <- tibble(
  stat = names(wcClusterQuality(dist_m18, large_m18$typo_cah)$stats),
  cah = wcClusterQuality(dist_m18, large_m18$typo_cah)$stats,
  pam = wcClusterQuality(dist_m18, large_m18$typo_pam)$stats
)
gt::gt(tab) %>% gt::fmt_number(2:3, decimals = 2, sep_mark = " ")
# We now seldect the PAM partition




seqIplot(seq_m18, group = large_m18$typo_pam, sortv = large_m18$ordre_cmd)
# Nous voyons émerger quatre groupes distincts :
#   les rapides, qui entrent en soins et initient le traitement dès les premiers mois suivant le diagnostic ;
#   les lents, qui entrent en soins et initient le traitement plus tardivement, après M6 ;
#   les inaboutis, qui entrent en soins mais n’initient pas le traitement ;
#   les hors soins, qui ne sont pas entrés en soins où n’y sont pas restés.




# ggplot version of the index plots
# nommer les groupes
large_m18$groupe <- factor(
  large_m18$typo_pam,
  c(85, 23, 410, 6),
  c("Rapides", "Lents", "Inaboutis", "Hors soins")
)

# calculer le rang des individus dans chaque groupe
large_m18 <- large_m18 %>%
  group_by(groupe) %>%
  arrange(ordre_cmd) %>%
  mutate(rang_cmd = rank(ordre_cmd, ties.method = "first"))

# créer un fichier long
long_m18 <- care_trajectories %>%
  filter(id %in% large_m18$id & month <= 18) %>%
  left_join(
    large_m18 %>% dplyr::select(id, groupe, rang_cmd),
    by = "id"
  )

long_m18$care_statusF <- to_factor(long_m18$care_status)

# calculer les effectifs par groupe
tmp <- large_m18 %>% 
  dplyr::count(groupe) %>%
  mutate(groupe_n = paste0(groupe, "\n(n=", n, ")"))

long_m18 <- long_m18 %>%
  left_join(
    tmp %>% dplyr::select(groupe, groupe_n),
    by = "groupe"
  )

# graphique des tapis de séquences
ggplot(long_m18) +
  aes(x = month, y = rang_cmd, fill = care_statusF) +
  geom_raster() +
  facet_grid(groupe_n ~ ., space = "free", scales = "free") +
  scale_x_continuous(breaks = 0:6*3, labels = paste0("M", 0:6*3)) +
  scale_y_continuous(breaks = 0:5*100, minor_breaks = NULL) +
  xlab("") + ylab("") +
  theme_light() +
  theme(legend.position = "bottom") +
  labs(fill = "Statut dans les soins") + 
  scale_fill_viridis(discrete = TRUE, direction = -1) +
  guides(fill = guide_legend(nrow = 2))



#### IV. Factors associated to each group ####


#### __a. Groups caracterisation ####

# Group description per socio-demographic factor
large_m18 <- large_m18 %>%
  left_join(
    care_trajectories %>%
      dplyr::filter(month == 0) %>%
      dplyr::select(id, sex, age, education),
    by = "id"
  )
large_m18 %>%
  ungroup() %>%
  unlabelled() %>% 
  tbl_summary(by = "groupe", include = c("groupe", "sex", "age", "education")) %>%
  add_p() %>%
  add_overall(last = TRUE)


# Graphs
res <- tibble()
explanatory <- c(
  "sex" = "Sexe", 
  "age" = "Âge",
  "education" = "Education"
)
for (v in names(explanatory)) {
  tmp <- tibble::as_tibble(table(large_m18$groupe, to_factor(large_m18[[v]])), .name_repair = "unique")
  names(tmp) <- c("groupe", "level", "n")
  test <- chisq.test(large_m18$groupe, to_factor(large_m18[[v]]))
  tmp$var <- paste0(
    explanatory[v],
    "\n",
    scales::pvalue(test$p.value, add_p = TRUE)
  )
  res <- bind_rows(res, tmp)
}
# stat_prop() a besoin d'un facteur
res$level <- factor(res$level)
ggplot(res) +
  aes(x = level, fill = groupe, weight = n) +
  geom_bar(position = "fill") +
  geom_text(
    aes(by = level, label = scales::percent(..prop.., accuracy = 1)), 
    stat = "prop", position = position_fill(.5)
  ) +
  facet_grid(var ~ ., scales = "free", space = "free") +
  scale_y_continuous(labels = scales::percent, breaks = 0:5/5) +
  coord_flip() +
  theme(legend.position = "bottom") +
  xlab("") + ylab("") + labs(fill = "")




#### __b. Multiomiale regression ####


large_m18 <- large_m18 %>% ungroup()
large_m18$groupe2 <- relevel(large_m18$groupe, "Hors soins")
regm <- multinom(
  groupe2 ~ sex + age + education, 
  data = to_factor(large_m18)
)
ggcoef_multinom(regm, exponentiate = TRUE)
cowplot::plot_grid(plotlist = plot(ggeffect(regm)), ncol = 3)


#### __c. Latent Class Mixed Model ####


# care_trajectories <- care_trajectories %>%
#   mutate(
#     num_status = as.integer(to_factor(care_status)),
#     sexF = to_factor(sex),
#     ageF = to_factor(age),
#     educationF = to_factor(education)
#   )
# 
# mod4 <-lcmm(
#   num_status ~ month, random = ~ month, subject = 'id', 
#   mixture = ~ month, ng = 4, idiag = TRUE, data = care_trajectories, 
#   link = "thresholds",
#   B = 
# )
# summary(mod4)