library(tidyverse)
library(ggpubr)
library(rstatix)
library(datarium)



# I. ANOVA à un facteur sur mesures répétées ####

# _1. Préparation des données ####

# Préparation des données
# Format large
data("selfesteem", package = "datarium")
head(selfesteem, 3)


# Rassembler les colonnes t1, t2 et t3 en format long
# Convertir l'identifiant et le temps en facteurs
selfesteem <- selfesteem %>%
  gather(key = "time", value = "score", t1, t2, t3) %>%
  convert_as_factor(id, time)
head(selfesteem, 3)

# _2. Statistiques descriptives ####

selfesteem %>%
  group_by(time) %>%
  get_summary_stats(score, type = "mean_sd")


# _3. Visualisation ####
bxp <- ggboxplot(selfesteem, x = "time", y = "score", add = "point")
bxp

# _4. Vérifier les hypothèses ####

# __a. Valeurs aberrantes ####


selfesteem %>%
  group_by(time) %>%
  identify_outliers(score)



# __b. Hypothèse de normalité ####
selfesteem %>%
  group_by(time) %>%
  shapiro_test(score)

ggqqplot(selfesteem, "score", facet.by = "time")


# __c. Hypothèse de sphéricité ####
#Automatiquement verifie dans le calul de l'anova


# _5. Calculs ####
res.aov <- anova_test(data = selfesteem, dv = score, wid = id, within = time)
get_anova_table(res.aov)

# comparaisons par paires
pwc <- selfesteem %>%
  pairwise_t_test(
    score ~ time, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc


# _6. Rapporter ####

# Visualisation : Boxplots avec p-values
pwc <- pwc %>% add_xy_position(x = "time")
bxp + 
  stat_pvalue_manual(pwc) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )




# II. ANOVA à deux facteurs sur mesures répétées ####


# _1. Préparation des données ####

# Préparation des données
# Format large
set.seed(123)
data("selfesteem2", package = "datarium")
selfesteem2 %>% sample_n_by(treatment, size = 1)


# Rassemblez les colonnes t1, t2 et t3 en format long.
# Convertir l'identifiant et le temps en facteurs
selfesteem2 <- selfesteem2 %>%
  gather(key = "time", value = "score", t1, t2, t3) %>%
  convert_as_factor(id, time)
# Inspecter quelques lignes aléatoires des données par groupes
set.seed(123)
selfesteem2 %>% sample_n_by(treatment, time, size = 1)

# _2. Statistiques descriptives ####

selfesteem2 %>%
  group_by(treatment, time) %>%
  get_summary_stats(score, type = "mean_sd")


# _3. Visualisation ####
bxp <- ggboxplot(
  selfesteem2, x = "time", y = "score",
  color = "treatment", palette = "jco"
)
bxp

# _4. Vérifier les hypothèses ####

# __a. Valeurs aberrantes ####


selfesteem2 %>%
  group_by(treatment, time) %>%
  identify_outliers(score)



# __b. Hypothèse de normalité ####
selfesteem2 %>%
  group_by(treatment, time) %>%
  shapiro_test(score)

ggqqplot(selfesteem2, "score", ggtheme = theme_bw()) +
  facet_grid(time ~ treatment, labeller = "label_both")


# __c. Hypothèse de sphéricité ####
#Automatiquement verifie dans le calul de l'anova


# _5. Calculs ####
res.aov <- anova_test(
  data = selfesteem2, dv = score, wid = id,
  within = c(treatment, time)
)
get_anova_table(res.aov)

# __a. Procédure pour une interaction significative à deux facteurs ####

# Effet du traitement à chaque instant
one.way <- selfesteem2 %>%
  group_by(time) %>%
  anova_test(dv = score, wid = id, within = treatment) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way



# Comparaisons par paires entre les groupes de traitement
pwc <- selfesteem2 %>%
  group_by(time) %>%
  pairwise_t_test(
    score ~ treatment, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc



# Effet du temps à chaque niveau de traitement
one.way2 <- selfesteem2 %>%
  group_by(treatment) %>%
  anova_test(dv = score, wid = id, within = time) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
# Comparaisons par paires entre les points dans le temps
pwc2 <- selfesteem2 %>%
  group_by(treatment) %>%
  pairwise_t_test(
    score ~ time, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc2


# __b. Procédure pour une interaction non significative à deux facteurs ####



# comparaisons pour la variable traitement
selfesteem2 %>%
  pairwise_t_test(
    score ~ treatment, paired = TRUE, 
    p.adjust.method = "bonferroni"
  )
# comparaisons pour la variable time
selfesteem2 %>%
  pairwise_t_test(
    score ~ time, paired = TRUE, 
    p.adjust.method = "bonferroni"
  )

# _6. Rapporter ####

# Visualisation : Boxplots avec p-values
pwc <- pwc %>% add_xy_position(x = "time")
bxp + 
  stat_pvalue_manual(pwc, tip.length = 0, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )

