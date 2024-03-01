rm(list = ls())


# Import ------------------------------------------------------------------
load(file = "C:/Users/h.marthinet.CESPA1/Documents/Codes/00 - Codes & Scripts/99 - Rdata/RegLog Multinomiale.Rdata")

library(nnet)
library(gtsummary)
library(car)
library(GGally)
library(breakDown)
library(ggplot2)
library(broom)
library(broom.helpers)
library(questionr)
library(forestmodel)
library(effects)
library(ggeffects)
library(MASS)
library(finalfit)
library(glmulti)
library(tab)
library(stringr)
library(aod)
library(lmtest)
library(DescTools)


# I. Premiere regression --------------------------------------------------

# Binarisation du satut de covid pour une regression logistique simple
DATA1 <- na.exclude(DATA_mod[DATA_mod$Statut == "Militaire d’active",])
DATA1$Covid19 <- factor(DATA1$Covid19,
                        levels = levels(DATA1$Covid19),
                        labels = c("Negatif", "Positif", "Positif"))

# Premier modele complet
reg <- glm(Covid19 ~ Age + Sexe + Grade + Lieu + Armee + Situation, 
           data = DATA1, family = binomial(logit))
reg


# _a. Tableaux ####

# Summary
summary(reg)
vif(reg)
# Coefficient du modele
exp(coef(reg))

# tabglm(reg, columns = c("or", "orci", "p")) %>%
#   mutate(Variable = str_replace_all(Variable, "\\\\ \\\\ \\\\ ", "  ")) %>%
#   dplyr::filter(Variable != "Intercept")

# Tableau resume
reg %>%
  tbl_regression(
    exponentiate = TRUE,
    pvalue_fun = ~ style_pvalue(.x, digits = 3)
  ) %>%
  add_global_p(keep = TRUE) %>%
  as.data.frame()




# _b. Graphiques ####


# Graphiques des OR
ggcoef_model(reg, exponentiate = TRUE)
or_plot(DATA1, "Covid19", c("Age", "Sexe", "Grade", "Lieu", "Armee", "Situation"))
forest_model(reg)


# Graphique des effets
plot(allEffects(reg))
cowplot::plot_grid(plotlist = plot(ggeffect(reg)))



# _c. Autres informations ####


# Matrice de confusion
table(
  predict(reg, type = "response", newdata = DATA1) > 0.5, 
  DATA1$Covid19
)

# Les VIF
vif(reg)

# P-valeurs par variable
Anova(reg)

# A minimiser
AIC(reg)
BIC(reg)
# A maximiser
logLik(reg)
# A tendre vers 1
deviance(reg)/df.residual(reg)
# Test goodness of fit
pchisq(deviance(reg), df.residual(reg))







# II. Selection de variables ####

# _a. Procedure stepwise ####

# __1. Step AIC ####

# Step backward
reg2 <- step(reg)
# Both
reg2 <- step(reg, direction = "both")
# Pour les type de modele ou step() n'existe pas :
# reg2 <- stepAIC(reg)
summary(reg2)
Anova(reg2)
AIC(reg2)

# __2. Step BIC ####

reg3 <- step(reg, k = log(nrow(model.matrix(reg))))
summary(reg3)

# __3. Step a la main ####

Anova(reg)
reg4 <- glm(Covid19 ~ Age + Sexe + Grade + Armee + Situation, 
           data = DATA1, family = binomial(logit))
Anova(reg4)
AIC(reg4)
reg4 <- glm(Covid19 ~ Age + Sexe + Armee + Situation, 
            data = DATA1, family = binomial(logit))
Anova(reg4)
AIC(reg4)

# _b. Les interactions ####


reg5 <- glm(Covid19 ~ Age + Grade + Armee + Age * Sexe, 
            data = DATA1, 
            family = binomial(logit))
tbl_regression(reg5, exponentiate = TRUE)
Anova(reg5)


plot(allEffects(reg5))
plot(ggeffect(reg5, c("Age", "Sexe")))

#Si une interaction est presente dans le modele
#Il faut faire un modele par strate
reg5_H <- glm(Covid19 ~ Age + Grade + Armee, 
              data = DATA1[DATA1$Sexe == "Homme",], 
              family = binomial(logit))
tbl_regression(reg5_H, exponentiate = TRUE)

reg5_F <- glm(Covid19 ~ Age + Grade + Armee, 
              data = DATA1[DATA1$Sexe == "Femme",], 
              family = binomial(logit))
tbl_regression(reg5_F, exponentiate = TRUE)






# III. Comparaison de modeles ####

# _a. Informations des modeles ####

# p-valeurs des variables
Anova(reg)
Anova(reg4)

# Les AIC
AIC(reg)
AIC(reg4)

# Les vraisemblances
logLik(reg)
logLik(reg4)





# _b. Tests de comparaison ####

# __1. Vraisemblance ####

# H0 : Pas de difference dans l'explication de la variance entre les deux modeles
# Deux fonctions equivalentes
anova(reg, reg4, test = "Chisq") 
lrtest(reg, reg4)
# Ici, eme explication de variance pour le modele reduit tout en etant plus parcimonieux


# On peut aussi tester la validité du modele contre le modele vide
reg_vide <- glm(Covid19 ~ 1, 
                data = DATA1, family = binomial(logit))
anova(reg_vide, reg, test = "Chisq")
# Le modele reduit est mieux que le modele vide





# __2. Test de Wald ####

# H0 : Les coefficient du modele sont nuls
wald.test(Sigma = vcov(reg), b = coef(reg), Terms = 1:4)



# __3. Pseudo R2 ####

# Peudo R2 de MsFadden
# Part de la variance de Y expliquee par le modele
PseudoR2(reg)






# _c. Graphiques ####

# Les coefficients
ggcoef_compare(
  list("Modele complet" = reg, "Modele reduit" = reg4),
  exponentiate = TRUE
)
ggcoef_compare(
  list("Modele complet" = reg, "Modele reduit" = reg4),
  type = "faceted",
  exponentiate = TRUE
)

# Les effets
plot(allEffects(reg))
plot(allEffects(reg4))



#glmulti::glmulti(reg4)






# IV. Sorties finale ####

# _a. Indicateurs ####

# __1. FE et FP ####

coefs <- exp(coef(reg4))

# FE : Fraction etiologique
# Utilise quand les OR sont superieur a 1
# Part de l'imputabilite du facteur sur la maladie
# Proportion des cas qui aurait pu etre evite si le fateur etait supprime
(coefs - 1)/coefs

# FP : Fraction preventive
# Utilise quand les OR sont inferieurs a 1
# Proportion des cas evites par le facteur
1-coefs





# _b. Tableaux resumes ####
# __1. gtsummary ####

theme_gtsummary_language("fr", decimal.mark = ",", big.mark = " ")

# Univarie
tbl_u <- DATA1 %>%
  dplyr::select("Covid19", "Age", "Sexe", "Grade", "Lieu", "Armee", "Situation") %>%
  tbl_uvregression(
    method = glm,
    y = Covid19,
    method.args = list(family = binomial),
    exponentiate = TRUE,
    pvalue_fun = ~ style_pvalue(.x, digits = 3)
  ) %>%
  add_global_p(keep = TRUE)

# Multivarie complet
tbl_m_c <- reg %>%
  tbl_regression(
    exponentiate = TRUE,
    pvalue_fun = ~ style_pvalue(.x, digits = 3)
  ) %>%
  add_global_p(keep = TRUE)

# Multivarie reduit
tbl_m_r <- reg4 %>%
  tbl_regression(
    exponentiate = TRUE,
    pvalue_fun = ~ style_pvalue(.x, digits = 3)
  ) %>%
  add_global_p(keep = TRUE)


# Combinaisons des tableaux
tbl_mc_mr <- tbl_merge(
  list(tbl_m_c, tbl_m_r),
  tab_spanner = c("**Modele complet**", "**Modele reduit**")
)

tbl_u_mr <- tbl_merge(
  list(tbl_u, tbl_m_r),
  tab_spanner = c("**Univarie**", "**Multivarie**")
)

tbl_u_mc_mr <- tbl_merge(
  list(tbl_u, tbl_m_c, tbl_m_r),
  tab_spanner = c("**Univarie**", "**Multivarie Complet**", "**Multivarie Reduit**")
) %>% as.data.frame()

# Possibilite d'imprimer apres conversion en tibble
openxlsx::write.xlsx(as_tibble(tbl_u_mc_mr), ...)


# __2. finalfit ####


dep <- "Covid19"
vars <- c("Age", "Sexe", "Grade", "Lieu", "Armee", "Situation")
vars_multi <- c("Age", "Sexe", "Armee", "Situation")


tab <- finalfit(DATA1, dep, vars, explanatory_multi = vars_multi)



# c. Graphiques

# Graphiques des OR
ggcoef_model(reg, exponentiate = TRUE)
or_plot(DATA1, "Covid19", c("Age", "Sexe", "Grade", "Lieu", "Armee", "Situation"))
forest_model(reg)














#### END ####
# 
# 
# 
# 
# 
# 
# logit <- function(x) exp(x) / (1 + exp(x))
# nouvelle_observation <- d[1, ]
# nouvelle_observation$sexe[1] <- "Femme"
# nouvelle_observation$grpage[1] <- "[45,65)"
# nouvelle_observation$etud[1] <- "Primaire"
# nouvelle_observation$relig[1] <- "Pratiquant regulier"
# nouvelle_observation$heures.tv[1] <- 0
# plot(
#   broken(mod2, nouvelle_observation, predict.function = betas),
#   trans = logit
# ) + ylim(0, 1) + ylab("Probabilite de faire du sport")
# 
# 
# 
# nouvelle_observation$sexe[1] <- "Homme"
# nouvelle_observation$grpage[1] <- "[16,25)"
# plot(
#   broken(mod2, nouvelle_observation, predict.function = betas),
#   trans = logit
# ) + ylim(0, 1) + ylab("Probabilite de faire du sport")
# 
# 
# 
# nouvelle_observation$grpage[1] <- "[45,65)"
# plot(
#   broken(mod2, nouvelle_observation, predict.function = betas),
#   trans = logit
# ) + ylim(0, 1) + ylab("Probabilite de faire du sport")
# 
# 
# 
# 
# mod3 <- glm(sport ~ sexe:grpage + etud + heures.tv + relig, data = d, family = binomial())
# tbl_regression(mod3, exponentiate = TRUE)
# 
# anova(mod2, mod3, test = "Chisq")
# plot(allEffects(mod3))
# ggcoef_model(mod3, exponentiate = TRUE)
# 
# 
# 
# plot(
#   broken(mod3, nouvelle_observation, predict.function = betas),
#   trans = logit
# ) + ylim(0, 1) + ylab("Probabilite de faire du sport")
# 
# 
# 
# mod4 <- glm(sport ~ sexe * etud + grpage + heures.tv + relig, data = d, family = binomial())
# 
# plot(allEffects(mod4))
# 
# plot(ggeffect(mod4, c("etud", "sexe")))
# ggcoef_model(mod4, exponentiate = TRUE)
# tbl_regression(mod4, exponentiate = TRUE)
# anova(mod4, test = "Chisq")
# 
# 
# 
# 
# 
# 
# 
# 
# glmulti(sport ~ sexe + grpage + etud + heures.tv + relig, data = d, family = binomial())
# 
# best <- glm(sport ~ 1 + sexe + grpage + etud + heures.tv + grpage:sexe + sexe:heures.tv, data = d, family = binomial())
# odds.ratio(best)
# ggcoef_model(best, exponentiate = TRUE)
# plot(allEffects(best))
# 
# 
# 
# 
# vif(mod)







# Regression logistique multinomiale ####
# Modeles
reg_full <- multinom(formule, data = DATA_mod, family = binomial)


reg_final <- step(reg_full, direction = "both", trace = FALSE)


# Comparaisons
tidy(reg_full, exponentiate = TRUE)
tidy(reg_final, exponentiate = TRUE)
Anova(reg_full)
Anova(reg_final)
lmtest::lrtest(reg_full, reg_final)

# Sorties
labels_mod <- c(labs_socio_demo, labs_psycho)[c(vars_socio_demo, vars_psycho) %in% colnames(reg_final$model)[-1]]
names(labels_mod) <- c(vars_socio_demo, vars_psycho)[c(vars_socio_demo, vars_psycho) %in% colnames(reg_final$model)[-1]]
#ggcoef_multinom(reg_final, exponentiate = TRUE)
multi_cluster <- tidy(reg_final, exponentiate = TRUE, conf.int = TRUE)
multi_cluster <- as.data.frame(multi_cluster[, c(1, 2 ,3, 7, 8, 6)])
multi_cluster$OR <- paste0(formatC(multi_cluster$estimate, digits = 2, format = "f"),
                           " [", formatC(multi_cluster$conf.low, digits = 2, format = "f"),
                           "-", formatC(multi_cluster$conf.high, digits = 2, format = "f"), "]")
multi_cluster




regm_mili <- multinom(Covid19 ~ Age + Sexe + Grade + Lieu + Armee + Situation, 
                      data = DATA_mod[DATA_mod$Statut == "Militaire d'active",])

# Resumes
  summary(regm_mili)
  odds.ratio(regm_mili)
  
  # En tiddle
  tidy(regm_mili, exponentiate = T, conf.int = TRUE)
  
  # En image dans le Viewer 
  tbl_regression(regm_mili, exponentiate = TRUE)

# Controles
Anova(regm_mili)
vif(regm_mili)
table(predict(regm_mili, newdata = DATA_mod), DATA_mod$Covid19)


# Graph d'OR par modalités
a <- tidy(regm_mili, exponentiate = T, conf.int = TRUE)
b <- a[a$term != "(Intercept)",]

b$var <- NA
vars <- c("Age", "Sexe", "Grade", "Lieu", "Armee", "Situation")
for (i in 1:nrow(b)) {
  for (j in 1:length(vars)) {
    if (str_detect(b$term[i], vars[j])) {
      b$term[i] <- str_replace(b$term[i], vars[j], "")
      b$var[i] <- vars[j]
    }
  }
}

ggplot(b,
       aes(
         x = term, 
         y = estimate, 
         ymin = conf.low, 
         ymax = conf.high, 
         color = y.level)
  ) + 
  geom_hline(
    yintercept = 1, 
    color = "gray25", 
    linetype = 2
  ) + 
  geom_errorbar(
    position = position_dodge(0.5), 
    width = 0
  ) + 
  geom_point(
    position = position_dodge(width = 0.5)
  ) + 
  scale_y_log10() +
  facet_grid(
    rows = vars(var),
    scales = "free",
    switch = "both"
  ) +
  coord_flip() +
  labs(y = "Odds ratio", 
    x = "", 
    title = "",
    subtitle = "",
    color = "Covid long") +
  theme_classic() +
  theme(
    panel.grid.major.y = element_line(colour = "lightgray")
  )

