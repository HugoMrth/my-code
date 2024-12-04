rm(list = ls())

#### Packages ####
library(survival)
library(survminer)
library(broom)
library(finalfit)

#### Load des donnees ####
load("C:/Users/h.marthinet/Documents/32 - R/Codes & Scripts/Rdata/Survie.Rdata")
DATA <- DATA_surv






#### Mise en place ####

# Définition des variables Temps et Evenement
time <- DATA$`Temps (j)`
event <- as.numeric(DATA$Appareillage == 1)

# Duree de censure
duree_obs <- 90
# Censure
time[event == 0] <- duree_obs
event[time > duree_obs] <- 0

# Premiere visualisation simple
(km_global <- survfit(Surv(time, event) ~ 1, data = DATA))
# fun = "event" --> Probabilité de 'deces'
ggsurvplot(km_global, 
           fun = "event",
           risk.table = TRUE, 
           surv.scale = "percent")
# Rien du tout --> Probabilité de 'survie'
ggsurvplot(km_global, 
           #fun = "event",
           risk.table = TRUE, 
           surv.scale = "percent")

# RMST - Restricted Mean Survival Time
cbind(
    round(summary(km_global, rmean = 30)$table[,3], 2),
    round(summary(km_global, rmean = 60)$table[,3], 2),
    round(summary(km_global, rmean = 90)$table[,3], 2))






#### Selection des variables du modele complet ####
# Par modeles de cox univarie

# Formatage des noms de colonnes pour le as.formula
colnames(DATA)[3:7] <- c("duree.reanimation.jours", "infection.precoce", "nombre.de.membre.ampute",
  "nombre.de.blocs.avant.amputation", "chir.conservatrice.avant.amputation")

# Boucles des modeles
var_signif <- c()
for (i in colnames(DATA)[2:6]) {
  # Cox univarié
  form <- as.formula(paste0("Surv(time, event) ~ ", i))
  mod <- coxph(form, data = DATA)
  # Si pval du modele < 5%
  if (summary(mod)$coefficients[5] < 0.05) {
    # Ajout dans la liste des variables signif
    var_signif <- c(var_signif, i)
  }
}
var_signif






#### Procédure step ####

# Modèle complet
mod_complet <- coxph(
  Surv(time, event) ~ duree.reanimation.jours + infection.precoce + nombre.de.membre.ampute + nombre.de.blocs.avant.amputation,
  data = DATA
)
# Summary avec HR
tidy(mod_complet, exponentiate = TRUE)
Anova(mod_complet)
# Graph avec AIC 
ggforest(mod_complet)


# Step 1
modstep <- coxph(
  Surv(time, event) ~ duree.reanimation.jours + infection.precoce + nombre.de.membre.ampute,
  data = DATA
)
tidy(modstep, exponentiate = TRUE)
Anova(modstep)
ggforest(modstep)

# Step 2
modstep <- coxph(
  Surv(time, event) ~ infection.precoce + nombre.de.membre.ampute,
  data = DATA
)
tidy(modstep, exponentiate = TRUE)
Anova(modstep)
ggforest(modstep)

# Modele final
mod <- coxph(
  Surv(`Temps (j)`, Appareillage) ~ infection.precoce + nombre.de.membre.ampute,
  data = DATA
)


#### Comparaison de models ####
summary(mod_complet)
summary(mod)

AIC(mod_complet)
AIC(mod)

logLik(mod_complet)
logLik(mod)

anova(mod_complet, mod)



#### Resultats modele final ####

# Summaries
summary(mod)
tidy(mod, exponentiate = TRUE)
# Graph HR
ggforest(mod)
# Test validation residus
# On veut p > 0.05 (seulement le global)
ggcoxzph(cox.zph(mod))


# Final fit
(fit_coxph <- finalfit.coxph(DATA,
                            dependent = "Surv(time, event)",
                            explanatory = c("duree.reanimation.jours",
                                            "infection.precoce",
                                            "nombre.de.blocs.avant.amputation",
                                            "chir.conservatrice.avant.amputation",
                                            "nombre.de.membre.ampute"),
                            explanatory_multi = c("infection.precoce",
                                                  "nombre.de.membre.ampute")))





#### Courbes de survie ####

# Simple
ggsurvplot(km_global, pval = TRUE, risk.table = TRUE, surv.scale = "percent", fun = "event",
           xlab = "Délai (j)", ylab = "Probabilité d'appareillage",
           # Breaks du Time
           xlim = c(0, 90), break.x.by = 30,
           risk.table.title = "Nombre d'individus non appareillés",
           # Pour enlever la legende
           legend = "none", legend.title = "")

# A 1 facteur
ggsurvplot(survfit(Surv(DATA$`Temps (j)`, Appareillage) ~ DATA$infection.precoce, data = DATA), 
           pval = TRUE, risk.table = TRUE, surv.scale = "percent", fun = "event", 
           # Par défaut conf.int = FALSE
           conf.int = TRUE,
           xlab = "Délai (j)", ylab = "Probabilité d'appareillage",
           xlim = c(0, 90), break.x.by = 30,
           risk.table.title = "Nombre d'individus non appareillés",
           legend.title = "Groupes")

ggsurvplot(survfit(Surv(DATA$`Temps (j)`, Appareillage) ~ DATA$nombre.de.membre.ampute, data = DATA), 
           pval = TRUE, risk.table = TRUE, surv.scale = "percent", fun = "event", conf.int = TRUE,
           xlab = "Délai (j)", ylab = "Probabilité d'appareillage",
           xlim = c(0, 90), break.x.by = 30,
           risk.table.title = "Nombre d'individus non appareillés",
           legend.title = "Groupes")

# A plusieurs facteurs
ggsurvplot(fit = list( # Liste nommée des survfit uni
  "1" = survfit(Surv(DATA$`Temps (j)`, Appareillage) ~ DATA$infection.precoce, data = DATA),
  "2" = survfit(Surv(DATA$`Temps (j)`, Appareillage) ~ DATA$nombre.de.membre.ampute, data = DATA)),
  # combine  = TRUE --> NECESSAIRE ICI
  combine = TRUE, 
  pval = TRUE, risk.table = TRUE, surv.scale = "percent", fun = "event",
  xlab = "Délai (j)", ylab = "Probabilité d'appareillage",
  xlim = c(0, 90), break.x.by = 30,
  risk.table.title = "Nombre d'individus non appareillés",
  legend.title = "Groupes")

