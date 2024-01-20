
#### Import ####
rm(list = ls())
load(file = "C:/Users/h.marthinet/Documents/32 - R/Codes & Scripts/Rdata/Poisson V Negative Binomiale.Rdata")
DATA_TI$CLASSE_AGE[is.na(DATA_TI$CLASSE_AGE)] <- "18-24"
DATA_TI <- na.exclude(DATA_TI)

# Base pour les modeles
library(MASS)# Fonction glm.nb() pour la regression binomiale negative
library(stats) # Fonction glm() pour la regression de poisson
library(pscl) # Fonction zeroinfl() pour la regression ZIP

# Tests de performance de modeles
library(performance) 
library(AER)
library(DHARMa)
library(vcdExtra)

# ZIP
library(mpath)
library(zic)

# Tableaux de sortie
library(tab) 
library(stringr) 


# I. Exploration des donnees ####


#On commence par fit un modele de Poisson
reg_P <- glm(TI_CAA ~ CLASSE_AGE + ARMEE + SEXE, 
             data = DATA_TI, family = poisson)


# _a. Sur-dispersion ####

# Regression de Poisson assume mean = var
# Et n'est pas adaptee aux echantillons sur-disperses
# Dans le cas ou var >>> mean : regression binomiale negative plus adapte
mean(DATA_TI$TI_CAA)
var(DATA_TI$TI_CAA)

# Tests de performance du modele
performance::check_overdispersion(reg_P)
AER::dispersiontest(reg_P)



# _b. Zero-inflation ####

# Visualisation des donnees
table(DATA_TI$TI_CAA)
hist(DATA_TI$TI_CAA, breaks = 300)
barplot(table(DATA_TI$TI_CAA))


# Tests de la zero inflation
# Score text sur Y
vcdExtra::zero.test(DATA_TI$TI_CAA)

# Test sur les performances du modele de Poisson
# Est ce que le modele predit plus de zero que prevu
performance::check_zeroinflation(reg_P)
DHARMa::testZeroInflation(DHARMa::simulateResiduals(fittedModel = reg_P))

# Test de Vuong pour comparer un ZIP et un Poisson
# Ne teste pas la zero inflation, mais la performance des modeles
# Uniquement pour les non nested model aka pas de multiniveau
reg_ZIP <- zeroinfl(TI_CAA ~ CLASSE_AGE + ARMEE + SEXE,  data = DATA_TI)
pscl::vuong(reg_P, reg_ZIP)





#### II. Construction des modeles ####

# _a. Poisson ####
# Regression basique sur les donnees de comptage
reg_P <- glm(TI_CAA ~ CLASSE_AGE + ARMEE + SEXE, 
             data = DATA_TI, 
             family = poisson)
(summ_P <- summary(reg_P))
logLik(reg_P)
AIC(reg_P)

# Pour le step, la fonciton classique fonctionne
step(reg_P, trace = FALSE)

# Pour un LRT entre deux poissons
# Trois facons equivalentes
anova(reg_P, glm(TI_CAA ~ CLASSE_AGE + ARMEE, data = DATA_TI, family = poisson), test = "LRT")
lrtest(reg_P, glm(TI_CAA ~ CLASSE_AGE + ARMEE, data = DATA_TI, family = poisson))
pchisq(2 * (logLik(reg_P) - logLik(glm(TI_CAA ~ CLASSE_AGE + ARMEE, data = DATA_TI, family = poisson))),
       df = 1, lower.tail = FALSE)








# _b. Zero Inflated Poisson ####
# Si les donnes presentent de la zero-inflation
reg_ZIP <- zeroinfl(TI_CAA ~ CLASSE_AGE + ARMEE + SEXE,  
                    data = DATA_TI)
(summ_ZIP <- summary(reg_ZIP))
logLik(reg_ZIP)
AIC(reg_ZIP)

# Fonction step specifique car objet non supporte par la fonction classique
be.zeroinfl(reg_ZIP, data = DATA_TI)
# Meme chose pour le LRT : test de Vuong
vuong(reg_ZIP, zeroinfl(TI_CAA ~ CLASSE_AGE + ARMEE, data = DATA_TI))







# _c. Negative Binomial ####
# Si les donnees presentent de la sur-dispersion
reg_NB <- glm.nb(TI_CAA ~ CLASSE_AGE + ARMEE + SEXE, 
                 data = DATA_TI)
(summ_NB <- summary(reg_NB))
logLik(reg_NB)
AIC(reg_NB)

# Pour le step et le LRT, idem que pour le poisson
step(reg_NB, trace = FALSE)
anova(reg_NB, glm.nb(TI_CAA ~ CLASSE_AGE + SEXE, data = DATA_TI))

# Trois facons equivalente
anova(reg_NB, glm.nb(TI_CAA ~ CLASSE_AGE + ARMEE, data = DATA_TI))
lrtest(reg_NB, glm.nb(TI_CAA ~ CLASSE_AGE + ARMEE, data = DATA_TI))
pchisq(2 * (logLik(reg_NB) - logLik(glm.nb(TI_CAA ~ CLASSE_AGE + ARMEE, data = DATA_TI))),
       df = 1, lower.tail = FALSE)








# _d. Zero Inflated Negative Binomial ####
# Si les donnes presentent de la zero-inflation ET de la sur-dispersion
reg_ZINB <- zeroinfl(TI_CAA ~ CLASSE_AGE + ARMEE + SEXE,  
                     data = DATA_TI, 
                     dist = "negbin")
(summ_ZINB <- summary(reg_ZINB))
logLik(reg_ZINB)
AIC(reg_ZINB)

# Meme probleme que pour le ZIP, usage de fonctions specifique
# step
be.zeroinfl(reg_ZINB, data = DATA_TI, dist = "negbin")
# LRT
vuong(reg_ZINB, zeroinfl(TI_CAA ~ CLASSE_AGE + ARMEE, data = DATA_TI, dist = "negbin"))








#### III. Choix du modele ####


# _a. LRT ####
# POur comparer un ZIP/ZINB et un Poisson/NB, ou deux ZIP/ZINB
# La fonction anova n'est pas suppotre et est remplace par un test de Vuong pour les ZI
# Attention, que pour des non-nested models
pscl::vuong(reg_P, reg_ZIP)

AIC(reg_P)
AIC(reg_ZIP)


pscl::vuong(reg_NB, reg_ZINB)

AIC(reg_NB)
AIC(reg_ZINB)

# Tests classique quand uniquement Poisson ou NB
AIC(reg_NB)
AIC(reg_P)
lrtest(reg_P, reg_NB)
anova(reg_P, reg_NB, test = "LRT")



#### _b. Criteres et ratios ####


# Variance residuelle
summ_NB$deviance
summ_P$deviance


#### Par les ratios 
## Ratio de la variance residuelle sur le nombre de DF, 
##    ou du chi-2 de goodness-of-fit sur le nombre de DF
## Idealement, les ratio doivent etre proches de 1 

# Ratio deviance to DF
(ratio_deviance_NB <- summ_NB$deviance/summ_NB$df.resid)
(ratio_deviance_P <- summ_P$deviance/summ_P$df.resid)
# Ratio pearson goodness of fit chi square to DF
chisq.test(x = DATA_TI$TI_CAA, y = floor(predict(reg_NB)))$statistic/summ_NB$df.resid
chisq.test(x = DATA_TI$TI_CAA, y = floor(predict(reg_P)))$statistic/summ_P$df.resid







#### IV. Amelioration des modeles ####




#### _a. Estimation des coefficients avec des effet-fixes ####
# La regression de Poisson n'est pas une regression a effet fixe,
# Et ce probleme se reporte sur la regression binomiale negative

# La negative multinomiale impose elle une trop grosse contrainte sur ces effets

# Deux solutions pour pallier au probleme sont proposees par 
### Allison et Waterman - Fixed Effects negative binomial regression models
# Une pour chaque regression


#### __1. Ajustement des SE par ratio deviance/df ####
## La premiere methode part du modele de Poisson et ajuste les SE pour prendre en compte la sur-dispersion
## On multiplie les SE par la racine d'un des ratio presente au dessus (var/DF ou GoF/DF)
## Ces ratios sont superieurs a 1, donc on elargit les IC des odd ratios

# On repart de la regression de Poisson classique
tab_P_ajust <- as.data.frame(summ_P$coefficients[-1, ])
# Et ajuste les SE
tab_P_ajust[, "ICinf"] <- round(exp(tab_P_ajust[, "Estimate"] - 1.96*tab_P_ajust[, "Std. Error"]*sqrt(ratio_deviance_P)), 2)
tab_P_ajust[, "ICsup"] <- round(exp(tab_P_ajust[, "Estimate"] + 1.96*tab_P_ajust[, "Std. Error"]*sqrt(ratio_deviance_P)), 2)

# Reste du tableau de sortie
tab_P_ajust[, "Estimate"] <- round(exp(tab_P_ajust[, "Estimate"]), 2)
tab_P_ajust[, "Pr(>|z|)"] <- ifelse(tab_P_ajust[, "Pr(>|z|)"] < 0.001, "<0.001", round(tab_P_ajust[, "Pr(>|z|)"], 3))
tab_P_ajust[, "IC"] <- paste0("(", tab_P_ajust[, "ICinf"], ", ", tab_P_ajust[, "ICsup"], ")")
tab_P_ajust <- tab_P_ajust[, c("Estimate", "IC", "Pr(>|z|)")]

tab_P_ajust



#### __2. Unconditionnal Negative Binomial ####
# La deuxieme methode consiste a estimer les effet fixes de la regression binomiale negative
# en introduisant des dummy variables (variables muettes) categorielles 

# Minimum une variable, mais techniquement autant qu'on veut
DATA_TI$dummy1 <- sample.int(2, size = nrow(DATA_TI), replace = TRUE)
DATA_TI$dummy2 <- sample.int(2, size = nrow(DATA_TI), replace = TRUE)
DATA_TI$dummy3 <- sample.int(2, size = nrow(DATA_TI), replace = TRUE)

# Nouveau modele en incluant les dummies
reg_NB_UC <- glm.nb(TI_CAA ~ CLASSE_AGE + ARMEE + SEXE + 
                      factor(dummy1) + factor(dummy2) + factor(dummy3), 
                    data = DATA_TI)
(summ_NB_UC <- summary(reg_NB_UC))

# Tableau de sortie
tab_NB_UC <- tabglm(reg_NB_UC, columns = c('hr', 'hrci', 'p'))
tab_NB_UC[, 1] <- str_replace_all(tab_NB_UC[, 1], "\\\\ \\\\ \\\\ ", "  ")
tab_NB_UC <- tab_NB_UC[-which(tab_NB_UC[,1] == "Intercept"), ]
tab_NB_UC











# V. Tableau de sortie ####

# _a. Pour un Poisson ou un NB ####

# Choix des stats a compiler
tab_NB <- tabglm(reg_NB, columns = c('hr', 'hrci', 'p')) # ou columns = c('or', 'orci', 'p')
tab_NB[, 1] <- str_replace_all(tab_NB[, 1], "\\\\ \\\\ \\\\ ", "  ") # Mise en page des modalites
tab_NB <- tab_NB[-which(tab_NB[,1] == "Intercept"), ] # Suppression de 'l'effet fixe'
colnames(tab_NB) <- c("", "", "", "")
tab_NB

# _b. Pour un ZIP ou ZINB

### Partie zero
tab_ZINB <- as.data.frame(summary(reg_ZINB)$coefficients$zero[-1, ]) # Coef du model pour la partie zero
tab_ZINB[, "ICinf"] <- round(exp(tab_ZINB[, "Estimate"] - 1.96), 2) # Calcul de l'IC
tab_ZINB[, "ICsup"] <- round(exp(tab_ZINB[, "Estimate"] + 1.96), 2)
tab_ZINB[, "Estimate"] <- round(exp(tab_ZINB[, "Estimate"]), 2) # Calcul des OR
tab_ZINB[, "Pr(>|z|)"] <- ifelse(tab_ZINB[, "Pr(>|z|)"] < 0.001, "<0.001", round(tab_ZINB[, "Pr(>|z|)"], 3))
tab_ZINB[, "IC"] <- paste0("(", tab_ZINB[, "ICinf"], ", ", tab_ZINB[, "ICsup"], ")")
tab_ZINB_zero <- tab_ZINB[, c("Estimate", "IC", "Pr(>|z|)")]

tab_ZINB_zero

### Partie count
tab_ZINB <- as.data.frame(summary(reg_ZINB)$coefficients$count[-1, ]) # Coef du model pour la partie count
tab_ZINB[, "ICinf"] <- round(exp(tab_ZINB[, "Estimate"] - 1.96), 2) # Calcul de l'IC
tab_ZINB[, "ICsup"] <- round(exp(tab_ZINB[, "Estimate"] + 1.96), 2)
tab_ZINB[, "Estimate"] <- round(exp(tab_ZINB[, "Estimate"]), 2) # Calcul des OR
tab_ZINB[, "Pr(>|z|)"] <- ifelse(tab_ZINB[, "Pr(>|z|)"] < 0.001, "<0.001", round(tab_ZINB[, "Pr(>|z|)"], 3))
tab_ZINB[, "IC"] <- paste0("(", tab_ZINB[, "ICinf"], ", ", tab_ZINB[, "ICsup"], ")")
tab_ZINB_count <- tab_ZINB[, c("Estimate", "IC", "Pr(>|z|)")]

tab_ZINB_count


### Concatenation
# Corps du tableau
tab_ZINB <- as.data.frame(rbind(c("", "", "", "Zero", "", "", "Count", ""),
      c("", "", rep(c("OR", "IC 95%", "p"), 2)),
      cbind(names(unlist(reg_ZINB$levels)), 
                     unlist(reg_ZINB$levels),
                     matrix(rep("", 6*length(unlist(reg_ZINB$levels))), ncol = 6)), 
      c("", "Log(theta)", rep("", 6))))

# Rempli
tab_ZINB[which(!str_detect(names(unlist(reg_ZINB$levels)), "1"))+2, 3:5] <- tab_ZINB_zero
tab_ZINB[c(which(!str_detect(names(unlist(reg_ZINB$levels)), "1"))+2, nrow(tab_ZINB)), 6:8] <- tab_ZINB_count
tab_ZINB[tab_ZINB[, 7] == "", 3:8] <- matrix(rep(c("(ref)", "-", "-", "(ref)", "-", "-"), sum(tab_ZINB[, 7] == "")), 
                                             ncol = 6, byrow = TRUE)
tab_ZINB[, 1] <- gsub("[0-9]+", "", tab_ZINB[, 1])
colnames(tab_ZINB) <- NULL
rownames(tab_ZINB) <- NULL
tab_ZINB