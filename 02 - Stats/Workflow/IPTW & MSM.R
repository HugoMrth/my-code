#### PACKAGE IPW ####

rm(list = ls())

library(ipw)
library(survey)
library(nlme)
library(cobalt)
library(boot)


# In the stabilised weights, the numerator is given by the probability of being exposed
# conditional on the history of the previous level of the exposure and
# only baseline confounders. MSMs is the fact that they are a natural extension of the
# classical logistic and Cox model


#### Regression Logistique ####

# Simultaion de donnees
# l le facteur de confusion
# a l'exposition
set.seed(16)
n <- 1000
simdat <- data.frame(l = rnorm(n, 10, 5))
a.lin <- simdat$l - 10
pa <- exp(a.lin)/(1 + exp(a.lin))
simdat$a <- rbinom(n, 1, prob = pa)
simdat$y <- 10*simdat$a + 0.5*simdat$l + rnorm(n, -10, 5)
simdat[1:5,]

summary(simdat$a)

# calcul des poids
temp <- ipwpoint(exposure = a, family = "binomial", link = "logit",
                 numerator = ~ 1, denominator = ~ l, data = simdat)
summary(temp$ipw.weights)

# plot de distribution des poids
ipwplot(weights = temp$ipw.weights, logscale = FALSE,
        main = "Stabilized weights", xlim = c(0, 8))

# Resumes de modeles des numerateur et denominateur
summary(temp$num.mod)
summary(temp$den.mod)

# application des poids sur les data
simdat$sw <- temp$ipw.weights
msm <- (svyglm(y ~ a, design = svydesign(~ 1, weights = ~ sw, data = simdat)))
coef(msm)
confint(msm)
# Our estimate of the marginal causal effect of A on Y is 10.65 
# with 95% confidence interval(CI) 8.31-12.98.



ps.logit <- glm(a ~ l,
                family = binomial(link="logit"),
                data = simdat)
simdat$pscore <- ps.logit$fitted
simdat$poids <- with(simdat, a+(1-a)*ps.logit$fitted/(1-ps.logit$fitted))
ipwplot(weights = simdat$poids, logscale = FALSE,
        main = "Stabilized weights", xlim = c(0, 8))
balIPTW <- bal.tab(x = a ~ l,
                   data = simdat,
                   distance = "pscore",
                   weights = "poids",
                   method = "weighting",
                   disp.v.ratio = TRUE,
                   un = TRUE,
                   s.d.denom = "treated")

## Graphique des différences de moyennes (standardisées) et de proportions
love.plot(x = balIPTW,
          stat = "mean.diffs",
          abs = TRUE,
          ## options graphiques
          var.names = new.names,
          var.order = "unadjusted",
          threshold = 0.1,
          shape = 23)

bal.plot(x = a ~ l,
         data = simdat,
         var.name="l",
         weights = "poids",
         method = "weighting",
         estimand="ATT",
         which = "both")
















#### Survie ####

# data
# haartind : exposition au traitement
# dropout : binaire
# tstart & fuptime : debut et fin de chaque intervalle de temps
# premier tstart negatif pour permettre la modeilsation a T0
data("haartdat")
haartdat[1:10,]

# calcul des poids stabilises
temp <- ipwtm(exposure = haartind, family = "survival",
              numerator = ~ sex + age, denominator = ~ cd4.sqrt + sex + age,
              id = patient, 
              tstart = tstart, 
              timevar = fuptime, 
              type = "first",
              data = haartdat)
summary(temp$ipw.weights)


# calcul des poids non stabilises
# the unstabilized weights have a much wider distribution than
# the stabilized weights
temp.unstab <- ipwtm(exposure = haartind, family = "survival",
                     denominator = ~ cd4.sqrt, 
                     id = patient, 
                     tstart = tstart,
                     timevar = fuptime, 
                     type = "first", 
                     data = haartdat)
summary(temp.unstab$ipw.weights)

# plot des poids
ipwplot(weights = temp$ipw.weights, timevar = haartdat$fuptime,
        binwidth = 100, ylim = c(-1.5, 1.5), main = "Stabilized weights",
        xaxt = "n", yaxt = "n")
axis(side = 1, at = seq(0, 35, 5),
        labels = as.character(seq(0, 35, 5)*100))
axis(side = 2, at = seq(-1.5, 1.5, 0.5),
        labels = as.character(seq(-1.5, 1.5, 0.5)))



# si l'exposition a aussi un effet sur le dropout, on peut aussi modeliser
temp2 <- ipwtm(exposure = dropout, family = "survival",
               numerator = ~ sex + age, denominator = ~ cd4.sqrt + sex + age,
               id = patient, tstart = tstart, timevar = fuptime, type = "first",
               data = haartdat)


# construction du marginal structural model (MSM)
summary(coxph(Surv(tstart, fuptime, event) ~ haartind + cluster(patient),
              data = haartdat, weights = temp$ipw.weights*temp2$ipw.weights))
# We estimate a hazard ratio corresponding to the marginal causal effect 
# of HAART on mortality of 0.39 (95% CI 0.16-0.95).




#### Survie avec intervalles irreguliers ####
data("basdat")
data("timedat")
basdat[1:4,]
timedat$cd4.sqrt <- sqrt(timedat$cd4count)

# calcul des intervalle de temps
# avec un jour de decalage entre la variation de l'etat et le temps de mesure
timedat <- merge(timedat, basdat[,c("id","Ttb")], by = "id", all.x = TRUE)
timedat$tb.lag <- ifelse(with(timedat, !is.na(Ttb) & fuptime > Ttb), 1, 0)





# ajout d'effets aleatoir pour imputer des valeurs de CD4 en dehors des temps de mesure
cd4.lme <- lme(cd4.sqrt ~ fuptime + tb.lag, random = ~ fuptime | id,
               data = timedat)






# Construction du jeu de donnes pour calcul des poids
# uniquement avec les temps entre la premiere exposition et le dernier temps
times <- sort(unique(c(basdat$Ttb, basdat$Tend)))
startstop <- data.frame(
  id = rep(basdat$id, each = length(times)),
  fuptime = rep(times, nrow(basdat)))
startstop <- merge(startstop, basdat, by = "id", all.x = TRUE)
startstop <- startstop[with(startstop, fuptime <= Tend), ]

# calcul des debut de chaque intervalle de temps
startstop$tstart <- tstartfun(id, fuptime, startstop)

# calcul des statut de TB reels et lag de 1
startstop$tb <- ifelse(with(startstop, !is.na(Ttb) & fuptime >= Ttb), 1, 0)
startstop$tb.lag <- ifelse(with(startstop, !is.na(Ttb) & fuptime > Ttb), 1, 0)

# calcul du statut de mort
startstop$event <- ifelse(with(startstop, !is.na(Tdeath) & fuptime >= Tdeath), 1, 0)

# calcul des valeurs de cd4 au timestamp non dispos
startstop$cd4.sqrt <- predict(cd4.lme, 
                              newdata = data.frame(id = startstop$id, 
                                                   fuptime = startstop$fuptime, 
                                                   tb.lag = startstop$tb.lag))



# Calcul des IPW stabilises pour correction des effets des variabels de confusion
# sur cd4
temp <- ipwtm(exposure = tb, family = "survival",
              numerator = ~ 1, denominator = ~ cd4.sqrt, id = id,
              tstart = tstart, timevar = fuptime, type = "first", data = startstop)
summary(temp$ipw.weights)

ipwplot(weights = temp$ipw.weights, timevar = startstop$fuptime,
        binwidth = 100, main = "Stabilized weights", 
        xlab = "Days since HIV seroconversion", ylab = "Logarithm of weights", 
        xaxt = "n")
axis(side = 1, at = seq(0, 35, 5), labels = as.character(seq(0, 35, 5)*100))

# MSM des effets marginaux de la TB sur la mortalite
summary(coxph(Surv(tstart, fuptime, event) ~ tb + cluster(id),
              data = startstop, weights = temp$ipw.weights))
# Modele sans ajustement IPTW
summary(coxph(Surv(tstart, fuptime, event) ~ tb, data = startstop))
# Modele standart
summary(coxph(Surv(tstart, fuptime, event) ~ tb + cd4.sqrt, data = startstop))


# The estimated hazard ratio corresponding to the causal effect of TB on mortality 
# is 2.25 (95% CI 1.35-3.75). 

# Note that the estimate from an unadjusted model of 4.46 (95% CI 3.13-6.36) is an overestimate, 
# since both TB and death are more likely at lower CD4 counts. 

# The estimate from the conditional model of 1.28 (95% CI 0.79{2.06) is an underestimate, 
# since the indirect effect of TB through CD4 count is "conditioned away",













#### METHODE INSEE ####


library(cobalt)
library(boot)



#### Data ####

## Chargement des données : Lalonde Data from cobalt
data(lalonde,package="cobalt")
mybase <- lalonde
## Construction d!indicatrices à partir de la variable
## qualitative RACE
mybase <- splitfactor(data = mybase, var.name = "race",
                      replace = FALSE,
                      drop.level=NULL,
                      drop.first=FALSE)
## Changement d!unité des variables de revenus (en milliers de dollars)
mybase$re74 <- mybase$re74/1000
mybase$re75 <- mybase$re75/1000
## Créations d!indicatrices d!absence de revenus
mybase$u74 <- ifelse(test=(mybase$re74==0),1,0)
mybase$u75 <- ifelse(test=(mybase$re75==0),1,0)
## Création variable factorielle de groupe
mybase$group <- factor(x=mybase$treat,levels=c(0,1),
                       labels=c("Control","Treated"))

var <- names(mybase)[-c(1,4,8,15)]
round(t(sapply(mybase[var], function(x) tapply(x, mybase$treat, mean))), digits = 3)

new.names <- data.frame(
  old = c("age", "educ", "race_black",
          "race_hispan","race_white", "married",
          "nodegree", "re74", "re75","u74",
          "u75", "pscore"),
  new = c("Age", "Years of Education", "Black",
          "Hispanic", "White", "Married", "No Degree Earned",
          "Earnings 1974", "Earnings 1975", "Employed in 1974",
          "Employed in 1975", "Propensity Score"))

#### Scores de propension #####

ps.logit <- glm(treat ~ re74 + u74 + re75 + u75 + educ + race +
                  married + nodegree + age + I(age^2) +
                  nodegree:educ + re74:nodegree + u75:educ,
                family = binomial(link="logit"),
                data = mybase)
mybase$pscore <- ps.logit$fitted


## Calcul des pondérations pour estimer l!effet moyen sur les traités
mybase$poids <- with(mybase, treat+(1-treat)*ps.logit$fitted/(1-ps.logit$fitted))
ipwplot(weights = mybase$poids, logscale = FALSE,
        main = "Stabilized weights", xlim = c(0, 8))


# ipw_temp <- ipwpoint(exposure = treat, 
#                              family = "binomial", 
#                              link = "logit",
#                      numerator = ~ educ + race +
#                        married + nodegree + age,
#                      denominator = ~ re74 + u74 + re75 + u75 + educ + race +
#                                married + nodegree + age + I(age^2) +
#                                nodegree:educ + re74:nodegree + u75:educ, 
#                              data = mybase)
# summary(ipw_temp$ipw.weights)
# mybase$poids_ipw <- ipw_temp$ipw.weights
# 
# ipwplot(weights = mybase$poids_ipw, logscale = FALSE,
#         main = "Stabilized weights", xlim = c(0, 8))




balIPTW <- bal.tab(x = treat ~ age + educ + race +
                     married + nodegree + re74 + re75,
                   data = mybase,
                   distance = "pscore",
                   weights = "poids",
                   method = "weighting",
                   disp.v.ratio = TRUE,
                   un = TRUE,
                   s.d.denom = "treated")

## Graphique des différences de moyennes (standardisées) et de proportions
love.plot(x = balIPTW,
          stat = "mean.diffs",
          abs = TRUE,
          ## options graphiques
          var.names = new.names,
          var.order = "unadjusted",
          threshold = 0.1,
          shape = 23)


## Après ajustement avec des pondérations
bal.plot(x = treat ~ age + educ + race + married +
           nodegree + re74 + re75,
         data = mybase,
         var.name="educ",
         weights = "poids",
         method = "weighting",
         estimand="ATT",
         which = "both")


## Définition de la fonction de calcul de l!estimateur souhaité
iptw.boot=function(dat,indices) {
  dat=dat[indices,]
  ## Estimation du score de propension
  ps.out <- glm(formula = treat ~ re74 + u74 + re75 + u75 +
                  educ + race_black + race_hispan +
                  married + nodegree + age + I(age^2) +
                  nodegree:educ + re74:nodegree + u75:educ,
                , family = binomial(link=logit),
                data = dat)
  dat$pscore <- ps.out$fitted
  ## Calcul des pondérations pour estimer l!effet moyen sur les traités
  dat$att.weights <- with(dat, treat + (1-treat)*pscore/(1-pscore))
  ## Calcul de l!effet moyen
  glm.out=glm(re78 ~ treat,weight=att.weights,data=dat)
  ## Stockage de l!effet moyen estimé et de l!écart-type associé
  summary(glm.out)$coefficients[2,1:2]
}

## Estimation initiale de l!effet du traitement
iptw.boot(mybase,indices=1:nrow(mybase))

## 2000 réplications du calcul de l!estimateur
boot.out <- boot(data = mybase,
                 statistic = iptw.boot,
                 R = 2000)
## Calcul de l!intervalle de confiance
boot.ci(boot.out)
