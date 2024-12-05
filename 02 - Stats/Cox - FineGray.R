
###################################################################################################
###################################################################################################
###################################################################################################
###################################################################################################

#### Exemple concret : Etude PENURIE ####
#### __TTT ~ EXPO + ATC ####
dataf$etime <-
  ifelse(dataf$deces_suivi_ON == "Oui",
         dataf$delai_dc,
         dataf$duree_1st_Tref)
dataf$event <- factor(
  ifelse(
    dataf$deces_suivi_ON == "Oui",
    "Décès",
    ifelse(
      dataf$duree_1st_Tref == 365,
      "Censure",
      ifelse(
        dataf$duree_1st_Tref - dataf$delai_1st_Tswi_niv1 == -1,
        "Substitution",
        "Arrêt"
      )
    )
  ),
  levels = c("Censure", "Arrêt", "Substitution", "Décès")
)


datam <- dataf[, c("EXPO", "atc7", "etime", "event")]
data_fg1 <- finegray(Surv(etime, event) ~ ., data = datam, etype = "Arrêt")
fgfit1 <- coxph(Surv(fgstart, fgstop, fgstatus) ~ EXPO + atc7, weight = fgwt, data = data_fg1)
fgfit1
data_fg2 <- finegray(Surv(etime, event) ~ ., data = datam, etype = "Substitution")
fgfit2 <- coxph(Surv(fgstart, fgstop, fgstatus) ~ EXPO + atc7, weight = fgwt, data = data_fg2)
fgfit2
data_fg3 <- finegray(Surv(etime, event) ~ ., data = datam, etype = "Décès")
fgfit3 <- coxph(Surv(fgstart, fgstop, fgstatus) ~ EXPO + atc7, weight = fgwt, data = data_fg3)
fgfit3


paste0(formatC(exp(coef(fgfit1)), 2, format = "f")," [",
  formatC(exp(confint(fgfit1)[,1]), 2, format = "f"),"-",
  formatC(exp(confint(fgfit1)[,2]), 2, format = "f"),"]")
paste0(formatC(exp(coef(fgfit2)), 2, format = "f")," [",
  formatC(exp(confint(fgfit2)[,1]), 2, format = "f"),"-",
  formatC(exp(confint(fgfit2)[,2]), 2, format = "f"),"]")
paste0(formatC(exp(coef(fgfit3)), 2, format = "f")," [",
       formatC(exp(confint(fgfit3)[,1]), 2, format = "f"),"-",
       formatC(exp(confint(fgfit3)[,2]), 2, format = "f"),"]")





###################################################################################################
###################################################################################################
###################################################################################################
###################################################################################################


#### START ####
rm(list = ls())
library(survival)




#### Estimateur de Kaplan Meier ####

par(mfrow=c(1,2))
# Histogramme des ages au diagnostic
hist(mgus2$age, nclass=30, main='', xlab="Age")
with(mgus2, tapply(age, sex, mean))


# Coubre de Kaplan Meier de la survie post-diagnostic
(mfit1 <- survfit(Surv(futime, death) ~ sex, data=mgus2))
plot(mfit1, col=c(1,2), xscale=12, mark.time=FALSE, lwd=2,
     xlab="Years post diagnosis", ylab="Survival")
legend("topright", c("female", "male"), col=1:2, lwd=2, bty='n')





#### Risques Competitifs ####
# NB : la variable event dans le risque competitif est un factor
#      alors que c'est un numeric ou logique dans la survie classique

# On considère l'etat PCM comme absorbant : pas de transition PCM -> Death
# Si PCM : delai PCM, sinon delai Death
mgus2$etime <- with(mgus2, ifelse(pstat==0, futime, ptime))
# Creation de de la variable event trinaire
event <- with(mgus2, ifelse(pstat==0, 2*death, 1*pstat))
mgus2$event <- factor(event, 0:2, labels=c("censor", "pcm", "death"))
table(mgus2$event)

# arg istate pour renseigner l'etat initial (ignore ici)
mfit2 <- survfit(Surv(etime, event) ~ sex, data=mgus2)
print(mfit2, rmean=240, scale=12)

# Coubre de survie des risques competitifs
par(mfrow = c(1, 1))
plot(mfit2, col=c(1,2,1,2), lty=c(2,2,1,1),
       mark.time=FALSE, lwd=2, xscale=12,
       xlab="Years post diagnosis", ylab="Probability in State")
legend(240, .6, c("death:female", "death:male", "pcm:female", "pcm:male"),
         col=c(1,2,1,2), lty=c(1,1,2,2), lwd=2, bty='n')


#### Occurence simultanee des deux evenements ####

# 9 individus nt la survenue de la PCM et de la mort le même mois
# Pas problematique si survenue a des mois diffrence car on considere PCM absordant
mgus_test <- mgus2[mgus2$ptime == mgus2$futime, ]
table(mgus_test$pstat, mgus_test$death)

# On repousse le time des PCM de 0.1 mois pour les exaequo
ptemp <- with(mgus2, ifelse(ptime==futime & pstat==1, ptime-.1, ptime))
# On dedouble les lignes pour les individus presentant les deux evenement
# Des facon a ce qu'un seul des evenement ne soit present par lignes
# Creation de la variable enum pour decompter les event
data3 <- tmerge(mgus2, mgus2, id=id, death=event(futime, death),
                  pcm = event(ptemp, pstat))
data3 <- tmerge(data3, data3, id, enum=cumtdc(tstart))
with(data3, table(death, pcm))
       
# Cette fois on peut transitionner de PCM a death
temp <- with(data3, ifelse(death==1, 2, pcm))
data3$event <- factor(temp, 0:2, labels=c("censor", "pcm", "death"))
mfit3 <- survfit(Surv(tstart, tstop, event) ~ sex, data=data3, id=id)
print(mfit3, rmean=240, digits=2)
mfit3$transitions
# Courne de survie de la PCM avec transition to death
# La coubre redescned dans le patient transisitonne vers la mort
plot(mfit3[,"pcm"], mark.time=FALSE, col=1:2, lty=1:2, lwd=2,
     xscale=12,
     xlab="Years post MGUS diagnosis", ylab="Fraction in the PCM state")
legend(40, .04, c("female", "male"), lty=1:2, col=1:2, lwd=2, bty='n')



# On ajoute encore un etat de transition
# En déclinant la mort selon mort apres/sans PCM

# Death after PCM will correspond to data rows with
# enum = 2 and event = death
d2 <- with(data3, ifelse(enum==2 & event=='death', 4, as.numeric(event)))
e2 <- factor(d2, labels=c("censor", "pcm", "death w/o pcm",
                            "death after pcm"))
mfit4 <- survfit(Surv(tstart, tstop, e2) ~ sex, data=data3, id=id)
plot(mfit2[2,], lty=c(1,2),
       xscale=12, mark.time=FALSE, lwd=2,
       xlab="Years post diagnosis", ylab="Probability in State")
lines(mfit4[2,4], mark.time=FALSE, col=2, lty=1, lwd=2,
        conf.int=FALSE)
legend(200, .5, c("Death w/o PCM", "ever PCM", "Death after PCM"), 
       col=c(1,1,2), lty=c(2,1,1),
       lwd=2, bty='n', cex=.82)









#### Fine Gray Univarie ####
# The primary idea of the Fine-Gray approach is to 
# turn the multi-state problem into a collection of two-state ones

# An interesting aspect of this is that the fit can be done as a two stage process: the first stage
# creates a special data set while the second fits a weighted coxph or survfit model to the data.

# Meme depart que precedemment
etime <- with(mgus2, ifelse(pstat==0, futime, ptime))
event <- with(mgus2, ifelse(pstat==0, 2*death, 1))
event <- factor(event, 0:2, labels=c("censor", "pcm", "death"))

# Creation des data frame pour chaque sous modele a deux etats
pcmdat <- finegray(Surv(etime, event) ~ ., data=mgus2, etype="pcm")
pcmdat[1:4, c(1:3, 11:14)]

deathdat <- finegray(Surv(etime, event) ~ ., data=mgus2, etype="death")
# Les dataframes crees sont beaucoup plus long que ceux de depart
dim(pcmdat)
dim(deathdat)
dim(mgus2)

# Les courbes de survie sont quasiment identiques
# et le seraient si on tenait compte des patternes de censures différents

# The PCM curves of the multi-state model
pfit2 <- survfit(Surv(fgstart, fgstop, fgstatus) ~ sex, data=pcmdat, weights=fgwt)
plot(pfit2, col=c(1,2), xscale=12, mark.time=FALSE, lwd=2,
     xlab="Years post diagnosis", ylab="No PCM")
# The death curves of the multi-state model
dfit2 <- survfit(Surv(fgstart, fgstop, fgstatus) ~ sex, data=deathdat, weights=fgwt)
plot(dfit2, col=c(1,2), xscale=12, mark.time=FALSE, lwd=2,
     xlab="Years post diagnosis", ylab="Survival")



# Un modele de Cox sur ces data frame revient a un Fine Gray
fgfit1 <- coxph(Surv(fgstart, fgstop, fgstatus) ~ sex, data=pcmdat,
                weights= fgwt)
summary(fgfit1)

fgfit2 <- coxph(Surv(fgstart, fgstop, fgstatus) ~ sex, data=deathdat,
                  weights= fgwt)
fgfit2


ndata <- data.frame(sex=c("F", "M"))
fgsurv1 <- survfit(fgfit1, ndata)
plot(fgsurv1, fun="event",
     col=1:2,
     lwd=2, xscale=12,
     conf.times=c(5, 15, 25)*12,
     xlab="Years post diagnosis", ylab="Fraction with PCM")
legend("topleft", c("Female", "Male"),
         col=1:2, lty=c(1,1,2,2), bty='n')



#### Fine Gray Multivarie ####
fgfit2a <- coxph(Surv(fgstart, fgstop, fgstatus) ~ age + sex + mspike,
                 data=pcmdat, weights=fgwt)
fgfit2b <- coxph(Surv(fgstart, fgstop, fgstatus) ~ age + sex + mspike,
                   data=deathdat, weights=fgwt)
round(rbind(PCM= coef(fgfit2a), death=coef(fgfit2b)), 3)
# Males have a lower lifetime risk of PCM before death and a higher risk of death before PCM, while a high
# serum m-spike works in the opposite direction


# Tableaux resume du modele
summary(fgfit2a)
tidy(fgfit2a)

tab_FG <- as.data.frame(summary(fgfit2a)$coefficients) # Coef du model pour la partie zero
tab_FG[, "ICinf"] <- round(exp(tab_FG[, "coef"] - 1.96), 2) # Calcul de l'IC
tab_FG[, "ICsup"] <- round(exp(tab_FG[, "coef"] + 1.96), 2)
tab_FG[, "Estimate"] <- round(exp(tab_FG[, "coef"]), 2) # Calcul des OR
#tab_FG[, "Pr(>|z|)"] <- ifelse(tab_FG[, "Pr(>|z|)"] < 0.001, "<0.001", round(tab_FG[, "Pr(>|z|)"], 3))
tab_FG[, "IC"] <- paste0("(", tab_FG[, "ICinf"], ", ", tab_FG[, "ICsup"], ")")
(tab_FG <- tab_FG[, c("Estimate", "IC", "Pr(>|z|)")])










# Predicted survival curves for the model
dummy <- expand.grid(sex= c("F", "M"), age=c(60, 80), mspike=1.2)
fsurv1 <- survfit(fgfit2a, dummy) # time to progression curves
plot(fsurv1, xscale=12, col=1:2, lty=c(1,1,2,2), lwd=2, fun='event',
       xlab="Years", ylab="Fine-Gray predicted",
       xmax=12*25, ylim=c(0, .15))
legend(1, .15, c("Female, 60", "Male, 60","Female: 80", "Male, 80"),
         col=c(1,2,1,2), lty=c(1,1,2,2), lwd=2, bty='n')



# Proportional hazards assumption
# p must be greater than 5%
# here age violates the proportionnal hazard assumtpion
zph.fgfit2a <- cox.zph(fgfit2a)
zph.fgfit2a

plot(zph.fgfit2a[1])
abline(h=coef(fgfit2a)[1], lty=2, col=2)



# A further weakness of the Fine-Gray approach is that since the two endpoints are modeled
# separately, the results do not have to be consistent
fsurv2 <- survfit(fgfit2b, dummy) # time to progression curves
xtime <- 0:(30*12) #30 years
y1a <- 1 - summary(fsurv1, times=xtime)$surv #predicted pcm
y1b <- 1 - summary(fsurv2, times=xtime)$surv #predicted deaths before pcm
y1 <- (y1a + y1b) #either
matplot(xtime/12, y1, col=1:2, lty=c(1,1,2,2), type='l',
          xlab="Years post diagnosis", ylab="FG: either endpoint")
abline(h=1, col=3)
legend("bottomright", c("Female, 60", "Male, 60","Female: 80", "Male, 80"),
         col=c(1,2,1,2), lty=c(1,1,2,2), lwd=2, bty='n')

#### END ####
