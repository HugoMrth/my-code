rm(list = ls())


# Packages #
library(rpart) # rpart = Recursive PARTitioning

# Jeu de donnees #
stagec

# I. Creation de l'arbre ####

# _a. Premier arbre ####

# Codage en facteur
progstat <- factor(stagec$pgstat, levels = 0:1, labels = c("No", "Prog"))
# Creation de l'arbre
cfit <- rpart(progstat ~ age + eet + g2 + grade + gleason + ploidy,
              data = stagec, method = 'class')

# Affichage des noeuds
# Listing des regles de décision
print(cfit)

# Graphique de l'arbre 
par(mar = rep(0.1, 4))
plot(cfit)
text(cfit)

# _b. Importance des variables
temp <- with(stagec, table(cut(grade, c(0, 2.5, 4)),
                           cut(gleason, c(2, 5.5, 10)),
                           exclude = NULL))
temp

# II. Découpage de l'arbre ####

printcp(cfit)


fit <- prune(cfit, cp = 0.02)
par(mar = rep(0.1, 4))
plot(fit, branch = 0.3, compress = TRUE)
text(fit)


summary(cfit, cp = 0.06)

# III. Autres methodes ####

# _a. Poisson ####
sfit <- rpart(skips ~ Opening + Solder + Mask + PadType + Panel,
              data = solder.balance, method = 'poisson',
              control = rpart.control(cp = 0.05, maxcompete = 2))
sfit
summary(sfit, cp = 0.1)

par(mar = rep(0.1, 4))
plot(sfit)
text(sfit, use.n = TRUE, min = 3)



fit.prune <- prune(sfit, cp = 0.10)
plot(fit.prune)
text(fit.prune, use.n = TRUE, min = 2)

# _b. Survie ####

require(survival)
temp <- coxph(Surv(pgtime, pgstat) ~ 1, stagec)
newtime <- predict(temp, type = 'expected')
pfit <- rpart(Surv(pgtime, pgstat) ~ age + eet + g2 + grade +
                gleason + ploidy, data = stagec)
print(pfit)





pfit2 <- prune(pfit, cp = 0.016)
par(mar = rep(0.2, 4))
plot(pfit2, uniform = TRUE, branch = 0.4, compress = TRUE)
text(pfit2, use.n = TRUE)



temp <- snip.rpart(pfit2, 6)
km <- survfit(Surv(pgtime, pgstat) ~ temp$where, stagec)
plot(km, lty = 1:4, mark.time = FALSE,
       xlab = "Years", ylab = "Progression")
legend(10, 0.3, paste('node', c(4,5,6,7)), lty = 1:4)
8.5