

#### GEE ####
# Cas d'un modèle univarié selon un groupe d'exposisition
# Les individus sont appariés : N=3663 par groupe
# Donc nous avons 3663 clusters de 2 individus (1 exposé, 1 non-exposé)
library(geepack)

gee_model <- geeglm(formula = Y ~ GROUPE,
         family = binomial,       # binomial car Y binaire
         id = id,                 # Les numeros des clusters
         corstr = "exchangeable", # Pour données appariées
         data = DATA_APP %>% 
           mutate(Y = ifelse(contracep_ON == "Oui", 1, 0),
                  GROUPE = factor(GROUPE, levels = c("Témoin", "Exposé")), # Témoins en reférence
                  id = ifelse(GROUPE == "Témoin", 1:3663, "Exposé"), # Calcul dela variabel cluster
                  id = ifelse(GROUPE == "Exposé", 1:3663, id)) %>%
           dplyr::select(Y, GROUPE, id)
         )

# Sortie du modèle 
coefficients <- summary(gee_model)$coefficients[, "Estimate"]
se <- summary(gee_model)$coefficients[, "Std.err"]

paste0(formatC(exp(coefficients)[2], digits = 2, format = "f"), " [",                 # OR
       formatC(exp(coefficients - 1.96 * se)[2], digits = 2, format = "f"), "-",      # IC inf
       formatC(exp(coefficients + 1.96 * se)[2], digits = 2, format = "f"), "] p = ", # IC sup
       formatC(summary(gee_model)$coefficients[2, 4], digits = 3, format = "f"))      # p val








#### RRRRRRRR ####


# https://larmarange.github.io/analyse-R/modeles-a-effets-aleatoires.html#cas-des-donn%C3%A9es-corr%C3%A9l%C3%A9es-discr%C3%A8tes
# https://larmarange.github.io/analyse-R/modeles-a-effets-aleatoires.html#cas-des-donn%C3%A9es-corr%C3%A9l%C3%A9es-discr%C3%A8tes
# https://larmarange.github.io/analyse-R/modeles-a-effets-aleatoires.html#cas-des-donn%C3%A9es-corr%C3%A9l%C3%A9es-discr%C3%A8tes
# https://larmarange.github.io/analyse-R/modeles-a-effets-aleatoires.html#cas-des-donn%C3%A9es-corr%C3%A9l%C3%A9es-discr%C3%A8tes


library(geepack)
library(gee)
library(doBy)
library(broom)
library(latticeExtra)

#### DATA ####

data(ohio)
head(ohio, n = 8)


smoke.yes <- xtabs(resp ~ id + age, subset(ohio, smoke == 1))
smoke.no <- xtabs(resp ~ id + age, subset(ohio, smoke == 0))
marg.means <- data.frame(
  resp = c(apply(smoke.yes, 2, mean), apply(smoke.no, 2, mean)),
  age = gl(4, 1, labels = 7:10, ordered = TRUE),
  smoke = gl(2, 4, labels = c("smoking", "not smoking"))
)

p <- xyplot(resp ~ age,
            data = marg.means, group = smoke, type = c("l", "g"),
            xlab = "Age (years)", ylab = "Wheeziness (%)", lwd = 2
)
update(p, par.settings = custom.theme())


#### MODELES ####

# Premier modele avec matrice de correlation symetrique
gee.fit <- geese(resp ~ age * smoke,
                 id = id, data = ohio, family = binomial,
                 corstr = "exch", scale.fix = TRUE
)
summary(gee.fit)


# sous-estimation les paramètres de variance des effets stationnaires dans le temps
gee.fit.o <- gee(resp ~ age * smoke,
                 id = id, data = ohio, family = binomial,
                 corstr = "independence", scale.fix = TRUE
)
summary(gee.fit.o)$coefficients

# matrices de variance-covariance robuste et naive du mdoele
summary(as.vector(gee.fit$vbeta - gee.fit$vbeta.naiv))

# meme chose avec un estimateur sandwich
gee.fit.exch <- geeglm(resp ~ age * smoke,
                       id = id, data = ohio, family = binomial,
                       corstr = "exch", scale.fix = TRUE, std.err = "san.se"
)
summary(gee.fit.exch)




# Test individuel de l'effet de chaque facteur
# fait en comparant deux modeles emboites
gee.fit.exch2 <- geeglm(update(resp ~ age * smoke, . ~ . - age),
                        id = id, data = ohio, family = binomial,
                        corstr = "exch", scale.fix = TRUE, std.err = "san.se"
)
anova(gee.fit.exch, gee.fit.exch2)
# IC
exp(coef(gee.fit.exch)["age"] + c(-1, 1) * rob.se[2] * qnorm(0.975))



# summary
gee.fit.ci <- esticon(gee.fit.exch, diag(4))
tidy(gee.fit.ci, conf.int = TRUE)

# odds ratio
exp(tidy(gee.fit.ci, conf.int = TRUE)[, c("estimate", "conf.low", "conf.high")])



# odds ratio de l'age
gee.fit2 <- geeglm(update(resp ~ age * smoke, . ~ . - age:smoke),
                   id = id,
                   data = ohio, family = binomial,
                   corstr = "exch", scale.fix = TRUE
)
exp(coef(gee.fit2)["age"])



# dernier modele
gee.fit.exch3 <- geeglm(update(resp ~ age * smoke, . ~ . - age:smoke),
                        id = id, data = ohio, family = binomial,
                        corstr = "exch", scale.fix = TRUE, std.err = "san.se"
)
newdf <- expand.grid(age = seq(-2, 1, length = 50), smoke = c(0, 1))
newdf <- cbind.data.frame(resp = predict(gee.fit.exch3, newdata = newdf), newdf)
p <- xyplot(exp(resp) ~ age,
            data = newdf, group = smoke, type = c("l", "g"),
            xlab = "Age (years)", ylab = "Wheeziness (%)",
            auto.key = list(corner = c(0, 0), text = c("No", "Yes"), cex = .8)
)
update(p, par.settings = custom.theme())


#### SASASASASASAS ####

/** Création d'un jeu de données rassemblant les paires non-exp/exp pour le modèle GEE **/
 
proc sql;
	create table contra_diu_ne as 
	select pairnum, statut, diu_cuivre
	from sasdata1.gynaja_volet2_V2 
	where statut=0 ;
quit;
proc sql;
	create table contra_diu_e as 
	select pairnum, statut, diu_cuivre
	from sasdata1.gynaja_volet2_V2 
	where statut=1 ;
quit;
data Analy_contra_diu; set contra_diu_e contra_diu_ne; run;
proc sort data=Analy_contra_diu; by pairnum statut; run;
/** Modèle GEE **/
proc genmod data=Analy_contra_diu descending;
       class pairnum statut;
       model diu_cuivre = statut / dist=binomial link=logit;
       repeated subject=pairnum / corr=unstr corrw;
	   estimate "Statut" statut -1 1 /exp;
       lsmeans statut / ilink diff cl e;
run;





