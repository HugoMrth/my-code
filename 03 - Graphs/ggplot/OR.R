library(nnet)
library(gtsummary)
library(car)
library(GGally)
library(ggplot2)
library(broom)


#### Militaires ####

regm_mili <- multinom(Covid19 ~ Age + Sexe + Grade + Lieu + Armee + Situation, data = DATA_mod[DATA_mod$Statut == "Militaire d’active",])

# summary(regm_mili)
# odds.ratio(regm_mili)
# tbl_regression(regm_mili, exponentiate = TRUE)

Anova(regm_mili)
Anova(regm_mili, test.statistic = "Wald")
Anova(regm_mili, test.statistic = "Wald", multivariate = TRUE)

vif(regm_mili)

table(predict(regm_mili, newdata = DATA_mod), DATA_mod$Covid19)


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

# Mise en page pour impression
res_mili <- as.data.frame(b[, -c(4, 5)])
res_mili <- res_mili[, c(1, 7, 2, 3, 5, 6, 4)]
res_mili <- as.data.frame(rbind(res_mili[1,], res_mili[1,],
                         c("Covid-19+ sans APC probable", "Sexe", "Homme", 1, 1, 1, 1), res_mili[2,],
                         c("Covid-19+ sans APC probable","Grade", "Officier", 1, 1, 1, 1), res_mili[3:4,],
                         c("Covid-19+ sans APC probable","Lieu", "Métropole", 1, 1, 1, 1), res_mili[5:6,],
                         c("Covid-19+ sans APC probable","Armee", "Terre", 1, 1, 1, 1), res_mili[7:10,],
                         c("Covid-19+ sans APC probable","Situation", "Célibataire", 1, 1, 1, 1), res_mili[11,],
                         res_mili[1+11,], res_mili[1+11,],
                         c("APC probable","Sexe", "Homme", 1, 1, 1, 1), res_mili[2+11,],
                         c("APC probable","Grade", "Officier", 1, 1, 1, 1), res_mili[3:4+11,],
                         c("APC probable","Lieu", "Métropole", 1, 1, 1, 1), res_mili[5:6+11,],
                         c("APC probable","Armee", "Terre", 1, 1, 1, 1), res_mili[7:10+11,],
                         c("APC probable","Situation", "Célibataire", 1, 1, 1, 1), res_mili[11+11,]))

res_mili[c(1, 18), 3] <- "Tranche de 1 an"
res_mili[c(2, 19), 3] <- "Tranche de 10 ans"
res_mili$estimate <- as.numeric(res_mili$estimate)
res_mili$conf.low <- as.numeric(res_mili$conf.low)
res_mili$conf.high <- as.numeric(res_mili$conf.high)
res_mili$p.value <- as.numeric(res_mili$p.value)
res_mili[c(2, 19), 4:6] <- res_mili[c(2, 19), 4:6]^10
res_mili$estimate <- round(res_mili$estimate, 2)
res_mili$conf.low <- round(res_mili$conf.low, 2)
res_mili$conf.high <- round(res_mili$conf.high, 2)
res_mili$p.value <- round(res_mili$p.value, 3)
res_mili[res_mili == 1] <- "-"
res_mili$p.value[res_mili$p.value == "0"] <- "<0.001"


# Mise en page pour le graph
b <- as.data.frame(b[, -c(4, 5, 6)])
b <- as.data.frame(rbind(b[1,],
           c("Covid-19+ sans APC probable", "Homme", 1, 1, 1,"Sexe"), b[2,],
           c("Covid-19+ sans APC probable", "Officier", 1, 1, 1,"Grade"), b[3:4,],
           c("Covid-19+ sans APC probable", "Métropole", 1, 1, 1,"Lieu"), b[5:6,],
           c("Covid-19+ sans APC probable", "Terre", 1, 1, 1,"Armee"), b[7:10,],
           c("Covid-19+ sans APC probable", "Célibataire", 1, 1, 1,"Situation"), b[11,],
           b[1+11,],
           c("APC probable", "Homme", 1, 1, 1,"Sexe"), b[2+11,],
           c("APC probable", "Officier", 1, 1, 1,"Grade"), b[3:4+11,],
           c("APC probable", "Métropole", 1, 1, 1,"Lieu"), b[5:6+11,],
           c("APC probable", "Terre", 1, 1, 1,"Armee"), b[7:10+11,],
           c("APC probable", "Célibataire", 1, 1, 1,"Situation"), b[11+11,]))

b[b$var == "Age", 2] <- "Tranche de 10 ans"
b$var <- factor(b$var, levels = unique(b$var))
b$term <- factor(b$term, levels = rev(unique(b$term)))
b$estimate <- as.numeric(b$estimate)
b$conf.low <- as.numeric(b$conf.low)
b$conf.high <- as.numeric(b$conf.high)
b[b$var == "Age", 3:5] <- b[b$var == "Age", 3:5]^10


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
  geom_hline(
    yintercept = c(0.25, 0.5, 2, 4),
    color = "gray75",
    linetype = "dotted"
  ) +
  geom_errorbar(
    position = position_dodge(0.5), 
    width = 0.2
  ) + 
  geom_point(
    position = position_dodge(width = 0.5)
  ) + 
  scale_y_log10(
    breaks = c(0.25, 0.5, 1, 2),
    limits = c(0.24, 3)
  ) +
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
    color = " ") +
  theme_classic() +
  theme(
    panel.grid.major.y = element_line(colour = "lightgray")
  )

