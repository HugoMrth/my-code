model <- glm(EXPO ~ age_index + MRMI + nb_pass_urg_av +  nb_pass_urg_ap + sexe + 
               combi_acs_cmu + quint_defa + cat_nbatc + apl_2019_T, 
             data = dataf, family = "binomial")
summary(model)
eps <- predict(model, type = "response")
WEIGHTS <- ifelse(dataf$EXPO == "Exposé", 1/eps, 1/(1 - eps))
## Need to change race (categorical) into indicators (numerical)
data_diff <- dataf %>%
  dplyr::select(
    EXPO, age_index, MRMI, nb_pass_urg_av, nb_pass_urg_ap, sexe,
    combi_acs_cmu, quint_defa, apl_2019_T, cat_nbatc
  ) %>%
  mutate(
    EXPO = ifelse(EXPO == "Exposé", 1, 0),
    sexe = ifelse(sexe == "Masculin", 1, 0),
    combi_acs_cmu = ifelse(combi_acs_cmu == "Oui" & !is.na(combi_acs_cmu), 1, 0),
    quint_defa1 = ifelse(quint_defa == "Quintile n°1", 1, 0),
    quint_defa2 = ifelse(quint_defa == "Quintile n°2", 1, 0),
    quint_defa3 = ifelse(quint_defa == "Quintile n°3", 1, 0),
    quint_defa4 = ifelse(quint_defa == "Quintile n°4", 1, 0),
    quint_defa5 = ifelse(quint_defa == "Quintile n°5", 1, 0),
    APL1 = ifelse(apl_2019_T == "Tertile n°1", 1, 0),
    APL2 = ifelse(apl_2019_T == "Tertile n°2", 1, 0),
    APL3 = ifelse(apl_2019_T == "Tertile n°3", 1, 0),
    cat_nbatc1 = ifelse(cat_nbatc == "<2 classes ATC7 différentes", 1, 0),
    cat_nbatc2 = ifelse(cat_nbatc == "de 2 à 4 classes ATC7 différentes", 1, 0),
    cat_nbatc3 = ifelse(cat_nbatc == "de 5 à 9", 1, 0),
    cat_nbatc4 = ifelse(cat_nbatc == "10 ou plus", 1, 0)
  ) %>%
  dplyr::select(
    -sexe, -quint_defa, -apl_2019_T, -cat_nbatc
  )
  
    
  ## Draw love plot
  love.plot = function(cov, treat,  ## cov is the matrix of covariates and treat is a vector of treatment assignment
                       weights = rep(1, length(treat)),
                       plot = F) 
  {
    
    ## mean with normalized weights \sum w_i x_i / (\sum w_i)
    treat.means <- colSums(cov[treat == 1,] * weights[treat == 1], na.rm = TRUE)/sum(weights[treat == 1], na.rm = TRUE)
    treat.var <- colSums(t(t(cov[treat == 1,]) - treat.means)^2 *
                           weights[treat == 1], na.rm = TRUE)/sum(weights[treat == 1], na.rm = TRUE)
    
    control.means <- colSums(cov[treat == 0,] * weights[treat == 0], na.rm = TRUE)/sum(weights[treat == 0], na.rm = TRUE)
    control.var <- colSums(t(t(cov[treat == 0,]) - control.means)^2 *
                             weights[treat == 0], na.rm = TRUE)/sum(weights[treat == 0], na.rm = TRUE)
    
    ## the standardized mean differences for every covariate
    smd <- (treat.means - control.means)/sqrt((treat.var + control.var)/2)
    names(smd) <- colnames(cov)
    
    if (plot == T) {
      plot.data <- data.frame(smd = smd, covariates = names(smd))
      range <- max(abs(smd))
      ggplot(plot.data) + geom_point(aes(x = as.numeric(smd), y = covariates)) +
        geom_vline(xintercept = 0) + xlim(-range, range) +
        labs(x = 'Standardized Difference in Means')
    }
    return(smd)
  }

  raw.smd <- love.plot(data_diff[, 2:17], data_diff$EXPO)
  weighted.smd <- love.plot(data_diff[, 2:17], data_diff$EXPO, weights = WEIGHTS)
  
  
  plot.data <- data.frame(smd = c(raw.smd, weighted.smd), 
                          covariates = c(names(raw.smd), names(weighted.smd)),
                          category = c(rep("Original", length(raw.smd)), rep("IPW", length(weighted.smd))))
  range <- max(abs(plot.data$smd))
  
  ggplot(plot.data) + geom_point(aes(x = as.numeric(smd), y = covariates, color = category)) +
    geom_vline(xintercept = c(-0.1, -0.05, 0, 0.05, 0.1),
               linetype = c("solid", "dashed", "solid", "dashed", "solid")) + 
    xlim(-range, range) +
    labs(x = 'Standardized Difference in Means')







#### __TTT ~ EXPO + ATC + weight ####


datam <- dataf[, c("EXPO", "atc7", "etime", "event")]
data_fg1 <- finegray(Surv(etime, event) ~ ., data = datam, etype = "Arrêt", weights = WEIGHTS)
fgfit1 <- coxph(Surv(fgstart, fgstop, fgstatus) ~ EXPO + atc7, weight = fgwt, data = data_fg1)
fgfit1
data_fg2 <- finegray(Surv(etime, event) ~ ., data = datam, etype = "Substitution", weights = WEIGHTS)
fgfit2 <- coxph(Surv(fgstart, fgstop, fgstatus) ~ EXPO + atc7, weight = fgwt, data = data_fg2)
fgfit2


paste0(formatC(exp(coef(fgfit1)), 2, format = "f")," [",
       formatC(exp(confint(fgfit1)[,1]), 2, format = "f"),"-",
       formatC(exp(confint(fgfit1)[,2]), 2, format = "f"),"]")
paste0(formatC(exp(coef(fgfit2)), 2, format = "f")," [",
       formatC(exp(confint(fgfit2)[,1]), 2, format = "f"),"-",
       formatC(exp(confint(fgfit2)[,2]), 2, format = "f"),"]")
