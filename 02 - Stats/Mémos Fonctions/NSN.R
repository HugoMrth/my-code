#### Regression logistique ####

NSN_reglog <- function(p0, p1, 
                       alpha = 0.05, power = 0.80) {

    #Calcul des paramètres
  z_alpha <- abs(qnorm(p = alpha/2))
  z_beta <- abs(qnorm(p = power))
  
  beta <- log((p1*(1-p0)/(p0*(1-p1))))
  
  N <- (z_alpha + z_beta)^2 / (p0 * (1 - p0) * beta^2)
  return(N = N)
}

# NSN_reglog(p0 = 0.75, p1 = 0.85)
# NSN_reglog(p0 = 0.15, p1 = 0.25)





#### Prevalence ####

NSN_prevalence <- function(p, d, alpha = 0.05) {
  
  #Calcul des paramètres
  z_alpha <- abs(qnorm(p = alpha/2))
  
  N <- (z_alpha^2 * p * (1 - p))/(d^2)
  return(N = N)
}

# NSN_prevalence(p = 0.2, d = 0.02)




#### Cohorte ####

NSN_cohorte <- function(incidence.exp, incidence.non.exp = NULL,
                        prop.traitement, fact.risque = NULL,
                        temps.etude, 
                        alpha = 0.05, power = 0.80) {
  # browser()
  #Gestion cas incidence non exp vs facteur de risque
  if (is.null(incidence.non.exp)) {
    incidence.non.exp <- incidence.exp * fact.risque
  }
  
  #Calcul des paramètres
  d <- abs(incidence.exp - incidence.non.exp)
  gamma <- (1 - prop.traitement)/prop.traitement
  mu <- (incidence.exp + gamma * incidence.non.exp) / (1 + gamma)
  z_alpha <- abs(qnorm(p = alpha/2))
  z_beta <- abs(qnorm(p = power))
  
  #Calcul du NSN
  N.exp <- (z_alpha * sqrt((1 + gamma) * mu * (1 - mu)) + 
              z_beta * sqrt((1 + incidence.exp) * incidence.non.exp * (1 - incidence.non.exp)))^2 /
    (gamma * d^2)
  
  N.non.exp <- gamma * N.exp
  N.temps <- N.exp + N.non.exp
  N <- N.temps / temps.etude
  
  return(list(N.exp = N.exp,
              N.non.exp = N.non.exp,
              N.temps = N.temps,
              N = N))
}

# NSN_cohorte(incidence.exp = 2/1000, fact.risque = 3,
#             prop.traitement = 0.3, temps.etude = 5)



#### Sensibilite & Specificite ####

N <- data.frame(
  sensi = 0.9, 
  alpha = 0.05,
  precis = 0.1,
  speci = rep(c(0.7, 0.9), each = 4),
  prev = rep(rep(c(0.1, 0.588), each = 2), 2),
  drop = rep(rep(c(0.1, 0.2), 4))
)

N$N_sensi <- ceiling((qnorm(N$alpha/2)^2*(N$sensi*(1-N$sensi)/N$precis^2))/N$prev) 
N$N_speci <- ceiling((qnorm(N$alpha/2)^2*(N$speci*(1-N$speci)/N$precis^2))/(1-N$prev))
N$NSN <- ceiling(apply(N, 1, function(x) {max(x["N_sensi"], x["N_speci"]) / (1-x["drop"])}))

N
