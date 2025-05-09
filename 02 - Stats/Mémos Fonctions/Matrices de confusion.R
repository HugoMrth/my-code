rm(list = ls())
library(NMOF)


sensibilite <- function(table) {
  return(table[2,2]/(sum(table[, 2])))
}
specificite <- function(table) {
  return(table[1,1]/(sum(table[, 1])))
}
prevalence <- function(table) {
  return(sum(table[,2])/(sum(table)))
}

# VPP <- function(table) {
#   return((sensibilite(table) * prevalence(table)) /
#            ((sensibilite(table) * prevalence(table)) + (1 - specificite(table)) * (1 - prevalence(table))))
# }
# VPP(matrix(c(50, 10, 30, 10), ncol = 2))

VPP <- function(a, b, c, d) {
  table <- matrix(c(a, b, c, d), ncol = 2)
  return((sensibilite(table) * prevalence(table)) /
           ((sensibilite(table) * prevalence(table)) + (1 - specificite(table)) * (1 - prevalence(table))))
}
# VPP(50, 10, 30, 10)
# VPP(0.50, 0.10, 0.30, 0.10)


valeur_predictive_neg <- function(table) {
  return((specificite(table) * (1 - prevalence(table))) /
           (((1 - sensibilite(table)) * prevalence(table)) + specificite(table) * (1 - prevalence(table))))
}






acc <- 0.01
values <- seq(acc, 1-acc, acc)
parm_dt <- data.frame(a = 0, b = 0, c = 0, d = 0)
i <- 0

for (a in values) {
  for (b in values) {
    if (round(a + b, 3) > (1 - 2*acc)) next
    for (c in values) {
      if (round(a + b + c, 3) > (1 - acc)) next
      i <- i + 1
      parm_dt[i,] <- c(a, b, c, round(1 - a - b - c, 3))
      # for (d in seq(acc, 1-acc, acc)) {
      #   if (a + b + c + d != 1) next
      #   i <- i + 1
      #   parm_dt[i,] <- c(a, b, c, d)
      # }
    }
  }
}

parm_dt <- cbind(parm_dt,
                 prev = apply(parm_dt, 1, function(x) prevalence(matrix(c(x[1], x[2], x[3], x[4]), ncol = 2))),
                 sensi = apply(parm_dt, 1, function(x) sensibilite(matrix(c(x[1], x[2], x[3], x[4]), ncol = 2))),
                 speci = apply(parm_dt, 1, function(x) specificite(matrix(c(x[1], x[2], x[3], x[4]), ncol = 2))),
                VPP = apply(parm_dt, 1, function(x) VPP(x[1], x[2], x[3], x[4])),
                VPN = apply(parm_dt, 1, function(x) valeur_predictive_neg(matrix(c(x[1], x[2], x[3], x[4]), ncol = 2))))

save(parm_dt, file = "C:/Users/h.marthinet.CESPA1/Documents/Codes/00 - Codes & Scripts/02 - Stats/Fonctions/Matrices de confusion.Rdata")








gridSearchVPP <- function(target_vpp, acc, win) {
  values <- seq(acc, 1-acc, acc)
  parm_dt <- data.frame(a = 0, b = 0, c = 0, d = 0)
  i <- 0

  for (a in values) {
    for (b in values) {
      if (round(a + b, 3) > (1 - 2*acc)) next
      for (c in values) {
        if (round(a + b + c, 3) > (1 - acc)) next
        i <- i + 1
        parm_dt[i,] <- c(a, b, c, round(1 - a - b - c, 3))
        # for (d in seq(acc, 1-acc, acc)) {
        #   if (a + b + c + d != 1) next
        #   i <- i + 1
        #   parm_dt[i,] <- c(a, b, c, d)
        # }
      }
    }
  }

  vpp_dt <- cbind(parm_dt,
                  VPP = apply(parm_dt, 1, function(x) VPP(x[1], x[2], x[3], x[4])))
  vpp_dt <- vpp_dt[vpp_dt$VPP >= (target_vpp - win) &
                     vpp_dt$VPP <= (target_vpp + win),]
}


gs_vpp <- gridSearchVPP(target_vpp = 0.9, acc = 0.025, win = 0.025)
