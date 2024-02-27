

#### Krippendorf ####


cohen.kappa(table(ordered(DATA_bin[, "Scan_duod_pres"], levels = c("Pas d'atteinte", "Lésion Probable")),
                  ordered(DATA_bin[, "Chir_duodenum_pres"], levels = c("Absence de lésion", "Présence de lésion"))))

kappa_cohen <- function(table) {
  p0 <- (table[1,1] + table[2,2]) / (sum(table))
  p1 <- (colSums(table)[1] * rowSums(table)[1] / sum(table) +
    colSums(table)[2] * rowSums(table)[2] / sum(table)) / sum(table)

  return((p0 - p1)/(1 - p1))
}
kappa_fleiss <- function(table) {
  if (!(is.matrix(table))) {
    table <- as.matrix(table)
  }

  p0 <- 1
  for (i in 2:nrow(table)) {
    p0 <- p0 + (1/(sum(table[i-1, ])*(sum(table[i-1, ])-1))) *
      (sum(table[i, ]^2) - sum(table[i-1, ]))
  }
  p0 <- p0/nrow(table)

  p1 <- sum((colSums(table)/sum(table))^2)

  return((p0 - p1)/(1 - p1))
}
sensibilite <- function(table) {
  return(table[2,2]/(sum(table[, 2])))
}
specificite <- function(table) {
  return(table[1,1]/(sum(table[, 1])))
}
prevalence <- function(table) {
  return(sum(table[,2])/(sum(table)))
}
valeur_predictive_pos <- function(table) {
  return((sensibilite(table) * prevalence(table)) /
           ((sensibilite(table) * prevalence(table)) + (1 - specificite(table)) * (1 - prevalence(table))))
}
valeur_predictive_neg <- function(table) {
  return((specificite(table) * (1 - prevalence(table))) /
           (((1 - sensibilite(table)) * prevalence(table)) + specificite(table) * (1 - prevalence(table))))
}








#### Concordance v2 ####

c(
                 kripp.alpha(as.matrix(rbind(factor(DATA[, "Scan_appareil_urinaire"]),
                                             factor(DATA[, "Chir_appareil_urinaire"])),
                                       method = "nominal"))$value,
                 kripp.alpha(as.matrix(rbind(factor(DATA[, "Scan_bloc_duod_pancrea"]),
                                             factor(DATA[, "Chir_bloc_duod_pancrea"])),
                                       method = "nominal"))$value,
                 kripp.alpha(as.matrix(rbind(factor(DATA[, "Scan_organes_creux"]),
                                             factor(DATA[, "Chir_organes_creux"])),
                                       method = "nominal"))$value,
                 kripp.alpha(as.matrix(rbind(factor(DATA[, "Scan_pnp_pres"]),
                                             factor(DATA[, "Chir_organes_creux"])),
                                       method = "nominal"))$value,
                 kripp.alpha(as.matrix(rbind(factor(DATA[, "Scan_organes_pleins"]),
                                             factor(DATA[, "Chir_organes_pleins"])),









