
#### Tableau TM année ####

#Initialisation tableau final
tab_TM <- c("", "",
                  "Age", levels(DATA_DECES_2017_2022$Age),
                  "Sexe", levels(DATA_DECES_2017_2022$Sexe),
                  "Armée", levels(DATA_DECES_2017_2022$Armee),
                  "Modalité de décès", levels(DATA_DECES_2017_2022$Moda_DC),
                  "Lieu de survenue", levels(DATA_DECES_2017_2022$Lieu_Surv),
            "Total des décès")

for (i in 2017:2022) {
  #Inititalisation des vecteurs
  tab_n <- c(i, "N")
  tab_TI <- c(i, "TM")

  for (v in 2:6) {
    #Calcul des valeurs
    n <- table(DATA_DECES_2017_2022[DATA_DECES_2017_2022$Annee == i, colnames(DATA_DECES_2017_2022)[v]])
    pop <- BDD_EFFECTIFS_TOTAL_2017_2022[BDD_EFFECTIFS_TOTAL_2017_2022$Variable == colnames(DATA_DECES_2017_2022)[v], as.character(i)]

    #Incrementation des vecteurs
    tab_n <- c(tab_n, "", n)

    if (length(pop) != 0) {
      tab_TI <- c(tab_TI, "", round(n / pop * 100000, 2))
    } else {
      pop <- BDD_EFFECTIFS_TOTAL_2017_2022[BDD_EFFECTIFS_TOTAL_2017_2022$Variable == "Totaux", as.character(i)]
      tab_TI <- c(tab_TI, "", round(n / pop * 100000, 2))
    }
  }

  #Calcul des valeurs
  n <- nrow(DATA_DECES_2017_2022[DATA_DECES_2017_2022$Annee == i, ])
  pop <- BDD_EFFECTIFS_TOTAL_2017_2022[BDD_EFFECTIFS_TOTAL_2017_2022$Variable == "Totaux", as.character(i)]

  #Incrementation des vecteurs
  tab_n <- c(tab_n, n)
  tab_TI <- c(tab_TI, round(n / pop * 100000, 2))

  #Incrementation tableau final
  tab_TM <- cbind(tab_TM, tab_n, tab_TI)
}

tab_TM_print <- tab_TM

