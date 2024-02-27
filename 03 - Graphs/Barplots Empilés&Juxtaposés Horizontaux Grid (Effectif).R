
#### -_-_-_ Barres -_-_-_ ######

### Symptome  ####
# Occurence
SPT_OMS_1_1_2 <- data.frame(
  Symptome = names(sort(apply(SPT_OMS_1_1[DATA$Premier_episode_bin == "Positif", -1], 2, sum, na.rm = TRUE))),
  N1 = sort(apply(SPT_OMS_1_1[DATA$Premier_episode_bin == "Positif", -1], 2, sum, na.rm = TRUE)) 
)
SPT_OMS_1_1_2$Symptome <- factor(SPT_OMS_1_1_2$Symptome, levels = SPT_OMS_1_1_2$Symptome)
LEVELS <- SPT_OMS_1_1_2$Symptome
#3 mois
SPT_OMS_1_2_2 <- data.frame(
  Symptome = names(sort(apply(SPT_OMS_1_2[DATA$Premier_episode_bin == "Positif", -1], 2, sum, na.rm = TRUE))),
  N2 = sort(apply(SPT_OMS_1_2[DATA$Premier_episode_bin == "Positif", -1], 2, sum, na.rm = TRUE)) 
)
SPT_OMS_1_2_2$Symptome <- factor(SPT_OMS_1_2_2$Symptome, levels = LEVELS)
#Actuellemnt
SPT_OMS_1_3_2 <- data.frame(
  Symptome = names(sort(apply(SPT_OMS_1_3[DATA$Premier_episode_bin == "Positif", -1], 2, sum, na.rm = TRUE))),
  N3 = sort(apply(SPT_OMS_1_3[DATA$Premier_episode_bin == "Positif", -1], 2, sum, na.rm = TRUE)) 
)
SPT_OMS_1_3_2$Symptome <- factor(SPT_OMS_1_3_2$Symptome, levels = LEVELS)
#Merge des trois
SPT_OMS_1 <- left_join(left_join(SPT_OMS_1_1_2, SPT_OMS_1_2_2, by = "Symptome"), SPT_OMS_1_3_2, by = "Symptome")
SPT_OMS_1$N <- SPT_OMS_1$N1
SPT_OMS_1$N1 <- SPT_OMS_1$N1 - SPT_OMS_1$N2
SPT_OMS_1$N2 <- SPT_OMS_1$N2 - SPT_OMS_1$N3

# Symptome dernier episode
# Occurence
SPT_OMS_2_1_2 <- data.frame(
  Symptome = names(sort(apply(SPT_OMS_2_1[DATA$Dernier_episode_bin == "Positif", -1], 2, sum, na.rm = TRUE))),
  N1 = sort(apply(SPT_OMS_2_1[DATA$Dernier_episode_bin == "Positif", -1], 2, sum, na.rm = TRUE)) 
)
SPT_OMS_2_1_2$Symptome <- factor(SPT_OMS_2_1_2$Symptome, levels = LEVELS)
#3 mois
SPT_OMS_2_2_2 <- data.frame(
  Symptome = names(sort(apply(SPT_OMS_2_2[DATA$Dernier_episode_bin == "Positif", -1], 2, sum, na.rm = TRUE))),
  N2 = sort(apply(SPT_OMS_2_2[DATA$Dernier_episode_bin == "Positif", -1], 2, sum, na.rm = TRUE)) 
)
SPT_OMS_2_2_2$Symptome <- factor(SPT_OMS_2_2_2$Symptome, levels = LEVELS)
#Actuellemnt
SPT_OMS_2_3_2 <- data.frame(
  Symptome = names(sort(apply(SPT_OMS_2_3[DATA$Dernier_episode_bin == "Positif", -1], 2, sum, na.rm = TRUE))),
  N3 = sort(apply(SPT_OMS_2_3[DATA$Dernier_episode_bin == "Positif", -1], 2, sum, na.rm = TRUE)) 
)
SPT_OMS_2_3_2$Symptome <- factor(SPT_OMS_2_3_2$Symptome, levels = LEVELS)
#Merge des trois
SPT_OMS_2 <- left_join(left_join(SPT_OMS_2_1_2, SPT_OMS_2_2_2, by = "Symptome"), SPT_OMS_2_3_2, by = "Symptome")
SPT_OMS_2$N <- SPT_OMS_2$N1
SPT_OMS_2$N1 <- SPT_OMS_2$N1 - SPT_OMS_2$N2
SPT_OMS_2$N2 <- SPT_OMS_2$N2 - SPT_OMS_2$N3


DATA_bar <- rbind(
  data.frame(
    Episode = "Premier Ã©pisode",
    rbind(
      data.frame(DurÃ©e = "Phase aigue", Symptome = SPT_OMS_1$Symptome, N = SPT_OMS_1$N1, Ntot = SPT_OMS_1$N),
      data.frame(DurÃ©e = "3 mois ou plus", Symptome = SPT_OMS_1$Symptome, N = SPT_OMS_1$N2, Ntot = SPT_OMS_1$N),
      data.frame(DurÃ©e = "Encore prÃ©sent", Symptome = SPT_OMS_1$Symptome, N = SPT_OMS_1$N3, Ntot = SPT_OMS_1$N)
    )#[DATA$Premier_episode_bin == "Positif",]
  ),
  data.frame(
    Episode = "Dernier Ã©pisode",
    rbind(
      data.frame(DurÃ©e = "Phase aigue", Symptome = SPT_OMS_2$Symptome, N = SPT_OMS_2$N1, Ntot = SPT_OMS_2$N),
      data.frame(DurÃ©e = "3 mois ou plus", Symptome = SPT_OMS_2$Symptome, N = SPT_OMS_2$N2, Ntot = SPT_OMS_2$N),
      data.frame(DurÃ©e = "Encore prÃ©sent", Symptome = SPT_OMS_2$Symptome, N = SPT_OMS_2$N3, Ntot = SPT_OMS_2$N)
    )
  )#[DATA$Dernier_episode_bin == "Positif",]
)
DATA_bar$Episode <- factor(DATA_bar$Episode, levels = c("Premier Ã©pisode", "Dernier Ã©pisode"))
DATA_bar$DurÃ©e <- factor(DATA_bar$DurÃ©e, levels = c("Encore prÃ©sent", "3 mois ou plus", "Phase aigue"))

DATA_bar$Symptome <- relevel_factor(DATA_bar$Symptome,
                                    new.levels = list("Depression" = "DÃ©pression",
                                                      "Anxiete" = "AnxiÃ©tÃ©",
                                                      "Acouphenes_Pb_Audition" = "AcouphÃ¨nes",
                                                      "Pb_Menstrution_Regles" = "ProblÃ¨mes de menstruation",
                                                      "Trouble_Vision" = "Vision Trouble",
                                                      "Trouble_Sommeil" = "Troubles du sommeil",
                                                      "Etourdissement" = "Etourdissements",
                                                      "Coryza" = "Rhume",
                                                      "Mal_de_gorge" = "Mal de gorge",
                                                      "Nouvelle_Allergies" = "Allergies nouvelles",
                                                      "Nevralgie" = "NÃ©vralgies",
                                                      "Douleur_Abdominale" = "Douleurs abdominales",
                                                      "Tachycardie_Palpitations" = "Tachycardie / Palpitations",
                                                      "Douleur_Thoracique" = "Douleurs thoraciques",
                                                      "Dysfonct_Cognitif_ET_Trouble_Memoire" = "Dysfonctionnement cognitif ",
                                                      "Alteration_Odorat_Gout" = "AltÃ©ration de lâodorat / du goÃ»t",
                                                      "Pb_Gastrique" = "ProblÃ¨mes gastro-intestinaux ",
                                                      "Essouflement" = "Essoufflement",
                                                      "Douleur_Articulaire" = "Douleurs articulaires",
                                                      "Toux" = "Toux",
                                                      "Cephalee" = "CÃ©phalÃ©es",
                                                      "Fievre_Intermitente" = "FiÃ¨vre intermittente",
                                                      "Fatigue" = "Fatigue"))

DATA_bar <- DATA_bar[DATA_bar$Symptome %ni% c("Etourdissements", "Troubles du sommeil", "Vision Trouble", 
                                              "ProblÃ¨mes de menstruation", "AnxiÃ©tÃ©", "DÃ©pression", "AcouphÃ¨nes"), ]
# CrÃ©ation du graphique
ggplot(
  DATA_bar
) + # Couche barplot
  geom_col(
    mapping = aes(N, 
                  Symptome,
                  fill = DurÃ©e),
    width = 0.6,
    position = "stack"
  ) + 
  facet_grid(
    cols = vars(Episode)
  )  +
  geom_vline(
    aes(xintercept = 0)
  ) + # Couche axe x
  scale_x_continuous(
    limits = c(0, 6250),
    breaks = seq(0, 6000, 1000),
    minor_breaks = seq(500, 5500, 500),
    expand = c(0, 0)
  ) + # Couche labels des barres
  # geom_text(
  #   mapping = aes(0, y = Symptome, label = Symptome),
  #   hjust = 0,
  #   nudge_x = 1
  # ) + # Couche labels des barres
  geom_text(
    mapping = aes(Ntot+25, y = Symptome, label = Ntot),
    hjust = 0,
    nudge_x = 1
  ) + # Couche Titre
  labs(
    title = " ",
    subtitle = ""
  ) + # Couche style
  theme(
    axis.title = element_blank(),
    #axis.text.x = element_blank(),
    #axis.text.y = element_blank(),
    panel.background = element_rect(fill = "white"),
    panel.grid.major.x = element_line(color = "darkgray"),
    panel.grid.minor.x = element_line(colour = "lightgray")
  )





#### Symptomes + Statut ####

# apply(SPT_OMS_1_1[DATA$Premier_episode_bin == "Positif", -1], 2, 
#       function(x){
#         by(x, DATA$Q_A2[DATA$Premier_episode_bin == "Positif"], sum, na.rm = TRUE)
#       })
# Occurence
SPT_OMS_1_1_2 <- cbind(
  Symptome = names(apply(SPT_OMS_1_1[DATA$Premier_episode_bin == "Positif", -1], 2, sum, na.rm = TRUE)),
  as.data.frame(t(apply(SPT_OMS_1_1[DATA$Premier_episode_bin == "Positif", -1], 2, 
                        function(x){
                          by(x, DATA$Q_A2[DATA$Premier_episode_bin == "Positif"], sum, na.rm = TRUE)
                        })))
)
SPT_OMS_1_1_2 <- data.frame(
  Symptome = rep(as.character(SPT_OMS_1_1_2$Symptome), 3),
  Statut = rep(c("Famille de militaire", "Militaire dâactive", "RetraitÃ© militaire"), each = length(LEVELS)),
  N1 = as.numeric(c(SPT_OMS_1_1_2$`Famille de militaire`, SPT_OMS_1_1_2$`Militaire dâactive`, SPT_OMS_1_1_2$`RetraitÃ© militaire`))
)
SPT_OMS_1_1_2$Symptome <- factor(SPT_OMS_1_1_2$Symptome, levels = LEVELS)
#3 mois
SPT_OMS_1_2_2 <- cbind(
  Symptome = names(apply(SPT_OMS_1_2[DATA$Premier_episode_bin == "Positif", -1], 2, sum, na.rm = TRUE)),
  as.data.frame(t(apply(SPT_OMS_1_2[DATA$Premier_episode_bin == "Positif", -1], 2, 
                        function(x){
                          by(x, DATA$Q_A2[DATA$Premier_episode_bin == "Positif"], sum, na.rm = TRUE)
                        })))
)
SPT_OMS_1_2_2 <- data.frame(
  Symptome = rep(as.character(SPT_OMS_1_2_2$Symptome), 3),
  Statut = rep(c("Famille de militaire", "Militaire dâactive", "RetraitÃ© militaire"), each = length(LEVELS)),
  N2 = as.numeric(c(SPT_OMS_1_2_2$`Famille de militaire`, SPT_OMS_1_2_2$`Militaire dâactive`, SPT_OMS_1_2_2$`RetraitÃ© militaire`))
)
SPT_OMS_1_2_2$Symptome <- factor(SPT_OMS_1_2_2$Symptome, levels = LEVELS)
#Actuellemnt
SPT_OMS_1_3_2 <- cbind(
  Symptome = names(apply(SPT_OMS_1_3[DATA$Premier_episode_bin == "Positif", -1], 2, sum, na.rm = TRUE)),
  as.data.frame(t(apply(SPT_OMS_1_3[DATA$Premier_episode_bin == "Positif", -1], 2, 
                        function(x){
                          by(x, DATA$Q_A2[DATA$Premier_episode_bin == "Positif"], sum, na.rm = TRUE)
                        })))
)
SPT_OMS_1_3_2 <- data.frame(
  Symptome = rep(as.character(SPT_OMS_1_3_2$Symptome), 3),
  Statut = rep(c("Famille de militaire", "Militaire dâactive", "RetraitÃ© militaire"), each = length(LEVELS)),
  N3 = as.numeric(c(SPT_OMS_1_3_2$`Famille de militaire`, SPT_OMS_1_3_2$`Militaire dâactive`, SPT_OMS_1_3_2$`RetraitÃ© militaire`))
)
SPT_OMS_1_3_2$Symptome <- factor(SPT_OMS_1_3_2$Symptome, levels = LEVELS)
#Merge des trois
SPT_OMS_1 <- left_join(left_join(SPT_OMS_1_1_2, SPT_OMS_1_2_2, by = c("Symptome" = "Symptome", "Statut" = "Statut")), 
                       SPT_OMS_1_3_2, by = c("Symptome" = "Symptome", "Statut" = "Statut"))
SPT_OMS_1$N <- SPT_OMS_1$N1
SPT_OMS_1$N1 <- SPT_OMS_1$N1 - SPT_OMS_1$N2
SPT_OMS_1$N2 <- SPT_OMS_1$N2 - SPT_OMS_1$N3
SPT_OMS_1 <- SPT_OMS_1 %>% arrange(Statut, Symptome)

# Symptome dernier episode
# Occurence
SPT_OMS_2_1_2 <- cbind(
  Symptome = names(apply(SPT_OMS_2_1[DATA$Premier_episode_bin == "Positif", -1], 2, sum, na.rm = TRUE)),
  as.data.frame(t(apply(SPT_OMS_2_1[DATA$Premier_episode_bin == "Positif", -1], 2, 
                        function(x){
                          by(x, DATA$Q_A2[DATA$Premier_episode_bin == "Positif"], sum, na.rm = TRUE)
                        })))
)
SPT_OMS_2_1_2 <- data.frame(
  Symptome = rep(as.character(SPT_OMS_2_1_2$Symptome), 3),
  Statut = rep(c("Famille de militaire", "Militaire dâactive", "RetraitÃ© militaire"), each = length(LEVELS)),
  N1 = as.numeric(c(SPT_OMS_2_1_2$`Famille de militaire`, SPT_OMS_2_1_2$`Militaire dâactive`, SPT_OMS_2_1_2$`RetraitÃ© militaire`))
)
SPT_OMS_2_1_2$Symptome <- factor(SPT_OMS_2_1_2$Symptome, levels = LEVELS)
#3 mois
SPT_OMS_2_2_2 <- cbind(
  Symptome = names(apply(SPT_OMS_2_2[DATA$Premier_episode_bin == "Positif", -1], 2, sum, na.rm = TRUE)),
  as.data.frame(t(apply(SPT_OMS_2_2[DATA$Premier_episode_bin == "Positif", -1], 2, 
                        function(x){
                          by(x, DATA$Q_A2[DATA$Premier_episode_bin == "Positif"], sum, na.rm = TRUE)
                        })))
)
SPT_OMS_2_2_2 <- data.frame(
  Symptome = rep(as.character(SPT_OMS_2_2_2$Symptome), 3),
  Statut = rep(c("Famille de militaire", "Militaire dâactive", "RetraitÃ© militaire"), each = length(LEVELS)),
  N2 = as.numeric(c(SPT_OMS_2_2_2$`Famille de militaire`, SPT_OMS_2_2_2$`Militaire dâactive`, SPT_OMS_2_2_2$`RetraitÃ© militaire`))
)
SPT_OMS_2_2_2$Symptome <- factor(SPT_OMS_2_2_2$Symptome, levels = LEVELS)
#Actuellemnt
SPT_OMS_2_3_2 <- cbind(
  Symptome = names(apply(SPT_OMS_2_3[DATA$Premier_episode_bin == "Positif", -1], 2, sum, na.rm = TRUE)),
  as.data.frame(t(apply(SPT_OMS_2_3[DATA$Premier_episode_bin == "Positif", -1], 2, 
                        function(x){
                          by(x, DATA$Q_A2[DATA$Premier_episode_bin == "Positif"], sum, na.rm = TRUE)
                        })))
)
SPT_OMS_2_3_2 <- data.frame(
  Symptome = rep(as.character(SPT_OMS_2_3_2$Symptome), 3),
  Statut = rep(c("Famille de militaire", "Militaire dâactive", "RetraitÃ© militaire"), each = length(LEVELS)),
  N3 = as.numeric(c(SPT_OMS_2_3_2$`Famille de militaire`, SPT_OMS_2_3_2$`Militaire dâactive`, SPT_OMS_2_3_2$`RetraitÃ© militaire`))
)
SPT_OMS_2_3_2$Symptome <- factor(SPT_OMS_2_3_2$Symptome, levels = LEVELS)
#Merge des trois
SPT_OMS_2 <- left_join(left_join(SPT_OMS_2_1_2, SPT_OMS_2_2_2, by = c("Symptome" = "Symptome", "Statut" = "Statut")), 
                       SPT_OMS_2_3_2, by = c("Symptome" = "Symptome", "Statut" = "Statut"))
SPT_OMS_2$N <- SPT_OMS_2$N1
SPT_OMS_2$N1 <- SPT_OMS_2$N1 - SPT_OMS_2$N2
SPT_OMS_2$N2 <- SPT_OMS_2$N2 - SPT_OMS_2$N3
SPT_OMS_2 <- SPT_OMS_2 %>% arrange(Statut, Symptome)


DATA_bar <- rbind(
  data.frame(
    Episode = "Premier Ã©pisode",
    rbind(
      data.frame(DurÃ©e = "Phase aigue", Symptome = SPT_OMS_1$Symptome, Statut = SPT_OMS_1$Statut,  N = SPT_OMS_1$N1, Ntot = SPT_OMS_1$N),
      data.frame(DurÃ©e = "3 mois ou plus", Symptome = SPT_OMS_1$Symptome, Statut = SPT_OMS_1$Statut, N = SPT_OMS_1$N2, Ntot = SPT_OMS_1$N),
      data.frame(DurÃ©e = "Encore prÃ©sent", Symptome = SPT_OMS_1$Symptome, Statut = SPT_OMS_1$Statut, N = SPT_OMS_1$N3, Ntot = SPT_OMS_1$N)
    )#[DATA$Premier_episode_bin == "Positif",]
  ),
  data.frame(
    Episode = "Dernier Ã©pisode",
    rbind(
      data.frame(DurÃ©e = "Phase aigue", Symptome = SPT_OMS_2$Symptome, Statut = SPT_OMS_1$Statut, N = SPT_OMS_2$N1, Ntot = SPT_OMS_2$N),
      data.frame(DurÃ©e = "3 mois ou plus", Symptome = SPT_OMS_2$Symptome, Statut = SPT_OMS_1$Statut, N = SPT_OMS_2$N2, Ntot = SPT_OMS_2$N),
      data.frame(DurÃ©e = "Encore prÃ©sent", Symptome = SPT_OMS_2$Symptome, Statut = SPT_OMS_1$Statut, N = SPT_OMS_2$N3, Ntot = SPT_OMS_2$N)
    )
  )#[DATA$Dernier_episode_bin == "Positif",]
)
DATA_bar$Episode <- factor(DATA_bar$Episode, levels = c("Premier Ã©pisode", "Dernier Ã©pisode"))
DATA_bar$DurÃ©e <- factor(DATA_bar$DurÃ©e, levels = c("Encore prÃ©sent", "3 mois ou plus", "Phase aigue"))

DATA_bar$Symptome <- relevel_factor(DATA_bar$Symptome,
                                    new.levels = list("Depression" = "DÃ©pression",
                                                      "Anxiete" = "AnxiÃ©tÃ©",
                                                      "Acouphenes_Pb_Audition" = "AcouphÃ¨nes",
                                                      "Pb_Menstrution_Regles" = "ProblÃ¨mes de menstruation",
                                                      "Trouble_Vision" = "Vision Trouble",
                                                      "Trouble_Sommeil" = "Troubles du sommeil",
                                                      "Etourdissement" = "Etourdissements",
                                                      "Coryza" = "Rhume",
                                                      "Mal_de_gorge" = "Mal de gorge",
                                                      "Nouvelle_Allergies" = "Allergies nouvelles",
                                                      "Nevralgie" = "NÃ©vralgies",
                                                      "Douleur_Abdominale" = "Douleurs abdominales",
                                                      "Tachycardie_Palpitations" = "Tachycardie / Palpitations",
                                                      "Douleur_Thoracique" = "Douleurs thoraciques",
                                                      "Dysfonct_Cognitif_ET_Trouble_Memoire" = "Dysfonctionnement cognitif ",
                                                      "Alteration_Odorat_Gout" = "AltÃ©ration de lâodorat / du goÃ»t",
                                                      "Pb_Gastrique" = "ProblÃ¨mes gastro-intestinaux ",
                                                      "Essouflement" = "Essoufflement",
                                                      "Douleur_Articulaire" = "Douleurs articulaires",
                                                      "Toux" = "Toux",
                                                      "Cephalee" = "CÃ©phalÃ©es",
                                                      "Fievre_Intermitente" = "FiÃ¨vre intermittente",
                                                      "Fatigue" = "Fatigue"))

DATA_bar <- DATA_bar[DATA_bar$Symptome %ni% c("Etourdissements", "Troubles du sommeil", "Vision Trouble", 
                                              "ProblÃ¨mes de menstruation", "AnxiÃ©tÃ©", "DÃ©pression", "AcouphÃ¨nes"), ]

# CrÃ©ation du graphique
ggplot(
  DATA_bar
) + # Couche barplot
  geom_col(
    mapping = aes(N, 
                  Symptome,
                  fill = DurÃ©e),
    width = 0.6,
    position = "stack"
  ) + 
  facet_grid(
    cols = vars(Episode),
    rows = vars(Statut)
  )  +
  geom_vline(
    aes(xintercept = 0)
  ) + # Couche axe x
  scale_x_continuous(
    limits = c(0, 3750),
    breaks = seq(0, 3000, 1000),
    minor_breaks = seq(500, 3500, 500),
    expand = c(0, 0)
  ) + # Couche labels des barres
  geom_text(
    mapping = aes(Ntot+25, y = Symptome, label = Ntot),
    hjust = 0,
    nudge_x = 1
  ) + # Couche Titre
  labs(
    title = " ",
    subtitle = ""
  ) + # Couche style
  theme(
    axis.title = element_blank(),
    #axis.text.x = element_blank(),
    #axis.text.y = element_blank(),
    panel.background = element_rect(fill = "white"),
    panel.grid.major.x = element_line(color = "darkgray"),
    panel.grid.minor.x = element_line(colour = "lightgray")
  )



#### Comorbidites ####
sel <- c("Q_A2", "COVID_long_3mod",
         "Q_C4_1", "Q_C4_2", "Q_C4_3", "Q_C4_4", "Q_C4_5", "Q_C4_6", "Q_C4_7", "Q_C4_8", 
         "Q_C4_9", "Q_C4_10", "Q_C4_11", "Q_C4_12", "Q_C4_13", "Q_C4_14")
DATA_plot <- DATA[, sel]
colnames(DATA_plot) <- c("Statut", "Etat",
                         "ObÃ©sitÃ©", "DiabÃ¨te", "Hypertension", "Cardiopathie", "Maladie pulmonaire", 
                         "Maladie rÃ©nale chronique", "Maladie neurologique", "Maladie du foie", 
                         "Cancer ou hÃ©mopathie", "Transplantation organes", "Maladie auto-immmune", 
                         "Deficit immunitaire", "DrÃ©panocytose", "ATCD splÃ©nectomie")
DATA_plot[, -c(1:2)] <- apply(DATA_plot[, -c(1:2)], 2, function(x) {
  x[x == "Oui"] <- 1
  x[x == "Non"] <- 0
  as.numeric(x)
})





DATA_bar <- DATA_plot %>%
  pivot_longer(c("ObÃ©sitÃ©", "DiabÃ¨te", "Hypertension", "Cardiopathie", "Maladie pulmonaire", 
                 "Maladie rÃ©nale chronique", "Maladie neurologique", "Maladie du foie", 
                 "Cancer ou hÃ©mopathie", "Transplantation organes", "Maladie auto-immmune", 
                 "Deficit immunitaire", "DrÃ©panocytose", "ATCD splÃ©nectomie")) %>%
  group_by(Etat, name) %>%
  summarise(N = sum(value)) %>%
  left_join(data.frame(table(DATA$COVID_long_3mod)), 
            by = c("Etat" = "Var1")) %>%
  mutate(Val = round(N/Freq*100, 1),
         lower = epitools::pois.exact(N, Freq)[, "lower"] * 100,
         upper = epitools::pois.exact(N, Freq)[, "upper"] * 100)

DATA_bar$name <- factor(DATA_bar$name,
                        levels = DATA_bar %>%
                          group_by(name) %>%
                          summarise(N = sum(N)) %>%
                          arrange(N) %>%
                          dplyr::select(name) %>% 
                          pull())





ggplot(
  DATA_bar
) + # Couche barplot
  geom_col(
    mapping = aes(Val, 
                  name,
                  fill = Etat),
    width = 0.8,
    position = "dodge"
  ) +
  geom_vline(
    aes(xintercept = 0)
  ) + # Couche axe x
  scale_x_continuous(
    limits = c(0, 15),
    breaks = seq(0, 15, 1),
    minor_breaks = seq(.5, 14.5, 1),
    expand = c(0, 0),
    labels = paste0(seq(0, 15, 1), "%")
  ) + # Couche labels des barres
  geom_text(
    mapping = aes(Val+.1, 
                  y = name, 
                  label = Val,
                  group = Etat),
    hjust = 0,
    position = position_dodge(width = 0.9),
    size = 3
  ) +
  scale_fill_manual(
    "Covid",
    values = c("#F8766D", "#00BA38", "#619CFF"),
    labels = c("Covid-19-",
               "Covid-19+ sans APC probable",
               "APC probable")
  ) +
  # geom_crossbar(
  #   aes(
  #     y = name,
  #     x = Val,
  #     xmin = lower,
  #     xmax = upper,
  #     group = Etat,
  #     width = 0.2
  #   ),
  #   position = position_dodge(width = 0.9)
  # ) + # Couche Titre
labs(
  title = " ",
  subtitle = ""
) + # Couche style
  theme(
    axis.title = element_blank(),
    #axis.text.x = element_blank(),
    #axis.text.y = element_blank(),
    panel.background = element_rect(fill = "white"),
    panel.grid.major.x = element_line(color = "darkgray"),
    panel.grid.minor.x = element_line(colour = "lightgray")
  )








#### Comorbidites + Statut ####
DATA_bar <- DATA_plot %>%
  pivot_longer(c("ObÃ©sitÃ©", "DiabÃ¨te", "Hypertension", "Cardiopathie", "Maladie pulmonaire", 
                 "Maladie rÃ©nale chronique", "Maladie neurologique", "Maladie du foie", 
                 "Cancer ou hÃ©mopathie", "Transplantation organes", "Maladie auto-immmune", 
                 "Deficit immunitaire", "DrÃ©panocytose", "ATCD splÃ©nectomie")) %>%
  group_by(Statut, Etat, name) %>%
  summarise(N = sum(value)) %>%
  left_join(DATA %>%
              group_by(Q_A2, COVID_long_3mod) %>%
              summarise(Ntot = n()), 
            by = c("Etat" = "COVID_long_3mod", "Statut" = "Q_A2")) %>%
  mutate(Val = round(N/Ntot*100, 1))




DATA_bar$name <- factor(DATA_bar$name,
                        levels = DATA_bar %>%
                          group_by(name) %>%
                          summarise(N = sum(N)) %>%
                          arrange(N) %>%
                          dplyr::select(name) %>% 
                          pull())



ggplot(
  DATA_bar
) + # Couche barplot
  geom_col(
    mapping = aes(Val, 
                  name,
                  fill = Etat),
    width = 0.8,
    position = "dodge"
  ) + 
  facet_grid(
    cols = vars(Statut)
  ) +
  geom_vline(
    aes(xintercept = 0)
  ) + # Couche axe x
  scale_x_continuous(
    limits = c(0, 18),
    breaks = seq(0, 16, 2),
    minor_breaks = seq(1, 17, 2),
    expand = c(0, 0),
    labels = paste0(seq(0, 16, 2), "%")
  ) + # Couche labels des barres
  geom_text(
    mapping = aes(Val+.1, 
                  y = name, 
                  label = Val,
                  group = Etat),
    hjust = 0,
    position = position_dodge(width = 0.9),
    size = 3
  ) + # Couche Titre
  labs(
    title = " ",
    subtitle = ""
  ) + # Couche style
  theme(
    axis.title = element_blank(),
    #axis.text.x = element_blank(),
    #axis.text.y = element_blank(),
    panel.background = element_rect(fill = "white"),
    panel.grid.major.x = element_line(color = "darkgray"),
    panel.grid.minor.x = element_line(colour = "lightgray")
  ) +
  scale_fill_manual(
    "Covid",
    values = c("#F8766D", "#00BA38", "#619CFF"),
    labels = c("Covid-19-",
               "Covid-19+ sans APC probable",
               "APC probable")
  )
