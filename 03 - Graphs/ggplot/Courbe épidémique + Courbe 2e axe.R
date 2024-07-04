

############### -ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ- ##########################
#### Courbe EPI FG ####
DATA_FG_courbe <- DATA_FG %>%
  dplyr::filter(cas_grave == "Oui") %>%
  dplyr::select(cas_date) %>%
  # Regroupement par tranche de temps
  mutate(cas_date = as.POSIXct(cut.POSIXt(as.POSIXct(cas_date), breaks = "month")))


# Decoupage deu graphs : ticks
monthly_breaks <- seq.POSIXt(from = as_datetime("2020-01-01 00:00:00", tz = "CET"),
                             to = as_datetime("2023-04-01 00:00:00", tz = "CET"),
                             by = "month")

ymax <- max(table(DATA_FG_courbe$cas_date))

ggplot(
  DATA_FG_courbe
) + # Histogramme de base
  geom_histogram(
    mapping = aes(
      x = cas_date
    ),
    breaks = monthly_breaks,
    closed = "left",
    color = "white",
    fill = 'aquamarine3'
  ) + # Lignes horizontales
  geom_hline(
    yintercept = 1:ymax,
    color = "white"
  ) + # Lignes horizontales
  geom_vline(
    xintercept = as_datetime(c("2021-01-01 00:00:00", "2022-01-01 00:00:00"), tz = "CET"),
    color = "lavenderblush4"
  ) + # Gestion des labels & ticks de l'axe x
  scale_x_datetime(
    expand = c(0,0),
    date_breaks = "month",
    date_minor_breaks = "month",
    date_labels = "%m/%y"
  ) +
  theme_classic() +# Suppression grille horizontale
  theme(
    panel.grid.minor.y = element_line(
      colour = "lightgray",
      size = 1
    ),
    panel.grid.major.y = element_line(
      colour = "lavenderblush4",
      size = 1
    ),
    axis.text.x = element_text(
      angle = 90
    )
  ) + # Separation de l'axe y
  scale_y_continuous(
    expand = c(0,0),
    limits = c(0, ymax+1),
    breaks = seq(0, 90, 10)
  ) + # Titres
  labs(
    title = "",
    x = " ",
    y = "Nombre de formes graves"
  ) + geom_text(
    mapping = aes_q(
      x = as_datetime("2020-06-01 00:00:00", tz = "CET"),
      y = 82,
      label = "Période 1"
    )
  ) + geom_text(
    mapping = aes_q(
      x = as_datetime("2021-07-01 00:00:00", tz = "CET"),
      y = 82,
      label = "Période 2"
    )
  ) + geom_text(
    mapping = aes_q(
      x = as_datetime("2022-08-01 00:00:00", tz = "CET"),
      y = 82,
      label = "Période 3"
    )
  )


#### Courbe EPI EIG ####
DATA_EIG_courbe <- DATA_EIG %>%
  dplyr::filter(effet_type == "graves") %>%
  dplyr::select(dat_decla) %>%
  # Regroupement par tranche de temps
  mutate(dat_decla = as.POSIXct(cut.POSIXt(as.POSIXct(dat_decla), breaks = "month")))



ymax <- max(table(DATA_EIG_courbe$dat_decla))

ggplot(
  DATA_EIG_courbe
) + # Histogramme de base
  geom_histogram(
    mapping = aes(
      x = dat_decla
    ),
    breaks = monthly_breaks,
    closed = "left",
    color = "white",
    fill = 'aquamarine3'
  ) + # Lignes horizontales
  geom_hline(
    yintercept = 1:ymax,
    color = "white"
  ) + # Lignes horizontales
  geom_vline(
    xintercept = as_datetime(c("2021-01-01 00:00:00", "2022-01-01 00:00:00"), tz = "CET"),
    color = "lavenderblush4"
  ) + # Gestion des labels & ticks de l'axe x
  scale_x_datetime(
    expand = c(0,0),
    date_breaks = "month",
    date_minor_breaks = "month",
    date_labels = "%m/%y"
  ) +
  theme_classic() +# Suppression grille horizontale
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_line(
      colour = "lavenderblush4",
      size = 1
    ),
    axis.text.x = element_text(
      angle = 90
    )
  ) + # Separation de l'axe y
  scale_y_continuous(
    expand = c(0,0),
    limits = c(0, ymax+1),
    breaks = seq(0, 90, 2)
  ) + # Titres
  labs(
    title = "",
    x = " ",
    y = "Nombre d'effets indésirables graves"
  ) + geom_text(
    mapping = aes_q(
      x = as_datetime("2020-06-01 00:00:00", tz = "CET"),
      y = 10.5,
      label = "Période 1"
    )
  ) + geom_text(
    mapping = aes_q(
      x = as_datetime("2021-07-01 00:00:00", tz = "CET"),
      y = 10.5,
      label = "Période 2"
    )
  ) + geom_text(
    mapping = aes_q(
      x = as_datetime("2022-08-01 00:00:00", tz = "CET"),
      y = 10.5,
      label = "Période 3"
    )
  )


#### Courbe EPI Nb Doses ####

# DATA_nb_doses <- read.xlsx("T:/MBDS/PARTAGE/Biostat/Saisine vaccin covid/0 - Base/2023-03-14_Résultats_descriptif_vaccin_admin_vmp.xlsx",
#                            sheet = 3, detectDates = TRUE)
# DATA_nb_doses$date_de_realisation <- as.POSIXct(cut.POSIXt(as.POSIXct(DATA_nb_doses$date_de_realisation), breaks = "month"))
table_dose <- table(DATA_nb_doses$date_de_realisation)
ymax <- max(table_dose)

ggplot(
  DATA_nb_doses
) + # Histogramme de base
  geom_histogram(
    mapping = aes(
      x = date_de_realisation
    ),
    breaks = monthly_breaks,
    closed = "left",
    color = "white"
  ) + # Lignes horizontales
  geom_hline(
    yintercept = seq(0, 150000, 1000),
    color = "white"
  ) + # Lignes horizontales
  geom_vline(
    xintercept = as_datetime(c("2021-01-01 00:00:00", "2022-01-01 00:00:00"), tz = "CET"),
    color = "lavenderblush4"
  ) + # Gestion des labels & ticks de l'axe x
  scale_x_datetime(
    expand = c(0,0),
    date_breaks = "month",
    date_minor_breaks = "month",
    date_labels = "%m/%y"
  ) +
  theme_classic() +# Suppression grille horizontale
  theme(
    panel.grid.minor.y = element_line(
      colour = "lightgray",
      size = 1
    ),
    panel.grid.major.y = element_line(
      colour = "lavenderblush4",
      size = 1
    ),
    axis.text.x = element_text(
      angle = 90
    )
  ) + # Separation de l'axe y
  scale_y_continuous(
    expand = c(0,0),
    limits = c(0, 150000),
    breaks = seq(0, 150000, 10000)
  ) + # Titres
  labs(
    title = "",
    x = " ",
    y = "Nombre d'incidents"
  ) 
# + geom_text(
#     mapping = aes_q(
#       x = as_datetime("2020-09-01 00:00:00", tz = "CET"),
#       y = 145000,
#       label = "Période 1"
#     )
#   ) + geom_text(
#     mapping = aes_q(
#       x = as_datetime("2021-07-01 00:00:00", tz = "CET"),
#       y = 145000,
#       label = "Période 2"
#     )
#   ) + geom_text(
#     mapping = aes_q(
#       x = as_datetime("2022-08-01 00:00:00", tz = "CET"),
#       y = 145000,
#       label = "Période 3"
#     )
#   )



#### Courbe EPI groupee ####
DATA_courbe <- rbind(
  data.frame(
    Nature = "Forme Grave",
    # cl_age = DATA_FG$cl_age,
    # scenarii = DATA_FG$scenarii,
    cas_date = DATA_FG_courbe$cas_date
  ),
  data.frame(
    Nature = "EI Grave",
    # cl_age = DATA_EIG$cl_age,
    # scenarii = DATA_EIG$scenarii,
    cas_date = DATA_EIG_courbe$dat_decla
  )
)



DATA_courbe <- DATA_courbe %>%
  mutate(cas_date = as.POSIXct(cut.POSIXt(as.POSIXct(cas_date), breaks = "month")))
ymax <- max(table(DATA_courbe$cas_date))


ggplot(
  DATA_courbe
) + # Histogramme de base
  geom_histogram(
    mapping = aes(
      x = cas_date,
      fill = Nature
    ),
    breaks = monthly_breaks,
    closed = "left",
    color = "white",
    position = "dodge"
  )  + # Lignes horizontales
  geom_hline(
    yintercept = 1:ymax,
    color = "white"
  ) + # Lignes horizontales
  geom_vline(
    xintercept = as_datetime(c("2021-01-01 00:00:00", "2022-01-01 00:00:00"), tz = "CET"),
    color = "lavenderblush4"
  ) + # Gestion des labels & ticks de l'axe x
  scale_x_datetime(
    expand = c(0,0),
    date_breaks = "month",
    date_minor_breaks = "month",
    date_labels = "%m/%y"
  ) +
  theme_classic() +# Suppression grille horizontale
  theme(
    panel.grid.minor.y = element_line(
      colour = "lightgray",
      size = 1
    ),
    panel.grid.major.y = element_line(
      colour = "lavenderblush4",
      size = 1
    ),
    axis.text.x = element_text(
      angle = 90
    )
  ) + # Separation de l'axe y
  scale_y_continuous(
    expand = c(0,0),
    limits = c(0, ymax+1),
    breaks = seq(0, 90, 10)
  ) + # Titres
  labs(
    title = "",
    x = " ",
    y = "Nombre d'incidents"
  ) + geom_text(
    mapping = aes_q(
      x = as_datetime("2020-09-01 00:00:00", tz = "CET"),
      y = 82,
      label = "Période 1"
    )
  ) + geom_text(
    mapping = aes_q(
      x = as_datetime("2021-07-01 00:00:00", tz = "CET"),
      y = 82,
      label = "Période 2"
    )
  ) + geom_text(
    mapping = aes_q(
      x = as_datetime("2022-08-01 00:00:00", tz = "CET"),
      y = 82,
      label = "Période 3"
    )
  )





















#### Courbe EPI groupee + Nb Doses ####
DATA_courbe <- rbind(
  data.frame(
    Incident = "Forme de Covid Grave",
    # cl_age = DATA_FG$cl_age,
    # scenarii = DATA_FG$scenarii,
    cas_date = DATA_FG_courbe$cas_date
  ),
  data.frame(
    Incident = "Effet Indésirable Grave",
    # cl_age = DATA_EIG$cl_age,
    # scenarii = DATA_EIG$scenarii,
    cas_date = DATA_EIG_courbe$dat_decla
  )
)



DATA_courbe <- DATA_courbe %>%
  mutate(cas_date = as.POSIXct(cut.POSIXt(as.POSIXct(cas_date), breaks = "month")))
ymax <- max(table(DATA_courbe$cas_date))

col_sec <- "darkslateblue"
table_dose2 <- as.data.frame(table_dose)
coeff <- (140000/ymax)*(ymax/80)
table_dose2$Freq <- table_dose2$Freq/coeff

ggplot(
  DATA_courbe
) + # Histogramme de base
  geom_histogram(
    mapping = aes(
      x = cas_date,
      fill = Incident
    ),
    breaks = monthly_breaks,
    closed = "left",
    color = "white",
    position = "dodge"
  ) + # Lignes horizontales
  geom_hline(
    yintercept = 1:ymax,
    color = "white"
  ) + # Lignes horizontales
  geom_vline(
    xintercept = as_datetime(c("2021-01-01 00:00:00", "2022-01-01 00:00:00"), tz = "CET"),
    color = "lavenderblush4"
  ) +
  geom_line(
    data = table_dose2,
    aes(
      x = as_datetime(paste(Var1, "00:00:00"), tz = "CET"),
      y = Freq,
      colour = "Nombre de doses\nde vaccins"
    ),
    linetype = 5
  ) +
  geom_point(
    data = table_dose2,
    aes(
      x = as_datetime(paste(Var1, "00:00:00"), tz = "CET"),
      y = Freq
    ),
    colour = col_sec,
    shape = 16,
    size = 2
  ) + # Gestion des labels & ticks de l'axe x
  scale_x_datetime(
    expand = c(0,0),
    date_breaks = "month",
    date_minor_breaks = "month",
    date_labels = "%m/%y"
  ) +
  scale_colour_manual(
    "Doses",
    values = col_sec
    ) +
  theme_classic() +# Suppression grille horizontale
  theme(
    panel.grid.minor.y = element_line(
      colour = "lightgray",
      size = 1
    ),
    panel.grid.major.y = element_line(
      colour = "lavenderblush4",
      size = 1
    ),
    axis.text.x = element_text(angle = 90),
    axis.text.y.right = element_text(color = col_sec),
    axis.line.y.right = element_line(color = col_sec),
    axis.title.y.right = element_text(color = col_sec)
  ) + # Separation de l'axe y
  scale_y_continuous(
    expand = c(0,0),
    limits = c(0, ymax+1),
    breaks = seq(0, 90, 10),
    sec.axis = sec_axis(
      trans = ~.*coeff,
      name = "Nombre de doses dispensées",
      breaks = floor(seq(0, 140000, length.out = 9)),
      labels = formatC(floor(seq(0, 140000, length.out = 9)), format = "f", big.mark = " ", digits = 0)
    )
  ) + # Titres
  labs(
    title = "",
    x = " ",
    y = "Nombre d'incidents"
  ) + geom_text(
    mapping = aes_q(
      x = as_datetime("2020-07-01 00:00:00", tz = "CET"),
      y = 82,
      label = "Période 1"
    )
  ) + geom_text(
    mapping = aes_q(
      x = as_datetime("2021-07-01 00:00:00", tz = "CET"),
      y = 82,
      label = "Période 2"
    )
  ) + geom_text(
    mapping = aes_q(
      x = as_datetime("2022-08-01 00:00:00", tz = "CET"),
      y = 82,
      label = "Période 3"
    )
  )



