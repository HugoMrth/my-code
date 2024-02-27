

#### -_-_-_ Courbes -_-_-_ ######

#### Définition des données

DATA_plot <- as.data.frame(rbind(
  cbind(
    Episode = "Premier",
    Date = paste0(DATA$Q_C8A_1, "/", stringr::str_pad(as.numeric(DATA$Q_C8M_1), 2, "left", "0"), "/01"),
    Statut = as.character(DATA$Q_A2)
  )[DATA$Premier_episode_bin == "Positif",],
  cbind(
    Episode = "Dernier",
    Date = paste0(DATA$Q_C8A_2, "/", stringr::str_pad(as.numeric(DATA$Q_C8M_2), 2, "left", "0"), "/01"),
    Statut = as.character(DATA$Q_A2)
  )[DATA$Dernier_episode_bin == "Positif",]
))
DATA_plot$Date <- as.Date(DATA_plot$Date)
DATA_plot$Episode <- factor(DATA_plot$Episode, levels = c("Premier", "Dernier"))
DATA_plot <- DATA_plot %>% filter(!is.na(Date))



DATA_vacc <- data.frame(
  table(DATA$Q_C38ter)
)
DATA_vacc <- DATA_vacc[DATA_vacc$Var1 != "2022-09-15",]
col_sec <- "darkslateblue"
ymax <- 800
coeff <- ymax/4800



# Decoupage deu graphs : ticks
monthly_breaks <- seq.Date(from = as.Date("2020-01-01"),
                             to = as.Date("2022-09-01"),
                             by = "month")


#### Cas ####

ggplot(
  DATA_plot
) + # Histogramme de base
  geom_histogram(
    mapping = aes(
      x = Date,
      fill = Episode
    ),
    breaks = monthly_breaks,
    closed = "left",
    color = "white",
    position = "dodge"
  ) + # Lignes horizontales
  geom_hline(
    yintercept = seq(10, ymax, by = 10),
    color = "white"
  ) + # Separation de l'axe y
  scale_y_continuous(
    expand = c(0,0),
    limits = c(0, ymax),
    breaks = seq(0, ymax, 100)
  ) + 
  geom_vline(
    xintercept = as.Date(c("2021-06-01", "2022-01-01")),
    color = "lavenderblush4",
    size = 1.5
  ) +
  annotate(
    "rect",
    xmin = as.Date("2020-04-01"),
      xmax = as.Date("2020-06-01"),
      ymin = 0, 
      ymax = ymax,
    fill = "gray",
    alpha = 0.4
  )  +
  annotate(
    "rect",
    xmin = as.Date("2020-11-01"),
    xmax = as.Date("2021-01-01"),
    ymin = 0, 
    ymax = ymax,
    fill = "gray",
    alpha = 0.4
  )  +
  annotate(
    "rect",
    xmin = as.Date("2021-04-01"),
    xmax = as.Date("2021-06-01"),
    ymin = 0, 
    ymax = ymax,
    fill = "gray",
    alpha = 0.4
  ) + 
  scale_shape_manual(
    values = "gray"
  ) +
  scale_x_date(
    expand = c(0,0),
    date_breaks = "month",
    date_minor_breaks = "month",
    date_labels = "%m/%y"
  ) + # Titres
  labs(
    title = "",
    caption = "",
    x = " ",
    y = "Nombre de cas"
  ) + geom_text(
    mapping = aes_q(
      x = as.Date("2020-09-01"),
      y = ymax-25,
      label = "Alpha"
    ),
    size = 6
  ) + geom_text(
    mapping = aes_q(
      x = as.Date("2021-09-15"),
      y = ymax-25,
      label = "Delta"
    ),
    size = 6
  ) + geom_text(
    mapping = aes_q(
      x = as.Date("2022-05-01"),
      y = ymax-25,
      label = "Omicron"
    ),
    size = 6
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
      angle = 0,
      vjust = 0.5,
      hjust = -0.1
    ),
    axis.text.y.right = element_text(color = col_sec),
    axis.line.y.right = element_line(color = col_sec),
    axis.title.y.right = element_text(color = col_sec)
  )


#### Cas + doses ####

ymax <- 800
coeff <- ymax/4800

ggplot(
  DATA_plot
) + # Histogramme de base
  geom_histogram(
    mapping = aes(
      x = Date,
      fill = Episode
    ),
    breaks = monthly_breaks,
    closed = "left",
    color = "white",
    position = "dodge"
  ) + # Lignes horizontales
  geom_hline(
    yintercept = seq(10, ymax, by = 10),
    color = "white"
  ) + # Lignes horizontales
  geom_vline(
    xintercept = as.Date(c("2021-06-01", "2022-01-01")),
    color = "lavenderblush4",
    size = 1.5
  )  +
  annotate(
    "rect",
    xmin = as.Date("2020-04-01"),
    xmax = as.Date("2020-06-01"),
    ymin = 0, 
    ymax = ymax,
    fill = "gray",
    alpha = 0.25
  )  +
  annotate(
    "rect",
    xmin = as.Date("2020-11-01"),
    xmax = as.Date("2021-01-01"),
    ymin = 0, 
    ymax = ymax,
    fill = "gray",
    alpha = 0.25
  )  +
  annotate(
    "rect",
    xmin = as.Date("2021-04-01"),
    xmax = as.Date("2021-06-01"),
    ymin = 0, 
    ymax = ymax,
    fill = "gray",
    alpha = 0.25
  ) +
  geom_line(
    data = DATA_vacc,
    aes(
      x = as.Date(Var1),
      y = Freq*coeff,
      colour = "Nombre de doses\nde vaccins"
    ),
    linetype = 5
  ) +
  geom_point(
    data = DATA_vacc,
    aes(
      x = as.Date(Var1),
      y = Freq*coeff
    ),
    colour = col_sec,
    shape = 16,
    size = 2
  )  +
  scale_colour_manual(
    "Dernière dose",
    values = col_sec
  ) + # Gestion des labels & ticks de l'axe x
  scale_x_date(
    expand = c(0,0),
    date_breaks = "month",
    date_minor_breaks = "month",
    date_labels = "%m/%y"
  ) + # Separation de l'axe y
  scale_y_continuous(
    expand = c(0,0),
    limits = c(0, ymax),
    breaks = seq(0, ymax, 100),
    sec.axis = sec_axis(
      trans = ~./coeff,
      name = "Nombre de doses",
      breaks = seq(0, ymax/coeff, length.out = ymax/100 + 1),
      labels = formatC(floor(seq(0, ymax/coeff, length.out = ymax/100 + 1)), format = "f", big.mark = " ", digits = 0)
    )
  ) + # Titres
  labs(
    title = "",
    caption = "",
    x = " ",
    y = "Nombre de cas"
  ) + geom_text(
    mapping = aes_q(
      x = as.Date("2020-09-01"),
      y = ymax-25,
      label = "Alpha"
    ),
    size = 6
  ) + geom_text(
    mapping = aes_q(
      x = as.Date("2021-09-15"),
      y = ymax-25,
      label = "Delta"
    ),
    size = 6
  ) + geom_text(
    mapping = aes_q(
      x = as.Date("2022-05-01"),
      y = ymax-25,
      label = "Omicron"
    ),
    size = 6
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
      angle = 0,
      vjust = 0.5,
      hjust = -0.1
    ),
    axis.text.y.right = element_text(color = col_sec),
    axis.line.y.right = element_line(color = col_sec),
    axis.title.y.right = element_text(color = col_sec)
  )

#### Cas + Satut ####

ymax <- 600


ggplot(
  DATA_plot
) + # Histogramme de base
  geom_histogram(
    mapping = aes(
      x = Date,
      fill = Episode
    ),
    breaks = monthly_breaks,
    closed = "left",
    color = "white",
    position = "dodge"
  ) + # Lignes horizontales
  geom_hline(
    yintercept = seq(10, ymax, by = 10),
    color = "white"
  ) +
  geom_hline(
    aes(yintercept = 0)
  ) + # Lignes horizontales
  geom_vline(
    xintercept = as.Date(c("2021-06-01", "2022-01-01")),
    color = "lavenderblush4",
    size = 1.5
  )  +
  annotate(
    "rect",
    xmin = as.Date("2020-04-01"),
    xmax = as.Date("2020-06-01"),
    ymin = 0, 
    ymax = ymax,
    fill = "gray",
    alpha = 0.25
  )  +
  annotate(
    "rect",
    xmin = as.Date("2020-11-01"),
    xmax = as.Date("2021-01-01"),
    ymin = 0, 
    ymax = ymax,
    fill = "gray",
    alpha = 0.25
  )  +
  annotate(
    "rect",
    xmin = as.Date("2021-04-01"),
    xmax = as.Date("2021-06-01"),
    ymin = 0, 
    ymax = ymax,
    fill = "gray",
    alpha = 0.25
  )+ # Gestion des labels & ticks de l'axe x
  scale_x_date(
    expand = c(0,0),
    date_breaks = "month",
    date_minor_breaks = "month",
    date_labels = "%m/%y"
  ) + # Separation de l'axe y
  scale_y_continuous(
    expand = c(0,0),
    limits = c(0, ymax),
    breaks = seq(0, ymax, 100)
  ) + # Titres
  labs(
    title = "",
    caption = "",
    x = " ",
    y = "Nombre de cas"
  ) + geom_text(
    mapping = aes_q(
      x = as.Date("2020-09-01"),
      y = ymax-25,
      label = "Alpha"
    ),
    size = 6
  ) + geom_text(
    mapping = aes_q(
      x = as.Date("2021-09-15"),
      y = ymax-25,
      label = "Delta"
    ),
    size = 6
  ) + geom_text(
    mapping = aes_q(
      x = as.Date("2022-05-01"),
      y = ymax-25,
      label = "Omicron"
    ),
    size = 6
  ) +
  facet_grid(
    rows = vars(Statut)
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
      angle = 0,
      vjust = 0.5,
      hjust = -0.1
    ),
    axis.text.y.right = element_text(color = col_sec),
    axis.line.y.right = element_line(color = col_sec),
    axis.title.y.right = element_text(color = col_sec)
  )


#### Cas + dose + Statut ####


DATA_vacc <- data.frame(
  table(DATA$Q_C38ter, DATA$Q_A2)
)
colnames(DATA_vacc) <- c("Date", "Statut", "Val")
DATA_vacc <- DATA_vacc[DATA_vacc$Date != "2022-09-15",]

ymax <- 600
coeff <- ymax/3000

ggplot(
  DATA_plot
) + # Histogramme de base
  geom_histogram(
    mapping = aes(
      x = Date,
      fill = Episode
    ),
    breaks = monthly_breaks,
    closed = "left",
    color = "white",
    position = "dodge"
  ) + # Lignes horizontales
  geom_hline(
    yintercept = seq(10, ymax, by = 10),
    color = "white"
  )  +
  geom_hline(
    aes(yintercept = 0)
  ) + # Lignes horizontales
  geom_vline(
    xintercept = as.Date(c("2021-06-01", "2022-01-01")),
    color = "lavenderblush4",
    size = 1.5
  )  +
  annotate(
    "rect",
    xmin = as.Date("2020-04-01"),
    xmax = as.Date("2020-06-01"),
    ymin = 0, 
    ymax = ymax,
    fill = "gray",
    alpha = 0.25
  )  +
  annotate(
    "rect",
    xmin = as.Date("2020-11-01"),
    xmax = as.Date("2021-01-01"),
    ymin = 0, 
    ymax = ymax,
    fill = "gray",
    alpha = 0.25
  )  +
  annotate(
    "rect",
    xmin = as.Date("2021-04-01"),
    xmax = as.Date("2021-06-01"),
    ymin = 0, 
    ymax = ymax,
    fill = "gray",
    alpha = 0.25
  ) +
  geom_line(
    data = DATA_vacc,
    aes(
      x = as.Date(Date),
      y = Val*coeff,
      colour = "Nombre de doses\nde vaccins"
    ),
    linetype = 5
  ) +
  geom_point(
    data = DATA_vacc,
    aes(
      x = as.Date(Date),
      y = Val*coeff
    ),
    colour = col_sec,
    shape = 16,
    size = 2
  )  +
  scale_colour_manual(
    "Dernière dose",
    values = col_sec
  ) + # Gestion des labels & ticks de l'axe x
  scale_x_date(
    expand = c(0,0),
    date_breaks = "month",
    date_minor_breaks = "month",
    date_labels = "%m/%y"
  ) + # Separation de l'axe y
  scale_y_continuous(
    expand = c(0,0),
    limits = c(0, ymax),
    breaks = seq(0, ymax, 100),
    sec.axis = sec_axis(
      trans = ~./coeff,
      name = "Nombre de doses",
      breaks = seq(0, 3000, 500),
      labels = formatC(floor(seq(0, 3000, 500)), format = "f", big.mark = " ", digits = 0)
    )
  ) + # Titres
  labs(
    title = "",
    caption = "",
    x = " ",
    y = "Nombre de cas"
  ) + geom_text(
    mapping = aes_q(
      x = as.Date("2020-09-01"),
      y = ymax-25,
      label = "Alpha"
    ),
    size = 6
  ) + geom_text(
    mapping = aes_q(
      x = as.Date("2021-09-15"),
      y = ymax-25,
      label = "Delta"
    ),
    size = 6
  ) + geom_text(
    mapping = aes_q(
      x = as.Date("2022-05-01"),
      y = ymax-25,
      label = "Omicron"
    ),
    size = 6
  ) +
  facet_grid(
    rows = vars(Statut)
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
      angle = 0,
      vjust = 0.5,
      hjust = -0.1
    ),
    axis.text.y.right = element_text(color = col_sec),
    axis.line.y.right = element_line(color = col_sec),
    axis.title.y.right = element_text(color = col_sec)
  )







