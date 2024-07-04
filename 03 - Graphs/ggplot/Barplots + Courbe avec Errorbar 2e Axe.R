library(ggplot2)
library(dplyr)
library(stringr)
library(openxlsx)
library(lubridate)



ymax <- 200
coeff <- 70/200

DATA$TI_tot <- DATA$TI_tot/coeff
DATA$TI_tot_B_inf <- DATA$TI_tot_B_inf/coeff
DATA$TI_tot_B_sup <- DATA$TI_tot_B_sup/coeff

ggplot(DATA,
       aes(
         x = cas_annee,
         y = n_cas_tot,
         width = 0.95
       )) +
  geom_col(
    mapping = aes(
      fill = "Incidence" # Nom dans la legende
    ),
    fill = "dodgerblue" # couleur des barres
  ) +
  geom_line(
    aes(
      x = cas_annee,
      y = TI_tot,
      colour = "Taux d'incidence" # Nmo dans la legende
    ),
    linetype = 1,
    size = 1,
    colour = "black"
  ) + 
  geom_errorbar(
    aes(
      x = cas_annee,
      ymin = TI_tot_B_inf,
      ymax = TI_tot_B_sup,
      width = 0.2
    )
  ) +# Separation de l'axe y
  scale_y_continuous(
    expand = c(0,0),
    limits = c(0, ymax),
    breaks = seq(0, ymax, 25),
    name = "Incidence",
    sec.axis = sec_axis(
      trans = ~.*coeff,
      name = "Taux d'incidence",
      breaks = seq(0, ymax*coeff, 10)
    )
  ) + 
  scale_x_continuous(
    breaks = 2012:2022,
    labels = 2012:2022,
    name = ""
  ) +
  theme_classic() + 
  theme(
    panel.grid.major.y = element_line(colour = "grey"),
    panel.grid.minor.y = element_line(colour = "lightgray"),
    axis.text.x = element_text(size = 11),
    axis.line.y.left = element_line(color = "dodgerblue"),
    axis.title.y.left = element_text(color = "dodgerblue"),
    axis.text.y.left = element_text(color = "dodgerblue")
  ) + # Titres
  labs(
    title = "",
    fill = "Nature de l'incident",
    x = " ",
    y = " "
  ) 
