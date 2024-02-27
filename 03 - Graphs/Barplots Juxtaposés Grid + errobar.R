#### Balance par Age - FG/EIG - V/NC ####

DATA <- rbind(
  data.frame(
    Nature = "Forme Grave VaccinÃ©",
    DATA_FG %>%
      filter(cas_grave == "Oui" & vacc_covid_recod == "Oui") %>%
      group_by(cl_age, scenarii) %>%
      summarise(N = n()) %>% # Ajout effctifs OSD
      left_join(
        EFF %>% dplyr::select(Label, "2021"),
        by = c("cl_age" = "Label")
      ) %>%
      rename("Ntot" = "2021")
  ),
  data.frame(
    Nature = "Forme Grave Non VaccinÃ©",
    DATA_FG %>%
      filter(cas_grave == "Oui" & vacc_covid_recod == "Non") %>%
      group_by(cl_age, scenarii) %>%
      summarise(N = n()) %>% # Ajout effctifs OSD
      left_join(
        EFF %>% dplyr::select(Label, "2021"),
        by = c("cl_age" = "Label")
      ) %>%
      rename("Ntot" = "2021")
  ),
  data.frame(
    Nature = "EI Grave",
    DATA_EIG %>%
      filter(effet_type == "graves") %>%
      group_by(cl_age, scenarii) %>%
      summarise(N = n()) %>% # Ajout effctifs OSD
      left_join(
        DATA_AXONE_AGE,
        by = c("cl_age" = "cl_age", "scenarii" = "scenarii")
      )
  )
) 

DATA_rates <- epitools::pois.exact(DATA$N, DATA$Ntot)

DATA <- DATA %>%
  mutate(
    N_inf = round(DATA_rates$lower * DATA_rates$pt, 4),
    N_sup = round(DATA_rates$upper * DATA_rates$pt, 4)
  )

DATA$cl_age <- ordered(DATA$cl_age,
                       levels = c("Moins de 25", "25-34", "35-44", "45 et plus"))

Ymax <- max(DATA$N_sup) * 1.02

ggplot(DATA,
       aes(
         x = Nature,
         y = N,
         width = 0.95
       )) +
  geom_col(
    aes(fill = Nature)
  ) +
  geom_errorbar(
    aes(
      x = Nature,
      ymin = N_inf,
      ymax = N_sup,
      width = 0.2
    )
  ) +
  geom_hline(
    aes(yintercept = 0)
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, Ymax),
    breaks = seq(0, Ymax, 20)
  ) + 
  scale_x_discrete(
    # expand = c(1, 1),
    labels = c("Effet IndÃ©sirable\nGrave", "Forme Grave\nNon VaccinÃ©", "Forme Grave\nVaccinÃ©")
  ) +
  scale_fill_manual(
    values = c("#F8766D", "#00BFC4", "steelblue")
  ) +
  facet_grid(rows = vars(cl_age),
             cols = vars(scenarii)) +
  theme_classic() + 
  theme(
    panel.grid.major.y = element_line(colour = "grey"),
    panel.grid.minor.y = element_line(colour = "lightgray"),
    axis.text.x = element_text(size = 11)
  ) + # Titres
  labs(
    title = "",
    fill = "Nature de l'incident",
    x = " ",
    y = " "
  ) +
  geom_text(
    aes(
      label = N,
      y = N + 7
    ),
    nudge_x = 0.2
  )



#### Balance par Sexe - FG/EIG ####

DATA <- rbind(
  data.frame(
    Nature = "Forme Grave",
    DATA_FG %>%
      filter(cas_grave == "Oui") %>%
      group_by(sexe, scenarii) %>%
      summarise(N = n()) %>% # Ajout effctifs OSD
      left_join(
        EFF %>% dplyr::select(Label, "2021"),
        by = c("sexe" = "Label")
      ) %>%
      rename("Ntot" = "2021")
  ),
  data.frame(
    Nature = "EI Grave",
    DATA_EIG %>%
      filter(effet_type == "graves") %>%
      group_by(sexe, scenarii) %>%
      summarise(N = n()) %>% # Ajout effctifs OSD
      left_join(
        DATA_AXONE_SEXE,
        by = c("sexe" = "sexe", "scenarii" = "scenarii")
      )
  )
) 

DATA_rates <- epitools::pois.exact(DATA$N, DATA$Ntot)

DATA <- DATA %>%
  mutate(
    N_inf = round(DATA_rates$lower * DATA_rates$pt, 4),
    N_sup = round(DATA_rates$upper * DATA_rates$pt, 4)
  )


Ymax <- max(DATA$N_sup) * 1.02

ggplot(DATA,
       aes(
         x = Nature,
         y = N,
         width = 0.95
       )) +
  geom_col(
    aes(fill = Nature)
  ) +
  geom_errorbar(
    aes(
      x = Nature,
      ymin = N_inf,
      ymax = N_sup,
      width = 0.2
    )
  ) +
  geom_hline(
    aes(yintercept = 0)
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, Ymax),
    breaks = seq(0, Ymax, 20)
  ) + 
  scale_x_discrete(
    # expand = c(1, 1),
    labels = c("Effet IndÃ©sirable\nGrave", "Forme Grave")
  ) +
  facet_grid(rows = vars(sexe),
             cols = vars(scenarii)) +
  theme_classic() + 
  theme(
    panel.grid.major.y = element_line(colour = "grey"),
    panel.grid.minor.y = element_line(colour = "lightgray"),
    axis.text.x = element_text(size = 12)
  ) + # Titres
  labs(
    title = "",
    fill = "Nature de l'incident",
    x = " ",
    y = " "
  ) +
  geom_text(
    aes(
      label = N,
      y = N + 7
    ),
    nudge_x = 0.2
  )



#### Balance par Sexe - FG/EIG - V/NC ####

DATA <- rbind(
  data.frame(
    Nature = "Forme Grave VaccinÃ©",
    DATA_FG %>%
      filter(cas_grave == "Oui" & vacc_covid_recod == "Oui") %>%
      group_by(sexe, scenarii) %>%
      summarise(N = n()) %>% # Ajout effctifs OSD
      left_join(
        EFF %>% dplyr::select(Label, "2021"),
        by = c("sexe" = "Label")
      ) %>%
      rename("Ntot" = "2021")
  ),
  data.frame(
    Nature = "Forme Grave Non VaccinÃ©",
    DATA_FG %>%
      filter(cas_grave == "Oui" & vacc_covid_recod == "Non") %>%
      group_by(sexe, scenarii) %>%
      summarise(N = n()) %>% # Ajout effctifs OSD
      left_join(
        EFF %>% dplyr::select(Label, "2021"),
        by = c("sexe" = "Label")
      ) %>%
      rename("Ntot" = "2021")
  ),
  data.frame(
    Nature = "EI Grave",
    DATA_EIG %>%
      filter(effet_type == "graves") %>%
      group_by(sexe, scenarii) %>%
      summarise(N = n()) %>% # Ajout effctifs OSD
      left_join(
        DATA_AXONE_SEXE,
        by = c("sexe" = "sexe", "scenarii" = "scenarii")
      )
  )
) 

DATA_rates <- epitools::pois.exact(DATA$N, DATA$Ntot)

DATA <- DATA %>%
  mutate(
    N_inf = round(DATA_rates$lower * DATA_rates$pt, 4),
    N_sup = round(DATA_rates$upper * DATA_rates$pt, 4)
  )


Ymax <- max(DATA$N_sup) * 1.02

ggplot(DATA,
       aes(
         x = Nature,
         y = N,
         width = 0.95
       )) +
  geom_col(
    aes(fill = Nature)
  ) +
  geom_errorbar(
    aes(
      x = Nature,
      ymin = N_inf,
      ymax = N_sup,
      width = 0.2
    )
  ) +
  geom_hline(
    aes(yintercept = 0)
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, Ymax),
    breaks = seq(0, Ymax, 20)
  ) + 
  scale_x_discrete(
    # expand = c(1, 1),
    labels = c("Effet IndÃ©sirable\nGrave", "Forme Grave\nNon VaccinÃ©", "Forme Grave\nVaccinÃ©")
  ) +
  scale_fill_manual(
    values = c("#F8766D", "#619CFF", "steelblue")
  ) +
  facet_grid(rows = vars(sexe),
             cols = vars(scenarii)) +
  theme_classic() + 
  theme(
    panel.grid.major.y = element_line(colour = "grey"),
    panel.grid.minor.y = element_line(colour = "lightgray"),
    axis.text.x = element_text(size = 11)
  ) + # Titres
  labs(
    title = "",
    fill = "Nature de l'incident",
    x = " ",
    y = " "
  ) +
  geom_text(
    aes(
      label = N,
      y = N + 7
    ),
    nudge_x = 0.2
  )