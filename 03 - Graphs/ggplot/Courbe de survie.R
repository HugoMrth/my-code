mfit_expo <- survfit(Surv(etime, event) ~ EXPO, data = data)
autoplot(mfit_expo, surv.linetype = 'blank', surv.colour = NA) +
  geom_step(aes(linetype = strata, color = event), size = 0.8) +
  scale_linetype_manual(name = "Groupe",
                        values = c('dashed', 'solid'), 
                        labels = c('Non-exposé', 'Exposé')) +
  scale_color_manual(name = "Etat",
                     values = c("#AAC0AF", "#1C4073", "#B28B84", "#0f766e"), 
                     labels = c('MREF', 'O Med','MSUD', 'Décès')) + 
  #scale_size_manual(guide = "none") +
  labs(x = "Période de suivi (jours)", y = "Proportion de la population", 
       title = "") +
  geom_vline(
    aes(xintercept = 0),
    size = 1
  ) + 
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.1),
    labels = paste0(seq(0, 100, 10), "%"),
    expand = c(0,0)
  ) +
  scale_x_continuous(
    breaks = seq(0, 360, 60),
    expand = c(0,0)
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="black", size = 12),
        axis.title.y = element_text(face="bold", colour="black", size = 12),
        legend.title = element_text(face="bold", size = 10),
        panel.grid.major.y = element_line(colour = "lightgray", size = 0.3),
        panel.grid.minor.x = element_blank())
