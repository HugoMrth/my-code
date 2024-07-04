

# Graphiques ####

#### _1 G17 ####
DATA_plot <- DATA %>%
  dplyr::select(Q_G17_3, Q_A2) %>%
  group_by(Q_A2) %>%
  mutate(moy = mean(Q_G17_3, na.rm = TRUE))

ggplot(
  DATA_plot
) + # Couche barplot
  geom_boxplot(
    mapping = aes(y = Q_G17_3,
                  x = Q_A2,
                  colour = Q_A2),
    show.legend = FALSE
  )  + # Couche axe x
  scale_y_continuous(
    limits = c(0, 10),
    breaks = seq(0, 10, 1),
    minor_breaks = seq(1, 9, 2),
    expand = c(0, 0)
  ) +
  geom_vline(
    aes(xintercept = 0)
  )  + 
  geom_point(
    aes(x = Q_A2,
        y = moy,
        colour = Q_A2),
    size = 3,
    show.legend = FALSE,
    shape = 2
  ) + # Couche Titre
  labs(
    title = " ",
    subtitle = ""
  ) + # Couche style
  theme(
    axis.text.x = element_text(
      size = 12
    ),
    axis.title.x = element_blank(),
    axis.title.y = element_text(
      size = 14
    ),
    panel.background = element_rect(fill = "white"),
    panel.grid.major.y = element_line(
      color = "lightgray",
      linetype = 2
      )
  ) +
  ylab(
    "Impact de la crise"
  )

# _2 Barplots / Statuts ####


createPlotImpact <- function(var, title, labels, showLegend = TRUE) {
  DATA_plot <- DATA[, c("Q_A2", var)]
  colnames(DATA_plot) <- c("Q_A2", "var")
  DATA_plot <- DATA_plot %>%
    dplyr::filter(!is.na(var)) %>%
    group_by(Q_A2, var) %>%
    summarise(N = n()) %>%
    left_join(DATA_plot %>%
                dplyr::filter(!is.na(var)) %>%
                group_by(Q_A2) %>%
                summarise(Tot = n())) %>%
    mutate(P = N/Tot*100,
           var = factor(var,
                        levels = labels))
  
  
  DATA_plot$Q_A2 <- factor(DATA_plot$Q_A2,
                           levels = c("Retraité militaire", "Militaire d’active", "Famille de militaire"))
  
  ggplot(
    DATA_plot
  ) + # Couche barplot
    geom_col(
      mapping = aes(P,
                    Q_A2,
                    fill = var),
      width = 0.6,
      position = "stack",
      color = "white",
      show.legend = showLegend
    ) +
    geom_vline(
      aes(xintercept = 0)
    ) +
    geom_vline(
      aes(xintercept = 100)
    ) + # Couche axe x
    scale_x_continuous(
      limits = c(0, 100.2),
      breaks = seq(0, 100, 10),
      minor_breaks = seq(5, 95, 10),
      expand = c(0, 0),
      labels = paste0(seq(0, 100, 10), "%")
    ) + # Couche Titre
    labs(
      title = " ",
      subtitle = title
    ) + # Couche style
    theme(
      axis.title = element_blank(),
      #axis.text.x = element_blank(),
      axis.text.y = element_text(size = 12),
      panel.background = element_rect(fill = "white"),
      panel.grid.major.x = element_line(color = "darkgray"),
      panel.grid.minor.x = element_line(colour = "lightgray"),
      plot.subtitle = element_text(size = 18)
    ) +
    scale_fill_manual(
      " ",
      values = rev(RColorBrewer::brewer.pal(3, "RdYlGn")),
      labels = labels
    )
}

