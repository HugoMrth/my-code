# Proportion of renal stone (calculi) across age
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Data
data <- data.frame(
  age = c(
    rep("30-39", 384 + 951),
    rep("40-49", 536 + 869),
    rep("50-59", 335 + 438)
  ),
  stone = c(
    rep("yes", 384),
    rep("no", 951),
    rep("yes", 536),
    rep("no", 869),
    rep("yes", 335),
    rep("no", 438)
  )
)


xtab <- table(data$stone, data$age)
xtab

# Compare the proportion of survived between groups
prop_trend_test(xtab)





data <- data %>%
  group_by(age, stone) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n) * 100)



  ggplot(
    data
  ) + # Couche barplot
  geom_col(
    mapping = aes(x = age,
                  y = freq,
                  fill = stone),
    width = 0.6,
    position = "stack"
  )  +
    geom_vline(
      aes(xintercept = 0)
    ) +
    geom_vline(
      aes(xintercept = 100)
    ) + # Couche axe x
    scale_y_continuous(
      limits = c(0, 100.2),
      breaks = seq(0, 100, 10),
      minor_breaks = seq(5, 95, 10),
      expand = c(0, 0),
      labels = paste0(seq(0, 100, 10), "%")
    ) + # Couche Titre
    labs(
      title = " "
    ) + # Couche style
    theme(
      axis.title = element_blank(),
      #axis.text.x = element_blank(),
      axis.text.y = element_text(size = 12),
      panel.background = element_rect(fill = "white"),
      panel.grid.major.y = element_line(color = "darkgray"),
      panel.grid.minor.y = element_line(colour = "lightgray"),
      plot.subtitle = element_text(size = 18)
    ) +
    scale_fill_manual(
      "Calcul rénal",
      values = c("indianred", "royalblue"),
      labels = c("Non", "Oui")
    )
panel.grid.major.y =
