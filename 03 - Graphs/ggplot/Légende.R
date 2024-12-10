ggplot(data=nb_tlc,
          aes (x = d1))+
  geom_line(aes(y=nb_tlc, color = "TLC disp", lty = "TLC disp"))+ 
  geom_point(aes(x = d1, y= nb_tlc, color = "TLC disp"), size=1.0)+
  geom_line(aes(y=nb_tlc1, color = "TLC", lty="TLC"))+ 
  geom_point(aes(x = d1, y= nb_tlc1, color = "TLC"), size=1.0)+
  geom_line(aes(y=(pourcentage/100)*max(nb_tlc1), color= "Ratio", lty="Ratio"))+ 
  geom_point(aes(x = d1, y= (pourcentage/100)*max(nb_tlc1), color = "Ratio"), size=1.0)+
  labs(color = "Légende",
       lty = "Légende")+
  scale_colour_manual(values=c("TLC disp" = "#be123c", "TLC" = "#1e3a8a", "Ratio" = "#64748b")) + 
  scale_linetype_manual(values=c("TLC disp" = 1, "TLC" = 1, "Ratio" = 2))




theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(colour = "black", size = 0.3),
        axis.ticks.x = element_line(colour = "black", size = 0.3),
        axis.line.y = element_line(colour = "black", size = 0.3),
        axis.ticks.y = element_line(colour = "black", size = 0.3),
        legend.position = c(0.75, 0.85),
        legend.background = element_rect(fill = "transparent", color = "white"))
