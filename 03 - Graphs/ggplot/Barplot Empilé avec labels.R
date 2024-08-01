library(ggplot2)
library(haven)
library(tidyr)
library(dplyr)
library(stringr)
library(stringi)



nb_atc4 <- read_sas("sasdata1/nb_atc4.sas7bdat",  NULL)
dataplot<- subset(nb_atc4, select=-c(A,B,C,D,G,H,J,L,M,N,P,R,S,V,Z,Tot))
dataplot2<- dataplot%>% pivot_longer(cols= c("Part_a","Part_b", "Part_c", "Part_d", "Part_g", "Part_h", 
                                             "Part_j", "Part_l", "Part_m", "Part_n", "Part_p", "Part_r", 
                                             "Part_s","Part_v","Part_z"),names_to="ATC1",values_to="Part") %>%
  mutate(cols = rep(c('#1E5471', '#276C91', '#5EABD4', '#28AFB0', '#7DCFB6',
                           '#FFDB70',  '#F6803C', '#C15933', '#CC0300', 
                                                      '#82354F' ,'#832041' ,'#3C1155', '#E0D6FF', 
                                                      '#D8FDFB', '#80DED9'), 30),
                           d1 = as.Date(d1, origin = "1960-01-01"),
         ATC1 = toupper(str_replace_all(ATC1, "Part_", "")))
dataplot2 <- dataplot2 %>%
               mutate(lab = ifelse(Part > 0.01, paste0(formatC(Part*100, 1, format = "f"), "%"), ""))
x_texte <- c()
for (i in unique(dataplot2$d1)) {
  for (j in 1:sum(dataplot2$d1 == i)) {
    ifelse(j == 1,
           x_texte <- c(x_texte, dataplot2$Part[dataplot2$d1 == i][j]/2),
           x_texte <- c(x_texte, sum(dataplot2$Part[dataplot2$d1 == i][1:(j-1)]) +
                          dataplot2$Part[dataplot2$d1 == i][j]/2))
  }
}
dataplot2$x_texte <- 1 - x_texte
 
 
ggplot(data=dataplot2, aes(x = d1, y= Part)) + 
  geom_bar(
    stat="identity", 
    aes(fill = ATC1),
    colour = dataplot2$cols) +
  geom_text(
    aes(label = lab, 
        y = x_texte),
    size = 3,
    colour = "white") +
  labs(
    x="", 
    y = "Part des classes ATC1", 
    title = "Part des classes ATC1 dispensées par mois")+
  scale_y_continuous(
    breaks = c(0, 0.25, 0.5, 0.75, 1),
    labels = c("0%", "25%", "50%", "75%", "100%"),
    name = "Part des classes ATC1"
  )+
  scale_fill_manual(
    values=unique(dataplot2$cols), 
    name = "Classe ATC1")+
  theme_minimal()+
  theme(
    legend.position = "bottom")
