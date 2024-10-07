library(plotly)


#### Basique ####


fig <- plot_ly(
  type = "sankey",
  orientation = "h",
  
  node = list(
    label = c("A1", "A2", "B1", "B2", "C1", "C2"),
    color = c("blue", "blue", "blue", "blue", "blue", "blue"),
    pad = 15,
    thickness = 20,
    line = list(
      color = "black",
      width = 0.5
    )
  ),
  
  link = list(
    source = c(0,1,0,2,3,3),
    target = c(2,3,3,4,4,5),
    value =  c(8,4,2,8,4,2)
  )
)
fig <- fig %>% layout(
  title = "Basic Sankey Diagram",
  font = list(
    size = 10
  )
)

fig





#### Exemple SNDS ordre ok ####

dataSankeyTem <- list(
  Nodes = data.frame(
    label = c(rep(c("Aucune", "DIU cuivre","Hormonale", "Orale"), 5)),
    color = c(rep(c("blue", "red", "green", "orange"), 5))
  ),
  Links = data.frame(
    source = c(rep(1:16, each = 4)) - 1,
    target = c(rep(5:8, 4), rep(9:12, 4), rep(13:16, 4), rep(17:20, 4)) - 1,
    value = c(table(datap$cat_couvdet_date_index_P1A[datap$cat_couvdet_date_index == "Aucune"]), 
              table(datap$cat_couvdet_date_index_P1A[datap$cat_couvdet_date_index == "DIU cuivre"]),
              table(datap$cat_couvdet_date_index_P1A[datap$cat_couvdet_date_index == "Hormonale"]),
              table(datap$cat_couvdet_date_index_P1A[datap$cat_couvdet_date_index == "Orale"]),
              
              table(datap$cat_couvdet_date_index_P2A[datap$cat_couvdet_date_index_P1A == "Aucune"]), 
              table(datap$cat_couvdet_date_index_P2A[datap$cat_couvdet_date_index_P1A == "DIU cuivre"]),
              table(datap$cat_couvdet_date_index_P2A[datap$cat_couvdet_date_index_P1A == "Hormonale"]),
              table(datap$cat_couvdet_date_index_P2A[datap$cat_couvdet_date_index_P1A == "Orale"]),
              
              table(datap$cat_couvdet_date_index_P3A[datap$cat_couvdet_date_index_P2A == "Aucune"]), 
              table(datap$cat_couvdet_date_index_P3A[datap$cat_couvdet_date_index_P2A == "DIU cuivre"]),
              table(datap$cat_couvdet_date_index_P3A[datap$cat_couvdet_date_index_P2A == "Hormonale"]),
              table(datap$cat_couvdet_date_index_P3A[datap$cat_couvdet_date_index_P2A == "Orale"]),
              
              table(datap$cat_couvdet_date_index_P4A[datap$cat_couvdet_date_index_P3A == "Aucune"]), 
              table(datap$cat_couvdet_date_index_P4A[datap$cat_couvdet_date_index_P3A == "DIU cuivre"]),
              table(datap$cat_couvdet_date_index_P4A[datap$cat_couvdet_date_index_P3A == "Hormonale"]),
              table(datap$cat_couvdet_date_index_P4A[datap$cat_couvdet_date_index_P3A == "Orale"])
    ),
    color = c(rep(rep(c("cornflowerblue", "lightcoral", "mediumseagreen", "tan"), each = 4), 4))
  ))









#### Exemple SNDS ordre chelou ####

dataSankey <- list(
  Nodes = data.frame(
  label = c("Date index", 
            "Traitement de référence", "Traitement de substitution", "Pas de traitement", "Décès", 
            "Traitement de référence", "Traitement de substitution", "Pas de traitement", "Décès", 
            "Traitement de référence", "Traitement de substitution", "Pas de traitement", "Décès", 
            "Traitement de référence", "Traitement de substitution", "Pas de traitement", "Décès"),
  color = c("white", 
            "blue", "red", "green", "gray",
            "blue", "red", "green", "gray",
            "blue", "red", "green", "gray",
            "blue", "red", "green", "gray")
),
Links = data.frame(
  source = c(rep(0:12, each = 4)),
  target = c(1:4, rep(5:8, 4), rep(9:12, 4), rep(13:16, 4)),
  value = c(table(data$etat_P3m)[c(1, 3, 2)], sum(is.na(data$etat_P3m)),
            
            table(data$etat_P6m[data$etat_P3m == "Traitement de référence"]), 
              sum(!is.na(data$etat_P3m) & is.na(data$etat_P6m) & data$etat_P3m == "Traitement de référence"),
            table(data$etat_P6m[data$etat_P3m == "Traitement de substitution"]), 
              sum(!is.na(data$etat_P3m) & is.na(data$etat_P6m) & data$etat_P3m == "Traitement de substitution"),
            table(data$etat_P6m[data$etat_P3m == "Pas de traitement"]), 
              sum(!is.na(data$etat_P3m) & is.na(data$etat_P6m) & data$etat_P3m == "Pas de traitement"),
            c(0, 0, 0, sum(is.na(data$etat_P6m) & is.na(data$etat_P3m))),
            
            table(data$etat_P9m[data$etat_P6m == "Traitement de référence"]), 
              sum(!is.na(data$etat_P6m) & is.na(data$etat_P9m) & data$etat_P6m == "Traitement de référence"),
            table(data$etat_P9m[data$etat_P6m == "Traitement de substitution"]),
              sum(!is.na(data$etat_P6m) & is.na(data$etat_P9m) & data$etat_P6m == "Traitement de substitution"),
            table(data$etat_P9m[data$etat_P6m == "Pas de traitement"])[c(1, 3, 2)], 
              sum(!is.na(data$etat_P6m) & is.na(data$etat_P9m) & data$etat_P6m == "Pas de traitement"),
            c(0, 0, 0, sum(is.na(data$etat_P9m) & is.na(data$etat_P6m))),
            
            table(data$etat_P12m[data$etat_P9m == "Traitement de référence"]), 
              sum(!is.na(data$etat_P9m) & is.na(data$etat_P12m) & data$etat_P9m == "Traitement de référence"),
            table(data$etat_P12m[data$etat_P9m == "Traitement de substitution"]), 
              sum(!is.na(data$etat_P9m) & is.na(data$etat_P12m) & data$etat_P9m == "Traitement de substitution"),
            table(data$etat_P12m[data$etat_P9m == "Pas de traitement"]), 
              sum(!is.na(data$etat_P9m) & is.na(data$etat_P12m) & data$etat_P9m == "Pas de traitement"),
            c(0, 0, 0, sum(is.na(data$etat_P12m) & is.na(data$etat_P9m)))
            ),
  color = c(rep("lightgray", 4), rep(rep(c("cornflowerblue", "mediumseagreen", "lightcoral", "lightgray"), each = 4), 3))
))
Links$source <- Links$source[c(1, 2, 3, 4, 
                               
                               5, 6, 7, 8, 
                               13, 14, 15, 16, 
                               9, 10, 11, 12,
                               17, 18, 19, 20,
                               
                               21, 22, 23, 24,
                               29, 30, 31, 32,
                               25, 26, 27, 28,
                               33, 34, 35, 36,
                               
                               37, 38, 39, 40, 
                               45, 46, 47, 48,
                               41, 42 ,43 ,44,
                               49, 50, 51, 52)]
