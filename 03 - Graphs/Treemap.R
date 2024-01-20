load("C:/Users/h.marthinet/Documents/34 - R/Codes & Scripts/treemap.Rdata")

library(treemap)

treemap(DATA_treemap,
        index = c("Groupe", "Sous_groupe"),
        vSize = "Value", # Ne pas changer
        type = "index", # Ne pas changer
        algorithm = "pivotSize", # ou algorithm = "squarified" -> change la facon d'ordonner les cases
        title = paste0("2016 (n=", sum(DATA_treemap$Value), ")"),
        align.labels = list( # Pour ne pas que les labels s'empilent
          c("left", "top"),
          c("left", "bottom")
        ),
        palette = "Pastel1",
        fontsize.title =  22,
        #fontsize.labels = 16,
        fontcolor.labels = "black",
        bg.labels = 0, # Pour enlever les encadrés autour des titre
        aspRatio = 1 # Ratio des cotes -> 1 pour avoir un carré
)