rm(list = ls())

library(xlsx)
library(stringi)
library(stringr)


#### Lecture format workbook pour avoir acces aux fonctions java

wb     <- loadWorkbook("C:/Users/h.marthinet/Documents/Travail/02 - Projets/004 - CLES/0 - Base/2022-02-10_AffectionsCIM10_conges_GT.xlsx")
sheet1 <- getSheets(wb)[[1]]

rows  <- getRows(sheet1)
cells <- getCells(rows)


styles <- sapply(cells, getCellStyle)


#### Fonction detection de couleurs de cellule
cellColor <- function(style) 
{
  fg  <- style$getFillForegroundXSSFColor()
  rgb <- tryCatch(fg$getRgb(), error = function(e) NULL)
  rgb <- paste(rgb, collapse = "")
  return(rgb)
}


#Ressort un vecteur avec les cellules jaunes
#Par contre il parcourt le data frame en lignes et pas en colonnes
mycolor <- list(jaune = "ffff00")
m     <- match(sapply(styles, cellColor), mycolor)
labs  <- names(mycolor)[m]



data_t <- read.xlsx("C:/Users/h.marthinet/Documents/Travail/02 - Projets/004 - CLES/0 - Base/2022-02-10_AffectionsCIM10_conges_GT.xlsx", 
                  sheetIndex = 1, encoding = "UTF-8")


#Je transpose data pour parcourir en ligne, et correspondre a la detection de couleurs
data <- t(data_t)
data_lab <- data[!is.na(data)]
data_lab <- data_lab[!is.na(labs[10:1415] == "jaune")]


#Nettoyage


data_lab <- data.frame(`Affection_principale` = data_lab)


data_w <- data.frame(cbind(data_t, `Affection_principale` = data_lab))
# data_w <- apply(data_w, 2, function(x) {str_replace_all(x, "Ã©", "é")}) 
# data_w <- apply(data_w, 2, function(x) {str_replace_all(x, "Ã»", "û")})
# data_w <- apply(data_w, 2, function(x) {str_replace_all(x, "Â¢", "Ã¢")})
# data_w <- apply(data_w, 2, function(x) {str_replace_all(x, "Ã¨", "è")})
# data_w <- apply(data_w, 2, function(x) {str_replace_all(x, "Ã´", "ô")})
# data_w <- apply(data_w, 2, function(x) {str_replace_all(x, "Ã???", "E")})
# data_w <- apply(data_w, 2, function(x) {str_replace_all(x, "Âª", "Ãª")})
# data_w <- apply(data_w, 2, function(x) {str_replace_all(x, "Ã", "à")})
# data_w <- apply(data_w, 2, function(x) {str_replace_all(x, "à¯", "ï")})



write.xlsx(data_w, 
           file = "T:/ECHANGE/MARTHINET Hugo/2022-02-10_AffectionsCIM10_conges_GT_HM_2.xlsx",
           row.names = FALSE)
write.csv2(data_w, 
           file = "T:/ECHANGE/MARTHINET Hugo/2022-02-10_AffectionsCIM10_conges_GT_HM_2.csv",
           row.names = FALSE,
           fileEncoding = "UTF-8")
