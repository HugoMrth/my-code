rm(list = ls())

library(dplyr)
library(stringr)

GPC <- function(
  data, 
  group,
  priorities,
  labels = NULL
) {
  # data <- data
  # priorities <- c("Sepal.Width.bin", "Species2", "Sepal.Length")
  # group <- "group"

  indC <- which(data[, "group"] == "Control")
  indT <- which(data[, "group"] == "Treatment")
  pc <- sapply(1:length(priorities), function(col) {
    x <- data[, priorities[col]]
    resfin <- matrix(NA, nrow = length(indT) * length(indC), ncol = length(priorities))
    res <- matrix(NA, nrow = length(indT),  ncol = length(indC))
    res <- sapply(1:sum(data[, "group"] == "Control"), function(y) {
      res[, y] <- ifelse(x[indC[y]] == x[indT], "Tie", x[indC[y]] > x[indT])
    })
    resfin[, col] <- as.vector(res)
  })
  
  gpc <- as.data.frame(cbind(
    pc[, 1],
    sapply(2:length(priorities), function(col) {
      apply(pc[, 1:col], 1, function(x) {
        ifelse(any(x[-length(x)] != "Tie"), NA, x[length(x)])
      })
    }),
    apply(pc,1,function(x) {ifelse(all(x == "Tie"), "Tie", x[which(x != "Tie")[1]])})
  ))
  
  tab <- as.data.frame(t(apply(gpc, 2, table))[, c(3, 1, 2)])
  
  ifelse(is.null(labels),
         rownames(tab) <- c(priorities, "Total"),
         rownames(tab) <- c(labels, "Total"))
  colnames(tab) <- c("Win", "Lose", "Tie")
  
  tab$N <- rowSums(tab)
  tab$NTB <- round((tab$Win - tab$Lose)/tab$N, 3)
  tab$WR <- round(tab$Win / tab$Lose, 2)
  tab$SO <- round((tab$Win + 0.5*tab$Tie) / (tab$Lose + 0.5*tab$Tie), 2)
  tab$Win <- paste0(tab$Win, " (", formatC(tab$Win/tab$N*100, format = "f", digits = 1), ")")
  tab$Lose <- paste0(tab$Lose, " (", formatC(tab$Lose/tab$N*100, format = "f", digits = 1), ")")
  tab$Tie <- paste0(tab$Tie, " (", formatC(tab$Tie/tab$N*100, format = "f", digits = 1), ")")
  
  return(tab)
}


data <- iris %>%
  mutate(
    Species2 = as.numeric(Species),
    Sepal.Width.bin = ifelse(Sepal.Width > 3.2, 1, 0),
    group = sample(c("Control", "Treatment"), 150, replace = TRUE)
  )

GPC(data = data, 
    group = "group", 
    priorities = c("Sepal.Width.bin", "Species2", "Sepal.Length"),
    labels = c("Width > 3.2", "Species", "Length"))



#### 450 lignes = ~50k comparaisons
# Time difference of 0.8980849 secs

#### 750 lignes = ~140k comparaisons
# Time difference of 3.264498 secs

#### 1500 lignes = ~550k comparaisons
# Time difference of 16.0319 secs
