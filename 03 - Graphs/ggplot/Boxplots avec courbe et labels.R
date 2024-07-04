# .---IMPRESSION DES RESULTATS ---. ####

pdf("3 - Résultats/Courbes apprentissage.pdf")

par(mfrow = c(2,1))

boxplot(formula = installation ~ operation,
        data = DATA_apprentissage,
        main = paste0("Total des critères d'installation (p=", 
                      round(summary(lm(installation ~ operation,  DATA_apprentissage))$coefficient[2, 4], 4), ")"),
        ylab = "", xlab = "Numéro de l'opération",
        ylim = c(min(DATA_apprentissage$installation), 
                 max(DATA_apprentissage$installation)+1.5),
        boxwex = 0.7, range = 0)
points(x = 1:max(DATA_apprentissage$operation),
       y = by(DATA_apprentissage$installation, DATA_apprentissage$operation, mean),
       type = "b", col = "firebrick3", lwd = 2)
text(x = 1:max(DATA_apprentissage$operation),
     y = by(DATA_apprentissage$installation, DATA_apprentissage$operation, max)+0.75,
     labels = paste0("(", table(DATA_apprentissage$operation), ")"))


boxplot(formula = realisation ~ operation,
        data = DATA_apprentissage,
        main = paste0("Total des critères de réalisation technique (p=", 
        round(summary(lm(realisation ~ operation,  DATA_apprentissage))$coefficient[2, 4], 4), ")"),
        ylab = "", xlab = "Numéro de l'opération",
        ylim = c(min(DATA_apprentissage$realisation), 
                 max(DATA_apprentissage$realisation)+1.5),
        boxwex = 0.7, range = 0)
points(x = 1:max(DATA_apprentissage$operation),
       y = by(DATA_apprentissage$realisation, DATA_apprentissage$operation, mean),
       type = "b", col = "firebrick3", lwd = 2)
text(x = 1:max(DATA_apprentissage$operation),
     y = by(DATA_apprentissage$realisation, DATA_apprentissage$operation, max)+0.75,
     labels = paste0("(", table(DATA_apprentissage$operation), ")"))




boxplot(formula = total ~ operation,
        data = DATA_apprentissage,
        main = paste0("Total des critères d'installation et de réalisation technique (p=", 
        round(summary(lm(total ~ operation,  DATA_apprentissage))$coefficient[2, 4], 4), ")"),
        ylab = "", xlab = "Numéro de l'opération",
        ylim = c(min(DATA_apprentissage$total), 
                 max(DATA_apprentissage$total)+2),
        boxwex = 0.7, range = 0)
points(x = 1:max(DATA_apprentissage$operation),
       y = by(DATA_apprentissage$total, DATA_apprentissage$operation, mean),
       type = "b", col = "firebrick3", lwd = 2)
text(x = 1:max(DATA_apprentissage$operation),
     y = by(DATA_apprentissage$total, DATA_apprentissage$operation, max)+0.1,
     labels = paste0("(", table(DATA_apprentissage$operation), ")"))





boxplot(formula = t.tot ~ operation,
        data = DATA_apprentissage,
        main = paste0("Temps total d'opération (en minutes) (p=", 
        round(summary(lm(t.tot ~ operation,  DATA_apprentissage))$coefficient[2, 4], 4), ")"),
        ylab = "", xlab = "Numéro de l'opération",
        ylim = c(min(DATA_apprentissage$t.tot), 
                 max(DATA_apprentissage$t.tot)+2),
        boxwex = 0.7, range = 0)
points(x = 1:max(DATA_apprentissage$operation),
       y = by(DATA_apprentissage$t.tot, DATA_apprentissage$operation, mean),
       type = "b", col = "firebrick3", lwd = 2)
text(x = 1:max(DATA_apprentissage$operation),
     y = by(DATA_apprentissage$t.tot, DATA_apprentissage$operation, max)+0.1,
     labels = paste0("(", table(DATA_apprentissage$operation), ")"))





boxplot(formula = ressenti ~ operation,
        data = DATA_apprentissage,
        main = paste0("Ressenti de difficulté de l'opérateur (p=", 
        round(summary(lm(ressenti ~ operation,  DATA_apprentissage))$coefficient[2, 4], 4), ")"),
        ylab = "", xlab = "Numéro de l'opération",
        ylim = c(min(DATA_apprentissage$ressenti, na.rm = TRUE), 
                 max(DATA_apprentissage$ressenti, na.rm = TRUE)+1.5),
        boxwex = 0.7, range = 0)
points(x = 1:max(DATA_apprentissage$operation),
       y = by(DATA_apprentissage$ressenti, DATA_apprentissage$operation, mean, na.rm = TRUE),
       type = "b", col = "firebrick3", lwd = 2)
text(x = 1:max(DATA_apprentissage$operation),
     y = by(DATA_apprentissage$ressenti, DATA_apprentissage$operation, max, na.rm = TRUE)+0.75,
     labels = paste0("(", table(DATA_apprentissage$operation), ")"))

dev.off()



desc.write(desc, "3 - Résultats/Descriptif patients.xlsx",
           columnsBorders = FALSE)
desc.write(desc_apprentissage, "3 - Résultats/Descriptif apprentissage.xlsx",
           columnsBorders = FALSE)
