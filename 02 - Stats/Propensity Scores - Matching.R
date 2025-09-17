formule <- paste(colnames(screened_covars)[colnames(screened_covars) != "type_pec"], collapse = " + ")
formule <- as.formula(paste("type_pec ~", formule))
ps.macthing <- matchit(
  formula = formule,
  data = screened_covars[!is.na(screened_covars$fdep),],
  method = "nearest",
  distance = "logit",
  ratio = 1,
  caliper = 0.001)

ps.macthing
table(ps.macthing$weights)

data_stddiff <- recodeStdDiff(screened_covars[!is.na(screened_covars$fdep),])
data_stddiff$weight <- ps.macthing$weights
vars <- colnames(data_stddiff)[colnames(data_stddiff) != "type_pec" & colnames(data_stddiff) != "weight"] # Include only numeric or binary coded vars
smds_before <- sapply(vars, function(x) smd_unweighted(data_stddiff, "type_pec", x))
smds_after  <- sapply(vars, function(x) smd_weighted(data_stddiff, "type_pec", x, "weight"))
tab_stddiff <- data.frame(Variable = vars, SMD_Before = round(smds_before, 3), SMD_After  = round(smds_after, 3))
sum(abs(tab_stddiff$SMD_Before) > 0.1)
sum(abs(tab_stddiff$SMD_After) > 0.1)
summary(tab_stddiff)

data_stddiff2 <- data_stddiff[data_stddiff$euroscoreFaible == 1,]
smds_after  <- sapply(vars, function(x) smd_weighted(data_stddiff2, "type_pec", x, "weight"))
tab_stddiff$smd_after_eurofaible <- smds_after
sum(abs(smds_after) > 0.1, na.rm = T)

data_stddiff2 <- data_stddiff[data_stddiff$euroscoreIntermediaire == 1,]
smds_after  <- sapply(vars, function(x) smd_weighted(data_stddiff2, "type_pec", x, "weight"))
tab_stddiff$smd_after_eurointer <- smds_after
sum(abs(smds_after) > 0.1, na.rm = T)

data_stddiff2 <- data_stddiff[data_stddiff$euroscoreEleve == 1,]
smds_after  <- sapply(vars, function(x) smd_weighted(data_stddiff2, "type_pec", x, "weight"))
tab_stddiff$smd_after_euroeleve <- smds_after
sum(abs(smds_after) > 0.1, na.rm = T)
