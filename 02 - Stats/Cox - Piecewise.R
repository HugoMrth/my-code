rm(list = ls())

library(survival)
mgus2

#### Fine and Gray ####

# Treat time to death and plasma cell malignancy as competing risks
etime <- with(mgus2, ifelse(pstat==0, futime, ptime))
event <- with(mgus2, ifelse(pstat==0, 2*death, 1))
event <- factor(event, 0:2, labels=c("censor", "pcm", "death"))

# FG model for PCM
data_fg <- finegray(Surv(etime, event) ~ ., data = mgus2)
fgfit <- coxph(Surv(fgstart, fgstop, fgstatus) ~ age + sex,
               weight = fgwt, data = data_fg)

fgfit

# Compute the weights separately by sex
data_fg2 <- finegray(Surv(etime, event) ~ . + strata(sex),
                  data=mgus2, na.action=na.pass)




#### Piecewise Fine and Gray ####

mgus2_pw <- survSplit(Surv(etime, event) ~., data = mgus2, zero = -1,
                   cut=c(100, 200), episode ="tgroup")
data_pw <- finegray(Surv(etime, event) ~ ., data = mgus2_pw)

# Partie traitement de reference
pwfit <- coxph(Surv(fgstart, fgstop, fgstatus) ~ sex*strata(tgroup), 
             data = data_pw, weights = fgwt)

c(t1 = paste0(formatC(exp(coef(pwfit)[1]), 2, format = "f"), " [",
                  formatC(exp(confint(pwfit)[1, 1]), 2, format = "f"), "-",
                  formatC(exp(confint(pwfit)[1, 2]), 2, format = "f"), "]"),
      t2 = paste0(formatC(exp(coef(pwfit)[1] + coef(pwfit)[2]), 2, format = "f"), " [",
                  formatC(exp(confint(pwfit)[1, 1] + confint(pwfit)[2, 1]), 2, format = "f"), "-",
                  formatC(exp(confint(pwfit)[1, 2] + confint(pwfit)[2, 2]), 2, format = "f"), "]"),
      t3 = paste0(formatC(exp(coef(pwfit)[1] + coef(pwfit)[3]), 2, format = "f"), " [",
                  formatC(exp(confint(pwfit)[1, 1] + confint(pwfit)[3, 1]), 2, format = "f"), "-",
                  formatC(exp(confint(pwfit)[1, 2] + confint(pwfit)[3, 2]), 2, format = "f"), "]"))

# c(t1 = paste0(formatC(exp(coef(pwfit)[1]), 2, format = "f"), " [",
#               formatC(exp(coef(pwfit)[1] - 1.96*summary(pwfit)$coefficients[1, 3]), 2, format = "f"), "-",
#               formatC(exp(coef(pwfit)[1] + 1.96*summary(pwfit)$coefficients[1, 3]), 2, format = "f"), "]"),
#   t2 = paste0(formatC(exp(coef(pwfit)[1] + coef(pwfit)[2]), 2, format = "f"), " [",
#               formatC(exp((coef(pwfit)[1] - 1.96*summary(pwfit)$coefficients[1, 3]) +
#                             (coef(pwfit)[2] - 1.96*summary(pwfit)$coefficients[2, 3])), 2, format = "f"), "-",
#               formatC(exp((coef(pwfit)[1] + 1.96*summary(pwfit)$coefficients[1, 3]) +
#                             (coef(pwfit)[2] + 1.96*summary(pwfit)$coefficients[2, 3])), 2, format = "f"), "]"),
#   t3 = paste0(formatC(exp(coef(pwfit)[1] + coef(pwfit)[2]), 2, format = "f"), " [",
#               formatC(exp((coef(pwfit)[1] - 1.96*summary(pwfit)$coefficients[1, 3]) +
#                             (coef(pwfit)[3] - 1.96*summary(pwfit)$coefficients[3, 3])), 2, format = "f"), "-",
#               formatC(exp((coef(pwfit)[1] + 1.96*summary(pwfit)$coefficients[1, 3]) +
#                             (coef(pwfit)[3] + 1.96*summary(pwfit)$coefficients[3, 3])), 2, format = "f"), "]"))

c(t1 = paste0(formatC(exp(coef(pwfit)[1]), 2, format = "f"), " [",
              formatC(exp(coef(pwfit)[1] - qnorm(0.975)*summary(pwfit)$coefficients[1, 3]), 2, format = "f"), "-",
              formatC(exp(coef(pwfit)[1] + qnorm(0.975)*summary(pwfit)$coefficients[1, 3]), 2, format = "f"), "]"),
  t2 = paste0(formatC(exp(coef(pwfit)[1] + coef(pwfit)[2]), 2, format = "f"), " [",
              formatC(exp(coef(pwfit)[1] + coef(pwfit)[2] - 
                            qnorm(0.975)*sqrt(summary(pwfit)$coefficients[1, 3]^2 + summary(pwfit)$coefficients[2, 3]^2 + 2*vcov(pwfit)[1, 2])), 2, format = "f"), "-",
              formatC(exp(coef(pwfit)[1] + coef(pwfit)[2] + 
                            qnorm(0.975)*sqrt(summary(pwfit)$coefficients[1, 3]^2 + summary(pwfit)$coefficients[2, 3]^2 + 2*vcov(pwfit)[1, 2])), 2, format = "f"), "]"),
  t3 = paste0(formatC(exp(coef(pwfit)[1] + coef(pwfit)[3]), 2, format = "f"), " [",
              formatC(exp(coef(pwfit)[1] + coef(pwfit)[3] - 
                            qnorm(0.975)*sqrt(summary(pwfit)$coefficients[1, 3]^2 + summary(pwfit)$coefficients[3, 3]^2 + 2*vcov(pwfit)[1, 3])), 2, format = "f"), "-",
              formatC(exp(coef(pwfit)[1] + coef(pwfit)[3] +
                            qnorm(0.975)*sqrt(summary(pwfit)$coefficients[1, 3]^2 + summary(pwfit)$coefficients[3, 3]^2 + 2*vcov(pwfit)[1, 3])), 2, format = "f"), "]"))





#### Test Piecewise a la mein ####

# Treat time to death and plasma cell malignancy as competing risks
etime <- with(mgus2, ifelse(pstat==0, futime, ptime))
event <- with(mgus2, ifelse(pstat==0, 2*death, 1))
event <- factor(event, 0:2, labels=c("censor", "pcm", "death"))


#### _0-100 ####

# Excluding data before the focus window and censor those after
etime_cens <- etime[etime >= 0]
table(etime_cens < 100)
event_cens <- event[etime >= 0]
table(event_cens, etime_cens < 100)
event_cens[etime_cens >= 100] <- "censor"
table(event_cens, etime_cens < 100)

mgus2_cens <- mgus2[etime >= 0, ]

# FG model for PCM
data_fg_cens <- finegray(Surv(etime_cens, event_cens) ~ ., data = mgus2_cens)
fgfit <- coxph(Surv(fgstart, fgstop, fgstatus) ~ sex,
               weight = fgwt, data = data_fg_cens)

paste0(formatC(exp(coef(fgfit)), 2, format = "f"), " [",
       formatC(exp(confint(fgfit)[1]), 2, format = "f"), "-",
       formatC(exp(confint(fgfit)[2]), 2, format = "f"), "]")




#### _100-200 ####

# Excluding data before the focus window and censor those after
etime_cens <- etime[etime >= 100]
table(etime_cens < 200)
event_cens <- event[etime >= 100]
table(event_cens, etime_cens < 200)
event_cens[etime_cens >= 200] <- "censor"
table(event_cens, etime_cens < 200)

mgus2_cens <- mgus2[etime >= 100, ]

# FG model for PCM
data_fg_cens <- finegray(Surv(etime_cens, event_cens) ~ ., data = mgus2_cens)
fgfit <- coxph(Surv(fgstart, fgstop, fgstatus) ~ sex,
               weight = fgwt, data = data_fg_cens)

paste0(formatC(exp(coef(fgfit)), 2, format = "f"), " [",
       formatC(exp(confint(fgfit)[1]), 2, format = "f"), "-",
       formatC(exp(confint(fgfit)[2]), 2, format = "f"), "]")



#### _200+ ####

# Excluding data before the focus window and censor those after
etime_cens <- etime[etime >= 200]
table(etime_cens < 500)
event_cens <- event[etime >= 200]
table(event_cens, etime_cens < 500)
event_cens[etime_cens >= 500] <- "censor"
table(event_cens, etime_cens < 500)

mgus2_cens <- mgus2[etime >= 200, ]

# FG model for PCM
data_fg_cens <- finegray(Surv(etime_cens, event_cens) ~ ., data = mgus2_cens)
fgfit <- coxph(Surv(fgstart, fgstop, fgstatus) ~ sex,
               weight = fgwt, data = data_fg_cens)

paste0(formatC(exp(coef(fgfit)), 2, format = "f"), " [",
       formatC(exp(confint(fgfit)[1]), 2, format = "f"), "-",
       formatC(exp(confint(fgfit)[2]), 2, format = "f"), "]")
