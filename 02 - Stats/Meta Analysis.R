# https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/effects.html
# https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/effects.html
# https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/effects.html
# https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/effects.html
# https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/effects.html
# https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/effects.html
# https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/effects.html
# https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/effects.html





#### Pooling effect sizes ####

#### _Fixed Effect Model ####

# Load dmetar, esc and tidyverse (for pipe)
library(dmetar)
library(esc)
library(tidyverse)

# Load data set from dmetar
data(SuicidePrevention)

# Calculate Hedges' g and the Standard Error
# - We save the study names in "study".
# - We use the pmap_dfr function to calculate the effect size
#   for each row.
SP_calc <- pmap_dfr(SuicidePrevention, 
                    function(mean.e, sd.e, n.e, mean.c,
                             sd.c, n.c, author, ...){
                      esc_mean_sd(grp1m = mean.e,
                                  grp1sd = sd.e,
                                  grp1n = n.e,
                                  grp2m = mean.c,
                                  grp2sd = sd.c,
                                  grp2n = n.c,
                                  study = author,
                                  es.type = "g") %>% 
                        as.data.frame()}) 

# Let us catch a glimpse of the data
# The data set contains Hedges' g ("es") and standard error ("se")
glimpse(SP_calc)


# Calculate the inverse variance-weights for each study
SP_calc$w <- 1/SP_calc$se^2

# Then, we use the weights to calculate the pooled effect
pooled_effect <- sum(SP_calc$w*SP_calc$es)/sum(SP_calc$w)
pooled_effect






#### _Effect Size Pooling ####


# If possible, it is preferable to use raw data in our meta-analysis. 
# This makes it easier for others to understand how we calculated the effect sizes, and replicate the results. 
# Yet, using raw data is often not possible in practice, because studies often report their results in a different way.
# 
# This leaves us no other choice than to pre-calculate the desired effect size for each study right away so that all have the same format.
# 
# The function of choice for pre-calculated effect sizes is metagen.
# 
# There are six core arguments which can be specified in each function:
#   studlab. This argument associates each effect size with a study label. 
#     If we have the name or authors of our studies stored in our data set, 
#     we simply have to specify the name of the respective column (e.g. studlab = author).
#   sm. This argument controls the summary measure, the effect size metric we want to use in our meta-analysis. 
#     This option is particularly important for functions using raw effect size data. 
#     The {meta} package uses codes for different effect size formats, for example "SMD" or "OR". 
#     The available summary measures are not the same in each function, and we will discuss the most common options in each case in the following sections.
#   fixed. We need to provide this argument with a logical (TRUE or FALSE), 
#     indicating if a fixed-effect model meta-analysis should be calculated22.
#   random. In a similar fashion, this argument controls if a random-effects model should be used. 
#     If both comb.fixed and comb.random are set to TRUE, both models are calculated and displayed23.
#   method.tau. This argument defines the τ2
#   estimator. All functions use the codes for different estimators that we already presented 
#     in the previous chapter (e.g. for the DerSimonian-Laird method: method.tau = "DL").
#   method.random.ci. This argument controls how confidence intervals should be calculated when 
#     using the random-effects model. When setting method.random.ci = "HK", the Knapp-Hartung adjustment will be applied. 
#     In older versions of {meta}, this feature was controlled by setting the hakn argument to TRUE instead. 
#     Using the Knapp-Hartung adjustment is advisable in most cases (see Chapter 4.1.2.2).
#   data. In this argument, we provide {meta} with the name of our meta-analysis data set.
#   title (not mandatory). This argument takes a character string with the name of the analysis. 
#     While it is not essential to provide input for this argument, it can help us to identify the analysis later on.


library(tidyverse) # needed for 'glimpse'
library(dmetar)
library(meta)

data(ThirdWave)
glimpse(ThirdWave)

# There are two function-specific arguments which we always have to specify when using the function:
#   TE. The name of the column in our data set which contains the calculated effect sizes.
#   seTE. The name of the column in which the standard error of the effect size is stored.


m.gen <- metagen(TE = TE,
                 seTE = seTE,
                 studlab = Author,
                 data = ThirdWave,
                 sm = "SMD",
                 fixed = FALSE,
                 random = TRUE,
                 method.tau = "REML",
                 method.random.ci = "HK",
                 title = "Third Wave Psychotherapies")
summary(m.gen)


#### __ Pooled HR examples ####
library(survival)
library(survminer)
library(meta)


logHR <- log(c(0.95, 1.5))
selogHR <- c(0.25, 0.35)
metagen(logHR, selogHR, sm = "HR")


#### ___With summary data ####

logHR <- c(-0.02, -0.17, -0.24, -0.12, -0.21)
selogHR <- c(0.22, 0.21, 0.14, 0.15, 0.12)
studies <- paste("Study", 1:5)
m.gen <- (metagen(logHR, selogHR, sm = "HR",
                  studlab = studies))
summary(m.gen)

#### ___With models ####

res.cox <- coxph(Surv(time, status) ~ sex, data = lung)
summary(res.cox)
# lnHR
summary(res.cox)$coefficients[1]
# se lnHR
summary(res.cox)$coefficients[3]


cox1 <- coxph(Surv(time, status) ~ sex, data = lung[1:114,])
cox2 <- coxph(Surv(time, status) ~ sex, data = lung[115:228,])
studies <- c("First half of data", "Second half of data")
m.gen <- metagen(c(summary(cox1)$coefficients[1], summary(cox2)$coefficients[1]), 
                 c(summary(cox1)$coefficients[3], summary(cox2)$coefficients[3]), 
                 sm = "HR",
                 studlab = studies)
summary(m.gen)
summary(res.cox)












#### _Binary Outcomes ####
library(dmetar)
library(tidyverse)
library(meta)

data(DepressionMortality)
glimpse(DepressionMortality)

# There are eight important function-specific arguments in metabin:
#   - event.e. The number of events in the treatment/experimental group.
#   - n.e. The number of observations in the treatment/experimental group.
#   - event.c. The number of events in the control group.
#   - n.c. The number of observations in the control group.
#   -  method. The pooling method to be used. This can either be 
#     "Inverse" (generic inverse-variance pooling), 
#     "MH" (Mantel-Haenszel; default and recommended for fixed-effect models), 
#     "Peto" (Peto method), or 
#     "SSW" (Bakbergenuly-sample size method; only when sm = "OR").
#   - sm. The summary measure (i.e. effect size metric) to be calculated. We can use 
#     "RR" for the risk ratio and 
#     "OR" for the odds ratio.
#   - incr. The increment to be added for continuity correction of zero cells. 
#       If we specify incr = 0.5, an increment of 0.5 is added. 
#       If we set incr = "TACC", the treatment arm continuity correction method is used (see Chapter 3.3.2.1). 
#       As mentioned before, it is usually recommended to leave out this argument and not apply continuity corrections.
#   - MH.exact. 
#         If method = "MH", we can set this argument to TRUE, indicating that we do not want 
#         that a continuity correction is used for the Mantel-Haenszel method.


m.bin <- metabin(event.e = event.e, 
                 n.e = n.e,
                 event.c = event.c,
                 n.c = n.c,
                 studlab = author,
                 data = DepressionMortality,
                 sm = "RR",
                 method = "MH",
                 MH.exact = TRUE,
                 fixed = TRUE,
                 random = TRUE,
                 method.tau = "PM",
                 method.random.ci = "HK",
                 title = "Depression and Mortality")
summary(m.bin)
m.bin_update <- update(m.bin, method.tau = "REML")


#### _Pooling Pre-Calculated Binary Effect Sizes ####

DepressionMortality$TE <- m.bin$TE
DepressionMortality$seTE <- m.bin$seTE

# Set seTE of study 7 to NA
DepressionMortality$seTE[7] <- NA

# Create empty columns 'lower' and 'upper'
DepressionMortality[,"lower"] <- NA
DepressionMortality[,"upper"] <- NA

# Fill in values for 'lower' and 'upper' in study 7
# As always, binary effect sizes need to be log-transformed
DepressionMortality$lower[7] <- log(1.26)
DepressionMortality$upper[7] <- log(2.46)

DepressionMortality[,c("author", "TE", "seTE", "lower", "upper")]


# We only have to provide the name of the columns containing the lower and upper 
# bound of the confidence interval to the lower and upper argument. 
# The metagen function will then use this information to weight the effects 
# when the standard error is not available. Our function call looks like this:
m.gen_bin <- metagen(TE = TE,
                     seTE = seTE,
                     lower = lower,
                     upper = upper,
                     studlab = author,
                     data = DepressionMortality,
                     sm = "RR",
                     method.tau = "PM",
                     fixed = FALSE,
                     random = TRUE,
                     title = "Depression Mortality (Pre-calculated)")

summary(m.gen_bin)



#### Forest Plots ####
meta::forest(m.gen, 
             sortvar = TE,
             prediction = TRUE, 
             print.tau2 = FALSE,
             leftlabs = c("Author", "g", "SE"))


meta::forest(m.gen, 
             sortvar = TE,
             prediction = TRUE, 
             print.tau2 = FALSE,
             leftcols = c("studlab", "TE", "seTE", "RiskOfBias"),
             leftlabs = c("Author", "g", "SE", "Risk of Bias"))


meta::forest(m.gen, layout = "JAMA")
meta::forest(m.gen, layout = "RevMan5")



#### Meta Regression ####

year <- c(2014, 1998, 2010, 1999, 2005, 2014, 
          2019, 2010, 1982, 2020, 1978, 2001,
          2018, 2002, 2009, 2011, 2011, 2013)

m.gen.reg <- metareg(m.gen, ~year)
m.gen.reg


bubble(m.gen.reg, studlab = TRUE)

metareg(m.gen, RiskOfBias)