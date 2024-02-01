library(ldbounds)
library(PwrGSD)
library(rpact)
library(gsDesign)



n_interim <- 3
duree <- 30
(timings <- commonbounds(n_interim, t = (1:n_interim)/n_interim, t2 = t,
             iuse = "OF", alpha = 0.05, sides = 2))


(time <- timings$time2)
(t_interim <- duree*timings$time2)





# TWO SIDED

summary(ldbounds::ldBounds(
  time,
  iuse = 1,
  alpha = 0.05,
  sides = 2
))


rpact::getDesignGroupSequential(
  sided = 2, alpha = 0.05,
  informationRates = time, typeOfDesign = "asOF"
)

# gsDesign::gsDesign(k = 3,
#                    timing = time, test.type = 2,
#                    alpha = 0.05, beta = 0.2,
#                    sfu = "OF")

# ONE SIDED

summary(ldbounds::ldBounds(
  time,
  iuse = 1,
  alpha = 0.025,
  sides = 1
))

rpact::getDesignGroupSequential(
  sided = 1, alpha = 0.025,
  informationRates = time, typeOfDesign = "asOF"
)

PwrGSD::GrpSeqBnds(frac=time,
                   EfficacyBoundary=LanDemets(alpha=0.025, spending=ObrienFleming))




# TWO SIDED WITH EFFICACY AND FUTILITY

# rpact::getDesignGroupSequential(
#   sided = 1, alpha = 0.025,
#   informationRates = time, typeOfDesign = "asOF",
#   futilityBounds = c(0.01, 0.01), bindingFutility = FALSE
# )
#
#
# rpact::getDesignGroupSequential(
#   sided = 1, alpha = 0.025,
#   informationRates = time, typeOfDesign = "asOF",
#   futilityBounds = c(0.01, 0.01), bindingFutility = FALSE
# )$alphaSpent*2
#
# getDesignCharacteristics(rpact::getDesignGroupSequential(
#   sided = 1, alpha = 0.025,
#   informationRates = time, typeOfDesign = "asOF",
#   futilityBounds = c(0, 0), bindingFutility = FALSE
# ))







n_interim <- 2
duree <- 30
(timings <- commonbounds(n_interim, t = (1:n_interim)/n_interim, t2 = (1:n_interim)/n_interim,
                         iuse = "OF", alpha = 0.05, sides = 2))


(time <- timings$time)
(t_interim <- duree*timings$time)





# TWO SIDED

summary(ldbounds::ldBounds(
  time,
  iuse = 1,
  alpha = 0.05,
  sides = 2
))
