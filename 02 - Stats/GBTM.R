library(gbmt)


#### Data ####
data(agrisus)
data(agrisus2)


# names of indicators (just a subset for illustration)
varNames <- c("TFP_2005", "NetCapital_GVA",
              "Income_rur", "Unempl_rur", "GHG_UAA", "GNB_N_UAA")



#### Trajectory Model ####

# model with 2 degrees and 3 groups using the imputed dataset
# - log ratio to the mean is used as normalisation (scaling=4), thus values
# represent relative changes with respect to country averages (see Magrini, 2022)
# - by default, standardization (scaling=2) is used.
m3_2 <- gbmt(x.names=varNames, unit="Country", time="Year", d=2, ng=3, data=agrisus2, scaling=4)

# resulting groups
m3_2$assign.list
# estimated group trajectories
m3_2$fitted
# summary of regressions by group
summary(m3_2)
# fit a model with 4 groups
m4_2 <- gbmt(x.names=varNames, unit="Country", time="Year", d=2, ng=4, data=agrisus2,
             scaling=4)
rbind(m3_2$ic, m4_2$ic) ## comparison



# group trajectories including 3 steps ahead prediction
mar0 <- c(3.1,2.55,3.1,1.2)
plot(m3_2, n.ahead=3, mar=mar0) ## overlapped groups
plot(m3_2, group=1, n.ahead=3, mar=mar0) ## group 1
plot(m3_2, group=2, n.ahead=3, mar=mar0) ## group 2
plot(m3_2, group=3, n.ahead=3, mar=mar0) ## group 3
# same scale to ease comparisons
plot(m3_2, n.ahead=3, mar=mar0, equal.scale=TRUE)
plot(m3_2, group=1, n.ahead=3, mar=mar0, equal.scale=TRUE, trim=0.05)
plot(m3_2, group=2, n.ahead=3, mar=mar0, equal.scale=TRUE, trim=0.05)
plot(m3_2, group=3, n.ahead=3, mar=mar0, equal.scale=TRUE, trim=0.05)
# overlapped groups with transparency
plot(m3_2, group=1, n.ahead=3, mar=mar0, equal.scale=TRUE, trim=0.05,
     transparency=80)
# trajectories including 3 steps ahead prediction for unit 'Italy'
plot(m3_2, unit="Italy", n.ahead=3, transparency=80)






# 3 steps ahead prediction of group trajectories
predict(m3_2, n.ahead=3)
predict(m3_2, n.ahead=3, in.sample=TRUE) ## include in-sample prediction
# 3 steps ahead prediction for unit 'Italy'
predict(m3_2, unit="Italy", n.ahead=3)
predict(m3_2, unit="Italy", n.ahead=3, in.sample=TRUE) ## include in-sample prediction






# pretend that 'Italy' is a new unit
posterior(m3_2, newdata=agrisus2[which(agrisus2$Country=="Italy"),])
# consider only the last 3 years
posterior(m3_2, newdata=
            agrisus2[which(agrisus2$Country=="Italy"&agrisus2$Year>=2016),]
)
# provide more than one new unit
posterior(m3_2, newdata=
            agrisus2[which(agrisus2$Country%in%c("Italy","Austria","Greece")),]
)
