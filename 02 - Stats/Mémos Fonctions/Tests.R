#### Tendance ####

#### __1 sample ####

# https://vsp.pnnl.gov/help/vsample/design_trend_mann_kendall.htm
trend::mk.test()
Kendall::MannKendall()

#### __2 samples

# 2 numeric samples
stats::prop.trend.test()

# 1 numeric 1 binary
DescTools::CochranArmitageTest()


#### Proportions ####

####__Count data
rstatix::mcnemar_test()


#### Variances ####

car::leveneTest()
DescTools::LeveneTest()

stats::bartlett.test()




#### Autocorrelation ####

car::durbinWatsonTest()
DescTools::DurbinWatsonTest()



#### Multiple Comparisons ####

DescTools::DunnTest()
