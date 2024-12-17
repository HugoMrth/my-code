rm(list = ls())

# Library corps humain
library(BodyMapR)
# https://github.com/TheMillerLab/BodyMapR/tree/main
library(bdgramR)
# https://github.com/josedv82/bdgramR
library(gganatogram)
# https://github.com/jespermaag/gganatogram
library(CHOIRBM)
# https://github.com/emcramer/CHOIRBM
library(ggplot2)
library(viridis)
library(gridExtra)

library(cartogram)
library(sf)
library(sfheaders)
library(mapsf)
library(sp)

library(tidyverse)
library(broom)



# Donnees brutes
data <- bdgramr(data = data, model = "athletesr") %>%
  mutate(
    #Id = paste(View, Part, Group, Muscle, Side, sep = "_"),
    x = -x,
    y = -y,
    Muscle = ifelse(Group == "Pectoral", "Pec_Major", as.character(Muscle))
  ) %>%
  dplyr::filter(Id %in% names(which(table(Id) != 1)))

apply(data[, 2:6], 2, table)
ggplot(data = data, aes(-x, -y, group = Id)) + geom_bdgramr()


# Hand-Wrist Joint ####
cond_insert <- data$Group %in% c("Hand", "Forearm") & data$Muscle %in% c("Hand", "Wrist")
data_insert <- data[cond_insert, ]
data_insert$y[data_insert$y == -312] <- -315
data_insert$y[data_insert$y == -318] <- -315
data[cond_insert, ] <- data_insert


# Foot-Ankle Joint ####
cond_insert <- data$Group %in% c("Foot")
data_insert <- data[cond_insert, ]
data_insert$y[data_insert$y == -568] <- -564
data_insert$y[data_insert$y == -561] <- -564
data[cond_insert, ] <- data_insert


# Ankle-Shin Joint ####
cond_insert <- (data$Group %in% c("Lower_Leg") |
                  data$Group %in% c("Foot")) &
  data$View == "Anterior"
data_insert <- data[cond_insert, ]
data_insert$y[data_insert$y == -553] <- -559
data_insert$y[data_insert$y == -546] <- -559
data[cond_insert, ] <- data_insert

# Shin-Knees Joint ####
cond_insert <- (data$Group %in% c("Lower_Leg") |
                  data$Group %in% c("Knee")) &
  data$View == "Anterior"
data_insert <- data[cond_insert, ]
data_insert$y[data_insert$y == -463] <- -460
data_insert$y[data_insert$y == -456] <- -460
data[cond_insert, ] <- data_insert


# Foot-Calves Joint ####
cond_insert <- (data$Group %in% c("Calves") |
                  data$Group %in% c("Foot")) &
  data$View == "Posterior"
data_insert <- data[cond_insert, ]
data_insert$y[data_insert$y == -513] <- -510
data_insert$y[data_insert$y == -506] <- -510
data_insert$y[data_insert$y == -553] <- -556
data_insert$y[data_insert$y == -560] <- -556
data[cond_insert, ] <- data_insert


# Calves-Knees Joint ####
cond_insert <- (data$Group %in% c("Calves") |
                  data$Group %in% c("Knee")) &
  data$View == "Posterior"
data_insert <- data[cond_insert, ]
data_insert$y[data_insert$y == -453] <- -450
data_insert$y[data_insert$y == -446] <- -450
data_insert$y[data_insert$y == -447] <- -450
data[cond_insert, ] <- data_insert


# Knees-Quadriceps Joint ####
cond_insert <- (data$Group %in% c("Quadriceps") |
                  data$Group %in% c("Knee"))  &
  data$View == "Anterior"
data_insert <- data[cond_insert, ]
data_insert$y[data_insert$y == -430] <- -430
data_insert$y[data_insert$y == -431] <- -430
data_insert$y[data_insert$y == -423] <- -430
data_insert$y[data_insert$y == -424] <- -430
data[cond_insert, ] <- data_insert


# Knees-Hamstrings Joint ####
cond_insert <- (data$Group %in% c("Hamstrings") |
                  data$Group %in% c("Knee"))  &
  data$View == "Posterior"
data_insert <- data[cond_insert, ]
data_insert$y[data_insert$y == -420] <- -415
data_insert$y[data_insert$y == -421] <- -415
data_insert$y[data_insert$y == -410] <- -415
data_insert$y[data_insert$y == -413] <- -415
data[cond_insert, ] <- data_insert



# Quadriceps-Groin Joint ####
# cond_insert <- (data$Group %in% c("Quadriceps") |
#                   data$Group %in% c("Groin"))  &
#   data$View == "Anterior"
# data_insert <- data[cond_insert, ]
# plot.new()
# plot(data_insert$x, data_insert$y, type = "n")
# text(data_insert$x, data_insert$y,
#      labels = rownames((data_insert)),
#      cex = .7)

data_insert <- data[1144:1117, ]
data_insert$Group <- "Quadriceps"
data_insert$Muscle <- "Quadriceps"
data_insert$Id <- 21

data <- rbind(data[1:1080, ],
              data_insert,
              data[1116:nrow(data), ])
rownames(data) <- 1:nrow(data)

# Quadriceps-Groin Right Side Joint
# cond_insert <- (data$Group %in% c("Quadriceps") |
#                   data$Group %in% c("Groin"))  &
#   data$View == "Anterior"
# data_insert <- data[cond_insert, ]
# plot.new()
# plot(data_insert$x, data_insert$y, type = "n")
# text(data_insert$x, data_insert$y,
#      labels = rownames((data_insert)),
#      cex = .7)

data_insert <- data[1185:1158, ]
data_insert$Group <- "Quadriceps"
data_insert$Muscle <- "Quadriceps"
data_insert$Id <- 24

data <- rbind(data[1:1186, ],
              data_insert,
              data[1226:nrow(data), ])
rownames(data) <- 1:nrow(data)



# Hamstrings-Groin Joint ####
# cond_insert <- (data$Group %in% c("Hamstrings") |
#                   data$Group %in% c("Groin"))  &
#   data$View == "Posterior"
# data_insert <- data[cond_insert, ]
# plot.new()
# plot(data_insert$x, data_insert$y, type = "n")
# text(data_insert$x, data_insert$y,
#      labels = rownames((data_insert)),
#      cex = .5)

data_insert <- data[2804:2781, ]
data_insert$Group <- "Hamstrings"
data_insert$Muscle <- "Hamstrings"
data_insert$Id <- 58

data <- rbind(data[1:2733, ],
              data_insert,
              data[2763:nrow(data), ])
rownames(data) <- 1:nrow(data)



# Hamstrings-Groin Left Side Joint
# cond_insert <- (data$Group %in% c("Hamstrings") |
#                   data$Group %in% c("Groin"))  &
#   data$View == "Posterior"
# data_insert <- data[cond_insert, ]
# plot.new()
# plot(data_insert$x, data_insert$y, type = "n")
# text(data_insert$x, data_insert$y,
#      labels = rownames((data_insert)),
#      cex = .5)

data_insert <- data[2852:2828, ]
data_insert$Group <- "Hamstrings"
data_insert$Muscle <- "Hamstrings"
data_insert$Id <- 61

data <- rbind(data[1:2867, ],
              data_insert,
              data[2896:nrow(data), ])
rownames(data) <- 1:nrow(data)

# Glutes-Groin/Hamstrings Joint ####
# cond_insert <- (data$Group %in% c("Hamstrings") |
#                   data$Group %in% c("Groin") |
#                   data$Muscle %in% c("Gluteus_Maximus"))  &
#   data$View == "Posterior"
# data_insert <- data[cond_insert, ]
# plot.new()
# plot(data_insert$x, data_insert$y, type = "n")
# text(data_insert$x, data_insert$y,
#      labels = rownames((data_insert)),
#      cex = .5)

data_insert <- data[c(2706:2702, 2773:2758, 2774:2775, 2815:2809), ]
data_insert$Group <- "Gluteus"
data_insert$Muscle <- "Gluteus_Maximus"
data_insert$Id <- 57

data <- rbind(data[1:2643, ],
              data_insert,
              data[2679:nrow(data), ])
rownames(data) <- 1:nrow(data)


ggplot(data = data, aes(-x, -y, group = Id)) + geom_bdgramr()


# Glutes-Groin/Hamstrings Right Side Joint
# cond_insert <- (data$Group %in% c("Hamstrings") |
#                   data$Group %in% c("Groin") |
#                   data$Muscle %in% c("Gluteus_Maximus"))  &
#   data$View == "Posterior"
# data_insert <- data[cond_insert, ]
# plot.new()
# plot(data_insert$x, data_insert$y, type = "n")
# text(data_insert$x, data_insert$y,
#      labels = rownames((data_insert)),
#      cex = .5)

data_insert <- data[c(2816:2811, 2853:2847, 2862:2854, 2926:2916), ]
data_insert$Group <- "Gluteus"
data_insert$Muscle <- "Gluteus_Maximus"
data_insert$Id <- 56

data <- rbind(data[1:2558, ],
              data_insert,
              data[2597:nrow(data), ])
rownames(data) <- 1:nrow(data)


# Posterior Groin Fusion ####
# cond_insert <- data$Group %in% c("Groin")  &
#   data$View == "Posterior"
# data_insert <- data[cond_insert, ]
# plot.new()
# plot(data_insert$x, data_insert$y, type = "n")
# text(data_insert$x, data_insert$y,
#      labels = rownames((data_insert)),
#      cex = .5)

data_insert <- data[c(2764:2789, 2818:2848, 2806:2811, 2798:2805), ]
data_insert$Side <- "Center"
data_insert$Id <- 60

data <- rbind(data[1:2763, ],
              data_insert,
              data[2849:nrow(data), ])
rownames(data) <- 1:nrow(data)


# Anterior Groin Fusion ####
# cond_insert <- data$Group %in% c("Groin")  &
#   data$View == "Anterior"
# data_insert <- data[cond_insert, ]
# plot.new()
# plot(data_insert$x, data_insert$y, type = "n")
# text(data_insert$x, data_insert$y,
#      labels = rownames((data_insert)),
#      cex = .5)

data_insert <- data[c(1110:1136, 1159:1185, 1149, 1148), ]
data_insert$Side <- "Center"
data_insert$Id <- 23

data <- rbind(data[1:1109, ],
              data_insert,
              data[1186:nrow(data), ])
rownames(data) <- 1:nrow(data)


ggplot(data = data, aes(-x, -y, group = Id)) + geom_bdgramr()





# Glutes-Lower Back Fusion ####
# cond_insert <- data$Group %in% c("Gluteus", "Lower_Back")  &
#   data$View == "Posterior"
# data_insert <- data[cond_insert, ]
# plot.new()
# plot(data_insert$x, data_insert$y, type = "n")
# text(data_insert$x, data_insert$y,
#      labels = rownames((data_insert)),
#      cex = .5)

data_insert <- data[c(2609:2593, 2670:2651), ]
data_insert$Group <- "Lower_Back"
data_insert$Muscle <- "Lower_Back"
data_insert$Id <- 54

data <- rbind(data[1:2384, ],
              data_insert,
              data[2418:nrow(data), ])
rownames(data) <- 1:nrow(data)

# Glutes-Lower Back Right Side Fusion
# cond_insert <- data$Group %in% c("Gluteus", "Lower_Back")  &
#   data$View == "Posterior"
# data_insert <- data[cond_insert, ]
# plot.new()
# plot(data_insert$x, data_insert$y, type = "n")
# text(data_insert$x, data_insert$y,
#      labels = rownames((data_insert)),
#      cex = .5)

data_insert <- data[c(2541:2517, 2596:2584), ]
data_insert$Group <- "Lower_Back"
data_insert$Muscle <- "Lower_Back"
data_insert$Id <- 55

data <- rbind(data[1:2455, ],
              data_insert,
              data[2491:nrow(data), ])
rownames(data) <- 1:nrow(data)



# Lower-Upper Back Fusion ####
# cond_insert <- data$Group %in% c("Upper_Back", "Lower_Back")  &
#   data$Muscle != 'Upper_Trapezius' &
#   data$View == "Posterior"
# data_insert <- data[cond_insert, ]
# plot.new()
# plot(data_insert$x, data_insert$y, type = "n")
# text(data_insert$x, data_insert$y,
#      labels = rownames((data_insert)),
#      cex = .5)

data_insert <- data[c(2365:2352, 2439:2423), ]
data_insert$Part <- "Upper_Body"
data_insert$Group <- "Upper_Back"
data_insert$Muscle <- "Upper_Back"
data_insert$Id <- 45

data <- rbind(data[1:1905, ],
              data_insert,
              data[1936:nrow(data), ])
rownames(data) <- 1:nrow(data)



# Lower-Upper Back Left Side Fusion
# cond_insert <- data$Group %in% c("Upper_Back", "Lower_Back")  &
#   data$Muscle != 'Upper_Trapezius' &
#   data$View == "Posterior"
# data_insert <- data[cond_insert, ]
# plot.new()
# plot(data_insert$x, data_insert$y, type = "n")
# text(data_insert$x, data_insert$y,
#      labels = rownames((data_insert)),
#      cex = .5)

data_insert <- data[c(2455:2441, 2520:2505), ]
data_insert$Part <- "Upper_Body"
data_insert$Group <- "Upper_Back"
data_insert$Muscle <- "Upper_Back"
data_insert$Id <- 44

data <- rbind(data[1:1838, ],
              data_insert,
              data[1870:nrow(data), ])
rownames(data) <- 1:nrow(data)


# UpperBack-Trapezius Fusion ####
# cond_insert <- data$Group %in% c("Upper_Back") &
#   data$View == "Posterior"
# data_insert <- data[cond_insert, ]
# plot.new()
# plot(data_insert$x, data_insert$y, type = "n")
# text(data_insert$x, data_insert$y,
#      labels = rownames((data_insert)),
#      cex = .5)

data_insert <- data[c(1775:1802, 1889, 1938, 1837, 1887, 1811:1836), ]
data_insert$Muscle <- "Upper_Trapezius"
data_insert$Side <- "Center"
data_insert$Id <- 42

data <- rbind(data[1:1774, ],
              data_insert,
              data[1837:nrow(data), ])
rownames(data) <- 1:nrow(data)


# UpperBack-Deltoids Posterior Fusion ####
# cond_insert <- (data$Muscle %in% c("Upper_Back") |
#   data$Muscle == "Deltoideus") &
#   data$View == "Posterior"
# data_insert <- data[cond_insert, ]
# plot.new()
# plot(data_insert$x, data_insert$y, type = "n")
# text(data_insert$x, data_insert$y,
#      labels = rownames((data_insert)),
#      cex = .5)

data_insert <- data[c(1885, 1977:1989, 1935:1974, 1886, 1887), ]
data_insert$Side <- "Right"
data_insert$Group <- "Arm"
data_insert$Muscle <- "Deltoideus"
data_insert$Id <- 46

data <- rbind(data[1:1934, ],
              data_insert,
              data[1990:nrow(data), ])
rownames(data) <- 1:nrow(data)


# UpperBack-Deltoids Posterior Left Side Fusion
# cond_insert <- (data$Muscle %in% c("Upper_Back") |
#                   data$Muscle == "Deltoideus") &
#   data$View == "Posterior"
# data_insert <- data[cond_insert, ]
# plot.new()
# plot(data_insert$x, data_insert$y, type = "n")
# text(data_insert$x, data_insert$y,
#      labels = rownames((data_insert)),
#      cex = .5)

data_insert <- data[c(1883, 2003:1991, 2043:2006, 1882, 1883), ]
data_insert$Side <- "Left"
data_insert$Group <- "Arm"
data_insert$Muscle <- "Deltoideus"
data_insert$Id <- 47

data <- rbind(data[1:1990, ],
              data_insert,
              data[2044:nrow(data), ])
rownames(data) <- 1:nrow(data)




# Deltoids-Triceps Posterior Fusion ####
# cond_insert <- data$Muscle %in% c("Deltoideus", "Triceps_Brachii") &
#   data$View == "Posterior"
# data_insert <- data[cond_insert, ]
# plot.new()
# plot(data_insert$x, data_insert$y, type = "n")
# text(data_insert$x, data_insert$y,
#      labels = rownames((data_insert)),
#      cex = .5)

data_insert <- data[c(2090:2053, 2042:2027), ]
data_insert$Side <- "Left"
data_insert$Group <- "Arm"
data_insert$Muscle <- "Triceps_Brachii"
data_insert$Id <- 48

data <- rbind(data[1:2044, ],
              data_insert,
              data[2096:nrow(data), ])
rownames(data) <- 1:nrow(data)



# Deltoids-Triceps Posterior Right Side Fusion
# cond_insert <- data$Muscle %in% c("Deltoideus", "Triceps_Brachii") &
#   data$View == "Posterior"
# data_insert <- data[cond_insert, ]
# plot.new()
# plot(data_insert$x, data_insert$y, type = "n")
# text(data_insert$x, data_insert$y,
#      labels = rownames((data_insert)),
#      cex = .5)

data_insert <- data[c(2149:2107, 1973:1986), ]
data_insert$Side <- "Right"
data_insert$Group <- "Arm"
data_insert$Muscle <- "Triceps_Brachii"
data_insert$Id <- 49

data <- rbind(data[1:2098, ],
              data_insert,
              data[2156:nrow(data), ])
rownames(data) <- 1:nrow(data)

# Triceps-Elbow Posterior Fusion ####
# cond_insert <- data$Muscle %in% c("Elbow", "Triceps_Brachii") &
#   data$View == "Posterior"
# data_insert <- data[cond_insert, ]
# plot.new()
# plot(data_insert$x, data_insert$y, type = "n")
# text(data_insert$x, data_insert$y,
#      labels = rownames((data_insert)),
#      cex = .5)

data_insert <- data[c(2045:2055, 2229:2235, 2198:2202, 2066:2098), ]
data_insert$Side <- "Left"
data_insert$Group <- "Arm"
data_insert$Muscle <- "Triceps_Brachii"
data_insert$Id <- 48

data <- rbind(data[1:2044, ],
              data_insert,
              data[2099:nrow(data), ])
rownames(data) <- 1:nrow(data)



# Triceps-Elbow Posterior Right Side Fusion
# cond_insert <- data$Muscle %in% c("Elbow", "Triceps_Brachii") &
#   data$View == "Posterior"
# data_insert <- data[cond_insert, ]
# plot.new()
# plot(data_insert$x, data_insert$y, type = "n")
# text(data_insert$x, data_insert$y,
#      labels = rownames((data_insert)),
#      cex = .5)

data_insert <- data[c(2101:2120, 2192:2199, 2158:2161, 2132:2157), ]
data_insert$Side <- "Right"
data_insert$Group <- "Arm"
data_insert$Muscle <- "Triceps_Brachii"
data_insert$Id <- 49

data <- rbind(data[1:2100, ],
              data_insert,
              data[2158:nrow(data), ])
rownames(data) <- 1:nrow(data)


# Elbow-Forearm Posterior Fusion ####
# cond_insert <- data$Muscle %in% c("Elbow", "Extensor_Digitorum") &
#   data$View == "Posterior"
# data_insert <- data[cond_insert, ]
# plot.new()
# plot(data_insert$x, data_insert$y, type = "n")
# text(data_insert$x, data_insert$y,
#      labels = rownames((data_insert)),
#      cex = .5)

data_insert <- data[c(2159:2173, 2300:2298, 2356:2352, 2182:2200), ]
data_insert$Side <- "Right"
data_insert$Group <- "Arm"
data_insert$Muscle <- "Elbow"
data_insert$Id <- 50

data <- rbind(data[1:2158, ],
              data_insert,
              data[2201:nrow(data), ])
rownames(data) <- 1:nrow(data)


# Elbow-Forearm Posterior Left Side Fusion
# cond_insert <- data$Muscle %in% c("Elbow", "Extensor_Digitorum") &
#   data$View == "Posterior"
# data_insert <- data[cond_insert, ]
# plot.new()
# plot(data_insert$x, data_insert$y, type = "n")
# text(data_insert$x, data_insert$y,
#      labels = rownames((data_insert)),
#      cex = .5)

data_insert <- data[c(2201:2214, 2244:2239, 2297:2295, 2222:2238), ]
data_insert$Side <- "Left"
data_insert$Group <- "Arm"
data_insert$Muscle <- "Elbow"
data_insert$Id <- 51

data <- rbind(data[1:2200, ],
              data_insert,
              data[2239:nrow(data), ])
rownames(data) <- 1:nrow(data)


# Forearm-Wrist Posterior Fusion ####
# cond_insert <- data$Muscle %in% c("Wrist", "Extensor_Digitorum") &
#   data$View == "Posterior"
# data_insert <- data[cond_insert, ]
# plot.new()
# plot(data_insert$x, data_insert$y, type = "n")
# text(data_insert$x, data_insert$y,
#      labels = rownames((data_insert)),
#      cex = .5)

data_insert <- data[c(2241:2281, 3115:3114, 3127:3122, 2284:2299), ]
data_insert$Side <- "Left"
data_insert$Group <- "Forearm"
data_insert$Muscle <- "Extensor_Digitorum"
data_insert$Id <- 52

data <- rbind(data[1:2240, ],
              data_insert,
              data[2300:nrow(data), ])
rownames(data) <- 1:nrow(data)





# Forearm-Wrist Posterior Right Side Fusion
# cond_insert <- data$Muscle %in% c("Wrist", "Extensor_Digitorum") &
#   data$View == "Posterior"
# data_insert <- data[cond_insert, ]
# plot.new()
# plot(data_insert$x, data_insert$y, type = "n")
# text(data_insert$x, data_insert$y,
#      labels = rownames((data_insert)),
#      cex = .5)

data_insert <- data[c(2306:2320, 3136:3137, 3147:3145, 2321:2364), ]
data_insert$Side <- "Right"
data_insert$Group <- "Forearm"
data_insert$Muscle <- "Extensor_Digitorum"
data_insert$Id <- 53

data <- rbind(data[1:2305, ],
              data_insert,
              data[2365:nrow(data), ])
rownames(data) <- 1:nrow(data)


# Wrist-Hand Posterior Fusion ####
# cond_insert <- data$Muscle %in% c("Wrist", "Hand") &
#   data$View == "Posterior" &
#   data$Side == "Right"
# data_insert <- data[cond_insert, ]
# plot.new()
# plot(data_insert$x, data_insert$y, type = "n")
# text(data_insert$x, data_insert$y,
#      labels = rownames((data_insert)),
#      cex = .5)

data_insert <- data[c(3155:3169, 3198:3215, 3217:3221, 3149), ]
data_insert$Side <- "Right"
data_insert$Group <- "Hand"
data_insert$Muscle <- "Hand"
data_insert$Id <- 72

data <- rbind(data[1:3153, ],
              data_insert,
              data[3222:nrow(data), ])
rownames(data) <- 1:nrow(data)


# Wrist-Hand Posterior Left Side Fusion
# cond_insert <- data$Muscle %in% c("Wrist", "Hand") &
#   data$View == "Posterior" &
#   data$Side == "Left"
# data_insert <- data[cond_insert, ]
# plot.new()
# plot(data_insert$x, data_insert$y, type = "n")
# text(data_insert$x, data_insert$y,
#      labels = rownames((data_insert)),
#      cex = .5)

data_insert <- data[c(3193, 3127, 3195:3200, 3208:3224, 3245:3264), ]
data_insert$Side <- "Left"
data_insert$Group <- "Hand"
data_insert$Muscle <- "Hand"
data_insert$Id <- 73

data <- rbind(data[1:3192, ],
              data_insert,
              data[3265:nrow(data), ])
rownames(data) <- 1:nrow(data)



# Trapezius_Neck Posterior Fusion ####
# cond_insert <- data$Muscle %in% c("Neck", "Upper_Trapezius") &
#   data$View == "Posterior"
# data_insert <- data[cond_insert, ]
# plot.new()
# plot(data_insert$x, data_insert$y, type = "n")
# text(data_insert$x, data_insert$y,
#      labels = rownames((data_insert)),
#      cex = .5)

data_insert <- data[c(1772:1774, 1757:1768, 1776, 1832, 1751:1756, 1739:1747), ]
data_insert$Side <- "Center"
data_insert$Group <- "Neck"
data_insert$Muscle <- "Neck"
data_insert$Id <- 40

data <- rbind(data[1:1738, ],
              data_insert,
              data[1775:nrow(data), ])
rownames(data) <- 1:nrow(data)



# Spine Creation####
# cond_insert <- data$Group %in% c("Lower_Back", "Upper_Back", "Groin", "Gluteus") &
#   data$View == "Posterior"
# data_insert <- data[cond_insert, ]
# plot.new()
# plot(data_insert$x, data_insert$y, type = "n",
#      xlim = c(-410, -400))
# text(data_insert$x, data_insert$y,
#      labels = rownames((data_insert)),
#      cex = .8)

data_insert <- data[c(1930, 1929, 2672, 2828, 2560, 2558, 1830, 1801), ]

data_insert$Part <- "Upper_Body"
data_insert$Side <- "Center"
data_insert$Group <- "Spine"
data_insert$Muscle <- "Spine"
data_insert$Id <- 75

data <- rbind(data,
              data_insert)
rownames(data) <- 1:nrow(data)


# Forearm Wrist Fusion ####
# cond_insert <- data$Group %in% c("Forearm") &
#   data$View == "Anterior" &
#   data$Side == "Right"
# data_insert <- data[cond_insert, ]
# plot.new()
# plot(data_insert$x, data_insert$y, type = "n")
# text(data_insert$x, data_insert$y,
#      labels = rownames((data_insert)),
#      cex = .8)

data_insert <- data[c(831:828, 1265:1271, 835:832), ]

data_insert$Muscle <- "Wrist"
data_insert$Id <- 26

data <- rbind(data[1:1258, ],
              data_insert,
              data[1272:nrow(data), ])
rownames(data) <- 1:nrow(data)




# Forearm Wrist Left Side Fusion
# cond_insert <- data$Group %in% c("Forearm") &
#   data$View == "Anterior" &
#   data$Side == "Left"
# data_insert <- data[cond_insert, ]
# plot.new()
# plot(data_insert$x, data_insert$y, type = "n")
# text(data_insert$x, data_insert$y,
#      labels = rownames((data_insert)),
#      cex = .8)

data_insert <- data[c(741, 1275:1282, 746:742), ]

data_insert$Muscle <- "Wrist"
data_insert$Id <- 27

data <- rbind(data[1:1273, ],
              data_insert,
              data[1287:nrow(data), ])
rownames(data) <- 1:nrow(data)



# Forearm Elbow Fusion ####
# cond_insert <- data$Muscle %in% c("Flexor_Digitorum", "Elbow") &
#   data$View == "Anterior" &
#   data$Side == "Right"
# data_insert <- data[cond_insert, ]
# plot.new()
# plot(data_insert$x, data_insert$y, type = "n")
# text(data_insert$x, data_insert$y,
#      labels = rownames((data_insert)),
#      cex = .8)

data_insert <- data[c(641:654, 792:786, 847:846, 663:684), ]

data_insert$Muscle <- "Elbow"
data_insert$Group <- "Arm"
data_insert$Id <- 14

data <- rbind(data[1:640, ],
              data_insert,
              data[685:nrow(data), ])
rownames(data) <- 1:nrow(data)


# Forearm Elbow Left Side Fusion
# cond_insert <- data$Muscle %in% c("Flexor_Digitorum", "Elbow") &
#   data$View == "Anterior" &
#   data$Side == "Left"
# data_insert <- data[cond_insert, ]
# plot.new()
# plot(data_insert$x, data_insert$y, type = "n")
# text(data_insert$x, data_insert$y,
#      labels = rownames((data_insert)),
#      cex = .8)

data_insert <- data[c(686:698, 729:726, 786:781, 707:725), ]

data_insert$Muscle <- "Elbow"
data_insert$Group <- "Arm"
data_insert$Id <- 15

data <- rbind(data[1:685, ],
              data_insert,
              data[726:nrow(data), ])
rownames(data) <- 1:nrow(data)




# Biceps Fusion ####
# cond_insert <- data$Group %in% c("Arm") &
#   data$View == "Anterior" &
#   data$Side == "Right"
# data_insert <- data[cond_insert, ]
# plot.new()
# plot(data_insert$x, data_insert$y, type = "n")
# text(data_insert$x, data_insert$y,
#      labels = rownames((data_insert)),
#      cex = .8)

data_insert <- data[c(290:274, 587:605, 644:641, 685:676, 616:628), ]

data_insert$Muscle <- "Biceps_Brachii"
data_insert$Group <- "Arm"
data_insert$Id <- 13

data <- rbind(data[1:582, ],
              data_insert,
              data[641:nrow(data), ])
rownames(data) <- 1:nrow(data)




# Biceps Left Side Fusion
# cond_insert <- data$Group %in% c("Arm") &
#   data$View == "Anterior" &
#   data$Side == "Left"
# data_insert <- data[cond_insert, ]
# plot.new()
# plot(data_insert$x, data_insert$y, type = "n")
# text(data_insert$x, data_insert$y,
#      labels = rownames((data_insert)),
#      cex = .8)

data_insert <- data[c(3286:3270, 377:388, 694:692, 732:725, 399:418), ]

data_insert$Muscle <- "Biceps_Brachii"
data_insert$Group <- "Arm"
data_insert$Id <- 10

data <- rbind(data[1:369, ],
              data_insert,
              data[429:nrow(data), ])
rownames(data) <- 1:nrow(data)




# Deltoideus-Chest Fusion ####
# cond_insert <- data$Muscle %in% c("Deltoideus", "Pec_Major") &
#   data$View == "Anterior" &
#   data$Side == "Left"
# data_insert <- data[cond_insert, ]
# plot.new()
# plot(data_insert$x, data_insert$y, type = "n")
# text(data_insert$x, data_insert$y,
#      labels = rownames((data_insert)),
#      cex = .8)

data_insert <- data[c(3288:3298, 3246:3287, 347:346), ]

data_insert$Muscle <- "Deltoideus"
data_insert$Group <- "Arm"
data_insert$Id <- 74

data <- rbind(data[1:3245, ],
              data_insert,
              data[3299:nrow(data), ])
rownames(data) <- 1:nrow(data)




# Deltoideus-Chest Right Side Fusion
# cond_insert <- data$Muscle %in% c("Deltoideus", "Pec_Major") &
#   data$View == "Anterior" &
#   data$Side == "Right"
# data_insert <- data[cond_insert, ]
# plot.new()
# plot(data_insert$x, data_insert$y, type = "n")
# text(data_insert$x, data_insert$y,
#      labels = rownames((data_insert)),
#      cex = .8)

data_insert <- data[c(345:340, 274:319, 268:273), ]

data_insert$Muscle <- "Deltoideus"
data_insert$Group <- "Arm"
data_insert$Id <- 7

data <- rbind(data[1:267, ],
              data_insert,
              data[320:nrow(data), ])
rownames(data) <- 1:nrow(data)




# Deltoideus-Chest  Fusion
# cond_insert <- data$Muscle %in% c("Head", "Neck") &
#   data$View == "Posterior"
# data_insert <- data[cond_insert, ]
# plot.new()
# plot(data_insert$x, data_insert$y, type = "n")
# text(data_insert$x, data_insert$y,
#      labels = rownames((data_insert)),
#      cex = .8)

data_insert <- data[c(1695:1699, 1640:1685, 1768:1757, 1788:1777, 1716:1756, 1700:1705), ]

data_insert$Muscle <- "Head"
data_insert$Group <- "Head"
data_insert$Side <- "Center"
data_insert$Id <- 38

data <- rbind(data[1:1639, ],
              data_insert,
              data[1757:nrow(data), ])
rownames(data) <- 1:nrow(data)




# Chest-Abdomen Right Side Fusion
# cond_insert <- data$Group %in% c("Abdominals", "Pectoral") &
#   data$View == "Anterior" &
#   data$Side == "Right"
# data_insert <- data[cond_insert, ]
# plot.new()
# plot(data_insert$x, data_insert$y, type = "n")
# text(data_insert$x, data_insert$y,
#      labels = rownames((data_insert)),
#      cex = .8)

data_insert <- data[c(326:327, 532:515, 588:589, 346:351), ]

data_insert$Muscle <- "Pec_Major"
data_insert$Group <- "Pectoral"
data_insert$Side <- "Right"
data_insert$Id <- 8

data <- rbind(data[1:325, ],
              data_insert,
              data[352:nrow(data), ])
rownames(data) <- 1:nrow(data)




# Chest-Abdomen  Fusion ####
# cond_insert <- data$Group %in% c("Abdominals", "Pectoral") &
#   data$View == "Anterior" &
#   data$Side == "Left"
# data_insert <- data[cond_insert, ]
# plot.new()
# plot(data_insert$x, data_insert$y, type = "n")
# text(data_insert$x, data_insert$y,
#      labels = rownames((data_insert)),
#      cex = .8)

data_insert <- data[c(354:355, 438:440, 516:499, 376:377), ]

data_insert$Muscle <- "Pec_Major"
data_insert$Group <- "Pectoral"
data_insert$Side <- "Left"
data_insert$Id <- 9

data <- rbind(data[1:353, ],
              data_insert,
              data[378:nrow(data), ])
rownames(data) <- 1:nrow(data)




# Abdomen-Hip Right Side Fusion
# cond_insert <- data$Muscle %in% c("Rectus_Abdominis", "Hip") &
#   data$View == "Anterior" &
#   data$Side == "Right"
# data_insert <- data[cond_insert, ]
# plot.new()
# plot(data_insert$x, data_insert$y, type = "n")
# text(data_insert$x, data_insert$y,
#      labels = rownames((data_insert)),
#      cex = .8)

data_insert <- data[c(518:537, 873:868, 949:922, 568:592), ]

data_insert$Muscle <- "Rectus_Abdominis"
data_insert$Group <- "Abdominals"
data_insert$Side <- "Right"
data_insert$Id <- 12

data <- rbind(data[1:517, ],
              data_insert,
              data[593:nrow(data), ])
rownames(data) <- 1:nrow(data)




# Abdomen-Hip Fusion ####
# cond_insert <- data$Muscle %in% c("Rectus_Abdominis", "Hip") &
#   data$View == "Anterior" &
#   data$Side == "Left"
# data_insert <- data[cond_insert, ]
# plot.new()
# plot(data_insert$x, data_insert$y, type = "n")
# text(data_insert$x, data_insert$y,
#      labels = rownames((data_insert)),
#      cex = .8)

data_insert <- data[c(439:464, 969:954, 1039:1019, 498:517), ]

data_insert$Muscle <- "Rectus_Abdominis"
data_insert$Group <- "Abdominals"
data_insert$Side <- "Left"
data_insert$Id <- 11

data <- rbind(data[1:438, ],
              data_insert,
              data[518:nrow(data), ])
rownames(data) <- 1:nrow(data)




# Abdomen Sides Fusion ####
# cond_insert <- data$Muscle %in% c("Rectus_Abdominis") &
#   data$View == "Anterior"
# data_insert <- data[cond_insert, ]
# plot.new()
# plot(data_insert$x, data_insert$y, type = "n")
# text(data_insert$x, data_insert$y,
#      labels = rownames((data_insert)),
#      cex = .8)

data_insert <- data[c(504:521, 439:501, 542:600, 522:539), ]

data_insert$Muscle <- "Rectus_Abdominis"
data_insert$Group <- "Abdominals"
data_insert$Side <- "Center"
data_insert$Id <- 11

data <- rbind(data[1:438, ],
              data_insert,
              data[601:nrow(data), ])
rownames(data) <- 1:nrow(data)


# Hip Sides Fusion ####
# cond_insert <- data$Group %in% c("Hip", "Groin") &
#   data$View == "Anterior"
# data_insert <- data[cond_insert, ]
# plot.new()
# plot(data_insert$x, data_insert$y, type = "n")
# text(data_insert$x, data_insert$y,
#      labels = rownames((data_insert)),
#      cex = .8)

data_insert <- data[c(1019:1039, 954:1016, 1132, 1187:1185, 879:953, 871:877), ]

data_insert$Muscle <- "Hip"
data_insert$Group <- "Hip"
data_insert$Side <- "Center"
data_insert$Id <- 18

data <- rbind(data[1:870, ],
              data_insert,
              data[1040:nrow(data), ])
rownames(data) <- 1:nrow(data)


# Hip Fusion ####
# cond_insert <- data$Muscle %in% c("Hip", "Hip_External", "Adductor_Longus", "Quadriceps") &
#   data$View == "Anterior"
# data_insert <- data[cond_insert, ]
# plot.new()
# plot(data_insert$x, data_insert$y, type = "n",
#      xlim = c(-100, -60))
# text(data_insert$x, data_insert$y,
#      labels = rownames((data_insert)),
#      cex = .8)

data_insert <- data[c(1240, 1251:1262, 1001:958, 1189), ]

data_insert$Muscle <- "Hip_External"
data_insert$Group <- "Hip"
data_insert$Side <- "Right"
data_insert$Id <- 25

data <- rbind(data[1:1240, ],
              data_insert,
              data[1282:nrow(data), ])
rownames(data) <- 1:nrow(data)


# Hip Left Side Fusion
# cond_insert <- data$Muscle %in% c("Hip", "Hip_External", "Adductor_Longus", "Quadriceps") &
#   data$View == "Anterior"
# data_insert <- data[cond_insert, ]
# plot.new()
# plot(data_insert$x, data_insert$y, type = "n",
#      xlim = c(-180, -140))
# text(data_insert$x, data_insert$y,
#      labels = rownames((data_insert)),
#      cex = .8)

data_insert <- data[c(1046:1055, 1076, 1132, 1134, 953:911), ]

data_insert$Muscle <- "Hip_External"
data_insert$Group <- "Hip"
data_insert$Side <- "Left"
data_insert$Id <- 20

data <- rbind(data[1:1040, ],
              data_insert,
              data[1076:nrow(data), ])
rownames(data) <- 1:nrow(data)



# Clavicles Fusion ####
# cond_insert <- data$Muscle %in% c("Clavicle", "Pec_Major", "Neck") &
#   data$View == "Anterior"
# data_insert <- data[cond_insert, ]
# plot.new()
# plot(data_insert$x, data_insert$y, type = "n")
# text(data_insert$x, data_insert$y,
#      labels = rownames((data_insert)),
#      cex = .8)

data_insert <- data[c(210:230, 198:209, 354, 378, 326, 353, 253:267, 231:252), ]

data_insert$Muscle <- "Clavicle"
data_insert$Group <- "Neck"
data_insert$Side <- "Center"
data_insert$Id <- 5

data <- rbind(data[1:197, ],
              data_insert,
              data[268:nrow(data), ])
rownames(data) <- 1:nrow(data)



# Neck Fusion ####
# cond_insert <- data$Group %in% c("Clavicle", "Head", "Neck") &
#   data$View == "Anterior"
# data_insert <- data[cond_insert, ]
# plot.new()
# plot(data_insert$x, data_insert$y, type = "n",
#      xlim = c(-140, -100))
# text(data_insert$x, data_insert$y,
#      labels = rownames((data_insert)),
#      cex = .8)

data_insert <- data[c(56:43, 132:136, 213:198, 232:233, 271:258, 188, 190, 79:65), ]

data_insert$Muscle <- "Clavicle"
data_insert$Group <- "Neck"
data_insert$Side <- "Center"
data_insert$Id <- 3

data <- rbind(data[1:129, ],
              data_insert,
              data[198:nrow(data), ])
rownames(data) <- 1:nrow(data)



# Head Fusion ####
# cond_insert <- data$Group %in% c("Neck", "Head") &
#   data$View == "Anterior"
# data_insert <- data[cond_insert, ]
# plot.new()
# plot(data_insert$x, data_insert$y, type = "n",
#      xlim = c(-140, -100))
# text(data_insert$x, data_insert$y,
#      labels = rownames((data_insert)),
#      cex = .8)

data_insert <- data[c(57:62, 1:56, 65:129,63:64), ]

data_insert$Muscle <- "Head"
data_insert$Group <- "Head"
data_insert$Side <- "Center"
data_insert$Id <- 1

data <- rbind(data_insert,
              data[130:nrow(data), ])
rownames(data) <- 1:nrow(data)



# Chest Fusion ####
# cond_insert <- data$Muscle %in% c("Pec_Major") &
#   data$View == "Anterior" &
#   data$Side == "Right"
# data_insert <- data[cond_insert, ]
# plot.new()
# plot(data_insert$x, data_insert$y, type = "n")
# text(data_insert$x, data_insert$y,
#      labels = rownames((data_insert)),
#      cex = .8)

data[380:382, "x"] <- -120
data[330:332, "x"] <- -120






# Biceps-Chest Fusion ####
# cond_insert <- data$Group %in% c("Pectoral", "Arm") &
#   data$View == "Anterior" &
#   data$Side == "Right"
# data_insert <- data[cond_insert, ]
# plot.new()
# plot(data_insert$x, data_insert$y, type = "n")
# text(data_insert$x, data_insert$y,
#      labels = rownames((data_insert)),
#      cex = .8)

data_insert <- data[c(617:601, 663:626, 350:354), ]

data_insert$Muscle <- "Biceps_Brachii"
data_insert$Group <- "Arm"
data_insert$Side <- "Right"
data_insert$Id <- 13

data <- rbind(data[1:600, ],
              data_insert,
              data[664:nrow(data), ])
rownames(data) <- 1:nrow(data)






# Biceps-Chest Left Side Fusion
# cond_insert <- data$Group %in% c("Pectoral", "Arm") &
#   data$View == "Anterior" &
#   data$Side == "Left"
# data_insert <- data[cond_insert, ]
# plot.new()
# plot(data_insert$x, data_insert$y, type = "n")
# text(data_insert$x, data_insert$y,
#      labels = rownames((data_insert)),
#      cex = .8)

data_insert <- data[c(383:436, 362:358), ]

data_insert$Muscle <- "Biceps_Brachii"
data_insert$Group <- "Arm"
data_insert$Side <- "Right"
data_insert$Id <- 10

data <- rbind(data[1:382, ],
              data_insert,
              data[443:nrow(data), ])
rownames(data) <- 1:nrow(data)




# Ears Creation####
# cond_insert <- data$Group %in% c("Head") &
#   data$View == "Posterior"
# data_insert <- data[cond_insert, ]
# plot.new()
# plot(data_insert$x, data_insert$y, type = "n")
# text(data_insert$x, data_insert$y,
#      labels = rownames((data_insert)),
#      cex = .8)

data_insert1 <- data[1713:1728, ]
data_insert1$Part <- "Upper_Body"
data_insert1$View <- "Posterior"
data_insert1$Side <- "Right"
data_insert1$Group <- "Head"
data_insert1$Muscle <- "Ear"
data_insert1$Id <- 76

data_insert2 <- data[1770:1783, ]
data_insert2$Part <- "Upper_Body"
data_insert2$View <- "Posterior"
data_insert2$Side <- "Left"
data_insert2$Group <- "Head"
data_insert2$Muscle <- "Ear"
data_insert2$Id <- 77

data_insert3 <- data[c(1686:1713, 1728:1770, 1783:1807), ]

data <- rbind(data[1:1685,],
              data_insert1,
              data_insert2,
              data_insert3,
              data[1808:nrow(data), ])
rownames(data) <- 1:nrow(data)




# cond_insert <- data$Group %in% c("Head") &
#   data$View == "Anterior"
# data_insert <- data[cond_insert, ]
# plot.new()
# plot(data_insert$x, data_insert$y, type = "n")
# text(data_insert$x, data_insert$y,
#      labels = rownames((data_insert)),
#      cex = .8)

data_insert1 <- data[87:106, ]
data_insert1$Part <- "Upper_Body"
data_insert1$View <- "Anterior"
data_insert1$Side <- "Right"
data_insert1$Group <- "Head"
data_insert1$Muscle <- "Ear"
data_insert1$Id <- 78

data_insert2 <- data[23:38, ]
data_insert2$Part <- "Upper_Body"
data_insert2$View <- "Anterior"
data_insert2$Side <- "Left"
data_insert2$Group <- "Head"
data_insert2$Muscle <- "Ear"
data_insert2$Id <- 79

data_insert3 <- data[c(1:23, 38:87, 106:129), ]

data <- rbind(data_insert1,
              data_insert2,
              data_insert3,
              data[130:nrow(data), ])
rownames(data) <- 1:nrow(data)



# Mouth Creation ####
# cond_insert <- data$Group %in% c("Head") &
#   data$View == "Anterior"
# data_insert <- data[cond_insert, ]
# plot.new()
# plot(data_insert$x, data_insert$y, type = "n")
# text(data_insert$x, data_insert$y,
#      labels = rownames((data_insert)),
#      cex = .8)

data_insert <- data[76:91, ]
data_insert$y <- data_insert$y + 10
# data_insert2 <- data[c(40, 85), ]
# data_insert2$y[1] <- data_insert2$y + 10

data_insert$Part <- "Upper_Body"
data_insert$View <- "Anterior"
data_insert$Side <- "Center"
data_insert$Group <- "Head"
data_insert$Muscle <- "Mouth"
data_insert$Id <- 80

data <- rbind(data,
              data_insert)
rownames(data) <- 1:nrow(data)



# Eyes Creation ####
# cond_insert <- data$Group %in% c("Head") &
#   data$View == "Anterior"
# data_insert <- data[cond_insert, ]
# plot.new()
# plot(data_insert$x, data_insert$y, type = "n")
# text(data_insert$x, data_insert$y,
#      labels = rownames((data_insert)),
#      cex = .8)

data_insert1 <- data.frame(
  Id = 81,
  View = "Anterior",
  Part = "Upper_Body",
  Group = "Head",
  Muscle = "Eye",
  Side = "Right",
  x = c(-105, -107, -109, -111, -113, -115, -113, -111, -109, -107),
  y = c(-55, -57, -59, -59, -57, -55, -53, -51, -51, -53)
)

data_insert2 <- data.frame(
  Id = 82,
  View = "Anterior",
  Part = "Upper_Body",
  Group = "Head",
  Muscle = "Eye",
  Side = "Left",
  x = c(-105, -107, -109, -111, -113, -115, -113, -111, -109, -107) - 20,
  y = c(-55, -57, -59, -59, -57, -55, -53, -51, -51, -53)
)

data <- rbind(data,
              data_insert1,
              data_insert2)
rownames(data) <- 1:nrow(data)



# Ankle and Feet Fix ####
# cond_insert <- data$Group %in% c("Foot") &
#   data$Side == "Right" &
#   data$View == "Posterior"
# data_insert <- data[cond_insert, ]
# plot.new()
# plot(data_insert$x, data_insert$y, type = "n")
# text(data_insert$x, data_insert$y,
#      labels = rownames((data_insert)),
#      cex = .8)


data <- data[-1645, ]
rownames(data) <- 1:nrow(data)
data <- data[-1635, ]
rownames(data) <- 1:nrow(data)
data <- data[-1654, ]
rownames(data) <- 1:nrow(data)
data <- data[-1643, ]
rownames(data) <- 1:nrow(data)


data[1653, "x"] <- -160
data[1685, "x"] <- -79
data[1667, "x"] <- -141
data[1669, "x"] <- -97
data[1668, "x"] <- -97
data[1651, "x"] <- -145
data[1652, "x"] <- -145
data[1635, "x"] <- -94
data[1636, "x"] <- -94


data[3152, "x"] <- -443
data[3173, "x"] <- -428
data[3174, "x"] <- -383
data[3193, "x"] <- -367


data <- data[-c(3175, 3176), ]
rownames(data) <- 1:nrow(data)
data <- data[-c(3189, 3190), ]
rownames(data) <- 1:nrow(data)
data <- data[-c(3156, 3157, 3160), ]
rownames(data) <- 1:nrow(data)



# Biceps-Chest Fusion ####
# cond_insert <- data$Group %in% c("Pectoral", "Arm") &
#   data$View == "Posterior" &
#   data$Side == "Right"
# data_insert <- data[cond_insert, ]
# plot.new()
# plot(data_insert$x, data_insert$y, type = "n")
# text(data_insert$x, data_insert$y,
#      labels = rownames((data_insert)),
#      cex = .8)

data_insert <- data[c(2058:2059, 2182:2227), ]

data_insert$Muscle <- "Triceps_Brachii"
data_insert$Group <- "Arm"
data_insert$Side <- "Right"
data_insert$Id <- 49

data <- rbind(data[1:2169, ],
              data_insert,
              data[2228:nrow(data), ])
rownames(data) <- 1:nrow(data)



cond_insert <- data$Group %in% c("Pectoral", "Arm") &
  data$Muscle == "Triceps_Brachii" &
  data$View == "Posterior" &
  data$Side == "Left"
data_insert <- data[cond_insert, ]
plot.new()
plot(data_insert$x, data_insert$y, type = "n")
text(data_insert$x, data_insert$y,
     labels = rownames((data_insert)),
     cex = .8)

data_insert <- data[c(2112, 2145:2114, 2169:2154), ]

data_insert$Muscle <- "Triceps_Brachii"
data_insert$Group <- "Arm"
data_insert$Side <- "Left"
data_insert$Id <- 48

data <- rbind(data[1:2113, ],
              data_insert,
              data[2170:nrow(data), ])
rownames(data) <- 1:nrow(data)

ggplot(data = data, aes(-x, -y, group = Id)) + geom_bdgramr()


