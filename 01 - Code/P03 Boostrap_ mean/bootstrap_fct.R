
# Load packages -----------------------------------------------------------

library(tidyverse)
library(MDBS)
library(boot)
library(DescTools)
library(lubridate)

# file_conn <- file("tmp/descriptif2_from_package.R")
# dump("descriptif2","tmp/descriptif2_from_package.R")
# close(file_conn)

source("tmp/descriptif2.R")

iris <- iris %>% tibble

a7 <- iris %>% 
  mutate(Species = as.character(Species)) %>% 
  descriptif2(., factor = "Species", conf.method.cat = "boot", conf.method.num = "boot")



# Tests -------------------------------------------------------------------

# univariate
a0 <- iris %>% 
  mutate(Species = as.character(Species)) %>% 
  descriptif2(.)

a1 <- iris %>% 
  mutate(Species = as.character(Species)) %>% 
  descriptif2(.,conf.method.cat = "boot",conf.method.num = "boot")

a2 <- iris %>% 
  mutate(Species = as.character(Species)) %>% 
  descriptif2(.,conf.method.cat = "boot",conf.method.num = "boot",num.type="med")

a3 <- iris %>% 
  mutate(Species = as.character(Species)) %>% 
  descriptif2(.,conf.method.cat = "boot",conf.method.num = "boot",num.type="both")

# Retro compatibility

a4 <- iris %>% 
  mutate(Species = as.character(Species)) %>% 
  descriptif2(., conf.method = "boot")

a5 <- iris %>% 
  mutate(Species = as.character(Species)) %>% 
  descriptif2(., conf.method = "waldcc")

# Bivariate

a6 <- iris %>% 
  mutate(Species = as.character(Species)) %>% 
  descriptif2(., factor = "Species")

a7 <- iris %>% 
  mutate(Species = as.character(Species)) %>% 
  descriptif2(., factor = "Species", conf.method.cat = "boot", conf.method.num = "boot")

a8 <- iris %>% 
  mutate(Species = as.character(Species)) %>% 
  descriptif2(.,factor = "Species",conf.method.cat = "boot",conf.method.num = "boot",num.type="med")

a9 <- iris %>% 
  mutate(Species = as.character(Species)) %>% 
  descriptif2(.,factor = "Species",conf.method.cat = "boot",conf.method.num = "boot",num.type="both")


# Retro compatibility

a10 <- iris %>% 
  mutate(Species = as.character(Species)) %>% 
  descriptif2(.,factor = "Species",conf.method = "boot")


a11 <- iris %>% 
  mutate(Species = as.character(Species)) %>% 
  descriptif2(.,factor = "Species",conf.method = "waldcc")


# END

