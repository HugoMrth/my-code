rm(list = ls())

#### Librarires ####
library(haven)
library(sas7bdat)
library(dplyr)
library(stringr)
library(purrr)
library(lubridate)
library(tidyr)




#### Tables ####
BDD_EXPOSES <- read_sas('/sasdata/prd/users/44a001348510899/t_exposes_c03ea04_v1.sas7bdat')
BDD_TEMOINS <- read_sas('/sasdata/prd/users/44a001348510899/t_temoins_c03ea04_v1.sas7bdat')


write.csv2(descriptif_all, file = "~/Citrix_documents/EXPORT/descriptif_export.csv",
           row.names = FALSE)
