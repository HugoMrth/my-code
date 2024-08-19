---
title: "Testing gtsummary table crossref in Word output"
format: docx
execute:
  echo: false
  warning: false
output: word_document
---
 
```{r}
#| label: load-packages
#| include: false
 
library(tidyverse)
library(gtsummary)
library(flextable)
```
 
## Results
 
@tbl-mtcars-summary summarises the cars in the mtcars dataset by transmission.
 
```{r}
#| label: tbl-mtcars-summary
#| tbl-cap: Summary of cars in mtcars by transmission.
 
mtcarsSummaryTbl <- mtcars %>% 
  tbl_summary(by = am) %>% 
  modify_header(
    stat_1 = "**Automatic**, N = {n}",
    stat_2 = "**Manual**, N = {n}",
  ) %>% 
  add_overall() %>% 
  bold_labels()
 
mtcarsSummaryTbl
```
