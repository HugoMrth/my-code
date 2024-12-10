datam <- dataf %>%
  dplyr::select(EXPO, atc7, etime, event) %>%
  group_by(EXPO, atc7) %>%
  mutate(freq = n()) %>%
  ungroup() %>%
  slice_sample(n = 10000, weight_by = freq, replace = FALSE) 
