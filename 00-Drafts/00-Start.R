library(tidyverse)
library(rio)

# Load the data
data <- import("01-Data/replication_ds.dta")

# Check the dimensions of the data
dim(data)

# Check the structure of the data
view(data)

# Check the statistics of each variable
# D38 that end with W9 refer to data in 2016, D38 that end with post refer to data in 2020
summary(select(data, D38_01_W9, D38_02_W9, D38_03_W9, D38_04_W9,
               D38_post_01, D38_post_02, D38_post_03, D38_post_04,
               SEX, ANNO, AMP, ZONA, scolarita,
               S21_1, S21_2, S21_3, S21_4))

# Data management: variables recode and creation
data <- data %>%
  mutate(across(c(D38_01_W9:D38_04_W9), ~ ifelse(. == 12, NA, .))) %>%
  mutate(moon1 = D38_01_W9 - 1,
         vacc1 = D38_02_W9 - 1,
         stam1 = D38_03_W9 - 1,
         chem1 = D38_04_W9 - 1
  ) %>%
  mutate(consp1 = rowMeans(select(., moon1, vacc1, stam1, chem1), na.rm = TRUE)) %>%
  mutate(moon2 = D38_post_01 - 1,
         vacc2 = D38_post_02 - 1,
         stam2 = D38_post_03 - 1,
         chem2 = D38_post_04 - 1
  ) %>%
  mutate(consp2 = rowMeans(select(., moon2, vacc2, stam2, chem2), na.rm = TRUE)) %>%
  mutate(across(c(moon1, vacc1, stam1, chem1, moon2, vacc2, stam2, chem2),
                list(b = ~ ifelse(. <= 5.9999, 0, ifelse(. >= 6, 1, NA)),
                     c = ~ ifelse(. == 0, 0, ifelse(. <= 5, 1, ifelse(. >= 6, 2, NA)))))) %>%
  mutate(diff_moon = moon2 - moon1,
         diff_vacc = vacc2 - vacc1,
         diff_stam = stam2 - stam1,
         diff_chem = chem2 - chem1)
