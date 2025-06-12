library(tidyverse)
library(rio)

# Load the data
df <- import("01-Data/replication_ds.dta")

# Check the dimensions of the data
dim(df)

# Check the structure of the data
view(df)

# Check the statistics of each variable
# D38 that end with W9 refer to data in 2016, D38 that end with post refer to data in 2020
summary(select(df, D38_01_W9, D38_02_W9, D38_03_W9, D38_04_W9,
               D38_post_01, D38_post_02, D38_post_03, D38_post_04,
               SEX, ANNO, AMP, ZONA, scolarita,
               S21_1, S21_2, S21_3, S21_4))

# Data management: variables recode and creation
df <- df %>%
  mutate(across(c(D38_01_W9, D38_02_W9, D38_03_W9, D38_04_W9), ~ ifelse(. == 12, NA, .)))%>%
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

# Control variables
df <- df %>%
  mutate(anno2 = suppressWarnings(as.numeric(as.character(ANNO))),
         age = 2020 - anno2,
         titstu = case_when(
           scolarita %in% c(1, 2) ~ "Low",
           scolarita %in% c(3, 4, 5) ~ "Medium",
           scolarita %in% 6:11 ~ "High"
         ),
         sindes = case_when(
           D4_post %in% c(1, 2) ~ "Sx",
           D4_post %in% c(3, 4, 5) ~ "Csx",
           D4_post == 6 ~ "C",
           D4_post %in% c(7, 8, 9) ~ "Cdx",
           D4_post %in% c(10, 11) ~ "Dx",
           D4_post == 13 ~ "NC"
         )) %>%
  mutate(
    stealth1 = ifelse(S21_1 == 12, NA, S21_1),
    stealth2 = ifelse(S21_2 == 12, NA, S21_2),
    stealth3 = ifelse(S21_3 == 12, NA, S21_3),
    stealth4 = ifelse(S21_4 == 12, NA, S21_4),
    stealth = rowMeans(across(c(stealth1, stealth2, stealth3, stealth4)), na.rm = TRUE)
  ) %>%
  rename(gender = SEX, zgp5 = ZONA)

# Regression tables
model_moon <- lm(diff_moon ~ factor(gender) + age + factor(titstu) + stealth + factor(sindes), data = df, subset = !is.na(consp1) & !is.na(consp2))
summary(model_moon)


model_vacc <- lm(diff_vacc ~ factor(gender) + age + factor(titstu) + stealth + factor(sindes), data = df, subset = !is.na(consp1) & !is.na(consp2))
summary(model_vacc)

model_stam <- lm(diff_stam ~ factor(gender) + age + factor(titstu) + stealth + factor(sindes), data = df, subset = !is.na(consp1) & !is.na(consp2))
summary(model_stam)

model_chem <- lm(diff_chem ~ factor(gender) + age + factor(titstu) + stealth + factor(sindes), data = df, subset = !is.na(consp1) & !is.na(consp2))
summary(model_chem)


# Margins (TBD) - table 2

# Table A1
# Group 1: pre-beliefs
summary(select(df, moon1, vacc1, stam1, chem1))

# Group 2: post-beliefs
summary(select(df, moon2, vacc2, stam2, chem2))

# Group 3: control variables
summary(select(df, gender, age, titstu, stealth, sindes))

#Table A2

# Table A3
print_joint_frequencies <- function(df, var_pre, var_post) {
  tab <- table(df[[var_pre]], df[[var_post]])
  cat("\nJoint frequency:", var_pre, "vs", var_post, "\n")
  print(tab)
  cat("\nPercentage (of the total):\n")
  print(round(prop.table(tab) * 100, 2))
}

# Exdcuting the function for each conspiracy theory
print_joint_frequencies(df, "moon1_c", "moon2_c")
print_joint_frequencies(df, "vacc1_c", "vacc2_c")
print_joint_frequencies(df, "stam1_c", "stam2_c")
print_joint_frequencies(df, "chem1_c", "chem2_c")

# Figure 1


