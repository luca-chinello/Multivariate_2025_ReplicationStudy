library(tidyverse)
library(rio)

# Load the data
df <- import("01-Data/replication_ds.dta")

export(df, "01-Data/replication_ds.csv")


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
         vacc1 = D38_02_W9 - 1,             # Renaming the variables for the pre-beliefs and changing the scale
         stam1 = D38_03_W9 - 1,             # from 1-12 to 0-11 (11 = NA)            
         chem1 = D38_04_W9 - 1
  ) %>%
  mutate(consp1 = rowMeans(select(., moon1, vacc1, stam1, chem1), na.rm = TRUE)) %>%   # Calculating the mean for each variable
  mutate(moon2 = D38_post_01 - 1,
         vacc2 = D38_post_02 - 1,           # Renaming the variables for the post-beliefs and changing the scale
         stam2 = D38_post_03 - 1,           # from 1-12 to 0-11 (11 = NA)
         chem2 = D38_post_04 - 1
  ) %>%
  mutate(consp2 = rowMeans(select(., moon2, vacc2, stam2, chem2), na.rm = TRUE)) %>%   # Calculating the mean for each variable
  mutate(across(c(moon1, vacc1, stam1, chem1, moon2, vacc2, stam2, chem2),    #creating new columns for recoding purposes
                list(b = ~ ifelse(. <= 5.9999, 0, ifelse(. >= 6, 1, NA)),
                     c = ~ ifelse(. == 0, 0, ifelse(. <= 5, 1, ifelse(. >= 6, 2, NA)))))) %>%
  mutate(diff_moon = moon2 - moon1,
         diff_vacc = vacc2 - vacc1,         # Calculating the difference between pre and post beliefs
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

# Check the reliability of stealth variables
library(psych)
psych::alpha(df[, c("stealth1", "stealth2", "stealth3", "stealth4")])

# Regression tables
model_moon <- lm(diff_moon ~ factor(gender) + age + factor(titstu) + stealth + factor(sindes), data = df, subset = !is.na(consp1) & !is.na(consp2))
summary(model_moon)

model_vacc <- lm(diff_vacc ~ factor(gender) + age + factor(titstu) + stealth + factor(sindes), data = df, subset = !is.na(consp1) & !is.na(consp2))
summary(model_vacc)

model_stam <- lm(diff_stam ~ factor(gender) + age + factor(titstu) + stealth + factor(sindes), data = df, subset = !is.na(consp1) & !is.na(consp2))
summary(model_stam)

model_chem <- lm(diff_chem ~ factor(gender) + age + factor(titstu) + stealth + factor(sindes), data = df, subset = !is.na(consp1) & !is.na(consp2))
summary(model_chem)

library(stargazer)
stargazer(model_moon, model_vacc, model_stam, model_chem, type = "text")

# Margins - Table 2
library(margins)
margin_moon <- margins(model_moon)
summary(margin_moon)

margin_vacc <- margins(model_vacc)
summary(margin_vacc)

margin_stam <- margins(model_stam)
summary(margin_stam)

margin_chem <- margins(model_chem)
summary(margin_chem)







library(emmeans)

df <- df %>%
  mutate(across(c(gender, sindes, titstu), as.factor))


# Margini per model_moon
emmeans(model_moon, ~ gender, at = list(gender = c(1, 2)))
emmeans(model_moon, ~ age, at = list(age = c(25, 65)))
emmeans(model_moon, ~ stealth, at = list(stealth = c(3, 9)))
emmeans(model_moon, ~ sindes, at = list(sindes = c("Sx", "Dx")))

# Margini per model_vacc
emmeans(model_vacc, ~ gender, at = list(gender = c(1, 2)))
emmeans(model_vacc, ~ age, at = list(age = c(25, 65)))
emmeans(model_vacc, ~ stealth, at = list(stealth = c(3, 9)))
emmeans(model_vacc, ~ sindes, at = list(sindes = c("Sx", "Dx")))

# Margini per model_stam
emmeans(model_stam, ~ gender, at = list(gender = c(1, 2)))
emmeans(model_stam, ~ age, at = list(age = c(25, 65)))
emmeans(model_stam, ~ stealth, at = list(stealth = c(3, 9)))
emmeans(model_stam, ~ sindes, at = list(sindes = c("Sx", "Dx")))

# Margini per model_chem
emmeans(model_chem, ~ gender, at = list(gender = c(1, 2)))
emmeans(model_chem, ~ age, at = list(age = c(25, 65)))
emmeans(model_chem, ~ stealth, at = list(stealth = c(3, 9)))
emmeans(model_chem, ~ sindes, at = list(sindes = c("Sx", "Dx")))

# Summary statistics for the variables
summary(emmeans(model_moon, ~ gender, at = list(gender = c(1, 2))))
plot(emmeans(model_moon, ~ gender, at = list(gender = c(1, 2))))


# Table A1
# Group 1: pre-beliefs
summary(select(df, moon1, vacc1, stam1, chem1))

# Group 2: post-beliefs
summary(select(df, moon2, vacc2, stam2, chem2))

# Group 3: control variables
summary(select(df, gender, age, titstu, stealth, sindes))

#Table A2
df <- df %>%
  mutate(
    consp1_tot = ifelse(!is.na(consp1) & !is.na(consp2),
                        moon1_b + vacc1_b + stam1_b + chem1_b, NA),
    consp2_tot = ifelse(!is.na(consp1) & !is.na(consp2),
                        moon2_b + vacc2_b + stam2_b + chem2_b, NA)
  )
summary(df$consp1_tot, na.rm = TRUE)
summary(df$consp2_tot, na.rm = TRUE)

# Absolute frequencies
cat("\nconsp1_tot frequencies:\n")
print(table(df$consp1_tot, useNA = "ifany"))

cat("\nconsp2_tot frequencies:\n")
print(table(df$consp2_tot, useNA = "ifany"))

# Percentages
cat("\nconsp1_tot percentages:\n")
print(round(prop.table(table(df$consp1_tot)) * 100, 2))

cat("\nconsp2_tot percentages:\n")
print(round(prop.table(table(df$consp2_tot)) * 100, 2))


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
library(fixest)  # per modelli panel con effetti fissi
library(emmeans) # per i margini

# Renaming the variables
df <- df %>%
  rename(
    pre_1 = moon1, pre_2 = vacc1, pre_3 = stam1, pre_4 = chem1,
    post_1 = moon2, post_2 = vacc2, post_3 = stam2, post_4 = chem2,
    diff_1 = diff_moon, diff_2 = diff_vacc, diff_3 = diff_stam, diff_4 = diff_chem
  ) %>%
  mutate(id_ = row_number())

# Reshape long
pre_long <- df %>%
  select(id_, starts_with("pre_")) %>%
  pivot_longer(cols = starts_with("pre_"), names_to = "consp", names_prefix = "pre_", values_to = "pre")

post_long <- df %>%
  select(id_, starts_with("post_")) %>%
  pivot_longer(cols = starts_with("post_"), names_to = "consp", names_prefix = "post_", values_to = "post")

diff_long <- df %>%
  select(id_, starts_with("diff_")) %>%
  pivot_longer(cols = starts_with("diff_"), names_to = "consp", names_prefix = "diff_", values_to = "diff")

df_long <- pre_long %>%
  left_join(post_long, by = c("id_", "consp")) %>%
  left_join(diff_long, by = c("id_", "consp"))

# Modello a effetti fissi
model <- feols(diff ~ factor(pre) * factor(consp) | id_, data = df_long)

# Calcolo dei margini attesi
emm <- emmeans(model, ~ pre * consp, at = list(pre = 0:10, consp = 1:4))
summary(emm)

# Visualizzazione
plot(emm, comparisons = TRUE) +
  labs(title = "Margini attesi per pre e consp", x = "pre", y = "diff")


