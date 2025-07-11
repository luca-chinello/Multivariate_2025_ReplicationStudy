#REPLICATION MATERIAL - The life cycle of conspiracy theories: 
#Evidence from a long-term panel survey on conspiracy beliefs in Italy
# Original authors: M. Mancosu - S. Vassallo
# Replicated by: Luca Chinello
# SCRIPT R - from STATA


#Loading necessary libraries

library(haven)       # From Stata .dta to R data frame
library(dplyr)       
library(psych)       # for Cronbach's alpha and descriptive statistics
library(margins)     # For marginal effects
library(plm)         # For fixed effects models
library(tidyr)       
library(ggplot2) 
library(stargazer)  # For tables plotting


# Load the dataset

df <- read_dta("01-Data/replication_ds.dta")


#DESCRIPTIVES OF THE VARIABLES EMPLOYED

# Printing the frequencies of each column of interest

print(table(df$D38_01_W9, useNA = "ifany"))
print(table(df$D38_02_W9, useNA = "ifany"))
print(table(df$D38_03_W9, useNA = "ifany"))
print(table(df$D38_04_W9, useNA = "ifany"))

print(table(df$D38_post_01, useNA = "ifany"))
print(table(df$D38_post_02, useNA = "ifany"))
print(table(df$D38_post_03, useNA = "ifany"))
print(table(df$D38_post_04, useNA = "ifany"))

print(table(df$SEX, useNA = "ifany"))
print(table(df$ANNO, useNA = "ifany"))
print(table(df$AMP, useNA = "ifany"))
print(table(df$ZONA, useNA = "ifany"))
print(table(df$scolarita, useNA = "ifany"))

print(table(df$S21_1, useNA = "ifany"))
print(table(df$S21_2, useNA = "ifany"))
print(table(df$S21_3, useNA = "ifany"))
print(table(df$S21_4, useNA = "ifany"))


# DATA MANAGEMENT 

df <- df %>%
  mutate(
    
    # Renaming pre-2020 the variables + substituting 12 with NA
    moon1 = na_if(D38_01_W9, 12),
    vacc1 = na_if(D38_02_W9, 12),
    stam1 = na_if(D38_03_W9, 12),
    chem1 = na_if(D38_04_W9, 12),
    
    # Rescaling from 1-12 to 0-11
    moon1 = moon1 - 1,
    vacc1 = vacc1 - 1,
    stam1 = stam1 - 1,
    chem1 = chem1 - 1,
    
    # renaming post-2020 variables + substituting 12 with NA
    moon2 = na_if(D38_post_01, 12),
    vacc2 = na_if(D38_post_02, 12),
    stam2 = na_if(D38_post_03, 12),
    chem2 = na_if(D38_post_04, 12),
    
    # Rescaling from 1-12 to 0-11
    moon2 = moon2 - 1,
    vacc2 = vacc2 - 1,
    stam2 = stam2 - 1,
    chem2 = chem2 - 1
  )

View(df)

# Generating the means of pre-2020 conspiracy beliefs

df$consp1 <- rowMeans(df[, c("moon1", "vacc1", "stam1", "chem1")], na.rm = FALSE)


# Generating the means of post-2020 conspiracy beliefs

df$consp2 <- rowMeans(df[, c("moon2", "vacc2", "stam2", "chem2")], na.rm = FALSE)


# Generating new columns with different types of dummy variables

df <- df %>%
  mutate(
    across(
      .cols = c(moon1, vacc1, stam1, chem1, moon2, vacc2, stam2, chem2),
      .fns = ~ case_when(
        . >= 0 & . < 6 ~ 0,
        . >= 6 & . <= 10 ~ 1,
        TRUE ~ NA_real_
      ),
      .names = "{.col}_b"
      )
    )
  


# Equivalente a: recode ... (0 = 0) (1/5 = 1) (6/10=2),gen(...)

df <- df %>%
  mutate(
    across(
      .cols = c(moon1, vacc1, stam1, chem1, moon2, vacc2, stam2, chem2),
      .fns = ~ case_when(
        . == 0 ~ 0,
        . > 0 & . <= 5 ~ 1,
        . >= 6 & . <= 10 ~ 2,
        TRUE ~ NA_real_
      ),
      .names = "{.col}_c"
    )
  )


# Generating columns with  means differences pre and post 2020

df <- df %>%
  mutate(
    diff_moon = moon2 - moon1,
    diff_vacc = vacc2 - vacc1,
    diff_stam = stam2 - stam1,
    diff_chem = chem2 - chem1
  )

View(df)


# CONTROLS

df <- df %>%
  mutate(
    ANNO = ifelse(ANNO == ".", NA, ANNO),
    anno2 = as.numeric(ANNO),
    age = 2020 - anno2,
    
    # Recoding education level: 1 = Low, 2 = Medium, 3 = High
    titstu = factor(case_when(
      scolarita %in% c(1, 2) ~ 1,
      scolarita %in% c(3, 4, 5) ~ 2,
      scolarita %in% c(6, 7, 8, 9, 10, 11) ~ 3
    ), 
    labels = c("Bassa", "Media", "Alta")),
    
    # Recoding political beliefs from 1 = Left to 5 = Right
    sindes = factor(case_when(
      D4_post %in% c(1, 2) ~ 1,
      D4_post %in% c(3, 4, 5) ~ 2,
      D4_post == 6 ~ 3,
      D4_post %in% c(7, 8, 9) ~ 4,
      D4_post %in% c(10, 11) ~ 5,
      D4_post == 13 ~ 6,
      TRUE ~ NA_real_
    ), labels = c("Sx", "Csx", "C", "Cdx", "Dx", "NC"))
  ) %>%
  rename(zgp5 = ZONA) %>%
  rename(gender = SEX) %>% 
  mutate(gender = as.factor(gender))


# Stealth variables (political trust)
# stealth1 = necessity of a party system
# stealth2 = participation to the democratic life thanks to the party system
# stealth3 = people are not enough interested in politics
# stealth4 = No party system means no democracy. How much do you agree?

df <- df %>%
  mutate(
    stealth1 = na_if(S21_1, 12),
    stealth2 = na_if(S21_2, 12),
    stealth3 = na_if(S21_3, 12),
    stealth4 = na_if(S21_4, 12)
  )

# Chronbach's alpha for stealth variables

print(psych::alpha(df[, c("stealth1", "stealth2", "stealth3", "stealth4")], check.keys=FALSE))


# Mean of stealth variables

df$stealth <- rowMeans(df[, c("stealth1", "stealth2", "stealth3", "stealth4")], na.rm = FALSE)


# Removing rows with NA in consp1 and consp2

regression_data <- df %>%
  filter(!is.na(consp1) & !is.na(consp2))



# TABLE 1

# diff_moon regression model
model_moon <- lm(diff_moon ~ gender + age + titstu + stealth + sindes, data = regression_data)
summary(model_moon)

# diff_vacc regression model
model_vacc <- lm(diff_vacc ~ gender + age + titstu + stealth + sindes, data = regression_data)
summary(model_vacc)

# diff_stam regression model
model_stam <- lm(diff_stam ~ gender + age + titstu + stealth + sindes, data = regression_data)
summary(model_stam)

# diff_chem regression model
model_chem <- lm(diff_chem ~ gender + age + titstu + stealth + sindes, data = regression_data)
summary(model_chem)

stargazer(model_moon, model_vacc, model_stam, model_chem, type = "html", out = "03-Output/Table 1.html")

# TABLE 2
# COEFFICIENTS ONLY

# model_moon margins
margins(model_moon, at = list(gender = 1:2))
margins(model_moon, at = list(age = c(25, 65)))
margins(model_moon, at = list(stealth = c(3, 9)))
margins(model_moon, at = list(sindes = c("Sx", "Dx")))

# model_vacc margins
print(margins(model_vacc, at = list(gender = 1:2)))
print(margins(model_vacc, at = list(age = c(25, 65))))
print(margins(model_vacc, at = list(stealth = c(3, 9))))
print(margins(model_vacc, at = list(sindes = c("Sx", "Dx"))))

# model_stam margins
print(margins(model_stam, at = list(gender = 1:2)))
print(margins(model_stam, at = list(age = c(25, 65))))
print(margins(model_stam, at = list(stealth = c(3, 9))))
print(margins(model_stam, at = list(sindes = c("Sx", "Dx"))))

# model_chem margins
print(margins(model_chem, at = list(gender = 1:2)))
print(margins(model_chem, at = list(age = c(25, 65))))
print(margins(model_chem, at = list(stealth = c(3, 9))))
print(margins(model_chem, at = list(sindes = c("Sx", "Dx"))))


# TABLE A1
# COEFFICIENTS ONLY

describe(df[, c("moon1", "vacc1", "stam1", "chem1")])
describe(df[, c("moon2", "vacc2", "stam2", "chem2")])
describe(df[, c("gender", "age", "stealth")])

table(df$titstu)
table(df$sindes)


# TABLE A2 
# COEFFICIENTS ONLY 
# Generating total conspiracy belief scores before and after 2020

analysis_data_A2 <- df %>%
  filter(!is.na(consp1) & !is.na(consp2)) %>%
  mutate(
    consp1_tot = rowSums(select(., moon1_b, vacc1_b, stam1_b, chem1_b), na.rm = FALSE),
    consp2_tot = rowSums(select(., moon2_b, vacc2_b, stam2_b, chem2_b), na.rm = FALSE)
  )

table(analysis_data_A2$consp1_tot, useNA = "ifany")
table(analysis_data_A2$consp2_tot, useNA = "ifany")

# Results show how much interviewees believed in conspiracy theories before (consp1_tot) and 
# after (consp2_tot) 2020
# 0 = none, 4 = all four beliefs


# TABLE A3 
# Displaying how beliefs have changed pre and post 2020: 0 (no belief), 1 (low belief), 2 (high belief)

# Moon landing
table(df$moon1_c, useNA = "ifany")
table(df$moon2_c, useNA = "ifany")

# Vaccines effects
table(df$vacc1_c, useNA = "ifany")
table(df$vacc2_c, useNA = "ifany")

# Stamina method
table(df$stam1_c, useNA = "ifany")
table(df$stam2_c, useNA = "ifany")


# Chemtrails
table(df$chem1_c, useNA = "ifany")
table(df$chem2_c, useNA = "ifany")


# FIGURE 1

# Creation of the long format data frame for Figure 1

df_long <- df %>%
  mutate(id_ = row_number()) %>%
  rename(
    pre_moon = moon1, pre_vacc = vacc1, pre_stam = stam1, pre_chem = chem1,
    post_moon = moon2, post_vacc = vacc2, post_stam = stam2, post_chem = chem2,
    diff_moon = diff_moon, diff_vacc = diff_vacc, diff_stam = diff_stam, diff_chem = diff_chem
  ) %>%
  pivot_longer(
    cols = starts_with(c("pre_", "post_", "diff_")),
    names_to = c(".value", "consp"),
    names_pattern = "([a-z]+)_([a-z]+)"
  ) %>%
  mutate(consp = factor(consp, 
                        levels = c("moon", "vacc", "stam", "chem"),
                        labels = c("Moon", "Vaccine", "Stamina", "Chemtrails")))

model_data <- df_long %>%
  filter(!is.na(diff) & !is.na(pre) & !is.na(consp))


# Generating a simple, broad regression model

final_lm_model <- lm(diff ~ factor(pre) * consp, data = model_data)

predictions_final <- ggeffects::ggpredict(final_lm_model, terms = c("pre [0:10]", "consp"))


# Plot

figure1 <- ggplot(predictions_final, aes(x = x, y = predicted, group = group)) +
  geom_line(aes(linetype = group), color = "black") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, color = "black") +
  geom_point(aes(shape = group), color = "black", fill = "white", size = 2.5) +
  scale_linetype_manual(values = c("Moon" = "solid", 
                                   "Vaccine" = "dashed", 
                                   "Stamina" = "dotted", 
                                   "Chemtrails" = "longdash")) +
  scale_shape_manual(values = c("Moon" = 21,
                                "Vaccine" = 24,
                                "Stamina" = 23,
                                "Chemtrails" = 22)) +
  labs(
    y = "2020-2016 difference",
    x = "2016 wave average",
    linetype = NULL, 
    shape = NULL     
  ) +
  theme_classic() +
  theme(
    legend.position = "right",
    legend.key = element_rect(fill = "transparent", colour = "transparent"),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black")
  )

figure1

ggsave("02-Plots/figure1_replicationR.png", plot = figure1, width = 10, height = 6)
