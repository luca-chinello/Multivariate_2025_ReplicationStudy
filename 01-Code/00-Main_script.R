#REPLICATION MATERIAL - The life cycle of conspiracy theories: 
#Evidence from a long-term panel survey on conspiracy beliefs in Italy
# Original authors: M. Mancosu - S. Vassallo
# Replicated by: Luca Chinello
# SCRIPT R - from STATA


#Loading necessary libraries

library(haven)       # From Stata.dta to R data frame
library(dplyr)       
library(psych)       # for Cronbach's alpha and descriptive statistics
library(margins)     # For marginal effects
library(tidyr)       
library(ggplot2) 
library(stargazer)   # For tables plotting
library(modelsummary) # For big tables plotting


# Load the dataset

df <- read_dta("00-Data/replication_ds.dta")


#DESCRIPTIVES OF THE VARIABLES EMPLOYED

# Printing the frequencies of each column of interest

table(df$D38_01_W9, useNA = "ifany")
table(df$D38_02_W9, useNA = "ifany")
table(df$D38_03_W9, useNA = "ifany")
table(df$D38_04_W9, useNA = "ifany")

table(df$D38_post_01, useNA = "ifany")
table(df$D38_post_02, useNA = "ifany")
table(df$D38_post_03, useNA = "ifany")
table(df$D38_post_04, useNA = "ifany")

table(df$SEX, useNA = "ifany")
table(df$ANNO, useNA = "ifany")
table(df$AMP, useNA = "ifany")
table(df$ZONA, useNA = "ifany")
table(df$scolarita, useNA = "ifany")

table(df$S21_1, useNA = "ifany")
table(df$S21_2, useNA = "ifany")
table(df$S21_3, useNA = "ifany")
table(df$S21_4, useNA = "ifany")


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

#Type b recode: 0 (values 0-5); 1 (values 6-10); NA (values >10)
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


# Type c recode: 0 (value 0); 1 (values 1-5); 2 (values 6-10); NA (values >10)

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

# Function to calculate predicted margins for a given variable and values

predict_margin <- function(model, var, values) {
  sapply(values, function(v) {
    newdata <- regression_data
    if (is.factor(regression_data[[var]])) {
      newdata[[var]] <- factor(rep(v, nrow(newdata)), levels = levels(regression_data[[var]]))
    } else {
      newdata[[var]] <- rep(v, nrow(newdata))
    }
    preds <- predict(model, newdata = newdata)
    mean(preds, na.rm = TRUE)
  })
}


# Marginal effects table for each model and variable

margins_table <- data.frame(
  Variabile = rep(c("gender", "age", "stealth", "sindes"), each = 2),
  Valore = c("1", "2", 25, 65, 3, 9, "Sx", "Dx"),
  moon = c(predict_margin(model_moon, "gender", c("1", "2")),
           predict_margin(model_moon, "age", c(25, 65)),
           predict_margin(model_moon, "stealth", c(3, 9)),
           predict_margin(model_moon, "sindes", c("Sx", "Dx"))),
  vacc = c(predict_margin(model_vacc, "gender", c("1", "2")),
           predict_margin(model_vacc, "age", c(25, 65)),
           predict_margin(model_vacc, "stealth", c(3, 9)),
           predict_margin(model_vacc, "sindes", c("Sx", "Dx"))),
  stam = c(predict_margin(model_stam, "gender", c("1", "2")),
           predict_margin(model_stam, "age", c(25, 65)),
           predict_margin(model_stam, "stealth", c(3, 9)),
           predict_margin(model_stam, "sindes", c("Sx", "Dx"))),
  chem = c(predict_margin(model_chem, "gender", c("1", "2")),
           predict_margin(model_chem, "age", c(25, 65)),
           predict_margin(model_chem, "stealth", c(3, 9)),
           predict_margin(model_chem, "sindes", c("Sx", "Dx")))
)


margins_table


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

round(prop.table(table(analysis_data_A2$consp1_tot)) * 100, 1)
round(prop.table(table(analysis_data_A2$consp2_tot)) * 100, 1)

# Results show how much interviewees believed in conspiracy theories before (consp1_tot) and 
# after (consp2_tot) 2020
# 0 = none, 4 = all four beliefs


# TABLE A3 
# Displaying how beliefs have changed pre and post 2020: 0 (no belief), 1 (low belief), 2 (high belief)

# Moon landing
round(prop.table(table(df$moon1_c, useNA = "ifany")) * 100, 1)
round(prop.table(table(df$moon2_c, useNA = "ifany")) * 100, 1)


# Vaccines effects
round(prop.table(table(df$vacc1_c, useNA = "ifany")) * 100, 1)
round(prop.table(table(df$vacc2_c, useNA = "ifany")) * 100, 1)


# Stamina method
round(prop.table(table(df$stam1_c, useNA = "ifany")) * 100, 1)
round(prop.table(table(df$stam2_c, useNA = "ifany")) * 100, 1)


# Chemtrails
round(prop.table(table(df$chem1_c, useNA = "ifany")) * 100, 1)
round(prop.table(table(df$chem2_c, useNA = "ifany")) * 100, 1)


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

# Generating the predictions for the model just created

predictions_final <- ggeffects::ggpredict(final_lm_model, terms = c("pre [0:10]", "consp"))


# Plot

figure1 <- ggplot(predictions_final, aes(x = x, y = predicted, group = group)) +
  geom_line(aes(linetype = group), color = "black") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, color = "black") +
  geom_point(aes(shape = group), color = "black", fill = "white", size = 2.5) +
  scale_linetype_manual(values = c("Moon" = "solid", 
                                   "Vaccine" = "dashed",              # using the same lines as in the original plot
                                   "Stamina" = "dotted", 
                                   "Chemtrails" = "longdash")) +
  scale_shape_manual(values = c("Moon" = 21,
                                "Vaccine" = 22,
                                "Stamina" = 23,                       # reproducing the same shapes as in the original plot
                                "Chemtrails" = 24)) +
  scale_x_continuous(limits = c(0, 10), breaks = 0:10) +              # setting a continuous scale 0-10
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

ggsave("02-Plots/Figure 1.png", plot = figure1, width = 10, height = 6)



### EXTENSION!
## This extension consists in applying a GLM (logit) to the data.
# We already have a binary version of the variables (value  < 6 means "I don't believe"; value >= 6 means "I believe")
# The logit model will allow us to understand the factors that influence the belief in conspiracy theories in 2016 vs 2020.
# When the logit model is applied to 2020 beliefs, it gives us an overview of the factors that influence the belief 
# in conspiracy theories in 2020. Once we have the results, we can compare the original OLS models (which tell us which
# factors contribute the most to reducing the belief in conspiracy theories) with the logit models (which tell us the 
# belief distribution in 2020).

# LOGIT MODELS:

# Moon
logit_moon1 <- glm(moon1_b ~ gender + age + titstu + stealth + sindes, 
                   data = df, family = binomial(link = "logit"))
summary(logit_moon1)


logit_moon2 <- glm(moon2_b ~ gender + age + titstu + stealth + sindes, 
                  data = df, family = binomial(link = "logit"))
summary(logit_moon2)

stargazer(logit_moon1, logit_moon2, type = "html", out = "03-Output/Moon_logit_models.html",
          title = "Logit models for Moon landing belief pre and post 2020")


# Chemtrails
logit_chem1 <- glm(chem1_b ~ gender + age + titstu + stealth + sindes, 
                   data = df, family = binomial(link = "logit"))
summary(logit_chem1)


logit_chem2 <- glm(chem2_b ~ gender + age + titstu + stealth + sindes, 
                  data = df, family = binomial(link = "logit"))
summary(logit_chem2)

stargazer(logit_chem1, logit_chem2, type = "html", out = "03-Output/Chem_logit_models.html",
          title = "Logit models for Chemtrails belief pre and post 2020")


# Vaccines
logit_vacc1 <- glm(vacc1_b ~ gender + age + titstu + stealth + sindes, 
                   data = df, family = binomial(link = "logit"))
summary(logit_vacc1)

logit_vacc2 <- glm(vacc2_b ~ gender + age + titstu + stealth + sindes, 
                  data = df, family = binomial(link = "logit"))
summary(logit_vacc2)

stargazer(logit_vacc1, logit_vacc2, type = "html", out = "03-Output/Vacc_logit_models.html",
          title = "Logit models for Vaccine belief pre and post 2020")


# Stamina
logit_stam1 <- glm(stam1_b ~ gender + age + titstu + stealth + sindes, 
                   data = df, family = binomial(link = "logit"))
summary(logit_stam1)

logit_stam2 <- glm(stam2_b ~ gender + age + titstu + stealth + sindes, 
                  data = df, family = binomial(link = "logit"))
summary(logit_stam2)

stargazer(logit_stam1, logit_stam2, type = "html", out = "03-Output/Stam_logit_models.html",
          title = "Logit models for Stamina belief pre and post 2020")


# Table with the results of the logit models
stargazer(logit_moon1, logit_chem1, logit_vacc1, logit_stam1,                       # log-odds results
          type = "html", out = "03-Output/Logit1_models.html",
          title = "2016 Separate logit models for each conspiracy belief")

stargazer(logit_moon2, logit_chem2, logit_vacc2, logit_stam2,                       # log-odds results
          type = "html", out = "03-Output/Logit2_models.html",
          title = "2020 Separate logit models for each conspiracy belief")


# Table with all logit models

models <- list(
  "Moon 2016" = logit_moon1,
  "Chem 2016" = logit_chem1,
  "Vacc 2016" = logit_vacc1,
  "Stam 2016" = logit_stam1,
  "Moon 2020" = logit_moon2,
  "Chem 2020" = logit_chem2,
  "Vacc 2020" = logit_vacc2,
  "Stam 2020" = logit_stam2
)
modelsummary(models, output = "03-Output/All_Logit_models.html")


# Comparison OLS vs logit model results
stargazer(model_moon, logit_moon2, type = "html", out = "03-Output/Moon_comparison.html",
          title = "Comparison between lm and logit models for Moon landing belief")

stargazer(model_vacc, logit_vacc2, type = "html", out = "03-Output/Vacc_comparison.html",
          title = "Comparison between lm and logit models for Vaccine belief")

stargazer(model_stam, logit_stam2, type = "html", out = "03-Output/Stam_comparison.html",
          title = "Comparison between lm and logit models for Stamina belief")

stargazer(model_chem, logit_chem2, type = "html", out = "03-Output/Chem_comparison.html",
          title = "Comparison between lm and logit models for Chemtrails belief")



# Single Marginal effects for all logit models

# Moon landing
marg_moon_logit1 <- margins(logit_moon1)
summary(marg_moon_logit1)

marg_moon_logit2 <- margins(logit_moon2)
summary(marg_moon_logit2)

# Chemtrails
marg_chem_logit1 <- margins(logit_chem1)
summary(marg_chem_logit1)

marg_chem_logit2 <- margins(logit_chem2)
summary(marg_chem_logit2)

# Vaccini
marg_vacc_logit1 <- margins(logit_vacc1)
summary(marg_vacc_logit1)

marg_vacc_logit2 <- margins(logit_vacc2)
summary(marg_vacc_logit2)

# Stamina
marg_stam_logit1 <- margins(logit_stam1)
summary(marg_stam_logit1)

marg_stam_logit2 <- margins(logit_stam2)
summary(marg_stam_logit2)


# 2020 Marginal Effects table thanks to library(modelsummary)

models_2020 <- list(
  "Moon 2020" = logit_moon2,
  "Chem 2020" = logit_chem2,
  "Vacc 2020" = logit_vacc2,
  "Stam 2020" = logit_stam2
)

modelsummary(
  models_2020,
  output = "03-Output/Logit_marginal_effects.html",
  title = "Logit models Average Marginal Effects (AME) - 2020",
  estimates = "AME",
  stars = TRUE,      
  statistic = "p.value", 
  gof_map = c("nobs", "r.squared")
)

# END OF THE SCRIPT
