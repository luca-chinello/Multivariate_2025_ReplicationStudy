#----------------------------------------------------------------#
#****** REPLICATION MATERIAL - The life cycle of conspiracy theories: 
#****** Evidence from a long-term panel survey on conspiracy beliefs in Italy
#****** M. Mancosu - S. Vassallo
#****** #****** SCRIPT R - Traduzione da Stata
#----------------------------------------------------------------#


# Caricamento delle librerie necessarie
library(haven)       # Per leggere i file .dta di Stata
library(dplyr)       # Per la manipolazione dei dati
library(psych)       # Per l'alpha di Cronbach e le statistiche descrittive
library(margins)     # Per calcolare gli effetti marginali (equivalente a `margins` di Stata)
library(plm)         # Per modelli a effetti fissi (equivalente a `xtreg, fe`)
library(tidyr)       # Per la trasformazione dei dati (reshape)
library(ggplot2)     # Per i grafici (alternativa a `marginsplot`)



# Caricare il dataset (equivalente a `use "replication_ds.dta",clear`)
df <- read_dta("01-Data/replication_ds.dta")


#----------------------------------------------------------------#
#*********************** DESCRIPTIVES OF THE VAR EMPLOYED *********
#----------------------------------------------------------------#

# Equivalente a `fre D38_01_W9 D38_02_W9 D38_03_W9 D38_04_W9` ecc.
# Usiamo la funzione `table()` di R per ottenere le frequenze
# Le stamperemo una per una come nello script Stata.
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


#----------------------------------------------------------------#
#*********************** DATA MANAGEMENT **************************
#----------------------------------------------------------------#

df <- df %>%
  mutate(
    # Equivalente a: recode D38_01_W9 D38_02_W9 D38_03_W9 D38_04_W9 (12=.),gen(moon1 vacc1 stam1 chem1)
    moon1 = na_if(D38_01_W9, 12),
    vacc1 = na_if(D38_02_W9, 12),
    stam1 = na_if(D38_03_W9, 12),
    chem1 = na_if(D38_04_W9, 12),
    
    # Equivalente a: replace moon1 = moon1 - 1 (e per le altre variabili)
    moon1 = moon1 - 1,
    vacc1 = vacc1 - 1,
    stam1 = stam1 - 1,
    chem1 = chem1 - 1,
    
    # Equivalente a: recode D38_post_01 D38_post_02 D38_post_03 D38_post_04 (12=.),gen(moon2 vacc2 stam2 chem2)
    moon2 = na_if(D38_post_01, 12),
    vacc2 = na_if(D38_post_02, 12),
    stam2 = na_if(D38_post_03, 12),
    chem2 = na_if(D38_post_04, 12),
    
    # Equivalente a: replace moon2 = moon2 - 1 (e per le altre variabili)
    moon2 = moon2 - 1,
    vacc2 = vacc2 - 1,
    stam2 = stam2 - 1,
    chem2 = chem2 - 1
  )

# Equivalente a: gen consp1 = (moon1 + vacc1 + stam1 + chem1)/4
df$consp1 <- rowMeans(df[, c("moon1", "vacc1", "stam1", "chem1")], na.rm = FALSE)

# Equivalente a: gen consp2 = (moon2 + vacc2 + stam2 + chem2)/4
df$consp2 <- rowMeans(df[, c("moon2", "vacc2", "stam2", "chem2")], na.rm = FALSE)


# Equivalente a: recode ... (0/5.9999 = 0) (6/10 = 1),gen(...)
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

# Equivalente a: gen diff_moon = moon2-moon1 (e per le altre)
df <- df %>%
  mutate(
    diff_moon = moon2 - moon1,
    diff_vacc = vacc2 - vacc1,
    diff_stam = stam2 - stam1,
    diff_chem = chem2 - chem1
  )

#****** controls

df <- df %>%
  # Equivalente a: destring ANNO,gen(anno2) e gen age = 2020 - anno2
  mutate(
    anno2 = as.numeric(ANNO),
    age = 2020 - anno2,
    # Equivalente a: recode scolarita (1 2=1 "Bassa") (3 4 5=2 "Media") (6 7 8 9 10 11=3 "Alta"),gen(titstu)
    titstu = factor(case_when(
      scolarita %in% c(1, 2) ~ 1,
      scolarita %in% c(3, 4, 5) ~ 2,
      scolarita %in% c(6, 7, 8, 9, 10, 11) ~ 3
    ), 
    labels = c("Bassa", "Media", "Alta")),
    # Equivalente a: recode D4_post ... gen(sindes)
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
  # Equivalente a: rename ZONA zgp5
  rename(zgp5 = ZONA) %>%
  # Equivalente a: rename SEX gender
  rename(gender = SEX) %>% 
  mutate(gender = as.factor(gender))

# Gestione delle variabili "stealth"
df <- df %>%
  mutate(
    # Equivalente a: recode S21_1 S21_2 S21_3 S21_4 (12=.),gen(stealth1 stealth2 stealth3 stealth4)
    stealth1 = na_if(S21_1, 12),
    stealth2 = na_if(S21_2, 12),
    stealth3 = na_if(S21_3, 12),
    stealth4 = na_if(S21_4, 12)
  )

# Equivalente a: alpha stealth1 stealth2 stealth3 stealth4
print(psych::alpha(df[, c("stealth1", "stealth2", "stealth3", "stealth4")], check.keys=FALSE))

# Equivalente a: gen stealth = (stealth1 + stealth2 + stealth3 + stealth4)/4
df$stealth <- rowMeans(df[, c("stealth1", "stealth2", "stealth3", "stealth4")], na.rm = FALSE)

# Filtrare i dati per le regressioni, come `if consp1!=. & consp2!=.` in Stata
regression_data <- df %>%
  filter(!is.na(consp1) & !is.na(consp2))


#----------------------------------------------------------------#
#*********************** TABLE 1 **********************************
#***************** COEFFICIENTS ONLY ****************************
#----------------------------------------------------------------#
# In R, `i.variable` di Stata è gestito creando un fattore `factor(variable)`. 
# Le variabili numeriche sono trattate come continue di default (`c.`).

# Modello per diff_moon
model_moon <- lm(diff_moon ~ gender + age + titstu + stealth + sindes, data = regression_data)
summary(model_moon)

# Modello per diff_vacc
model_vacc <- lm(diff_vacc ~ gender + age + titstu + stealth + sindes, data = regression_data)
summary(model_vacc)

# Modello per diff_stam
model_stam <- lm(diff_stam ~ gender + age + titstu + stealth + sindes, data = regression_data)
summary(model_stam)

# Modello per diff_chem
model_chem <- lm(diff_chem ~ gender + age + titstu + stealth + sindes, data = regression_data)
summary(model_chem)


#----------------------------------------------------------------#
#*********************** TABLE 2 **********************************
#***************** COEFFICIENTS ONLY ****************************
#----------------------------------------------------------------#
# Equivalente a `margins,at(...)`

# Margini per il modello diff_moon
margins(model_moon, at = list(gender = 1:2))
margins(model_moon, at = list(age = c(25, 65)))
margins(model_moon, at = list(stealth = c(3, 9)))
margins(model_moon, at = list(sindes = c("Sx", "Dx"))) # Nota: `sindes` è un fattore, si usano i livelli

# Margini per il modello diff_vacc
print(margins(model_vacc, at = list(gender = 1:2)))
print(margins(model_vacc, at = list(age = c(25, 65))))
print(margins(model_vacc, at = list(stealth = c(3, 9))))
print(margins(model_vacc, at = list(sindes = c("Sx", "Dx"))))

# Margini per il modello diff_stam
print(margins(model_stam, at = list(gender = 1:2)))
print(margins(model_stam, at = list(age = c(25, 65))))
print(margins(model_stam, at = list(stealth = c(3, 9))))
print(margins(model_stam, at = list(sindes = c("Sx", "Dx"))))

# Margini per il modello diff_chem
print(margins(model_chem, at = list(gender = 1:2)))
print(margins(model_chem, at = list(age = c(25, 65))))
print(margins(model_chem, at = list(stealth = c(3, 9))))
print(margins(model_chem, at = list(sindes = c("Sx", "Dx"))))


#----------------------------------------------------------------#
#*********************** TABLE A1 *********************************
#***************** COEFFICIENTS ONLY ****************************
#----------------------------------------------------------------#
# Equivalente a `tabstat ..., statistics(mean sd min max)`
# La funzione `describe` del pacchetto `psych` è molto simile

describe(df[, c("moon1", "vacc1", "stam1", "chem1")])
describe(df[, c("moon2", "vacc2", "stam2", "chem2")])
describe(df[, c("gender", "age", "stealth")])
# Per le variabili fattoriali, `describe` non è l'ideale, mostriamo le tabelle di frequenza
table(df$titstu)
table(df$sindes)


#----------------------------------------------------------------#
#*********************** TABLE A2 *********************************
#***************** COEFFICIENTS ONLY ****************************
#----------------------------------------------------------------#
# Equivalente a `gen consp1_tot = ...` e `gen consp2_tot = ...`
analysis_data_A2 <- df %>%
  filter(!is.na(consp1) & !is.na(consp2)) %>%
  mutate(
    consp1_tot = rowSums(select(., moon1_b, vacc1_b, stam1_b, chem1_b), na.rm = FALSE),
    consp2_tot = rowSums(select(., moon2_b, vacc2_b, stam2_b, chem2_b), na.rm = FALSE)
  )

# Equivalente a `fre consp1_tot consp2_tot`
table(analysis_data_A2$consp1_tot, useNA = "ifany")
table(analysis_data_A2$consp2_tot, useNA = "ifany")

# Questi risultati mostrano la distribuzione degli intervistati in base al numero totale di teorie del 
# complotto a cui credevano, prima (consp1_tot) e dopo (consp2_tot).
# Il punteggio va da 0 (non credere a nessuna delle quattro teorie) a 4 (credere a tutte e quattro).

#----------------------------------------------------------------#
#*********************** TABLE A3 *********************************
#***************** RAW PERCENTAGES ONLY *************************
#----------------------------------------------------------------#
# Equivalente a `fre moon1_c moon2_c`, ecc.
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


#----------------------------------------------------------------#
#*********************** FIGURE 1 *********************************
#----------------------------------------------------------------#

#----------------------------------------------------------------#
#*********************** FIGURE 1 *********************************
#----------------------------------------------------------------#
# 1. PREPARAZIONE DATI LUNGHI (come nella prima versione)
#    (Assumiamo che 'df' esista e contenga le variabili moon1, diff_moon, etc.)

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

# 2. CREAZIONE DEL MODELLO CORRETTO
#    Trattiamo 'pre' come un FATTORE, come indicato da 'i.pre' in Stata.
#    Useremo il modello lm() perché è quello che più probabilmente è stato usato
#    per la visualizzazione nel paper, come discusso in precedenza.
final_lm_model <- lm(diff ~ factor(pre) * consp, data = model_data)

# 3. CALCOLO DELLE PREVISIONI DAL MODELLO CORRETTO
predictions_final <- ggeffects::ggpredict(final_lm_model, terms = c("pre [0:10]", "consp"))


# 4. CREAZIONE DEL GRAFICO FINALE
#    Il codice per il grafico non cambia.
ggplot(predictions_final, aes(x = x, y = predicted, group = group)) +
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

