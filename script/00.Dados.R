
# Pacotes -----------------------------------------------------------------

library(flextable)
library(GGally)
library(ggplot2)
library(patchwork)
library(janitor)
library(magrittr)
library(purrr)
library(stringr)
library(tidyr)
library(dplyr)

# Semente - Para reprodutibilidade ----------------------------------------

seed = 2023

# Data --------------------------------------------------------------------

df <- readr::read_csv("data/heart_failure_clinical_records_dataset.csv") %>% clean_names() %>% select(-time)

numeric_var <- c('age', 'creatinine_phosphokinase', 'ejection_fraction', 'platelets', 'serum_creatinine', 'serum_sodium')

df_catnum <- df %>% 
  mutate(
    death_event = ifelse(death_event==0, "sobreviveu", "faleceu"),
    sex = if_else(sex==0, "F", "M"), 
    anaemia = if_else(anaemia==0, "Não", "Sim"), 
    diabetes = if_else(diabetes==0, "Não", "Sim"), 
    high_blood_pressure = if_else(high_blood_pressure==0, "Não", "Sim"), 
    smoking = if_else(smoking==0, "Não", "Sim")
  )

# -------------------------------------------------------------------------
