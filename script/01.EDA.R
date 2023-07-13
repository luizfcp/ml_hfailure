
# Data --------------------------------------------------------------------

source("script/00.Dados.R")

# EDA ---------------------------------------------------------------------

# Variáveis Categóricas
df %>% 
  mutate(
    death_event = ifelse(death_event==0, "Paciente Sobrevivente", "Paciente Falecido"),
    sex = if_else(sex==0, "0: Feminino", "1: Masculino"), 
    anaemia = if_else(anaemia==0, "0: Não", "1: Sim"), 
    diabetes = if_else(diabetes==0, "0: Não", "1: Sim"), 
    high_blood_pressure = if_else(high_blood_pressure==0, "0: Não", "1: Sim"), 
    smoking = if_else(smoking==0, "0: Não", "1: Sim")
  ) %>% 
  select(-all_of(numeric_var)) %>% 
  pivot_longer(!death_event) %>% 
  mutate(
    name = case_when(
      name=="anaemia" ~ "Anemia",
      name=="diabetes" ~ "Diabetes",
      name=="high_blood_pressure" ~ "Pressão Alta",
      name=="smoking" ~ "Fumante",
      name=="sex" ~ "Sexo",
      TRUE ~ "NA"
    )
  ) %>% 
  group_by(name, value, death_event) %>% 
  mutate(total_target = n()) %>% 
  group_by(name, value) %>% 
  mutate(total_geral = n()) %>% 
  ungroup() %>% 
  distinct_all() %>% 
  pivot_wider(names_from = "death_event", values_from = "total_target") %>%
  mutate(paciente_total = "Base Completa") %>% 
  pivot_wider(names_from = "paciente_total", values_from = "total_geral") %>% 
  arrange(name, value) %>% 
  `colnames<-`(c("Variável", "Categoria", "Total de Paciente Falecido", "Total de Paciente Sobrevivente", "Total de Pacientes Geral")) %>% 
  flextable() %>% 
  theme_box() %>% 
  autofit() %>% 
  bg(j = 1:5, bg = "white", part = "all") %>% 
  merge_v(j = 1) %>% 
  align(j = 3:5, align = "center", part = "all") %>% 
  save_as_image("output/var_cat.jpeg")

# Variáveis Numéricas
df %>% 
  select(all_of(numeric_var), death_event) %>% 
  mutate(death_event = ifelse(death_event==0, "Paciente Sobrevivente", "Paciente Falecido")) %>% 
  pivot_longer(!death_event) %>% 
  mutate(
    name = case_when(
      name=="age" ~ "Idade",
      name=="creatinine_phosphokinase" ~ "Creatina\nFosfoquinase",
      name=="ejection_fraction" ~ "Fração de Ejeção",
      name=="serum_creatinine" ~ "Creatinina",
      name=="serum_sodium" ~ "Sódio",
      name=="platelets" ~ "Plaquetas",
      TRUE ~ "NA"
    )
  ) %>% 
  group_by(name, death_event) %>% 
  mutate(
    media_target = mean(value),
    mediana_target = median(value),
    devpad_target = sd(value)
  ) %>% 
  group_by(name) %>% 
  mutate(
    media = mean(value),
    mediana = median(value),
    devpad = sd(value)
  ) %>% 
  ungroup() %>% 
  select(-value) %>% 
  distinct_all() %>% 
  select(name, death_event, everything()) %>% 
  arrange(name, death_event) %>% 
  mutate_at(3:8, ~ .x %>% round(2) %>% as.character() %>% str_replace_all("\\.", ",")) %>% 
  `colnames<-`(
    c("Variável", "Classe: Classificação\ndo Paciente", 
      paste( 
        rep(c("Média", "Mediana", "Desvio Padrão"), 2), 
        rep(c("\npor Classe", "\nTotal Pacientes"), each = 3) 
      )
    )) %>% 
  flextable() %>% 
  theme_box() %>% 
  autofit() %>%
  bg(j = 1:8, bg = "white", part = "all") %>% 
  merge_v(j = 1:8) %>% 
  align(j = 3:8, align = "center", part = "all") %>% 
  save_as_image("output/var_num.jpeg")

# Correlação --------------------------------------------------------------

ggsave(
  ggcorr(df[-12], label = TRUE, label_alpha = TRUE, low = "steelblue", mid = "white", high = "darkred"),
  filename = "output/corr.png", dpi = "retina", width = 15, height = 7
)

# -------------------------------------------------------------------------
