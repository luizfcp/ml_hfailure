
# Pacotes -----------------------------------------------------------------

library(arules)

# Data --------------------------------------------------------------------

source("script/00.Dados.R")

# Data - Discretização ----------------------------------------------------

df_discret <- df_catnum %>% 
  mutate(
    age = ifelse(age<=64, "40..64", "65..95"),
    creatinine_phosphokinase = ifelse(creatinine_phosphokinase<=120, "Normal", "Anormal"),
    ejection_fraction = case_when(
      ejection_fraction>=55 & ejection_fraction<=75 ~ "Normal",
      TRUE ~ "Anormal"
    ),
    platelets = case_when(
      platelets>=150000 & platelets<=400000 ~ "Normal",
      TRUE ~ "Anormal"
    ),
    serum_creatinine = case_when(
      sex=="F" & serum_creatinine>=0.6 & serum_creatinine<=1.1 ~ "Normal",
      sex=="M" & serum_creatinine>=0.7 & serum_creatinine<=1.3 ~ "Normal",
      TRUE ~ "Anormal"
    ),
    serum_sodium = case_when(
      serum_sodium>=135 & serum_sodium<=145 ~ "Normal",
      TRUE ~ "Anormal"
    )
  ) %>% 
  `colnames<-`(
    c("idade", "anemia", "creatina_fosfoquinase", "diabetes", "fracao_ejecao", "pressao_alta", 
      "plaquetas", "creatinina", "sodio", "sexo", "fumante", "evento_morte")
  ); df_discret

###########################################################################
# Regras de Associação ----------------------------------------------------
###########################################################################

table_ra <- function(data_ra) {
  data_ra %>% 
    mutate_at(6, ~ round(.x, 3) %>% str_replace_all("\\.", ",")) %>%
    mutate_at(3:5, ~ scales::percent(.x, accuracy = 0.01, decimal.mark = ",")) %>%
    select(antec, conseq, coverage, support, confidence, lift) %>%
    `colnames<-`(c("Antecedente", "Consequente", "Suporte Antecedente", "Suporte Regra", "Confiança Regra", "Lift")) %>%
    select(-`Suporte Antecedente`) %>% 
    flextable() %>%
    theme_box() %>%
    autofit() %>%
    bg(j = 1:5, bg = "white", part = "all") %>%
    merge_v(j = 2) %>%
    align(j = 3:5, align = "center", part = "all") %>%
    align(align = "center", part = "header")
}

# RAs ---------------------------------------------------------------------

df_ra <- as(df_discret, "transactions")
inspect(df_ra)

ra_freq <- itemFrequency(df_ra, type='absolute')

plot_freq_ra <- tibble(
  ra = names(ra_freq), freq = ra_freq
) %>%
  arrange(-freq) %>%
  head(10) %>%
  ggplot(aes(x = reorder(ra, freq), y = freq)) +
  geom_col() + #fill = "blue"
  coord_flip() +
  geom_label(aes(label = freq)) +
  labs(x = "", y = "Frequência") +
  theme_minimal(); plot_freq_ra
ggsave(plot_freq_ra, filename = "output/ra_freq.jpeg", dpi = "retina")

# Mineração de regras de associação
regras <- apriori(df_ra, parameter = list(supp=0.033, conf = 0.1, minlen = 2), control = list(verbose=TRUE))

# Regras encontradas
ra <- as(regras, "data.frame") %>% 
  as_tibble() %>% 
  arrange(-lift) %>% 
  separate(col = rules, into = c("antec", "conseq"), sep = " => ") %>%
  mutate(conseq = paste("=>", conseq)); ra

ra_conseq_faleceu <- ra %>% filter(str_detect(conseq, " [:punct:]evento_morte=faleceu"))
ra_conseq_sobrevi <- ra %>% filter(str_detect(conseq, " [:punct:]evento_morte=sobreviveu"))

# Regras Gerais - Maior e Menor Lift
regras_mine <- ra_conseq_sobrevi %>% arrange(lift) %>% head(1) %>% 
  bind_rows({ ra_conseq_sobrevi %>% arrange(-lift) %>% head(1) }) %>%
  bind_rows({ ra_conseq_faleceu %>% arrange(lift) %>% head(1) }) %>%
  bind_rows({ ra_conseq_faleceu %>% arrange(-lift) %>% head(1) }); regras_mine

regras_mine %>% 
  mutate(antec = antec %>% str_replace_all(",", ", ")) %>% 
  table_ra() %>% 
  save_as_image("output/ras.jpeg")

# Agrupando Faleceu - Maior Lift

regras_mine[4,]$antec

## 1
regras_mine_faleceu_malift %>% 
  ## 2
  bind_rows({ 
    ra_conseq_faleceu %>% filter(str_detect(antec, "\\{idade=65..95,creatina_fosfoquinase=Anormal\\}")) 
  }) %>% 
  bind_rows({ 
    ra_conseq_faleceu %>% filter(str_detect(antec, "\\{idade=65..95,pressao_alta=Sim\\}")) 
  }) %>% 
  bind_rows({ 
    ra_conseq_faleceu %>% filter(str_detect(antec, "\\{idade=65..95,creatinina=Anormal\\}")) 
  }) %>% 
  bind_rows({ 
    ra_conseq_faleceu %>% filter(str_detect(antec, "\\{creatina_fosfoquinase=Anormal,pressao_alta=Sim\\}")) 
  }) %>% 
  bind_rows({ 
    ra_conseq_faleceu %>% filter(str_detect(antec, "\\{creatina_fosfoquinase=Anormal,creatinina=Anormal\\}")) 
  }) %>% 
  bind_rows({ 
    ra_conseq_faleceu %>% filter(str_detect(antec, "\\{pressao_alta=Sim,creatinina=Anormal\\}")) 
  }) %>% 
  ## 3
  bind_rows({ 
    ra_conseq_faleceu %>% filter(str_detect(antec, "\\{idade=65..95,creatina_fosfoquinase=Anormal,pressao_alta=Sim\\}")) 
  }) %>% 
  bind_rows({ 
    ra_conseq_faleceu %>% filter(str_detect(antec, "\\{idade=65..95,creatina_fosfoquinase=Anormal,creatinina=Anormal\\}")) 
  }) %>% 
  bind_rows({ 
    ra_conseq_faleceu %>% filter(str_detect(antec, "\\{idade=65..95,pressao_alta=Sim,creatinina=Anormal\\}")) 
  }) %>% 
  bind_rows({ 
    ra_conseq_faleceu %>% filter(str_detect(antec, "\\{creatina_fosfoquinase=Anormal,pressao_alta=Sim,creatinina=Anormal\\}")) 
  }) %>% 
  mutate(antec = antec %>% str_replace_all(",", ", ")) %>% 
  table_ra() %>% 
  save_as_image("output/ras_regra4.jpeg")

# -------------------------------------------------------------------------
