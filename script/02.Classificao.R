
# Pacotes -----------------------------------------------------------------

library(bonsai)
library(discrim)
library(tidymodels)

# Data --------------------------------------------------------------------

source("script/00.Dados.R")

###########################################################################
# MODELAGEM ---------------------------------------------------------------
###########################################################################

df_pp <- df %>%
  mutate(
    death_event = factor(death_event, levels = c(0, 1), labels = c("sobreviveu", "faleceu"))
  )

# split
set.seed(seed)
initial_split <- initial_split(df_pp, prop = 4/5, strata = 'death_event')
df_pp_train <- training(initial_split)
df_pp_test <- testing(initial_split)

# Recipe Baseline
recipe_base <- recipe(death_event ~ ., data = df_pp_train) %>% 
  step_bin2factor(sex, anaemia, diabetes, high_blood_pressure)
  
juice(prep(recipe_base))

# Recipe Baseline Normalizada
recipe_base_norm <- recipe(death_event ~ ., data = df_pp_train) %>%
  step_normalize(all_numeric())

# Recipe SmoteNC
recipe_smote <- recipe(death_event ~ ., data = df_pp_train) %>% 
  step_bin2factor(sex, anaemia, diabetes, high_blood_pressure) %>%
  themis::step_smotenc(death_event)

# Recipe SmoteNC Normalizada
recipe_smote_norm <- recipe(death_event ~ ., data = df_pp_train) %>%
  themis::step_smotenc(death_event) %>%
  step_normalize(all_numeric())

# Recipe UnderSampling
recipe_down <- recipe(death_event ~ ., data = df_pp_train) %>% 
  step_bin2factor(sex, anaemia, diabetes, high_blood_pressure) %>%
  themis::step_downsample(death_event) 

# Recipe UnderSampling Normalizada
recipe_down_norm <- recipe(death_event ~ ., data = df_pp_train) %>%
  themis::step_downsample(death_event) %>%
  step_normalize(all_numeric())

# Modelos -----------------------------------------------------------------

# Arv
model_arv <- 
  decision_tree(
    cost_complexity = tune(),
    min_n = tune(),
    tree_depth = tune()
  ) %>% 
  set_mode("classification") %>% 
  set_engine("rpart")

# GLM
model_reglog <- 
  logistic_reg(
    penalty = tune(),
    mixture = tune()
  ) %>% 
  set_mode("classification") %>% 
  set_engine("glmnet")

# SVM Linear
model_svml <- 
  svm_linear(
    cost = tune()
  ) %>% 
  set_mode("classification") %>% 
  set_engine("kernlab")

# SVM Radial
model_svmr <- 
  svm_rbf(
    cost = tune(),
    rbf_sigma = tune()
  ) %>% 
  set_mode("classification") %>% 
  set_engine("kernlab")

# RF
model_rf <- 
  rand_forest(
    mtry = tune(),
    trees = tune(),
    min_n = tune()
  ) %>% 
  set_mode("classification") %>% 
  set_engine("randomForest")

# KNN
model_knn <- 
  nearest_neighbor(
    neighbors = tune(),
    weight_func = tune(),
    dist_power = tune()
  ) %>% 
  set_mode("classification") %>% 
  set_engine("kknn")

# Naive Bayes
model_nb <-
  naive_Bayes(
    smoothness = tune(),
    Laplace = tune()
  ) %>%
  set_mode("classification") %>%
  set_engine("klaR")

# XGBoost
model_xgb <-
  boost_tree(
    min_n = tune(),
    mtry = tune(),
    trees = tune(),
    tree_depth = tune(),
    learn_rate = tune(),
    loss_reduction = tune(),
    sample_size = tune()
  ) %>%
  set_mode("classification") %>%
  set_engine("xgboost")

# LightGBM
model_lgbm <-
  boost_tree(
    mtry = tune(),
    trees = tune(),
    tree_depth = tune(),
    min_n = tune(),
    learn_rate = tune(),
    loss_reduction = tune()
  ) %>% 
  set_mode("classification") %>% 
  set_engine("lightgbm")

# MLP
model_mlp <- mlp(
  hidden_units = tune(),
  penalty = tune(),
  epochs = tune(),
  activation = tune()
) %>%  
  set_mode("classification") %>% 
  set_engine("brulee") 

#  Workflow ---------------------------------------------------------------

wf <- bind_rows(
  workflow_set(
    preproc = list(
      base = recipe_base, baseN = recipe_base_norm,
      smote = recipe_smote, smoteN = recipe_smote_norm,
      down = recipe_down, downN = recipe_down_norm
    ), 
    models = list(
      arv = model_arv, rf = model_rf, knn = model_knn, nb = model_nb, svmL = model_svml, svmR = model_svmr
    )
  ),
  workflow_set(
    preproc = list(baseN = recipe_base_norm, smoteN = recipe_smote_norm, downN = recipe_down_norm), 
    models = list(reglog = model_reglog, lgbm = model_lgbm, xgb = model_xgb, mlp = model_mlp)
  )
)

#  Tunagem de Hiperparâmetros ---------------------------------------------

# Fold
set.seed(seed)
resamples <- vfold_cv(df_pp_train, v = 10, strata = 'death_event')

tune_grid <- wf %>%
  workflow_map(
    seed = seed,
    resamples = resamples,
    grid = 100,
    control = control_grid(
      verbose = TRUE, allow_par = FALSE, save_workflow = TRUE, save_pred = TRUE, parallel_over = "everything"
    )
  )

# Desempenho
rank <- tune_grid %>% 
  rank_results(rank_metric = "roc_auc", select_best = TRUE) %>% 
  filter(.metric == "roc_auc"); rank

autoplot(tune_grid)
autoplot(tune_grid, metric = "roc_auc", id = rank[1,1][[1]])
collect_metrics(tune_grid)

# Avaliando ---------------------------------------------------------------

# Tibble para armazenar dados dos modelos
model_output <- tibble()

cont = 0
for (rank_model in rank$wflow_id) {
  best_hiperparams <- extract_workflow_set_result(tune_grid, rank_model) %>% select_best()
  wf_win <- extract_workflow(tune_grid, rank_model) %>% finalize_workflow(best_hiperparams)
  last_fit <- wf_win %>% last_fit(split = initial_split)
  
  # Desempenho
  df_result <- collect_predictions(last_fit)
  
  # Matriz de Confusão
  cm <- conf_mat(df_result, truth = death_event, estimate = .pred_class)
  
  model_output <- 
    model_output %>% bind_rows({
      tibble(
        model = rank_model,
        model_desc = list(extract_fit_parsnip(last_fit)), # dados do modelo
        best_params = list(best_hiperparams), # melhores parâmetros
        tune_plot = list(autoplot(tune_grid, metric = "roc_auc", id = rank_model)), # plot dos dados de tuning
        metricas_desempenho = list( # avaliação/desempenho
          cm %>% 
            summary() %>% 
            dplyr::select(.metric, .estimate) %>%
            bind_rows({
              df_result %>% roc_auc(truth = death_event, .pred_sobreviveu) %>% dplyr::select(.metric, .estimate)
            }) %>% 
            filter(.metric %in% c("accuracy", "sens", "spec", "kap", "precision", "recall", "f_meas", "roc_auc"))
        ),
        cm_plot = list(autoplot(cm, type = "heatmap")), # matriz de confusão
        roc_plot = list(roc_curve(df_result, truth = death_event, .pred_sobreviveu) %>% autoplot()), # curva ROC
        var_import = list( try( vip::vip(extract_fit_parsnip(last_fit$.workflow[[1]])) ) ) # importância das variáveis
      )
    })
  
  cont = cont+1
  cat("\nFalta", nrow(rank)-cont)
}

# Tabela
model_output %>%
  dplyr::select(model, metricas_desempenho) %>%
  unnest(cols = c(metricas_desempenho)) %>%
  pivot_wider(names_from = .metric, values_from = .estimate) %>%
  separate(col = "model", into = c("cenario", "modelo")) %>%
  mutate(
    rank = (sens+spec*2)/3,
    cenario = case_when(
      cenario=="smote" ~ "Smote-NC",
      cenario=="smoteN" ~ "Smote-NC (N)",
      cenario=="down" ~ "UnderSampling",
      cenario=="downN" ~ "UnderSampling (N)",
      cenario=="base" ~ "Baseline",
      cenario=="baseN" ~ "Baseline (N)",
      TRUE ~ "A"
    ),
    modelo = case_when(
      modelo=="svmR" ~ "SVM Radial",
      modelo=="svmL" ~ "SVM Linear",
      modelo=="nb" ~ "Naive Bayes",
      modelo=="rf" ~ "Random Forest",
      modelo=="reglog" ~ "Regressão Logística",
      modelo=="lgbm" ~ "LightGBM",
      modelo=="xgb" ~ "XGBoost",
      modelo=="knn" ~ "KNN",
      modelo=="arv" ~ "Árv. Decisão",
      modelo=="mlp" ~ "Rede Neural",
      TRUE ~ "A"
    )
  ) %>%
  arrange(-rank) %>%
  mutate_at(3:11, ~ ifelse( .x==max(.x), paste0(round(.x, 3), "*"), round(.x, 3) )) %>%
  head(10) %>%
  dplyr::select(cenario, modelo, accuracy, precision, sens, spec, f_meas, roc_auc, kap) %>%
  `colnames<-`(
    c("Cenário", "Modelo", "Acurácia", "Precisão", "Sensibilidade", "Especificidade", "F1 Score", "ROC AUC", "Kappa")
  ) %>%
  flextable() %>%
  theme_box() %>%
  autofit() %>%
  bg(j = 1:9, bg = "white", part = "all") %>%
  merge_v(j = 2) %>%
  align(j = 3:9, align = "center", part = "all") %>%
  add_footer_lines(values = "*: top performance\n(N): Normalizado")

# -------------------------------------------------------------------------
