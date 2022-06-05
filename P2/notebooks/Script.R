library(tidyverse)
library(lubridate)

conditions_scenario01 <- read_csv(url("https://raw.githubusercontent.com/santanche/lab2learn/master/data/synthea/scenario01/csv/conditions.csv"))

encounters_scenario01 <- read_csv(url("https://raw.githubusercontent.com/santanche/lab2learn/master/data/synthea/scenario01/csv/encounters.csv"))

conditions_scenario02 <- read_csv(url("https://raw.githubusercontent.com/santanche/lab2learn/master/data/synthea/scenario02/csv/conditions.csv"))

encounters_scenario02 <- read_csv(url("https://raw.githubusercontent.com/santanche/lab2learn/master/data/synthea/scenario02/csv/encounters.csv"))

patients_scenario01 <- read_csv(url("https://raw.githubusercontent.com/santanche/lab2learn/master/data/synthea/scenario01/csv/patients.csv"))

patients_scenario02 <- read_csv(url("https://raw.githubusercontent.com/santanche/lab2learn/master/data/synthea/scenario02/csv/patients.csv"))

allergies_scenario01 <- read_csv(url("https://raw.githubusercontent.com/santanche/lab2learn/master/data/synthea/scenario01/csv/allergies.csv"))

allergies_scenario02 <- read_csv(url("https://raw.githubusercontent.com/santanche/lab2learn/master/data/synthea/scenario02/csv/allergies.csv"))

# Bases de dados em cada um dos cenários com as variáveis que serão utilizadas
base_scenario01 <- encounters_scenario01 %>%
  separate(START, c("START", "HOUR"), " ") %>%
  mutate(CODE = as.character(CODE),
         START = ymd(START)) %>%
  filter(CODE %in% c("22298006", "410429000"), ENCOUNTERCLASS == "emergency") %>%
  left_join(patients_scenario01, by = c("PATIENT" = "Id")) %>%
  left_join(allergies_scenario01 %>%
              select(PATIENT, TYPE, CATEGORY, SEVERITY1), by = "PATIENT") %>%
  select(PATIENT, "ENCOUNTER" = Id, CODE, DESCRIPTION, START, DEATHDATE,
         ETHNICITY, GENDER, HEALTHCARE_EXPENSES, HEALTHCARE_COVERAGE,
         TYPE, CATEGORY, SEVERITY1) 

base_scenario02 <- encounters_scenario02 %>%
  separate(START, c("START", "HOUR"), " ") %>%
  mutate(CODE = as.character(CODE),
         START = ymd(START)) %>%
  filter(CODE %in% c("22298006", "410429000"), ENCOUNTERCLASS == "emergency") %>%
  left_join(patients_scenario02, by = c("PATIENT" = "Id")) %>%
  left_join(allergies_scenario01 %>%
              select(PATIENT, TYPE, CATEGORY, SEVERITY1), by = "PATIENT") %>%
  select(PATIENT, "ENCOUNTER" = Id, CODE, DESCRIPTION, START, DEATHDATE,
         ETHNICITY, GENDER, HEALTHCARE_EXPENSES, HEALTHCARE_COVERAGE,
         TYPE, CATEGORY, SEVERITY1) 

# Resumo das bases de dados
# summary(base_scenario01)
# summary(base_scenario02)

# Criação de variáveis para modelagem
base_scenario01 <- base_scenario01 %>%
  mutate(DIFF = DEATHDATE-START,
         DEATH = if_else(DIFF <= 365, 1, 0),
         DEATH = if_else(is.na(DEATH), 0, DEATH),
         ALLERGIES = if_else(is.na(TYPE), 0, 1),
         DEATH = as.factor(DEATH),
         ALLERGIES = as.factor(ALLERGIES)) %>%
  group_by(PATIENT) %>%
  mutate(N_ENCOUNTERS = n_distinct(ENCOUNTER)) %>%
  ungroup()

base_scenario02 <- base_scenario02 %>%
  mutate(DIFF = DEATHDATE-START,
         DEATH = if_else(DIFF <= 365, 1, 0),
         DEATH = if_else(is.na(DEATH), 0, DEATH),
         ALLERGIES = if_else(is.na(TYPE), 0, 1),
         DEATH = as.factor(DEATH),
         ALLERGIES = as.factor(ALLERGIES)) %>%
  group_by(PATIENT) %>%
  mutate(N_ENCOUNTERS = n_distinct(ENCOUNTER)) %>%
  ungroup()

# CASO 1: Modelo treinado no scenario01 e testado no scenario02
set.seed(001)

# Amostra de treino e validação no scenario01
n_scenario01 <- round(nrow(base_scenario01)*0.7)
sample_train_scenario01 <- sample(nrow(base_scenario01), 
                                  size = n_scenario01, replace = FALSE)

train_scenario01 <- base_scenario01[sample_train_scenario01,] %>%
  select(DESCRIPTION, ETHNICITY, GENDER, HEALTHCARE_EXPENSES,
         HEALTHCARE_COVERAGE, ALLERGIES, N_ENCOUNTERS, DEATH)

test_scenario01 <- base_scenario01[-sample_train_scenario01,] %>%
  select(DESCRIPTION, ETHNICITY, GENDER, HEALTHCARE_EXPENSES,
         HEALTHCARE_COVERAGE, ALLERGIES, N_ENCOUNTERS, DEATH)

# CASO 2: Modelo treinado no scenario02 e testado no scenario01
set.seed(002)

# Amostra de treino e validação no scenario01
n_scenario02 <- round(nrow(base_scenario02)*0.7)
sample_train_scenario02 <- sample(nrow(base_scenario02), 
                                  size = n_scenario02, replace = FALSE)

train_scenario02 <- base_scenario02[sample_train_scenario02,] %>%
  select(DESCRIPTION, ETHNICITY, GENDER, HEALTHCARE_EXPENSES,
         HEALTHCARE_COVERAGE, N_ENCOUNTERS, DEATH)

test_scenario02 <- base_scenario02[-sample_train_scenario02,] %>%
  select(DESCRIPTION, ETHNICITY, GENDER, HEALTHCARE_EXPENSES,
         HEALTHCARE_COVERAGE, N_ENCOUNTERS, DEATH)

library(pROC)
library(caret)
# CASO 1
# Ajuste do modelo e seleção de variáveis via stepwise
fit_scenario01 <- glm(DEATH ~ ., 
                      data = train_scenario01,
                      family = "binomial")

fit_scenario01 <- step(fit_scenario01, direction = "both")

# Características do modelo final
summary(fit_scenario01)
predicted_scenario01 <- predict(fit_scenario01, test_scenario01,
                                type="response")

plot(roc(test_scenario01$DEATH, predicted_scenario01),
     ylab = "Sensitividade", xlab = "Especificidade",
     main = "CASO 1", cex.main=1)

# Performance do modelo no conjunto de validação
auc(test_scenario01$DEATH, predicted_scenario01)
plot(roc(test_scenario01$DEATH, predicted_scenario01))

# CASO 2
# Ajuste do modelo e seleção de variáveis via stepwise
fit_scenario02 <- glm(DEATH ~ ., 
                      data = train_scenario02,
                      family = "binomial") #diferença: scenario02 não temos nenhum paciente com alergias, então não conseguimos incluir essa covariável no modelo

fit_scenario02 <- step(fit_scenario02, direction = "both")

plot(roc(test_scenario02$DEATH, predicted_scenario02),
     ylab = "Sensitividade", xlab = "Especificidade",
     main = "CASO 2", cex.main=1)

# Características do modelo final
summary(fit_scenario02)
predicted_scenario02 <- predict(fit_scenario02, test_scenario02,
                                type="response")

# Performance do modelo no conjunto de validação
auc(test_scenario02$DEATH, predicted_scenario02)
plot(roc(test_scenario02$DEATH, predicted_scenario02))

# CASO 1
# Teste do modelo no scenario02
predicted_scenario01_02 <- predict(fit_scenario01, base_scenario02,
                                   type="response")

# auc(base_scenario02$DEATH, predicted_scenario01_02)
plot(roc(base_scenario02$DEATH, predicted_scenario01_02),
     ylab = "Sensitividade", xlab = "Especificidade",
     main = "CASO 1", cex.main=1)

predicted_scenario01_02 <- data.frame(predict = predict(fit_scenario01,
                                                        base_scenario02,
                                                        type="response"))

predicted_scenario01_02 <- predicted_scenario01_02 %>% 
  mutate(predict = as.factor(ifelse(predict > .5, "1", "0")))

confusionMatrix(base_scenario02$DEATH, 
                predicted_scenario01_02$predict)

# CASO 2
# Teste do modelo no scenario01
predicted_scenario02_01 <- predict(fit_scenario02, base_scenario01,
                                   type="response")

# auc(base_scenario01$DEATH, predicted_scenario02_01)
plot(roc(base_scenario01$DEATH, predicted_scenario02_01),
     ylab = "Sensitividade", xlab = "Especificidade",
     main = "CASO 2", cex.main=1)

predicted_scenario02_01 <- data.frame(predict = predict(fit_scenario02,
                                                        base_scenario01,
                                                        type="response"))

predicted_scenario02_01 <- predicted_scenario02_01 %>% 
  mutate(predict = as.factor(ifelse(predict > .5, "1", "0")))

confusionMatrix(base_scenario01$DEATH, 
                predicted_scenario02_01$predict)