#--------------------------------------------------------------------------------------------------
# BESCHRIJVING 
# run models over all versions of the abt
#
# WIJZIGINGSHISTORIE
# 18-07-2018, Medewerker1; complete rewrite om alle data rondom modellen tidy te maken
# 24-11-2018, Medewerker4; omzetting character > factor ongedaan gemaakt i.v.m. iml
# 21-08-2020, Medewerker4; aanpassingen voor P010 run
#--------------------------------------------------------------------------------------------------

library(gbm)
library(tidyverse)
library(caret)
library(feather)
library(doParallel) ## backend of foreach

source("D:/DATA/SHARED/Analytics_Uitkeringsfraude/1.Code/10.Functies/init.R")
source("D:/DATA/SHARED/Analytics_Uitkeringsfraude/1.Code/10.Functies/modelleren.R")
setwd(root)
cores <- detectCores() - 1

#--------------------------------------------------------------------------------------------------
# data inlezen en split in train en test
#-------------------------------------------------------------------------------------------------
# featureselecties
# NB.: component-features laten we er alsnog uit. Deze gegevens blijken we niet historisch te kunnen reconstrueren, omdat het DWH vanwege
# de omvang alleen de actuele records overhaalt uit Socrates. We zien dus gegevens die achteraf gemuteerd kunnen zijn.
no_nzv <- read_feather(paste0(featureselectiesfolder, "no_nzv_",dtlaad, "_", label, ".feather")) %>% 
  pull(features) %>%
  grep("^(?!component).*", ., perl=TRUE, value=TRUE)
no_nzv_no_mc <- read_feather(paste0(featureselectiesfolder, "no_nzv_no_mc_", dtlaad, "_", label, ".feather")) %>% 
  pull(features) %>%
  grep("^(?!component).*", ., perl=TRUE, value=TRUE)

# ABT met traintest data. Onderscheid maken tussen traintest in kolom traintest.
set.seed(123) # resultaat reproduceerbaar maken
abt_traintest <- read_feather(paste0(abtfolder, "abt_basis_", dtlaad , "_", label, ".feather")) %>%
  filter(type %in% c("metlabel_traintest")) %>%
  mutate(type_traintest = ifelse(row_number() %in% createDataPartition(.$target_per, p = 0.80, list=FALSE)[,1], "train", "test"))

# maken train- en testset
abt_train <- filter(abt_traintest, type_traintest %in% "train") 
abt_test <- filter(abt_traintest, type_traintest %in% "test") 

rm(abt)

#--------------------------------------------------------------------------------------------------
# traincontrol (Gaan we ook in elke fase van trainen opnieuw herbruiken. Ook verplaasen naar aparte functie????)
#--------------------------------------------------------------------------------------------------
# we leggen vooraf de seeds vast voor elke iteratie in het train proces
# dat zijn er folds*repeats plus 1 voor het "final" model o.b.v. alle traindata
set.seed(111) # resultaat reproduceerbaar maken
cv_folds <- 9 
cv_repeats <- 3
seeds <- rerun(cv_folds * cv_repeats, sample.int(1000, 36)) # waar komt die 36 vandaan? cv_folds*cv_repeats?
seeds[[cv_folds * cv_repeats + 1]] <- sample.int(1000, 1) # seed voor "final" model binnen algoritme

# trainControl te gebruiken voor metric="ROC"
trControl_ROC <- trainControl(method = "repeatedcv", 
                              number = cv_folds, 
                              repeats = cv_repeats,
                              seeds = seeds, 
                              selectionFunction = "oneSE", # default: "best"
                              summaryFunction = twoClassSummary,
                              classProbs = TRUE)
# trainControl te gebruiken voor metric="Accuracy" of "Kappa".
trControl_Acc_Kappa <- trainControl(method = "repeatedcv", 
                                    number = cv_folds, 
                                    repeats = cv_repeats,
                                    seeds = seeds,
                                    selectionFunction = "oneSE")


#--------------------------------------------------------------------------------------------------
# Functies die we gebruiken bij modelleren
# Moeten worden verplaatst naar map waar functies opgeslagen worden zodat ze aangeroepen kunnen worden vanuit diverse scripts.
#--------------------------------------------------------------------------------------------------
grid_glm <- list(expand.grid(alpha = c(0, 1),
                             lambda = c(10^(-4:4))))
grid_gbm <- list(expand.grid(interaction.depth = c(1, 5, 9),
                             n.trees = c(1:20)*50,
                             shrinkage = c(0.1, 0.01, 0.001),
                             n.minobsinnode = c(10)))
grid_xgbTree <- list(expand.grid(nrounds = c(50, 100, 150, 200, 250, 300), # all default except nrounds
                                 max_depth = c(1, 2, 3),
                                 eta = c(0.3, 0.4),
                                 gamma = c(0),
                                 colsample_bytree = c(0.6, 0.8),
                                 min_child_weight = c(1),
                                 subsample = c(0.5, 0.75, 1)))
grid_rf <- list(expand.grid(mtry = c(10, 30, 100, 300)))
grid_rpart <- list(expand.grid(cp = c(0.1, 0.01, 0.001)))



#### Modelspecificaties aanmaken. Kruising tussen parameters met expand.grid().

modellen <- expand.grid(methode = c("glmnet", "gbm","xgbTree", "rf", 'rpart'),
                        metric = c("Accuracy"),
                        #modellen <- expand.grid(methode = c("rpart", "gbm", "xgbTree", "rf"), 
                        #                        metric = c("ROC", "Accuracy"),
                        featureselect = c("no_nzv_no_mc"), 
                        stringsAsFactors = FALSE) %>%
  as.tibble() %>%
  mutate(dtlaad = dtlaad,
         traindata = "abt_train",
         testdata = "abt_test",
         trControl = if_else(metric %in% "ROC", "trControl_ROC", "trControl_Acc_Kappa"),
         params = case_when(
           methode == "glmnet" ~ grid_glm,
           methode == "gbm" ~ grid_gbm,
           methode == "xgbTree" ~ grid_xgbTree,
           methode == "rf" ~ grid_rf,
           methode == "rpart" ~ grid_rpart,
           TRUE ~ list(NULL)))


# Trainen modellen.
train_model()

### Hieronder predictions en performance metrics toevoegen
modellen <- modellen %>% 
  mutate(prediction = map2(model, testdata, generate_prediction),
         metrics = map(prediction, generate_performance_metrics)
  ) 

### Metrics kunnen worden bekeken door:
modellen %>% select(methode, metric, featureselect, metrics) %>% unnest(metrics) %>% as.data.frame()

### wegschrijven modelresultaat
model_location <-  paste0(modelfolder, dtlaad, "_",label, "/kandidaat_modellen.rds")
dir.create(dirname(model_location), showWarnings = FALSE)
write_rds(modellen, model_location)

# Maken rapportage
rmarkdown::render(paste0(rapportfolder, "modelinformatie_kandidaat.Rmd"), 
                  params=list(modelfile=model_location),
                  output_dir = dirname(model_location),
                  output_file = paste0(tools::file_path_sans_ext(basename(model_location)), ".html"))
