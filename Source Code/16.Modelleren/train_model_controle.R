#--------------------------------------------------------------------------------------------------
# BESCHRIJVING 
# Deze code traint een "controlemodel"
# We gebruiken het type model en de modelparameters die als beste uit het reguliere train-testproces
# komen. Hiermee trainen we een nieuw model op data waar de controleset uitgehaald is (een set met
# aselect uitgevoerde onderzoeken) en bepalen de accuracy en hitrates van dit model op de
# controleset.
# NB.: deze training is dus zonder crossvalidatie en parametertuning.
#
# WIJZIGINGSHISTORIE
# 21-08-2020, Medewerker4: initiële versie
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
# data inlezen en split in train(+test)-controle en controle
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

# ABT laden en subsets maken
abt <- read_feather(paste0(abtfolder, "abt_basis_", dtlaad , "_", label, ".feather"))

abt_traintest <- abt %>%  filter(type %in% c("metlabel_traintest"))
# controleset
abt_controle <- abt %>% filter(type %in% c("metlabel_controle"))
# nu de "nieuwe" trainset voor het controlemodel, bestaande uit de originele traintest set minus de records in de controleset
ids_controle <- abt_controle %>% select(persoon_id, melding_om) %>% mutate(ind_controle = 1L)
abt_traincontr <- abt_traintest %>% left_join(ids_controle, by = c("persoon_id", "melding_om")) %>% filter(is.na(ind_controle)) %>% 
  mutate(type = "metlabel_traincontr") %>% select(-ind_controle)

rm(abt, abt_traintest)

#--------------------------------------------------------------------------------------------------
# traincontrol (hier dus geen crossvalidatie en tuning)
#--------------------------------------------------------------------------------------------------

# trainControl te gebruiken voor metric="ROC"
trControl_ROC <- trainControl(method = "none", 
                              summaryFunction = twoClassSummary,
                              classProbs = TRUE)
# trainControl te gebruiken voor metric="Accuracy" of "Kappa".
trControl_Acc_Kappa <- trainControl(method = "none")


#### Kies settings van het finale model
modellen <- read_rds(paste0(modelfolder, dtlaad, "_",label, "/finale_model.rds")) %>%
  select(methode, metric, featureselect, dtlaad, trControl, params) %>%
  mutate(traindata = "abt_traincontr",
         testdata = "abt_controle")

## NB: als het finale model nog niet gemaakt is, kies dan het best presterende 
## kandidaatmodel (waaruit het finale model gemaakt gaat worden)
# modellen <- read_rds(paste0(modelfolder, dtlaad, "_",label, "/kandidaat_modellen.rds")) %>%
#   mutate(params = map(model, getElement, "bestTune")) %>%
#   select(methode, metric, featureselect, dtlaad, trControl, params) %>%
#   filter(methode == "gbm") %>%   # <<<< VUL HIER DE CRITERIA VOOR HET BESTE MODEL IN
#   mutate(traindata = "abt_traincontr",
#          testdata = "abt_controle")


# Trainen modellen.
train_model()

### Hieronder predictions en performance metrics toevoegen
modellen <- modellen %>%
  mutate(prediction = map2(model, testdata, generate_prediction),
         metrics = try(map(prediction, generate_performance_metrics))
  ) 

### wegschrijven modelresultaat
model_location <-  paste0(modelfolder, dtlaad, "_",label, "/controle_model.rds")
dir.create(dirname(model_location), showWarnings = FALSE)
write_rds(modellen, model_location)

### Metrics kunnen worden bekeken door:
#modellen <- read_rds(model_location)
modellen %>% select(methode, metric, featureselect, metrics) %>% unnest(metrics) %>% as.data.frame()

# Maken rapportage
rmarkdown::render(paste0(rapportfolder, "modelinformatie_controlemodel.Rmd"), 
                  params = list(modelfile = model_location, 
                                n_bins_hist = 7), # het aantal bins in histogram voor onderscheidingsvermogen
                  output_dir = dirname(model_location),
                  output_file = paste0(tools::file_path_sans_ext(basename(model_location)), ".html"))
