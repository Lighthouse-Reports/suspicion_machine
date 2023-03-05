#--------------------------------------------------------------------------------------------------
# BESCHRIJVING 
# Finale model trainen met alle beschikbare data
#
# WIJZIGINGSHISTORIE
# 27-08-2018, Medewerker1; complete rewrite om alle data rondom modellen tidy te maken
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
# data inlezen
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

# ABT met alle labeled cases
set.seed(123) # resultaat reproduceerbaar maken
abt_labeled <- read_feather(paste0(abtfolder, "abt_basis_", dtlaad , "_", label, ".feather")) %>%
  filter(type %in% c("metlabel_traintest"))

#--------------------------------------------------------------------------------------------------
# traincontrol
#--------------------------------------------------------------------------------------------------

# trainControl te gebruiken voor metric="ROC"
trControl_ROC <- trainControl(method = "none", 
                              summaryFunction = twoClassSummary,
                              classProbs = TRUE)
# trainControl te gebruiken voor metric="Accuracy" of "Kappa".
trControl_Acc_Kappa <- trainControl(method = "none")


#### Kies settings van best presterende model
modellen <- read_rds(paste0(modelfolder, dtlaad, "_",label, "/kandidaat_modellen.rds")) %>%
  mutate(params = map(model, getElement, "bestTune")) %>%
  select(methode, metric, featureselect, dtlaad, trControl, params) %>%
  filter(methode == "gbm") %>%  # <<<< VUL HIER DE CRITERIA VOOR HET BESTE MODEL IN
  mutate(traindata = "abt_labeled")


# Trainen modellen.
train_model()


### wegschrijven modelresultaat
model_location <-  paste0(modelfolder, dtlaad, "_",label, "/finale_model.rds")
dir.create(dirname(model_location), showWarnings = FALSE)
write_rds(modellen, model_location)
