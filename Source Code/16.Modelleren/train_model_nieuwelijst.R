#--------------------------------------------------------------------------------------------------
# BESCHRIJVING 
# Nieuwe lijst: (finale) model trainen met alle beschikbare data, met model en parameters van fullrun
#
# WIJZIGINGSHISTORIE
# 02-12-2016, Medewerker9, created original version
# 05-12-2016, Medewerker9, fixed some errors
# 07-12-2016: Medewerker3, voeg functies toe om uitkomsten uit te rekenen 
# 14-12-2016: Medewerker3, reken belangrijke variables uit
# 04-01-2014: Medewerker3, Voeg tuning parameters toe voor gbm
# 23-01-2014: Medewerker3, voeg random seeds toe en tune het model op basis van het specificity ipv accuratie.
# 24-01-2014: Medewerker3, voeg random seeds toe en tune het model op basis van het sensitivity ipv accuratie.
# 25-01-2014: Medewerker3, voeg random seeds toe en tune het model op basis van het area under the ROC curve ipv accuratie.
# 26-01-2014: Medewerker3, verzamel resultaten van de verschillende tuning sessies
# 17-02-2017, Medewerker3, modellen draaien over nieuwe abt
# 20-02-2017, Medewerker3, near_zero_variance passen
# 22-02-2017, Medewerker3, modellen klaarzetten voor de pilot fase
# 23-02-2017, Medewerker3, tuning grids aanpassen en resultaten tussendoor laten opslaan
# 24-02-2017, Medewerker3, tuning grids aanpassen voor rf, svm toevoegen en matrix best toevoegen
# 03-03-2017, Medewerker3, modellen draaien zonder fraude features en final model trainen
# 10-01-2018, Medewerker3, init
# 11-01-2018, Medewerker3; input train function naar een expressie
# 18-07-2018, Medewerker1; complete rewrite om alle data rondom modellen tidy te maken
# 23-07-2019, Medewerker2; toevoegen chunks topkandidaat_model en finalemodel
# 29-09-2020, Medewerker4; aanpassingen voor P010 nieuwe lijst run
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

abt_naam <- paste0("abt_basis_", dtlaad)
label_refmodel <- "2020-07-23_fullrun_p010" # Pas aan aan beste model vorige run


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


#--------------------------------------------------------------------------------------------------
# (Her-)train een model met alle labeled data en met de settings van het finale model van 
# de referentie full run (geen testset dus, want we doen geen modelselectie)
#--------------------------------------------------------------------------------------------------

#### Kies settings van het finale model van de referentie full run
modellen <- read_rds(paste0(modelfolder, label_refmodel, "/finale_model.rds")) %>% 
  select(methode, metric, featureselect, dtlaad, trControl, params) %>%
  mutate(traindata = "abt_labeled")

# dt-laad updaten naar dtlaad van huidige run ipv run van het vorige beste model
modellen$dtlaad <- dtlaad

# Trainen modellen.
train_model()

### wegschrijven modelresultaat
model_location <- paste0(modelfolder, dtlaad, "_", label, "/finale_model.rds")
dir.create(dirname(model_location), showWarnings = FALSE)
write_rds(modellen, model_location)


