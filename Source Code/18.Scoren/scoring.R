#--------------------------------------------------------------------------------------------------
# BESCHRIJVING 
# Opleveren finale lijst, aflopend gesorteerd op risicoscore
#
# WIJZIGINGSHISTORIE
# 06-03-2017, Medewerker1 & Medewerker3, Initiele versie
# 26-07-2017, Medewerker3, abt met probs wegschrijven voor verdere analyse
# 08-11-2017, Medewerker4, script naar iteratie3 overgezet
# 03-01-2018, Medewerker3, script aanpassen aan het nieuwe process
# 10-01-2018, Medewerker3; init
# 11-07-2018, Medewerker4; migratie naar WOB server
# 27-08-2018, Medewerker4; aanpassingen voor final run pilot 2
# 23-08-2019, Medewerker4; aanpassingen voor nieuwe lijst run aug 2019
# 03-09-2020, Medewerker4: code opgeschoond voor P010 run
#--------------------------------------------------------------------------------------------------


#--------------------------------------------------------------------------------------------------
# Initialise 
#--------------------------------------------------------------------------------------------------
library(tidyverse)
library(feather)
library(lubridate)

source("D:/DATA/SHARED/Analytics_Uitkeringsfraude/1.Code/10.Functies/init.R")

setwd(root)

## set om te scoren selecteren
abt <- read_feather(paste0(abtfolder, "abt_basis_", dtlaad , "_", label, ".feather")) %>%
  filter(type == "scoring")

#### definitief model inlezen
finale_model <- readRDS(paste0(modelfolder, dtlaad, "_", label, "/finale_model.rds"))

## voorspellen van de risicoscores
abt_prob <- predict(finale_model$model[[1]], newdata = abt, type = "prob")

## riscioscores en abt samenvoegen tot 1 tabel
abt_prob <- cbind(abt_prob, abt)

#--------------------------------------------------------------------------------------------------
# lijst maken
#--------------------------------------------------------------------------------------------------

lijst_alle <- abt_prob %>%
  arrange(-Ja) %>%
  mutate(volgnummer = 1:nrow(.)) %>%
  select(volgnummer, persoon_id, Ja) 

lijst_alle %>% write.table(paste0(resultatenfolder, "lijst_alle_finalemodel_", dtlaad, "_", label, ".csv"), row.names=FALSE, sep = ";", dec = ".")
abt_prob %>% write_feather(paste0(resultatenfolder, "abt_prob_finalemodel_", dtlaad, "_", label, ".feather"))
