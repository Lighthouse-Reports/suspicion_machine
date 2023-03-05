#--------------------------------------------------------------------------------------------------
# Beschrijving
#--------------------------------------------------------------------------------------------------
# Doel: feature selectie o.b.v. handmatige opgave en o.b.v. near zero variance
# Output: csv met namen van de features die overgebleven zijn na de feature selectie
#
# WIJZIGINGSHISTORIE
# 21-12-2017, Medewerker3; eerste versie
# 01-10-2018, Medewerker3; init
# 02-07-2018, Medewerker4; migratie naar WOB server
# 11-07-2018, Medewerker1; Selectie op indices vervangen door selectie op naam)
# 03-08-2020, Medewerker2; filter op type aangepast naar nieuwe situatie (controle zit ook in traintest)
# 19-08-2020, Medewerker4, op het einde update feature profiles toegevoegd
#--------------------------------------------------------------------------------------------------

#--------------------------------------------------------------------------------------------------
# init
#--------------------------------------------------------------------------------------------------
library(feather)
library(tidyverse)
library(stringr)

source("D:/DATA/SHARED/Analytics_Uitkeringsfraude/1.Code/10.Functies/init.R")

abt <- read_feather(paste0(abtfolder, "abt_basis_", dtlaad , "_", label, ".feather"))

#--------------------------------------------------------------------------------------------------
# Feature selectie
#--------------------------------------------------------------------------------------------------

## Handmatige featureverwijdering
drop_features <- ""

## Near zero variance
# op de gelabelde data
near_zero_var_lbl <- abt %>% 
  filter(type %in% c("metlabel_traintest")) %>%  #RZ 03-08-2020; "metlabel_validatie" verwijderd.
  select(-one_of(features_permanent)) %>%
  select(caret::nearZeroVar(., freqCut = 97/3, allowParallel=TRUE, uniqueCut = 10)) %>%
  names()
# op de ongelabelde data
near_zero_var_nolbl <- abt %>%
  filter(type == "scoring") %>% 
  select(-one_of(features_permanent)) %>%
  select(caret::nearZeroVar(., freqCut = 97/3, allowParallel=TRUE, uniqueCut = 10)) %>%
  names()

near_zero_var <- unique(c(near_zero_var_lbl, near_zero_var_nolbl))

# features verwijderen die in de gelabelde of ongelabelde set te weinig variantie hebben
nzv_features_to_keep <- data.frame(features = names(abt)) %>%
  filter(!features %in% c(near_zero_var, drop_features))


#--------------------------------------------------------------------------------------------------
# output
#--------------------------------------------------------------------------------------------------
write_feather(nzv_features_to_keep, 
              paste0(featureselectiesfolder,"no_nzv_", dtlaad, "_", label, ".feather"))
write.csv(nzv_features_to_keep, 
           paste0(featureselectiesfolder,"no_nzv_", dtlaad, "_", label, ".csv"), row.names = FALSE)

#--------------------------------------------------------------------------------------------------
# Profile van alle features bijwerken
#--------------------------------------------------------------------------------------------------
update_feature_profiles(abt, stap = "nzv", input = nzv_features_to_keep)

