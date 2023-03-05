#--------------------------------------------------------------------------------------------------
# Beschrijving
# in dit script worden alle folders en instellingen bijgehouden. Alle scripts in de pipeline maken gebruikt
# van deze namen
#
# HISTORIE
# 05-01-2018, Medewerker3; initiele versie
# 02-07-2018, Medewerker4; migratie naar WOB server
# 29-08-2018, Medewerker1; modelfolder_[1:6] verwijderd omdat deze niet meer gebruikt worden.
# 24-10-2018, Medewerker4; "params" uitgesloten van rm() vanwege markdowns
# 17-06-2020, Medewerker4; dtlaad en label ingesteld voor Prestatie010 model
# 19-08-2020, Medewerker4, source make_feature_profiles.R toegevoegd
#--------------------------------------------------------------------------------------------------

# general options
#rm(list = ls())
rm(list = setdiff(ls(), "params"))
Sys.setenv(TZ = "CET")
Sys.setenv(ORA_SDTZ = "CET") # timezone Oracle database aangeven (anders interpretatie als UTC)
seed = 501338
set.seed(seed)
options(stringsAsFactors = F)
 
# # packages train model kan hier niet mee omgaan omdat deze gebruik maakt van Caret. Voorlopig daarom geen packages in dit script
# library(tidyverse)
# library(feather)
# library(lubridate)
# library(readxl)
# library(stringr)

# root
root <- "D:/DATA/SHARED/Analytics_Uitkeringsfraude/"

# datums/labels
dtlaad <- as.Date("2020-09-29", format = "%Y-%m-%d")          # Datum extractie uit DWH
label <- "newlist_p010" # "fullrun1"           ,              # Label (kenmerk) van de huidige run
dtscopemodel <- as.Date("2014-01-01", format = "%Y-%m-%d")    # Datum vanaf wanneer we onderzoeken meenemen in het model

# Codefolders
functionfolder <- paste0(root, "1.Code/10.Functies/")
extractfolder <- paste0(root, "1.Code/11.Extractie/")
loadfolder <- paste0(root, "1.Code/12.Laden/")
featurefolder <- paste0(root, "1.Code/14.ABT/")
scorefolder <- paste0(root, "1.Code/18.Scoren/")
rapportfolder <- paste0(root, "1.Code/99.Rapporten/")

# Datafolders
stamdatafolder <- paste0(root, "2.Data/20.Stamdata/")
bronfolder <- paste0(root, "2.Data/21.Brontabellen/")
datafolder <- paste0(root, "2.Data/22.RData/")
refdatafolder <- paste0(root, "2.Data/23.Ref/")
featuredatafolder <- paste0(root, "2.Data/24.Features/")
abtfolder <- paste0(root, "2.Data/25.ABT/")
featureselectiesfolder <- paste0(root, "2.Data/26.Featureselecties/")
modelfolder <- paste0(root, "2.Data/27.Model/")
resultatenfolder <- paste0(root, "2.Data/28.Resultaten/")
metadatafolder <- paste0(root, "2.Data/29.Metadata/")

# functions
source(paste0(functionfolder, "categorize.R"))
source(paste0(functionfolder, "tardis.R"))
source(paste0(functionfolder, "left_join0.R"))
source(paste0(functionfolder, "check_features.R"))
source(paste0(functionfolder, "format_categories.R"))
source(paste0(functionfolder, "make_feature_profiles.R"))

# vectors
features_permanent <- c("persoon_id", "target_per", "melding_om", "type", "id") # vaste features in elke abt

