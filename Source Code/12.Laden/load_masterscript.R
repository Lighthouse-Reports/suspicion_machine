#--------------------------------------------------------------------------------------------------
# BESCHRIJVING 
# Nieuwste versie van de data inlezen en klaarzetten voor gebruik.
#
# WIJZIGINGSHISTORIE
# 25-08-2017, Medewerker3, intiele versie
# 26-09-2017, Medewerker3, toevoegen van uwv inschrijving
# 02-07-2018, Medewerker4, migratie naar WOB server
# 22-06-2020, Medewerker4, kleine aanpassingen voor deze run, inlezen werkopdracht toegevoegd
#--------------------------------------------------------------------------------------------------


#--------------------------------------------------------------------------------------------------
# Init
#--------------------------------------------------------------------------------------------------
library(feather)
library(lubridate)
library(tidyverse)
library(readxl)

options(stringsAsFactors = F)


source("D:/DATA/SHARED/Analytics_Uitkeringsfraude/1.Code/10.Functies/init.R")

setwd(root)

#--------------------------------------------------------------------------------------------------
# Set variables
#--------------------------------------------------------------------------------------------------

dwh_omgeving <- "_dwhprd.feather"

tabellencsv <- paste0(metadatafolder, "tabellen_", dtlaad, ".csv")
env_s <- new.env()

file_handmatig <- read.csv(paste0(metadatafolder, "files_handmatig.csv"))

#--------------------------------------------------------------------------------------------------
# tabel met versies van de tabellen
#--------------------------------------------------------------------------------------------------

tabellen <- data.frame(file = tolower(list.files(bronfolder))) %>%
  filter(grepl(paste0(dwh_omgeving, "$"), .$file)) %>%
  mutate(file2 = gsub("x_weifra_", "", file),
         tabel_datum = gsub(dwh_omgeving, "", file2)) %>%
#  separate(tabel_datum, into = c("tabel", "datum"), sep = "_20", remove = F) %>%
  mutate(tabel = gsub("^(.*)_([0-9]*)$", "\\1", .$tabel_datum),
         datum = gsub("^(.*)_([0-9]*)$", "\\2", .$tabel_datum)) %>%
  arrange(tabel_datum) %>%
  group_by(tabel) %>%
  slice(n()) %>%
  # deze verwijderen: CONTACT bestaat nu uit 3 files, HUISVESTING nemen we niet meer mee
  filter(!file %in% c("x_weifra_contact_20190815_dwhprd.feather", "x_weifra_huisvesting_20190815_dwhprd.feather")) %>%
  left_join(file_handmatig) %>%
  mutate(file_def = ifelse(!is.na(file_handmatig), file_handmatig, file))

#write_excel_csv(tabellen, tabellencsv)
write.csv(tabellen, tabellencsv, row.names = FALSE)



#--------------------------------------------------------------------------------------------------
# uitvoeren van alle load scripts
#--------------------------------------------------------------------------------------------------

sys.source(paste0(loadfolder, "load_persoon.R"), envir = env_s)
sys.source(paste0(loadfolder, "load_adres.R"), envir = env_s)
sys.source(paste0(loadfolder, "load_component.R"), envir = env_s)
#sys.source(paste0(loadfolder, "load_huisvesting.R"), envir = env_s)  # vervallen
sys.source(paste0(loadfolder, "load_relatie_kind.R"), envir = env_s)
sys.source(paste0(loadfolder, "load_relatie_partner.R"), envir = env_s)
sys.source(paste0(loadfolder, "load_relatie_overige.R"), envir = env_s)
sys.source(paste0(loadfolder, "load_contact.R"), envir = env_s)
sys.source(paste0(loadfolder, "load_instrument.R"), envir = env_s)
sys.source(paste0(loadfolder, "load_afspraak.R"), envir = env_s)
sys.source(paste0(loadfolder, "load_belemmering.R"), envir = env_s)
sys.source(paste0(loadfolder, "load_beschikbaarheid.R"), envir = env_s)
sys.source(paste0(loadfolder, "load_competentie.R"), envir = env_s)
sys.source(paste0(loadfolder, "load_deelname_act.R"), envir = env_s)
sys.source(paste0(loadfolder, "load_ontheffing.R"), envir = env_s)
sys.source(paste0(loadfolder, "load_pers_eigenschap.R"), envir = env_s)
sys.source(paste0(loadfolder, "load_pla.R"), envir = env_s)
sys.source(paste0(loadfolder, "load_typering.R"), envir = env_s)
sys.source(paste0(loadfolder, "load_fraude.R"), envir = env_s)
sys.source(paste0(loadfolder, "load_uitkering.R"), envir = env_s)
#sys.source(paste0(loadfolder, "load_persoon_koppel.R"), envir = env_s)  # niet nodig
sys.source(paste0(loadfolder, "load_werkopdracht.R"), envir = env_s)

