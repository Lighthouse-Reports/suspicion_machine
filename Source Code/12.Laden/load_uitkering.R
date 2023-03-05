#--------------------------------------------------------------------------------------------------
# BESCHRIJVING 
# Laad nieuwste versie van de data en schrijf dit weg naar een feather bestand met de tabelnaam
#
# WIJZIGINGSHISTORIE
# 05-09-2017, Medewerker3, initiele versie
# 22-06-2020, Medewerker4, naar lubridate as_date(), want dan geen expliciete timezone nodig
#--------------------------------------------------------------------------------------------------

tabel <- "uitkering"

read_feather(paste0(bronfolder, tabellen$file_def[tabellen$tabel == tabel])) %>%
  # DWH kolommen droppen
  select(-c(X_WEIFRA_UITKERING_ID, MTG_BRON, MTG_DATUM, MTG_LOB_ID, MTG_GELDIG_VAF, MTG_GELDIG_TEM)) %>%
  # Overige omzettingen
  set_names(tolower(names(.))) %>%
  rename(persoon_id = bsn) %>% 
  mutate(dtopvoer = as_date(dtopvoer),
         dtafvoer = as_date(dtafvoer),
         dtbegindienst = as_date(dtbegindienst),
         dteindedienst = as_date(dteindedienst),
         dtbeginstopzetting = as_date(dtbeginstopzetting)) %>% 
  write_feather(paste0(datafolder, tabel, ".feather"))
  
