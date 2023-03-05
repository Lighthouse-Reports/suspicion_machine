#--------------------------------------------------------------------------------------------------
# BESCHRIJVING 
# Laad nieuwste versie van de data en schrijf dit weg naar een feather bestand met de tabelnaam
#
# WIJZIGINGSHISTORIE
# 22-06-2020, Medewerker4, initiele versie
#--------------------------------------------------------------------------------------------------

tabel <- "werkopdracht"

read_feather(paste0(bronfolder, tabellen$file_def[tabellen$tabel == tabel])) %>%
  # DWH kolommen droppen
  select(-c(X_WEIFRA_WERKOPDRACHT_ID, MTG_BRON, MTG_DATUM, MTG_LOB_ID, MTG_GELDIG_VAF, MTG_GELDIG_TEM)) %>%
  # Overige omzettingen
  set_names(tolower(names(.))) %>%
  rename(persoon_id = bsn) %>%
  mutate(datum_afvoer = as_date(datum_afvoer),
         datum_melding = as_date(datum_melding),
         datum_begin_fraude = as_date(datum_begin_fraude),
         datum_einde_fraude = as_date(datum_einde_fraude)) %>% 
  write_feather(paste0(datafolder, tabel, ".feather"))

