#--------------------------------------------------------------------------------------------------
# BESCHRIJVING 
# Laad nieuwste versie van de data en schrijf dit weg naar een feather bestand met de tabelnaam
#
# WIJZIGINGSHISTORIE
# 10-07-2017, Medewerker4, initiele versie
# 23-08-2017, Medewerker4, verplaatsing naar cloud server
# 25-08-2017, Medewerker3, wegschrijven naar feather
#--------------------------------------------------------------------------------------------------

tabel <- "persoon_koppel"

read_feather(paste0(bronfolder, tabellen$file_def[tabellen$tabel == tabel])) %>%
  select(-c(X_WEIFRA_PERSOON_KOPPEL_ID, MTG_BRON, MTG_LOB_ID, MTG_GELDIG_VAF, MTG_GELDIG_TEM, MTG_DATUM)) %>%
  set_names(tolower(names(.))) %>%
  write_feather(paste0(datafolder, tabel, ".feather"))



