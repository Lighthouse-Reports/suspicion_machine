#--------------------------------------------------------------------------------------------------
# BESCHRIJVING 
# Laad nieuwste versie van de data en schrijf dit weg naar een feather bestand met de tabelnaam
#
# WIJZIGINGSHISTORIE
# 10-07-2017, Medewerker4, initiele versie
# 23-08-2017, Medewerker4, verplaatsing naar cloud server
# 25-08-2017, Medewerker3, wegschrijven naar feather
#--------------------------------------------------------------------------------------------------

tabel <- "huisvesting"

read_feather(paste0(bronfolder, tabellen$file_def[tabellen$tabel == tabel])) %>%
  select(-c(X_WEIFRA_HUISVESTING_ID, MTG_BRON, MTG_LOB_ID, MTG_GELDIG_VAF, MTG_GELDIG_TEM)) %>%
  set_names(tolower(names(.))) %>%
  mutate(dtbegin = as.Date(dtbegin, format = "%d-%m-%Y %H:%M:%S", tz = "CET"),
         dteinde = as.Date(dteinde , format = "%d-%m-%Y %H:%M:%S", tz = "CET"),
         dtopvoer = as.Date(dtopvoer, format = "%d-%m-%Y %H:%M:%S", tz = "CET"),
         dtafvoer = as.Date(dtafvoer, format = "%d-%m-%Y %H:%M:%S", tz = "CET")) %>%
  rename(persoon_id = bsn,
         dtaanvang = dtbegin) %>% 
  write_feather(paste0(datafolder, tabel, ".feather"))



