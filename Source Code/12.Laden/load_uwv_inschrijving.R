#--------------------------------------------------------------------------------------------------
# BESCHRIJVING 
# Laad nieuwste versie van de data en schrijf dit weg naar een feather bestand met de tabelnaam
#
# WIJZIGINGSHISTORIE
# 10-07-2017, Medewerker4, initiele versie
# 23-08-2017, Medewerker4, verplaatsing naar cloud server
# 25-08-2017, Medewerker3, wegschrijven naar feather
#--------------------------------------------------------------------------------------------------

tabel <- "uwv_inschrijving"

df <- read_feather(paste0(bronfolder, tabellen$file_def[tabellen$tabel == tabel]))

df %>%
  select(-c(X_WEIFRA_UWV_INSCHRIJVING_ID, MTG_BRON, MTG_LOB_ID, MTG_GELDIG_VAF, MTG_GELDIG_TEM)) %>%
  set_names(tolower(names(.))) %>%
  mutate(datum_inschrijving = as.Date(datum_inschrijving, format = "%d-%m-%Y %H:%M:%S", tz = "CET"),
         dat_einde_inschrijving = as.Date(dat_einde_inschrijving, format = "%d-%m-%Y %H:%M:%S", tz = "CET"),
         dat_einde_geldigheid_inschr = as.Date(dat_einde_geldigheid_inschr, format = "%d-%m-%Y %H:%M:%S", tz = "CET")) %>%
  rename(persoon_id = bsn,
         dtaanvang = datum_inschrijving,
         dteinde = dat_einde_inschrijving) %>% 
  write_feather(paste0(datafolder, tabel, ".feather"))



