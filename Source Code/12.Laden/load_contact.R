#--------------------------------------------------------------------------------------------------
# BESCHRIJVING 
# Laad nieuwste versie van de data en schrijf dit weg naar een feather bestand met de tabelnaam
#
# WIJZIGINGSHISTORIE
# 10-07-2017, Medewerker4, initiele versie
# 23-08-2017, Medewerker4, verplaatsing naar cloud server
# 25-08-2017, Medewerker3, wegschrijven naar feather
# 18-06-2020, Medewerker4, tabel in 3 delen wegens error feather bij groot aantal rijen
#--------------------------------------------------------------------------------------------------


for (tabel in c("contact1", "contact2", "contact3")) {
  read_feather(paste0(bronfolder, tabellen$file_def[tabellen$tabel == tabel])) %>%
    # DWH kolommen droppen
    select(-c(X_WEIFRA_CONTACT_ID, MTG_BRON, MTG_DATUM, MTG_LOB_ID, MTG_GELDIG_VAF, MTG_GELDIG_TEM)) %>%
    # Overige omzettingen
    set_names(tolower(names(.))) %>%
    rename(jn_nr = contact_nr,
           persoon_id = bsn) %>% 
    write_feather(paste0(datafolder, tabel, ".feather"))
}

