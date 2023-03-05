#--------------------------------------------------------------------------------------------------
# BESCHRIJVING 
# Laad nieuwste versie van de data en schrijf dit weg naar een feather bestand met de tabelnaam
#
# WIJZIGINGSHISTORIE
# 05-09-2017, Medewerker3, initiele versie
# 27-09-2017, Medewerker4, contains > dplyr::contains wegens conflict met purrr (tidyverse)
# 22-06-2020, Medewerker4, toevoegen uitvoerder en behandelaar vervalt, zit in tabel werkopdracht
#--------------------------------------------------------------------------------------------------

tabel <- "fraude"

read_feather(paste0(bronfolder, tabellen$file_def[tabellen$tabel == tabel])) %>%
  # DWH kolommen droppen
  select(-c(X_WEIFRA_FRAUDE_ID, MTG_BRON, MTG_DATUM, MTG_LOB_ID, MTG_GELDIG_VAF, MTG_GELDIG_TEM)) %>%
  # Overige omzettingen
  set_names(tolower(names(.))) %>%
  rename(persoon_id = bsn, 
         oms_project = project_omschrijving, 
         gevolgen = gevolg_omschrijving) %>% 
  mutate(datum_melding = as_date(datum_melding)) %>% 
  write_feather(paste0(datafolder, tabel, ".feather"))

### Code hieronder komt te vervallen

### om de behandelaar op te zoeken moeten we verschillende bestanden koppelen

# uitvoerder_project <- read.csv(paste0(stamdatafolder, "uitvoerder_project.csv"), na.strings = "")
# uitvoerder_afgehandeld <- read.csv(paste0(stamdatafolder, "uitvoerder_afd_onderzoek_afgehandeld.csv"), na.strings = "")
# behandelaar <- read_excel(paste0(stamdatafolder, "Fraudenummer behandelaar versie 000.xlsx")) %>%
#   set_names(tolower(names(.))) %>%
#   group_by(fraude_nr) %>%
#   summarise(behandelaar = ifelse(length(unique(behandelaar)) == 1, unique(behandelaar), "Beheer Inkomen")) %>%
#   rename(fraudenr = fraude_nr)

### de behandelaar (zoals beschreven door Medewerker4) en de uitvoerder (zoals wij uit de data kunnen halen) toegevoegd
### aan het bestand
# df <- df %>% 
#   left_join(uitvoerder_project) %>%
#   left_join(uitvoerder_afgehandeld) %>%
#   left_join(behandelaar) %>%
#   mutate(uitvoerder = ifelse(is.na(uitvoerder), uitvoerder2, uitvoerder)) %>%
#   select(-uitvoerder2)
# 
# df %>% write_feather(paste0(datafolder, tabel, ".feather"))
#   
