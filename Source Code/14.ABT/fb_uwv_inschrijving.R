#--------------------------------------------------------------------------------------------------
# BESCHRIJVING 
# Maken features voor 'contacten'
# 
# WIJZIGINGSHISTORIE
# 13-02-2017, Medewerker1, initiele versie.
# 08-05-2017, Medewerker1, code rewrite + aanpassing key van persoon_id naar id
# 17-09-2017, Medewerker3, toepassen tardis en nieuwe data
#--------------------------------------------------------------------------------------------------

df_inschrijving_uwv <- read_feather(paste0(datafolder, "uwv_inschrijving.feather")) 

df_inschrijving_uwv_inscope <- df_inschrijving_uwv %>% 
  inner_join(ref) %>%
  #  filter(dtaanvang < melding_om)
  filter(is.na(dteinde) | (dteinde > melding_om - historie)) ## informatie ouder dan historie verwijderen
  

#### Deel 1: actueel ingeschreven en aantal records
abt_inschrijving_uwv_part1 <- df_inschrijving_uwv_inscope %>%
  mutate(ingeschreven = is.na(dteinde)) %>%
  group_by(id) %>%
  dplyr::summarize(inschrijving_uwv_ingeschreven = max(ingeschreven),
            inschrijving_uwv_nrecords = n())

#### Deel 2: aantal keer inschrijving laten verlopen totaal
abt_inschrijving_uwv_part2 <- df_inschrijving_uwv_inscope %>%
  filter(reden_einde_inschrijving %in% "verlopen inschrijving") %>%
  group_by(id) %>%
  summarize(inschrijving_uwv_aantalverlopen = n())

#### Deel 3:  
abt_inschrijving_uwv_part3 <- df_inschrijving_uwv_inscope %>%
  group_by(id) %>%
  summarise(reden_inschrijving_werkend = any(inschrijfreden == "Werkend", na.rm = T),
            reden_inschrijving_niet_werkend = any(inschrijfreden == "Niet werkend", na.rm = T),
            afgelopen_jaar_inschrijven = any(max(dtaanvang) >= melding_om - 365), na.rm = T)


#### SAMENVOEGEN onderdelen
abt_uwv <- ref %>%
  left_join0(abt_inschrijving_uwv_part1) %>%
  left_join0(abt_inschrijving_uwv_part2) %>%
  left_join0(abt_inschrijving_uwv_part3)
  
# tabelnaam toevoegen voor alle features
refstring = c("id", "persoon_id", "target_per", "melding_om", "type" )
names(abt_uwv)[!names(abt_uwv) %in% refstring] <- paste0("uwv_inschrijving_", names(abt_uwv)[!names(abt_uwv) %in% refstring] )

#### Wegschrijven featuretabel
abt_uwv %>%
  write_feather(paste0(featuredatafolder, "features_uwv_inschrijving.feather"))
    
