#--------------------------------------------------------------------------------------------------
# BESCHRIJVING 
# Maken features voor 'contacten'
# 
# WIJZIGINGSHISTORIE
# 13-02-2017, Medewerker1, initiele versie.
# 08-05-2017, Medewerker1, code rewrite + aanpassing key van persoon_id naar id
# 18-09-2017, Medewerker1, omschrijven zodat script werkt met tardis + nieuwe features (huidig + reden einde) toegevoegd
# 19-01-2018, Medewerker1, controledatum toegevoegd
# 22-07-2020, Medewerker4, format_categories() toegepast
#--------------------------------------------------------------------------------------------------

df_instrument <- read_feather(paste0(datafolder, "instrument.feather")) 

df_instrument_inscope <- df_instrument %>%
  tardis() %>%
  filter(is.na(dteinde) | (dteinde > melding_om - historie)) %>% ## informatie ouder dan historie verwijderen
  mutate(huidig = melding_om <= dteinde | (!is.na(dtaanvang) & is.na(dteinde))) 
 
controledatum <- as.Date(max(df_instrument_inscope$jn_datetime)) #datum actualiteit tabel


#### Deel 1: aantal instrumenten per reintegratie type - huidig
abt_aanmelding_instrument_part1 <- df_instrument_inscope %>%
  filter(huidig) %>%
  mutate(reintegratieladder = paste0("instrument_ladder_huidig_", categorize(reintegratieladder)),
         reintegratieladder = format_categories(reintegratieladder)) %>%
  count(id, reintegratieladder) %>%
  spread(reintegratieladder, n, fill = 0L)


#### Deel 3: aantal instrumenten per reintegratie type - historie
abt_aanmelding_instrument_part3 <- df_instrument_inscope %>%
  mutate(reintegratieladder = paste0("instrument_ladder_historie_", categorize(reintegratieladder)),
         reintegratieladder = format_categories(reintegratieladder)) %>%
  count(id, reintegratieladder) %>%
  spread(reintegratieladder, n, fill = 0L)

#### Deel 4: aantal reden_beeindiging
abt_aanmelding_instrument_part4 <- df_instrument_inscope %>%
  filter(!is.na(reden_beeindiging)) %>%
  mutate(reden_beeindiging = paste0("instrument_reden_beeindiging_historie_", categorize(reden_beeindiging, afkap_p = 95)),
         reden_beeindiging = format_categories(reden_beeindiging)) %>%
  count(id, reden_beeindiging) %>%
  spread(reden_beeindiging, n, fill = 0L)
    
#### Deel 5: aantal instrumenten afgelopen 12 maanden
abt_aanmelding_instrument_part5 <- df_instrument_inscope %>%
  filter(melding_om - datum_invoer <= 365) %>%
  group_by(id) %>%
  summarize(instrument_aantal_laatstejaar = n())
 
#### Deel 6: aantal instrumenten totaal
abt_aanmelding_instrument_part6 <- df_instrument_inscope %>%
  group_by(id) %>%
  summarize(instrument_aantal_totaal = n()) 


#### SAMENVOEGEN onderdelen
abt_aanmelding_instrument <- ref %>%
  mutate(instrument_controledatum = controledatum) %>%
  left_join0(abt_aanmelding_instrument_part1) %>%
  left_join0(abt_aanmelding_instrument_part3) %>%
  left_join0(abt_aanmelding_instrument_part4) %>%
  left_join0(abt_aanmelding_instrument_part5) %>%
  left_join0(abt_aanmelding_instrument_part6) %>% 
  select(-type, -persoon_id, -target_per, -melding_om) # niet nodig in features tabel

#### Wegschrijven featuretabel
abt_aanmelding_instrument %>%
  write_feather(paste0(featuredatafolder, "features_instrument.feather"))
    
## Opruimen
rm(df_instrument,
   df_instrument_inscope,
   abt_aanmelding_instrument_part1,
   abt_aanmelding_instrument_part3,
   abt_aanmelding_instrument_part4,
   abt_aanmelding_instrument_part5,
   abt_aanmelding_instrument_part6,
   abt_aanmelding_instrument)
