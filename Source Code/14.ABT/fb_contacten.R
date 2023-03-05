#--------------------------------------------------------------------------------------------------
# BESCHRIJVING 
# Maken features voor 'contacten'
# 
# WIJZIGINGSHISTORIE
# 13-02-2017, Medewerker1, initiele versie.
# 19-04-2017, Medewerker1, code rewrite + aanpassing key van persoon_id naar id
# 11-09-2017, Medewerker1, omschrijven zodat script werkt met tardis
# 11-10-2017, Medewerker4, datum_vastlegging gebruikt om functioneel historie te beperken
# 19-01-2018, Medewerker1, controledatum toegevoegd
# 23-11-2018, Medewerker4, onderwerp_boolean naar datatype integer i.v.m. iml
# 15-07-2020, Medewerker4, inlezen uit 3 feather files, format_categories() toegepast
#--------------------------------------------------------------------------------------------------

df_contacten <- read_feather(paste0(datafolder, "contact1.feather")) %>% 
  bind_rows(read_feather(paste0(datafolder, "contact2.feather"))) %>% 
  bind_rows(read_feather(paste0(datafolder, "contact3.feather")))

df_contacten_inscope <- df_contacten %>% 
  tardis() %>%
  filter(melding_om < as.Date(datum_vastlegging) + historie) ## selecteer alleen de contacten die na een bepaalde tijd zijn vastgelegd

controledatum <- as.Date(max(df_contacten_inscope$jn_datetime)) #datum actualiteit tabel


#### Deel 1: aantal type contacten ooit
abt_contacten_part1 <- df_contacten_inscope %>%
  mutate(soort = paste0("contacten_soort_", format_categories(soort_contact_omschrijving))) %>%
  #mutate(soort = gsub(" ", "_", soort, fixed = TRUE)) %>%
  count(id, soort) %>%
  spread(soort, n, fill = 0L)

#### Deel 2:  type contacten afgelopen 365 dagen
abt_contacten_part2 <- df_contacten_inscope %>%
  filter(melding_om - as.Date(jn_datetime) <= 365) %>%
  mutate(soort = paste0("contacten_soort_afgelopenjaar_", format_categories(soort_contact_omschrijving))) %>%
  #mutate(soort = gsub(" ", "_", soort, fixed = TRUE)) %>%
  count(id, soort) %>%
  spread(soort, n, fill = 0L)

#### Deel 3: aantallen contacten naar onderwerp
abt_contacten_part3 <- df_contacten_inscope %>%  
  mutate(onderwerp = paste0("contacten_onderwerp_", format_categories(onderwerp))) %>%
  #mutate(onderwerp = gsub(" ", "_", onderwerp, fixed = TRUE)) %>%
  count(id, onderwerp) %>%
  spread(onderwerp, n, fill = 0L) 

#### Deel 4: boolean contacten naar onderwerp
abt_contacten_part4 <- df_contacten_inscope %>%
  mutate(onderwerp = paste0("contacten_onderwerp_boolean_", format_categories(onderwerp))) %>%
  #mutate(onderwerp = gsub(" ", "_", onderwerp, fixed = TRUE)) %>%
  count(id, onderwerp) %>%
  mutate(n = n>0) %>%
  #spread(onderwerp, n, fill = FALSE)
  mutate(n = as.integer(n)) %>% 
  spread(onderwerp, n, fill = 0L)

#### SAMENVOEGEN onderdelen
abt_contacten <- ref %>%
  mutate(contacten_controledatum = controledatum) %>%
  left_join0(abt_contacten_part1) %>%
  left_join0(abt_contacten_part2) %>%
  left_join0(abt_contacten_part3) %>%
  left_join0(abt_contacten_part4) %>% 
  select(-type, -persoon_id, -target_per, -melding_om) # niet nodig in features tabel

#### Wegschrijven featuretabel
abt_contacten %>%
  write_feather(paste0(featuredatafolder, "features_contacten.feather"))
   
## Opruimen
rm(df_contacten,
   df_contacten_inscope,
   abt_contacten_part1,
   abt_contacten_part2,
   abt_contacten_part3,
   abt_contacten_part4,
   abt_contacten)

  