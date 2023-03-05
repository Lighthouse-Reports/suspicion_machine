#--------------------------------------------------------------------------------------------------
# BESCHRIJVING 
# Maken features voor 'belemmering'
# 
# WIJZIGINGSHISTORIE
# 13-02-2017, Medewerker4, initiele versie.
# 16-02-2017, Medewerker3, toevoegen van extra historische features
# 01-05-2017, Medewerker1, code rewrite + aanpassing key van persoon_id naar id
# 13-09-2017, Medewerker4, implementatie tardis
# 19-01-2018, Medewerker1, controledatum toegevoegd
# 20-07-2020, Medewerker4, format_categories() toegepast, kleine wijzigingen
#--------------------------------------------------------------------------------------------------

df_belemmering <- read_feather(paste0(datafolder, "belemmering.feather"))

df_belemmering_inscope <- df_belemmering %>% 
  tardis() %>%
  filter(is.na(dteinde) | (dteinde > melding_om - historie)) %>% ## informatie ouder dan historie verwijderen
  mutate(huidige = melding_om <= dteinde | (!is.na(dtaanvang) & is.na(dteinde)))

controledatum <- as.Date(max(df_belemmering_inscope$jn_datetime)) #datum actualiteit tabel


#### Deel 1: huidige belemmeringen
abt_part1 <- df_belemmering_inscope %>% 
  filter(huidige) %>%
  mutate(belemmering = categorize(belemmering),
         belemmering = paste0("belemmering_", format_categories(belemmering))) %>%
  count(id, belemmering) %>%
  ungroup() %>%
  spread(belemmering, n, fill = 0L) %>%
  mutate(belemmering_aantal_huidig = as.integer(rowSums(.[,-1])), belemmering_ind = 1L)

#### Deel 2: alle belemmeringen ooit
abt_part2 <- df_belemmering_inscope %>% 
  mutate(belemmering = categorize(belemmering),
         belemmering = paste0("belemmering_hist_", format_categories(belemmering))) %>%
  count(id, belemmering) %>%
  ungroup() %>%
  spread(belemmering, n, fill = 0L) %>%
  mutate(belemmering_aantal_ooit = as.integer(rowSums(.[,-1])), belemmering_ind_hist = 1L)

#### Deel 3: duur van belemmeringen (ooit)
abt_part3 <- df_belemmering_inscope %>% 
  filter(!is.na(belemmering)) %>%
  select(id, belemmering, dtaanvang, dteinde, melding_om) %>%
  mutate(duur = as.integer(ifelse(is.na(dteinde), melding_om - dtaanvang, dteinde - dtaanvang)), 
         belemmering = categorize(belemmering),
         belemmering = paste0("belemmering_dagen_", format_categories(belemmering))) %>%
  count(id, belemmering, wt=duur) %>%
  ungroup() %>%
  spread(belemmering, n, fill = 0L) %>%
  mutate(belemmering_dagen_totaal = as.integer(rowSums(.[,-1])))


#### SAMENVOEGEN onderdelen
abt_belemmering <- ref %>%
  mutate(belemmering_controledatum = controledatum) %>%
  left_join0(abt_part1) %>%
  left_join0(abt_part2) %>%
  left_join0(abt_part3) %>% 
  select(-type, -persoon_id, -target_per, -melding_om) # niet nodig in features tabel

#### Wegschrijven featuretabel
abt_belemmering %>%
  write_feather(paste0(featuredatafolder, "features_belemmering.feather"))
    
## Opruimen
rm(df_belemmering,
   df_belemmering_inscope,
   abt_part1,
   abt_part2,
   abt_part3,
   abt_belemmering)
