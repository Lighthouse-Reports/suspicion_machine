#--------------------------------------------------------------------------------------------------
# BESCHRIJVING 
# Maken features huisvesting tabel
#
# WIJZIGINGSHISTORIE
# 13-02-2017: Medewerker2, feature building huisvesting
# 08-05-2017: Medewerker1; code rewrite + aanpassing key van persoon_id naar id
# 18-09-2017: Medewerker1, omschrijven zodat script werkt met tardis
# 19-01-2018, Medewerker1, controledatum toegevoegd
#--------------------------------------------------------------------------------------------------

df_huisvesting <- read_feather(paste0(datafolder, "huisvesting.feather")) %>%
  tardis() %>%
  filter(is.na(dteinde) | (dteinde > melding_om - historie)) %>% ## informatie ouder dan historie verwijderen
  mutate(huidig = melding_om <= dteinde | (!is.na(dtaanvang) & is.na(dteinde))) %>%
  mutate(dteinde = pmin(dteinde, melding_om))

controledatum <- max(df_huisvesting$dtopvoer) #datum actualiteit tabel

#****************************************************************************************************
#features huisvesting maken
#****************************************************************************************************


#### Deel 1: meest recente huidige huisvesting omschrijving en aantal huisvestigingsadressen
abt_huisvesting_part1 <- df_huisvesting %>%
  filter(huidig) %>%
  group_by(id) %>%
  mutate(huisvesting_aantal_huisvestingen = n(),
         huisvesting_dagen_op_huisvesting = melding_om - dtaanvang) %>%
  arrange(dtopvoer) %>%
  slice(n()) %>%
  select(id, huisvesting_aantal_huisvestingen, huisvesting_dagen_op_huisvesting)
         
  
#### Deel 2: gemiddeld aantal dagen in huisvesting
abt_huisvesting_part2 <- df_huisvesting %>%
  group_by(id) %>%
  mutate(duur = dteinde - dtaanvang) %>%
  summarise(huisvesting_duur_gemiddeld = round(mean(duur)), 
            huisvesting_duur_totaal = sum(duur))


#### Deel 3: soort huisvesting
abt_huisvesting_part3 <- df_huisvesting %>%
  mutate(huisvesting_omsch = paste0("huisvesting_aantal_", categorize(huisvesting_omsch))) %>%
  count(id, huisvesting_omsch) %>%
  spread(huisvesting_omsch, n, fill = 0L)
  

#### Deel 4: totaal aantal (unieke) huisvesting en ratio - hoe vaak verandert iemand van huisvesting omschrijving?
abt_huisvesting_part4 <- df_huisvesting %>%
  group_by(id) %>%
  summarise(huisvesting_aantal_huisvesting = n(), 
            huisvesting_aantal_unieke_huisvesting = n_distinct(huisvesting_omsch),
            huisvesting_verander_ratio = huisvesting_aantal_unieke_huisvesting / huisvesting_aantal_huisvesting)



#### samenvoegen en wegschrijven
abt_huisvesting <- ref %>%
  mutate(huisvesting_controledatum = controledatum) %>%
  left_join0(abt_huisvesting_part1) %>%
  left_join0(abt_huisvesting_part2) %>%
  left_join0(abt_huisvesting_part3) %>%
  left_join0(abt_huisvesting_part4)
  
abt_huisvesting %>%
  write_feather(paste0(featuredatafolder, "features_huisvesting.feather"))
