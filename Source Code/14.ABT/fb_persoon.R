#--------------------------------------------------------------------------------------------------
# BESCHRIJVING 
# Maken features voor 'persoon'
# 
# WIJZIGINGSHISTORIE
# 14-02-2017, Medewerker5, initiele versie. 
# 01-05-2017, Medewerker1, code rewrite + aanpassing key van persoon_id naar id
# 14-09-2017, Medewerker1, kleine aanpassingen iv wijzigingen structuur data
# 19-01-2018, Medewerker1, controledatum toegevoegd
# 26-11-2018, Medewerker4, categorische vars omgezet in dummy's i.v.m. iml
# 11-07-2019, Medewerker2, slices om dubbele records per id te voorkomen.
# 21-07-2020, Medewerker4, slice niet meer nodig (features worden op nieuw id gejoind), "persoon_" als prefix toegevoegd
# 19-08-2020, Medewerker4, persoon_leeftijd gewijzigd in persoon_leeftijd_bij_onderzoek
#--------------------------------------------------------------------------------------------------

df_persoon <- read_feather(paste0(datafolder, "persoon.feather"))

controledatum <- as.Date(max(df_persoon$mtg_datum)) #datum actualiteit tabel

abt_persoon <- inner_join(df_persoon, ref) %>%
  mutate(persoon_controledatum = controledatum) %>%
  mutate(persoon_geslacht = as.factor(tolower(geslacht)),
         persoon_gebjaar = round(gebjaarmaand / 100),
         persoon_leeftijd_bij_onderzoek = as.integer(format(melding_om, '%Y')) - persoon_gebjaar) %>%
  select(id, persoon_controledatum, persoon_geslacht, persoon_gebjaar, persoon_leeftijd_bij_onderzoek) 
  #group_by(id, persoon_controledatum, geslacht, gebjaar, leeftijd) %>% #unieke rijen per id overhouden
  #slice(1) %>%
  #ungroup()

# factors omzetten naar dummy variabelen
abt_dummys <- abt_persoon %>% select(persoon_geslacht) %>% 
  predict(dummyVars("~.", data = ., fullRank = FALSE, sep = "_"), newdata = .) %>% 
  as.data.frame()
# dummy variabelen in plaats van originele
abt_persoon <- abt_persoon %>% select(-persoon_geslacht) %>% 
  bind_cols(abt_dummys)

#### Wegschrijven featuretabel
abt_persoon %>%
  write_feather(paste0(featuredatafolder, "features_persoon.feather"))

## Opruimen
rm(df_persoon,
   abt_dummys,
   abt_persoon)
