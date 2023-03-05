#--------------------------------------------------------------------------------------------------
# BESCHRIJVING 
# Maken features voor beschikbaarheid
# 
# WIJZIGINGSHISTORIE
# 14-02-2017, Medewerker3, initiele versie.
# 02-02-2017, Medewerker1, aanpassen om categorieen binnen 'beschikbaarheid' en 'recent_beschikbaarheid' vast te zetten voor nieuwe data
# 19-04-2017, Medewerker1, code rewrite + aanpassing key van persoon_id naar id
# 11-09-2017, Medewerker1, omschrijven zodat script werkt met tardis
# 25-09-2917, Medewerker3, einde aangepast naar dteinde en historie toegevoegd
# 19-01-2018, Medewerker1, controledatum toegevoegd
# 26-11-2018, Medewerker4, categorische vars omgezet in dummy's i.v.m. iml
# 15-07-2020, Medewerker4, format_categories() toegepast, in historie_per_groep prefix in kolomnaam toegevoegd
#****************************************************************************************************

df_beschikbaarheid <- read_feather(paste0(datafolder, "beschikbaarheid.feather"))

df_beschikbaarheid_inscope <- df_beschikbaarheid %>% 
  tardis() %>%
  filter(melding_om < dteinde + historie) # records ouder dan historie weg ### maar hier maken we dan al een duidelijke beslissing dat we dit doen op basis van dteinde willen we dit?

controledatum <- as.Date(max(df_beschikbaarheid_inscope$jn_datetime)) #datum actualiteit tabel


# huidige (op dat moment geldende) status beschikbaarheid en indicator wel/niet bekend
abt_huidig <- df_beschikbaarheid_inscope %>%
  filter(melding_om <= dteinde | (!is.na(dtaanvang) & is.na(dteinde))) %>% # huidig geldend
  group_by(id) %>%
  arrange(desc(jn_datetime)) %>%
  slice(1) %>% ## hoezo kan je hier de eerste pakken? Ga je er dan vanuit dat iemand maar een beschikbaarheid kan hebben? 
  ungroup() %>%
  mutate(beschikbaarheid_huidig_bekend = 1L,
         beschikbaarheid_huidig = categorize(beschikbaarheid_naam, other = 0L)) %>%
  select(id, beschikbaarheid_huidig, beschikbaarheid_huidig_bekend)

# spread naar kolommen
# as.factor is nodig voor dummyVars()
abt_huidig_spread <- abt_huidig %>% 
  select(beschikbaarheid_huidig) %>% 
  mutate(beschikbaarheid_huidig = as.factor(format_categories(beschikbaarheid_huidig))) %>% 
  predict(dummyVars("~.", data = ., fullRank = FALSE, sep = "_"), newdata = .) %>% 
  as.data.frame()

abt_huidig <- abt_huidig %>% select(-beschikbaarheid_huidig) %>% 
  bind_cols(abt_huidig_spread)

# recentste (op dat moment geldende) status beschikbaarheid en indicator wel/niet bekend
abt_recent <- df_beschikbaarheid_inscope %>%
  group_by(id) %>%
  arrange(desc(jn_datetime)) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(beschikbaarheid_recent_bekend = 1L,
         beschikbaarheid_recent = categorize(beschikbaarheid_naam, other = 0L)) %>%
  select(id, beschikbaarheid_recent, beschikbaarheid_recent_bekend)

# spread naar kolommen
# as.factor is nodig voor dummyVars()
abt_recent_spread <- abt_recent %>% 
  select(beschikbaarheid_recent) %>% 
  mutate(beschikbaarheid_recent = as.factor(format_categories(beschikbaarheid_recent))) %>% 
  predict(dummyVars("~.", data = ., fullRank = FALSE, sep = "_"), newdata = .) %>% 
  as.data.frame()

abt_recent <- abt_recent %>% select(-beschikbaarheid_recent) %>% 
  bind_cols(abt_recent_spread)

## alle historische meldingen van beschikbaarheid
abt_historie <- df_beschikbaarheid_inscope %>%
  group_by(id) %>%
  summarise(beschikbaarheid_aantal_historie = n())

abt_historie_per_groep <- df_beschikbaarheid_inscope %>%
  mutate(beschikbaarheid_groep = paste0("beschikbaarheid_aantal_historie_", format_categories(beschikbaarheid_naam))) %>% 
  count(id, beschikbaarheid_groep) %>%
  spread(beschikbaarheid_groep, n, fill = 0L)


#### SAMENVOEGEN onderdelen
abt_beschikbaarheid <- ref %>%
  mutate(beschikbaarheid_controledatum = controledatum) %>%
  left_join0(abt_huidig) %>%
  left_join0(abt_recent) %>%
  left_join0(abt_historie) %>%
  left_join0(abt_historie_per_groep) %>% 
  select(-type, -persoon_id, -target_per, -melding_om) # niet nodig in features tabel
  
#### Wegschrijven featuretabel
abt_beschikbaarheid %>%
  write_feather(paste0(featuredatafolder, "features_beschikbaarheid.feather"))
    
## Opruimen
rm(df_beschikbaarheid,
   df_beschikbaarheid_inscope,
   abt_huidig,
   abt_recent,
   abt_historie,
   abt_historie_per_groep,
   abt_beschikbaarheid)
