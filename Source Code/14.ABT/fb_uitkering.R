#--------------------------------------------------------------------------------------------------
# BESCHRIJVING 
# Maken features voor 'ontheffingne
# 
# WIJZIGINGSHISTORIE
# 16-02-2017, Medewerker3, initiele versie.
# 20-02-2017, Medewerker5, aanpassing recentst feature + uitstroom_oms verwijderd => TimeTravel!!
# 21-02-2017, Medewerker5, actief indicator eruit: kan time travel zijn omdat deze later aangepast wordt
# 22-02-2017, Medewerker5, duur aangepast: kan time travel zijn omdat deze later aangepast wordt
# 03-05-2017, Medewerker1, code rewrite + aanpassing key van persoon_id naar id
# 14-09-2017, Medewerker1, omschrijven zodat script werkt met tardis
#--------------------------------------------------------------------------------------------------

df_all <- read_feather(paste0(datafolder,"uitkering.feather"))

df <- df_all %>% 
  left_join(ref, by = "persoon_id") %>%
  filter(!is.na(dtopvoer) | dtopvoer >= melding_om) %>% 
  mutate(huidig = melding_om <= dtafvoer | 
           (!is.na(dtopvoer) & is.na(dtafvoer)))

#****************************************************************************************************
# FEATURES bouwen
#****************************************************************************************************

# ga ervan uit dat de meest recente uitkering actief was op het moment van melding_om.
# hierdoor voorkomen we dat met terugwerkende kracht dteinde gelijk wordt gezet aan dtaanvang
# .. als blijkt dat deze persoon vanaf de aanvang al geen recht had op de uitkering
rownrs <- df %>%
            mutate(rown = 1:nrow(.)) %>%
            group_by(id) %>%
            filter(uitkering == max(uitkering)) %>%
            ungroup() %>%
            select(rown) %>%
            collect() %>%
            .[["rown"]]
df[rownrs, "dteinde"] <- df[rownrs, "melding_om"]

# oude check: dteinde > melding_om of is.na(dteinde) vervangen met melding_om
# wordt als het goed is niet meer gebruikt door bovenstaande nieuwe aanpassing
# .. maar behouden kan geen kwaad (indien er toch iets missing is bijv.)
df$dteinde[df$dteinde > df$melding_om | is.na(df$dteinde)] <- df$melding_om[df$dteinde > df$melding_om | is.na(df$dteinde)]

# aantal dagen uitkering
df <- df %>%
  mutate(uitkering_duur_dagen = as.integer(pmin((dteinde - dtaanvang), (melding_om - dtaanvang)))) 

# info over recentste uitkering
abt_uitkering <- df %>%
  group_by(id) %>%
  filter(uitkering == max(uitkering)) %>% 
  select(id, uitkering_aantal = uitkering, uitkering_recentst = uitkering_oms, uitkering_duur_dagen) %>% 
  mutate(uitkering_in_database = TRUE) %>% 
  ungroup()

# info over historische uitkeringen
abt_uitkering_hist <- df %>%
  group_by(id) %>%
  arrange(id, dtaanvang) %>%
  summarise(uitkering_gemiddeld_aantal_dagen = mean(uitkering_duur_dagen),
            uitkering_som_aantal_dagen = sum(uitkering_duur_dagen),
            uitkering_langste = max(uitkering_duur_dagen),
            uitkering_kortst = min(uitkering_duur_dagen),
            uitkering_ooit_stopgezet = (sum(!is.na(stopzetting_oms)) > 0),
            uitkering_ooit_weekbetaling = (sum(!is.na(week_betaling)) > 0))

### join alle verschillende abts samen
abt <- ref %>% 
  left_join0(abt_uitkering) %>%
  left_join0(abt_uitkering_hist)


#### Wegschrijven featuretabel
abt %>%  write_feather(paste0(datafolder, "features_uitkering.feather"))
