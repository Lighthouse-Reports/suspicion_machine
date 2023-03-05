#--------------------------------------------------------------------------------------------------
# BESCHRIJVING 
# Maken features voor 'ontheffingne
# 
# WIJZIGINGSHISTORIE
# 15-02-2017, Medewerker3, initiele versie.
# 28-02-2017, Medewerker1, resultaat van categorize() opgeslagen in lookup tabel voor reproduceerbaarheid
# 01-05-2017, Medewerker1, code rewrite + aanpassing key van persoon_id naar id
# 14-09-2017, Medewerker1, omschrijven zodat script werkt met tardis
# 19-01-2018, Medewerker1, controledatum toegevoegd
# 23-11-2018, Medewerker4, typering_ind naar datatype integer i.v.m. iml
# 20-07-2020, Medewerker4, format_categories() toegepast, kleine wijzigingen, afkap_p = 75 vanwege vele categorieën
#--------------------------------------------------------------------------------------------------

df_typering <- read_feather(paste0(datafolder,"typering.feather"))

df_typering_inscope <- df_typering %>% 
  tardis() %>%
  filter(is.na(dteinde) | (dteinde > melding_om - historie)) %>% ## informatie ouder dan historie verwijderen
  mutate(huidige = dteinde>=melding_om | (!is.na(dtaanvang) & is.na(dteinde)),
         dteinde = pmin(dteinde, melding_om, na.rm = T))

controledatum <- as.Date(max(df_typering_inscope$jn_datetime)) #datum actualiteit tabel


### huidige belemmeringen typeringen
abt_typering <- df_typering_inscope %>% 
  filter(huidige) %>%
  mutate(typering_naam = categorize(typering_naam, afkap_p = 75),
         typering_naam = paste0("typering_", format_categories(typering_naam))) %>%
  count(id, typering_naam) %>%
  spread(key = typering_naam, value = n, fill = 0L) %>%
  mutate(typering_aantal = as.integer(rowSums(select(., contains("typering"))))) %>%
  #mutate(typering_ind = TRUE)
  mutate(typering_ind = 1L)


### tijd in huidige typeringen (dagen)
abt_typering_tijd <- df_typering_inscope %>% 
  filter(huidige) %>%
  mutate(value = as.integer(melding_om - dtaanvang),
         typering_naam = categorize(typering_naam, afkap_p = 75),
         typering_naam = paste0("typering_dagen_", format_categories(typering_naam))) %>%
  count(id, typering_naam, wt = value) %>%
  spread(key = typering_naam, value = n, fill = 0L) %>% 
  mutate(typering_dagen_som = as.integer(rowSums(select(., contains("typering")))),
         typering_dagen_mean = as.integer(rowMeans(select(., contains("typering")))))
         

### hist belemmeringen typeringen
abt_typering_hist <- df_typering_inscope %>% 
  mutate(typering_naam = categorize(typering_naam, afkap_p = 75),
         typering_naam = paste0("typering_hist_", format_categories(typering_naam))) %>%
  count(id, typering_naam) %>%
  spread(key = typering_naam, value = n, fill = 0L) %>%
  mutate(typering_hist_aantal = as.integer(rowSums(select(., contains("typering"))))) %>%
  #mutate(typering_hist_ind = TRUE)
  mutate(typering_hist_ind = 1L)


### tijd in hist typeringen (dagen)
abt_typering_hist_tijd <- df_typering_inscope %>% 
  mutate(value = as.integer(coalesce(dteinde, melding_om) - dtaanvang),
         typering_naam = categorize(typering_naam, afkap_p = 75),
         typering_naam = paste0("typering_hist_dagen_", format_categories(typering_naam))) %>%
  count(id, typering_naam, wt = value) %>%
  spread(key = typering_naam, value = n, fill = 0L) %>% 
  mutate(typering_hist_dagen_som = as.integer(rowSums(select(., contains("typering")))),
         typering_hist_dagen_mean = as.integer(rowMeans(select(., contains("typering")))))

#### SAMENVOEGEN onderdelen
abt_typering <- ref %>%
  mutate(typering_controledatum = controledatum) %>%
  left_join0(abt_typering) %>%
  left_join0(abt_typering_hist) %>%
  left_join0(abt_typering_tijd) %>%
  left_join0(abt_typering_hist_tijd) %>% 
  select(-type, -persoon_id, -target_per, -melding_om) # niet nodig in features tabe


#### Wegschrijven featuretabel
abt_typering %>%
  write_feather(paste0(featuredatafolder, "features_typering.feather"))
    
## Opruimen
rm(df_typering,
   df_typering_inscope,
   abt_typering_hist,
   abt_typering_tijd,
   abt_typering_hist_tijd,
   abt_typering)
