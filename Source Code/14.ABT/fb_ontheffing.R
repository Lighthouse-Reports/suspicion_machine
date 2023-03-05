#--------------------------------------------------------------------------------------------------
# BESCHRIJVING 
# Maken features voor ontheffing
# 
# WIJZIGINGSHISTORIE
# 15-02-2017, Medewerker3, initiele versie.
# 28-02-2017, Medewerker1, resultaat van categorize() opgeslagen in lookup tabel voor reproduceerbaarheid
# 26-04-2017, Medewerker1, code rewrite + aanpassing key van persoon_id naar id
# 13-09-2017, Medewerker4, implementatie tardis
# 21-12-2017, Medewerker3; na's vervangen door 0
# 19-01-2018, Medewerker1, controledatum toegevoegd
# 20-07-2020, Medewerker4, format_categories() toegepast, kleine wijzigingen
#--------------------------------------------------------------------------------------------------

df_ontheffing <- read_feather(paste0(datafolder, "ontheffing.feather"))

df_ontheffing_inscope <- df_ontheffing %>% 
  tardis() %>% 
  filter(is.na(dteinde) | (dteinde > melding_om - historie)) %>%  ## informatie ouder dan historie verwijderen
  mutate(huidige = melding_om <= dteinde | (!is.na(dtaanvang) & is.na(dteinde)))

controledatum <- as.Date(max(df_ontheffing_inscope$jn_datetime)) #datum actualiteit tabel


#****************************************************************************************************
# FEATURES bouwen
#****************************************************************************************************

#part1: ontheffing actueel
abt_part1 <- df_ontheffing_inscope %>% 
  filter(huidige) %>%
  mutate(ontheffing = ifelse(grepl("kind", ontheffing), "Vanwege kinderen jonger dan 12 jaar fb_samengevoegd", ontheffing),
         ontheffing = categorize(ontheffing),
         ontheffing = paste0("ontheffing_reden_", format_categories(ontheffing))) %>%
  count(id, ontheffing) %>%
  ungroup() %>%
  spread(ontheffing, n, fill = 0L) %>%
  mutate(ontheffing_actueel_ind = 1L)
  
#part2: ontheffing actueel aantal
abt_part2 <- df_ontheffing_inscope %>% 
  filter(huidige) %>%
  count(id) %>%
  ungroup() %>%
  select(id, ontheffing_actueel_aantal = n) 

#part3: ontheffing actueel tijd (dagen)
abt_part3 <- df_ontheffing_inscope %>% 
  filter(huidige) %>%
  mutate(ontheffing = ifelse(grepl("kind", ontheffing), "Vanwege kinderen jonger dan 12 jaar fb_samengevoegd", ontheffing),
         ontheffing = categorize(ontheffing),
         ontheffing = paste0("ontheffing_dagen_", format_categories(ontheffing))) %>%
  mutate(tijd = as.integer(melding_om - dtaanvang)) %>%
  count(id, ontheffing, wt = tijd) %>%
  ungroup() %>%
  spread(ontheffing, n, fill = 0L) 

#part4: ontheffing actueel tijd totaal (dagen)
abt_part4 <- df_ontheffing_inscope %>% 
  filter(huidige) %>%
  mutate(tijd = as.integer(melding_om - dtaanvang)) %>%
  count(id, wt = tijd) %>%
  ungroup() %>%
  select(id, ontheffing_dagen_totaal = n)


#part5: ontheffing hist
abt_part5 <- df_ontheffing_inscope %>% 
  mutate(ontheffing = ifelse(grepl("kind", ontheffing), "Vanwege kinderen jonger dan 12 jaar fb_samengevoegd", ontheffing),
         ontheffing = categorize(ontheffing),
         ontheffing = paste0("ontheffing_reden_hist_", format_categories(ontheffing))) %>%
  count(id, ontheffing) %>%
  ungroup() %>%
  spread(ontheffing, n, fill = 0L) %>%
  mutate(ontheffing_hist_ind = 1L)

#part6: ontheffing hist aantal
abt_part6 <- df_ontheffing_inscope %>% 
  count(id) %>%
  ungroup() %>%
  select(id, ontheffing_hist_aantal = n) 

#part7: ontheffing hist tijd (dagen)
abt_part7 <- df_ontheffing_inscope %>% 
  mutate(ontheffing = ifelse(grepl("kind", ontheffing), "Vanwege kinderen jonger dan 12 jaar fb_samengevoegd", ontheffing),
         ontheffing = categorize(ontheffing),
         ontheffing = paste0("ontheffing_dagen_hist_", format_categories(ontheffing))) %>%
  mutate(tijd = as.integer(melding_om - dtaanvang)) %>%
  count(id, ontheffing, wt = tijd) %>%
  ungroup() %>%
  spread(ontheffing, n, fill = 0L) 

#part8: ontheffing hist tijd totaal (dagen)
abt_part8 <- df_ontheffing_inscope %>% 
  mutate(tijd = as.integer(melding_om - dtaanvang)) %>%
  count(id, wt = tijd) %>%
  ungroup() %>%
  select(id, ontheffing_dagen_hist_totaal = n)


abt_ontheffing <- ref %>%
  mutate(ontheffing_controledatum = controledatum) %>%
  left_join0(abt_part1) %>%
  left_join0(abt_part2) %>%
  left_join0(abt_part3) %>%
  left_join0(abt_part4) %>%
  left_join0(abt_part5) %>%
  left_join0(abt_part6) %>%
  left_join0(abt_part7) %>%
  left_join0(abt_part8) %>%
  mutate(ontheffing_dagen_hist_mean = if_else(ontheffing_hist_aantal == 0, 0, ontheffing_dagen_hist_totaal / ontheffing_hist_aantal),
         ontheffing_dagen_mean = if_else(ontheffing_actueel_aantal == 0, 0, ontheffing_dagen_totaal / ontheffing_actueel_aantal)) %>% 
  select(-type, -persoon_id, -target_per, -melding_om) # niet nodig in features tabel
  

### Wegschrijven featuretabel
abt_ontheffing %>%
  write_feather(paste0(featuredatafolder, "features_ontheffing.feather"))
    
## Opruimen
rm(df_ontheffing,
   df_ontheffing_inscope,
   abt_part1,
   abt_part2,
   abt_part3,
   abt_part4,
   abt_part5,
   abt_part6,
   abt_part7,
   abt_part8,
   abt_ontheffing)
