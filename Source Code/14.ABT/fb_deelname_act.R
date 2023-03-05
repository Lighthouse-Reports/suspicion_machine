#--------------------------------------------------------------------------------------------------
# BESCHRIJVING 
# Maken features voor 'deelnames_activiteiten'
# 
# WIJZIGINGSHISTORIE
# 13-02-2017, Medewerker1, initiele versie.
# 10-04-2017, Medewerker1, key gewijzigd van persoons_id naar id
# 14-09-2017, Medewerker3, tardis en nieuwe data
# 19-01-2018, Medewerker1, controledatum toegevoegd + correctie prefix features
# 22-07-2020, Medewerker4, format_categories() toegepast, kleine wijzigingen
#--------------------------------------------------------------------------------------------------

df_deelnames_activiteiten <- read_feather(paste0(datafolder, "deelname_act.feather"))

df_deelname_act_inscope <- df_deelnames_activiteiten %>%
  tardis() %>%
  filter(is.na(dteinde) | (dteinde > melding_om - historie)) %>%
  mutate(huidige = dteinde>=melding_om | (!is.na(dtaanvang) & is.na(dteinde)),
         dteinde = pmin(dteinde, melding_om, na.rm = T))

controledatum <- as.Date(max(df_deelname_act_inscope$jn_datetime)) #datum actualiteit tabel

#### Deel 1: aantal gestarte en niet-gestarte trajecten actueel
abt_deelnames_part1 <- df_deelname_act_inscope %>%
  filter(huidige) %>%
  mutate(gestart = ifelse(is.na(dtaanvang), 0L, 1L),
         nietgestart = ifelse(is.na(dtaanvang), 1L, 0L)) %>%
  group_by(id) %>%
  dplyr::summarize(deelname_act_actueel_projecten_gestart = sum(gestart),
                   deelname_act_actueel_projecten_niet_gestart = sum(nietgestart),
                   deelname_act_actueel_projecten_totaal = n(),
                   deelname_act_actueel_projecten_uniek = length(unique(jn_nr)))

#### Deel 1: aantal gestarte en niet-gestarte trajecten historie
abt_deelnames_part11 <- df_deelname_act_inscope %>%
  mutate(gestart = ifelse(is.na(dtaanvang), 0L, 1L),
         nietgestart = ifelse(is.na(dtaanvang), 1L, 0L)) %>%
  group_by(id) %>%
  dplyr::summarize(deelname_act_hist_projecten_gestart = sum(gestart),
                   deelname_act_hist_projecten_niet_gestart = sum(nietgestart),
                   deelname_act_hist_projecten_totaal = n(),
                   deelname_act_hist_projecten_uniek = length(unique(jn_nr)))

#### Deel 2: aantallen records per categorie 'reintegratieladder'.
abt_deelnames_part2 <- df_deelname_act_inscope %>%
  mutate(reintegratieladder = paste0("deelname_act_reintegratieladder_", format_categories(reintegratieladder))) %>%
  count(id, reintegratieladder) %>%
  spread(reintegratieladder, n, fill = 0L)
  
#### Deel 3: aantallen records per categorie 'voortijdig_afgebroken' (afkap 80: alleen grotere groepen)
abt_deelnames_part3 <- df_deelname_act_inscope %>%
  filter(!is.na(voortijdig_afgebroken)) %>% 
  mutate(voortijdig_afgebroken = categorize(voortijdig_afgebroken, afkap_p = 80),
         voortijdig_afgebroken = paste0("deelname_act_afgebroken_", format_categories(voortijdig_afgebroken))) %>% 
  count(id, voortijdig_afgebroken) %>%
  spread(voortijdig_afgebroken, n, fill = 0L)

#### SAMENVOEGEN onderdelen
abt_deelnames <- ref %>%
  mutate(deelname_act_controledatum = controledatum) %>%
  left_join0(abt_deelnames_part1) %>%
  left_join0(abt_deelnames_part11) %>%
  left_join0(abt_deelnames_part2) %>%
  left_join0(abt_deelnames_part3) %>% 
  select(-type, -persoon_id, -target_per, -melding_om) # niet nodig in features tabel

#### Wegschrijven featuretabel
abt_deelnames %>%
  write_feather(paste0(featuredatafolder, "features_deelname_act.feather"))
    
## Opruimen
rm(df_deelnames_activiteiten,
   df_deelname_act_inscope,
   abt_deelnames_part1,
   abt_deelnames_part11,
   abt_deelnames_part2,
   abt_deelnames_part3,
   abt_deelnames)
