#--------------------------------------------------------------------------------------------------
# BESCHRIJVING 
# Maken features voor 'werkervaring_uwv
# 
# WIJZIGINGSHISTORIE
# 14-02-2017, Medewerker3, initiele versie.
# 21-02-2017, Medewerker3, dteinde toevoegen in het maken van pla features. einde kan pas ingevuld worden na dat einde.
# 26-04-2017, Medewerker1, code rewrite + aanpassing key van persoon_id naar id
# 13-09-2017, Medewerker3, Tardis implementeren
# 19-01-2018, Medewerker1, controledatum toegevoegd
# 23-11-2018, Medewerker4, pl_historie naar datatype integer i.v.m. iml
# 20-07-2020, Medewerker4, format_categories() toegepast, kleine wijzigingen
#--------------------------------------------------------------------------------------------------

df_pla <- read_feather(paste0(datafolder, "pla.feather"))
lookup_doelstelling <- read.csv(paste0(stamdatafolder, "lookup_doelstelling.csv"))

df_pla_inscope <- df_pla %>% 
  tardis() %>%
  filter(is.na(dteinde) | (dteinde > melding_om - historie)) %>%  ## informatie ouder dan historie verwijderen
  mutate(huidige = dteinde>=melding_om | (!is.na(dtaanvang) & is.na(dteinde)),
         dteinde = pmin(dteinde, melding_om, na.rm = T))

controledatum <- as.Date(max(df_pla_inscope$jn_datetime)) #datum actualiteit tabel

### part1: einde feature
abt_part1 <- df_pla_inscope %>%
  mutate(reden_einde_naam = ifelse(is.na(reden_einde_naam), "geen_einde_bekend", reden_einde_naam),
         reden_einde_naam = categorize(reden_einde_naam, afkap_p = 99),
         reden_einde_naam = paste0("pla_einde_", format_categories(reden_einde_naam))) %>%
  count(id, reden_einde_naam) %>%
  ungroup() %>%
  spread(reden_einde_naam, n, fill = 0L)

### part2: doelstelling feature actueel
abt_part2 <- df_pla_inscope %>%
  filter(huidige) %>% # actuele records filteren
  left_join(lookup_doelstelling, by = "doelstelling_tekst") %>%
  filter(!is.na(cat)) %>%
  mutate(cat = categorize(cat, other = "pla_categorie_doelstelling_other"),
         cat = paste0("pla_actueel_", format_categories(cat))) %>%
  count(id, cat) %>%
  ungroup() %>%
  spread(cat, n, fill = 0L)

### part3: doelstelling feature historie
abt_part3 <- df_pla_inscope %>%
  left_join(lookup_doelstelling, by = "doelstelling_tekst") %>%
  filter(!is.na(cat)) %>%
  mutate(cat = categorize(cat, other = "pla_categorie_doelstelling_other"), 
         cat = paste0("pla_hist_", format_categories(cat))) %>%
  count(id, cat) %>%
  ungroup() %>%
  spread(cat, n, fill = 0L)

### part4:  pla feature actueel
abt_part4 <- df_pla_inscope %>% 
  filter(huidige,
         !is.na(pla_naam)) %>%
  mutate(pla_naam = categorize(pla_naam, afkap_p = 95),
         pla_naam = paste0("pla_actueel_", format_categories(pla_naam))) %>%
  count(id, pla_naam) %>%
  ungroup() %>%
  mutate(n = n >= 1) %>%
  spread(pla_naam, n, fill = 0)

### part5:  pla feature historie
abt_part5 <- df_pla_inscope %>% 
  filter(!is.na(pla_naam)) %>%
  mutate(pla_naam = categorize(pla_naam, afkap_p = 95),
         pla_naam = paste0("pla_historie_", format_categories(pla_naam))) %>%
  count(id, pla_naam) %>%
  ungroup() %>%
  mutate(n = n >= 1) %>%
  #spread(pla_naam, n)
  mutate(n = as.integer(n)) %>% 
  spread(pla_naam, n, fill = 0L)


### part6: dtondertekening feature actueel
abt_part6 <- df_pla_inscope %>%
  filter(huidige) %>% # actuele records filteren
  select(id, jn_nr, datum_ondertekening) %>%
  distinct() %>%
  group_by(id) %>%
  summarize(pla_ondertekeningen_actueel = sum(!is.na(datum_ondertekening)))
  
### part7: dtondertekening feature historie
abt_part7 <- df_pla_inscope %>%
  select(id, jn_nr, datum_ondertekening) %>%
  distinct() %>%
  group_by(id) %>%
  summarize(pla_ondertekeningen_historie = sum(!is.na(datum_ondertekening)))


# Samenvoegen en wegschrijven
abt_pla <- ref %>%
  mutate(pla_controledatum = controledatum) %>%
  left_join0(abt_part1) %>%
  left_join0(abt_part2) %>%
  left_join0(abt_part3) %>%
  left_join0(abt_part4) %>%
  left_join0(abt_part5) %>%
  left_join0(abt_part6) %>%
  left_join0(abt_part7) %>% 
  select(-type, -persoon_id, -target_per, -melding_om) # niet nodig in features tabel
  
#### Wegschrijven featuretabel
abt_pla %>% 
  write_feather(paste0(featuredatafolder, "features_pla.feather"))
    
## Opruimen
rm(df_pla,
   df_pla_inscope,
   abt_part1,
   abt_part2,
   abt_part3,
   abt_part4,
   abt_part5,
   abt_part6,
   abt_part7,
   abt_pla)
