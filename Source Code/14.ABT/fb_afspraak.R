#--------------------------------------------------------------------------------------------------
# BESCHRIJVING 
# Maken features voor 'contacten'
# 
# WIJZIGINGSHISTORIE
# 13-02-2017, Medewerker1, initiele versie.
# 08-05-2017, Medewerker1, code rewrite + aanpassing key van persoon_id naar id
# 15-09-2017, Medewerker3, tardis + nieuwe data
# 11-10-2017, Medewerker4, datum_afgehandeld gebruikt om functioneel historie te beperken
# 19-01-2018, PMedewerker1, controledatum toegevoegd
# 22-07-2020, Medewerker4, format_categories() toegepast, typo "ingevulgd" > "ingevuld
#--------------------------------------------------------------------------------------------------

df_afspraken <- read_feather(paste0(datafolder, "afspraak.feather"))

df_afspraken_inscope <- df_afspraken %>% 
  tardis() %>% 
  ## selecteer alleen de afspraken die na een bepaalde tijd zijn vastgelegd
  filter(is.na(datum_afgehandeld) | (as.Date(datum_afgehandeld) > melding_om - historie))
  
controledatum <- as.Date(max(df_afspraken_inscope$jn_datetime)) #datum actualiteit tabel

#### Deel 1: aantal afspraken ooit
abt_afspraken_part1 <- df_afspraken_inscope %>%
  group_by(id) %>%
  summarize(count_uniek = length(unique(jn_nr)),
            afgehandeld = sum(!is.na(datum_afgehandeld)),
            resultaat_ingevuld = sum(!is.na(resultaat_naam)),
            resultaat_ingevuld_uniek = length(unique(resultaat_naam)),
            afspraak_naam_ingevuld = sum(!is.na(afspraak_naam)),
            afspraak_naam_ingevuld_uniek = unique(length(afspraak_naam)),
            tekst_ingevuld = sum(!is.na(tekst)),
            aantal_tekens = sum(nchar(tekst), na.rm = T),
            aantal_woorden = sum(nchar(gsub("[[:punct:][:alnum:]]", "", tekst)) + 1, na.rm = T))


#### Deel 2: aantal afspraken laatste jaar
abt_afspraken_part2 <- df_afspraken_inscope %>%
  filter(melding_om - as.Date(jn_datetime) <= 365) %>% # technische datum gebruikt omdat ik geen goede functionele datum zie
  group_by(id) %>%
  summarize(laatstejaar_count_uniek = length(unique(jn_nr)),
            laatstejaar_afgehandeld = sum(!is.na(datum_afgehandeld)),
            laatstejaar_resultaat_ingevuld = sum(!is.na(resultaat_naam)),
            laatstejaar_resultaat_ingevuld_uniek = length(unique(resultaat_naam)),
            laatstejaar_afspraak_naam_ingevuld = sum(!is.na(afspraak_naam)),
            laatstejaar_afspraak_naam_ingevuld_uniek = unique(length(afspraak_naam)),
            laatstejaar_tekst_ingevuld = sum(!is.na(tekst)),
            laatstejaar_aantal_tekens = sum(nchar(tekst), na.rm = T),
            laatstejaar_aantal_woorden = sum(nchar(gsub("[[:punct:][:alnum:]]", "", tekst)) + 1, na.rm = T))

#### Deel 3: soort afspraken ooit
abt_afspraken_part3 <- df_afspraken_inscope %>%
  mutate(afspraak_naam_cat = categorize(afspraak_naam),
         afspraak_naam_cat = format_categories(afspraak_naam_cat)) %>%
  count(id, afspraak_naam_cat) %>%
  spread(afspraak_naam_cat, n, fill = 0L)

#### Deel 4: soort afspraken laatste jaar
abt_afspraken_part4 <- df_afspraken_inscope %>%
  filter(melding_om - as.Date(jn_datetime) <= 365) %>%
  mutate(afspraak_naam_cat = categorize(afspraak_naam),
         afspraak_naam_cat = paste0("afgelopen_jaar_", format_categories(afspraak_naam_cat))) %>%
  count(id, afspraak_naam_cat) %>%
  spread(afspraak_naam_cat, n, fill = 0L)


#### SAMENVOEGEN onderdelen
abt_afspraak <- ref %>%
  mutate(controledatum = controledatum) %>%
  left_join0(abt_afspraken_part1) %>%
  left_join0(abt_afspraken_part2) %>%
  left_join0(abt_afspraken_part3) %>%
  left_join0(abt_afspraken_part4) %>% 
  select(-type, -persoon_id, -target_per, -melding_om) %>%            # niet nodig in features tabel
  rename_at(vars(-one_of("id")), function(x) paste0("afspraak_", x))  # prefix tabelnaam op alle kolommen behalve id

#### Wegschrijven featuretabel
abt_afspraak %>%
  write_feather(paste0(featuredatafolder, "features_afspraak.feather"))
    
## Opruimen
rm(df_afspraken,
   df_afspraken_inscope,
   abt_afspraken_part1,
   abt_afspraken_part2,
   abt_afspraken_part3,
   abt_afspraken_part4,
   abt_afspraak)
