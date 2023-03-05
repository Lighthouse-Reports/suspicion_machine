#--------------------------------------------------------------------------------------------------
# BESCHRIJVING 
# Maken features voor 'contacten'
# 
# WIJZIGINGSHISTORIE
# 17-02-2017, Medewerker1, initiele versie.
# 08-05-2017, Medewerker1, code rewrite + aanpassing key van persoon_id naar id
# 13-09-2017, Medewerker3, toevoegen tardis en nieuwe data
# 19-01-2018, Medewerker1, controledatum toegevoegd
# 23-11-2018, Medewerker4, indicator huidige naar datatype integer i.v.m. iml
# 21-07-2020, Medewerker4, kleine aanpassingen
#--------------------------------------------------------------------------------------------------

df_kinderen <- read_feather(paste0(datafolder, "relatie_kind.feather"))

df_kinderen_inscope <- df_kinderen %>% 
  tardis() %>%
  filter(is.na(dteinde) | (dteinde > melding_om - historie)) %>%  ## informatie ouder dan historie verwijderen
  mutate(dteinde = pmin(dteinde, melding_om),
         huidige = as.integer(melding_om <= dteinde | (!is.na(dtaanvang) & is.na(dteinde))))

controledatum <- max(df_kinderen_inscope$dtopvoer) #datum actualiteit tabel


#### Deel 1: aantal kinderen actueel
abt_kinderen_part1 <- df_kinderen_inscope %>%
  filter(huidige == 1) %>%
  group_by(id) %>%
  summarize(relatie_kind_huidige_aantal_regels = n(),
            relatie_kind_huidige_aantal = length(unique(subjectnrrelatie)),
            relatie_kind_heeft_kinderen = as.integer(n() > 0)) 
  

#### Deel 11: aantal kinderen historie
abt_kinderen_part12 <- df_kinderen_inscope %>%
  group_by(id) %>%
  summarize(relatie_kind_historie_aantal_regels = n(),
            relatie_kind_historie_aantal = length(unique(subjectnrrelatie)))

    
#### Deel 2: aantal per lft_aanvang_klasse
abt_kinderen_part2 <- df_kinderen_inscope %>%
  mutate(leeftijdverschil = as.integer(substr(as.character(relatie_gebjaarmaand), 1, 4)) -
           as.integer(substr(as.character(persoon_gebjaarmaand), 1, 4))) %>%
  group_by(id) %>%
  summarise(relatie_kind_leeftijd_verschil_ouder_eerste_kind = min(leeftijdverschil),
            relatie_kind_leeftijd_verschil_ouder_laatste_kind = max(leeftijdverschil))
  
#### Deel 3: aantallen contacten naar onderwerp
abt_kinderen_part3 <- df_kinderen_inscope %>%
  mutate(leeftijd_kind = as.integer(substr(as.character(melding_om), 1, 4)) -
           as.integer(substr(as.character(relatie_gebjaarmaand), 1, 4)),
         leeftijd_groep = case_when(leeftijd_kind < 4  ~ "peuter",
                                    leeftijd_kind < 12 ~ "basisschool_kind",
                                    leeftijd_kind < 18 ~ "tiener",
                                    leeftijd_kind < 27 ~ "jongvolwassen",
                                    TRUE ~ "volwassen"),
         leeftijd_groep = paste0("relatie_kind_", leeftijd_groep)) %>%
  count(id, leeftijd_groep) %>% 
  spread(leeftijd_groep, n, fill = 0L)

#### pleegkinderen
abt_kinderen_part4 <- df_kinderen_inscope %>%
  filter(grepl("pleeg", soort)) %>%
  select(id) %>%
  distinct() %>%
  mutate(relatie_kind_pleegkind = 1)

#### SAMENVOEGEN onderdelen
abt_kind <- ref %>%
  mutate(relatie_kind_controledatum = controledatum) %>%
  left_join0(abt_kinderen_part1) %>%
  left_join0(abt_kinderen_part12) %>%
  left_join0(abt_kinderen_part2) %>%
  left_join0(abt_kinderen_part3) %>%
  left_join0(abt_kinderen_part4) %>% 
  select(-type, -persoon_id, -target_per, -melding_om) # niet nodig in features tabel

#### Wegschrijven featuretabel
abt_kind %>%
  write_feather(paste0(featuredatafolder, "features_relatie_kind.feather"))
    
## Opruimen
rm(df_kinderen,
   df_kinderen_inscope,
   abt_kinderen_part1,
   abt_kinderen_part12,
   abt_kinderen_part2,
   abt_kinderen_part3,
   abt_kinderen_part4,
   abt_kind)
