#--------------------------------------------------------------------------------------------------
# BESCHRIJVING 
# Maken features voor 'component'.
# 
# WIJZIGINGSHISTORIE
# 20-02-2017, Medewerker1, initiele versie
# 08-05-2017, Medewerker1, code rewrite + aanpassing key van persoon_id naar id
# 30-06-2017, Medewerker1, kostendeler toegevoegd
# 17-09-2017, Medewerker3, tardis en nieuwe data
# 19-01-2018, Medewerker1, controledatum toegevoegd
# 23-11-2018, Medewerker4, ind component kostendeler naar datatype integer i.v.m. iml
# 22-07-2020, Medewerker4, format_categories() toegepast, afkap op 95
#--------------------------------------------------------------------------------------------------

df_component <- read_feather(paste0(datafolder, "component.feather")) 

df_component_inscope <- df_component %>%
  tardis() %>%
  filter(melding_om < dteinde + historie) %>% 
  mutate(huidige = dteinde >= melding_om)
#  mutate(dteinde = pmin(melding_om, dteinde))

controledatum <- max(df_component_inscope$dtopvoer) #datum actualiteit tabel


#### Deel 0: wel of niet kostendeler
abt_component_part0 <- df_component_inscope %>%
  mutate(kostendeler = grepl("Kostendeler", omschrijving)) %>%
  filter((dteinde+31) >= melding_om) %>% #selecteer laatste en voorlaatste maand start onderzoek
  group_by(id) %>%
  summarize(component_kostendeler = as.integer(any(kostendeler))) %>%
  select(id, component_kostendeler)


#### Deel 1: aantalen per component
abt_component_part1 <- df_component_inscope %>%
  mutate(omschrijving_cat = categorize(omschrijving, afkap_p = 95),
         omschrijving_cat = format_categories(omschrijving_cat)) %>%
  count(id, omschrijving_cat) %>%
  spread(omschrijving_cat, n, fill = 0L)

#### Deel 2: aantalen per component laatste 12 maanden
abt_component_part2 <- df_component_inscope %>%
  filter(dteinde <= melding_om - 365) %>%
  mutate(omschrijving_cat = categorize(omschrijving, afkap_p = 95),
         omschrijving_cat = paste0("laatste12mdn_", format_categories(omschrijving_cat))) %>%
  count(id, omschrijving_cat) %>%
  spread(omschrijving_cat, n, fill = 0L)


#### Deel 3: meeste recente compontent
abt_component_part3 <- df_component_inscope %>%
  group_by(id) %>%
  filter(dteinde == max(pmin(dteinde, melding_om))) %>%
  ungroup() %>%
  mutate(omschrijving_cat = categorize(omschrijving, afkap_p = 95),
         omschrijving_cat = paste0("recentste_", format_categories(omschrijving_cat))) %>%
  count(id, omschrijving_cat) %>%
  spread(omschrijving_cat, n, fill = 0L)

#### Samenvoegen en wegschrijven featuretabel
abt_component <- ref %>%
  mutate(controledatum = controledatum) %>%
  left_join0(abt_component_part0) %>%
  left_join0(abt_component_part1) %>%
  left_join0(abt_component_part2) %>%
  left_join0(abt_component_part3) %>% 
  select(-type, -persoon_id, -target_per, -melding_om) %>%             # niet nodig in features tabel
  rename_at(vars(-one_of("id")), function(x) paste0("component_", x))  # prefix tabelnaam op alle kolommen behalve id

abt_component %>%
  write_feather(paste0(featuredatafolder, "features_component.feather"))

## Opruimen
rm(df_component,
   df_component_inscope,
   abt_component_part0,
   abt_component_part1,
   abt_component_part2,
   abt_component_part3,
   abt_component)
