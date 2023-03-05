#--------------------------------------------------------------------------------------------------
# BESCHRIJVING 
# Maken features voor 'competenties'
# 
# WIJZIGINGSHISTORIE
# 15-02-2017, Medewerker5, initiele versie.
# 01-05-2017, Medewerker1, code rewrite + aanpassing key van persoon_id naar id
# 14-09-2017, Medewerker1, omschrijven zodat script werkt met tardis
# 23-10-2017, Medewerker4, geen JN tabel, dus tardis niet mogelijk
# 19-01-2018, Medewerker1, controledatum toegevoegd
# 23-11-2018, Medewerker4, indicatoren naar datatype integer i.v.m. iml
# 21-07-2020, Medewerker4, format_categories() toegepast, kleine wijzigingen, afkap_p = 85 vanwege vele categorieën
#--------------------------------------------------------------------------------------------------


df_competenties <- read_feather(paste0(datafolder, "competentie.feather")) 

df_competenties_inscope <- df_competenties %>%
  inner_join(ref, by='persoon_id') %>%
#  in_scope() %>% 
  filter(is.na(dteinde) | (dteinde > melding_om - historie)) %>%  ## informatie ouder dan historie verwijderen
  mutate(comp_huidig = as.integer(melding_om <= dteinde | (!is.na(dtaanvang) & is.na(dteinde))))

controledatum <- as.Date(max(df_competenties_inscope$mtg_datum)) #datum actualiteit tabel


### features competenties indeling

# counts per huidige competentie, indicatie voor huidige competentie, en aantal huidige 
abt_competenties_1 <- df_competenties_inscope %>%
  filter(comp_huidig == 1) %>% 
  mutate(comp_omschrijving = categorize(comp_omschrijving, afkap_p = 85),
         comp_omschrijving = paste0("competentie_", format_categories(comp_omschrijving))) %>%
  count(id, comp_omschrijving) %>%
  spread(key = comp_omschrijving, value = n, fill = 0L) %>%
  mutate(competentie_huidig_aantal = as.integer(rowSums(select(., contains("comp")))),
         competentie_huidig_ind = 1L)

# aantal afgesloten competenties verleden
abt_competenties_2 <- df_competenties_inscope %>%
  filter(comp_huidig == 0) %>%
  group_by(id) %>%
  summarise(competentie_verleden_ind = 1L, competentie_verleden_aantal = n())

# text aanwezig ja/nee?
abt_competenties_3 <- df_competenties_inscope %>%
  filter(!is.na(comp_toelichting)) %>% 
  group_by(id) %>%
  summarise(competentie_ind_tekst = 1L)


#### SAMENVOEGEN onderdelen
abt_competenties <- ref %>%
  mutate(competentie_controledatum = controledatum) %>%
  left_join0(abt_competenties_1) %>%
  left_join0(abt_competenties_2) %>%
  left_join0(abt_competenties_3) %>% 
  select(-type, -persoon_id, -target_per, -melding_om) # niet nodig in features tabel


#### Wegschrijven featuretabel
abt_competenties %>%
  write_feather(paste0(featuredatafolder, "features_competentie.feather"))

## Opruimen
rm(df_competenties,
   df_competenties_inscope,
   abt_competenties_1,
   abt_competenties_2,
   abt_competenties_3,
   abt_competenties)
