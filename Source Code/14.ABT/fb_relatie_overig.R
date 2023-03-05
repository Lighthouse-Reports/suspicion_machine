#--------------------------------------------------------------------------------------------------
# BESCHRIJVING 
# Maken features partner tabel
#
# WIJZIGINGSHISTORIE
# 13-02-2017, Medewerker2, feature building adres
# 26-04-2017, Medewerker1, code rewrite + aanpassing key van persoon_id naar id
# 13-09-2017, Medewerker3, toevoegen Tardis
# 19-01-2018, Medewerker1, controledatum toegevoegd
# 23-11-2018, Medewerker4, ind kostendeler/bewindvoerder naar datatype integer i.v.m. iml
# 17-07-2020, Medewerker4, format_categories() toegepast, kleine wijzigingen
#--------------------------------------------------------------------------------------------------

df_overig <- read_feather(paste0(datafolder, "relatie_overig.feather"))

df_overig_inscope <- tardis(df_overig) %>%
  filter(is.na(dteinde) | (dteinde > melding_om - historie)) %>%  ## informatie ouder dan historie verwijderen
  mutate(huidige = as.integer(dteinde>melding_om | (!is.na(dtaanvang) & is.na(dteinde))),
         dteinde = pmin(dteinde, melding_om, na.rm = T)) %>%
  separate(soort, into = c("subject", "vorm"), sep = "->")

controledatum <- max(df_overig_inscope$dtopvoer) #datum actualiteit tabel

#****************************************************************************************************
# STEP: maken van features
#****************************************************************************************************

abt_part0 <- df_overig_inscope %>%
  mutate(kostendeler = grepl("kostendeler", .$vorm),
         bewindvoerder = grepl("bewindvoerder", .$vorm)) %>% 
  filter((dteinde + 30) > melding_om) %>%
  group_by(id) %>%
  summarize(relatie_overig_kostendeler = as.integer(any(kostendeler)),
            relatie_overig_bewindvoerder = as.integer(any(bewindvoerder)))

  
# stap1: FEATURES relatie overige actueel
abt_part1 <- df_overig_inscope %>%
  filter(huidige, !is.na(categorie)) %>%
  mutate(categorie = categorize(categorie),
         categorie = paste0("relatie_overig_actueel_categorie_", format_categories(categorie))) %>%
  count(id, categorie) %>%
  ungroup() %>%
  spread(categorie, n, fill = 0L)

# stap1b: FEATURES relatie overige historie
abt_part12 <- df_overig_inscope %>%
  filter(!is.na(categorie)) %>%
  mutate(categorie = categorize(categorie),
         categorie = paste0("relatie_overig_historie_categorie_", format_categories(categorie))) %>%
  count(id, categorie) %>%
  ungroup() %>%
  spread(categorie, n, fill = 0L)


# stap2: FEATURES subject actueel
abt_part2 <- df_overig_inscope %>%
  filter(huidige, !is.na(subject)) %>%
  mutate(subject = categorize(subject),
         subject = paste0("relatie_overig_actueel_subject_", format_categories(subject))) %>%
  count(id, subject) %>%
  ungroup() %>%
  spread(subject, n, fill = 0L)

# stap22: FEATURES subject historie
abt_part22 <- df_overig_inscope %>%
  filter(!is.na(subject)) %>%
  mutate(subject = categorize(subject),
         subject = paste0("relatie_overig_historie_subject_", format_categories(subject))) %>%
  count(id, subject) %>%
  ungroup() %>%
  spread(subject, n, fill = 0L)

# stap3: FEATURES vorm actueel
abt_part3 <- df_overig_inscope %>%
  filter(huidige, !is.na(vorm)) %>%
  mutate(vorm = categorize(vorm),
         vorm = paste0("relatie_overig_actueel_vorm_", format_categories(vorm))) %>%
  count(id, vorm) %>%
  ungroup() %>%
  spread(vorm, n, fill = 0L)

# stap3: FEATURES vorm historie
abt_part4 <- df_overig_inscope %>%
  filter(!is.na(vorm)) %>%
  mutate(vorm = categorize(vorm),
         vorm = paste0("relatie_overig_historie_vorm_", format_categories(vorm))) %>%
  count(id, vorm) %>%
  ungroup() %>%
  spread(vorm, n, fill = 0L)


# Samenvoegen en wegschrijven
abt_overig <- ref %>%
  mutate(relatie_overig_controledatum = controledatum) %>%
  left_join0(abt_part0) %>%
  left_join0(abt_part1) %>%
  left_join0(abt_part12) %>%
  left_join0(abt_part2) %>%
  left_join0(abt_part22) %>%
  left_join0(abt_part3) %>%
  left_join0(abt_part4) %>% 
  select(-type, -persoon_id, -target_per, -melding_om) # niet nodig in features tabel

abt_overig %>%
  write_feather(paste0(featuredatafolder, "features_relatie_overig.feather"))

## Opruimen
rm(df_overig,
   df_overig_inscope,
   abt_part0,
   abt_part1,
   abt_part12,
   abt_part2,
   abt_part22,
   abt_part3,
   abt_part4,
   abt_overig)
