#--------------------------------------------------------------------------------------------------
# BESCHRIJVING 
# Maken features partner tabel
#
# WIJZIGINGSHISTORIE
# 13-02-2017, Medewerker2, feature building partner
# 27-02-2017, Medewerker1, aanpassing gedaan ([1] toegevoegd op 2 plekken) omdat code vastliep op meerdere huidige partners.
# 26-04-2017, Medewerker1, code rewrite + aanpassing key van persoon_id naar id
# 13-09-2017, Medewerker3, rewrite en toevoegen tardis
# 19-01-2018, Medewerker1, controledatum toegevoegd
# 17-07-2020, Medewerker4, format_categories() toegepast, kleine wijzigingen
#--------------------------------------------------------------------------------------------------

df_partner <- read_feather(paste0(datafolder, "relatie_partner.feather"))

df_partner_inscope <- df_partner %>%
  tardis() %>% 
  filter(is.na(dteinde) | (dteinde > melding_om - historie)) %>%  ## informatie ouder dan historie verwijderen
  mutate(huidige = dteinde>=melding_om | (!is.na(dtaanvang) & is.na(dteinde)),
         dteinde = pmin(dteinde, melding_om, na.rm = T))

controledatum <- max(df_partner_inscope$dtopvoer) #datum actualiteit tabel

#****************************************************************************************************
# STEP: maken van features

#****************************************************************************************************

#part1: huidige categorie relatie

abt_part1 <- df_partner_inscope %>%
  filter(huidige) %>%
  select(id) %>%
  distinct() %>%
  mutate(huidige_relatie = 1)
  

# soort relaties
abt_part11 <- df_partner_inscope %>%
  mutate(soort = paste0("aantal_", format_categories(soort))) %>%
  group_by(id, soort) %>%
  summarise(waarde = n()) %>%
  spread(soort, waarde, fill = 0)
  
abt_part12 <- df_partner_inscope %>%
  mutate(soort = paste0("dagen_", format_categories(soort))) %>%
  group_by(id, soort) %>%
  summarise(tijd = sum(as.integer(dteinde - dtaanvang))) %>%
  spread(soort, tijd, fill = 0)

abt_part13 <- df_partner_inscope %>%
  filter(huidige) %>%
  mutate(soort = paste0("huidige_",format_categories(soort))) %>%
  group_by(id, soort) %>%
  summarise(waarde = n()) %>%
  spread(soort, waarde, fill = 0)


#part2: totaal aantal partners
abt_part2 <- df_partner_inscope %>%
  group_by(id) %>%
  dplyr :: summarise(aantal_partners = n())


#part3: aantal dagen huidig getrouwd
abt_part3 <- df_partner_inscope %>%
  filter(huidige) %>%
  group_by(id) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(huidige_dagen_partner = as.integer(dteinde - dtaanvang)) %>%
  select(id, huidige_dagen_partner)

#part3: totaal aantal dagen met een partner
abt_part4 <- df_partner_inscope %>%
  mutate(dagen_partner = as.integer(dteinde - dtaanvang)) %>%
  group_by(id) %>%
  summarise(totaal_dagen_partner = sum(dagen_partner)) %>%
  ungroup() %>%
  select(id, totaal_dagen_partner)

#part4: trouwleeftijd
abt_part5 <-  df_partner_inscope %>%
  filter(soort == "partner - partner (gehuwd)") %>%
  arrange(dtaanvang) %>%
  mutate(persoon_gebjaarmaand2 = substr(as.character(persoon_gebjaarmaand), 1, 4),
         trouwleeftijd = as.numeric(substr(as.character(dtaanvang), 1, 4)) - as.numeric(persoon_gebjaarmaand2),
         verschil_leeftijd_partners = abs(as.integer(as.Date(paste0(persoon_gebjaarmaand, "01"), format = "%Y%m%d")- 
                                                     as.Date(paste0(relatie_gebjaarmaand, "01"), format = "%Y%m%d")))) %>%
  group_by(id) %>%
  summarise(jongste_trouwleeftijd = min(trouwleeftijd),
            oudste_trouwleeftijd = max(trouwleeftijd),
            grootste_leeftijdsverschil_dagen = max(verschil_leeftijd_partners),
            kleinste_leeftijdsverschil_dagen = min(verschil_leeftijd_partners))

# Samenvoegen en wegschrijven
abt_partner <- ref %>%
  mutate(controledatum = controledatum) %>%
  left_join0(abt_part1) %>%
  left_join0(abt_part11) %>%
  left_join0(abt_part12) %>%
  left_join0(abt_part13) %>%
  left_join0(abt_part2) %>%
  left_join0(abt_part3) %>%
  left_join0(abt_part4) %>%
  left_join0(abt_part5) %>% 
  select(-type, -persoon_id, -target_per, -melding_om) %>%                   # niet nodig in features tabel
  rename_at(vars(-one_of("id")), function(x) paste0("relatie_partner_", x))  # prefix tabelnaam op alle kolommen behalve id

abt_partner %>%
  write_feather(paste0(featuredatafolder, "features_relatie_partner.feather"))

## Opruimen
rm(df_partner,
   df_partner_inscope,
   abt_part1,
   abt_part11,
   abt_part12,
   abt_part13,
   abt_part2,
   abt_part3,
   abt_part4,
   abt_part5,
   abt_partner)
