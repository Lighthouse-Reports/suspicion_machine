#--------------------------------------------------------------------------------------------------
# BESCHRIJVING 
# Maken features adres tabel
#
# WIJZIGINGSHISTORIE
# 13-02-2017, Medewerker2, feature building adres
# 10-04-2017, Medewerker1, code rewrite + aanpassing key van persoon_id naar id
# 06-09-2017, Medewerker3, omschrijven zodat script werkt met tardis
# 11-09-2017, Medewerker1, (zelfgeintroduceerd) foutje eruit gehaald. bepalen adres_aantal_verschillende_wijken
#             verplaatsen binnen de group_by(id) ipv erbuiten.
# 04-01-2018, Medewerker3, aantal features geven fouten. oplossinging met case_when 
# 19-01-2018, Medewerker1, controledatum toegevoegd
# 26-11-2018, Medewerker4, categorische vars omgezet in dummy's i.v.m. iml
# 17-07-2020, Medewerker4, dummy namen als lowercase en met spaties/leestekens als underscores, typo "recenste" -> "recentste", sort desc op dtaanvang voor slice(1)
#--------------------------------------------------------------------------------------------------

df_adres <- read_feather(paste0(datafolder, "adres.feather"))

#--------------------------------------------------------------------------------------------------
# features adres maken
#--------------------------------------------------------------------------------------------------

df_adres_inscope <- df_adres %>%
  tardis() %>%
  filter(melding_om < dteinde + historie) %>% 
  mutate(dteinde = pmin(melding_om, dteinde))

controledatum <- max(df_adres_inscope$dtopvoer) #datum actualiteit tabel


# recentste buurt, wijk en plaats en aantal dagen op dit adres, aantal adressen, gemiddeld aantal dagen op adres
onderdeel_rdam <- c("rotterdam", "hoogvliet rotterdam", "hoogvliet", "rozenburg", "rozenburg zh", "hoek van holland", "pernis rotterdam")   # plaats
onderdeel_zuid <- c("charlois", "hoogvliet", "rozenburg", "feijenoord", "ijsselmonde", "pernis", "waalhaven-e")                             # wijknaam

abt_adres_part1 <- df_adres_inscope %>%
  mutate(adres_dagen_op_adres = as.integer(dteinde - dtaanvang)) %>%
  group_by(id) %>%
  mutate(adres_aantal_adressen = n(),
         adres_aantal_verschillende_wijken = n_distinct(wijknaam),
         adres_gemiddeld_aantal_dagen = as.integer(sum(adres_dagen_op_adres) / adres_aantal_adressen)) %>%
  arrange(desc(dtaanvang)) %>%
  slice(1) %>% # neem meest recente adres
  ungroup() %>%
  mutate(adres_dagen_op_adres = as.integer(dteinde - dtaanvang),
         adres_recentste_plaats = categorize(.$plaats, afkap_p = 95),
         adres_recentste_buurt = categorize(.$buurtnaam, afkap_p = 60),
         adres_recentste_wijk = categorize(.$wijknaam, afkap_p = 95),
         adres_recentst_onderdeel_rdam = ifelse(plaats %in% onderdeel_rdam, 1L, 0L),
         adres_recentst_onderdeel_rdamzuid = ifelse(wijknaam %in% onderdeel_zuid, 1L, 0L),
         adres_unieke_wijk_ratio = adres_aantal_verschillende_wijken / adres_aantal_adressen) %>%
  select(id, contains("adres_"))

# aantal soort adres 
abt_adres_part2 <- df_adres_inscope %>%
  mutate(soort_adres = paste("adres_aantal", format_categories(soort_adres), sep="_")) %>%
  count(id, soort_adres) %>%
  spread(soort_adres, n, fill = 0L)

abt_adres <- ref %>%
  mutate(adres_controledatum = controledatum) %>%
  left_join0(abt_adres_part1) %>%
  left_join0(abt_adres_part2)

# spread plaats/buurt/wijk naar kolommen
# as.factor is nodig voor dummyVars()
abt_adres_spread <- abt_adres %>% 
  select(adres_recentste_plaats, adres_recentste_buurt, adres_recentste_wijk) %>% 
  mutate(adres_recentste_plaats = as.factor(format_categories(adres_recentste_plaats)),
         adres_recentste_buurt = as.factor(format_categories(adres_recentste_buurt)),
         adres_recentste_wijk = as.factor(format_categories(adres_recentste_wijk))) %>% 
  predict(dummyVars("~.", data = ., fullRank = FALSE, sep = "_"), newdata = .) %>% 
  as.data.frame()

# spread variabelen in plaats van originele
abt_adres <- abt_adres %>% select(-adres_recentste_plaats, -adres_recentste_buurt, -adres_recentste_wijk) %>% 
  bind_cols(abt_adres_spread) %>% 
  select(-type, -persoon_id, -target_per, -melding_om) # niet nodig in features tabel

abt_adres %>%
  write_feather(paste0(featuredatafolder, "features_adres.feather"))

## Opruimen
rm(df_adres,
   df_adres_inscope,
   abt_adres_part1,
   abt_adres_part2,
   abt_adres)
