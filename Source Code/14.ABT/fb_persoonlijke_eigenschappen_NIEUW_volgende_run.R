#--------------------------------------------------------------------------------------------------
# BESCHRIJVING 
# Maken features voor 'persoonlijke_eigenschappen'
# 
# WIJZIGINGSHISTORIE
# 15-02-2017, Medewerker4, initiele versie.
# 16-02-2017, Medewerker3, script verder afmaken
# 08-05-2017, Medewerker1, code rewrite + aanpassing key van persoon_id naar id
# 14-09-2017, Medewerker1, Tardis
# 19-01-2018, Medewerker1, controledatum toegevoegd
# 23-11-2018, Medewerker4, vars uit varslist_isna naar datatype integer i.v.m. iml
# 11-07-2019, Medewerker2, -c naar one_of in regel 57 wegens "invalid argument to unary operator error"
# 22-07-2020, Medewerker4, in features "tijd" vervangen door "dagen", ind_verkl_goed_gedrag uit varlist_behouden verwijderd (AVG)
# 26-10-2020, Medewerker4, vulling bepaald als percentage van aantal (relevante) kolommen, typo varlist -> varslist, onnodige group/arrange/slice weg
#--------------------------------------------------------------------------------------------------

df_pers_eig <- read_feather(paste0(datafolder, "pers_eigenschap.feather")) %>%
  tardis()

controledatum <- as.Date(max(df_pers_eig$jn_datetime)) #datum actualiteit tabel


# van onderstaande vars alleen behouden of het ingevuld is ja of nee:
varslist_isna <- c("motivatie_opm", "flexibiliteit_opm", "doorzettingsvermogen_opm", 
                   "zelfstandigheid_opm", "initiatief_opm", "communicatie_opm", "presentatie_opm", 
                   "uiterlijke_verzorging_opm", "leergierigheid_opm", "houding_opm", 
                   "hobbies_sport", "opstelling", "soll_gedrag", "overige_opmerkingen", 
                   "spreektaal_anders", "diagnose", "advies")
# vars die factor worden (en daarna dummy)
varslist_isfactor <- c("nl_spreken", "nl_lezen", "nl_schrijven", "nl_begrijpen")
# overige te behouden vars:
varslist_behouden <- c("spreektaal", "ind_regulier_arbeidsritme", 
                       "ind_regulier_arbeidsritme", "ind_buiten_kantoortijden", "ind_activering_traject", 
                       "uitstroom_verw_vlgs_klant", "uitstroom_verw_vlgs_km", "taaleis_voldaan", 
                       "taaleis_spreekv_ok", "taaleis_luisterv_ok", "taaleis_gespreksv_ok", 
                       "taaleis_schrijfv_ok", "taaleis_leesv_ok")
# nieuwe vars die we aanmaken en willen behouden;
varslist_behouden_nieuw <- c("vulling", "dagen_sinds_opvoer", "dagen_sinds_taaleis")

abt_pers_eig <- df_pers_eig %>%
  # tel hoeveel van de velden (in varslist_*) NA zijn
  # NB: binnen rowSums() zit een sub-pipe select_at() %>% mutate_all()
  mutate(vulling = rowSums( 
    select_at(., .vars = vars(varslist_isna, varslist_isfactor, varslist_behouden)) %>% 
    mutate_all(.funs = function (x) {as.numeric(!is.na(x))})
  )) %>% 
  # daarna vulling naar percentage omrekenen
  mutate(vulling = round(100 * vulling / length(c(varslist_isna, varslist_isfactor, varslist_behouden)), 0),
         dagen_sinds_opvoer = as.integer(melding_om - as.Date(jn_datetime)),
         dagen_sinds_taaleis = as.integer(melding_om - as.Date(taaleis_voldaan_datum_ja))) %>%
  # numeriek dan wel factor maken
  mutate_at(varslist_isna, funs(as.integer(!is.na(.)))) %>%
  mutate_at(varslist_isfactor, funs(if_else(is.na(.), as.character(!is.na(.)), .))) %>%
  mutate_at(varslist_isfactor, funs(as.factor(.))) %>%
  select_at(vars("id", varslist_isna, varslist_isfactor, varslist_behouden, varslist_behouden_nieuw))

# factors omzetten naar dummy variabelen
abt_dummys <- abt_pers_eig %>% select(eval(varslist_isfactor)) %>% 
  predict(dummyVars("~.", data = ., fullRank = FALSE, sep = NULL), newdata = .) %>% 
  as.data.frame()

# dummy variabelen in plaats van originele
abt_pers_eig <- abt_pers_eig %>% select(-one_of(eval(varslist_isfactor))) %>% 
  bind_cols(abt_dummys)

names(abt_pers_eig)[-1] <- paste0("persoonlijke_eigenschappen_", names(abt_pers_eig)[-1])

abt_pers_eig <- ref %>%
  mutate(persoonlijke_eigenschappen_controledatum = controledatum) %>%
  left_join0(abt_pers_eig) %>% 
  select(-type, -persoon_id, -target_per, -melding_om) # niet nodig in features tabel

#### Wegschrijven featuretabel
abt_pers_eig %>% 
  write_feather(paste0(featuredatafolder, "features_persoonlijke_eigenschappen.feather"))

## Opruimen
rm(df_pers_eig,
   abt_dummys,
   abt_pers_eig)
