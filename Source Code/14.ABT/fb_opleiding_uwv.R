#--------------------------------------------------------------------------------------------------
# BESCHRIJVING 
# Maken features opleiding_uwv tabel
#
# WIJZIGINGSHISTORIE
# 13-02-2017, Medewerker2, feature building adres
# 08-05-2017, Medewerker1, code rewrite + aanpassing key van persoon_id naar id
# 11-09-2017, Medewerker1, omschrijven zodat script werkt met tardis
#--------------------------------------------------------------------------------------------------

df_opleiding_uwv <- read_feather(paste0(datafolder, "opleiding_uwv.feather")) %>%
  in_scope()

#****************************************************************************************************
#features opleiding_uwv maken
#****************************************************************************************************


# 1: aantal opleidingen, diploma's en ratio
abt_opleiding_1 <- df_opleiding_uwv %>%
  group_by(id) %>%
  summarise(uwv_opleiding_genoteerd = n(),
            uwv_opleiding_diploma = sum(diploma %in% "JA")) %>%
  mutate(uwv_opleiding_diploma_ratio = uwv_opleiding_diploma / uwv_opleiding_genoteerd)
  


# 2: soort opleidingen
abt_opleiding_2 <- df_opleiding_uwv %>%
  mutate(soort_opleiding = ifelse(grepl("basisschool|lagere school|vso|lom|zmok", tefst), "uwv_opleiding_aantal_basisschool", NA),
         soort_opleiding = ifelse(grepl("mavo|vbo|lbo|lts|vmbo|havo|vakopl|llw|vmto|basisberoeps|lhno|leao|ulo|lds", tefst), "uwv_opleiding_aantal_middelbaar", soort_opleiding),
         soort_opleiding = ifelse(grepl("mbo|mts|meao|llw tertiar|mdgo", tefst), "uwv_opleiding_aantal_mbo", soort_opleiding),
         soort_opleiding = ifelse(grepl("hbo|hts|heao|hsao", tefst), "uwv_opleiding_aantal_hbo", soort_opleiding),
         soort_opleiding = ifelse(grepl("wo", tefst), "uwv_opleiding_aantal_wo", soort_opleiding),
         soort_opleiding = ifelse(grepl("cursus", tefst), "uwv_opleiding_aantal_cursus", soort_opleiding),
         soort_opleiding = ifelse(is.na(soort_opleiding), "uwv_opleiding_aantal_overig", soort_opleiding)) %>%
  count(id, soort_opleiding) %>%
  ungroup() %>%
  spread(soort_opleiding, n, fill = 0L)
         

abt <- ref %>%
  left_join0(abt_opleiding_1) %>%
  left_join0(abt_opleiding_2)
  
abt %>%
  write_feather(paste0(datafolder, "features_opleiding_uwv.feather"))

