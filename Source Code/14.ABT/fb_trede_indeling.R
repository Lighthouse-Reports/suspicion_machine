#****************************************************************************************************
# DESCRIPTION 
# Maken features voor 'trede'
# 
#
# FILE HISTORY
# 14-02-2017, Medewerker5; initiele versie.
# 01-05-2017: Medewerker1; code rewrite + aanpassing key van persoon_id naar id
#
#****************************************************************************************************


Sys.setenv(TZ = "CET")
options(stringsAsFactors = F)


# ref <- read_feather(paste(datafolder, "ref.feather", sep = "/"))
df_trede <- read_feather(paste(datafolder, "trede_indeling.feather", sep = "/")) %>%
  in_scope() %>%
  filter(!is.na(dtopvoer)) %>%
  filter(is.na(dteinde) | (dteinde > melding_om - historie)) ## informatie ouder dan historie verwijderen
  

### features trede indeling

# count per trede
abt_trede_1 <- df_trede %>% 
  mutate(trede = paste0("trede_cnt_", trede)) %>%
  reshape2::dcast(id ~ trede, fun.aggregate = length, value.var = "trede") %>%
  mutate(test = rowSums(select_(., "-id")) == (ncol(.)-1))

# aantal tredes totaal
abt_trede_2 <- df_trede %>% 
  group_by(id) %>%
  summarise(trede_count = n())

# meest recente trede
abt_trede_3 <- df_trede %>% 
  group_by(id) %>%
  arrange(desc(dtaanvang)) %>%
  slice(1) %>%
  select(id, trede)

# actieve trede (dteinde is NA)
abt_trede_4 <- df_trede %>% 
  group_by(id) %>%
  filter(is.na(dteinde)) %>% 
  mutate(trede_actief_ind = 1L, trede_actief = trede) %>% 
  select(id, trede_actief_ind, trede_actief)

#### SAMENVOEGEN onderdelen
abt_trede <- ref %>%
  left_join0(abt_trede_1) %>%
  left_join0(abt_trede_2) %>%
  left_join0(abt_trede_3) %>%
  left_join0(abt_trede_4)

#### Wegschrijven featuretabel
abt_trede %>%
  write_feather(paste(datafolder, "features_trede_indeling.feather", sep="/"))
   