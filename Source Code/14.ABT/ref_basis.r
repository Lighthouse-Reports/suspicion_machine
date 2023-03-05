#--------------------------------------------------------------------------------------------------
# BESCHRIJVING 
# Maken basis van abt en referentietabel (ref)
# Deze laatste wordt als basis gebruikt voor de records bij feature building
#
# WIJZIGINGSHISTORIE
# 13-02-2017, Medewerker1, initiele versie.
# 15-02-2017, Medewerker3, script aanpassen op nieuwe data
# 16-02-2017, Medewerker3, toevoegen van target bepaling en referentie aanpassen zonder aselect
# 21-02-2017, Medewerker5, toevoegen uitkeringstabel voor te scoren populatie met huidige uitkering
# 23-02-2017, Medewerker3, vervangen van de anti-join door een filter met grepl. Hierdoor worden enkel
#                             de aselecte onderzoeken uitgesloten en niet alle mensen die ooit een aselect onderzoek
#                             hebben gehad.
# 10-04-2017, Medewerker1, rewrite en combineren ref_training en ref_scoring in een tabel.
# 05-09-2017, Medewerker3, rewrite voor de nieuwe data
# 11-01-2018, Medewerker3, toevoegen validaie
# 16-08-2018, Medewerker4, Aselect onderzoeken 2018 deels gebruiken als validatie, deels als train/test
# 20-08-2019, Medewerker4, aselecte selectie uit alle afgeronde onderzoeken als validatieset +
#                          filter op soort_ltste_werkopdracht toegevoegd
# 21-08-2019, Medewerker4, filter df_fraude_all op fraudenummers zoals aangeleverd door Medewerker7
# 17-07-2020, Medewerker4, aanpassingen voor P010 run: fraude-onderzoeken nu o.b.v. tabel werkopdracht, indeling traintest/controle/scoring, id uitgebreid met type
# 29-09-2020, Medewerker4, selectie P010 uitgebreid met Locatie Schiekade
#--------------------------------------------------------------------------------------------------

# voor bepalen trainings data:
df_fraude_all <- read_feather(paste0(datafolder, "werkopdracht.feather")) 
# voor bepalen te scoren dataset:
df_uitkering <- read_feather(paste0(datafolder,"uitkering.feather"))

#--------------------------------------------------------------------------------------------------
# Target bepalen en selectie Prestatie010 groep
#--------------------------------------------------------------------------------------------------
# Targetbepaling:
# Er is sprake van onrechtmatigheid ALS:
# - Het veld 'Bedrag' IS NIET leeg  OF
# - Het veld 'Gevolgen' == "Beëindiging lopende uitkering" OF
# - Het veld 'Gevolgen' == "Beëindiging lopende uitkering ABO" OF
# - Het veld 'Gevolgen' == "Aanpassing lopende uitkering".
# Met andere woorden: als de uitkomst van het onderzoek was dat er een bedrag is teruggevorderd of de uitkering is aangepast of beëindigd, 
# dan wordt dit aangemerkt als onrechtmatigheid

df_fraude <- df_fraude_all %>%
  # Selectie Prestatie010
  filter(afdeling_klm_op_dtmelding %in% c("Locatie Delfshaven", "Locatie Herenwaard", "Locatie Schiekade")) %>% 
  # Bepaling target (onderzoek en persoon)
  mutate(gevolgen = ifelse(is.na(gevolgen), "geen gevolg bekend", gevolgen), # alle lege gevolgen voor gevolgen onbekend
         target_ond = case_when(!is.na(fraude_bedrag) ~ "Ja", 
                                grepl("lopende uitkering", gevolgen) ~ "Ja",
                                TRUE ~ "Nee")) %>%
  group_by(persoon_id) %>% # dit is het target per persoon
  mutate(n_tgt_per = sum(target_ond == "Ja"),
         target_per = case_when(sum(target_ond == "Ja") > 0 ~ "Ja",
                                TRUE ~ "Nee")) %>%
  ungroup()

#--------------------------------------------------------------------------------------------------
# STEP: set datum van melding voor alle gelabelde werkzoekenden.
# We nemen voor elke werkzoekende de datum van het eerste onderzoek met target Ja of kiezen een 
# random onderzoek indien alle onderzoeken target Nee hebben
#--------------------------------------------------------------------------------------------------

# datums wanneer de eerste positieve fraude melding/onderzoek is begonnen. 
df_fraude_group1 <- df_fraude %>% 
  filter(target_ond == "Ja") %>% 
  group_by(persoon_id) %>% 
  arrange(datum_melding) %>%
  slice(1) %>%
  ungroup()

# random uitkiezen van een melding om ook de mensen zonder om ook een datum van melding mee te geven. 
df_fraude_group2 <- df_fraude %>%
  filter(target_per == "Nee") %>%
  group_by(persoon_id) %>%
  sample_n(1) %>%
  ungroup()

# samenvoegen 
df_onderzoeken <-
  bind_rows(df_fraude_group1, df_fraude_group2) %>%
  rename(melding_om = datum_melding) 

#--------------------------------------------------------------------------------------------------
# STEP: bepalen subsets train, test, controle en scoring
# df_onderzoeken -> train + testset 
# df_onderzoeken -> filter aselect -> controleset
# df_uitkering   -> scoreset
#--------------------------------------------------------------------------------------------------

ref_traintest <- df_onderzoeken %>% 
  mutate(type = "metlabel_traintest",
         id = paste(persoon_id, melding_om, type, sep = "_")) %>% 
  select(id, type, persoon_id, target_per, melding_om) 

ref_controle <- df_onderzoeken %>% 
  filter(oms_project %in% c("HO2018_Aselect NPRZ", "HO2018_Aselect_Noordoever")) %>% 
  mutate(type = "metlabel_controle",
         id = paste(persoon_id, melding_om, type, sep = "_")) %>% 
  select(id, type, persoon_id, target_per, melding_om) 

ref_scoring <- df_uitkering %>% 
  # Selectie Prestatie010
  filter(afdeling_werk %in% c("Locatie Delfshaven", "Locatie Herenwaard", "Locatie Schiekade")) %>% 
  # Lopende uitkeringen
  filter(dteindedienst > dtlaad + 31, ## als iemand binnen een maand al uitstroomt is het ook niet meer de moeite waard.  
         is.na(dtbeginstopzetting),
         srtdienst_omschrijving == "Participatiewet",
         besluit == "Toekennen") %>%
  select(persoon_id) %>%
  distinct() %>%
  mutate(melding_om = dtlaad, 
         target_per = NA,
         type = "scoring",
         id = paste(persoon_id, melding_om, type, sep = "_")) %>% 
  select(id, type, persoon_id, target_per, melding_om)


#--------------------------------------------------------------------------------------------------
# Append traintest + controle + scoring
#--------------------------------------------------------------------------------------------------

ref_basis <- ref_traintest %>%
  bind_rows(ref_controle) %>% 
  bind_rows(ref_scoring)

ref_basis %>% write_feather(paste0(refdatafolder, "ref_basis_", label, ".feather"))
