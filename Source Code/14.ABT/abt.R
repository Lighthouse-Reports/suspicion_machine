#--------------------------------------------------------------------------------------------------
# BESCHRIJVING 
# Samenvoegen featuretabellen tot een abt.
#
# WIJZIGINGSHISTORIE
# 13-02-2017, Medewerker1; initiele versie.
# 13-02-2017, Medewerker2; adres toevoegen
# 14-02-2017, Medewerker3; aan check features functie postieve feedback toevoegen
# 14-02-2017, Medewerker3; beschikbaarheid, werkervaring_uwv toevoegen
# 15-02-2017, Medewerker2; relatie_overige, partner toevoegen
# 15-02-2017, Medewerker3; ontheffingen, typering toevoegen
# 15-02-2017, Medewerker4; belemmeringen toevoegen, dtlaad
# 15-02-2017, Medewerker5; opleiding_duo, competenties, trede_indeling & persoon toevoegen, dtlaad
# 16-02-2017, Medewerker3; uitkering, persoonlijke eigenschappen toevoegen
# 16-02-2017, Medewerker2; opleiding_uwv, huisvesting toevoegen
# 17-02-2017, Medewerker1, inschrijvingen_uwv, aanmelding_instrument, afspraken en kinderen toegevoegd.
# 17-02-2017, Medewerker3; aanpassen vna sys.source en enviroment
# 20-02-2017, Medewerker1: component toegevoegd.
# 21-02-2017, Medewerker5: fraude features toegevoegd.
# 03-03-2017, Medewerker5: fraude features 2 aangepast > issue met fraude features
# 10-04-2017, Medewerker1: grote rewrite.
# 15-06-2017, Medewerker3: Script testen op ABT voor pilot
# 01-08-2017, Medewerker3; ref terugschrijven in abt script zodat er altijd met de juiste ref wordt gewerkt.
# 10-08-2017, Medewerker3; type pilot bestand toevoegen.
# 01-09-2017, Medewerker1; Diverse wijzigingen om script gereed te maken voor iteratie3
# 22-09-2017, Medewerker3; Mogelijk gemaakt dat er verschillende refbestanden gebruikt kunnen worden
# 15-11-2017, Medewerker4, functie check_features() in apart script ondergebracht
# 21-12-2017, Medewerker3; toevoegen check NA's in abt en fix abt names
# 04-01-2018, Medewerker3; toevoegen checks op verschilende levels tussen training en scoring
# 10-01-2018, Medewerker3; init
# 02-07-2018, Medewerker4; migratie naar WOB server
# 25-07-2018, Medewerker6; ABT filteren op scope_doelgroep toegevoegd (W&I business rules)
# 21-07-2020, Medewerker4, soort_ref verwijderd, left_joins expliciet op "id", features instrument vervallen
# 19-08-2020, Medewerker4, op het einde aanmaken feature profiles toegevoegd
#--------------------------------------------------------------------------------------------------

#--------------------------------------------------------------------------------------------------
# Init
#--------------------------------------------------------------------------------------------------
library(tidyverse)
library(feather)
library(readxl)
library(lubridate)
library(caret) # t.b.v. dummyVar() functie

source("D:/DATA/SHARED/Analytics_Uitkeringsfraude/1.Code/10.Functies/init.R")

setwd(root)

#--------------------------------------------------------------------------------------------------
# Set variables
#--------------------------------------------------------------------------------------------------
historie <- 365*80 # aantal dagen historie meenemen voor maken features
env_s <- new.env()

#--------------------------------------------------------------------------------------------------
# Make ref
#--------------------------------------------------------------------------------------------------

sys.source(paste0(featurefolder, "ref_basis.R"), envir = env_s) 
abt <- ref <- read_feather(paste0(refdatafolder, "ref_basis_", label, ".feather")) 

#--------------------------------------------------------------------------------------------------
# Make features
#--------------------------------------------------------------------------------------------------

#### features_adres
sys.source(paste0(featurefolder, "fb_adres.R"), envir = env_s)
features <- read_feather(paste0(featuredatafolder, "features_adres.feather"))
check_features()
abt <- abt %>% left_join(features, by = "id")
rm(features)

#### features_contact
sys.source(paste0(featurefolder, "fb_contacten.R"), envir = env_s)
features <- read_feather(paste0(featuredatafolder, "features_contacten.feather"))
check_features()
abt <- abt %>% left_join(features, by = "id")
rm(features)

#### features_beschikbaarheid
sys.source(paste0(featurefolder, "fb_beschikbaarheid.R"), envir = env_s)
features <- read_feather(paste0(featuredatafolder, "features_beschikbaarheid.feather"))
check_features()
abt <- abt %>% left_join(features, by = "id")
rm(features)

#### features_relatie_partner
sys.source(paste0(featurefolder, "fb_relatie_partner.R"), envir = env_s)
features <- read_feather(paste0(featuredatafolder, "features_relatie_partner.feather"))
check_features()
abt <- abt %>% left_join(features, by = "id")
rm(features)

#### features_pla
sys.source(paste0(featurefolder, "fb_pla.R"), envir = env_s)
features <- read_feather(paste0(featuredatafolder, "features_pla.feather"))
check_features()
abt <- abt %>% left_join(features, by = "id")
rm(features)

#### features_relatie_overige
sys.source(paste0(featurefolder, "fb_relatie_overig.R"), envir = env_s)
features <- read_feather(paste0(featuredatafolder, "features_relatie_overig.feather"))
check_features()
abt <- abt %>% left_join(features, by = "id")

#### features_ontheffing
sys.source(paste0(featurefolder, "fb_ontheffing.R"), envir = env_s)
features <- read_feather(paste0(featuredatafolder, "features_ontheffing.feather"))
check_features()
abt <- abt %>% left_join(features, by = "id")
rm(features)

#### features_belemmering
sys.source(paste0(featurefolder, "fb_belemmering.R"), envir = env_s)
features <- read_feather(paste0(featuredatafolder, "features_belemmering.feather"))
check_features()
abt <- abt %>% left_join(features, by = "id")

#### features_typering
sys.source(paste0(featurefolder, "fb_typering.R"), envir = env_s)
features <- read_feather(paste0(featuredatafolder, "features_typering.feather"))
check_features()
abt <- abt %>% left_join(features, by = "id")

#### features_persoon
sys.source(paste0(featurefolder, "fb_persoon.R"), envir = env_s)
features <- read_feather(paste0(featuredatafolder, "features_persoon.feather"))
check_features()
abt <- abt %>% left_join(features, by = "id")

#### features_competenties, geen tardis mogelijk
sys.source(paste0(featurefolder, "fb_competentie.R"), envir = env_s)
features <- read_feather(paste0(featuredatafolder, "features_competentie.feather"))
check_features()
abt <- abt %>% left_join(features, by = "id")

#### features_persoonlijke eigenschappen: geen historie
sys.source(paste0(featurefolder, "fb_persoonlijke_eigenschappen.R"), envir = env_s)
features <- read_feather(paste0(featuredatafolder, "features_persoonlijke_eigenschappen.feather"))
check_features()
abt <- abt %>% left_join(features, by = "id")

#### features_kinderen
sys.source(paste0(featurefolder, "fb_relatie_kind.R"), envir = env_s)
features <- read_feather(paste0(featuredatafolder, "features_relatie_kind.feather"))
check_features()
abt <- abt %>% left_join(features, by = "id")
rm(features)

#### features_deelname
sys.source(paste0(featurefolder, "fb_deelname_act.R"), envir = env_s)
features <- read_feather(paste0(featuredatafolder, "features_deelname_act.feather"))
check_features()
abt <- abt %>% left_join(features, by = "id")
rm(features)

#### features_afspraken hoe historie inbouwen
sys.source(paste0(featurefolder, "fb_afspraak.R"), envir = env_s)
features <- read_feather(paste0(featuredatafolder, "features_afspraak.feather"))
check_features()
abt <- abt %>% left_join(features, by = "id")

####. features_aanmelding_instrument;
sys.source(paste0(featurefolder, "fb_instrument.R"), envir = env_s)
features <- read_feather(paste0(featuredatafolder, "features_instrument.feather"))
check_features()
abt <- abt %>% left_join(features, by = "id")

####. features_component hoe historie inbouwen
sys.source(paste0(featurefolder, "fb_component.R"), envir = env_s)
features <- read_feather(paste0(featuredatafolder, "features_component.feather"))
check_features()
abt <- abt %>% left_join(features, by = "id")

#### features_huisvesting  VERVALLEN m.i.v. 2020
#sys.source(paste0(featurefolder, "fb_huisvesting.R"), envir = env_s)
#features <- read_feather(paste0(featuredatafolder, "features_huisvesting.feather"))
#check_features()
#abt <- abt %>% left_join(features, by = "id")


#--------------------------------------------------------------------------------------------------
# checks and save
#--------------------------------------------------------------------------------------------------
abt_goed = TRUE

# controleer voor NA
nummer_na <- data.frame(features = names(abt), nummer_na = colSums(is.na(abt)))%>%
  filter(features != "target_per", nummer_na >0)

if (nrow(nummer_na)>0){
  warning("Het behouden van NA's zorgt voor errors in latere scripts")
  message("De volgende features hebben nog NA's, dit moet opgelost worden in feature scripts")
  nummer_na
  abt_goed = FALSE
} else{
  cat("De abt bevat geen NA's meer en is in orde")
}

# fix names abt
# EBE 18-08-2020: dit zou niet meer nodig moeten zijn, maar soms nog Boolean FALSE in hoofdletters
names(abt) <- names(abt) %>% 
  tolower()%>%
  str_trim(side = "both")%>%
  str_replace_all("[[:space:]]", "_")%>%
  str_replace_all("(?!_)[[:punct:][:blank:][+]]", "")

if(nrow(ref) == nrow(abt)){
  cat("Aantal rijen abt is gelijk aan aantal rijen ref, ABT is in orde")
} else {
  warning("Aantal rijen abt is niet gelijk aan aantal rijen ref")
  abt_goed = FALSE
}

### test of levels gelijk zijn tussen training en scoring
### vind alle features die characters zijn
abt_char <- abt %>% 
  select(
    abt %>%
      map_df(class)%>%
      gather(features, class)%>%
      filter(class %in% c("character"))%>%
      pull(features),
    -persoon_id, -id, -target_per 
  )

### vergelijk of deze verschillende levels hebben
verschillende_levels <- map2_df(
  abt_char %>%filter(type == "metlabel_traintest"), 
  abt_char %>%filter(type == "scoring"),
  function(df1,df2){
    any(!(
      unique(df2) %in% unique(df1)
    ))
  })%>%
  gather(features, verschillende_levels)

if (sum(verschillende_levels$verschillende_levels)>1){ ## 1 omdat type altijd verschillend is 
  warning("Er zijn nieuwe levels ontstaan bij scoring, dit zorgt voor problemen tijdens scoring")
  verschillende_levels%>%filter(verschillende_levels)%>%print()
  abt_goed = FALSE
}else{
  cat("Er zijn geen nieuwe levels in de scoringsabt en ABT is in orde")
}

if(abt_goed){
  cat("Alle checks zijn voldaan en ABT wordt opgeslagen")
  abt %>% write_feather(paste0(abtfolder, "abt_basis_", dtlaad ,"_", label, ".feather"))
} else{
  warning("Aan niet alle checks is voldaan. controleer meldingen")
}

#--------------------------------------------------------------------------------------------------
# Profile van alle features maken
#--------------------------------------------------------------------------------------------------
update_feature_profiles(abt, stap = "init")

# Maken rapportage
# NB: dit duurt even (10-15 min). Met feat_range kun je eventueel een beperktere set features aangeven.
rmarkdown::render(paste0(rapportfolder, "controlerapport_features.Rmd"), 
                  params=list(feat_range = c(5:length(abt))),  # eerste 4 niet (id, type, persoon_id, target_per)
                  output_dir = paste0(rapportfolder, label, "/"),
                  output_file = paste0(rapportfolder, label, "/controlerapport_features.html"))

