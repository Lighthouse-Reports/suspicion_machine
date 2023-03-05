#****************************************************************************************************
# DESCRIPTION 
# Maken features voor 'werkervaring_uwv
# 
#
# FILE HISTORY
# 14-02-2017, Medewerker3; initiele versie.
# 19-04-2017: Medewerker1; code rewrite + aanpassing key van persoon_id naar id
# 14-09-2017: Medewerker1, omschrijven zodat script werkt met tardis
#
#****************************************************************************************************


Sys.setenv(TZ = "CET")
options(stringsAsFactors = F)


# ref <- read_feather(paste(datafolder, "ref.feather", sep = "/"))

df_all <- read_feather(paste(datafolder, "werkervaring_uwv.feather", sep = "/"))
vak <- read.csv(paste(datafolder, "lookup_vak.csv", sep = "/"), header = F)

df <- inner_join(ref, df_all) %>%
  mutate(tekst = tolower(tekst))


# features van tijdveld en andere meta data
#df <- df_all%>% in_scope() geen datum velden 

abt_werk <- df %>% 
  group_by(id)%>%
  summarise(werkervaring_uwv_aantal_verschillende_beroepen = length(unique(tekst)),
            werkervaring_uwv_totaal_aantal_jaren_werkzaam_in_beroepen = sum(aantal_jr_werkzaam_in_beroep, na.rm=T),
            werkervaring_uwv_gemiddeld_aantal_jaren_werkzaam_in_beroep = 
              round(werkervaring_uwv_totaal_aantal_jaren_werkzaam_in_beroepen/ 
                      werkervaring_uwv_aantal_verschillende_beroepen, digits = 1)) %>%
  mutate(werkervaring_uwv_ind_heeft_werkervaring_bij_uwv = 1L) 


# features van beroep zelf
# Medewerker1: kan onderstaande ook anders? Het ziet er nu wat geknutseld uit.
vak %<>% 
  as.matrix() %>% 
  as.vector() # vak is een lookup table van veel voorkomende termen in de beroepen. 
vak <- gsub(" ", "", vak)
for (i in vak){
  df$tekst[grepl(i, df$tekst)]<- i
}
df$tekst[!df$tekst %in% vak] <- "overige"

abt_soort_werk <- df %>% 
  mutate(tekst = paste("werkervaring_uwv_", tekst, sep="")) %>%
  count(id, tekst) %>%
  mutate(n = (n > 0) * 1L) %>%
  spread(tekst, n, fill = FALSE) %>%
  ungroup()


# voeg verschillende werk_abts samen
abt <- ref %>%
  left_join0(abt_soort_werk) %>%
  left_join0(abt_werk)



#### Wegschrijven featuretabel
abt %>%  
  write_feather(paste(datafolder, "features_werkervaring_uwv.feather", sep="/"))
    
  