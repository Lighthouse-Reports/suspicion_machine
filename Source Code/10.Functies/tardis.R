#--------------------------------------------------------------------------------------------------
# BESCHRIJVING 
# Koppelen van tabel waar features uit gemaakt gaan worden aan de tabel met actuele subjecten.
#
# WIJZIGINGSHISTORIE
# 08-02-2017, Medewerker1, initiele versie
# 14-02-2017, Medewerker3, veranderen van row_number in 1:nrow vanwege errors
# 15-02-2017, Medewerker5, if-then-else constructie toegevoegd zodat de functie ook werkt als geen dtopvoer
# 15-02-2017, Medewerker3, aantalpersonen_na veranderd. Nu was dit een vrij verwarrend veld omdat de left_join veel lege velden creert. 
#                              Nu heeft veld een overzicht van de hoeveelheid mensen waar informatie voor aanwezig is.   
# 17-02-2017, Medewerker1, includeNA toegevoegd als argument. default is FALSE. Indien TRUE worden alle personen uit ref toegeveogd 
#                                 aan return table. Dit leidt dan mogelijk tot persoon_id's met alleen maar NA's.  
# 22-02-2017, Medewerker1, aanpassing dat melding_om groter moet zijn dan aanvang en opvoer ipv groter of gelijk aan
# 10-04-2017, Medewerker1, aanpassing om id key te maken ipv patient_id
# 01-09-2017, Medewerker1, Rewrite van de logica op basis van de Tardis voor Socrates en RAAK.
# 07-07-2020, Medewerker4, select(-mtg_datum) eruit, overbodig
#--------------------------------------------------------------------------------------------------

tardis <- function(tabel, basis = ref, includeNA = FALSE) {
 
  ### Stap 1: tabel joinen met referentietabel
  if (includeNA){ # wel of geen lege rijen teruggeven als van een persoon in reftabel niets bekend is?
    abtdf <- left_join(basis, tabel, by = "persoon_id")
  } else { 
  abtdf <- inner_join(basis, tabel, by  ="persoon_id")
  }
  
  # Stap 2: soort tabel bepalen en tardis uitvoeren.
  if ("jn_datetime" %in% colnames(tabel)) {
    cat("Tabel herkend als RAAK-tabel.\n")
    abtdf <- abtdf %>% 
      filter(jn_datetime < melding_om) %>%
      group_by(id, jn_nr) %>%
      arrange(jn_datetime, jn_operation) %>%
      slice(n()) %>%
      filter(!jn_operation %in% "DEL") %>%
      ungroup()
  } else if ("status" %in% colnames(tabel)) {
    cat("Tabel herkend als Socrates-tabel.\n")
    abtdf <- abtdf %>% 
      filter(dtopvoer < melding_om) %>%
      mutate(status = if_else(dtafvoer >= melding_om, "Vrijgegeven", status, status),
             dtafvoer = as.Date(ifelse(dtafvoer >= melding_om, NA, dtafvoer), origin = "1970-01-01")) %>%
      filter(!status %in% "Ingevoerd") %>%
      mutate(dtafvoer = as.Date(ifelse(status %in% "Oud", NA, dtafvoer), origin = "1970-01-01"),
             status = if_else(status %in% "Oud", "Vrijgegeven", status)) %>%
      filter(geldig %in% "GELDIG", status %in% "Vrijgegeven") %>%
      select(-geldig, -status)
  } else {
    cat("Tabel NIET herkend als RAAK of Socrates tabel. Tardis niet uitgevoerd\n")
  }
  
  ## Stap 3: beschrijving input
  aantalpersonen_voor <- length(unique(basis$persoon_id))
  aantalpersonen_na_info <- length(unique(basis$persoon_id[basis$persoon_id %in% tabel$persoon_id])) # van hoeveel mensen er uiteindelijke informatie wordt meegenomen
  cat(paste(aantalpersonen_voor, "personen in referentietabel.\n"))
  cat(paste(length(unique(tabel$persoon_id))), "personen in featuretabel.\n")
  if(sum(names(tabel) %in% names(ref %>% select(-persoon_id)))) {cat("WAARSCHUWING: Kolomnamen niet uniek\n")} #alleen persoon_id mag in beide tabellen voorkomen
  cat(paste("In de featuretabel is informatie beschikbaar van", aantalpersonen_na_info, "van de", aantalpersonen_voor, "personen in de referentietabel.\n"))
  
  return(abtdf)  
}
