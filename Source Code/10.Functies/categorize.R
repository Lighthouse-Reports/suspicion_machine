#--------------------------------------------------------------------------------------------------
# BESCHRIJVING 
# Samenvoegen van kleine categorieen.
# Deze versie is compatible voor gebruik met dplyr/mutate (input/output is enkele vector).
#
# WIJZIGINGSHISTORIE
# 09-01-2017, Medewerker1, initiele versie
#--------------------------------------------------------------------------------------------------

library(ggplot2)

categorize <- function(input, afkap_p = 98, exploratie = FALSE, other = "other") {
  #functie voegt kleine categorieen samen.
  #input = de vector met categorieen die samengevoegd moeten worden
  #afkap_p = welk percentage van de oude categorieen moet ongewijzigd blijven
  #exploratie = of er een exploratie gedaan moet worden gedaan.  
  input <- data.frame(input = input)

  cumsum <- input %>%
    filter(!is.na(input)) %>%
    mutate(totaal = n()) %>%
    group_by(input, totaal) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    arrange(-n) %>%
    mutate(groupnum = row_number(),
           cumsum_p=cumsum(n)/totaal)
  
  if(exploratie){
    print(ggplot2::ggplot() + ggplot2::geom_point(data = cumsum, aes(groupnum, cumsum_p)))
    return(cumsum)
  } else {
    mutatietabel <- cumsum %>%
      mutate(nieuw=ifelse(cumsum_p <= afkap_p/100, input, other)) %>%
      select(input, nieuw) %>%
      rbind(c(NA, other))

    output <- input %>%
      mutate(rownum = row_number()) %>%
      left_join(mutatietabel, by = "input") %>%
      arrange(rownum)
      return(output$nieuw)
  }
}
