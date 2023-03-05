#--------------------------------------------------------------------------------------------------
# BESCHRIJVING 
# Left join met argument om NA een andere waarde te geven. Standaard 0.
#
# WIJZIGINGSHISTORIE
# 17-02-2017, Medewerker1, initiele versie
# 03-05-2017, Medewerker1, Default fill gewijzigd van 0L naar FALSE. 
#--------------------------------------------------------------------------------------------------

left_join0 <- function(x, y, fill = FALSE) {
  z <- left_join(x, y)
  tmp <- setdiff(names(z), names(x))
  z <- replace_na(z, setNames(as.list(rep(fill, length(tmp))), tmp))
}
