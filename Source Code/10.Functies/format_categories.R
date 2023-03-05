#--------------------------------------------------------------------------------------------------
# BESCHRIJVING 
# Characterstrings omvormen voordat ze als waarde in een dummy kolomnaam komen
# - leestekens en spaties vervangen door een underscore
# - alles omzetten naar lowercase
# - NB: diakrieten blijven behouden
#
# WIJZIGINGSHISTORIE
# 15-07-2020, Medewerker4, initiele versie
#--------------------------------------------------------------------------------------------------

format_categories <- function (x) {
  gsub("[[:punct:]]| ", "_", tolower(x))
}
