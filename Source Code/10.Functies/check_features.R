#--------------------------------------------------------------------------------------------------
# BESCHRIJVING 
# Controleren of er onregelmatigheden in de features zitten
#
# WIJZIGINGSHISTORIE
# 15-11-2017, Medewerker4, functie in apart script ondergebracht
#--------------------------------------------------------------------------------------------------

check_features <- function() {
  probleem = "Nee"
  if(!(length(features$id) == length(unique(features$id)))) {
    cat("id's niet uniek in featuretabel")
    probleem = "Ja"
  }
  if(length(ref$id) < length(unique(features$id))) {
    cat("Featuretabel bevat id's die niet in referentietabel voorkomen")
    probleem = "Ja"
  }
  if(length(ref$id) > length(unique(features$id))) {
    cat("Featuretabel bevat niet alle id's die in referentietabel voorkomen")
    probleem = "Ja"
  }
  if(probleem != "Ja"){
    cat("Dataset is in orde")
  }
}
