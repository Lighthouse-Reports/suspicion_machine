#***************************************************************
# FILE HISTORY
# 28-06-2017 Medewerker4: initiele versie
# 16-08-2017 Medewerker4: uitbreiding
# 02-07-2018 Medewerker4: migratie naar WOB server
# 04-07-2018 Medewerker1: Fix doubles-->integer, beperking inlezen tabellen naar alleen maar te gebruiken tabellen
# 08-05-2020 Medewerker4: ingesteld voor Prestatie010 model en omgebouwd naar direct lezen uit Oracle
# 05-06-2020 Medewerker4: download X_WEIFRA_CONTACT in 3 delen vanwege error in feather bij grote files
#***************************************************************

#--------------------------------------------------------------------------------------------------
# Init
#--------------------------------------------------------------------------------------------------
library(feather)
library(lubridate)
library(tidyverse)
library(readxl)
#library(odbc)
#library(DBI)
library(getPass)
#library(magrittr)
library(testthat)
library(ROracle)


options(stringsAsFactors = F)

source("D:/DATA/SHARED/Analytics_Uitkeringsfraude/1.Code/10.Functies/init.R")

setwd(root)

#--------------------------------------------------------------------------------------------------
# Set variables
#--------------------------------------------------------------------------------------------------
#extractie_datum <- format(Sys.Date(), format = "%Y%m%d")
extractie_datum <- "20200929"  # NB: hardcoded, omdat extractie op een ander moment in de Citrix omgeving gedaan is
controle_datum <- "20190815" # Extractiedatum Vorige run om mee te vergelijken
conn_db   <- "DWHPRD" #  "DWHONT"  "DWHPRD
conn_host <- "sdrd12.sdr.rotterdam.nl"  # ONT: sdrd07.sdr.rotterdam.nl, PRD: sdrd12.sdr.rotterdam.nl
conn_port <- "1521"
conn_usr  <- "XTRWEIFRASELECT"
conn_msg  <- paste("Wachtwoord", conn_usr, "op", conn_db, ":")
conn_pwd  <- getPass(msg = conn_msg)

# loop door alle tabellen die in XTRWEIFRA staan, lees ze uit en schrijf naar bestand
# NB.: X_WEIFRA_CONTACT gaat in 3 delen
tabel_lijst <- data.frame(TABLE_NAME = c(
  'X_WEIFRA_ADRES',
  'X_WEIFRA_AFSPRAAK',
  'X_WEIFRA_BELEMMERING',
  'X_WEIFRA_BESCHIKBAARHEID',
  'X_WEIFRA_COMPETENTIE',
  'X_WEIFRA_COMPONENT',
  'X_WEIFRA_CONTACT1',
  'X_WEIFRA_CONTACT2',
  'X_WEIFRA_CONTACT3',
  'X_WEIFRA_DEELNAME_ACT',
#  'X_WEIFRA_DIENST',    # Doen we niets mee
#  'X_WEIFRA_DOELGROEP', # Doen we niets mee
  'X_WEIFRA_FRAUDE',
#  'X_WEIFRA_INKOMEN',   # Doen we niets mee
  'X_WEIFRA_INSTRUMENT',
  'X_WEIFRA_ONTHEFFING',
  'X_WEIFRA_PERSOON',
  'X_WEIFRA_PERS_EIGENSCHAP',
  'X_WEIFRA_PLA',
  'X_WEIFRA_RELATIE_KIND',
  'X_WEIFRA_RELATIE_OVERIG',
  'X_WEIFRA_RELATIE_PARTNER',
  'X_WEIFRA_TYPERING',
  'X_WEIFRA_UITKERING',
#  'X_WEIFRA_WERKERVARING',  # Doen we niets mee
  'X_WEIFRA_WERKOPDRACHT'
))
#tabel_lijst$query <- paste("SELECT * FROM", tabel_lijst$TABLE_NAME)

# Loop om alle tabellen stuk voor stuk uit te lezen. Telkens open/close connectie zodat proces na een time-out niet stopt.
for (i in 1:length(tabel_lijst$TABLE_NAME)) {
  cat(paste0(i, ": ", tabel_lijst$TABLE_NAME[i]))
  # Database connectie openen
  #dbconn <- dbConnect(odbc::odbc(), dsn = conn_db, uid = conn_usr, pwd = conn_pwd)
  dbconn <- dbConnect(dbDriver("Oracle"),
                      dbname = paste0("(DESCRIPTION=(ADDRESS=(PROTOCOL=TCP) (HOST=", conn_host, ") (PORT=", conn_port, ")) (CONNECT_DATA=(SID=", conn_db, ")))"),
                      username = conn_usr,
                      password = conn_pwd)
  # Tabel uitlezen 
  #if (tabel_lijst$TABLE_NAME[i] != "X_WEIFRA_CONTACT") {
  if (!grepl("X_WEIFRA_CONTACT", tabel_lijst$TABLE_NAME[i])) {
    df <- dbReadTable(dbconn, tabel_lijst$TABLE_NAME[i])
  } else {
    # Twee bijzonderheden met X_WEIFRA_CONTACT:
    # - veld OPMERKINGEN moeten we met een substring gedeeltelijk inlezen, anders stopt hij om onduidelijke redenen op
    #   de error ORA-24920: column size too large for client
    # - deel 1, 2 of 3 van X_WEIFRA_CONTACT vertalen naar de juiste where conditie
    whereclause <- switch(str_sub(tabel_lijst$TABLE_NAME[i], -1), 
                          "1" = " <= 2014",
                          "2" = " between 2015 and 2017",
                          "3" = " >= 2018")
    df <- dbGetQuery(dbconn, paste0("select X_WEIFRA_CONTACT_ID, BSN, CONTACT_NR, DATUM_VASTLEGGING, TOELICHTING_ONDERWERP, substr(OPMERKINGEN, 1, 2000) OPMERKINGEN, 
                                    SOORT_CONTACT_OMSCHRIJVING, ONDERWERP, JN_DATETIME, JN_OPERATION, MTG_BRON, MTG_DATUM, MTG_LOB_ID, MTG_GELDIG_VAF, MTG_GELDIG_TEM 
                                    from X_WEIFRA_CONTACT where extract(year from JN_DATETIME)", whereclause))
  }
  doublevars <- names(df)[sapply(df, is.numeric)] # character vector met doubles (geen is.double() gebruiken want dan includeer je de datetimes)
  doublevars <- doublevars[!grepl(pattern = "X_WEIFRA_*", doublevars)] # ID veld niet converteren omdat onzeker is of dit integer of double wordt. Nu blijft het altijd double.
  # DB connectie weer sluiten
  dbDisconnect(dbconn)
  # Tabel wegschrijven als feather
  cat("; data wegschrijven...\n")
  df <- df %>%
    mutate_at(vars(one_of(doublevars)), as.character) %>%  # ORACLE kent geen integer. Doubles converteren naar character
    mutate_at(vars(one_of(doublevars)), type.convert) %>%  # Daarna converteren naar integer
    mutate_if(is.character, iconv) 
  df %>%
    with_tz(tzone = "CET") %>% # expliciet transformeren naar CET, anders slaat feather hem 1-op-1 op als UTC
    write_feather(paste0(bronfolder, tabel_lijst$TABLE_NAME[i], "_", extractie_datum, "_", conn_db, ".feather"))
  # oude tabel inladen voor testcases en testcases uitvoeren
  #dfold <- read_feather(paste0(bronfolder, tabel_lijst$TABLE_NAME[i], "_", controle_datum, "_", odbc_dsn, ".feather"))
  #test_file("1.Code/11.Extractie/testcases.R")
  #rm(df, dfold, doublevars)
  rm(df, doublevars)
}

# testcases voor vergelijking met vorige run in aparte loop (niet alle tabellen zijn van toepassing)
# NB.: X_WEIFRA_CONTACT weer opbouwen uit 3 delen
check_lijst <- data.frame(TABLE_NAME = c(
 'X_WEIFRA_ADRES',
 'X_WEIFRA_AFSPRAAK',
 'X_WEIFRA_BELEMMERING',
 'X_WEIFRA_BESCHIKBAARHEID',
 'X_WEIFRA_COMPETENTIE',
 'X_WEIFRA_COMPONENT',
 'X_WEIFRA_CONTACT',
  'X_WEIFRA_DEELNAME_ACT',
  #'X_WEIFRA_DIENST',        # nvt
  #'X_WEIFRA_DOELGROEP',     # nvt
  'X_WEIFRA_FRAUDE',
  #'X_WEIFRA_INKOMEN',       # nvt
  'X_WEIFRA_INSTRUMENT',
  'X_WEIFRA_ONTHEFFING',
  'X_WEIFRA_PERSOON',
  'X_WEIFRA_PERS_EIGENSCHAP',
  'X_WEIFRA_PLA',
  'X_WEIFRA_RELATIE_KIND',
  'X_WEIFRA_RELATIE_OVERIG',
  'X_WEIFRA_RELATIE_PARTNER',
  'X_WEIFRA_TYPERING',
  'X_WEIFRA_UITKERING'
  #'X_WEIFRA_WERKERVARING',  # nvt
  #'X_WEIFRA_WERKOPDRACHT'   # nvt
))

for (i in 1:length(check_lijst$TABLE_NAME)) {
  cat(paste0(i, ": ", check_lijst$TABLE_NAME[i]))
  if (check_lijst$TABLE_NAME[i] != "X_WEIFRA_CONTACT") {
    df <- read_feather(paste0(bronfolder, check_lijst$TABLE_NAME[i], "_", extractie_datum, "_", conn_db, ".feather"))
  } else {
    df <- bind_rows(read_feather(paste0(bronfolder, "X_WEIFRA_CONTACT1", "_", extractie_datum, "_", conn_db, ".feather")),
                    read_feather(paste0(bronfolder, "X_WEIFRA_CONTACT2", "_", extractie_datum, "_", conn_db, ".feather")),
                    read_feather(paste0(bronfolder, "X_WEIFRA_CONTACT3", "_", extractie_datum, "_", conn_db, ".feather")))
  }
  # oude tabel inladen voor testcases en testcases uitvoeren
  dfold <- read_feather(paste0(bronfolder, check_lijst$TABLE_NAME[i], "_", controle_datum, "_", conn_db, ".feather"))
  test_file("1.Code/11.Extractie/testcases.R")
  #rm(df, dfold)
}
