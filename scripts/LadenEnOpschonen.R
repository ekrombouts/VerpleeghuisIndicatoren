# Datum: Augustus 2022
# Auteur: Eva Rombouts
# Oefenproject in het kader van leren R en R Studio
# Voornaamste gebruikte bronnen voor het leren van R: 
# Coursera cursus Johns Hopkins, R Ladies Sidney en R Programming 101

# Bron data: https://www.zorginzicht.nl/openbare-data/open-data-verpleeghuiszorg  
# Dit betreft de kwaliteitsindicatoren die zorginstellingen jaarlijks moeten 
# verzamelen en opsturen. Het doel van deze actie is niet zozeer om te 
# vergelijken of een organisatie "het wel goed doet", maar eerder als een 
# middel om te kunnen reflecteren. 

# Projectvraag: Presenteer deze gegevens op een overzichtelijke manier. 
# Waar mogelijk, geef de mogelijkheid om zorgorganisatie A te vergelijken met 
# andere - vergelijkbare - zorgorganisaties. 

# Laad packages ----
library(tidyverse) 
library(here)
library(skimr)
library(janitor)
library(readxl)
# library(dm)
library (lubridate)

# Laad data ----
df <- 
  read_excel(here("data", 
                  "openbaar-databestand-verpleeghuiszorg-verslagjaar-2020.xlsx"),
             sheet = "verpleeghuiszorg VJ2020")

# Opschonen ----
df <- df %>%
  clean_names() %>%
  rename(indicatorset = indicatorset_naam,
         organisatie = organisatie_naam,
         okvk = kvk_nummer,
         oagb = organisatie_agb_code,
         locatie = locatie_naam,
         lvestigingsnummer = vestigingsnummer,
         lpostcode = locatie_postcode,
         lhuisnummer = locatie_huisnummer,
         lplaats = locatie_plaats,
         lagb = locatie_ag_bcode,
         icode = indicator_code,
         isorteer = indicator_sorteernummer,
         inummer = indicator_nummer,
         inaam = indicator_naam,
         ieenheid = indicator_eenheid,
         iwaarde = indicator_waarde,
         invt = indicator_nvt,
         itype = indicator_type,
         begindat = meetperiode_begin_datum,
         einddat = meetperiode_eind_datum) %>%
  mutate(begindat = as_date(begindat),
         einddat = as_date(einddat),
         verslagjaar = as.Date(ISOdate(verslagjaar,12,31)),
         indicatorset = as.factor(indicatorset),
         indicatorset_code = as.factor(indicatorset_code),
         type_zorgaanbieder = as.factor(type_zorgaanbieder),
         # thema = as.factor(thema)   Tegen besloten
         ieenheid = as.factor(ieenheid),
         invt = as.logical(invt), 
         itype = as.factor(itype),
         # Sommige variabelen moeten juist wel als tekst
         bron = as.character(bron),
         okvk = as.character(okvk),
         oagb = as.character(oagb),
         lvestigingsnummer = as.character(lvestigingsnummer),
         lagb = as.character(lagb)
  ) 

# Factors - ik kwam er even niet uit hoe dit te doen met dplyr...
# Iets makkelijker leesbaar maken
levels(df$indicatorset) <- c("Basisveiligheid", "Personeelssamenstelling")

# skim (df)

# DIM & fact tabellen ----
organisaties <- df %>%
  distinct (
    organisatie,
    okvk,
    oagb, 
    type_zorgaanbieder
  ) %>%
  rowid_to_column("organisatie_ID")
# organisatie is uniek gedefinieerd door zijn okvk
# organisaties %>% count (okvk) %>% filter (n>1)

# De variabele locaties in de df is rommelig. 
# Door de platte structuur van de tabel, waarin indicatoren zijn opgenomen
# op zowel locatieniveau als organisatieniveau is het soms niet helemaal 
# duidelijk wat een daadwerkelijke locatie is. Voor de indcatorset 
# Personeelssamenstelling zijn namelijk dummie locaties aangemaakt, meestal 
# (maar niet altijd) aangeduid met "Concernniveau: "
# Gekozen om alleen de locaties van de Indicatorset Basisveiligheid mee te nemen. 
lokaties <- df %>%
  filter (indicatorset == "Basisveiligheid") %>% 
  distinct(
    locatie, lvestigingsnummer, lpostcode, lhuisnummer, 
    lplaats, lagb, okvk
  ) %>%
  rowid_to_column("lokatie_ID") %>%
  # Maak foreign key naar organisaties
  left_join(
    select (organisaties, organisatie_ID, okvk),
    by = "okvk"
  ) %>%
  select (-okvk) 
# lokatie is uniek gedefinieerd door locatie, lvestigingsnummer, lagb
# lokaties %>% count (locatie, lvestigingsnummer, lagb) %>% filter (n>1) 

indicatorsets <- df %>%
  distinct(
    indicatorset_code, 
    indicatorset
  ) %>%
  rowid_to_column("indicatorset_ID")

themas <- df %>%
  distinct(thema, indicatorset_code) %>%
  rowid_to_column("thema_ID") %>%
  # Maak foreign key naar indicatorsets
  left_join(
    select(indicatorsets, indicatorset_ID, indicatorset_code),
    by = "indicatorset_code"
  ) %>%
  select (
    -indicatorset_code
  )

indicatoren <- df %>%
  distinct (
    icode, 
    isorteer,
    inummer,
    inaam,
    ieenheid,
    itype,
    thema
  ) %>%
  rowid_to_column("indicator_ID") %>% 
  # Maak foreign key naar themas
  left_join(
    select (themas, thema_ID, thema),
    by = "thema"
  ) %>%
  select (-thema)
# indicator is uniek gedefinieerd door icode
# indicatoren %>% count (icode) %>% filter (n>1) 

dfact <- df %>%
  # Maak foreign key naar lokaties
  left_join(
    select (lokaties, lokatie_ID, locatie, lvestigingsnummer, lagb),
    by = c ("locatie", "lvestigingsnummer", "lagb")
  ) %>%
  select (
    - locatie,
    - lvestigingsnummer, 
    - lpostcode, 
    - lhuisnummer, 
    - lplaats, 
    - lagb,
    # En de organisatiegegevens hangen aan de lokatie
    - organisatie,
    - okvk,
    - oagb,
    - type_zorgaanbieder
  ) %>% 
  # Maak foreign key naar indicatoren
  left_join(
    select (indicatoren, indicator_ID, icode),
    by = "icode"
  ) %>%
  select (
    # Indicatorsets hangen aan de themas
    - indicatorset,
    - indicatorset_code,
    # themas hangen aan indicatoren
    - thema, 
    - icode,
    - isorteer, 
    - inummer, 
    - inaam, 
    - ieenheid, 
    - itype,
    # aanleverfrequentie is altijd jaarlijks...
    - aanlever_frequentie
  )

# Maak aparte tabellen voor Basisveiligheid en Personeelssamenstelling. 
bv <- dfact %>% 
  left_join(
    select (indicatoren, indicator_ID, thema_ID),
    by = "indicator_ID"
  ) %>% 
  left_join(
    select (themas, thema_ID, indicatorset_ID),
    by = "thema_ID"
  ) 
  
ps <- bv %>%
  filter (indicatorset_ID == 2) %>% 
  select (
    - thema_ID,
    - indicatorset_ID
  )

bv <- bv %>%
  filter (indicatorset_ID == 1) %>% 
  select (
    - thema_ID,
    - indicatorset_ID
  )

# Verder opschonen / aanvullen van deze lokaties. 
# Om de grootte van de locatie in te schatten zijn twee waardevolle
# gegevens bekend. Medicatieveiligheid en voedselvoorkeuren zijn verplichte 
# indicatoren en wordt ingevuld op respectievelijk afdelings- en op clientniveau. 
# Indicator_ID 6 = "Percentage afdelingen ... medicatiefouten"
# Indicator_ID 32 = "Percentage cliënten op de afdeling ... voedselvoorkeuren"

dlokaties <- lokaties %>% 
  left_join(
    filter (select(bv, noemer, lokatie_ID, indicator_ID), indicator_ID == 6),
    by = "lokatie_ID"
  ) %>% 
  rename (nafdelingen = noemer) %>% 
  select (-indicator_ID) %>% 
  left_join(
    filter (select(bv, noemer, lokatie_ID, indicator_ID), indicator_ID == 32),
    by = "lokatie_ID"
  ) %>% 
  rename (nclienten = noemer) %>% 
  select (-indicator_ID) %>% 
  mutate (cltPerAfd = round(nclienten/nafdelingen)) %>% 
  arrange(-cltPerAfd)
  
