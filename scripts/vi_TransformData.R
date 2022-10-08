# Leesbaarder maken ----------------------------------------------------------
df <- df %>%
  # Aanpassen kolomnamen
  clean_names() %>% # Uit de janitor package
  # Overzichtelijker kolomnamen
  rename(
    indicatorset = indicatorset_naam,
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
    einddat = meetperiode_eind_datum
  ) %>%
  # Aanpassen datatypes
  mutate(
    begindat = as_date(begindat),
    einddat = as_date(einddat),
    #verslagjaar = as.Date(ISOdate(verslagjaar, 12, 31)),
    indicatorset = factor(
      case_when(
        indicatorset == 'Verpleeghuiszorg Basisveiligheid' ~
          "Basisveiligheid",
        indicatorset == 'Verpleeghuiszorg Personeelssamenstelling' ~
          "Personeelssamenstelling"
      )
    ),
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
    lagb = as.character(lagb),
    # Netjes weergeven
    organisatie = str_to_title(organisatie),
    locatie = str_to_title(locatie)
  )

# Foutieve invoer herstellen ----------------------------------------------
# Onderstaande organisatie / lokatie koppels gaven problemen
# Careyn heeft ten onrechte meerdere concerns ingevoerd.
df <- df %>%
  filter (!(
    locatie %in% c(
      "Concernniveau: Aveant B.v.",
      "Concernniveau: Careyn Dwo/Nwn B.v.",
      "Concernniveau: Careyn Zuid-Hollandse eilanden B.v.",
      "Concernniveau: Zuwe Zorg B.v."
    )
  ))

# King Arthur Groep heeft dezelfde gegevens voor organisatie en lokatie
# opgegeven, wat problemen geeft met unieke lokaties
df <- df %>%
  mutate (
    locatie = if_else(
      locatie == "Stichting King Arthur Groep" &
        indicatorset == "Personeelssamenstelling" &
        verslagjaar == 2020,
      true = "Concernniveau: Stichting King Arthur Groep",
      false = locatie
    )
  ) %>%
  mutate (
    locatie = if_else(
      locatie == "Concernniveau: Stichting Protestants-Christelijk Zorgcentrum 't Anker" &
        indicatorset == "Basisveiligheid" &
        verslagjaar == 2020,
      true = "Stichting Protestants-Christelijk Zorgcentrum 't Anker",
      false = locatie
    )
  )

# Thebe heeft apart voor concernniveaus gerapporteerd, zonder per lokatie te
# specificeren bij welk concern ze horen. Uitgegaan van postcodes.
df <- df %>%
  mutate(
    organisatie = if_else(
      organisatie == "Stichting Thebe Wonen En Zorg" &
        str_sub(lpostcode, 1, 1) == "4" &
        verslagjaar == 2020,
      "Stichting Thebe Wonen en Zorg - West",
      organisatie
    )
  ) %>%
  mutate(
    organisatie = if_else(
      organisatie == "Stichting Thebe Wonen En Zorg" &
        str_sub(lpostcode, 1, 1) == "5" &
        verslagjaar == 2020,
      "Stichting Thebe Wonen en Zorg - Midden",
      organisatie
    )
  )


# DIM en Fact tabellen ----------------------------------------------------
## Indicatorsets -----------------------------------------------------------
indicatorsets <- df %>%
  distinct(indicatorset_code,
           indicatorset)

## Themas ------------------------------------------------------------------
themas <- df %>%
  distinct(thema, indicatorset_code) %>%
  rowid_to_column("thema_ID")

## Indicatoren -------------------------------------------------------------
indicatoren <- df %>%
  distinct (
    verslagjaar,
    icode,
    inummer,
    inaam,
    isorteer,
    ieenheid,
    itype,
    indicatorset_code,
    thema
  ) %>%
  # Onderstaande regels zorgen ervoor dat per indicatorcode alleen de laatste
  # omschrijving etc wordt gebruikt. Zo wordt een unieke waarde van icode
  # gegarandeerd. Bovendien geeft de variabele verslagjaar nu het jaar aan
  # wanneer de indicator als laatste werd gebruikt.
  arrange(verslagjaar) %>%
  group_by(icode) %>%
  slice_tail (n = 1) %>%
  # Extra variabele met korte naam voor de indicator. Deze zal later als
  # kolomnaam worden gebruikt voor pivot-wider
  mutate (
    ind = recode(
      icode,
      `INID014307` = "DecGek",
      `INID013307` = "DecPercC",
      `INID014308` = "DecCasGek",
      `INID014309` = "DecCasPercA",
      `INID013309` = "ACPPercC",
      `INID014310` = "MedFtPercA",
      `INID014311` = "MedRevGek",
      `INID014312` = "MedRevPercC",
      `INID014313` = "MMGekozen",
      `INID013318` = "MMMechanischPercC",
      `INID013319` = "MMFysiekPercC",
      `INID013320` = "MMFarmacPercC",
      `INID013321` = "MMPsychPercC",
      `INID013322` = "MMElecPercC",
      `INID013323` = "MM1op1PercC",
      `INID013324` = "MMAfzonPercC",
      `INID013325` = "MMOverigPercC",
      `INID013326` = "MMOverigTekst",
      `INID014314` = "VrijBepGek",
      `INID014315` = "VrijBepTekst",
      `INID014316` = "VrijBevGek",
      `INID014317` = "VrijBevTekst",
      `INID014318` = "ContGek",
      `INID014321` = "ContWPlanPercC",
      `INID014322` = "ContGPlanPercC",
      `INID014323` = "ContOPlanPercC",
      `INID014325` = "ContVoorkeurenJN",
      `INID014326` = "ContOndersteuningJN",
      `INID014327` = "ContZelfstandigJN",
      `INID014328` = "ContMateriaalJN",
      `INID014329` = "ContAndersJN",
      `INID014330` = "VoedWVoorkeurPercC",
      `INID014331` = "VoedGVoorkeurPercC",
      `INID014332` = "VoedOVoorkeurPercC",
      `INID014334` = "VoedWelkJN",
      `INID014335` = "VoedVormJN",
      `INID014336` = "VoedHulpJN",
      `INID014337` = "VoedTijdPlaatsJN",
      `INID014338` = "VoedOverigJN",
      `INID010008` = "KwalVerslURL",
      `INID013383` = "BronAanbeveling",
      `INID013381` = "CENPS8910PercR",
      `INID013382` = "CENPSJaPercR",
      `INID014477` = "CEScoreGet",
      `INID013384` = "CEnRespondenten",
      `INID013385` = "CEOpm",
      `INID013486` = "nMedew",
      `INID013520` = "nFTE",
      `INID013521` = "Tijd",
      `INID013522` = "Pnil",
      `INID013523` = "PnilKosten",
      `INID013524` = "GemContr",
      `INID013534` = "Niv1",
      `INID013535` = "Niv2",
      `INID013536` = "Niv3",
      `INID013537` = "Niv4",
      `INID013538` = "Niv5",
      `INID013539` = "Niv6",
      `INID013540` = "Bdnst",
      `INID013541` = "OvPers",
      `INID013542` = "Lling",
      `INID013526` = "nStag",
      `INID013527` = "nVrijw",
      `INID013528` = "VerzuimPerc",
      `INID013529` = "VerzuimFreq",
      `INID013530` = "Instroom",
      `INID013531` = "Uitstroom",
      `INID013532` = "Doorstroom",
      `INID013533` = "FTEperCt"
    ),
    .after = inaam
  )

## Organisaties ------------------------------------------------------------
organisaties <- df %>%
  # Een organisatie wordt uniek gedefinieerd door zijn kvk nummer
  mutate (# codeer missing AGB codes als zodanig
    oagb = if_else(oagb == "0",
                   NA_character_,
                   oagb)) %>%
  # Filter alleen de organisaties met personeelsinfo eruit
  filter (indicatorset_code == "ISID000160") %>%
  # Haal de organisatie-specifieke kolommen eruit
  distinct (verslagjaar,
            okvk,
            organisatie,
            oagb,
            type_zorgaanbieder,
            locatie,
            lpostcode) %>%
  arrange(verslagjaar) %>%
  group_by(okvk) %>%
  # Bepaal van wanneer tot wanneer de organisatie bestond
  mutate (van = min (verslagjaar),
          tot = max (verslagjaar)) %>%
  # Dit zorgt ervoor dat alleen de meest recente gegevens van de organisatie
  # worden gebruikt, mocht er een naamsverandering of een adreswijziging zijn
  # geweest
  slice_tail (n = 1) %>%
  # maak nog wat netter
  mutate (type_zorgaanbieder = str_to_title(type_zorgaanbieder)) %>%
  rename (opostcode = lpostcode) %>%
  # En selecteer kolommen
  select (okvk, organisatie, locatie, opostcode, oagb, van, tot, type_zorgaanbieder)

DoubleOrgs<- df %>% 
  # Sommige organisaties hebben niet goed geregistreerd en komen per indicator 
  # per jaar en per kvk nummer vaker voor in de personeelssamenstelling. 
  filter (indicatorset_code == "ISID000160") %>% 
  select (okvk, verslagjaar, icode) %>% 
  left_join(organisaties,
            by = "okvk") %>% 
  group_by(verslagjaar, okvk, organisatie) %>% 
  summarise (n = n()) %>% 
  filter (n>30) 

## Lokaties ----------------------------------------------------------------
lokaties <- df %>%
  mutate (# codeer missing AGB codes als zodanig
    lagb = if_else(lagb == "0",
                   NA_character_,
                   lagb)) %>%
  # Neem alleen lokaties, dus geen organisaties
  filter (indicatorset_code == "ISID000151") %>%
  distinct (verslagjaar, okvk, lpostcode, locatie, lagb, lvestigingsnummer) %>% 
  arrange (verslagjaar) %>% 
  group_by (okvk, lpostcode, locatie) %>% 
  mutate (van = min (verslagjaar),
          tot = max (verslagjaar)) %>% 
  slice_tail(n=1) %>% 
  select (okvk, locatie, lpostcode, lagb, lvestigingsnummer, van, tot)

DoubleLocs <- df %>% 
  # Een locatie is dubbel geregistreerd. Selecteer
  filter (indicatorset_code == "ISID000151") %>% 
  group_by(verslagjaar, okvk, lpostcode) %>% 
  summarise (n= n()) %>% 
  arrange (-n) %>% 
  filter (n>50) %>% 
  left_join(select (organisaties, okvk, organisatie),
            by = "okvk") %>% 
  left_join(select (lokaties, okvk, lpostcode),
            by = c("okvk", "lpostcode"))  

## Aparte tabellen Personeelssamenstelling en Basisveiligheid ---------------
bv1 <- df %>%
  filter (indicatorset == "Basisveiligheid") %>%
  select (
    verslagjaar,
    okvk,
    locatie,
    lpostcode,
    thema,
    icode,
    iwaarde,
    teller,
    noemer,
    invt,
    opmerking
  )

ps1 <- df %>%
  filter (indicatorset == "Personeelssamenstelling") %>%
  select (
    verslagjaar,
    okvk,
    icode,
    iwaarde,
    teller,
    noemer,
    invt,
    opmerking
  )
