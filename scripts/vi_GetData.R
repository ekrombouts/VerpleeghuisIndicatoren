# Verpleeghuisindicatoren -----------------------------------------------------
# Datum: Oktober 2022
# Auteur: Eva Rombouts
# Oefenproject in het kader van opleiding data science

# Bron data: https://www.zorginzicht.nl/openbare-data/open-data-verpleeghuiszorg

# Data laden --------------------------------------------------------------
# TODO Van internet laden
# https://www.zorginzicht.nl/openbare-data/open-data-verpleeghuiszorg#verslagjaar-2020

df19 <-
  read_excel(
    here(
      "data",
      "Openbaar databestand Verpleeghuiszorg verslagjaar 2019.xlsx"
    )
  )

df19 <- df19 %>% 
  # Pas aan zodat de structuur overeenkomt met 2020 en 2021
  rename(OrganisatieAGBCode = ...13,
         LocatieAGBcode = AGBcode
         ) %>% 
  select (
    IndicatorsetNaam,
    IndicatorsetCode,
    Verslagjaar,
    OrganisatieNaam,
    KvkNummer,
    OrganisatieAGBCode,
    LocatieNaam,
    Vestigingsnummer,
    LocatiePostcode,
    LocatieHuisnummer,
    LocatiePlaats,
    LocatieAGBcode,
    'Type Zorgaanbieder',
    Thema,
    IndicatorCode,
    IndicatorSorteernummer,
    IndicatorNummer,
    IndicatorNaam,
    IndicatorEenheid,
    IndicatorWaarde,
    Teller,
    Noemer,
    Indicator_nvt,
    Opmerking,
    IndicatorType,
    Bron,
    MeetperiodeBeginDatum,
    MeetperiodeEindDatum,
    AanleverFrequentie
  )

df20 <-
  read_excel(
    here(
      "data",
      "openbaar-databestand-verpleeghuiszorg-verslagjaar-2020.xlsx"
    ),
    sheet = "verpleeghuiszorg VJ2020"
  ) 

df21 <-
  read_excel(
    here(
      "data",
      "openbaar-databestand-verpleeghuiszorg-verslagjaar-2021.xlsx"
    ),
    sheet = "VHZ VJ2021"
  )

df <- rbind (df19, df20, df21)

# https://www.cbs.nl/nl-nl/longread/diversen/2021/statistische-gegevens-per-vierkant-en-postcode-2020-2019-2018?onepage=true#c-4--Beschrijving-cijfers
# tbv stedelijkheid.
cbs_postcode <-
  read_excel(
    here("data",
         "cbs_pc4_2020_v1.xlsx"),
    sheet = "PC4_2020_v1",
    na = "-99997",
    # Deze data is door CBS verborgen gehouden ivm privacy
    skip = 8
  )
# Eerste rij bevat uitleg, eruit
cbs_postcode <- cbs_postcode[-1,]

