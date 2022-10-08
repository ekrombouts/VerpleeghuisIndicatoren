# WerkTabel ---------------------------------------------------------------
ps <- Personeelssamenstelling %>%
  rename(
    stedelijk = Stedelijk,
    nMedew = PSnMedew,
    nFTE = PSnFTE,
    noemer = n_PSnoemer,
    pTijd = p_PSTijdPerc,
    pPNIL = p_PSPnilPerc,
    pPNILkost = p_PSPnilKostPerc,
    persKost = n_PSPnilKostPerc,
    gemContr = PSGemContr,
    pNiv1 = p_PSNiv1,
    pNiv2 = p_PSNiv2,
    pNiv3 = p_PSNiv3,
    pNiv4 = p_PSNiv4,
    pNiv5 = p_PSNiv5,
    pNiv6 = p_PSNiv6,
    pBehandel = p_PSBehandel,
    pOverig = p_PSOverig,
    pLeerling = p_PSLeerling,
    nStagiair = PSnStag,
    nVrijw = PSnVrijw,
    pVerzuim = p_PSVerzuimPerc,
    verzuimFreq = PSVerzuimFreq,
    pInstroom = p_PSInstroom,
    pUitstroom = p_PSUitstroom,
    pDoorstroom = p_PSDoorstroom,
    FTEperCt = PSFTEperCt
  ) %>%
  # Grootte van de organisatie wordt uitgedrukt in aantal clienten
  # Bereken gerelateerde cijfers relatief hieraan
  mutate (
    nCpLokatie = nClienten / nLokaties,
    nCpAfdeling = nClienten / nAfdelingen,
    nCpMedew = nClienten / nMedew,
    nCpFTE = nClienten / nFTE,
    persKost = persKost + t_PSPnilKostPerc,
    vrijwpClient = ordered(cut(nVrijw / nClienten, breaks=c(-Inf, 0.5, 1, Inf), labels=c("<0.5","0.5-1",">1")))
  ) %>%
  select (
    organisatie,
    organisatie_ID,
    lokatie_ID,
    nClienten,
    persKost,
    nFTE,
    nCpMedew,
    nCpLokatie,
    nCpAfdeling,
    nCpFTE,
    nStagiair,
    vrijwpClient,
    stedelijk,
    pTijd,
    pPNIL,
    pPNILkost,
    gemContr,
    pNiv1,
    pNiv2,
    pNiv3,
    pNiv4,
    pNiv5,
    pNiv6,
    pBehandel,
    pOverig,
    pLeerling,
    pVerzuim,
    verzuimFreq,
    pInstroom,
    pUitstroom,
    pDoorstroom,
    FTEperCt
  )

# Clean ----
ps <- ps %>%
  # TODO impute missing
  # Organisaties zonder clienten eruit (NB moet naar missing, maar daar ben ik nog niet)
  filter (!(nClienten == 0 )) %>%
  mutate (
    # Onwaarschijnlijke FTEperCt naar NA
    FTEperCt = if_else(FTEperCt > 10,
                       NA_real_,
                       FTEperCt),
    # 2 organisaties hebben duidelijk een invoerfout in personeelskosten
    persKost = if_else(persKost / nFTE > 10 ^ 6,
                       persKost / 100,
                       persKost),
    # Gemiddeld contract > 1 naar NA
    gemContr = if_else(gemContr > 1,
                       NA_real_,
                       gemContr),
    # Onbekend aantal afdelingen of afdelingen = 0. Wordt missing
    nCpAfdeling = if_else(nCpAfdeling == Inf |nCpAfdeling > 200,
                         NA_real_,
                         nCpAfdeling),
    # Als de personeelskosten minder dan 5000 euro per client per jaar zijn
    # is dit niet reeel.
    persKost = if_else(persKost/nClienten<5000,
                       NA_real_,
                       persKost),
    # Opgegeven personeelskosten per patient komen niet overeen met berekend
    FTEperCt = if_else(FTEperCt/nCpFTE > 5 |
                         FTEperCt/nCpFTE < 0.2, 
                       NA_real_,
                       FTEperCt),
    # Bereken nog personeelskosten per client
    pkPerClient = persKost/nClienten
  )

