
# WerkTabel ---------------------------------------------------------------
VHPersoneel <- Personeelssamenstelling %>%
  mutate(
    NivMix = 
      (p_PSNiv1 * 1 + p_PSNiv2 * 2 + p_PSNiv3 * 3 + p_PSNiv4 * 4 + 
         p_PSNiv5 * 5 + p_PSNiv6 * 6) /
      (p_PSNiv1 + p_PSNiv2 + p_PSNiv3 + p_PSNiv4 + p_PSNiv5 + p_PSNiv6)
  ) %>%
  mutate(Stedelijk = round(Stedelijk)) %>%
  mutate (Stedelijk = factor(
    case_when(
      Stedelijk == 1 ~ "Zeer sterk",
      Stedelijk == 2 ~ "Sterk",
      Stedelijk == 3 ~ "Matig",
      Stedelijk == 4 ~ "Weinig",
      Stedelijk == 5 ~ "Niet"
    )
  )) %>%
  mutate(Stedelijk = ordered (
    Stedelijk,
    levels =
      c("Zeer sterk",
        "Sterk",
        "Matig",
        "Weinig",
        "Niet")
  )) %>%
  rename (
    nMedew = PSnMedew,
    nFTE = PSnFTE,
    pTijdelijk = p_PSTijdPerc,
    pPNIL = p_PSPnilPerc,
    pPNilKosten = p_PSPnilKostPerc,
    GemContract = PSGemContr,
    PercBehandelDienst = p_PSBehandel,
    PercLeerling = p_PSLeerling,
    nStagiairs = PSnStag,
    nVrijwilligers = PSnVrijw,
    pVerzuim = p_PSVerzuimPerc,
    VerZuimFreq = PSVerzuimFreq,
    pInstroom = p_PSInstroom,
    pUitstroom = p_PSUitstroom,
    pDoorstroom = p_PSDoorstroom,
    FTEperClient = PSFTEperCt,
  ) %>%
  select(
    organisatie_ID,
    organisatie,
    nLokaties,
    nAfdelingen,
    nClienten,
    Stedelijk,
    nMedew,
    nFTE,
    pTijdelijk,
    pPNIL,
    pPNilKosten,
    GemContract,
    NivMix,
    PercBehandelDienst,
    PercLeerling,
    nStagiairs,
    nVrijwilligers,
    pVerzuim,
    VerZuimFreq,
    pInstroom,
    pUitstroom,
    pDoorstroom,
    FTEperClient
  )

 # Plaatjes ----------
names(ps)
Coloriet <- ps %>% filter (organisatie == "Stichting Amsta")

# Univariaat
ps %>%
  ggplot(aes(
    x = pVerzuim
    )) +
#  scale_x_log10() +
#  geom_histogram() 
  geom_density()

# Boxplot
ps %>%
  filter (pVerzuim < 30) %>% 
  ggplot (aes(
    x = Stedelijk, 
    y = pVerzuim
    )) +
  geom_boxplot() +
  geom_point(data = Coloriet,
             aes(x=Stedelijk, pVerzuim),
             color = "red",
             size = 3) 
  
# Scatterplot
ps %>% 
#  filter(nMedew > 10) %>% 
#  filter (nClienten > 10) %>% 
  ggplot (aes(
    x = nMedew,
    y = pVerzuim
  ))+
  geom_point() + 
  geom_smooth(method = "lm") +
  geom_point(data = Coloriet,
             aes(x=nMedew, pVerzuim),
             color = "red",
             size = 3) +
  scale_x_log10() +
  scale_y_log10() +
  theme_bw()

# Clientervaring --------
ce <- BasisVeiligheid %>% 
  select(
    lokatie_ID,
    organisatie_ID,
    p_CENPS8910PercR,
    p_CENPSJaPercR,
    CEScoreGet,
  ) %>% 
  rename (
    Boven8 = p_CENPS8910PercR,
    Aanbeveling = p_CENPSJaPercR,
    Score = CEScoreGet
  )
hist (ce$Boven8)
ce %>% filter (!Boven8 ==0) %>% ggplot (aes(Boven8)) + geom_histogram()
ce %>% filter (!Aanbeveling == 0) %>% ggplot (aes(Aanbeveling)) + 
  geom_histogram()
ce %>% filter (!Score == 0) %>% ggplot (aes(Score)) + geom_histogram()

ce <- BasisVeiligheid %>% 
  select(
    lokatie_ID,
    organisatie_ID,
    p_CENPS8910PercR,
    p_CENPSJaPercR,
    CEScoreGet,
  )

ce <- ce %>%
  mutate(across(c("Boven8", "Aanbeveling", "Score"), ~na_if(., 0))) %>% 
  mutate (
    V8 = if_else(Boven8 < median(Boven8, na.rm = T), 0,1),
    VJ = if_else(Aanbeveling < median(Aanbeveling, na.rm = T), 0,1),
    VS = ifelse(Score < median(Score, na.rm = T), 0,1)
  ) %>% 
  mutate (
    CePos = pmax(V8, VJ, VS, na.rm = T)) %>% 
  select( -V8, -VJ, -VS)

ce %>% ggplot(aes(CePos)) + geom_bar()

VHPersoneel <- VHPersoneel %>% 
  group_by_all() %>% 
  left_join(select(ce, organisatie_ID, CePos),
            by = "organisatie_ID") %>% 
  summarise(mCePos = mean(CePos, na.rm = T))

VHPersoneel %>% 
  ggplot(aes(nLokaties, mCePos)) + geom_point()

# Statistiek opdracht 1 --------
# Deel 1
# 1. Lees de data in als dataframe en zorg ervoor dat de eerste kolom als label voor de
# rijen wordt gebruikt. Hint: zie ?row.names.
# 2. Voer beschrijvende statistiek uit dmv. het maken van een aantal grafieken. Deze
# grafieken mag je via de “Base R graphics” of mbv. ggplot maken.
# Cre ̈eer geschikte grafieken om de volgende vragen te beantwoorden:
#   (a) Zijn private universiteiten overwegend kleiner of groter dan publieke universi-
#   teiten? Je mag zelf een definitie voor groot/klein defini ̈eren.
# (b) Hoe ziet de verdeling eruit van het acceptatiepercentage? Wat is het accepta-
#   tiepercentage voor de meest selectieve universiteit?
#   (c) Zijn de meer selectieve universiteiten ook overwegend duurder dan minder selec-
#   tieve universiteiten? (Je mag zelf bepalen welke kosten je wel/niet mee neemt.)
# (d) Bedenk zelf een extra vraag en cre ̈eer een geschikte figuur om deze vraag mee
# te beantwoorden.
# 3. Voer hypothesetoetsen uit om de volgende vragen te beantwoorden. Geef telkens
# duidelijk aan wat de exacte nul- en alternatieve hypothese is die je toetst, motiveer
# de keuze van de specifieke toets en verwoord duidelijk de conclusie.
# (a) Ontvangen elite scholen een ander aantal aanmeldingen in vergelijking met niet-
#   elite scholen? Definieer “elite-school” als scholen waarvoor geldt dat meer dan
# 50% van de studenten tot de top 10% van hun high school behoort.
# (b) Is er een verband tussen acceptance rate en graduation rate?
#   (c) Bedenk zelf ook een extra hypothese om te toetsen en voer de hypothesetoets
# uit.