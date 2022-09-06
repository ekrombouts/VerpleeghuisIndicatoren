# Decubitus ---------------------------------------------------------------

# Datum: 30 Augustus 2022
# Auteur: Eva Rombouts
# Oefenproject in het kader van leren R en R Studio

source ('scripts/LadenEnOpschonen.R')

# Decubitus is een keuze indicator. Het thema heeft 2 indicatoren:
# Gekozen ja/nee en het percentage.


# Maak tabel --------------------------------------------------------------

# Zorg dat 1 rij 1 observatie van een lokatie is
decubitus <- bv %>%
  left_join(select(indicatoren, thema_ID, indicator_ID),
            "indicator_ID") %>%
  filter (thema_ID %in% c(1,2)) %>% #Thema decubitus
  mutate (ind = recode(indicator_ID,
                       `1` = "gekozen",
                       `2` = "percentage",
                       `3` = "casuistiek",
                       `4` = "nAfd")) %>%
  select (-verslagjaar,
          -bron,
          -begindat,
          -einddat,
          -thema_ID,
          -indicator_ID) %>%
  pivot_wider(
    names_from = ind,
    values_from = c(iwaarde, teller, noemer, opmerking, invt)
  ) %>%
  select (
    lokatie_ID, 
    iwaarde_gekozen, 
    iwaarde_percentage, 
    teller_percentage, 
    noemer_percentage, 
    iwaarde_casuistiek, 
    iwaarde_nAfd, 
    teller_nAfd, 
    noemer_nAfd, 
    opmerking_gekozen, 
    opmerking_percentage, 
    opmerking_casuistiek, 
    opmerking_nAfd, 
    invt_percentage, 
    invt_nAfd
  ) %>% 
  rename(
    gekozen = iwaarde_gekozen, 
    percentage = iwaarde_percentage, 
    teller = teller_percentage, 
    noemer = noemer_percentage, 
    casuistiek = iwaarde_casuistiek, 
    percafd = iwaarde_nAfd, 
    telafd = teller_nAfd, 
    nmrafd = noemer_nAfd, 
    ogekozen = opmerking_gekozen, 
    opercentage = opmerking_percentage, 
    ocasuistiek = opmerking_casuistiek, 
    onafd = opmerking_nAfd, 
    nvtperc = invt_percentage, 
    nvtnafd = invt_nAfd
  ) %>%
  mutate(
    gekozen = as.logical(recode(
      gekozen, 'ja' = TRUE, 'nee' = FALSE)),
    casuistiek = as.logical(recode(
      casuistiek, 'ja' = TRUE, 'nee' = FALSE)),
    percentage = as.numeric(percentage), 
    percafd = as.numeric(percafd)
  )

# Exploreer ---------------------------------------------------------------

# Eyeball gegevens:
skim (decubitus)
# Alle 2349 lokaties hebben aangeleverd.
# Van deze lokaties hebben 1236 decubitus al keuze-indicator gekozen.
# 1113 niet. 
# Wat opvalt is dat er dan ook 1113 missing percentages zijn, 
# maar 1152 missing tellers & noemers.

# VRAAGje -----------------------------------------------------------------
# Moet je bij subsetten in baseR echt altijd helemaal uitschrijven? (dus met $)
# For real? 
aap <- decubitus [(is.na(decubitus$teller)|
                     is.na(decubitus$noemer))
                  &!is.na(decubitus$percentage),]

# --> m.i. geen geldige gegevens, dus aanpassen naar missing. 
# !!VRAAG -------------------------------------------------------------------
# Hier gebeurt iets geks! 
noot <- decubitus %>% 
  mutate (percentage = 
            #percentage) --> dit mag gewoon...
            if_else(
              #condition = (is.na(teller) | is.na(noemer)) & !is.na(percentage), 
              condition = TRUE, # ff makkelijker leesbaar gemaakt
              true = percentage, 
              # Opeens een vector geworden? Hoezo is deze percentage
              # nou ineens anders geworden?   
              false = NA
            )
  )

noot <- decubitus %>% 
  mutate (
    percentage = na_if(percentage, 
                       (is.na(teller) | is.na(noemer)) & !is.na(percentage)
    )
  )

table(decubitus$gekozen, is.na(decubitus$percentage))

# VRAAG -------------------------------------------------------------------
# Zie functie fln () 
# Kan ik een comma en line/break separated lijstje naar mijn clipboard 
# krijgen? (doel: variabelnamen copy/pasten)


# Analyseer ---------------------------------------------------------------
# Vergelijk organisaties die wel/niet hebben aangeleverd
decubitus %>%
  left_join (select(lokaties, lokatie_ID, organisatie_ID),
             "lokatie_ID") %>%
  left_join(organisaties, "organisatie_ID") %>%
  group_by(organisatie_ID, nLokaties, nClienten) %>%
  summarise(pgek = round((sum(gekozen) / n()) * 100)) %>%
  # ggplot(aes(pgek))+
  # geom_histogram()    ## Meeste organisaties hebben voor alle of voor
  ## geen enkele lokatie gekozen voor decubitus
  mutate (gek = as.factor(if_else(pgek == 100, 2,
                                  if_else(pgek == 0, 0, 1)))) %>%
  mutate (logClienten = log(nClienten)) %>%

  # ggplot(aes(nClienten)) +
  # geom_histogram(bins = 10)+
  # facet_wrap(~gek) 
  
  # ggplot(aes(nLokaties)) +
  # geom_histogram(bins = 10)+
  # facet_wrap(~gek)
  
  group_by(gek) %>%
  summarise (
    n = n(),
    mean_nAfd = mean (nLokaties, na.rm = TRUE),
    mean_nCl = mean (nClienten, na.rm = TRUE),
    sd_nCl = sd (nClienten, na.rm = TRUE)
  )
# --> organisaties die niet rapporteren op decubitus lijken wat kleiner,
# verschillen zijn echter weinig relevant.

# Rijen met missing percentages mogen er dus uit:
decubitus <- decubitus %>%
  drop_na (percentage) %>%
  left_join (lokaties, "lokatie_ID") %>%
  left_join (organisaties, "organisatie_ID") %>% 
  select(
    organisatie_ID, lokatie_ID, 
    gekozen, percentage, teller, noemer, casuistiek, percafd, telafd, nmrafd, 
    ogekozen, opercentage, ocasuistiek, onafd, nvtperc, nvtnafd, # Erbij?
    organisatie, nLokaties, nAfdelingen, nClienten, Stedelijk, 
    locatie, nafdelingen, nclienten, cltPerAfd, nAfd, stedelijk, fstedelijk
  )



## Exploreer ---------------------------------------------------------------
# Kweenie... 
  
## Stedelijk? --------------------------------------------------------------

# Komt decubitus meer voor in stedelijke gebieden?
# Vergelijk means. Kan er geen verschil van maken. 
decubitus %>%
  group_by(fstedelijk) %>%
  summarise(
    n = n(),
    mPerc = mean(percentage),
    medPerc = median(percentage),
    sdPerc = sd(percentage),
    lmPerc = log(mean(percentage)),
    lmedPerc = log(median(percentage)),
    lsdPerc = log(sd(percentage))
  )

# plot
decubitus %>% 
  ggplot(aes(fstedelijk, percentage, color = nclienten)) +
  geom_jitter() 

decubitus %>%
#  mutate(percentage = log(percentage)) %>%
  ggplot(aes(fstedelijk, percentage)) +
  geom_boxplot() 
# scale_y_continuous(trans='log10')
#  stat_summary(fun = "mean")

decubitus %>%
  ggplot (aes(percentage, fill = fstedelijk)) +
  geom_histogram(bins = 15) +
  theme(legend.position = 'top') +
  facet_wrap( ~ fstedelijk) +
  scale_x_log10(expand = c(0,0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic()

decubitus %>% 
  ggplot (aes(percentage, color = fstedelijk)) +
  geom_density() +
  scale_x_log10()

## Grootte? ----------------------------------------------------------------
decubitus %>% 
  ggplot(aes(nclienten, percentage)) +
  geom_point()+
  scale_x_continuous(expand = c(0,0))+ 
  scale_y_continuous(expand = c(0,0))

decubitus %>% 
  ggplot (aes(nAfd, percentage)) +
  geom_jitter()

# Hier ben ik gebleven. Ik vertrouw een heleboel van de getalletjes niet. 
# Alle nul-waarden wil ik graag verder onderzoeken. 
