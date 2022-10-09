# Verpleeghuisindicatoren -----------------------------------------------------
# Datum: Oktober 2022
# Auteur: Eva Rombouts
# Oefenproject in het kader van opleiding data science

# Dit betreft de kwaliteitsindicatoren die zorginstellingen jaarlijks moeten
# verzamelen en opsturen. Het doel van de indicatoren is niet zozeer om te
# vergelijken of een organisatie "het wel goed doet", maar eerder als een
# middel om te kunnen reflecteren.
# Script aangepast voor data van 2021

# Projectvraag: Presenteer deze gegevens op een overzichtelijke manier.
# Waar mogelijk, geef de mogelijkheid om zorgorganisatie A te vergelijken met
# andere - vergelijkbare - zorgorganisaties.

# Packages ----------------------------------------------------------------
library (tidyverse)
library (here)
library (skimr)
library (janitor)
library (readxl)
library (lubridate)
library (mice)
library (todor)
library (clipr)
library (fedmatch)

# Source files ------------------------------------------------------------
source("scripts/vi_GetData.R")
source ("scripts/vi_TransformData.R")

source ("scripts/vi_TransformPS.R")

ps <- ps2
save(ps,file="data/ps.Rda")
