# Start -----
#install.packages("psych")
#install.packages("Hmisc")
library(psych)
library (Hmisc)

source("scripts/LadenEnOpschonen.R")
source("scripts/PersoneelsSamenstelling.R")

ps <- ps %>%
  select (organisatie,
          persKost,
          nClienten,
          nFTE,
          everything())

# Model: voorspel personeelskosten
# Zijn de personeelskosten normaal verdeeld?
pk <- log(ps$persKost)
hist (pk, 50)
shapiro.test(pk)
qqnorm(pk)
qqline(pk)

# Maak estimation & test samples
## 75% of the sample size
smp_size <- floor(0.75 * nrow(ps))
set.seed(203)
train_ind <- sample(seq_len(nrow(ps)), size = smp_size)

train <- ps[train_ind, ]
test <- ps[-train_ind, ]

# Onderzoek
# Afhankelijke variabele loggen
ps <- ps %>%
  mutate (lpk = log(persKost),
          .after = persKost)

m <- lm (data = train,
         formula = lpk ~
           nClienten +
           nCpMedew +
           nCpAfdeling + 
           nCpLokatie +
           nCpFTE +
           vrijwpClient +
           pPNIL +
           pBehandel + 
           pVerzuim +
           pInstroom +
           pUitstroom
           )

summary (m)
ar2 <- summary (m)$adj.r.squared
aic <-AIC (m)

rownames(train) <- train$organisatie
m2 <- lm (data = train,
         formula = lpk ~
           log(nClienten) +
           #nCpMedew +
           #nCpAfdeling + 
           #nCpLokatie +
           nCpFTE +
           #vrijwpClient +
           pPNIL +
           #pBehandel + 
           #pVerzuim +
           pInstroom 
           #pUitstroom
)

summary (m2)
ar2 <- c(summary (m2)$adj.r.squared, ar2)
aic <- c(AIC (m2), aic)

par (mfrow = c(2,2))
plot (m2)
