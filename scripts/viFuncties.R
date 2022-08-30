ToonVIvoorLocatie <- function (lc) {
  aap <- df[df$locatie == lc , ]
}

ToonVIvoorOrganisatieID <- function (ogi) {
  aap <- organisaties [organisaties$organisatie_ID==16, "organisatie"]
  noot <- df[df$organisatie == aap$organisatie , ]
}