ToonVIvoorLocatie <- function (lc) {
  aap <- df[df$locatie == lc , ]
}

ToonVIvoorOrganisatieID <- function (ogi) {
  aap <- organisaties [organisaties$organisatie_ID==16, "organisatie"]
  noot <- df[df$organisatie == aap$organisatie , ]
}

# Function to list variable names of dataframe to new lines, comma seperated
fln <- function (df) {
  write_clip (paste (names(df), collapse = ",\n"))
}

# Print column names to console and return df
flr <- function (df) {
  write_clip (paste (names(df), collapse = ",\n"))
  df
}
