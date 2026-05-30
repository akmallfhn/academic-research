library(psych)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

df <- read.csv("questioner.csv", header = TRUE, sep = ",")

# Setiap variabel dianalisis sebagai 1 faktor (single-factor EFA per konstruk)
constructs <- list(
  RS  = c("RS1", "RS2", "RS3", "RS4"),
  RL  = c("RL1", "RL2", "RL3", "RL4"),
  CR  = c("CR1", "CR2", "CR3", "CR4"),
  EM  = c("EM1", "EM2", "EM3", "EM4"),
  CT  = c("CT1", "CT2", "CT3", "CT4"),
  AT  = c("AT1", "AT2", "AT3", "AT4"),
  ITA = c("ITA1", "ITA2", "ITA3", "ITA4", "ITA5")
)

for (name in names(constructs)) {
  items <- constructs[[name]]
  sub_df <- df[, items]

  efa <- fa(sub_df, nfactors = 1, rotate = "none", fm = "ml")

  cat("\n=== COMPONENT / FACTOR MATRIX:", name, "===\n")
  cat("(Loading >= 0.50 dianggap signifikan)\n")

  loadings_df <- data.frame(
    Item   = items,
    Factor = round(as.numeric(efa$loadings[, 1]), 3),
    row.names = NULL
  )
  print(loadings_df)
}
