library(psych)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

df <- read.csv("questioner.csv", header = TRUE, sep = ",")

# EFA dengan 7 faktor (sesuai jumlah konstruk)
efa <- fa(df, nfactors = 7, rotate = "oblimin", fm = "ml")

print("=== COMMUNALITIES ===")
communalities_df <- data.frame(
  Item       = names(efa$communalities),
  Initial    = round(efa$communalities, 3),
  Extraction = round(efa$communalities, 3),
  row.names  = NULL
)
print(communalities_df)

print("\nCatatan: Nilai communality >= 0.50 menunjukkan item memiliki kesamaan")
print("yang cukup dengan faktor lain dalam model.")
