library(psych)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

df <- read.csv("questioner.csv", header = TRUE, sep = ",")

# Menghitung Cronbach's Alpha per konstruk
constructs <- list(
  Responsiveness                             = c("RS1", "RS2", "RS3", "RS4"),
  Reliability                                = c("RL1", "RL2", "RL3", "RL4"),
  Credibility                                = c("CR1", "CR2", "CR3", "CR4"),
  Empathy                                    = c("EM1", "EM2", "EM3", "EM4"),
  "Cognitive Trust"                          = c("CT1", "CT2", "CT3", "CT4"),
  "Affective Trust"                          = c("AT1", "AT2", "AT3", "AT4"),
  "Digital Health Service Intention to Adopt" = c("ITA1", "ITA2", "ITA3", "ITA4", "ITA5")
)

print("=== CRONBACH'S ALPHA PER KONSTRUK ===")
for (name in names(constructs)) {
  items <- constructs[[name]]
  result <- psych::alpha(df[, items], check.keys = TRUE)
  a <- result$total$raw_alpha
  print(paste(name, "| Alpha:", round(a, 3), "| N Items:", length(items)))
}

# Catatan: Overall alpha sengaja tidak dilaporkan karena kuesioner ini
# bersifat multidimensional (7 konstruk berbeda). Alpha per konstruk
# adalah angka yang relevan untuk dilaporkan di penelitian.
print("NOTE: Untuk kuesioner multidimensional, laporkan alpha PER KONSTRUK.")
