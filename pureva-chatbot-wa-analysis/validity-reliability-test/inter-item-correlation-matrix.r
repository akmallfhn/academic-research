setwd(dirname(rstudioapi::getSourceEditorContext()$path))

df <- read.csv("questioner.csv", header = TRUE, sep = ",")

constructs <- list(
  Responsiveness                              = c("RS1", "RS2", "RS3", "RS4"),
  Reliability                                 = c("RL1", "RL2", "RL3", "RL4"),
  Credibility                                 = c("CR1", "CR2", "CR3", "CR4"),
  Empathy                                     = c("EM1", "EM2", "EM3", "EM4"),
  "Cognitive Trust"                           = c("CT1", "CT2", "CT3", "CT4"),
  "Affective Trust"                           = c("AT1", "AT2", "AT3", "AT4"),
  "Digital Health Service Intention to Adopt" = c("ITA1", "ITA2", "ITA3", "ITA4", "ITA5")
)

print("=== INTER-ITEM CORRELATION MATRIX PER KONSTRUK ===")

for (name in names(constructs)) {
  items <- constructs[[name]]
  cor_matrix <- cor(df[, items], use = "pairwise.complete.obs")
  cat("\n---", name, "---\n")
  print(round(cor_matrix, 3))
}

cat("\n--- FULL CORRELATION MATRIX (semua 29 item) ---\n")
print(round(cor(df, use = "pairwise.complete.obs"), 3))
