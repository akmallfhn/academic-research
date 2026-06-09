library(psych)
library(tibble)
library(dplyr)

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

print("=== ITEM-TOTAL STATISTICS PER KONSTRUK ===")
print("(Corrected Item-Total Correlation < 0.30 → pertimbangkan hapus item)")

for (name in names(constructs)) {
  items <- constructs[[name]]
  alpha_val <- psych::alpha(df[, items])

  alpha_drop <- alpha_val$alpha.drop
  items_stats <- alpha_val$item.stats

  cat("\n---", name, "---\n")
  result <- data.frame(
    Item                       = rownames(items_stats),
    ScaleMeanIfItemDeleted     = round(items_stats$mean, 3),
    CorrectedItemTotalCorr     = round(items_stats$r.cor, 3),
    SquaredMultipleCorr        = round(items_stats$r.drop, 3),
    CronbachAlphaIfItemDeleted = round(alpha_drop$raw_alpha, 3)
  )
  print(result)
}
