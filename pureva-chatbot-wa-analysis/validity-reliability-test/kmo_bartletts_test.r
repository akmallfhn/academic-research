library(psych)
library(dplyr)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

df <- read.csv("pre-test-questioner.csv", header = TRUE, sep = ",")
numeric_df <- df %>% select(where(is.numeric))

# Hitung KMO (Kaiser-Meyer-Olkin Measure of Sampling Adequacy)
kmo_result <- KMO(numeric_df)
kmo_overall <- kmo_result$MSA

# Hitung Bartlett's Test of Sphericity
bartlett_result <- cortest.bartlett(cor(numeric_df), n = nrow(numeric_df))

print("=== FACTOR ANALYSIS ===")
print(paste("Kaiser-Meyer-Olkin Measure of Sampling Adequacy:", round(kmo_overall, 3)))
print(paste("Approx Chi Square:", round(bartlett_result$chisq, 3)))
print(paste("df:", bartlett_result$df))
print(paste("Sig:", round(bartlett_result$p.value, 4)))
