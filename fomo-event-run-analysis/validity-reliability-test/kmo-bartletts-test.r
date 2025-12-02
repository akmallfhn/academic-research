library(psych)
library(dplyr)

df <- read.csv("questioner.csv", header = TRUE, sep = ",")
numeric_df <- df %>% select(where(is.numeric))

# Hitung KMO (Kaiser-Meyer-Olkin Measure of Sampling Adequacy)
kmo_result <- KMO(numeric_df)
kmo_overall <- kmo_result$MSA

# Hitung Bartlett’s Test of Sphericity
bartlett_result <- cortest.bartlett(cor(numeric_df), n = nrow(numeric_df))


print("=== FACTOR ANALYSIS ===")
print(paste("Kaiser-Meyer-Olkin Measure of Sampling Adequacy", kmo_overall))
print(paste("Approx Chi Square", bartlett_result$chisq))
print(paste("df", bartlett_result$df))
print(paste("Sig", bartlett_result$p.value))
