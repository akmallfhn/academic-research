library(psych)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

df <- read.csv("questioner.csv", header = TRUE, sep = ",")

print("=== ITEM STATISTICS ===")

stats <- data.frame(
  Item   = colnames(df),
  N      = sapply(df, function(x) sum(!is.na(x))),
  Mean   = round(sapply(df, mean, na.rm = TRUE), 3),
  StdDev = round(sapply(df, sd, na.rm = TRUE), 3),
  Min    = sapply(df, min, na.rm = TRUE),
  Max    = sapply(df, max, na.rm = TRUE)
)

print(stats)

print("=== DESCRIPTIVE STATISTICS (psych) ===")
print(describe(df)[, c("n", "mean", "sd", "min", "max", "skew", "kurtosis")])
