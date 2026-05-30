library(psych)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

df <- read.csv("questioner.csv", header = TRUE, sep = ",")

# EFA 7 faktor
efa <- fa(df, nfactors = 7, rotate = "oblimin", fm = "ml")

print("=== TOTAL VARIANCE EXPLAINED ===")
variance <- efa$Vaccounted
variance_df <- as.data.frame(t(variance))
colnames(variance_df) <- rownames(variance)
variance_df$Factor <- paste0("Factor", 1:nrow(variance_df))
variance_df <- variance_df[, c("Factor", colnames(variance_df)[colnames(variance_df) != "Factor"])]
rownames(variance_df) <- NULL

print(round(variance_df, 3))

# Scree plot
print("\n=== SCREE PLOT ===")
eigenvalues <- efa$values
scree_df <- data.frame(
  Factor     = 1:length(eigenvalues),
  Eigenvalue = round(eigenvalues, 3)
)
print(scree_df[scree_df$Eigenvalue > 0, ])

# Plot
plot(eigenvalues, type = "b", pch = 19,
     main = "Scree Plot",
     xlab = "Factor Number",
     ylab = "Eigenvalue",
     col  = "steelblue")
abline(h = 1, lty = 2, col = "red")
