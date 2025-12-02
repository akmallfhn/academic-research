library(psych)

df <- read.csv("questioner.csv", header = TRUE, sep = ",")

# Pilih hanya numeric columns
items <- df[sapply(df, is.numeric)]

# Hitung communalities (Initial & Extraction)
# Initial = nilai komunialitas awal dari Principal Component Analysis (PCA)
# Extraction = komunlitas setelah faktor diekstraksi.
pca_res <- principal(items, nfactors = 3, rotate = "none", method = "principal")

comm <- pca_res$communality

communalities <- data.frame(
    Item = names(comm),
    Initial = rep(1, length(comm)),
    Extraction = as.numeric(comm)
)

print("=== COMMUNALITIES ===")
print(communalities)
