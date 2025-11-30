df <- read.csv("questioner.csv", header = TRUE, sep = ",")

# Hitung inter-item correlation
cor_matrix <- cor(df, use = "pairwise.complete.obs")

print("=== INTER CORRELATION MATRIX ===")
print(cor_matrix)
