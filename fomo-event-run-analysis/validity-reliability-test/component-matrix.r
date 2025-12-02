library(dplyr)
library(psych)

df <- read.csv("questioner.csv", header = TRUE, sep = ",")
numeric_df <- df %>% select(where(is.numeric))

pca_rotated <- principal(numeric_df, nfactors = 3, rotate = "none")

component_matrix <- as.data.frame(unclass(pca_rotated$loadings))
colnames(component_matrix) <- c("Component 1", "Component 2", "Component 3")

print("=== COMPONENT MATRIX ===")
print(component_matrix)
