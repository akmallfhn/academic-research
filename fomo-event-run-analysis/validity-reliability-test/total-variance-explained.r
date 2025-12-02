library(psych)
library(dplyr)

df <- read.csv("questioner.csv", header = TRUE, sep = ",")
numeric_df <- df %>% select(where(is.numeric))

pca_res <- principal(numeric_df, nfactors = ncol(numeric_df), rotate = "none")

# Eigenvalues dan varians awal
eigenvalues <- pca_res$values
variance_percent <- pca_res$Vaccounted[2, ] * 100
cumulative_percent <- cumsum(variance_percent)

# Extraction Sums of Squared Loadings
n_factors_extract <- 3
total_extraction <- ifelse(seq_along(eigenvalues) <= n_factors_extract, eigenvalues, 0)
variance_extraction_percent <- total_extraction / sum(total_extraction) * 100

total_variance_explained <- data.frame(
    Component = seq_along(eigenvalues),
    Total = eigenvalues,
    Initial_Eigenvalues_Perc_of_Variance = variance_percent,
    Cumulative = cumulative_percent,
    Total_Extraction_SS_Loading = total_extraction,
    Perc_of_Variance_Extraction_SS_Loading = variance_extraction_percent
)

print(total_variance_explained)
