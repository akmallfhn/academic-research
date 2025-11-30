library(dplyr)

df <- read.csv("questioner.csv", header = TRUE, sep = ",")

# Buat Items of Statistics
items_of_statistics <- df %>%
    # Operasi ke semua kolom
    summarise(across(
        everything(),
        list(
            Mean = ~ mean(., na.rm = TRUE),
            StdDeviation = ~ sd(., na.rm = TRUE),
            N = ~ sum(!is.na(.))
        ),
        .names = "{.col}_{.fn}"
    )) %>%
    # Pivot Table
    pivot_longer(
        everything(),
        names_to = c("Item", ".value"),
        names_sep = "_"
    )

print("=== ITEMS OF STATISTICS ===")
print(items_of_statistics, n = Inf)
