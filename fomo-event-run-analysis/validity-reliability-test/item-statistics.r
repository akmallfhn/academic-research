library(dplyr)
library(tidyr)

df <- read.csv("questioner.csv", header = TRUE, sep = ",")
numeric_df <- df %>% select(where(is.numeric))

# Buat Items of Statistics
item_statistics <- numeric_df %>%
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
print(item_statistics, n = Inf)
