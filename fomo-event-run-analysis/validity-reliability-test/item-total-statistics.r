library(psych)
library(tibble)
library(dplyr)

df <- read.csv("questioner.csv", header = TRUE, sep = ",")
numeric_df <- df %>% select(where(is.numeric))

alpha_vals <- psych::alpha(numeric_df)

alpha_drop <- alpha_vals$alpha.drop
items_stats <- alpha_vals$item.stats
# alpha_vals$

cronbachs <- alpha_drop["raw_alpha"]
smiid <- items_stats

# ScaleMeanIfItemDeleted
# ScaleVarIfItemDeleted
# CorrectedItemTotalCorrelation
# SquaredMultipleCorrelation
# CronbachAlphaIfItemDeleted

print(cronbachs)
print(items_stats)
