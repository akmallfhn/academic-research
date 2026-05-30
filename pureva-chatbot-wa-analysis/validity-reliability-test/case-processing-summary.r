setwd(dirname(rstudioapi::getSourceEditorContext()$path))

df <- read.csv("questioner.csv", header = TRUE, sep = ",")

total_cases  <- nrow(df)
valid_cases  <- sum(complete.cases(df))
excluded     <- total_cases - valid_cases

print("=== CASE PROCESSING SUMMARY ===")
summary_df <- data.frame(
  Category = c("Valid", "Excluded (missing)", "Total"),
  N        = c(valid_cases, excluded, total_cases),
  Percent  = round(c(valid_cases, excluded, total_cases) / total_cases * 100, 1)
)
print(summary_df)
