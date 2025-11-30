library(dplyr)

df <- read.csv("questioner.csv", header = TRUE, sep = ",")

# Buat Case Processing Summary dari Table
case_processing_summary <- df %>%
    summarise(
        # Count Valid Cases
        N_Cases_Valid = sum(complete.cases(.)),
        N_Cases_Excluded = n() - sum(complete.cases(.)),
        N_Cases_Total = n(),
        # Percent Valid Cases
        Perc_Cases_Valid = (sum(complete.cases(.)) / n()) * 100,
        Perc_Cases_Excluded = (n() - sum(complete.cases(.))) / n() * 100,
        Perc_Cases_Total = 100,
    )

print("=== CASE PROCESSING SUMMARY ===")
print(case_processing_summary)
