library(psych)

get_script_dir <- function() {
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    path <- rstudioapi::getSourceEditorContext()$path
    if (!is.null(path) && nzchar(path)) {
      return(dirname(normalizePath(path)))
    }
  }

  file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(file_arg) > 0) {
    path <- sub("^--file=", "", file_arg[1])
    if (file.exists(path)) {
      return(dirname(normalizePath(path)))
    }
  }

  getwd()
}

setwd(get_script_dir())

analysis_name <- "Descriptive Analysis"
run_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
owner <- "Akmal Luthfiansyah"

df <- read.csv("questioner.csv", header = TRUE, sep = ",")

constructs <- list(
  Responsiveness = c("RS1", "RS2", "RS3", "RS4"),
  Reliability = c("RL1", "RL2", "RL3", "RL4"),
  Credibility = c("CR1", "CR2", "CR3", "CR4"),
  Empathy = c("EM1", "EM2", "EM3", "EM4"),
  "Cognitive Trust" = c("CT1", "CT2", "CT3", "CT4"),
  "Affective Trust" = c("AT1", "AT2", "AT3", "AT4"),
  "Digital Health Service Intention to Adopt" = c("ITA1", "ITA2", "ITA3", "ITA4", "ITA5")
)

missing_items <- setdiff(unlist(constructs), names(df))
if (length(missing_items) > 0) {
  stop("Item berikut tidak ditemukan di questioner.csv: ", paste(missing_items, collapse = ", "))
}

interpret_mean <- function(value) {
  if (is.na(value)) {
    return("Tidak dapat dihitung")
  }
  if (value >= 5.81) {
    return("Sangat tinggi")
  }
  if (value >= 4.61) {
    return("Tinggi")
  }
  if (value >= 3.41) {
    return("Sedang")
  }
  if (value >= 2.21) {
    return("Rendah")
  }
  "Sangat rendah"
}

make_descriptive_row <- function(level, construct, item, values) {
  desc <- psych::describe(values)

  data.frame(
    Level = level,
    Construct = construct,
    Item = item,
    N = desc$n,
    Mean = round(desc$mean, 3),
    StdDev = round(desc$sd, 3),
    Min = desc$min,
    Max = desc$max,
    Skewness = round(desc$skew, 3),
    Kurtosis = round(desc$kurtosis, 3),
    MeanStatus = interpret_mean(desc$mean),
    row.names = NULL
  )
}

result_rows <- list()

for (name in names(constructs)) {
  items <- constructs[[name]]

  for (item in items) {
    result_rows[[length(result_rows) + 1]] <- make_descriptive_row(
      "Item",
      name,
      item,
      df[[item]]
    )
  }

  construct_score <- rowMeans(df[, items], na.rm = TRUE)
  result_rows[[length(result_rows) + 1]] <- make_descriptive_row(
    "Construct",
    name,
    "",
    construct_score
  )
}

result <- do.call(rbind, result_rows)
rownames(result) <- NULL

cat("=== DESCRIPTIVE ANALYSIS ===\n")
cat("(Mean status memakai rentang skala Likert 1-7)\n")
cat("\n--- Metadata ---\n")
cat("Analysis:", analysis_name, "\n")
cat("Run at  :", run_at, "\n")
cat("Owner   :", owner, "\n")

cat("\n--- Result Table ---\n")
print(result)

write.csv(result, "descriptive_analysis.csv", row.names = FALSE)
