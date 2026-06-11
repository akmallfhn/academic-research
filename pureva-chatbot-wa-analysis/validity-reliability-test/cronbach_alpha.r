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

analysis_name <- "Cronbach's Alpha"
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

interpret_alpha <- function(value) {
  if (is.na(value)) {
    return("Tidak dapat dihitung")
  }
  if (value >= 0.80) {
    return("Sangat baik")
  }
  if (value >= 0.70) {
    return("Baik")
  }
  if (value >= 0.60) {
    return("Cukup, perlu evaluasi")
  }
  "Lemah, item perlu dievaluasi"
}

interpret_average_r <- function(value) {
  if (is.na(value)) {
    return("Tidak dapat dihitung")
  }
  if (value >= 0.15 && value <= 0.50) {
    return("Ideal")
  }
  if (value > 0.50) {
    return("Terlalu repetitif")
  }
  "Terlalu rendah"
}

result_rows <- list()

for (name in names(constructs)) {
  items <- constructs[[name]]
  alpha_result <- psych::alpha(df[, items], check.keys = TRUE)

  raw_alpha <- alpha_result$total$raw_alpha
  std_alpha <- alpha_result$total$std.alpha
  average_r <- alpha_result$total$average_r

  result_rows[[name]] <- data.frame(
    Construct = name,
    ItemCount = length(items),
    RawAlpha = round(raw_alpha, 3),
    StandardizedAlpha = round(std_alpha, 3),
    G6 = round(alpha_result$total[["G6(smc)"]], 3),
    AverageInterItemCorrelation = round(average_r, 3),
    CronbachAlphaStatus = interpret_alpha(raw_alpha),
    MeanInterItemStatus = interpret_average_r(average_r),
    row.names = NULL
  )
}

result <- do.call(rbind, result_rows)
rownames(result) <- NULL

cat("=== CRONBACH'S ALPHA ===\n")
cat("(Cronbach alpha >= 0.70 baik; mean inter-item correlation ideal 0.15-0.50)\n")
cat("\n--- Metadata ---\n")
cat("Analysis:", analysis_name, "\n")
cat("Run at  :", run_at, "\n")
cat("Owner   :", owner, "\n")

cat("\n--- Result Table ---\n")
print(result)

write.csv(result, "cronbach_alpha.csv", row.names = FALSE)
