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

analysis_name <- "Component Matrix, AVE, and Composite Reliability"
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

interpret_loading <- function(value) {
  if (is.na(value)) {
    return("Tidak dapat dihitung")
  }
  if (value >= 0.708) {
    return("Ideal")
  }
  if (value >= 0.50) {
    return("Memadai")
  }
  "Lemah"
}

interpret_ave <- function(value) {
  if (is.na(value)) {
    return("Tidak dapat dihitung")
  }
  if (value >= 0.50) {
    return("Terpenuhi")
  }
  "Perlu evaluasi"
}

interpret_composite_reliability <- function(value) {
  if (is.na(value)) {
    return("Tidak dapat dihitung")
  }
  if (value >= 0.70) {
    return("Terpenuhi")
  }
  "Perlu evaluasi"
}

calc_one_factor_metrics <- function(sub_df) {
  fit <- tryCatch(
    fa(sub_df, nfactors = 1, rotate = "none", fm = "ml"),
    error = function(e) {
      fa(sub_df, nfactors = 1, rotate = "none", fm = "pa")
    }
  )

  loadings <- abs(as.numeric(fit$loadings[, 1]))
  ave <- mean(loadings^2, na.rm = TRUE)
  composite_reliability <- (sum(loadings, na.rm = TRUE)^2) /
    ((sum(loadings, na.rm = TRUE)^2) + sum(1 - loadings^2, na.rm = TRUE))

  list(
    loadings = loadings,
    ave = ave,
    composite_reliability = composite_reliability
  )
}

result_rows <- list()

for (name in names(constructs)) {
  items <- constructs[[name]]
  sub_df <- df[, items]
  metrics <- calc_one_factor_metrics(sub_df)

  result_rows[[name]] <- data.frame(
    Construct = name,
    Item = items,
    Loading = round(metrics$loadings, 3),
    Communality = round(metrics$loadings^2, 3),
    LoadingStatus = vapply(metrics$loadings, interpret_loading, character(1)),
    ItemCount = length(items),
    AVE = round(metrics$ave, 3),
    AVEStatus = interpret_ave(metrics$ave),
    CompositeReliability = round(metrics$composite_reliability, 3),
    CompositeReliabilityStatus = interpret_composite_reliability(metrics$composite_reliability),
    row.names = NULL
  )
}

result <- do.call(rbind, result_rows)
rownames(result) <- NULL

cat("=== COMPONENT MATRIX, AVE, AND COMPOSITE RELIABILITY ===\n")
cat("(Loading >= 0.50 memadai; AVE >= 0.50; Composite Reliability >= 0.70)\n")
cat("\n--- Metadata ---\n")
cat("Analysis:", analysis_name, "\n")
cat("Run at  :", run_at, "\n")
cat("Owner   :", owner, "\n")

cat("\n--- Result Table ---\n")
print(result)

write.csv(result, "component_matrix.csv", row.names = FALSE)
