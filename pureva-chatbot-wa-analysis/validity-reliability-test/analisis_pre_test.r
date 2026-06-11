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

analysis_name <- "Pre-Test Validity and Reliability"
run_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
owner <- "Akmal Luthfiansyah"

df <- read.csv("pre_test_questioner.csv", header = TRUE, sep = ",")

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
  stop("Item berikut tidak ditemukan di pre_test_questioner.csv: ", paste(missing_items, collapse = ", "))
}

interpret_loading <- function(value) {
  if (is.na(value)) {
    return("Tidak dapat dihitung")
  }
  if (abs(value) >= 0.70) {
    return("Ideal")
  }
  if (abs(value) >= 0.50) {
    return("Valid")
  }
  "Tidak valid"
}

interpret_kmo <- function(value) {
  if (is.na(value)) {
    return("Tidak dapat dihitung")
  }
  if (value >= 0.80) {
    return("Baik")
  }
  if (value >= 0.60) {
    return("Cukup")
  }
  if (value >= 0.50) {
    return("Minimum")
  }
  "Tidak layak"
}

interpret_alpha <- function(value) {
  if (is.na(value)) {
    return("Tidak dapat dihitung")
  }
  if (value >= 0.70) {
    return("Terpenuhi")
  }
  "Perlu evaluasi"
}

interpret_bartlett <- function(p_value) {
  if (is.na(p_value)) {
    return("Tidak dapat dihitung")
  }
  if (p_value < 0.05) {
    return("Signifikan")
  }
  "Tidak signifikan"
}

result_rows <- list()

for (name in names(constructs)) {
  items <- constructs[[name]]
  sub_df <- df[, items]
  cor_matrix <- cor(sub_df, use = "pairwise.complete.obs")

  kmo_result <- KMO(cor_matrix)
  bartlett_result <- cortest.bartlett(cor_matrix, n = nrow(sub_df))
  alpha_result <- psych::alpha(sub_df, check.keys = TRUE)
  loading_result <- principal(sub_df, nfactors = 1, rotate = "none")

  loadings <- as.numeric(loading_result$loadings[, 1])

  result_rows[[name]] <- data.frame(
    Construct = name,
    Item = items,
    ItemCount = length(items),
    KMO = round(kmo_result$MSA, 3),
    KMOStatus = interpret_kmo(kmo_result$MSA),
    CronbachAlpha = round(alpha_result$total$raw_alpha, 3),
    CronbachAlphaStatus = interpret_alpha(alpha_result$total$raw_alpha),
    BartlettPValue = sprintf("%.3f", bartlett_result$p.value),
    BartlettStatus = interpret_bartlett(bartlett_result$p.value),
    FactorLoading = round(loadings, 3),
    LoadingStatus = vapply(loadings, interpret_loading, character(1)),
    row.names = NULL
  )
}

result <- do.call(rbind, result_rows)
rownames(result) <- NULL

cat("=== PRE-TEST VALIDITY AND RELIABILITY ===\n")
cat("(Loading >= 0.50; KMO >= 0.50; alpha >= 0.70; Bartlett p-value < 0.05)\n")
cat("\n--- Metadata ---\n")
cat("Analysis:", analysis_name, "\n")
cat("Run at  :", run_at, "\n")
cat("Owner   :", owner, "\n")

cat("\n--- Result Table ---\n")
print(result)

write.csv(result, "analisis_pre_test.csv", row.names = FALSE)
