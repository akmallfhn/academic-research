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

analysis_name <- "Fornell-Larcker Criterion"
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

calc_one_factor_metrics <- function(sub_df) {
  fit <- tryCatch(
    fa(sub_df, nfactors = 1, rotate = "none", fm = "ml"),
    error = function(e) {
      fa(sub_df, nfactors = 1, rotate = "none", fm = "pa")
    }
  )

  loadings <- abs(as.numeric(fit$loadings[, 1]))
  ave <- mean(loadings^2, na.rm = TRUE)

  list(
    loadings = loadings,
    ave = ave
  )
}

construct_scores <- as.data.frame(lapply(constructs, function(items) {
  rowMeans(df[, items], na.rm = TRUE)
}), check.names = FALSE)

ave_rows <- list()

for (name in names(constructs)) {
  items <- constructs[[name]]
  metrics <- calc_one_factor_metrics(df[, items])

  ave_rows[[name]] <- data.frame(
    Construct = name,
    AVE = metrics$ave,
    SqrtAVE = sqrt(metrics$ave),
    row.names = NULL
  )
}

ave_result <- do.call(rbind, ave_rows)
rownames(ave_result) <- NULL

score_cor <- cor(construct_scores, use = "pairwise.complete.obs")
fornell_larcker <- score_cor
diag(fornell_larcker) <- ave_result$SqrtAVE[
  match(colnames(fornell_larcker), ave_result$Construct)
]

result <- data.frame(
  Construct = rownames(fornell_larcker),
  round(fornell_larcker, 3),
  check.names = FALSE,
  row.names = NULL
)

cat("=== FORNELL-LARCKER CRITERION ===\n")
cat("(Diagonal = sqrt(AVE); luar diagonal = korelasi antar konstruk)\n")
cat("\n--- Metadata ---\n")
cat("Analysis:", analysis_name, "\n")
cat("Run at  :", run_at, "\n")
cat("Owner   :", owner, "\n")

cat("\n--- Result Table ---\n")
print(result)

write.csv(result, "fornell_larcker.csv", row.names = FALSE)
