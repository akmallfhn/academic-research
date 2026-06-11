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

analysis_name <- "Item-Total Statistics"
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

interpret_corr <- function(value) {
  if (is.na(value)) {
    return("Tidak dapat dihitung")
  }
  if (value >= 0.50) {
    return("Kuat")
  }
  if (value >= 0.30) {
    return("Memadai")
  }
  "Lemah"
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

calc_deleted_scale_stats <- function(sub_df, item) {
  other_items <- setdiff(colnames(sub_df), item)
  complete_sub <- sub_df[complete.cases(sub_df), , drop = FALSE]
  deleted_scores <- rowSums(complete_sub[, other_items, drop = FALSE])

  list(
    mean = mean(deleted_scores, na.rm = TRUE),
    variance = var(deleted_scores, na.rm = TRUE)
  )
}

result_rows <- list()

for (name in names(constructs)) {
  items <- constructs[[name]]
  sub_df <- df[, items]
  alpha_val <- psych::alpha(sub_df, check.keys = TRUE)
  item_stats <- alpha_val$item.stats
  alpha_drop <- alpha_val$alpha.drop

  for (item in rownames(item_stats)) {
    deleted_stats <- calc_deleted_scale_stats(sub_df, item)
    corrected_corr <- item_stats[item, "r.drop"]
    alpha_if_deleted <- alpha_drop[item, "raw_alpha"]

    result_rows[[length(result_rows) + 1]] <- data.frame(
      Construct = name,
      Item = item,
      ItemMean = round(item_stats[item, "mean"], 3),
      ItemStdDev = round(item_stats[item, "sd"], 3),
      ScaleMeanIfItemDeleted = round(deleted_stats$mean, 3),
      ScaleVarianceIfItemDeleted = round(deleted_stats$variance, 3),
      RawItemTotalCorr = round(item_stats[item, "raw.r"], 3),
      CorrectedItemTotalCorr = round(corrected_corr, 3),
      CorrectedItemTotalStatus = interpret_corr(corrected_corr),
      CronbachAlphaIfItemDeleted = round(alpha_if_deleted, 3),
      AlphaIfDeletedStatus = interpret_alpha(alpha_if_deleted),
      row.names = NULL
    )
  }
}

result <- do.call(rbind, result_rows)
rownames(result) <- NULL

cat("=== ITEM-TOTAL STATISTICS ===\n")
cat("(Corrected item-total correlation >= 0.30; Cronbach alpha if item deleted >= 0.70)\n")
cat("\n--- Metadata ---\n")
cat("Analysis:", analysis_name, "\n")
cat("Run at  :", run_at, "\n")
cat("Owner   :", owner, "\n")

cat("\n--- Result Table ---\n")
print(result)

write.csv(result, "item_total_statistics.csv", row.names = FALSE)
