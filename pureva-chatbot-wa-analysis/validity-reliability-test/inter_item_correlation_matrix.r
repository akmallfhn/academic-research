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

analysis_name <- "Inter-Item Correlation Matrix"
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
  abs_value <- abs(value)
  if (is.na(abs_value)) {
    return("Tidak dapat dihitung")
  }
  if (abs_value >= 0.70) {
    return("Sangat kuat")
  }
  if (abs_value >= 0.50) {
    return("Kuat")
  }
  if (abs_value >= 0.30) {
    return("Sedang")
  }
  "Lemah"
}

cor_to_long <- function(table_name, construct_name, cor_matrix) {
  pairs <- expand.grid(
    ItemA = rownames(cor_matrix),
    ItemB = colnames(cor_matrix),
    stringsAsFactors = FALSE
  )
  pairs$Correlation <- round(cor_matrix[cbind(pairs$ItemA, pairs$ItemB)], 3)

  data.frame(
    Table = table_name,
    Construct = construct_name,
    ItemA = pairs$ItemA,
    ItemB = pairs$ItemB,
    Correlation = pairs$Correlation,
    Status = vapply(pairs$Correlation, interpret_corr, character(1)),
    row.names = NULL
  )
}

result_rows <- list()

for (name in names(constructs)) {
  items <- constructs[[name]]
  cor_matrix <- cor(df[, items], use = "pairwise.complete.obs")
  result_rows[[name]] <- cor_to_long("Per Konstruk", name, cor_matrix)
}

all_items <- unique(unlist(constructs))
overall_matrix <- cor(df[, all_items], use = "pairwise.complete.obs")
overall_result <- cor_to_long("Overall", "Keseluruhan", overall_matrix)

result <- do.call(rbind, c(result_rows, list(Overall = overall_result)))
rownames(result) <- NULL

cat("=== INTER-ITEM CORRELATION MATRIX ===\n")
cat("(CSV menggunakan format long: satu baris untuk setiap pasangan item)\n")
cat("\n--- Metadata ---\n")
cat("Analysis:", analysis_name, "\n")
cat("Run at  :", run_at, "\n")
cat("Owner   :", owner, "\n")

cat("\n--- Result Table ---\n")
print(result)

write.csv(result, "inter_item_correlation_matrix.csv", row.names = FALSE)
