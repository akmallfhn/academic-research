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

analysis_name <- "Corrected Item-Total Correlation"
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

print("=== CORRECTED ITEM-TOTAL CORRELATION PER KONSTRUK ===")
print("(Corrected Item-Total Correlation < 0.30 -> tinjau item)")
cat("\n--- Metadata ---\n")
cat("Analysis:", analysis_name, "\n")
cat("Run at  :", run_at, "\n")
cat("Owner   :", owner, "\n")

result_rows <- list()

for (name in names(constructs)) {
  items <- constructs[[name]]
  alpha_val <- psych::alpha(df[, items])
  item_stats <- alpha_val$item.stats

  result_rows[[name]] <- data.frame(
    Construct = name,
    Item = rownames(item_stats),
    CorrectedItemTotalCorr = round(item_stats$r.cor, 3),
    Status = vapply(item_stats$r.cor, interpret_corr, character(1)),
    row.names = NULL
  )
}

result <- do.call(rbind, result_rows)
rownames(result) <- NULL

cat("\n--- Result Table ---\n")
print(result)

write.csv(result, "corrected_item_total_corr.csv", row.names = FALSE)
