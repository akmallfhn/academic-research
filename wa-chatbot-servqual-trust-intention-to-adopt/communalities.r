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

analysis_name <- "Communalities"
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

interpret_communality <- function(value) {
  if (is.na(value)) {
    return("Tidak dapat dihitung")
  }
  if (value >= 0.50) {
    return("Memadai")
  }
  "Lemah"
}

all_items <- unique(unlist(constructs))
efa <- tryCatch(
  fa(df[, all_items], nfactors = length(constructs), rotate = "oblimin", fm = "ml"),
  error = function(e) {
    fa(df[, all_items], nfactors = length(constructs), rotate = "oblimin", fm = "pa")
  }
)

result <- data.frame(
  Item = names(efa$communality),
  Initial = round(efa$communality, 3),
  Extraction = round(efa$communality, 3),
  Status = vapply(efa$communality, interpret_communality, character(1)),
  row.names = NULL
)

cat("=== COMMUNALITIES ===\n")
cat("(Communality >= 0.50 menunjukkan item cukup dijelaskan oleh faktor)\n")
cat("\n--- Metadata ---\n")
cat("Analysis:", analysis_name, "\n")
cat("Run at  :", run_at, "\n")
cat("Owner   :", owner, "\n")

cat("\n--- Result Table ---\n")
print(result)

write.csv(result, "communalities.csv", row.names = FALSE)
