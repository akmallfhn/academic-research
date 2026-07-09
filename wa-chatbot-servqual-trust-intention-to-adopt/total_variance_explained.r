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

analysis_name <- "Total Variance Explained"
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

all_items <- unique(unlist(constructs))
efa <- tryCatch(
  fa(df[, all_items], nfactors = length(constructs), rotate = "oblimin", fm = "ml"),
  error = function(e) {
    fa(df[, all_items], nfactors = length(constructs), rotate = "oblimin", fm = "pa")
  }
)

variance <- as.data.frame(t(efa$Vaccounted), check.names = FALSE)
variance$Factor <- paste0("Factor", seq_len(nrow(variance)))
variance_result <- variance[, c("Factor", setdiff(names(variance), "Factor"))]
rownames(variance_result) <- NULL

names(variance_result) <- c(
  "Factor",
  "SSLoadings",
  "ProportionVar",
  "CumulativeVar",
  "ProportionExplained",
  "CumulativeProportion"
)

variance_result[, -1] <- round(variance_result[, -1], 3)

cat("=== TOTAL VARIANCE EXPLAINED ===\n")
cat("\n--- Metadata ---\n")
cat("Analysis:", analysis_name, "\n")
cat("Run at  :", run_at, "\n")
cat("Owner   :", owner, "\n")

cat("\n--- Result Table: Total Variance Explained ---\n")
print(variance_result)

write.csv(variance_result, "total_variance_explained.csv", row.names = FALSE)
