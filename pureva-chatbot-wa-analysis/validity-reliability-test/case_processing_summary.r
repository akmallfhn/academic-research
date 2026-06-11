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

analysis_name <- "Case Processing Summary"
run_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
owner <- "Akmal Luthfiansyah"

df <- read.csv("questioner.csv", header = TRUE, sep = ",")

total_cases <- nrow(df)
valid_cases <- sum(complete.cases(df))
excluded_cases <- total_cases - valid_cases

result <- data.frame(
  Category = c("Valid", "Excluded (missing)", "Total"),
  N = c(valid_cases, excluded_cases, total_cases),
  Percent = round(c(valid_cases, excluded_cases, total_cases) / total_cases * 100, 1),
  row.names = NULL
)

cat("=== CASE PROCESSING SUMMARY ===\n")
cat("(Valid = responden tanpa missing value di semua item)\n")
cat("\n--- Metadata ---\n")
cat("Analysis:", analysis_name, "\n")
cat("Run at  :", run_at, "\n")
cat("Owner   :", owner, "\n")

cat("\n--- Result Table ---\n")
print(result)

write.csv(result, "case_processing_summary.csv", row.names = FALSE)
