# AGENT.md

## Scope

This guidance applies to R analysis scripts in:

`wa-chatbot-servqual-trust-intention-to-adopt`

Use `corrected_item_total_corr.r` as the current standard shape for small
single-metric scripts.

## R Script Structure Standard

Write scripts in this order:

1. Library imports
2. `get_script_dir()`
3. `setwd(get_script_dir())`
4. Metadata variables
5. Data loading
6. Construct definition
7. Small helper functions
8. Calculation loop
9. Result table assembly
10. Metadata print
11. Result print
12. CSV write

## Library Imports

Put libraries at the top of the file.

Use direct imports:

```r
library(psych)
```

Do not use `suppressPackageStartupMessages()` unless the user explicitly asks
for quiet package loading.

## Script Directory

Each runnable R script should resolve its own directory before reading CSV
files. This keeps the script runnable from RStudio and from `Rscript`.

Use this pattern:

```r
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
```

## Metadata

Define metadata before loading or calculating results.

Use simple variables:

```r
analysis_name <- "Corrected Item-Total Correlation"
run_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
owner <- "Akmal Luthfiansyah"
```

Print metadata before the result table. Do not add metadata as repeated CSV
columns unless explicitly requested.

Use this print pattern:

```r
cat("\n--- Metadata ---\n")
cat("Analysis:", analysis_name, "\n")
cat("Run at  :", run_at, "\n")
cat("Owner   :", owner, "\n")
```

## Constructs

Keep construct mapping close to the top of each script, after data loading.

Use the same construct names and item names consistently:

```r
constructs <- list(
  Responsiveness = c("RS1", "RS2", "RS3", "RS4"),
  Reliability = c("RL1", "RL2", "RL3", "RL4"),
  Credibility = c("CR1", "CR2", "CR3", "CR4"),
  Empathy = c("EM1", "EM2", "EM3", "EM4"),
  "Cognitive Trust" = c("CT1", "CT2", "CT3", "CT4"),
  "Affective Trust" = c("AT1", "AT2", "AT3", "AT4"),
  "Digital Health Service Intention to Adopt" = c("ITA1", "ITA2", "ITA3", "ITA4", "ITA5")
)
```

## Result Table

Prefer one combined table with a `Construct` column instead of printing one
separate table per construct.

Use `result_rows <- list()`, append per construct, then combine:

```r
result <- do.call(rbind, result_rows)
rownames(result) <- NULL
```

Print the table after metadata:

```r
cat("\n--- Result Table ---\n")
print(result)
```

## CSV Output

Write the CSV at the end of the script.

Use the same snake_case base name as the script:

```r
write.csv(result, "corrected_item_total_corr.csv", row.names = FALSE)
```

CSV output should contain only the result table columns unless the user asks
for metadata columns.

## Naming

Use snake_case for new R files and CSV outputs.

Do not keep duplicate kebab-case versions when a snake_case replacement exists.

Examples:

- `corrected_item_total_corr.r`
- `corrected_item_total_corr.csv`
- `item_total_statistics.r`
- `item_statistics.r`

## Linting

The workspace uses `.lintr`.

Current intentional disables:

- `line_length_linter`
- `object_length_linter`

Keep other lint warnings meaningful unless the user asks to relax them.
