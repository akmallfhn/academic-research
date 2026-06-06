suppressPackageStartupMessages(library(psych))
suppressPackageStartupMessages(library(dplyr))

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
    return(dirname(normalizePath(path)))
  }

  getwd()
}

setwd(get_script_dir())

df_clean <- read.csv("questioner.csv", header = TRUE, sep = ",")
df_clean <- df_clean %>% mutate(across(everything(), as.numeric))

loc_service_quality <- list(
  Responsiveness = c("RS1", "RS2", "RS3", "RS4"),
  Reliability = c("RL1", "RL2", "RL3", "RL4"),
  Credibility = c("CR1", "CR2", "CR3", "CR4"),
  Empathy = c("EM1", "EM2", "EM3", "EM4")
)

other_constructs <- list(
  "Cognitive Trust" = c("CT1", "CT2", "CT3", "CT4"),
  "Affective Trust" = c("AT1", "AT2", "AT3", "AT4"),
  "Digital Health Service Intention to Adopt" = c("ITA1", "ITA2", "ITA3", "ITA4", "ITA5")
)

all_required_items <- c(unlist(loc_service_quality), unlist(other_constructs))
missing_items <- setdiff(all_required_items, names(df_clean))
if (length(missing_items) > 0) {
  stop("Item berikut tidak ditemukan di questioner.csv: ", paste(missing_items, collapse = ", "))
}

interpret_alpha <- function(value) {
  if (is.na(value)) return("Tidak dapat dihitung")
  if (value >= 0.8) return("Sangat baik")
  if (value >= 0.7) return("Baik")
  if (value >= 0.6) return("Cukup, perlu evaluasi")
  "Lemah, item perlu dievaluasi"
}

interpret_vif <- function(value) {
  if (is.na(value)) return("Tidak dapat dihitung")
  if (value < 3.3) return("Ideal")
  if (value < 5.0) return("Masih dapat diterima")
  "Kolinearitas tinggi"
}

interpret_loading <- function(value) {
  if (is.na(value)) return("Tidak dapat dihitung")
  if (value >= 0.708) return("Ideal")
  if (value >= 0.4) return("Cukup, tinjau AVE/CR")
  "Lemah"
}

one_factor_metrics <- function(sub_df) {
  fit <- tryCatch(
    suppressWarnings(fa(sub_df, nfactors = 1, rotate = "none", fm = "ml")),
    error = function(e) {
      tryCatch(
        suppressWarnings(fa(sub_df, nfactors = 1, rotate = "none", fm = "pa")),
        error = function(e2) NULL
      )
    }
  )

  if (is.null(fit)) {
    return(list(loadings = rep(NA_real_, ncol(sub_df)), ave = NA_real_, cr = NA_real_))
  }

  loads <- abs(as.numeric(fit$loadings[, 1]))
  names(loads) <- colnames(sub_df)
  ave <- mean(loads^2, na.rm = TRUE)
  cr <- (sum(loads, na.rm = TRUE)^2) /
    ((sum(loads, na.rm = TRUE)^2) + sum(1 - loads^2, na.rm = TRUE))

  list(loadings = loads, ave = ave, cr = cr)
}

calc_vif <- function(score_df) {
  vifs <- sapply(names(score_df), function(target) {
    predictors <- setdiff(names(score_df), target)
    formula <- as.formula(paste(target, "~", paste(predictors, collapse = " + ")))
    model <- lm(formula, data = score_df)
    r_squared <- summary(model)$r.squared
    1 / (1 - r_squared)
  })
  data.frame(
    Dimension = names(vifs),
    VIF = as.numeric(vifs),
    Status = vapply(vifs, interpret_vif, character(1)),
    row.names = NULL
  )
}

loc_scores <- as.data.frame(lapply(loc_service_quality, function(items) {
  rowMeans(df_clean[, items], na.rm = TRUE)
}), check.names = FALSE)

other_scores <- as.data.frame(lapply(other_constructs, function(items) {
  rowMeans(df_clean[, items], na.rm = TRUE)
}), check.names = FALSE)

service_quality_score <- rowMeans(loc_scores, na.rm = TRUE)
score_df <- data.frame(
  ServiceQuality = service_quality_score,
  loc_scores,
  other_scores,
  check.names = FALSE
)

cat("============================================================\n")
cat(" ANALISIS HOC SERVICE QUALITY - SCREENING DI R\n")
cat("============================================================\n")
cat("Model HOC       : Service Quality dibentuk dari RS, RL, CR, EM\n")
cat("Data            :", nrow(df_clean), "responden\n")
cat("Catatan penting : Script ini adalah screening statistik di R.\n")
cat("                  Pengujian HOC final tetap dilakukan di SmartPLS\n")
cat("                  melalui measurement model, bootstrapping, dan model struktural.\n\n")

cat("============================================================\n")
cat(" 1. RELIABILITAS LOWER-ORDER CONSTRUCTS (LOC)\n")
cat("============================================================\n")
cat("LOC harus layak terlebih dahulu sebelum dinaikkan menjadi HOC.\n\n")
cat(sprintf("%-18s %5s %7s %7s %7s %s\n", "LOC", "Item", "Alpha", "Std.A", "Mean.r", "Keterangan"))
cat(rep("-", 72), "\n", sep = "")

for (name in names(loc_service_quality)) {
  sub_df <- df_clean[, loc_service_quality[[name]]]
  result <- suppressWarnings(alpha(sub_df, check.keys = TRUE))
  cat(sprintf(
    "%-18s %5d %7.3f %7.3f %7.3f %s\n",
    name,
    length(loc_service_quality[[name]]),
    result$total$raw_alpha,
    result$total$std.alpha,
    result$total$average_r,
    interpret_alpha(result$total$raw_alpha)
  ))
}

cat("\n============================================================\n")
cat(" 2. KORELASI ANTAR DIMENSI SERVICE QUALITY\n")
cat("============================================================\n")
cat("Korelasi terlalu tinggi dapat menandakan kolinearitas; terlalu rendah dapat melemahkan HOC reflective.\n\n")
print(round(cor(loc_scores, use = "pairwise.complete.obs"), 3))

cat("\n============================================================\n")
cat(" 3. UJI KOLINEARITAS UNTUK HOC FORMATIVE\n")
cat("============================================================\n")
cat("Jika Service Quality diperlakukan sebagai formative HOC, cek VIF dimensi.\n")
cat("Kriteria praktis: VIF < 3.3 ideal; VIF < 5 masih sering diterima.\n\n")
print(calc_vif(loc_scores))

cat("\n============================================================\n")
cat(" 4. SCREENING REFLECTIVE HOC BERBASIS SKOR LOC\n")
cat("============================================================\n")
cat("Jika Service Quality diperlakukan sebagai reflective HOC, dimensi RS/RL/CR/EM\n")
cat("seharusnya memiliki loading kuat terhadap faktor umum Service Quality.\n\n")

hoc_metrics <- one_factor_metrics(loc_scores)
hoc_loading_table <- data.frame(
  Dimension = names(hoc_metrics$loadings),
  Loading = round(hoc_metrics$loadings, 3),
  Communality = round(hoc_metrics$loadings^2, 3),
  Status = vapply(hoc_metrics$loadings, interpret_loading, character(1)),
  row.names = NULL
)
print(hoc_loading_table)
cat(sprintf("AVE HOC reflective proxy = %.3f\n", hoc_metrics$ave))
cat(sprintf("CR HOC reflective proxy  = %.3f\n", hoc_metrics$cr))
cat("Catatan: angka ini bukan pengganti outer loading/weight dan bootstrapping di SmartPLS.\n\n")

cat("============================================================\n")
cat(" 5. SERVICE QUALITY COMPOSITE SCORE DAN NOMOLOGICAL CHECK\n")
cat("============================================================\n")
cat("Composite score dihitung sebagai rata-rata skor RS, RL, CR, dan EM.\n")
cat("Korelasi berikut hanya arah awal, bukan hasil uji hipotesis PLS-SEM.\n\n")

nomological <- cor(score_df[, c(
  "ServiceQuality",
  "Cognitive Trust",
  "Affective Trust",
  "Digital Health Service Intention to Adopt"
)], use = "pairwise.complete.obs")
print(round(nomological, 3))

cat("\nRingkasan skor Service Quality:\n")
print(round(describe(score_df$ServiceQuality)[, c("n", "mean", "sd", "min", "max", "skew", "kurtosis")], 3))

cat("\n============================================================\n")
cat(" 6. ARAH INTERPRETASI\n")
cat("============================================================\n")
cat("- Jika HOC formative: prioritaskan VIF dan signifikansi outer weight di SmartPLS.\n")
cat("- Jika HOC reflective: prioritaskan loading dimensi, AVE, CR, dan HTMT/Fornell-Larcker di SmartPLS.\n")
cat("- R script ini membantu screening, tetapi keputusan final HOC tetap dari SmartPLS.\n")
cat("============================================================\n")
