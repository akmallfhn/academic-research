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

constructs <- list(
  Responsiveness = c("RS1", "RS2", "RS3", "RS4"),
  Reliability = c("RL1", "RL2", "RL3", "RL4"),
  Credibility = c("CR1", "CR2", "CR3", "CR4"),
  Empathy = c("EM1", "EM2", "EM3", "EM4"),
  "Cognitive Trust" = c("CT1", "CT2", "CT3", "CT4"),
  "Affective Trust" = c("AT1", "AT2", "AT3", "AT4"),
  "Digital Health Service Intention to Adopt" = c("ITA1", "ITA2", "ITA3", "ITA4", "ITA5")
)

missing_items <- setdiff(unlist(constructs), names(df_clean))
if (length(missing_items) > 0) {
  stop("Item berikut tidak ditemukan di questioner.csv: ", paste(missing_items, collapse = ", "))
}

interpret_alpha <- function(value) {
  if (is.na(value)) {
    return("Tidak dapat dihitung")
  }
  if (value >= 0.8) {
    return("Sangat baik")
  }
  if (value >= 0.7) {
    return("Baik")
  }
  if (value >= 0.6) {
    return("Cukup, perlu evaluasi")
  }
  "Lemah, item perlu dievaluasi"
}

interpret_item_total <- function(value) {
  if (is.na(value)) {
    return("Tidak dapat dihitung")
  }
  if (value >= 0.5) {
    return("Kuat")
  }
  if (value >= 0.3) {
    return("Memadai")
  }
  "Lemah"
}

interpret_kmo <- function(value) {
  if (is.na(value)) {
    return("Tidak dapat dihitung")
  }
  if (value >= 0.8) {
    return("Memuaskan")
  }
  if (value >= 0.6) {
    return("Cukup")
  }
  "Kurang"
}

interpret_ave <- function(value) {
  if (is.na(value)) {
    return("Tidak dapat dihitung")
  }
  if (value >= 0.5) {
    return("Terpenuhi")
  }
  "Perlu evaluasi"
}

interpret_cr <- function(value) {
  if (is.na(value)) {
    return("Tidak dapat dihitung")
  }
  if (value >= 0.7) {
    return("Terpenuhi")
  }
  "Perlu evaluasi"
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

  loads <- as.numeric(fit$loadings[, 1])
  names(loads) <- colnames(sub_df)
  loads <- abs(loads)
  ave <- mean(loads^2, na.rm = TRUE)
  cr <- (sum(loads, na.rm = TRUE)^2) /
    ((sum(loads, na.rm = TRUE)^2) + sum(1 - loads^2, na.rm = TRUE))

  list(loadings = loads, ave = ave, cr = cr)
}

construct_scores <- as.data.frame(lapply(constructs, function(items) {
  rowMeans(df_clean[, items], na.rm = TRUE)
}), check.names = FALSE)

cat("============================================================\n")
cat(" ANALISIS MAIN TEST - FOKUS RELIABILITAS DAN VALIDITAS LOC\n")
cat("============================================================\n")
cat("Data              :", nrow(df_clean), "responden x", ncol(df_clean), "item\n")
cat("Jumlah konstruk   :", length(constructs), "\n")

cat("============================================================\n")
cat(" 1. PETA KONSTRUK DAN ITEM\n")
cat("============================================================\n")
for (name in names(constructs)) {
  cat(sprintf("%-44s : %s\n", name, paste(constructs[[name]], collapse = ", ")))
}

cat("\n============================================================\n")
cat(" 2. RINGKASAN RELIABILITAS PER KONSTRUK\n")
cat("============================================================\n")
cat("Kriteria: Cronbach alpha >= 0.70; mean inter-item correlation ideal 0.15-0.50.\n\n")
cat(sprintf(
  "%-44s %5s %7s %7s %7s %s\n",
  "Konstruk", "Item", "Alpha", "Std.A", "Mean.r", "Keterangan"
))
cat(rep("-", 92), "\n", sep = "")

alpha_results <- list()
summary_rows <- list()

for (name in names(constructs)) {
  items <- constructs[[name]]
  sub_df <- df_clean[, items]
  result <- suppressWarnings(alpha(sub_df, check.keys = TRUE))
  alpha_results[[name]] <- result

  raw_alpha <- result$total$raw_alpha
  std_alpha <- result$total$std.alpha
  mean_r <- result$total$average_r
  status <- interpret_alpha(raw_alpha)

  cat(sprintf(
    "%-44s %5d %7.3f %7.3f %7.3f %s\n",
    name, length(items), raw_alpha, std_alpha, mean_r, status
  ))

  summary_rows[[name]] <- data.frame(
    Konstruk = name,
    Item = length(items),
    Alpha = raw_alpha,
    StdAlpha = std_alpha,
    MeanInterItem = mean_r,
    AlphaStatus = status
  )
}

cat("\nInterpretasi:\n")
cat("- Alpha melihat konsistensi internal item dalam satu konstruk.\n")
cat("- Mean inter-item terlalu rendah menandakan item tidak bergerak searah.\n")
cat("- Mean inter-item terlalu tinggi dapat menandakan item terlalu repetitif.\n\n")

cat("============================================================\n")
cat(" 3. CORRECTED ITEM-TOTAL CORRELATION PER KONSTRUK\n")
cat("============================================================\n")
cat("Kriteria: r.drop >= 0.30. Item < 0.30 perlu ditinjau dari redaksi dan arah jawaban.\n\n")

weak_items <- data.frame(
  Konstruk = character(),
  Item = character(),
  RDrop = numeric(),
  Status = character(),
  stringsAsFactors = FALSE
)

for (name in names(constructs)) {
  cat("Konstruk:", name, "\n")
  item_stats <- alpha_results[[name]]$item.stats
  itc <- data.frame(
    Item = rownames(item_stats),
    CorrectedR = round(item_stats$r.cor, 3),
    RDrop = round(item_stats$r.drop, 3),
    Status = vapply(item_stats$r.drop, interpret_item_total, character(1)),
    row.names = NULL
  )
  print(itc)

  flagged <- itc[itc$RDrop < 0.30 | is.na(itc$RDrop), ]
  if (nrow(flagged) > 0) {
    weak_items <- rbind(
      weak_items,
      data.frame(
        Konstruk = name,
        Item = flagged$Item,
        RDrop = flagged$RDrop,
        Status = flagged$Status,
        stringsAsFactors = FALSE
      )
    )
  }
  cat("\n")
}

cat("============================================================\n")
cat(" 4. VALIDITAS KONVERGEN PER KONSTRUK: LOADING, AVE, CR\n")
cat("============================================================\n")
cat("Kriteria: loading ideal >= 0.708; AVE >= 0.50; composite reliability >= 0.70.\n")
cat("Catatan: CR dan AVE di sini adalah estimasi berbasis model 1 faktor di R.\n\n")

validity_rows <- list()

for (name in names(constructs)) {
  items <- constructs[[name]]
  sub_df <- df_clean[, items]
  metrics <- one_factor_metrics(sub_df)

  cat("Konstruk:", name, "\n")
  loading_table <- data.frame(
    Item = names(metrics$loadings),
    Loading = round(metrics$loadings, 3),
    Communality = round(metrics$loadings^2, 3),
    Status = ifelse(metrics$loadings >= 0.708, "Ideal", "Perlu evaluasi"),
    row.names = NULL
  )
  print(loading_table)
  cat(sprintf(
    "AVE = %.3f (%s); CR = %.3f (%s)\n\n",
    metrics$ave,
    interpret_ave(metrics$ave),
    metrics$cr,
    interpret_cr(metrics$cr)
  ))

  validity_rows[[name]] <- data.frame(
    Konstruk = name,
    AVE = metrics$ave,
    CR = metrics$cr,
    AVEStatus = interpret_ave(metrics$ave),
    CRStatus = interpret_cr(metrics$cr)
  )
}

cat("============================================================\n")
cat(" 5. FACTORABILITY PER KONSTRUK: KMO DAN BARTLETT\n")
cat("============================================================\n")
cat("Kriteria: KMO >= 0.60; Bartlett p-value < 0.05.\n\n")
cat(sprintf("%-44s %7s %12s %10s %s\n", "Konstruk", "KMO", "ChiSquare", "PValue", "Keterangan"))
cat(rep("-", 94), "\n", sep = "")

for (name in names(constructs)) {
  sub_df <- df_clean[, constructs[[name]]]
  kmo <- tryCatch(suppressWarnings(KMO(sub_df)$MSA), error = function(e) NA_real_)
  bartlett <- tryCatch(
    suppressWarnings(cortest.bartlett(cor(sub_df), n = nrow(sub_df))),
    error = function(e) list(chisq = NA_real_, p.value = NA_real_)
  )
  bartlett_status <- ifelse(!is.na(bartlett$p.value) && bartlett$p.value < 0.05, "Signifikan", "Perlu evaluasi")

  cat(sprintf(
    "%-44s %7.3f %12.2f %10.4f %s; %s\n",
    name,
    kmo,
    bartlett$chisq,
    bartlett$p.value,
    interpret_kmo(kmo),
    bartlett_status
  ))
}

cat("\n============================================================\n")
cat(" 6. KORELASI ANTAR KONSTRUK DAN FORNELL-LARCKER AWAL\n")
cat("============================================================\n")
cat("Diagonal = sqrt(AVE). Nilai diagonal sebaiknya lebih besar dari korelasi satu baris/kolom.\n")
cat("Catatan: ini screening awal; keputusan akhir measurement model tetap di SmartPLS.\n\n")

validity_summary <- bind_rows(validity_rows)
score_cor <- cor(construct_scores, use = "pairwise.complete.obs")
fornell <- score_cor
diag(fornell) <- sqrt(validity_summary$AVE[match(colnames(fornell), validity_summary$Konstruk)])
print(round(fornell, 3))

cat("\n============================================================\n")
cat(" 7. RINGKASAN TEMUAN\n")
cat("============================================================\n")

summary_df <- bind_rows(summary_rows) %>%
  left_join(validity_summary, by = "Konstruk")

print(transform(
  summary_df,
  Alpha = round(Alpha, 3),
  StdAlpha = round(StdAlpha, 3),
  MeanInterItem = round(MeanInterItem, 3),
  AVE = round(AVE, 3),
  CR = round(CR, 3)
))

cat("\nItem dengan r.drop < 0.30:\n")
if (nrow(weak_items) == 0) {
  cat("Tidak ada item yang berada di bawah ambang 0.30.\n")
} else {
  print(weak_items)
}

cat("\nKesimpulan penggunaan script:\n")
cat("- Script ini fokus pada konstruk utama/LOC: RS, RL, CR, EM, CT, AT, dan ITA.\n")
cat("- Gunakan output ini sebagai pemeriksaan kualitas instrumen sebelum PLS-SEM.\n")
cat("- Untuk model HOC Service Quality, jalankan analisis_hoc_service_quality.r.\n")
cat("============================================================\n")
