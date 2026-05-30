# ==============================================================================
# ANALISIS PILOT TEST - Kuesioner Chatbot WA Pureva
# Reliability & Validity Test (Cronbach's Alpha + EFA)
# ------------------------------------------------------------------------------
# Penulis  : Akmal
# Tanggal  : 2026-04-27
# Dataset  : Dataset_Simulasi_Pilot_Test.xlsx
# ==============================================================================

# ---- 1. Install & Load Package -----------------------------------------------
library(psych)
library(dplyr)


# ---- 2. Load Data ------------------------------------------------------------
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

df_clean <- read.csv("questioner.csv", header = TRUE, sep = ",")
df_clean <- df_clean %>% mutate(across(everything(), as.numeric))

cat("Dimensi data:", nrow(df_clean), "baris x", ncol(df_clean), "kolom\n\n")


# ---- 3. Definisi Konstruk & Item ---------------------------------------------
constructs <- list(
  Responsiveness                              = c("RS1", "RS2", "RS3", "RS4"),
  Reliability                                 = c("RL1", "RL2", "RL3", "RL4"),
  Credibility                                 = c("CR1", "CR2", "CR3", "CR4"),
  Empathy                                     = c("EM1", "EM2", "EM3", "EM4"),
  "Cognitive Trust"                           = c("CT1", "CT2", "CT3", "CT4"),
  "Affective Trust"                           = c("AT1", "AT2", "AT3", "AT4"),
  "Digital Health Service Intention to Adopt" = c("ITA1", "ITA2", "ITA3", "ITA4", "ITA5")
)


# ---- 4. RELIABILITY TEST (Cronbach's Alpha) ----------------------------------
cat("==========================================================\n")
cat("       RELIABILITY TEST — CRONBACH'S ALPHA\n")
cat("==========================================================\n")
cat(sprintf(
  "%-44s  %6s  %6s  %s\n",
  "Konstruk", "Alpha", "n Item", "Kesimpulan"
))
cat(rep("-", 58), "\n", sep = "")

alpha_results <- list()

for (name in names(constructs)) {
  items <- constructs[[name]]
  sub_df <- df_clean[, items]
  result <- alpha(sub_df, check.keys = TRUE)
  a <- result$total$raw_alpha

  kesimpulan <- ifelse(a >= 0.8, "Sangat Baik",
    ifelse(a >= 0.7, "Baik",
      ifelse(a >= 0.6, "Cukup (perlu revisi)",
        "Lemah (item perlu dievaluasi)"
      )
    )
  )

  cat(sprintf("%-22s  %6.3f  %6d  %s\n", name, a, length(items), kesimpulan))
  alpha_results[[name]] <- result
}

cat("\nCatatan: α ≥ 0.7 = reliabel untuk penelitian akademik\n\n")


# ---- 5. ITEM-TOTAL CORRELATION (per konstruk) --------------------------------
cat("==========================================================\n")
cat("       CORRECTED ITEM-TOTAL CORRELATION\n")
cat("==========================================================\n")
cat("(Nilai < 0.30 → pertimbangkan hapus item tersebut)\n\n")

for (name in names(constructs)) {
  cat("Konstruk:", name, "\n")
  itc <- alpha_results[[name]]$item.stats[, c("r.cor", "r.drop")]
  colnames(itc) <- c("r.corrected", "r.drop")
  print(round(itc, 3))
  cat("\n")
}


# ---- 6. VALIDITY TEST — Exploratory Factor Analysis (EFA) -------------------
cat("==========================================================\n")
cat("       VALIDITY TEST — EXPLORATORY FACTOR ANALYSIS\n")
cat("==========================================================\n")

# KMO & Bartlett Test
cat("--- KMO & Bartlett Test ---\n")
kmo_result <- KMO(df_clean)
cat(
  "KMO Overall:", round(kmo_result$MSA, 3),
  ifelse(kmo_result$MSA >= 0.8, "→ Memuaskan",
    ifelse(kmo_result$MSA >= 0.6, "→ Cukup", "→ Kurang")
  ), "\n"
)

bartlett <- cortest.bartlett(df_clean)
cat(
  "Bartlett's Test: χ² =", round(bartlett$chisq, 2),
  ", df =", bartlett$df,
  ", p =", round(bartlett$p.value, 4),
  ifelse(bartlett$p.value < 0.05, "→ Signifikan ✓\n", "→ Tidak Signifikan ✗\n")
)

# EFA dengan 7 faktor (sesuai jumlah konstruk)
cat("\n--- Factor Loadings (EFA, 7 faktor, rotasi oblimin) ---\n")
efa <- fa(df_clean, nfactors = 7, rotate = "oblimin", fm = "ml")
print(fa.sort(efa)$loadings, cutoff = 0.30, sort = TRUE)

cat("\nVariance Explained:\n")
print(round(efa$Vaccounted, 3))


# ---- 7. AVERAGE VARIANCE EXTRACTED (AVE) ------------------------------------
cat("\n==========================================================\n")
cat("       CONVERGENT VALIDITY — AVE per Konstruk\n")
cat("==========================================================\n")
cat("(AVE ≥ 0.5 = convergent validity terpenuhi)\n\n")
cat(sprintf("%-22s  %6s  %s\n", "Konstruk", "AVE", "Status"))
cat(rep("-", 45), "\n", sep = "")

for (name in names(constructs)) {
  items <- constructs[[name]]
  sub_df <- df_clean[, items]

  # Hitung factor loadings (PCA 1 faktor)
  fa_1 <- fa(sub_df, nfactors = 1, rotate = "none", fm = "ml")
  loads <- fa_1$loadings[, 1]
  ave <- mean(loads^2)

  status <- ifelse(ave >= 0.5, "✅ Terpenuhi", "⚠️  Perlu Evaluasi")
  cat(sprintf("%-22s  %6.3f  %s\n", name, ave, status))
}


# ---- 8. RINGKASAN AKHIR ------------------------------------------------------
cat("\n==========================================================\n")
cat("       RINGKASAN PILOT TEST\n")
cat("==========================================================\n")
cat("Jumlah responden   :", nrow(df_clean), "\n")
cat("Jumlah item valid  : 29 item (2 attention check dikeluarkan)\n")
cat("Jumlah konstruk    : 7\n")
cat("\nTarget untuk data nyata:\n")
cat("  • Cronbach's Alpha  ≥ 0.70 per konstruk\n")
cat("  • Item-Total Corr.  ≥ 0.30 per item\n")
cat("  • KMO               ≥ 0.60\n")
cat("  • Bartlett p-value  < 0.05\n")
cat("  • AVE               ≥ 0.50 per konstruk\n")
cat("==========================================================\n")
