library(psych)

if (requireNamespace("rstudioapi", quietly = TRUE) &&
  rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
}

# =====================================================================
# ANALISIS PRE-TEST (Validitas & Reliabilitas) - pre-test-questioner.csv
# Menghasilkan tabel gabungan per konstruk:
#   Variabel | Indikator | KMO | Cronbach's Alpha | Sig Bartlett |
#   Factor Loading | Keterangan
# =====================================================================

df <- read.csv("pre-test-questioner.csv", header = TRUE, sep = ",")

constructs <- list(
  "Responsiveness (RS)"                          = c("RS1", "RS2", "RS3", "RS4"),
  "Reliability (RL)"                             = c("RL1", "RL2", "RL3", "RL4"),
  "Credibility (CR)"                             = c("CR1", "CR2", "CR3", "CR4"),
  "Empathy (EM)"                                 = c("EM1", "EM2", "EM3", "EM4"),
  "Cognitive Trust (CT)"                         = c("CT1", "CT2", "CT3", "CT4"),
  "Affective Trust (AT)"                         = c("AT1", "AT2", "AT3", "AT4"),
  "Intention to Adopt (ITA)"                     = c("ITA1", "ITA2", "ITA3", "ITA4", "ITA5")
)

rows <- list()

for (name in names(constructs)) {
  items <- constructs[[name]]
  sub <- df[, items]
  R <- cor(sub)

  # --- KMO & Anti-image (MSA per item = diagonal anti-image correlation) ---
  kmo <- KMO(R)

  # --- Bartlett's Test of Sphericity ---
  bart <- cortest.bartlett(R, n = nrow(sub))

  # --- Cronbach's Alpha (per konstruk) ---
  a <- psych::alpha(sub, check.keys = TRUE)$total$raw_alpha

  # --- Factor loading (komponen utama pertama, 1 faktor) ---
  fa <- principal(sub, nfactors = 1, rotate = "none")
  loadings <- as.numeric(fa$loadings[, 1])

  for (i in seq_along(items)) {
    loading <- loadings[i]
    # Kriteria validitas: Factor loading > 0.5
    valid <- ifelse(abs(loading) > 0.5, "Valid", "TIDAK Valid")
    rows[[length(rows) + 1]] <- data.frame(
      Variabel = ifelse(i == 1, name, ""),
      Indikator = items[i],
      KMO = ifelse(i == 1, sprintf("%.3f", kmo$MSA), ""),
      Alpha = ifelse(i == 1, sprintf("%.3f", a), ""),
      Sig_Bart = ifelse(i == 1, sprintf("%.3f", bart$p.value), ""),
      Loading = sprintf("%.3f", loading),
      Keterangan = valid,
      stringsAsFactors = FALSE
    )
  }
}

tabel <- do.call(rbind, rows)

cat("\n================ HASIL ANALISIS PRE-TEST ================\n")
cat("N responden:", nrow(df), "\n\n")
print(tabel, row.names = FALSE, right = FALSE)

write.csv(tabel, "pre-test-validity-reliability.csv", row.names = FALSE)
cat("\nDisimpan ke: pre-test-validity-reliability.csv\n")
