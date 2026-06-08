library(dplyr)

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

df <- read.csv("questioner.csv", header = TRUE, sep = ",")
df <- df %>% mutate(across(everything(), as.numeric))

constructs <- list(
  "Responsiveness (RS)" = c("RS1", "RS2", "RS3", "RS4"),
  "Reliability (RL)" = c("RL1", "RL2", "RL3", "RL4"),
  "Credibility (CR)" = c("CR1", "CR2", "CR3", "CR4"),
  "Empathy (EM)" = c("EM1", "EM2", "EM3", "EM4"),
  "Cognitive Trust (CT)" = c("CT1", "CT2", "CT3", "CT4"),
  "Affective Trust (AT)" = c("AT1", "AT2", "AT3", "AT4"),
  "Intention to Adopt (ITA)" = c("ITA1", "ITA2", "ITA3", "ITA4", "ITA5")
)

kategori <- function(m) {
  if (is.na(m)) {
    return("-")
  }
  if (m >= 5.81) {
    return("Sangat Tinggi")
  }
  if (m >= 4.61) {
    return("Tinggi")
  }
  if (m >= 3.41) {
    return("Sedang")
  }
  if (m >= 2.21) {
    return("Rendah")
  }
  "Sangat Rendah"
}

item_rows <- list()
konstruk_rows <- list()

for (name in names(constructs)) {
  items <- constructs[[name]]
  sub <- df[, items]

  # --- Deskriptif per item ---
  for (it in items) {
    m <- mean(df[[it]], na.rm = TRUE)
    sd <- sd(df[[it]], na.rm = TRUE)
    item_rows[[length(item_rows) + 1]] <- data.frame(
      Variabel = ifelse(it == items[1], name, ""),
      Indikator = it,
      Mean = round(m, 3),
      Std_Dev = round(sd, 3),
      stringsAsFactors = FALSE
    )
  }

  # --- Deskriptif per konstruk ---
  skor_konstruk <- rowMeans(sub, na.rm = TRUE)
  m_k <- mean(skor_konstruk, na.rm = TRUE)
  sd_k <- sd(skor_konstruk, na.rm = TRUE)
  konstruk_rows[[length(konstruk_rows) + 1]] <- data.frame(
    Konstruk = name,
    N_Item = length(items),
    Total_Mean = round(m_k, 3),
    Std_Dev = round(sd_k, 3),
    Kategori = kategori(m_k),
    stringsAsFactors = FALSE
  )
}

tabel_item <- do.call(rbind, item_rows)
tabel_konstruk <- do.call(rbind, konstruk_rows)

cat("\n========== ANALISIS DESKRIPTIF MAIN TEST ==========\n")
cat("N responden:", nrow(df), "| Skala: 1-7\n\n")

cat("--- DESKRIPTIF PER INDIKATOR ---\n")
print(tabel_item, row.names = FALSE, right = FALSE)

cat("\n--- DESKRIPTIF PER KONSTRUK (TOTAL MEAN) ---\n")
print(tabel_konstruk, row.names = FALSE, right = FALSE)
