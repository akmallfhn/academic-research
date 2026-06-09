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

analysis_name <- "KMO and Bartlett's Test"
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

interpret_kmo <- function(value) {
    if (is.na(value)) {
        return("Tidak dapat dihitung")
    }
    if (value >= 0.80) {
        return("Baik")
    }
    if (value >= 0.60) {
        return("Cukup")
    }
    if (value >= 0.50) {
        return("Minimum")
    }
    "Tidak layak"
}

interpret_bartlett <- function(p_value) {
    if (is.na(p_value)) {
        return("Tidak dapat dihitung")
    }
    if (p_value < 0.05) {
        return("Signifikan")
    }
    "Tidak signifikan"
}

calc_kmo_bartlett <- function(table_name, construct_name, sub_df) {
    cor_matrix <- cor(sub_df, use = "pairwise.complete.obs")
    kmo_result <- KMO(cor_matrix)
    bartlett_result <- cortest.bartlett(cor_matrix, n = nrow(sub_df))

    data.frame(
        Table = table_name,
        Construct = construct_name,
        ItemCount = ncol(sub_df),
        KMO = round(kmo_result$MSA, 3),
        KMOStatus = interpret_kmo(kmo_result$MSA),
        BartlettChiSquare = round(bartlett_result$chisq, 3),
        BartlettDf = bartlett_result$df,
        "Bartlett's Test (Sig.)" = sprintf("%.2f", bartlett_result$p.value),
        BartlettStatus = interpret_bartlett(bartlett_result$p.value),
        check.names = FALSE,
        row.names = NULL
    )
}

all_items <- unique(unlist(constructs))
overall_result <- calc_kmo_bartlett("Overall", "Keseluruhan", df[, all_items])

result_rows <- list()

for (name in names(constructs)) {
    items <- constructs[[name]]
    result_rows[[name]] <- calc_kmo_bartlett("Per Konstruk", name, df[, items])
}

result <- do.call(rbind, result_rows)
csv_result <- rbind(overall_result, result)
rownames(result) <- NULL
rownames(csv_result) <- NULL

cat("=== KMO AND BARTLETT'S TEST ===\n")
cat("(KMO >= 0.50 minimum; Bartlett's Test (Sig.) < 0.05 signifikan)\n")
cat("\n--- Metadata ---\n")
cat("Analysis:", analysis_name, "\n")
cat("Run at  :", run_at, "\n")
cat("Owner   :", owner, "\n")

cat("\n--- Overall Table ---\n")
print(overall_result)

cat("\n--- Result Table: Per Konstruk ---\n")
print(result)

write.csv(csv_result, "kmo_bartletts_test.csv", row.names = FALSE)
