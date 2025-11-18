install.packages("psych")
install.packages("tidyverse")

library(psych)
library(tidyverse)

df <- tribble(
  ~Q1, ~Q2, ~Q3, ~Q4, ~Q5, ~Q6, ~Q7, ~Q8, ~Q9, ~Q10, ~Q11, ~Q12, ~Q13, ~Q14, ~Q15, ~Q16, ~Q17, ~Q18, ~Q19, ~Q20, ~Q21, ~Q22, ~Q23, # nolint: line_length_linter.
  6, 2, 3, 5, 6, 7, 7, 6, 6, 7, 5, 6, 6, 6, 6, 6, 7, 7, 7, 5, 5, 5, 6,
  6, 3, 2, 5, 7, 7, 7, 7, 7, 6, 5, 6, 3, 5, 5, 5, 5, 5, 3, 3, 7, 7, 3,
  7, 1, 1, 7, 7, 7, 7, 7, 7, 2, 2, 2, 2, 4, 7, 7, 5, 6, 6, 4, 7, 7, 7,
  4, 1, 5, 6, 7, 6, 7, 6, 7, 3, 6, 1, 4, 6, 6, 6, 7, 7, 5, 6, 6, 6, 5,
  5, 2, 2, 3, 7, 6, 7, 7, 7, 5, 5, 5, 5, 6, 6, 6, 6, 7, 6, 5, 6, 6, 5,
  5, 4, 4, 5, 5, 6, 7, 5, 5, 5, 4, 3, 7, 5, 5, 5, 5, 6, 4, 6, 6, 5, 6,
  7, 1, 1, 1, 4, 7, 7, 4, 7, 7, 7, 7, 1, 7, 5, 7, 7, 7, 7, 6, 6, 5, 3,
  7, 4, 4, 7, 7, 7, 7, 7, 7, 7, 6, 4, 4, 6, 7, 7, 7, 7, 7, 5, 7, 5, 4,
  3, 1, 1, 7, 7, 6, 7, 7, 7, 6, 3, 6, 6, 5, 7, 7, 7, 7, 5, 6, 7, 7, 7,
  5, 1, 1, 6, 6, 7, 6, 6, 6, 7, 6, 4, 4, 6, 6, 6, 6, 6, 6, 6, 5, 6, 5,
  6, 5, 3, 7, 6, 6, 7, 6, 7, 7, 5, 4, 2, 5, 6, 6, 6, 7, 6, 6, 6, 6, 7,
  2, 1, 1, 7, 7, 7, 7, 7, 7, 4, 1, 4, 5, 7, 7, 7, 7, 7, 7, 5, 6, 5, 6,
  5, 2, 5, 4, 6, 6, 6, 6, 6, 6, 6, 5, 4, 5, 6, 6, 6, 6, 6, 6, 5, 5, 5,
  5, 3, 6, 7, 7, 7, 7, 7, 7, 7, 7, 7, 5, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
  6, 4, 5, 7, 7, 7, 7, 7, 7, 6, 6, 5, 5, 6, 6, 6, 6, 6, 5, 5, 6, 6, 4,
  7, 5, 7, 7, 7, 6, 7, 7, 7, 6, 6, 5, 5, 7, 5, 6, 7, 7, 7, 6, 7, 7, 7,
  7, 1, 1, 7, 7, 7, 7, 7, 7, 7, 5, 6, 7, 6, 7, 7, 7, 7, 5, 6, 7, 7, 6,
  7, 1, 1, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
  7, 1, 2, 5, 7, 6, 7, 7, 6, 7, 5, 7, 7, 7, 7, 6, 7, 5, 7, 7, 7, 7, 7,
  4, 3, 4, 7, 7, 7, 7, 7, 7, 7, 6, 3, 3, 6, 6, 6, 5, 7, 5, 5, 7, 7, 5,
  4, 2, 3, 5, 5, 6, 6, 6, 6, 6, 4, 6, 6, 6, 6, 5, 5, 5, 5, 4, 5, 5, 5,
  6, 2, 2, 5, 6, 6, 7, 7, 7, 5, 2, 2, 2, 4, 5, 5, 6, 6, 5, 5, 6, 7, 6,
  7, 6, 4, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
  4, 1, 4, 6, 7, 7, 6, 6, 6, 5, 6, 7, 3, 5, 5, 5, 6, 6, 4, 5, 7, 6, 5,
  4, 2, 4, 5, 5, 5, 6, 5, 6, 7, 4, 3, 4, 5, 5, 5, 5, 6, 6, 5, 6, 6, 6,
  5, 3, 3, 4, 6, 5, 7, 7, 7, 3, 2, 5, 4, 4, 5, 5, 5, 5, 5, 4, 5, 5, 5,
  5, 3, 1, 4, 7, 7, 7, 7, 7, 7, 6, 5, 4, 6, 5, 5, 5, 5, 5, 6, 6, 6, 6,
  5, 1, 2, 4, 6, 3, 5, 6, 5, 3, 2, 3, 2, 6, 7, 6, 6, 6, 6, 4, 5, 6, 5,
  6, 1, 1, 6, 6, 3, 6, 6, 6, 4, 2, 1, 3, 4, 5, 6, 7, 7, 7, 7, 4, 5, 5,
  2, 1, 2, 2, 5, 5, 6, 5, 5, 7, 2, 2, 6, 6, 7, 7, 7, 7, 6, 6, 4, 6, 4
)

# Jumlah responden
n <- nrow(df)

# Menghitung r tabel (α = 0.05, dua sisi)
# r_table nilai kritis dari korelasi Pearson yang dipakai untuk menentukan
# apakah hubungan antara dua variabel signifikan secara statistik.
alpha <- 0.05
df_r <- n - 2
t_crit <- qt(1 - alpha / 2, df_r)
r_table <- sqrt(t_crit^2 / (t_crit^2 + df_r))

# Menghitung Cronbach's Alpha
alpha_full <- psych::alpha(df)

# Mengambil CITC (Corrected Item-Total Correlation) untuk setiap item
citc <- alpha_full$item.stats$r.drop

# Menentukan status validitas
# r > 0.30  → Valid
valid_flag <- ifelse(citc >= r_table, "Valid", "Tidak Valid")

# Membuat mapping table validitas
validity_table <- tibble(
  Item = paste0("Q", 1:23),
  CITC = round(citc, 3),
  R_Table = r_table,
  Status = valid_flag
)
print("=== HASIL UJI VALIDITAS ===")
print(validity_table, n = Inf)

print("=== HASIL UJI RELIABILITAS ===")
print(paste("Cronbach's Alpha:", alpha_full$total$raw_alpha))