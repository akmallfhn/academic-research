library(psych)

df <- read.csv("questioner.csv", header = TRUE, sep = ",")

# Menghitung Cronbach's Alpha
alpha_full <- psych::alpha(df)

print(paste("Cronbach's Alpha:", alpha_full$total$raw_alpha))
print(paste("N of Items:", ncol(df)))
