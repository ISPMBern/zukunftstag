# Analyze data from social contact survey
# Christian L. Althaus, 10 November 2023

# Load libraries
library(here)

# Define colors
colfunc <- colorRampPalette(c("steelblue", "white"))

# Read data
df <- read.csv(here("data/raw/zukunftstag.csv"))
df <- cbind(df, total = rowSums(df[, 3:6]))
head(df)

# Define age groups
age_groups <- c("0-14", "15-29", "30-64", "65+")
n_age <- length(age_groups)

# Summary statistics
dim(df)[1] # Number of participants
mean(df$total) # Average number of contacts
mean(df$total[df$age == 1]) # Average number of contacts in age group 1
mean(df$total[df$age == 2]) # Average number of contacts in age group 2
mean(df$total[df$age == 3]) # Average number of contacts in age group 3
mean(df$total[df$age == 4]) # Average number of contacts in age group 4

# Create social contact matrix
m <- matrix(NA, nrow = n_age, ncol = n_age)
colnames(m) <- age_groups
rownames(m) <- age_groups
for(i in 1:n_age) {
  for(j in 1:n_age) {
    m[i, j] <- mean(df[df$age == i, 2 + j])
  }
}
m

# Add social contact data for oldest age group (based on Prem et al.)
m[4, ] <- c(0.74, 0.81, 3.58, 1.83)
m
max(eigen(m)$values)

# Plot social contact matrix
par(mfrow = c(1, 2))
barplot(rowSums(m),
        col = colfunc(3)[2],
        xlab = "Altersgruppe Teilnehmer (Jahre)",
        ylab = "Anzahl Kontakte (pro Tag)",
        main = "Soziale Kontakte pro Altersgruppe")
image(1:n_age, 1:n_age, m,
      col = rev(colfunc(1e2)),
      xlab = "Altersgruppe Teilnehmer (Jahre)",
      ylab = "Altersgruppe Kontakt (Jahre)",
      axes = FALSE, main = "Soziale Kontaktmatrize", frame = TRUE)
axis(1, 1:n_age, age_groups)
axis(2, 1:n_age, age_groups)
for(i in 1:n_age) for (j in 1:n_age) text(i, j, round(m[i, j], 1))

# Function to create reciprocal contact matrix
reciprocal <- function(m, N) {
  m_rec <- m
  for(i in 1:length(N)) {
    for(j in 1:length(N)) {
      m_rec[i, j] <- (m[i, j]*N[i] + m[j, i]*N[j])/(2*N[i])
    }
  }
  return(m_rec)
}

# Swiss demographic data
# Source: https://www.bfs.admin.ch/bfs/de/home/statistiken/bevoelkerung/stand-entwicklung/bevoelkerung.assetdetail.23064766.html
pop_2022 <- readxl::read_excel(here("data/raw/su-d-01.02.03.06.xlsx"), sheet = "2022")
pop_2022 <- data.frame(age = 0:100, n = as.numeric(pop_2022[2, 3:103]))
age_sent <- c(0, 15, 30, 65) # Age groups for Sentinella (adapted)
pop <- numeric(length(age_sent))
age_range <- c(age_sent, 200)
for(i in 1:length(pop)) {
  pop[i] <- sum(pop_2022$n[pop_2022$age >= age_range[i] & pop_2022$age < age_range[i + 1]])
}
pop_2022_sent <- data.frame(lower.age.limit = age_sent, population = pop)
pop_2022_sent

# Final social contact matrix normalized to Swiss population
m <- reciprocal(m, pop_2022_sent$population)
m
max(eigen(m)$values)
png(here("output/figures/contact_matrix.png"), width = 800, height = 450)
par(mfrow = c(1, 2))
barplot(rowSums(m),
        col = colfunc(3)[2],
        xlab = "Altersgruppe Teilnehmer (Jahre)",
        ylab = "Anzahl Kontakte (pro Tag)",
        main = "Soziale Kontakte pro Altersgruppe")
image(1:n_age, 1:n_age, m,
      col = rev(colfunc(1e2)),
      xlab = "Altersgruppe Teilnehmer (Jahre)",
      ylab = "Altersgruppe Kontakt (Jahre)",
      axes = FALSE, main = "Soziale Kontaktmatrize", frame = TRUE)
axis(1, 1:n_age, age_groups, tick = FALSE)
axis(2, 1:n_age, age_groups, tick = FALSE)
for(i in 1:n_age) for (j in 1:n_age) text(i, j, round(m[i, j], 1))
dev.off()

# Write contact matrix to file
write.csv(m, here("data/processed/contact_matrix.csv"))
