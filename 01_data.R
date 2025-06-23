## ---------------------------
##
## Title: Data Prep for NetLogo
##
## Author: Eva- Maria Vogel
##
## Date Created: 2025-05
##
## ---------------------------

library(dplyr)
library(cluster)

# Load and prep data -----------------------------------------------------------
de_path <- file.path("..", "PSYDISC", "data", "processed", "DEdat.csv")
dat <- as.data.frame(read.csv(de_path))

# Recode vaccin variable: 
# 1 or 2 -> 1 (vaccinated), 
# 3 -> 0 (not vaccinated)
dat$vaccin_bin <- ifelse(dat$vaccin %in% c(1, 2), 1,
                         ifelse(dat$vaccin == 3, 0, NA))

# Subset
dat <- dat %>%
  select(
    soc,
    lit = literacy, 
    cons = CP, LR, 
    vote = vote.recall_foreign,
    age = age_cont,
    sex = sex_rec,
    edu = formal_edu,
    rel = religiosity,
    spirit,
    vaccin = vaccin_bin,
    FIS
  ) %>%
  na.omit()

# Normalize vars
normalize <- function(x) {
  return((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}

vars_to_norm <- c("age", "edu", "lit", "cons", "LR", "rel", "spirit", "soc", "FIS")
dat[vars_to_norm] <- lapply(dat[vars_to_norm], normalize)

# Set classes for gower distance
dat$age <- as.numeric(dat$age)
dat$edu <- as.numeric(dat$edu)
dat$lit <- as.numeric(dat$lit)
dat$vaccin <- as.numeric(dat$vaccin)

dat$FIS <- factor(dat$FIS, ordered = TRUE)
dat$soc <- factor(dat$soc, ordered = TRUE)
dat$LR <- factor(dat$LR, ordered = TRUE)
dat$cons <- factor(dat$cons, ordered = TRUE)
dat$rel <- factor(dat$rel, ordered = TRUE)
dat$spirit <- factor(dat$spirit, ordered = TRUE)

dat$vote <- factor(dat$vote)
dat$sex <- factor(dat$sex)

# Calculate distance between agents --------------------------------------------

# Subset demographic variables for distance
dist_dat <- dat %>% 
  select(age, sex, cons, LR, edu, vote)

# Calculate Gower distance
gower_dist <- daisy(dist_dat, metric = "gower")

# Do MDS to place agents in 2D space
mds_coords <- cmdscale(gower_dist, k = 2)

# Combine back into your original data
dat_with_coords <- dat %>%
  mutate(xcor = scale(mds_coords[, 1]),  # scaled for NetLogo
         ycor = scale(mds_coords[, 2]))

# Export to CSV
write.csv(dat_with_coords, "netlogo_input_with_coords.csv", row.names = FALSE)


# Calculate Logistic Regression Coefficients for Model -------------------------

#Set classes for regression (treat continously (dummy variables))
dat$vaccin <- as.numeric(dat$vaccin)
dat$LR <- as.numeric(dat$LR)
dat$rel <- as.numeric(dat$rel)
dat$spirit <- as.numeric(dat$spirit)
dat$FIS <- as.numeric(dat$FIS)

# Include variables shown to be significant in network
model <- glm(vaccin ~ LR + rel + spirit + FIS, data = dat, family = "binomial")

summary(model)
model$coefficients
