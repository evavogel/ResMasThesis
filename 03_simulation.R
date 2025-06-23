## ---------------------------
##
## Title: Simulation Experiment Analysis
##
## Author: Eva- Maria Vogel
##
## Date Created: 2025-05
##
## ---------------------------

library(dplyr)
library(ggplot2)

# Load and prep data -----------------------------------------------------------

de_path <- file.path("..", "Simulation", "Thesis_Simulation_Results")
dat <- read.csv(de_path, skip = 6, header = TRUE, stringsAsFactors = FALSE, quote = "")

# Get rid of "" in data (Behaviour Space)
dat <- as.data.frame(lapply(dat, function(x) gsub("\"", "", x)))

# Rename and organize
dat <- dat %>%
  rename(
    Run = X..run.number..,
    Step = X..step..,
    Gamma = X.gamma.,
    SocDistIntervention = X.soc.intervention.,
    VaccRate = X.vaccine.rate.,
    InitialVaccRate = X.initial.vacc.rate.
  ) %>%
  select(Run, Step, Gamma, SocDistIntervention, InitialVaccRate, VaccRate) %>%
  mutate(across(everything(), ~as.numeric(.)))

# Recode factors
dat$SocDistIntervention <- factor(dat$SocDistIntervention,
                                  levels = c(-1, 0, 1),
                                  labels = c("Positive", "Baseline", "Negative")
)

dat$Gamma <- factor(dat$Gamma, levels = c(0, 10), labels = c("No Social Influence", "With  Social Influence"))

# Group and summarise
dat_summary <- dat %>%
  group_by(Step, Gamma, InitialVaccRate, SocDistIntervention) %>%
  summarise(mean_VaccRate = mean(VaccRate), .groups = "drop")


# No Social Influence Plot -----------------------------------------------------

# Filter to just "No Social Influence"
no_social_influence <- dat_summary %>%
  filter(Gamma == "No Social Influence")

# Create the facet plot (one row, different initial vaccination rates)
p_no_influence <- ggplot(no_social_influence, aes(x = Step, y = mean_VaccRate, color = SocDistIntervention)) +
  geom_line(size = 1) +
  facet_wrap(~ InitialVaccRate, nrow = 1, labeller = label_both) +
  scale_y_continuous(limits = c(0, 1)) +
  theme_minimal() +
  labs(
    title = "Vaccination Over Time (No Social Influence)",
    x = "Steps",
    y = "Vaccination Rate",
    color = "Social Distance\nIntervention"
  ) +
  theme(
    legend.text = element_text(size = 11),
    legend.title = element_text(size = 11),
    strip.text = element_text(size = 10)
  )

ggsave("facet_social_influence.png", p_no_influence, width = 10, height = 6, dpi = 300)


# Social Influence Plot -----------------------------------------------------

# Filter for social influence condition
dat_social_influence <- dat_summary %>%
  filter(Gamma == "With  Social Influence")

# Plot
plot_social_influence <- ggplot(dat_social_influence, aes(x = Step, y = mean_VaccRate, color = SocDistIntervention)) +
  geom_line(size = 1) +
  facet_wrap(~ InitialVaccRate, ncol = 5, labeller = label_both) +
  theme_minimal() +
  labs(
    title = "Vaccination Over Time (With Social Influence)",
    x = "Steps",
    y = "Vaccination Rate",
    color = "Social Distance\nIntervention"
  ) +
  theme(
    legend.text = element_text(size = 11),
    plot.background = element_rect(fill = "white", color = NA)  # Ensure white background
  ) +
  scale_y_continuous(limits = c(0, 1))

ggsave("plot_with_social_influence.png", plot = plot_social_influence, width = 10, height = 6, dpi = 300)
