## ---------------------------
##
## Title: Network Analysis DE
##
## Author: Eva- Maria Vogel
##
## Date Created: 2025-05
##
## ---------------------------

library(qgraph)
library(bootnet)
library(dplyr)
library(NetworkComparisonTest)
library(tibble)
library(ggplot2)
library(tidyr)
library(flextable)
library(officer)

set.seed(12345)

# Load and prep data -----------------------------------------------------------
de_path <- file.path("..", "PSYDISC", "data", "processed", "DEdat.csv")
dat <- as.data.frame(read.csv(de_path))

# Recode vaccine variable: 
# 1 or 2 -> 1 (vaccinated), 
# 3 -> 0 (not vaccinated)
dat$vaccin_bin <- ifelse(dat$vaccin %in% c(1, 2), 1,
                         ifelse(dat$vaccin == 3, 0, NA))

# Select relevant variables
dat <- dat %>%
  select(# psydisc = psydisc_alt,
         soc,
         FIS,
         spirit, rel = religiosity, 
         lit = literacy, 
         cons = CP, LR,
         edu = formal_edu, age = age_cont, sex = sex_rec,
         vcc = vaccin_bin) %>%
  na.omit()

# DESCRIPTIVES ---------------------------------------------------------------
desc_cont <- dat %>%
  summarise(
    M_soc = mean(soc), SD_soc = sd(soc),
    Min_soc = min(soc), Max_soc = max(soc),
    
    M_FIS = mean(FIS), SD_FIS = sd(FIS),
    Min_FIS = min(FIS), Max_FIS = max(FIS),
    
    M_spirit = mean(spirit), SD_spirit = sd(spirit),
    Min_spirit = min(spirit), Max_spirit = max(spirit),
    
    M_rel = mean(rel), SD_rel = sd(rel),
    Min_rel = min(rel), Max_rel = max(rel),
    
    M_lit = mean(lit), SD_lit = sd(lit),
    Min_lit = min(lit), Max_lit = max(lit),
    
    M_cons = mean(cons), SD_cons = sd(cons),
    Min_cons = min(cons), Max_cons = max(cons),
    
    M_LR = mean(LR), SD_LR = sd(LR),
    Min_LR = min(LR), Max_LR = max(LR),
    
    M_edu = mean(edu), SD_edu = sd(edu),
    Min_edu = min(edu), Max_edu = max(edu)
  ) %>%
  pivot_longer(everything()) %>%
  separate(name, into = c("stat", "var"), sep = "_", extra = "merge") %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  mutate(
    Variable = recode(var,
                      psydisc = "Social Distance to Science (SOCDISC)",
                      FIS = "Faith in Science (FIS)",
                      spirit = "Spirituality",
                      rel = "Religiosity",
                      lit = "Science Literacy",
                      cons = "Cultural Conservatism",
                      LR = "Left-Right Ideology",
                      edu = "Education (Years)"
    ),
    M = round(M, 2),
    SD = round(SD, 2),
    Min = round(Min, 2),
    Max = round(Max, 2)
  ) %>%
  select(Variable, M, SD, Min, Max)

# Add binary vaccination variable (% vaccinated)
vcc_row <- tibble(
  Variable = "COVID-19 Vaccinated (%)",
  M = round(mean(dat$vcc, na.rm = TRUE) * 100, 2),
  SD = NA_real_,
  Min = NA_real_,
  Max = NA_real_
)

desc_table <- bind_rows(desc_cont, vcc_row)

# Create APA-style flextable
ft <- flextable(desc_table)
ft <- autofit(ft)
ft <- set_caption(ft, caption = "Table X\nDescriptive Statistics of Study Variables")

# Export to Word
doc <- read_docx() %>%
  body_add_par("Descriptive Statistics", style = "heading 1") %>%
  body_add_flextable(ft)

print(doc, target = "descriptives_table.docx")


# Network Analysis: Vaccine Status ---------------------------------------------
# Estimate network with mixed data handling
network <- estimateNetwork(dat,
                           default = "EBICglasso",
                           corMethod = "cor_auto")

# Extract adjacency matrix
ad_matrix <- network$graph

# Plot
png(filename = paste0(file.path("..", "PSYDISC", "results", "figures"), "DE", "_vaccin_network_PSYDISC.png"), width = 1600, height = 800, res = 150)
layout(matrix(1:2, nrow = 2), heights = c(6, 3))

# Network plot
qgraph(network$graph,
       layout = "spring",
       labels = colnames(dat),
       label.cex = 1.1,
       vsize = 6,
       minimum = 0.05)

# Legend
par(mar = c(1,0,1,0))
plot.new()

legend("center",
       legend = c("soc - Social Distance to Science",
                  "age - age",
                  "sex - Gender",
                  "edu - Education (years)",
                  "lit - Science Literacy",
                  "LR - Left–Right Ideology",
                  "cons - Political Conservatism",
                  "FIS - Faith in Science",
                  "spirit - Spirituality",
                  "rel - Religiosity",
                  "vcc - Vaccinated (yes/no)"),
       ncol = 3,
       cex = 1,
       inset = c(-0.05, 0),
       y.intersp = 1.5,
       bty = "n")
dev.off()


# Bootstrap edges and BCIs -----------------------------------------------------
boot1 <- bootnet(network, nBoots = 1000, , default = "EBICglasso", nCores = parallel::detectCores())

# Plot BCIs
png(filename = paste0(file.path("..", "PSYDISC", "results", "figures"), "DE", "bootstrap_edges.png"), width = 1000, height = 1000, res = 150)
plot(boot1, order = "sample", plot = "interval", split0 = TRUE)
dev.off()

# Strength centrality:
# Plot bootstrapped strength centrality
png(filename = paste0(file.path("..", "PSYDISC", "results", "figures"), "DE", "strength_centrality.png"), width = 1000, height = 1000, res = 150)
plot(boot1, plot = "area", statistics = "strength") +
  theme(
    axis.text = element_text(size = 11)
  )
dev.off()

# Betweenness centrality:
centralities <- as.data.frame(centrality(network)$Betweenness)
centralities <- data.frame(Variables = rownames(centralities), Betweenness = as.numeric(centralities[,1]))

# Plot betweenness centrality
png(filename = paste0(file.path("..", "PSYDISC", "results", "figures"), "DE", "between_centrality.png"), width = 1000, height = 1000, res = 150)
ggplot(centralities, aes(x = reorder(Variables, Betweenness), y = Betweenness)) +
  geom_bar(stat = "identity", fill = "darkgrey") +
  coord_flip() +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 15), breaks = seq(0, 15, 1)) +
  theme(
    axis.text = element_text(size = 11)
  ) +
  labs(x = "Variable",
       y = "Betweenness")
dev.off()


