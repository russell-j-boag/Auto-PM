# Clear workspace
rm(list=ls())

# Set working directory to top-level folder containing DMC
# setwd("~/AutoPM")

# Load libraries and DMC functions
source("dmc/dmc.R")
load_model("LBA", "lbaN_B.R")


# Table to store DICs
DIC_table <- data.frame(
  full = rep(NA, 24), 
  B = rep(NA, 24)
)

# Load samples
print(load("samples/sAutoPM_full.RData"))
samples <- samples1
DIC_table$full <- data.frame(h.IC.dmc(samples, DIC = TRUE))$IC

# Load samples
print(load("samples/sAutoPM_B.RData"))
samples <- samples1
DIC_table$B <- data.frame(h.IC.dmc(samples, DIC = TRUE))$IC


# Table of DICs for each subject and model
DIC_table


# Find overall winning model with minimum DIC
colSums(DIC_table)
which.min(colSums(DIC_table))


# Get counts of preferred model for each subject
min.col = function(m, ...) max.col(-m, ...)
min.col(DIC_table, "last")
data.frame(winner = colnames(DIC_table)[min.col(DIC_table, "last")])


# Winning model subject count
table(data.frame(winner = colnames(DIC_table)[min.col(DIC_table, "last")]))


# Winning model subject percentage
table(data.frame(winner = colnames(DIC_table)[min.col(DIC_table, "last")]))/24*100


# Convert to DIC difference from winner
colSums(DIC_table) - colSums(DIC_table)["full"]

