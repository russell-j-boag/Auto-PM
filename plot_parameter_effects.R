# Clear workspace
rm(list=ls())

# Set working directory
getwd()

# Load packages
library(tidyverse)
library(gridExtra)

# Source model functions
source("dmc/dmc.R")
load_model("LBA", "lba_B.R")


# Summary functions -------------------------------------------------------

fixedeffects.meanthetas <- function(samples){
  ## bring longer thetas down to min nmc by sampling
  nmcs <- sapply(samples, function(x) x$nmc)
  nmc <- min(nmcs)
  for (i in 1:length(samples)) if (nmcs[i] > nmc) samples[[i]]$theta <-
    samples[[i]]$theta[,,sample(1:dim(samples[[i]]$theta)[3], nmc)]
  samps <- lapply(samples, function(x) x["theta"])
  
  ## thetas into big array for apply
  samps2 <- unlist(samps)
  dim3 <- c(dim(samps[[1]]$theta), length(samps2)/prod(dim(samps[[1]]$theta)))
  dim(samps2) <- dim3
  samps3 <- apply(samps2, c(1,2,3), mean)
  
  ## back to a theta list after applied
  colnames(samps3) <- colnames(samps[[1]]$theta)
  samps5 <- list(samps3)
  attributes(samps5) <- attributes(samps[[1]])
  samps5
}


# -------------------------------------------------------------------------

# Load samples
# print(load("samples/sAutoPM_full_sdvS.RData"))
# samples <- samples1
print(load("samples/sAutoPM_full.RData"))
samples <- samples1
# print(load("samples/sAutoPM_B.RData"))
# samples <- samples1
samples[[1]]$p.names


# Get parameter summary ---------------------------------------------------
# summary.dmc()
# summary.dmc(samples[[1]])$statistics

# parms <- summary.dmc(samples)
# parms <- do.call(rbind, lapply(parms, function(x) x$statistics[,1]))
# parms

# Load parameter summary
# print(load("deriv/map_parms_full_sdvS.RData"))
print(load("deriv/map_parms_full.RData"))
# print(load("deriv/map_parms_B.RData"))
str(parms)
head(parms)
nrow(parms)

# Mean over participants
colMeans(parms)

# Summarize thetas
mean_thetas <- fixedeffects.meanthetas(samples)[[1]]

# Save
# save(mean_thetas, file = "deriv/mean_thetas_full_sdvS.RData")
save(mean_thetas, file = "deriv/mean_thetas_full.RData")
# save(mean_thetas, file = "deriv/mean_thetas_B.RData")

# Load
# print(load("deriv/mean_thetas_full_sdvS.RData"))
print(load("deriv/mean_thetas_full.RData"))
# print(load("deriv/mean_thetas_B.RData"))

# Explore mean thetas
str(mean_thetas)
dim(mean_thetas)

# Check for NAs (should not be any)
any(mean_thetas[is.na(mean_thetas)])


# Create parameter data frame ---------------------------------------------

msds <- cbind(apply(mean_thetas, 2, median), apply(mean_thetas, 2, sd))
colnames(msds) <- c("M", "SD")
msds <- data.frame(msds)
msds
colMeans(parms)


# Add factors for plotting ------------------------------------------------
ps <- data.frame(msds)
head(ps, 10)
ps$auto <- NA; ps$PM <- NA; ps$fail <- NA; ps$S <- NA; ps$R <- NA

ps$auto[grep(".A", rownames(ps))] <- "Auto"
ps$auto[grep(".M", rownames(ps))] <- "Manual"

ps$PM[grep("2", rownames(ps))] <- "Control"
ps$PM[grep("3", rownames(ps))] <- "PM"

ps$fail[grep("nonf", rownames(ps))] <- "Auto. success"
ps$fail[grep("Mnonf", rownames(ps))] <- "Manual"
ps$fail[grep("fail", rownames(ps))] <- "Auto. failure"

ps$S[grep("cc", rownames(ps))] <- "Conflict"
ps$S[grep("nn", rownames(ps))] <- "Non-conflict"
ps$S[grep("pc", rownames(ps))] <- "PM conflict"
ps$S[grep("pn", rownames(ps))] <- "PM non-conflict"
ps$S[grep("pp", rownames(ps))] <- "PM"

ps$R[grep("C", rownames(ps))] <- "Conflict"
ps$R[grep("N", rownames(ps))] <- "Non-conflict"
ps$R[grep("P", rownames(ps))] <- "PM"

ps$auto <- factor(ps$auto)
ps$PM <- factor(ps$PM)
ps$fail <- factor(ps$fail)
ps$S <- factor(ps$S)
ps$R <- factor(ps$R)
str(ps)
ps

# Get A
A <- ps[ grep("^A", rownames(ps)), c("M", "SD") ]
A

# Get B
B <- ps[ grep("B.", rownames(ps)), c("M", "SD", "auto", "PM", "R") ]
B

# Get v
v <- ps[ grep("mean_v.", rownames(ps)), ]
v <- v[ -grep("PMFA", rownames(v)), ]
v

# Exclude PM miss rates
v <- v[!(v$S == "PM conflict" & v$R != "PM") & !(v$S == "PM non-conflict" & v$R != "PM"),]
v

# Get t0
t0 <- ps[ grep("t0",rownames(ps)), c("M", "SD") ]
t0


# Make plots --------------------------------------------------------------

# Plot thresholds
B_plot <- B %>%
  ggplot(aes(x = auto, y = M)) +
  geom_point(stat = "identity", aes(color = R, shape = R), size = 3) +
  geom_line(aes(y = M, group = R, color = R), 
            linetype = "dashed", 
            size = 0.8) +
  geom_errorbar(aes(ymin = M - SD, 
                    ymax = M + SD, 
                    width = 0.3, color = R)) +
  ylim(0.5, 3.5) +
  facet_grid(. ~ PM) +
  labs(title = "Threshold", 
       x = "Automation condition", 
       y = "B", 
       color = "Response",
       shape = "Response") +
  theme_minimal()
B_plot

ggsave("plots/B_plot.png", plot = B_plot, 
       width = 2000, height = 1400, units = "px")


# Plot ongoing task rates - Manual vs. Auto (i.e., excluding failure trials)
v_ongoing_plot <- v[v$S != "PM" & v$fail != "Auto. failure" ,] %>%
  ggplot(aes(x = auto, y = M, shape = R, color = R)) +
  geom_point(stat = "identity", aes(), size = 3) +
  geom_line(aes(y = M, group = R), 
            linetype = "dashed", 
            size = 0.8) +
  geom_errorbar(aes(ymin = M - SD, 
                    ymax = M + SD, 
                    width = 0.3)) +
  ylim(0, 1.8) +
  facet_grid(S ~ PM) +
  labs(title = "Ongoing task accumulation rate", 
       x = "Automation condition", 
       y = "v", 
       color = "Response",
       shape = "Response") +
  theme_minimal()
v_ongoing_plot

ggsave("plots/v_ongoing_plot.png", plot = v_ongoing_plot, 
       width = 2400, height = 1400, units = "px")

# Plot ongoing task rates - Auto. success vs. Auto failure (i.e., excluding manual trials)
v_ongoing_failures_plot <- v[v$S != "PM" ,] %>%
  ggplot(aes(x = fail, y = M, shape = R, color = R)) +
  geom_point(stat = "identity", aes(), size = 3) +
  geom_line(aes(y = M, group = R), 
            linetype = "dashed", 
            size = 0.8) +
  geom_errorbar(aes(ymin = M - SD, 
                    ymax = M + SD, 
                    width = 0.3)) +
  ylim(0, 1.8) +
  facet_grid(S ~ PM) +
  labs(title = "Ongoing task accumulation rate (automation failure contrast)", 
       x = "Automation success/failure", 
       y = "v", 
       color = "Response",
       shape = "Response") +
  theme_minimal()
v_ongoing_failures_plot

ggsave("plots/v_ongoing_failures_plot.png", plot = v_ongoing_failures_plot, 
       width = 2400, height = 1400, units = "px")


# Plot PM rates - Manual vs. Auto (i.e., excluding failure trials)
v_PM_plot <- v[v$S == "PM",] %>%
  ggplot(aes(x = auto, y = M, shape = fail, color = fail)) +
  geom_point(stat = "identity", aes(), size = 3) +
  geom_line(aes(y = M, group = fail), 
            linetype = "dashed", 
            size = 0.8) +
  geom_errorbar(aes(ymin = M - SD, 
                    ymax = M + SD, 
                    width = 0.3)) +
  ylim(1.5, 1.8) +
  # facet_grid(. ~ PM) +
  labs(title = "PM accumulation rate", 
       x = "Automation condition", 
       y = "v", 
       color = "Automation success/failure",
       shape = "Automation success/failure") +
  theme_minimal()
v_PM_plot

ggsave("plots/v_PM_plot.png", plot = v_PM_plot, 
       width = 1600, height = 1000, units = "px")

