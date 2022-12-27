# Clear workspace
rm(list=ls())

# Set working directory
getwd()

# Load packages
library(tidyverse)
library(gridExtra)

# Source model functions
source("dmc/dmc.R")
load_model("LBA", "lbaN_B.R")


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
print(load("samples/sAutoPM_full_sdvS.RData"))
samples <- samples1
samples[[1]]$p.names


# Get parameter summary ---------------------------------------------------
# summary.dmc()
# summary.dmc(samples[[1]])$statistics

# parms <- summary.dmc(samples)
# parms <- do.call(rbind, lapply(parms, function(x) x$statistics[,1]))
# parms

# Load parameter summary
print(load("deriv/map_parms_full_sdvS.RData"))
str(parms)
head(parms)
nrow(parms)

# Mean over participants
colMeans(parms)

# Summarize thetas
mean_thetas <- fixedeffects.meanthetas(samples)[[1]]

# Save
save(mean_thetas, file = "deriv/mean_thetas_full_sdvS.RData")

# Load
print(load("deriv/mean_thetas_full_sdvS.RData"))

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
ps$auto <- NA; ps$PM <- NA; ps$PM_trial <- NA; ps$fail <- NA; ps$S <- NA; ps$R <- NA

ps$auto[grep(".A", rownames(ps))] <- "Auto"
ps$auto[grep(".M", rownames(ps))] <- "Manual"

ps$PM[grep("2", rownames(ps))] <- "Control"
ps$PM[grep("3", rownames(ps))] <- "PM"

ps$PM_trial[grep("cc", rownames(ps))] <- "Absent"
ps$PM_trial[grep("nn", rownames(ps))] <- "Absent"
ps$PM_trial[grep("pc", rownames(ps))] <- "Present"
ps$PM_trial[grep("pn", rownames(ps))] <- "Present"
ps$PM_trial[grep("pp", rownames(ps))] <- "Present"

ps$fail[grep("Anonf", rownames(ps))] <- "Auto. success"
ps$fail[grep("Mmanu", rownames(ps))] <- "Manual"
ps$fail[grep("Afail", rownames(ps))] <- "Auto. failure"

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
ps$fail <- factor(ps$fail, levels = c("Auto. success", "Manual", "Auto. failure"))
ps$S <- factor(ps$S)
ps$R <- factor(ps$R)
str(ps)
ps

# Congruence-coded rates
head(ps)
congruence <- ps[ grep("mean_v.", rownames(ps)), ]
congruence <- congruence[ -grep("PMFA", rownames(congruence)), ]
congruence <- congruence[ !(congruence$S == "PM conflict"|congruence$S == "PM non-conflict"|congruence$S == "PM"), ]
congruence$congruence <- NA

congruence$congruence[(congruence$fail == "Auto. success" & congruence$S == "Conflict") &
                        congruence$R == "Conflict"] <- "Congruent"
congruence$congruence[(congruence$fail == "Auto. success" & congruence$S == "Conflict") &
                        congruence$R == "Non-conflict"] <- "Incongruent"

congruence$congruence[(congruence$fail == "Auto. failure" & congruence$S == "Conflict") &
                        congruence$R == "Conflict"] <- "Incongruent"
congruence$congruence[(congruence$fail == "Auto. failure" & congruence$S == "Conflict") &
                        congruence$R == "Non-conflict"] <- "Congruent"

congruence$congruence[(congruence$fail == "Auto. success" & congruence$S == "Non-conflict") &
                        congruence$R == "Non-conflict"] <- "Congruent"
congruence$congruence[(congruence$fail == "Auto. success" & congruence$S == "Non-conflict") &
                        congruence$R == "Conflict"] <- "Incongruent"

congruence$congruence[(congruence$fail == "Auto. failure" & congruence$S == "Non-conflict") &
                        congruence$R == "Non-conflict"] <- "Incongruent"
congruence$congruence[(congruence$fail == "Auto. failure" & congruence$S == "Non-conflict") &
                        congruence$R == "Conflict"] <- "Congruent"

congruence$congruence[(congruence$fail == "Manual")] <- "Manual"
congruence$congruence <- factor(congruence$congruence, levels = c("Congruent", "Manual", "Incongruent"))
congruence

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


# Get reactive control v
reactive <- ps[ grep("mean_v.", rownames(ps)), ]
reactive <- reactive[ -grep("PMFA", rownames(reactive)), ]
reactive <- reactive[ -grep("P", rownames(reactive)), ]
reactive <- reactive[ (reactive$S == "Conflict" & reactive$R == "Conflict")|
                        (reactive$S == "PM conflict" & reactive$R == "Conflict")|
                        (reactive$S == "Non-conflict" & reactive$R == "Non-conflict")|
                        (reactive$S == "PM non-conflict" & reactive$R == "Non-conflict"), ]
reactive


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
  ylim(0.4, 2.1) +
  facet_grid(. ~ PM) +
  labs(title = "Threshold", 
       x = "Automation condition", 
       y = "B", 
       color = "Response",
       shape = "Response") +
  theme_minimal()
B_plot

ggsave("plots/B_plot.png", plot = B_plot, 
       width = 2000, height = 1200, units = "px")



# Plot ongoing task rates
v_ongoing_plot <- v[v$S != "PM" ,] %>%
  ggplot(aes(x = fail, y = M, shape = R, color = R)) +
  geom_point(stat = "identity", aes(), size = 3) +
  geom_line(aes(y = M, group = R), 
            linetype = "dashed", 
            size = 0.8) +
  geom_errorbar(aes(ymin = M - SD, 
                    ymax = M + SD, 
                    width = 0.3)) +
  ylim(0.1, 1.4) +
  facet_grid(S ~ PM) +
  labs(title = "Ongoing task accumulation rate", 
       x = "Trial type", 
       y = "v", 
       color = "Response",
       shape = "Response") +
  theme_minimal()
v_ongoing_plot

ggsave("plots/v_ongoing_plot.png", plot = v_ongoing_plot, 
       width = 2400, height = 1400, units = "px")



# Plot PM rates
v_PM_plot <- v[v$S == "PM",] %>%
  ggplot(aes(x = fail, y = M)) +
  geom_point(stat = "identity", aes(), size = 3) +
  geom_line(aes(y = M, group = 1), 
            linetype = "dashed", 
            size = 0.8) +
  geom_errorbar(aes(ymin = M - SD, 
                    ymax = M + SD, 
                    width = 0.3)) +
  ylim(1.44, 1.72) +
  # facet_grid(. ~ PM) +
  labs(title = "PM accumulation rate", 
       x = "Trial type", 
       y = "v") +
  theme_minimal()
v_PM_plot

ggsave("plots/v_PM_plot.png", plot = v_PM_plot, 
       width = 1600, height = 1000, units = "px")



# Plot reactive control
reactive_plot <- reactive[reactive$PM == "PM",] %>%
  ggplot(aes(x = PM_trial, y = M, shape = R, color = R)) +
  geom_point(stat = "identity", aes(), size = 3) +
  geom_line(aes(y = M, group = R), 
            linetype = "dashed", 
            size = 0.8) +
  geom_errorbar(aes(ymin = M - SD, 
                    ymax = M + SD, 
                    width = 0.3)) +
  ylim(0, 1.4) +
  facet_grid(. ~ fail) +
  labs(title = "Reactive control of ongoing task accumulation rate", 
       x = "PM stimulus", 
       y = "v", 
       color = "Response",
       shape = "Response") +
  theme_minimal()
reactive_plot

ggsave("plots/reactive_plot.png", plot = reactive_plot, 
       width = 2200, height = 1200, units = "px")



# Plot ongoing task rates (congruence-coded)
v_ongoing_plot_congruence <- congruence %>%
  ggplot(aes(x = congruence, y = M, shape = R, color = R)) +
  geom_point(stat = "identity", aes(), size = 3) +
  geom_line(aes(y = M, group = R), 
            linetype = "dashed", 
            size = 0.8) +
  geom_errorbar(aes(ymin = M - SD, 
                    ymax = M + SD, 
                    width = 0.3)) +
  ylim(0.1, 1.3) +
  facet_grid(S ~ PM) +
  labs(title = "Ongoing task accumulation rate", 
       x = "Congruence with automation", 
       y = "v", 
       color = "Response",
       shape = "Response") +
  theme_minimal()
v_ongoing_plot_congruence

ggsave("plots/v_ongoing_plot_congruence.png", plot = v_ongoing_plot_congruence, 
       width = 2400, height = 1400, units = "px")
