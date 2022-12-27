# Clear workspace
rm(list = ls())

# Set working directory
getwd()

# Load packages
library(tidyverse)
library(gridExtra)

# Source model functions
source("dmc/dmc.R")
load_model("LBA", "lbaN_B.R")

# Load samples
print(load("samples/sAutoPM_full_sdvS.RData"))
samples <- samples1


# Check how many runs it took to converge
# If any say "FAIL" then it didn't converge
for(i in 1:length(samples)) {
  print(attr(samples[[i]], "auto"))
}

# Check your samples are all the same length for every participant
for(i in 1:length(samples)) {
  print(samples[[i]]$nmc)
}

# Bring longer thetas down to min nmc by sampling
nmcs <- sapply(samples, function(x) x$nmc)
nmc  <- min(nmcs)
for (i in 1:length(samples)) if (nmcs[i] > nmc) samples[[i]]$theta <-
  samples[[i]]$theta[,,sample(1:dim(samples[[i]]$theta)[3], nmc)]

ci <- subject.average.ci(samples)
round(ci, 3)

# How many parameters? How many chains?
names(samples[[1]])
str(samples[[1]]$theta)



# Posterior predictive fits -----------------------------------------------

# # Generate posterior predictives
# post_pred_sims <- h.post.predict.dmc(samples, save.simulation = TRUE)
# save(post_pred_sims, file = "deriv/post_pred_sims_full_sdvS.RData")

# Load posterior predictives
print(load("deriv/post_pred_sims_full_sdvS.RData"))


# Stack into data frame
sims <- do.call(rbind, post_pred_sims)
# str(sims)
sims$correct <- NULL
dim(sims)
head(sims, 10)
sims <- sims[,c("reps","S","auto","PM","failtrial","R","RT")]

# Do the same for the data
data <- lapply(post_pred_sims, function(x) attr(x, "data"))
data <- do.call(rbind, data)
data$correct <- NULL
head(data, 10)
data <- data[,c("S","auto","PM","failtrial","R","RT")]

# Create match function to code accuracy
matchfun = function (d) {
  (d$S=="cc" & d$R=="C")|(d$S=="nn" & d$R=="N")|
    (d$S=="pc" & d$R=="P")|(d$S=="pn" & d$R=="P")
}

table(matchfun(data))
table(matchfun(sims))

# Get a gglist for plotting (as stored in post_pred_sims)
# get.fitgglist.dmc
GGLIST <- get.fitgglist.dmc(sims, data, acc.fun = matchfun)
GGLIST

# Remove NAs
GGLIST <- lapply(GGLIST, function(x)  x[is.finite(x$data),])
names(GGLIST)



# Correct response proportion ---------------------------------------------

# Re-order columns for desired panel order
rp <- GGLIST$pps
dim(rp)
head(rp, 10)
# rp <- rp[,c(1,3,2,4,5,6,7,8)]

matchfun(rp)

# Take only the correct responses and drop the R column
rp_correct <- rp[ matchfun(rp),
                  !(names(rp) %in% c("R")) ]

# Exclude non-existent rows (i.e., manual fail trials and PM stimuli in control blocks)
rp_correct <- rp_correct[!(rp_correct$auto == "M" & rp_correct$failtrial == "fail"),]
rp_correct <- rp_correct[!((rp_correct$S == "pc"|rp_correct$S == "pn") & rp_correct$PM == "2"),]
rp_correct

levels(rp_correct$auto) 
levels(rp_correct$auto) <- c("Auto", "Manual")
levels(rp_correct$failtrial) 
levels(rp_correct$failtrial) <- c("nonf", "fail", "manu")
rp_correct$failtrial[rp_correct$auto == "Manual"] <- "manu"
rp_correct$failtrial <- factor(rp_correct$failtrial, levels = c("nonf", "manu", "fail"), 
                               labels = c("Auto. success", "Manual", "Auto. failure"))
levels(rp_correct$failtrial)
levels(rp_correct$PM) 
levels(rp_correct$PM) <- c("Control", "PM")
levels(rp_correct$S)
levels(rp_correct$S) <- c("Conflict", "Non-conflict", "PM conflict", "PM non-conflict")
str(rp_correct)
head(rp_correct, 10)


# Ongoing task accuracy
rp_correct_ongoing_plot <- rp_correct[(rp_correct$S == "Conflict"|rp_correct$S == "Non-conflict"),] %>%
  ggplot(aes(x = failtrial)) +
  geom_point(stat = "identity", aes(y = data), size = 3, shape = 1) +
  geom_point(stat = "identity", aes(y = median), size = 2) +
  geom_line(aes(y = median, group = S),
            linetype = "dashed", 
            size = 0.8) +
  geom_errorbar(aes(ymin = lower, 
                    ymax = upper, 
                    width = 0.3)) +
  ylim(0.5, 1) +
  facet_grid(S ~ PM) +
  labs(title = "Ongoing task accuracy", 
       x = "Trial type", 
       y = "Response proportion", 
       color = "Stimulus",
       shape = "Stimulus") +
  theme_minimal()
rp_correct_ongoing_plot 



# PM accuracy
rp_correct_PM_plot <- rp_correct[(rp_correct$S == "PM conflict"|rp_correct$S == "PM non-conflict"),] %>%
  ggplot(aes(x = failtrial)) +
  geom_point(stat = "identity", aes(y = data), size = 3, shape = 1) +
  geom_point(stat = "identity", aes(y = median), size = 2) +
  geom_line(aes(y = median, group = S), 
            linetype = "dashed", 
            size = 0.8) +
  geom_errorbar(aes(ymin = lower, 
                    ymax = upper, 
                    width = 0.3)) +
  ylim(0.7, 1) +
  facet_grid(S ~ .) +
  labs(title = "PM accuracy", 
       x = "Trial type", 
       y = "Response proportion", 
       color = "Stimulus",
       shape = "Stimulus") +
  theme_minimal()
rp_correct_PM_plot 




# Correct response time ---------------------------------------------------

# Re-order columns for desired panel order
rt <- GGLIST$RTs
dim(rt)
head(rt, 10)
# rt <- rt[,c(1,3,2,4,5,6,7,8)]


# Take only the correct responses and drop the R column
rt_correct <- rt[ matchfun(rt),
                  !(names(rt) %in% c("R")) ]
rt_correct

levels(rt_correct$auto) 
levels(rt_correct$auto) <- c("Auto", "Manual")
levels(rt_correct$failtrial) 
levels(rt_correct$failtrial) <- c("nonf", "fail", "manu")
rt_correct$failtrial[rt_correct$auto == "Manual"] <- "manu"
rt_correct$failtrial <- factor(rt_correct$failtrial, levels = c("nonf", "manu", "fail"), 
                               labels = c("Auto. success", "Manual", "Auto. failure"))
levels(rt_correct$failtrial)
levels(rt_correct$PM) 
levels(rt_correct$PM) <- c("Control", "PM")
levels(rt_correct$S)
levels(rt_correct$S) <- c("Conflict", "Non-conflict", "PM conflict", "PM non-conflict")
str(rt_correct)
head(rt_correct, 10)


# Correct ongoing task RT
rt_correct_ongoing_plot <- rt_correct[(rt_correct$S == "Conflict"|rt_correct$S == "Non-conflict"),] %>%
  ggplot(aes(x = failtrial)) +
  geom_point(stat = "identity", aes(y = data), size = 3, shape = 1) +
  geom_point(stat = "identity", aes(y = median), size = 2) +
  geom_line(aes(y = median, group = quantile), 
            linetype = "dashed", 
            size = 0.8) +
  geom_errorbar(aes(ymin = lower, 
                    ymax = upper, 
                    width = 0.3)) +
  ylim(0.2, 4.5) +
  facet_grid(S ~ PM) +
  labs(title = "Ongoing task RT (correct responses)", 
       x = "Trial type", 
       y = "RT (s)", 
       color = "Stimulus",
       shape = "Stimulus") +
  theme_minimal()
rt_correct_ongoing_plot 



# Correct PM RT
rt_correct_PM_plot <- rt_correct[(rt_correct$S == "PM conflict"|rt_correct$S == "PM non-conflict"),] %>%
  ggplot(aes(x = failtrial)) +
  geom_point(stat = "identity", aes(y = data), size = 3, shape = 1) +
  geom_point(stat = "identity", aes(y = median), size = 2) +
  geom_line(aes(y = median, group = quantile), 
            linetype = "dashed", 
            size = 0.8) +
  geom_errorbar(aes(ymin = lower, 
                    ymax = upper, 
                    width = 0.3)) +
  ylim(0, 2.8) +
  facet_grid(S ~ .) +
  labs(title = "PM RT (correct responses)",  
       x = "Trial type", 
       y = "RT (s)", 
       color = "Stimulus",
       shape = "Stimulus") +
  theme_minimal()
rt_correct_PM_plot




# Error response time -----------------------------------------------------

# Pull out RTs
rt <- GGLIST$RTs
dim(rt)
head(rt, 10)

# Take only the correct responses and drop the R column
rt_error <- rt[ !matchfun(rt), ]
rt_error

levels(rt_error$auto) 
levels(rt_error$auto) <- c("Auto", "Manual")
levels(rt_error$failtrial) 
levels(rt_error$failtrial) <- c("nonf", "fail", "manu")
rt_error$failtrial[rt_error$auto == "Manual"] <- "manu"
rt_error$failtrial <- factor(rt_error$failtrial, levels = c("nonf", "manu", "fail"), 
                               labels = c("Auto. success", "Manual", "Auto. failure"))
levels(rt_error$failtrial)
levels(rt_error$PM) 
levels(rt_error$PM) <- c("Control", "PM")
levels(rt_error$S)
levels(rt_error$S) <- c("Conflict", "Non-conflict", "PM conflict", "PM non-conflict")
levels(rt_error$R) 
levels(rt_error$R) <- c("Conflict", "Non-conflict", "PM")
str(rt_error)
head(rt_error, 10)


# Incorrect ongoing task RT
rt_error_ongoing_plot <- rt_error[((rt_error$S == "Conflict" & rt_error$R == "Non-conflict")|
                                     (rt_error$S == "Non-conflict" & rt_error$R == "Conflict")),] %>%
  ggplot(aes(x = failtrial)) +
  geom_point(stat = "identity", aes(y = data), size = 3, shape = 1) +
  geom_point(stat = "identity", aes(y = median), size = 2) +
  geom_line(aes(y = median, group = quantile), 
            linetype = "dashed", 
            size = 0.8) +
  geom_errorbar(aes(ymin = lower, 
                    ymax = upper, 
                    width = 0.3)) +
  ylim(0.2, 5.2) +
  facet_grid(S ~ PM) +
  labs(title = "Ongoing task RT (incorrect responses)",
       x = "Trial type", 
       y = "RT (s)", 
       color = "Stimulus",
       shape = "Stimulus") +
  theme_minimal()
rt_error_ongoing_plot 



# Incorrect PM RT
rt_error_PM_plot <- rt_error[((rt_error$S != "PM conflict" & rt_error$R == "PM")|
                                (rt_error$S != "PM non-conflict" & rt_error$R == "PM")),] %>%
  ggplot(aes(x = failtrial)) +
  geom_point(stat = "identity", aes(y = data), size = 3, shape = 1) +
  geom_point(stat = "identity", aes(y = median), size = 2) +
  geom_line(aes(y = median, group = quantile), 
            linetype = "dashed", 
            size = 0.8) +
  geom_errorbar(aes(ymin = lower, 
                    ymax = upper, 
                    width = 0.3)) +
  ylim(0.2, 9) +
  facet_grid(S ~ PM) +
  labs(title = "PM RT (incorrect responses)", 
       x = "Trial type", 
       y = "RT (s)", 
       color = "Stimulus",
       shape = "Stimulus") +
  theme_minimal()
rt_error_PM_plot 




# Combine and save plots --------------------------------------------------

library("gridExtra")
library("ggpubr")

# Accuracy plots
ggsave("plots/fits_accuracy_ongoing.png", plot = rp_correct_ongoing_plot, 
       width = 2000, height = 1400, units = "px")

ggsave("plots/fits_accuracy_PM.png", plot = rp_correct_PM_plot, 
       width = 2000, height = 1400, units = "px")

ggsave("plots/fits_correct_RT_PM.png", plot = rt_correct_PM_plot, 
       width = 2000, height = 1400, units = "px")


# Ongoing task RT
# Remove plot legends
rt_correct_ongoing_plot <- rt_correct_ongoing_plot + theme(legend.position = "none",
                                                           axis.title.x = element_blank(),
                                                           axis.text.x = element_blank())

rt_error_ongoing_plot <- rt_error_ongoing_plot + theme(legend.position = "none",
                                                       strip.text.x = element_blank())


ggarrange(rt_correct_ongoing_plot, rt_error_ongoing_plot, nrow = 2,
          common.legend = TRUE, legend = "right")

ggsave("plots/fits_RT_ongoing.png", plot = last_plot(), 
       width = 2200, height = 2000, units = "px")



# PM RT
# Remove plot legends
rt_correct_PM_plot <- rt_correct_PM_plot + theme(legend.position = "none",
                                                 axis.title.x = element_blank(),
                                                 axis.text.x = element_blank())

rt_error_PM_plot <- rt_error_PM_plot + theme(legend.position = "none",
                                             strip.text.x = element_blank())

ggarrange(rt_correct_PM_plot, rt_error_PM_plot, nrow = 2,
          common.legend = TRUE, legend = "right")

ggsave("plots/fits_RT_PM.png", plot = last_plot(), 
       width = 2200, height = 2000, units = "px")

