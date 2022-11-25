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

group.inference.dist <- function (hsamples, fun) {
  # Bring longer thetas down to minimum nmc by sampling
  nmcs <- sapply(hsamples, function(x) x$nmc)
  nmc  <- min(nmcs)
  for (i in 1:length(hsamples)) if (nmcs[i] > nmc) {
    hsamples[[i]]$theta <- 
      hsamples[[i]]$theta[, , sample(1:dim(hsamples[[i]]$theta)[3], nmc)]
  }
  # inference <- lapply(hsamples, function(x) x["theta"])
  inference <- list()
  for (i in 1:length(hsamples)) {
    thetas <- hsamples[[i]]$theta
    inference [[i]] <- fun (thetas)
  }
  inf2 <- unlist(inference)
  dim3 <- c(dim(inference[[1]]), length(inf2)/prod(dim(inference[[1]])))
  dim(inf2) <- dim3
  apply(inf2, c(3, 1), mean)
}

subject.inference.dist <- function (hsamples, fun) {
  # Bring longer thetas down to minimum nmc by sampling
  nmcs <- sapply(hsamples, function(x) x$nmc)
  nmc  <- min(nmcs)
  for (i in 1:length(hsamples)) if (nmcs[i] > nmc) {
    hsamples[[i]]$theta <- 
      hsamples[[i]]$theta[, , sample(1:dim(hsamples[[i]]$theta)[3], nmc)]
  }
  # inference <- lapply(hsamples, function(x) x["theta"])
  inference <- list()
  for (i in 1:length(hsamples)) {
    thetas <- hsamples[[i]]$theta
    inference [[i]] <- fun (thetas)
  }
  inf2 <- unlist(inference)
  dim3 <- c(dim(inference[[1]]), length(inf2)/prod(dim(inference[[1]])))
  dim(inf2) <- dim3
  effect <- apply(inf2, c(4), mean)
  names(effect) <- names(samples)
  data.frame(effect)
}

minp <- function (effect) min(ecdf(effect)(0), 1 - ecdf(effect)(0))

zandp <- function (samples, fun) {
  effect <- group.inference.dist(samples, fun)
  Z <- mean(effect) / sd(effect)
  p <- minp(effect)
  round(data.frame(Z, p), 3)
}

mean.sd <- function (samples, fun) {
  effect <- group.inference.dist(samples, fun)
  M <- median(effect)
  SD <- sd(effect)
  round(data.frame(M, SD), 3)
}


# -------------------------------------------------------------------------

# Load samples
print(load("samples/sAutoPM_full.RData"))
samples <- samples1
length(samples)
samples[[1]]$p.names

# -------------------------------------------------------------------------
# Threshold effects

# Ongoing task thresholds auto/manual contrast
fun <- function (thetas) {
  (thetas[,"B.M2C",, drop=F] + thetas[,"B.M3C",, drop=F] + 
     thetas[,"B.M2N",, drop=F] + thetas[,"B.M3N",, drop=F])/4 -
    (thetas[,"B.A2C",, drop=F] + thetas[,"B.A3C",, drop=F] + 
       thetas[,"B.A2N",, drop=F] + thetas[,"B.A3N",, drop=F])/4 
}

# Ongoing task thresholds higher in manual blocks than with automation
mean.sd(samples, fun)
zandp(samples, fun)

# Get raw samples to plot effect density
effect <- as.numeric(group.inference.dist(samples, fun))
effect <- data.frame(effect = effect)
head(effect) 
mean(effect$effect)


# Density plot
B_ongoing_auto_plot <- ggplot(effect, aes(x = effect)) + 
  geom_histogram(aes(y = stat(density)), binwidth = 0.01,
                 alpha = 0.1,
                 col = "black",
                 fill = "steelblue",
                 size = 0.3) +
  geom_density(alpha = 0.1,
               linetype = "dashed",
               size = 0.3) +
  geom_vline(xintercept = quantile(as.numeric(group.inference.dist(samples, fun)), probs = c(0.025, 0.975)),
             alpha = 0.5, linetype = "dashed",
             size = 0.5) +
  geom_vline(aes(xintercept = 0),
             alpha = 0.8,
             col = "red",
             size = 1) +
  labs(title = "Ongoing task thresholds auto/manual contrast", 
       subtitle = paste("M =", mean.sd(samples, fun)$M,
                        " Z =", zandp(samples, fun)$Z,
                        " p =", zandp(samples, fun)$p),
       x = "Contrast", 
       y = "Density") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0),
    plot.subtitle = element_text(hjust = 0.5)
  )
B_ongoing_auto_plot


# PM thresholds auto/manual contrast
fun <- function (thetas) {
  thetas[,"B.M3P",, drop=F] - thetas[,"B.A3P",, drop=F] 
}

# PM thresholds higher in manual blocks than with automation
mean.sd(samples, fun)
zandp(samples, fun)

# Get raw samples to plot effect density
effect <- as.numeric(group.inference.dist(samples, fun))
effect <- data.frame(effect = effect)
head(effect) 
mean(effect$effect)

# Density plot
B_PM_auto_plot <- ggplot(effect, aes(x = effect)) + 
  geom_histogram(aes(y = stat(density)), binwidth = 0.01,
                 alpha = 0.1,
                 col = "black",
                 fill = "steelblue",
                 size = 0.3) +
  geom_density(alpha = 0.1,
               linetype = "dashed",
               size = 0.3) +
  geom_vline(xintercept = quantile(as.numeric(group.inference.dist(samples, fun)), probs = c(0.025, 0.975)),
             alpha = 0.5, linetype = "dashed",
             size = 0.5) +
  geom_vline(aes(xintercept = 0),
             alpha = 0.8,
             col = "red",
             size = 1) +
  labs(title = "PM thresholds auto/manual contrast", 
       subtitle = paste("M =", mean.sd(samples, fun)$M,
                        " Z =", zandp(samples, fun)$Z,
                        " p =", zandp(samples, fun)$p),
       x = "Contrast", 
       y = "Density") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0),
    plot.subtitle = element_text(hjust = 0.5)
  )
B_PM_auto_plot



# Ongoing task thresholds PM contrast 
fun <- function (thetas) {
  (thetas[,"B.A3C",, drop=F] + thetas[,"B.M3C",, drop=F] + 
     thetas[,"B.A3N",, drop=F] + thetas[,"B.M3N",, drop=F])/4 -
    (thetas[,"B.A2C",, drop=F] + thetas[,"B.M2C",, drop=F] + 
       thetas[,"B.A2N",, drop=F] + thetas[,"B.M2N",, drop=F])/4 
}

# Ongoing task thresholds higher in PM blocks than control blocks (delay theory)
mean.sd(samples, fun)
zandp(samples, fun)

# Get raw samples to plot effect density
effect <- as.numeric(group.inference.dist(samples, fun))
effect <- data.frame(effect = effect)
head(effect) 
mean(effect$effect)

# Density plot
B_ongoing_PM_plot <- ggplot(effect, aes(x = effect)) + 
  geom_histogram(aes(y = stat(density)), binwidth = 0.01,
                 alpha = 0.1,
                 col = "black",
                 fill = "steelblue",
                 size = 0.3) +
  geom_density(alpha = 0.1,
               linetype = "dashed",
               size = 0.3) +
  geom_vline(xintercept = quantile(as.numeric(group.inference.dist(samples, fun)), probs = c(0.025, 0.975)),
             alpha = 0.5, linetype = "dashed",
             size = 0.5) +
  geom_vline(aes(xintercept = 0),
             alpha = 0.8,
             col = "red",
             size = 1) +
  labs(title = "Ongoing task thresholds PM/control contrast", 
       subtitle = paste("M =", mean.sd(samples, fun)$M,
                        " Z =", zandp(samples, fun)$Z,
                        " p =", zandp(samples, fun)$p),
       x = "Contrast", 
       y = "Density") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0),
    plot.subtitle = element_text(hjust = 0.5)
  )
B_ongoing_PM_plot



# Ongoing task thresholds PM delay by automation interaction
fun <- function (thetas) {
  ((thetas[,"B.M3C",, drop=F] + thetas[,"B.M3N",, drop=F])/2 - 
     (thetas[,"B.M2C",, drop=F] + thetas[,"B.M2N",, drop=F])/2) -
    ((thetas[,"B.A3C",, drop=F] + thetas[,"B.A3N",, drop=F])/2 - 
       (thetas[,"B.A2C",, drop=F] + thetas[,"B.A2N",, drop=F])/2) 
}

# Ongoing task thresholds PM delay larger in manual blocks than with automation
mean.sd(samples, fun)
zandp(samples, fun)



# Bias effects ------------------------------------------------------------

# Ongoing task bias
fun <- function (thetas) {
  ((thetas[,"B.M2C",, drop=F] + thetas[,"B.M3C",, drop=F])/2 -
     (thetas[,"B.A2C",, drop=F] + thetas[,"B.A3C",, drop=F])/2) -
    ((thetas[,"B.M2N",, drop=F] + thetas[,"B.M3N",, drop=F])/2 -
       (thetas[,"B.A2N",, drop=F] + thetas[,"B.A3N",, drop=F])/2)
}

# Conflict thresholds higher than non-conflict thresholds
mean.sd(samples, fun)
zandp(samples, fun)

# Ongoing task bias auto/manual contrast 
fun <- function (thetas) {
  ((thetas[,"B.M2C",, drop=F] + thetas[,"B.M3C",, drop=F])/2 -
     (thetas[,"B.M2N",, drop=F] + thetas[,"B.M3N",, drop=F])/2) -
    ((thetas[,"B.A2C",, drop=F] + thetas[,"B.A3C",, drop=F])/2 -
       (thetas[,"B.A2N",, drop=F] + thetas[,"B.A3N",, drop=F])/2)
}

# Bias larger in manual blocks than auto blocks
mean.sd(samples, fun)
zandp(samples, fun)

# Ongoing task bias PM contrast 
fun <- function (thetas) {
  ((thetas[,"B.A3C",, drop=F] + thetas[,"B.M3C",, drop=F])/2 -
     (thetas[,"B.A3N",, drop=F] + thetas[,"B.M3N",, drop=F])/2) -
    ((thetas[,"B.A2C",, drop=F] + thetas[,"B.M2C",, drop=F])/2 -
       (thetas[,"B.A2N",, drop=F] + thetas[,"B.M2N",, drop=F])/2) 
}

# Bias larger in PM blocks than control blocks
mean.sd(samples, fun)
zandp(samples, fun)

# Get raw samples to plot effect density
effect <- as.numeric(group.inference.dist(samples, fun))
effect <- data.frame(effect = effect)
head(effect) 
mean(effect$effect)

# Density plot
B_bias_PM_plot <- ggplot(effect, aes(x = effect)) + 
  geom_histogram(aes(y = stat(density)), binwidth = 0.01,
                 alpha = 0.1,
                 col = "black",
                 fill = "steelblue",
                 size = 0.3) +
  geom_density(alpha = 0.1,
               linetype = "dashed",
               size = 0.3) +
  geom_vline(xintercept = quantile(as.numeric(group.inference.dist(samples, fun)), probs = c(0.025, 0.975)),
             alpha = 0.5, linetype = "dashed",
             size = 0.5) +
  geom_vline(aes(xintercept = 0),
             alpha = 0.8,
             col = "red",
             size = 1) +
  labs(title = "Ongoing task bias PM/control contrast", 
       subtitle = paste("M =", mean.sd(samples, fun)$M,
                        " Z =", zandp(samples, fun)$Z,
                        " p =", zandp(samples, fun)$p),
       x = "Contrast", 
       y = "Density") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0),
    plot.subtitle = element_text(hjust = 0.5)
  )
B_bias_PM_plot



# -------------------------------------------------------------------------

# Ongoing task rate quality effects

samples[[1]]$p.names

# Automation contrast (automation success - manual)
fun <- function (thetas) {
  ((thetas[,"mean_v.ccAnonf2C",, drop=F] + 
      thetas[,"mean_v.ccAnonf3C",, drop=F] +
      thetas[,"mean_v.nnAnonf2N",, drop=F] +
      thetas[,"mean_v.nnAnonf3N",, drop=F])/4 -
     (thetas[,"mean_v.nnAnonf2C",, drop=F] +
        thetas[,"mean_v.nnAnonf3C",, drop=F] +
        thetas[,"mean_v.ccAnonf2N",, drop=F] + 
        thetas[,"mean_v.ccAnonf3N",, drop=F])/4) -
    
    ((thetas[,"mean_v.ccMnonf2C",, drop=F] + 
        thetas[,"mean_v.ccMnonf3C",, drop=F] +
        thetas[,"mean_v.nnMnonf2N",, drop=F] +
        thetas[,"mean_v.nnMnonf3N",, drop=F])/4 -
       (thetas[,"mean_v.nnMnonf2C",, drop=F] +
          thetas[,"mean_v.nnMnonf3C",, drop=F] +
          thetas[,"mean_v.ccMnonf2N",, drop=F] + 
          thetas[,"mean_v.ccMnonf3N",, drop=F])/4) 
}

# Ongoing task quality higher on automation success trials than manual
mean.sd(samples, fun)
zandp(samples, fun)


# Automation contrast (automation success - failure)
fun <- function (thetas) {
  ((thetas[,"mean_v.ccAnonf2C",, drop=F] + 
      thetas[,"mean_v.ccAnonf3C",, drop=F] +
      thetas[,"mean_v.nnAnonf2N",, drop=F] +
      thetas[,"mean_v.nnAnonf3N",, drop=F])/4 -
     (thetas[,"mean_v.nnAnonf2C",, drop=F] +
        thetas[,"mean_v.nnAnonf3C",, drop=F] +
        thetas[,"mean_v.ccAnonf2N",, drop=F] + 
        thetas[,"mean_v.ccAnonf3N",, drop=F])/4) -
    
    ((thetas[,"mean_v.ccAfail2C",, drop=F] + 
        thetas[,"mean_v.ccAfail3C",, drop=F] +
        thetas[,"mean_v.nnAfail2N",, drop=F] +
        thetas[,"mean_v.nnAfail3N",, drop=F])/4 -
       (thetas[,"mean_v.nnAfail2C",, drop=F] +
          thetas[,"mean_v.nnAfail3C",, drop=F] +
          thetas[,"mean_v.ccAfail2N",, drop=F] + 
          thetas[,"mean_v.ccAfail3N",, drop=F])/4) 
}

# Ongoing task quality higher on automation success than failure trials
mean.sd(samples, fun)
zandp(samples, fun)


# Automation contrast (manual - failure)
fun <- function (thetas) {
  ((thetas[,"mean_v.ccMnonf2C",, drop=F] + 
      thetas[,"mean_v.ccMnonf3C",, drop=F] +
      thetas[,"mean_v.nnMnonf2N",, drop=F] +
      thetas[,"mean_v.nnMnonf3N",, drop=F])/4 -
     (thetas[,"mean_v.nnMnonf2C",, drop=F] +
        thetas[,"mean_v.nnMnonf3C",, drop=F] +
        thetas[,"mean_v.ccMnonf2N",, drop=F] + 
        thetas[,"mean_v.ccMnonf3N",, drop=F])/4) -
    
    ((thetas[,"mean_v.ccAfail2C",, drop=F] + 
        thetas[,"mean_v.ccAfail3C",, drop=F] +
        thetas[,"mean_v.nnAfail2N",, drop=F] +
        thetas[,"mean_v.nnAfail3N",, drop=F])/4 -
       (thetas[,"mean_v.nnAfail2C",, drop=F] +
          thetas[,"mean_v.nnAfail3C",, drop=F] +
          thetas[,"mean_v.ccAfail2N",, drop=F] + 
          thetas[,"mean_v.ccAfail3N",, drop=F])/4) 
}

# Ongoing task quality lower on automation failure trials than manual
mean.sd(samples, fun)
zandp(samples, fun)



# PM/control contrast
fun <- function (thetas) {
  ((thetas[,"mean_v.ccMnonf2C",, drop=F] + 
      thetas[,"mean_v.ccAnonf2C",, drop=F] + 
      thetas[,"mean_v.nnMnonf2N",, drop=F] +
      thetas[,"mean_v.nnAnonf2N",, drop=F])/4 -
     (thetas[,"mean_v.nnMnonf2C",, drop=F] +
        thetas[,"mean_v.nnAnonf2C",, drop=F] +
        thetas[,"mean_v.ccMnonf2N",, drop=F] +
        thetas[,"mean_v.ccAnonf2N",, drop=F])/4) -
    
    ((thetas[,"mean_v.ccMnonf3C",, drop=F] +
        thetas[,"mean_v.ccAnonf3C",, drop=F] +
        thetas[,"mean_v.nnMnonf3N",, drop=F] +
        thetas[,"mean_v.nnAnonf3N",, drop=F])/4 -
       (thetas[,"mean_v.nnMnonf3C",, drop=F] +
          thetas[,"mean_v.nnAnonf3C",, drop=F] +
          thetas[,"mean_v.ccMnonf3N",, drop=F] +
          thetas[,"mean_v.ccAnonf3N",, drop=F])/4)
}

# Ongoing task quality lower in PM blocks than control
mean.sd(samples, fun)
zandp(samples, fun)



# Automation benefit in control blocks
fun <- function (thetas) {
  ((thetas[,"mean_v.ccAnonf2C",, drop=F] + 
      thetas[,"mean_v.nnAnonf2N",, drop=F])/2 -
     (thetas[,"mean_v.nnAnonf2C",, drop=F] +
        thetas[,"mean_v.ccAnonf2N",, drop=F])/2) -
    ((thetas[,"mean_v.ccMnonf2C",, drop=F] + 
        thetas[,"mean_v.nnMnonf2N",, drop=F])/2 -
       (thetas[,"mean_v.nnMnonf2C",, drop=F] +
          thetas[,"mean_v.ccMnonf2N",, drop=F])/2)
}

# Successful automation benefits quality in control blocks
mean.sd(samples, fun)
zandp(samples, fun)


# Automation benefit in PM blocks
fun <- function (thetas) {
  ((thetas[,"mean_v.ccAnonf3C",, drop=F] + 
      thetas[,"mean_v.nnAnonf3N",, drop=F])/2 -
     (thetas[,"mean_v.nnAnonf3C",, drop=F] +
        thetas[,"mean_v.ccAnonf3N",, drop=F])/2) -
    ((thetas[,"mean_v.ccMnonf3C",, drop=F] + 
        thetas[,"mean_v.nnMnonf3N",, drop=F])/2 -
       (thetas[,"mean_v.nnMnonf3C",, drop=F] +
          thetas[,"mean_v.ccMnonf3N",, drop=F])/2)
}

# Successful automation also benefits quality in PM blocks
mean.sd(samples, fun)
zandp(samples, fun)


# Automation by PM interaction
fun <- function (thetas) {
  (((thetas[,"mean_v.ccAnonf2C",, drop=F] + 
       thetas[,"mean_v.nnAnonf2N",, drop=F])/2 -
      (thetas[,"mean_v.nnAnonf2C",, drop=F] +
         thetas[,"mean_v.ccAnonf2N",, drop=F])/2) -
     ((thetas[,"mean_v.ccMnonf2C",, drop=F] + 
         thetas[,"mean_v.nnMnonf2N",, drop=F])/2 -
        (thetas[,"mean_v.nnMnonf2C",, drop=F] +
           thetas[,"mean_v.ccMnonf2N",, drop=F])/2)) -
    (((thetas[,"mean_v.ccAnonf3C",, drop=F] + 
         thetas[,"mean_v.nnAnonf3N",, drop=F])/2 -
        (thetas[,"mean_v.nnAnonf3C",, drop=F] +
           thetas[,"mean_v.ccAnonf3N",, drop=F])/2) -
       ((thetas[,"mean_v.ccMnonf3C",, drop=F] + 
           thetas[,"mean_v.nnMnonf3N",, drop=F])/2 -
          (thetas[,"mean_v.nnMnonf3C",, drop=F] +
             thetas[,"mean_v.ccMnonf3N",, drop=F])/2))
}

# Automation benefit to quality larger in control than PM blocks 
# (i.e., automation less effective under PM load)
mean.sd(samples, fun)
zandp(samples, fun)



# -------------------------------------------------------------------------

# PM rates 

# Auto/manual contrast
fun <- function (thetas) {
  (thetas[,"mean_v.ppMnonf3P",, drop=F] -
     thetas[,"mean_v.ppAnonf3P",, drop=F])
}

# PM rates lower with automation compared to manual
mean.sd(samples, fun)
zandp(samples, fun)

# Auto success/failure contrast
fun <- function (thetas) {
  (thetas[,"mean_v.ppAnonf3P",, drop=F] -
     thetas[,"mean_v.ppAfail3P",, drop=F])
}

# PM rates lower on automation failure trials compared to successes
mean.sd(samples, fun)
zandp(samples, fun)

# Manual vs auto failure contrast
fun <- function (thetas) {
  (thetas[,"mean_v.ppMnonf3P",, drop=F] -
     thetas[,"mean_v.ppAfail3P",, drop=F])
}

# PM rates lower on automation failure trials compared to manual
mean.sd(samples, fun)
zandp(samples, fun)




# -------------------------------------------------------------------------

# Ongoing task rate quantity effects

samples[[1]]$p.names

# Automation contrast (automation success - manual)
fun <- function (thetas) {
  (thetas[,"mean_v.ccAnonf2C",, drop=F] + 
     thetas[,"mean_v.ccAnonf3C",, drop=F] +
     thetas[,"mean_v.nnAnonf2N",, drop=F] +
     thetas[,"mean_v.nnAnonf3N",, drop=F] +
     thetas[,"mean_v.nnAnonf2C",, drop=F] +
     thetas[,"mean_v.nnAnonf3C",, drop=F] +
     thetas[,"mean_v.ccAnonf2N",, drop=F] + 
     thetas[,"mean_v.ccAnonf3N",, drop=F])/8 -
    
    (thetas[,"mean_v.ccMnonf2C",, drop=F] + 
       thetas[,"mean_v.ccMnonf3C",, drop=F] +
       thetas[,"mean_v.nnMnonf2N",, drop=F] +
       thetas[,"mean_v.nnMnonf3N",, drop=F] +
       thetas[,"mean_v.nnMnonf2C",, drop=F] +
       thetas[,"mean_v.nnMnonf3C",, drop=F] +
       thetas[,"mean_v.ccMnonf2N",, drop=F] + 
       thetas[,"mean_v.ccMnonf3N",, drop=F])/8 
}

# Ongoing task quantity lower on automation success trials than manual
mean.sd(samples, fun)
zandp(samples, fun)


# Automation contrast (automation success - failure)
fun <- function (thetas) {
  (thetas[,"mean_v.ccAnonf2C",, drop=F] + 
     thetas[,"mean_v.ccAnonf3C",, drop=F] +
     thetas[,"mean_v.nnAnonf2N",, drop=F] +
     thetas[,"mean_v.nnAnonf3N",, drop=F] +
     thetas[,"mean_v.nnAnonf2C",, drop=F] +
     thetas[,"mean_v.nnAnonf3C",, drop=F] +
     thetas[,"mean_v.ccAnonf2N",, drop=F] + 
     thetas[,"mean_v.ccAnonf3N",, drop=F])/8 -
    
    (thetas[,"mean_v.ccAfail2C",, drop=F] + 
       thetas[,"mean_v.ccAfail3C",, drop=F] +
       thetas[,"mean_v.nnAfail2N",, drop=F] +
       thetas[,"mean_v.nnAfail3N",, drop=F] +
       thetas[,"mean_v.nnAfail2C",, drop=F] +
       thetas[,"mean_v.nnAfail3C",, drop=F] +
       thetas[,"mean_v.ccAfail2N",, drop=F] + 
       thetas[,"mean_v.ccAfail3N",, drop=F])/8
}

# Ongoing task quantity lower on automation success than failure trials
mean.sd(samples, fun)
zandp(samples, fun)


# Automation contrast (manual - failure)
fun <- function (thetas) {
  (thetas[,"mean_v.ccMnonf2C",, drop=F] + 
     thetas[,"mean_v.ccMnonf3C",, drop=F] +
     thetas[,"mean_v.nnMnonf2N",, drop=F] +
     thetas[,"mean_v.nnMnonf3N",, drop=F] +
     thetas[,"mean_v.nnMnonf2C",, drop=F] +
     thetas[,"mean_v.nnMnonf3C",, drop=F] +
     thetas[,"mean_v.ccMnonf2N",, drop=F] + 
     thetas[,"mean_v.ccMnonf3N",, drop=F])/8 -
    
    (thetas[,"mean_v.ccAfail2C",, drop=F] + 
       thetas[,"mean_v.ccAfail3C",, drop=F] +
       thetas[,"mean_v.nnAfail2N",, drop=F] +
       thetas[,"mean_v.nnAfail3N",, drop=F] +
       thetas[,"mean_v.nnAfail2C",, drop=F] +
       thetas[,"mean_v.nnAfail3C",, drop=F] +
       thetas[,"mean_v.ccAfail2N",, drop=F] + 
       thetas[,"mean_v.ccAfail3N",, drop=F])/8 
}

# Ongoing task quantity lower on automation failure trials than manual
mean.sd(samples, fun)
zandp(samples, fun)


# PM/control contrast
fun <- function (thetas) {
  (thetas[,"mean_v.ccMnonf2C",, drop=F] + 
     thetas[,"mean_v.ccAnonf2C",, drop=F] + 
     thetas[,"mean_v.nnMnonf2N",, drop=F] +
     thetas[,"mean_v.nnAnonf2N",, drop=F] +
     thetas[,"mean_v.nnMnonf2C",, drop=F] +
     thetas[,"mean_v.nnAnonf2C",, drop=F] +
     thetas[,"mean_v.ccMnonf2N",, drop=F] +
     thetas[,"mean_v.ccAnonf2N",, drop=F])/8 -
    
    (thetas[,"mean_v.ccMnonf3C",, drop=F] +
       thetas[,"mean_v.ccAnonf3C",, drop=F] +
       thetas[,"mean_v.nnMnonf3N",, drop=F] +
       thetas[,"mean_v.nnAnonf3N",, drop=F] +
       thetas[,"mean_v.nnMnonf3C",, drop=F] +
       thetas[,"mean_v.nnAnonf3C",, drop=F] +
       thetas[,"mean_v.ccMnonf3N",, drop=F] +
       thetas[,"mean_v.ccAnonf3N",, drop=F])/8
}

# Ongoing task quantity higher in PM blocks than control
mean.sd(samples, fun)
zandp(samples, fun)


# Automation effect in control blocks
fun <- function (thetas) {
  (thetas[,"mean_v.ccAnonf2C",, drop=F] + 
     thetas[,"mean_v.nnAnonf2N",, drop=F] +
     thetas[,"mean_v.nnAnonf2C",, drop=F] +
     thetas[,"mean_v.ccAnonf2N",, drop=F])/4 -
    (thetas[,"mean_v.ccMnonf2C",, drop=F] + 
       thetas[,"mean_v.nnMnonf2N",, drop=F] +
       thetas[,"mean_v.nnMnonf2C",, drop=F] +
       thetas[,"mean_v.ccMnonf2N",, drop=F])/4
}

# Successful automation reduces quantity (effort) in control blocks
mean.sd(samples, fun)
zandp(samples, fun)


# Automation benefit in PM blocks
fun <- function (thetas) {
  (thetas[,"mean_v.ccAnonf3C",, drop=F] + 
     thetas[,"mean_v.nnAnonf3N",, drop=F] +
     thetas[,"mean_v.nnAnonf3C",, drop=F] +
     thetas[,"mean_v.ccAnonf3N",, drop=F])/4 -
    (thetas[,"mean_v.ccMnonf3C",, drop=F] + 
       thetas[,"mean_v.nnMnonf3N",, drop=F] +
       thetas[,"mean_v.nnMnonf3C",, drop=F] +
       thetas[,"mean_v.ccMnonf3N",, drop=F])/4
}

# Successful automation also reduces quantity (effort) in PM blocks
mean.sd(samples, fun)
zandp(samples, fun)


# Automation by PM interaction
fun <- function (thetas) {
  ((thetas[,"mean_v.ccAnonf2C",, drop=F] + 
      thetas[,"mean_v.nnAnonf2N",, drop=F] +
      thetas[,"mean_v.nnAnonf2C",, drop=F] +
      thetas[,"mean_v.ccAnonf2N",, drop=F])/4 -
     (thetas[,"mean_v.ccMnonf2C",, drop=F] + 
        thetas[,"mean_v.nnMnonf2N",, drop=F] +
        thetas[,"mean_v.nnMnonf2C",, drop=F] +
        thetas[,"mean_v.ccMnonf2N",, drop=F])/4) -
    ((thetas[,"mean_v.ccAnonf3C",, drop=F] + 
        thetas[,"mean_v.nnAnonf3N",, drop=F] +
        thetas[,"mean_v.nnAnonf3C",, drop=F] +
        thetas[,"mean_v.ccAnonf3N",, drop=F])/4 -
       (thetas[,"mean_v.ccMnonf3C",, drop=F] + 
          thetas[,"mean_v.nnMnonf3N",, drop=F] +
          thetas[,"mean_v.nnMnonf3C",, drop=F] +
          thetas[,"mean_v.ccMnonf3N",, drop=F])/4)
}

# Automation reduces quantity more effectively in control than PM blocks 
# (i.e., automation less effective under PM load)
mean.sd(samples, fun)
zandp(samples, fun)



# -------------------------------------------------------------------------

# Reactive control

samples[[1]]$p.names

# Overall reactive control (collapsed over automation success/failure/manual trials)
fun <- function (thetas) {
  (thetas[,"mean_v.ccAnonf3C",, drop=F] + 
     thetas[,"mean_v.ccAfail3C",, drop=F] + 
     thetas[,"mean_v.ccMnonf3C",, drop=F] + 
      thetas[,"mean_v.nnAnonf3N",, drop=F] +
     thetas[,"mean_v.nnAfail3N",, drop=F] +
     thetas[,"mean_v.nnMnonf3N",, drop=F])/6 -
    (thetas[,"mean_v.pcAnonf3C",, drop=F] + 
       thetas[,"mean_v.pcAfail3C",, drop=F] + 
       thetas[,"mean_v.pcMnonf3C",, drop=F] + 
       thetas[,"mean_v.pnAnonf3N",, drop=F] +
       thetas[,"mean_v.pnAfail3N",, drop=F] +
       thetas[,"mean_v.pnMnonf3N",, drop=F])/6
}

# Ongoing task rates lower with PM target present versus absent (in PM blocks)
mean.sd(samples, fun)
zandp(samples, fun)


# Reactive control on automation failure versus success trials
fun <- function (thetas) {
  ((thetas[,"mean_v.ccAfail3C",, drop=F] +  
     thetas[,"mean_v.nnAfail3N",, drop=F])/2 -
    (thetas[,"mean_v.pcAfail3C",, drop=F] + 
       thetas[,"mean_v.pnAfail3N",, drop=F])/2) -
    ((thetas[,"mean_v.ccAnonf3C",, drop=F] +  
        thetas[,"mean_v.nnAnonf3N",, drop=F])/2 -
       (thetas[,"mean_v.pcAnonf3C",, drop=F] + 
          thetas[,"mean_v.pnAnonf3N",, drop=F])/2)
}

# Reactive control stronger (more inhibition) on automation failure trials
# (which receive additional inhibition from the incongruous advice) than success trials
mean.sd(samples, fun)
zandp(samples, fun)


# Reactive control in manual vs automation successes
fun <- function (thetas) {
  ((thetas[,"mean_v.ccMnonf3C",, drop=F] +  
      thetas[,"mean_v.nnMnonf3N",, drop=F])/2 -
     (thetas[,"mean_v.pcMnonf3C",, drop=F] + 
        thetas[,"mean_v.pnMnonf3N",, drop=F])/2) -
    ((thetas[,"mean_v.ccAnonf3C",, drop=F] +  
        thetas[,"mean_v.nnAnonf3N",, drop=F])/2 -
       (thetas[,"mean_v.pcAnonf3C",, drop=F] + 
          thetas[,"mean_v.pnAnonf3N",, drop=F])/2)
}

# Reactive control stronger in manual blocks than with successful automation
# (i.e., congruent automation inputs counteract/weaken the effect of reactive control on
# ongoing task rates)
mean.sd(samples, fun)
zandp(samples, fun)



# Reactive control in manual vs automation failures
fun <- function (thetas) {
  ((thetas[,"mean_v.ccMnonf3C",, drop=F] +  
      thetas[,"mean_v.nnMnonf3N",, drop=F])/2 -
     (thetas[,"mean_v.pcMnonf3C",, drop=F] + 
        thetas[,"mean_v.pnMnonf3N",, drop=F])/2) -
    ((thetas[,"mean_v.ccAfail3C",, drop=F] +  
        thetas[,"mean_v.nnAfail3N",, drop=F])/2 -
       (thetas[,"mean_v.pcAfail3C",, drop=F] + 
          thetas[,"mean_v.pnAfail3N",, drop=F])/2)
}

# Reactive control additionally stronger on automation failure trials compared to manual
# (i.e., incongruent automation inputs boost/strengthen the effect of reactive control on
# ongoing task rates)
mean.sd(samples, fun)
zandp(samples, fun)


