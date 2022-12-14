# Clear workspace
rm(list=ls())

getwd()
# Set working directory to top-level folder containing DMC
# setwd("~/AutoPM")
# setwd("D:/ATC experiments/Exp6 Auto-PM/Analysis/DMC")

# Load libraries and DMC functions
source("dmc/dmc.R")
load_model("LBA", "lbaN_B.R")


# Load data
print(load("Data/ATC_Auto-PM_clean.RData"))
dat <- cleandats[,c("s","S","auto","PM_block","failtrial","R","RT")]
names(dat)[c(3,4)] <- c("auto","PM")
levels(dat$R)
levels(dat$S)
head(dat)
str(dat)
# NB: RT has been truncated below 0.20s

# Rename PM factor levels
levels(dat$PM)
dat$PM <- factor(dat$PM, levels = c("control", "PM"), labels = c("2", "3"))

# Rename automation factor levels
levels(dat$auto)
dat$auto <- factor(dat$auto, levels = c("auto", "manual"), labels = c("A", "M"))

# Check factor levels
lapply(dat, levels)

# 3600 trials per subject
table(dat$s)

table(dat$auto, dat$failtrial)

# Create match maps -------------------------------------------------------

# expand.grid(list(S = c("cc","nn","pc","pn"),
#                  auto = c("A","M"),
#                  failtrial = c("nonf","fail"),
#                  PM = c("2","3"),
#                  R = c("C","N","P")))

# Mean v map
map_v <- empty.map(
  
  list(S = c("cc","nn","pc","pn"),
       auto = c("A","M"),
       failtrial = c("nonf","fail"),
       PM = c("2","3"),
       R = c("C","N","P")),
  
  levels = c("ccC","nnC",
             
             "pcC","pnC",
             
             "ccN","nnN",
             
             "pcN","pnN",
             
             "ppP",
             
             "PMFA",
             
             "FAKERATE"))
map_v
length(map_v)
length(levels(map_v))

map_v[1:96] <- c("ccC",      "nnC",      "FAKERATE", "FAKERATE",
                 "ccC",      "nnC",      "FAKERATE", "FAKERATE",
                 "ccC",      "nnC",      "FAKERATE", "FAKERATE",
                 "FAKERATE", "FAKERATE", "FAKERATE", "FAKERATE",
                 
                 "ccC",      "nnC",      "pcC",      "pnC",
                 "ccC",      "nnC",      "pcC",      "pnC",
                 "ccC",      "nnC",      "pcC",      "pnC",
                 "FAKERATE", "FAKERATE", "FAKERATE", "FAKERATE",
                 
                 "ccN",      "nnN",      "FAKERATE", "FAKERATE",
                 "ccN",      "nnN",      "FAKERATE", "FAKERATE",
                 "ccN",      "nnN",      "FAKERATE", "FAKERATE",
                 "FAKERATE", "FAKERATE", "FAKERATE", "FAKERATE",
                 
                 "ccN",      "nnN",      "pcN",      "pnN",
                 "ccN",      "nnN",      "pcN",      "pnN",
                 "ccN",      "nnN",      "pcN",      "pnN",
                 "FAKERATE", "FAKERATE", "FAKERATE", "FAKERATE",
                 
                 "FAKERATE", "FAKERATE", "FAKERATE", "FAKERATE",
                 "FAKERATE", "FAKERATE", "FAKERATE", "FAKERATE",
                 "FAKERATE", "FAKERATE", "FAKERATE", "FAKERATE",
                 "FAKERATE", "FAKERATE", "FAKERATE", "FAKERATE",
                 
                 "PMFA",     "PMFA",     "ppP",      "ppP",
                 "PMFA",     "PMFA",     "ppP",      "ppP",
                 "PMFA",     "PMFA",     "ppP",      "ppP",
                 "FAKERATE", "FAKERATE", "FAKERATE", "FAKERATE")
map_v



# SD v map
map_sdv <- empty.map(
  
  list(S = c("cc","nn","pc","pn"),
       auto = c("A","M"),
       failtrial = c("nonf","fail"),
       PM = c("2","3"),
       R = c("C","N","P")),
  
  levels = c("ccSDV","nnSDV","pcSDV","pnSDV",
             
             "FAKESDV"))
map_sdv
length(map_sdv)
length(levels(map_sdv))

map_sdv[1:96] <- c("ccSDV",   "nnSDV",   "FAKESDV", "FAKESDV",
                   "ccSDV",   "nnSDV",   "FAKESDV", "FAKESDV",
                   "ccSDV",   "nnSDV",   "FAKESDV", "FAKESDV",
                   "FAKESDV", "FAKESDV", "FAKESDV", "FAKESDV",
                   
                   "ccSDV",   "nnSDV",   "pcSDV",   "pnSDV",
                   "ccSDV",   "nnSDV",   "pcSDV",   "pnSDV",
                   "ccSDV",   "nnSDV",   "pcSDV",   "pnSDV",
                   "FAKESDV", "FAKESDV", "FAKESDV", "FAKESDV",
                   
                   "ccSDV",   "nnSDV",   "FAKESDV", "FAKESDV",
                   "ccSDV",   "nnSDV",   "FAKESDV", "FAKESDV",
                   "ccSDV",   "nnSDV",   "FAKESDV", "FAKESDV",
                   "FAKESDV", "FAKESDV", "FAKESDV", "FAKESDV",
                   
                   "ccSDV",   "nnSDV",   "pcSDV",   "pnSDV",
                   "ccSDV",   "nnSDV",   "pcSDV",   "pnSDV",
                   "ccSDV",   "nnSDV",   "pcSDV",   "pnSDV",
                   "FAKESDV", "FAKESDV", "FAKESDV", "FAKESDV",
                   
                   "FAKESDV", "FAKESDV", "FAKESDV", "FAKESDV",
                   "FAKESDV", "FAKESDV", "FAKESDV", "FAKESDV",
                   "FAKESDV", "FAKESDV", "FAKESDV", "FAKESDV",
                   "FAKESDV", "FAKESDV", "FAKESDV", "FAKESDV",
                   
                   "ccSDV",   "nnSDV",   "pcSDV",   "pnSDV",
                   "ccSDV",   "nnSDV",   "pcSDV",   "pnSDV",
                   "ccSDV",   "nnSDV",   "pcSDV",   "pnSDV",
                   "FAKESDV", "FAKESDV", "FAKESDV", "FAKESDV")
map_sdv



# Threshold map
map_B <- empty.map(
  
  list(S = c("cc","nn","pc","pn"),
       auto = c("A","M"),
       failtrial = c("nonf","fail"),
       PM = c("2","3"),
       R = c("C","N","P")),
  
  levels = c("A2C",
             "M2C",
             "A3C",
             "M3C",
             
             "A2N",
             "M2N",
             "A3N",
             "M3N",
             
             "A3P",
             "M3P",
             
             "FAKEB"))
map_B
length(map_B)
length(levels(map_B))

map_B[1:96] <- c("A2C",  "A2C",  "FAKEB","FAKEB",
                 "M2C",  "M2C",  "FAKEB","FAKEB",
                 "A2C",  "A2C",  "FAKEB","FAKEB",
                 "FAKEB","FAKEB","FAKEB","FAKEB",
                 
                 "A3C",  "A3C",  "A3C",  "A3C",
                 "M3C",  "M3C",  "M3C",  "M3C",
                 "A3C",  "A3C",  "A3C",  "A3C",
                 "FAKEB","FAKEB","FAKEB","FAKEB",
                 
                 "A2N",  "A2N",  "FAKEB","FAKEB",
                 "M2N",  "M2N",  "FAKEB","FAKEB",
                 "A2N",  "A2N",  "FAKEB","FAKEB",
                 "FAKEB","FAKEB","FAKEB","FAKEB",
                 
                 "A3N",  "A3N",  "A3N",  "A3N",
                 "M3N",  "M3N",  "M3N",  "M3N",
                 "A3N",  "A3N",  "A3N",  "A3N",
                 "FAKEB","FAKEB","FAKEB","FAKEB",
                 
                 "FAKEB","FAKEB","FAKEB","FAKEB",
                 "FAKEB","FAKEB","FAKEB","FAKEB",
                 "FAKEB","FAKEB","FAKEB","FAKEB",
                 "FAKEB","FAKEB","FAKEB","FAKEB",
                 
                 "A3P",  "A3P",  "A3P",  "A3P",
                 "M3P",  "M3P",  "M3P",  "M3P",
                 "A3P",  "A3P",  "A3P",  "A3P",
                 "FAKEB","FAKEB","FAKEB","FAKEB")
map_B



# Build model -------------------------------------------------------------

model <- model.dmc(
  p.map = list(
    A = "1",
    B = "MAPB",
    t0 = "1",
    mean_v = "MAPV",
    sd_v = "MAPSDV",
    st0 = "1",
    N = "PM"), 
  match.map = list(
    M = list(
      cc = "C", 
      nn = "N", 
      pc = "P", 
      pn = "P"),
    MAPSDV = map_sdv,
    MAPB = map_B,
    MAPV = map_v),
  factors = list(
    S = c("cc", "nn", "pc", "pn"),
    auto = c("A", "M"),
    failtrial = c("nonf", "fail"),
    PM = c("2", "3")),
  constants = c(N.2 = 2, N.3 = 3, 
                st0 = 0,
                sd_v.pnSDV = 0.5,
                sd_v.FAKESDV = 0,
                B.FAKEB = Inf,
                mean_v.FAKERATE = 1), 
  responses = c("C","N","P"),
  type = "normN")

length(attr(model, "p.vector"))


# Create parameter vector
p.vector <- c(t0 = 0.3, A = 1.5,
              
              B.A2C = 2,  B.M2C = 2,  B.A3C = 2,  B.M3C = 2,  
              B.A2N = 2,  B.M2N = 2,  B.A3N = 2,  B.M3N = 2, 
              B.A3P = 2,  B.M3P = 2, 
              
              mean_v.ccC = 1, mean_v.nnC = 0, 
              mean_v.pcC = 0, mean_v.pnC = 0, 
              
              mean_v.ccN = 0, mean_v.nnN = 1, 
              mean_v.pcN = 0, mean_v.pnN = 0, 
              
              mean_v.ppP = 1, mean_v.PMFA = 0,
              
              sd_v.ccSDV = 0.5, sd_v.nnSDV = 0.5, sd_v.pcSDV = 0.5)

length(p.vector)

# Check parameter vector matches model
check.p.vector(p.vector, model)

# Check model simulates
# head(simulate.dmc(p.vector, model), 10)

# Set priors
p.prior <- prior.p.dmc(
  dists = c("beta", rep("tnorm", length(p.vector)-1)),
  p1 = p.vector,                           
  p2 = c(0.2, 0.1, rep(1, 10), rep(1, 10), rep(1, 3)), 
  lower = c(0.1, 0, rep(0, 10), rep(-Inf, 10), rep(0, 3)),
  upper = c(1, 10, rep(Inf, 10), rep(Inf, 10), rep(Inf, 3))
)
length(p.prior)
length(p.vector)

# Plot priors
# par(mfcol = c(2, 2)); for (i in names(p.prior)) plot.prior(i, p.prior)
# plot.prior("A", p.prior)


# Make data model
dm <- data.model.dmc(dat, model)
save(dm, file = "samples/dmAutoPM_B_sdvS.RData")


# Initialize samples object
# 75 chains
n.chains <- length(p.prior) * 3

# Generate start points for fixed effects model
samples <- h.samples.dmc(nmc = 100, 
                         p.prior = p.prior, 
                         data = dm, 
                         thin = 10, 
                         n.chains = n.chains)

# Save
save(samples, file = "samples/sAutoPM_B_sdvS.RData")

# Load
print(load("samples/sAutoPM_B_sdvS.RData"))

# -------------------------------------------------------------------------
# 
# # Generate start points for hierarchical model
# # Hyper-level
# hstart <- make.hstart(samples)
# 
# # Subject-level
# theta <- make.theta1(samples)
# 
# 
# # Hyper-level priors
# # Mu
# mu.prior <- prior.p.dmc(
#   dists = rep("tnorm", 64),
#   p1 = c(t0 = 1, p.vector[-1]),
#   p2 = c(1, 1, rep(1, 12), rep(1.5, 37)), 
#   lower = c(0.1, 0, rep(0, 12), rep(NA, 37)),
#   upper = c(1, 10, rep(Inf, length(p.vector)-2))
# )
# 
# # Sigma
# sigma.prior <- prior.p.dmc(
#   dists = c(rep("gamma", 51)), 
#   p1 = c(1, 1, rep(1, 12), rep(1.5, 37)),                          
#   p2 = c(rep(1, 51))
# )
# 
# # Create hyper prior object
# pp.prior <- list(mu.prior, sigma.prior)
# 
# # Initialize samples object
# n.chains <- length(p.prior) * 3
# 
# # Generate start points for hierarchical model
# hsamples <- h.samples.dmc(nmc = 100, 
#                           p.prior = p.prior, 
#                           data = dm, 
#                           pp.prior = pp.prior, 
#                           thin = 10,
#                           hstart.prior = hstart, 
#                           theta1 = theta, 
#                           n.chains = n.chains
# )
# 
# # Save
# save(hsamples, file = "samples/hsAutoPM_B_sdvS.RData")