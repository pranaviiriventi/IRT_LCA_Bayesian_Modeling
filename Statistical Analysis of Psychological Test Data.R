# Library Imports
# Ensure libraries are installed and loaded properly
if (!require("tidyverse")) install.packages("tidyverse")
library(tidyverse)   # For data manipulation and visualization

if (!require("mirt")) install.packages("mirt")
library(mirt)        # For comprehensive IRT analyses

if (!require("cowplot")) install.packages("cowplot")
library(cowplot)     # To arrange ggplot figures

# Install and load ggmirt for extensions to mirt
if (!require("ggmirt")) devtools::install_github("masurp/ggmirt")
library(ggmirt)      # Load ggmirt


# Data Simulation
set.seed(42)  # Ensure reproducibility
# Simulate IRT data
d <- sim_irt(500, 10, discrimination = .25, seed = 42)
head(d)  # Display the first few rows of the dataset


# Model Fitting with 3PL
# Define a unidimensional model
unimodel <- 'F1 = 1-10'

# Fit a 3PL model
fit3PL <- mirt(data = d, model = unimodel, itemtype = "3PL", verbose = FALSE)

# Display model fit summary
summary(fit3PL)

# Extracting and Rounding Parameters
# Extract and round item parameters
params3PL <- coef(fit3PL, IRTpars = TRUE, simplify = TRUE)
round(params3PL$items, 2)  # Display rounded guessing parameters


# Model Diagnostics
# Fit indices and item fit statistics
fitIndices <- M2(fit3PL)
itemFitStats <- itemfit(fit3PL)
infitStats <- itemfit(fit3PL, fit_stats = "infit")

# Visualization of item fit
itemfitPlot(fit3PL)

# Person fit statistics and plots
personFitStats <- head(personfit(fit3PL))
personfitPlot(fit3PL)

# Item-person interaction map
itempersonMap(fit3PL)

# Trace plots for item characteristic curves
tracePlot(fit3PL, facet = F, legend = T) + scale_color_brewer(palette = "Set3")
tracePlot(fit3PL, items = c(1:3), facet = F, legend = T) + scale_color_brewer(palette = "Set2")

# Information plots
itemInfoPlot(fit3PL) + scale_color_brewer(palette = "Set3")
itemInfoPlot(fit3PL, facet = T)
testInfoPlot(fit3PL, adj_factor = 2)
scaleCharPlot(fit3PL)

# Score correlation
score <- fscores(fit3PL)
sumscore <- rowSums(d)
cor.test(score, sumscore)


# Comparing 2PL and Rasch Models
# Fit a 2PL model
fit2PL <- mirt(d, 1, itemtype = "2PL", verbose = FALSE)
anova(fit2PL, fit3PL)  # Compare 2PL and 3PL models

# Fit a Rasch model
fitRasch <- mirt(d, 1, itemtype = "Rasch", verbose = TRUE)
anova(fitRasch, fit2PL)  # Compare Rasch and 2PL models

# Trace plot for Rasch and 2PL
tracePlot(fit2PL, theta_range = c(-5, 5), facet = F, legend = T) +
  scale_color_brewer(palette = "Set3") +
  labs(title = "2PL - Traceplot")

tracePlot(fitRasch, theta_range = c(-5, 5), facet = F, legend = T) +
  scale_color_brewer(palette = "Set3") +
  labs(title = "Rasch - Traceplot")

itempersonMap(fitRasch)

# Latent Class Analysis (LCA)
# Ensure poLCA is installed and loaded
if (!requireNamespace("poLCA", quietly = TRUE)) install.packages("poLCA")
library(poLCA)

# Define a LCA model with 3 classes for a subset of items
lca_formula <- cbind(V1, V2, V3, V4, V5) ~ 1
lca_config <- poLCA(lca_formula, data = d, nclass = 3)

# Summary of the LCA model
summary(lca_config)

# Recode variables for LCA
d_recode <- d %>%
  mutate_at(vars(V1:V5), ~as.integer(factor(.)))

# Run the LCA model with the recoded dataset
lca_config_recode <- poLCA(lca_formula, data = d_recode, nclass = 3)
summary(lca_config_recode)

# Multidimensional IRT Models
multi_model <- 'F1 = V1-V5; F2 = V6-V10'
fitMIRT <- mirt(d, model = multi_model, itemtype = "2PL")
summary(fitMIRT)

# Bayesian IRT Models
# Ensure brms is installed and loaded
if (!require("brms")) install.packages("brms")
library(brms)

# Convert the dataset to long format for brms
d_long <- stack(d)

# Fit a Bayesian 1PL model using brms
bayes_1PL <- brm(values ~ 1 + (1|ind), data = d_long, family = bernoulli(), cores = 4)
summary(bayes_1PL)

# Check for convergence in the Bayesian model
if (bayes_1PL$Rhat > 1.1) {
  warning("Potential lack of convergence in Bayesian model, with Rhat > 1.1")
}

tracePlot(fit3PL, facet = F, legend = T) + 
  scale_color_brewer(palette = "Set3") +
  theme_minimal() +
  labs(title = "Item Characteristic Curves for 3PL Model",
       x = "Theta",
       y = "Probability of Correct Response",
       color = "Item")


