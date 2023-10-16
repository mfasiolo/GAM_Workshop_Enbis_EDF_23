
########################################
# [1] Modelling the GEFCom14 electricity demand data
########################################

# Load data and have a look at it:
library(testGam)
data("gefcom_small")
head(gefcom_small)   # wM_s95[t] = 0.95 * wM_s95[t-1] + 0.05 * wM[t]  

plot(gefcom_small$NetDemand)

# We divide into a train and a testing data set:
train <- gefcom_small[gefcom_small$Year < 2011, ]
test <- gefcom_small[gefcom_small$Year == 2011, ]

# Fit a basic Gaussian GAM:
library(mgcv)
fit1 <- gam(formula = NetDemand ~ NetDemand.24 + Dow + Trend + 
                                  wM + wM_s95 + s(Posan, bs = "cc"), 
            data = train)

# Visualise the seasonal effect with mgcViz:
library(mgcViz)
fit1 <- getViz(fit1, nsim = 100)
plot(fit1)

# Visualise all terms:
print(plot(fit1, allTerms = TRUE), pages = 1)

# Should we model temperature non-linearly? 
check1D(fit1, "wM") + l_gridCheck1D()

# It seems so. Update the model:
fit2 <- gamV(formula = NetDemand ~ NetDemand.24 + Dow + Trend + 
                       s(wM) + s(wM_s95) + s(Posan, bs = "cc"), 
             data = train, 
             aViz = list(nsim = 100))

check1D(fit2, "wM") + l_gridCheck1D()

# Get some diagnostics on the last model:
check(fit2)

# We might need more basis functions for `wM`:
fit3 <- gamV(formula = NetDemand ~ NetDemand.24 + Dow + Trend + 
                       s(wM, k = 15) + s(wM_s95) + s(Posan, bs = "cc"), 
             data = train, 
             aViz = list(nsim = 100))

AIC(fit1, fit2, fit3)

# Get p-values etc:
summary(fit3)

# Making predictions:
preds <- predict(fit3, newdata = test)
head(preds)

# Plot the fit:
plot(test$NetDemand)
lines(preds, col = 2)


########################################
# [2] Modelling the GEFCom14 data using bam()
########################################

library(testGam)
library(mgcViz)
data("gefcom_big")

head(gefcom_big)
nrow(gefcom_big) / nrow(gefcom_small)

# Suppose we want to use this model formula:
form <- NetDemand ~ Dow +
               s(NetDemand.24) +
               s(Trend, k = 6) + 
               s(wM) + 
               s(Instant) +
               s(wM_s95) + 
               s(Posan, bs='cc', k=30) +
               ti(NetDemand.24, Instant) +
               ti(wM, Instant, k = c(10, 10)) +
               ti(wM_s95, Instant, k = c(15, 15)) +
               ti(Posan, Instant, k = c(10, 15))

# Fit larger model with `gam` and `bam`:

###### DO NOT RUN!!!!!
# fit_g <- gam(formula = form, data = gefcom_big) # Takes ~ 3 min
# fit_b <- bam(formula = form, data = gefcom_big) # Takes ~ 1 min
#####

# Use discrete approximation:
fit_bd <- bam(formula = form, data = gefcom_big, discrete = TRUE) # Takes ~ 3 sec

# Compatible with `mgcViz`:
fit_bd <- getViz(fit_bd)

print(plot(fit_bd), pages = 2)

# Wood et al. (2017) use discretization fit model with n = 10^8 and p = 10^4, see.
#
# For details see:
# 
#   - Li, Z. and S. N. Wood (2019). Faster model matrix crossproducts for large generalized
# linear models with discretized covariates. Statistics and Computing , 1–7.
# 
#   - Wood, S. N., Y. Goude, and S. Shaw (2015). Generalized additive models for large data
# sets. Journal of the Royal Statistical Society: Series C (Applied Statistics) 64 (1),
# 139–155.
# 
#   - Wood, S. N., Z. Li, G. Shaddick, and N. H. Augustin (2017). Generalized additive
# models for gigadata: modeling the uk black smoke network daily data. Journal of the
# American Statistical Association 112 (519), 1199–1210.
# 
