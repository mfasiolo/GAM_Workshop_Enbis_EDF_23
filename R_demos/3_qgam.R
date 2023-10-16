######################################################
###### [1] Quantile modelling: the motorcycle data set
######################################################

# Let us consider the motorcycle data set:
library(MASS)
data(mcycle)
plot(mcycle)

# We fit a quantile GAM for quantile 0.9
library(qgam)
library(mgcViz)
fit09 <- qgam(accel ~ s(times, k=20, bs="ad"), 
              data = mcycle, 
              qu = 0.9)

# We now plot the quantile with confidence intervals
pred <- predict(fit09, se = TRUE)

plot(mcycle, ylim = c(-140, 85))
lines(mcycle$times, pred$fit)
lines(mcycle$times, pred$fit + 2 * pred$se, col = 2)
lines(mcycle$times, pred$fit - 2 * pred$se, col = 2)

# There are two problems with this fit: 
# * the confidence intervals have nearly constant width;
# * the estimated quantile is far above the data for `times < 15`.

# We address these issues by modelling the learning rate as well:
fit09_2 <- qgam(list(accel ~ s(times, k=20, bs="ad"),
                           ~ s(times)),
                 data = mcycle, 
                 qu = 0.9)

# Again predict and plot
pred <- predict(fit09_2, se = TRUE)

plot(mcycle, ylim = c(-140, 90))
lines(mcycle$times, pred$fit)
lines(mcycle$times, pred$fit + 2 * pred$se, col = 2)
lines(mcycle$times, pred$fit - 2 * pred$se, col = 2)

# Looks much better! Recall that 
class(fit09_2)

# hence we can do, for instance 
summary(fit09_2)

# We can use `mgcViz` plots by converting as usual
fit09_2 <- getViz(fit09_2, nsim = 100) # Note nsim does not make sense here! 

# or using shortcut
fit09_2 <- qgamV(list(accel ~ s(times, k=20, bs="ad"),                             ~ s(times)),
                 data = mcycle,
                 qu = 0.9)

# We fit multiple quantiles using the `mqgam` function
fitMQ <- mqgam(list(accel ~ s(times, k=20, bs="ad"),
                          ~ s(times)),
               data = mcycle, 
               qu = c(0.1, 0.25, 0.5, 0.75, 0.9)) 

# This should be manipulated using the `qdo` function
qdo(fitMQ, 0.1, summary)

# To plot one of the fitted quantile models we do
qdo(fitMQ, 0.1, plot)

# To plot effect of several quantile at once we use `mgcViz`
fitMQ_viz <- getViz(fitMQ)
plot(fitMQ_viz)

# We can plot the fitted quantile on the data
plot(mcycle, ylim = c(-150, 90))
for(ii in 1:5){
  lines(mcycle$times, predict(fitMQ_viz[[ii]]), col = 2)
}

# NB output of `getViz` is simply a list of `gam` models, 
# we don't need to use `qdo`. 
# But there is a memory price to pay (small for this small data set)
format(object.size(fitMQ), units = "MB")

format(object.size(fitMQ_viz), units = "MB")


######################################################
###### [2] UK load modelling example
######################################################

# Load data
library(mgcViz);
data("UKload")

head(UKload)

# Fit basic QGAM and plot smooths:
qu <- 0.5
form <- NetDemand~s(wM) + s(wM_s95) + 
        s(Posan, k = 30, bs = "ad") + Dow + s(Trend,k=4) + 
        NetDemand.48 + Holy
fit <- qgamV(form = form, data = UKload, qu = qu)

# 
print(plot(fit), pages = 1)

check1D(fit, x = "Posan") + l_gridCheck1D(mad)

# Add adaptive smooth for Posan and re-plot:
form <- list(NetDemand ~ s(wM) + s(wM_s95) + s(Posan, bs='ad', k=30) + 
                         Dow + s(Trend,k=4) + NetDemand.48 + Holy, 
                       ~ s(Posan)) 

# Fit five quantiles
nqu <- 5
qus <- seq(0.1, 0.9, length.out = nqu)
fitM <- mqgamV(form = form, data = UKload, qu = qus)

print(plot(fitM), pages = 1)

# We can also look at the parametric effects:
print(plot(fitM, allTerms = T, select = 5:7), pages = 1)
# It is interesting to notice that the holiday effect is stronger 
# (more negative) on the low quantiles.  

# We can check the fit:
pl <- check1D(fitM[[3]], 
              x = list("Dow", "wM", "wM_s95", "Posan")) + l_gridQCheck1D(qu = qus[3]) 
print(pl, pages = 1)

