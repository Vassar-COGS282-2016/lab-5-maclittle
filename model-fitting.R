# Here's a (simulated) experiment, with a single subject and 500 categorization trials.
all.data <- read.csv('experiment-data.csv')
source('memory-limited-exemplar-model.R')
rm(sample.data.set)
rm(sample.training.data)

# Use optim() to fit the model to this data.
# Note: In optim() you can tell it to display updates as it goes with:
# optim( ... , control=list(trace=4))

opt.wrapper <- function(params) {
  exemplar.memory.log.likelihood(all.data, params[1], params[2])
}

fit <- optim(c(1, 5), opt.wrapper, method="Nelder-Mead", control=list(trace=4))
print(fit$value)

# Now try fitting a restricted version of the model, where we assume there is no decay.
# Fix the decay.rate parameter to 1, and use optim to fit the sensitivity parameter.
# Note that you will need to use method="Brent" in optim() instead of Nelder-Mead. 
# The brent method also requires an upper and lower boundary:
# optim( ..., upper=100, lower=0, method="Brent")

opt.wrapper.fixed <- function(params) {
  exemplar.memory.log.likelihood(all.data, params[1], 1)
}

fit.fixed <- optim(c(1), opt.wrapper.fixed, upper=100, lower=0, method="Brent", control=list(trace=4))
print(fit.fixed$value)

# What's the log likelihood of both models? (see the $value in the result of optiom(),
# remember this is the negative log likeihood, so multiply by -1.

# For the first model, the likelihood is 346.5736
# For the second, the likelihood is also 346.5736

# What's the AIC and BIC for both models? Which model should we prefer?

aic.one <- (2*2) - (2*log(346.5736))
print(aic.one)

n <- as.numeric(max(rownames(all.data)))
bic.one <- ((-2)*log(346.5736)) + (2*log(n))
print(bic.one)

aic.two <- (2*1) - (2*log(346.5736))
print(aic.two)

bic.two <- ((-2)*log(346.5736)) + (1*log(n))
print(bic.two)

# First model: AIC is 7.69619
#              BIC is 2.505951
# Second model: AIC is 9.69619
#               BIC is 7.101071

# We should prefer the second model, which has consistently higher AIC and BIC.

#### BONUS...
# If you complete this part I'll refund you a late day. You do not need to do this.

# Use parametric bootstrapping to estimate the uncertainty on the decay.rate parameter.
# Unfortunately the model takes too long to fit to generate a large bootstrapped sample in
# a reasonable time, so use a small sample size of 10-100 depending on how long you are
# willing to let your computer crunch the numbers.

# Steps for parametric bootstrapping:
# Use the best fitting parameters above to generate a new data set (in this case, that means
# a new set of values in the correct column for all.data).
# Fit the model to this new data, record the MLE for decay.rate.
# Repeat many times to get a distribution of decay.rate values.
# Usually you would then summarize with a 95% CI, but for our purposes you can just plot a
# histogram of the distribution.

