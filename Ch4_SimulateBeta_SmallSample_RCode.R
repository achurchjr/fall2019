##
## ## Ch3_SimulateBeta_RCode.R
##
##


## Load data; data saved as object named "dta"

#load(file = "C:/Users/baileyma/Documents/Textbook/OUP/Supplementals_Jan2019/ProblemSets_R/Ch3_Exercise1_Incumbent_vote.RData")
dta$RDI = c(3.46910, 1.70638, 2.95381, 0.55880, 5.63215, 3.48730, 3.66471, 2.08994, -0.44449, 5.96776, 3.74859, 2.90608, 1.98308, 3.86820, 2.63631, 0.59109, 2.40738, 0.69215)


## Set model and simulation parameters
Obs       = 18		# Number of observations in each simulation
Reps      = 5000		# Number of times we run the simulation
TrueBeta0	= 48	# "True" beta0 for the simulated
TrueBeta1	= 0	# "True" beta1 for the simulated
SD 		    = 4	# The standard deviation of the error. The bigger this is, the larger the average value of epsilon
CoefMatrix	= matrix(NA, Reps, 2)	# Matrix to store our results.  
SEMatrix    = matrix(NA, Reps, 2)   # Create a matrix to store our results.  
  # 1st argument is NA, meaning we store "not available" as initial values in the matrix
  # 2nd argument is Reps, meaning the number of rows is equal to number of times we run the simulations
  # 3rd argument is 2 meaning we have 2 columns, one for storing the beta0 estimate and one for storing the beta1 estimate

# Loop: repeat the commands between the brackets multiple times
for (ii in 1:Reps) { 
  Y.sim 	= TrueBeta0+ TrueBeta1*dta$RDI + SD*rnorm(Obs) 	
  # Generate Salary = beta0 + beta1*Ed + epsilon
  # beta0 is the constant
  # beta1 is the number multiplied by the X variable
  # Epsilon has 2 parts: SD is the standard deviation; the bigger it is, the more epsilon varies.
  # "runif" is a uniform random variable between 0 and 1, with all values having equal probability
  OLS.result = lm(Y.sim ~ dta$RDI)               # Run a regression using simulated values of Y
  CoefMatrix[ii, ]	= coefficients(OLS.result)	 # Put OLS.result coefficients in row ii of CoefMatrix
  SEMatrix[ii, ]  = coef(summary(OLS.result))[, "Std. Error"] 
  # Put std errors from OLS.result in row ii of SEMatrix
  ## For fun: plot results for each survey
  ## plot(Ed, Salary, pch = 19, col= "darkgreen")		
  ## abline(OLS.result, lwd = 3, col= "darkgreen")
  ## Sys.sleep(0.075)		## Include to slow down calculations so we can see each plot (briefly); not necessary
}							 # This closes the "loop"


T.matrix = CoefMatrix/SEMatrix
## Minimum t-stats for both coefficients (b0, b1)
c(min(T.matrix[, 1]),  min(T.matrix[, 2]) )

## Maximum t-stats for both coefficients (b0, b1)
c(max(T.matrix[, 1]),  max(T.matrix[, 2]) )
## [1] 14.41  7.09
plot(density(T.matrix[, 2]), main = "Distribution of t-stats for beta1 estimates")
lines(density(rnorm(1000)), col = "red")


#
# Distribution of coefficient estimates
#

hist(T.matrix[,2], main = 'Kernel Density Estimate')

# Kernel Density Plot
plot(density(T.matrix[,2]), main = 'Kernel Density Estimate')
## Standard normal distribution (red line on figure)
  x.temp = seq(-4, 4, by = 0.1)
  dnorm(x.temp)
  lines(x.temp, dnorm(x.temp), col = "red")  
## For alpha = 0.05, one-tailed
abline(v = 1.64, lty = 3, col = "grey") # Vertical line
abline(v = quantile(sort(T.matrix[,2]), 0.95), lty = 1, col = "darkblue") # Verticle line
print(c("One-tailed critical value for alpha = 0.05 and normal distribution is ", qnorm(0.95)))
print(c("One-tailed critical value for alpha = 0.05 and t distribution with 16 d.f. is ", qt(1-0.05, 16)))
print(c("95% of t-stats are bigger than ", quantile(sort(T.matrix[,2]), 0.95)))


## For alpha = 0.01, one-tailed
#abline(v = 1.96, lty = 3, col = "grey") # Vertical line
#abline(v = quantile(sort(T.matrix[,2]), 0.99), lty = 1, col = "darkblue") # Verticle line
