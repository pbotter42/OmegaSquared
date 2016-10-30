# Compute Omega Squared using the F statistic 
# Intended for "oneway.test" objects 

# Note that while this calculation can be used for the 
# Welch ANOVA, it is recommended that more robust methodologies 
# are considered for effect size calculation (e.g. WRS2 package)

# Functions --------------------------------------------------------------------------
 
# Omega Squared

OmegaSquared <- function(x, n) {
  Fstat = x$statistic[["F"]]
  num_df <- x$parameter[["num df"]]
  numerator = num_df * (Fstat - 1)
  denominator =  (num_df * (Fstat - 1)) + n
  OS = numerator/denominator
  OS_round = round(OS, digits = 3)
  OS_named = setNames(OS_round, "Omega Squared")
  return(OS_named)
}

# Example ---------------------------------------------------------------------------

data(iris)

iris_n <- nrow(iris)
model <- oneway.test(Sepal.Length~Species, data = iris)

OmegaSquared(model, iris_n)