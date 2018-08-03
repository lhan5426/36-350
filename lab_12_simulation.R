generate_data = function(n,p) {
  covariates <-(matrix(runif(n*p), n, p))
  responses <- (as.vector(runif(n)))
  list(covariates,responses)
}
