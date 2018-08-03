#2a

generate_data <- function(n,p) {
  covariates <-(matrix(runif(n*p), n, p))
  responses <- (as.vector(runif(n)))
  list(covariates,responses)
}

#2b

model_select <- function(covariates, responses, cutoff) {
  lm(responses[which(covariates <= cutoff)]~covariates[which(covariates <= cutoff)])
}

