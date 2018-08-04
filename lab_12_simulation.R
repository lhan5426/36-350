
wage.df = read.csv("https://raw.githubusercontent.com/linnylin92/36-350_public/master/dat/wage.csv", skip = 15)


#2a

generate_data <- function(n,p) {
  covariates <-(matrix(runif(n*p),n,p))
  responses <- (as.vector(runif(n)))
  list(covariates,responses)
}


#2b

model_select <- function(covariates, responses, cutoff) {
  print(summary(lm(responses~covariates)))
  coeffcol = summary(lm(responses~covariates))$coefficients[,4]
  result =(summary(lm(responses~covariates))$coef[summary(lm(responses~covariates))$coef[,4] <= cutoff])
  result
}

model_select(matrix(c(1,2,3,4,5,6),2,3),c(9,68),200)

run_simulation <- function(n_trials, n, p, cutoff) {
  for (j in 1:3) {
    n = n[j]
    p = p[j]
    for (i in 1:n_trials) {
      list1 = generate_data(n,p)
      print(model_select(list1[[1]],list1[[2]],cutoff))
    }
  }
}


