library(ParamHelpers)
library(BBmisc)
library(checkmate)
library(rgenoud)
library(fields)


addUncertainty = function(fun, cliques, initial.design, initial.values){
  n = nrow(initial.design)
  p = ncol(initial.design)
  n.cl = length(cliques)
  c = which.min(initial.values)
  ### design for each clique (run c left out, already evaluated)
  new.designs = lapply(
    cliques, function(x, des){
      des[, x] = matrix(rep(initial.design[c, x],each = n-1),
                        nrow = n-1, byrow = FALSE)
      return(des)
    },
    des = initial.design[-c,]
  )
  initial.values.mod = initial.values[-c]
  ### points to add uncertainty
  na.vec = sample(1:n.cl ,n-1, replace = TRUE)
  ### combine new designs to one matrix
  designs = do.call(rbind, new.designs)
  ### leave out points to add uncertainty
  points = seq(1, n.cl*(n-1),by = n.cl) + (na.vec-1)
  designs.certain = designs[-points, ]
  ### fun evaluation
  responses = fun(designs.certain)
  ### matrix of responses (one clique per column)
  responses = matrix(responses, byrow = FALSE, ncol = n.cl-1)
  responses = cbind(responses, 0)
  ### fixed value for epsilon to test implementation
  # epsilon = 0.01 not here
  ### computing uncertain points and shifting matrix elements to correct positions
  for (i in 1:(n-1)){
    if (na.vec[i]< n.cl){
      responses[i, na.vec[i]:n.cl] = c(initial.values.mod[i] + (n.cl-1)* initial.values[c] - sum(responses[i,]) , responses[i, na.vec[i]:(n.cl-1)])
    }
    else{
      responses[i, n.cl] = initial.values.mod[i] + (n.cl-1)* initial.values[c] - sum(responses[i,]) 
    }
  }
  diagonal.elements = unlist(lapply(na.vec, function(x){vec = rep(0, n.cl); vec[x] = 1; return(vec)})) # for noise.var
  
  #par.set = makeParamSet(makeNumericVectorParam("theta", len = p, lower = 0), makeNumericParam("alpha", lower = 0, upper = 1))
  par.set = makeParamSet(makeNumericVectorParam("theta", len = p, lower = 0))
  model = partialNuggetModel(x = designs, y = as.vector(responses), par.set = par.set, dist2cov = "matern5_2", genoud.params = NULL, diagonal.elements = diagonal.elements)
  
  nugget = model$nugget.effect
  

#   for (i in 1:(n-1)){
#     responses[i, na.vec[i]] = responses[i, na.vec[i]] + nugget
#   }
  
  ### list of responses
  responses = split(responses, col(responses))
  ### if matrix-style output is desired
  responses = lapply(responses, as.matrix)
  ### add run c
  new.designs = lapply(new.designs, function(x){rbind(x, initial.design[c,])})
  responses = lapply(responses, function(x){rbind(x, initial.values[c])})
  return(list(designs = new.designs, responses = responses, nugget = nugget, diagonal.elements = diagonal.elements))
} 


partialNuggetModel = function(x, y, par.set, dist2cov = "matern5_2", nugget.estim = FALSE,
                         genoud.params = NULL, diagonal.elements = NULL) {
  
  # arg checks
  #assert(checkDataFrame(x, any.missing = FALSE), checkMatrix(x, any.missing = FALSE))
  if (is.matrix(x))
    x = as.data.frame(x)
  n = nrow(x)
  p = ncol(x)
  #assertNumeric(y, len = n, any.missing = FALSE)
  #assertClass(par.set, "ParamSet")
  #assertChoice(dist2cov, c("gauss", "matern3_2", "matern5_2"))
  genoud.defs = list(pop.size = 100, max.generations = 12, wait.generations = 5, boundary.enforcement = 2)
  if (is.null(genoud.params)) {
    genoud.params = genoud.defs
  } else {
    checkList(genoud.params, unique = TRUE)
    genoud.params = insert(genoud.defs, genoud.params)
  }
  
  
 neglik = function(z, get.covmat = FALSE) {
    theta = z[1:p]
    # alpha is z[p + 1] when nugget.estim = TRUE, otherwise 1 (inactive)
    alpha = na.omit(c(z[p + 1], 1L))[1L]
    # covmat = calcGowerCovMat(par.set = par.set, data.x = x, theta = theta, dist2cov = dist2cov)
    covmat = Matern(as.matrix(dist(x)), smoothness = 2.5)
    covmat = alpha * covmat + (1 - alpha) * diag(diagonal.elements) # diag(1, nrow = nrow(x))
    # Concentrated loglik (see Dice Kriging paper (2012))
    B = matrix(data = 1, ncol = 1L, nrow = n)
    a = t(solve(covmat, B))                         # B^T R^-1
    beta = 1 / (a %*% B) * (a %*% y)
    b = solve(covmat, (y - beta))                   # R^-1 (y - beta)
    sigma2 = 1 / n * t(y - beta) %*% b
    # get intercept, sigma^2 and covariance matrix for a specific theta
    if(get.covmat) return(list(cov.mat = covmat, intercept = beta, sigma = sigma2))
    
    d1 = n / 2 * log(sigma2 * 2 * pi)
    d2 = 1 / 2 * (2 * sum(log(diag(chol(covmat)))))#log(prod(diag(chol(covmat))^2))    # 0.5 * log(det(R))
    #2 * sum(log(diag(T)))
    d3 = n / 2 
    return(d1 + d2 + d3)
  }
  
  # optimize the likelihood function ==> estimate the parameters
  # FIXME: this should be done by a PH function
  #discrete = getParamTypes(par.set, use.names = TRUE) == "discrete"
  #discrete = names(discrete)[which(discrete)]
  #domains = getDomains(x, discrete)
  
  lower = rep(1e-10, p)
  upper = 2 * (apply(X = x, MARGIN = 2, FUN = max) - apply(X = x, MARGIN = 2, FUN = min))
  domains = cbind(lower, upper)
  domains = rbind(domains, c(1e-8, 1 - 1e-8))
  
  genoud.nvar = p + 1
    
  genoud.params = c(list(fn = neglik, nvars = genoud.nvar, Domains = domains), genoud.params)
  opt.res = do.call(rgenoud::genoud, genoud.params)
  
  # generate output:
  cov.params = opt.res$par
  # intercept, variance, covariance matrix estimates:
  interc.var.covmat = neglik(cov.params, TRUE)
  # alpha - see Dice Kriging paper (2012). Either estimated (cov.params[p + 1]) or 1 (inactive)
  alpha = na.omit(c(cov.params[p + 1], 1L))[1L]
  nugget.effect = (1 - alpha) * interc.var.covmat$sigma[1L, ]
  #cov.mat = calcGowerCovMat(par.set = par.set, data.x = x, theta = cov.params, dist2cov = dist2cov)
  
  makeS3Obj("partialNuggetModel",
            x = x,
            y = y,
            nugget.effect = nugget.effect,
            dist2cov = dist2cov,
            intercept = interc.var.covmat$intercept[1L, ],
            variance = interc.var.covmat$sigma[1L, ],
            par.set = par.set,
            cov.mat = interc.var.covmat$cov.mat,
            cov.params = cov.params[1:p]
  )
}

