addUncertainty = function(fun, cliques, initial.design, initial.values){
  
  n = nrow(initial.design)
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
  designs = designs[-points, ] 
  
  ### fun evaluation
  responses = fun(designs)
  
  ### matrix of responses (one clique per column)
  responses = matrix(responses, byrow = FALSE, ncol = n.cl-1)
  responses = cbind(responses, 0)
  
  ### fixed value for epsilon to test implementation
  epsilon = 0.01
  
  ### computing uncertain points and shifting matrix elements to correct positions
  for (i in 1:(n-1)){
    if (na.vec[i]< n.cl){
      responses[i, na.vec[i]:n.cl] = c(initial.values.mod[i] + (n.cl-1)* initial.values[c] - sum(responses[i,]) + epsilon , responses[i, na.vec[i]:(n.cl-1)])
    }
    else{
      responses[i, n.cl] = initial.values.mod[i] + (n.cl-1)* initial.values[c] - sum(responses[i,]) + epsilon
    }
  }
  ### list of responses 
  responses = split(responses, col(responses))
  ### if matrix-style output is desired
  responses = lapply(responses, as.matrix)
  
  ### add run c
  new.designs = lapply(new.designs, function(x){rbind(x, initial.design[c,])})
  responses = lapply(responses, function(x){rbind(x, initial.values[c])})
  
  return(list(designs = new.designs, responses = responses))
  
}  
  
  
  
  
  
  

  