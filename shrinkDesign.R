#' Generate shrinked DoEs for each clique from a FANOVA Graph. 
#' 
#' @param fun 
#' black-box function to evaluate
#' @param cliques
#' list of vectors of indices representing cliques after applying threshold to the FANOVA Graph
#' @param initial.design 
#' numerical matrix containing the initial design of experiments used to evaluate fun
#' @param initial.values
#' matrix consisting of only one column as response of fun 
#' @param n.new
#' integer value setting the reduced number of runs in new DoEs
#' 
#' @return list of two lists, one containing the new DoEs the 
#' other containing the responses for each clique

shrinkDesign = function(fun, cliques, initial.design, initial.values, n.new){

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
  
  ### reduce number of runs to n.new for each clique
  ### maximize avg min distance in designs by leaving out runs stepwise
  initial.values.mod = initial.values[-c] 
  while(nrow(new.designs[[1]]) > n.new-1){
    minimum.distances.mean = sapply(1:(nrow(new.designs[[1]])-1), function(run){
      mean(sapply(new.designs, function(x){min(dist(x[-run,]))}))
    })
  maximum = which.max(minimum.distances.mean)
  initial.values.mod = initial.values.mod[-maximum]
  new.designs = lapply(new.designs, function(x){x[-maximum, ]})
  }

  ### responses for every clique but one by evaluating fun
  responses = lapply(new.designs[1:(length(new.designs)-1)], fun)  
  ### response for left out clique without evaluating fun
  sums = rowSums(data.frame(responses))
  responses[[n.cl]] = matrix(initial.values.mod + (n.cl-1)* initial.values[c] - sums)

  ### add run c
  new.designs = lapply(new.designs, function(x){rbind(x, initial.design[c,])})
  responses = lapply(responses, function(x){rbind(x, initial.values[c])})

  return(list(designs = new.designs, responses = responses))

}