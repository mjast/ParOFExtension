shrinkDesign = function(fun, cliques, initial.design, initial.values, n.new){

  n = nrow(initial.design)
  n.cl = length(cliques)
  c = which.min(initial.values)

### one design for each clique
new.designs = lapply(cliques, function(x, des){des[, x] = matrix(rep(initial.design[c, x], each = n-1), nrow = n-1, byrow = FALSE); return(des)}, des = initial.design[-c,])
  
### reduce number of runs to n.new for each clique
### space filling measure? max avg min distance in designs
initial.values.mod = initial.values[-c] 
while(nrow(new.designs[[1]]) > n.new-1){
  minimum.distances.mean = sapply(1:(nrow(new.designs[[1]])-1), function(run){
    mean(sapply(new.designs, function(x){min(dist(x[-run,]))}))
  })
  maximum = which.max(minimum.distances.mean)
  initial.values.mod = initial.values.mod[-maximum]
  new.designs = lapply(new.designs, function(x){x[-maximum, ]} )
}

### responses for every clique but one from fun
responses = lapply(new.designs[1:(length(new.designs)-1)], fun)  
### responses for last clique without using fun
responses[[n.cl]] = matrix(initial.values.mod + (n.cl-1)* initial.values[c] - rowSums(data.frame(responses)))

### add run c
new.designs = lapply(new.designs, function(x){rbind(x, initial.design[c,])})
responses = lapply(responses, function(x){rbind(x, initial.values[c])})

return(list(designs = new.designs, responses = responses))

}