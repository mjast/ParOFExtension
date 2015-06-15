shrinkDesign = function(cliques, initial.design, initial.values, n.new){

###   
  c = which.min(initial.values)
  design = t(apply(initial.design[-c,], 1, "-", initial.design[c,]))
  new.designs = lapply(cliques, function(x, des){des[, x] = 0; return(des)}, des = design)
  
### space filling measure? maximin
while(nrow(new.designs[[1]]) > n.new-1){
  minimum.distances.mean = sapply(1:(nrow(new.designs[[1]])-1), function(point){
    mean(sapply(new.designs, function(x){min(dist(x[-point,]))}))
  })
maximum = which.max(minimum.distances.mean)
new.designs = lapply(new.designs, function(x){x[-maximum, ]} )
}

### split up design for computing new points
cut(1:(n.new-1), breaks = length(cliques))



lapply(new.designs, function(x){rbind(x,0)})

#mindist = lapply(new.designs, function(x){
#  apply(new.designs[[1]][-c,], 1, function(z){ # ensure c is not removed
#    min(dist(new.designs[[1]][-z,]))
#  })
#})