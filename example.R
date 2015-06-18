### example based on "Example6D"-Demo in fanovaGraph
require(fanovaGraph)

### definition of the underlying function:

d <- 6
domain <- c(-1, 1)

fun <- function(x) {
  beta <- c(-0.8, -1.1, 1.1, 1)
  gamma <- c(-0.5, 0.9, 1, -1.1)
  result <- cos(cbind(1, x[, c(1, 5, 3)]) %*% beta) + sin(cbind(1, 
                                                                x[, c(4, 2, 6)]) %*% gamma)
  return(result)
}


### maximin design via package 'lhs'

data(L)
x<-L

### (black-box) function values

y <- fun(x)

### kriging model via package 'DiceKriging'

KM <- km(~1, design = data.frame(x), response = y)

### estimation of total interaction indices via fixing method

g <- estimateGraph(f.mat = kmPredictWrapper, d = d, n.tot = 30000, q.arg = 
                     list(min = domain[1], max = domain[2]), km.object = KM) 

### threshold inactive edges and plot graph

g.cut <- threshold(g, delta = 0.01, scale = TRUE)
plot(g.cut)

### estimate new model

Cliques <- g.cut$cliques

### copmpute shrinked design and responses for each clique

source("shrinkDesign.R")
result = shrinkDesign(fun = fun, cliques = Cliques, initial.design = x, initial.values = y, n.new = 20)

all.equal(fun(result$designs[[2]]), result$responses[[2]])
