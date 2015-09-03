source("shrinkDesign.R")
source("addUncertainty.R")
### example based on "Example6D"-Demo in fanovaGraph
require(fanovaGraph)

### definition of the underlying function:

d <- 6
domain <- c(-1, 1)

fun <- function(x) {
  beta <- c(-0.8, -1.1, 1.1, 1)
  gamma <- c(-0.5, 0.9, 1, -1.1)
  result <- cos(cbind(1, x[, c(1, 5, 3)]) %*% beta) 
            + sin(cbind(1, x[, c(4, 2, 6)]) %*% gamma)
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

### compute shrinked design and responses for each clique


result = shrinkDesign(fun = fun, cliques = Cliques, initial.design = x,
                      initial.values = y, n.new = 20)

### comparison of computed response and real evaluation
all.equal(fun(result$designs[[2]]), result$responses[[2]])


### addUncertainty (provisional version) very time-consuming beacuse of genetic optimization algorithm
result2 = addUncertainty(fun = fun, cliques = Cliques, initial.design = x,
                         initial.values = y)
# ...
#
# NOTE: HARD MAXIMUM GENERATION LIMIT HIT
# At least one gradient is too large
# 
# Solution Fitness Value: 1.294542e+03
# 
# Parameters at the Solution (parameter, gradient):
#   
# X[ 1] :  2.879180e-01	G[ 1] :	0.000000e+00
# X[ 2] :	2.846102e+00	G[ 2] :	0.000000e+00
# X[ 3] :	2.786924e+00	G[ 3] :	0.000000e+00
# X[ 4] :	3.208513e+00	G[ 4] :	nan
# X[ 5] :	3.624421e+00	G[ 5] :	nan
# X[ 6] :	2.826873e+00	G[ 6] :	0.000000e+00
# X[ 7] :	9.991840e-01	G[ 7] :	6.476051e-01
# 
# Solution Found Generation 2
# Number of Generations Run 12
# 
# Thu Sep 03 16:38:45 2015
# Total run time : 0 hours 16 minutes and 37 seconds
# Warning message:
#   In (function (fn, nvars, max = FALSE, pop.size = 1000, max.generations = 100,  :
#                   Stopped because hard maximum generation limit was hit.
#                 At least one gradient is too large.
#
# ...

result2$nugget
# 3.592826