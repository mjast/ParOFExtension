source("shrinkDesign.R")
source("addUncertainty.R")

### example based on "Example6D"-Demo in fanovaGraph
library(fanovaGraph)

### definition of the underlying function:

d = 6
domain = c(-1, 1)

fun = function(x) {
  beta = c(-0.8, -1.1, 1.1, 1)
  gamma = c(-0.5, 0.9, 1, -1.1)
  result = cos(cbind(1, x[, c(1, 5, 3)]) %*% beta) 
            + sin(cbind(1, x[, c(4, 2, 6)]) %*% gamma)
  return(result)
}


### maximin design via package 'lhs'

data(L)
x = L

### (black-box) function values

y = fun(x)

### kriging model via package 'DiceKriging'

KM = km(~1, design = data.frame(x), response = y)

### estimation of total interaction indices via fixing method

g = estimateGraph(f.mat = kmPredictWrapper, d = d, n.tot = 30000, q.arg = 
                     list(min = domain[1], max = domain[2]), km.object = KM) 

### threshold inactive edges and plot graph

g.cut = threshold(g, delta = 0.01, scale = TRUE)
plot(g.cut)

### estimate new model

Cliques = g.cut$cliques

### compute shrinked design and responses for each clique


result = shrinkDesign(fun = fun, cliques = Cliques, initial.design = x,
                      initial.values = y, n.new = 20)

### comparison of computed response and real evaluation
all.equal(fun(result$designs[[2]]), result$responses[[2]])


### addUncertainty (provisional version) very time-consuming beacuse of genetic optimization algorithm

# result2 = addUncertainty(fun = fun, cliques = Cliques, initial.design = x,
#                          initial.values = y)
# Wed Sep 09 11:10:56 2015
# Domains:
#   1.000000e-10   <=  X1   <=    3.975224e+00 
# 1.000000e-10   <=  X2   <=    3.967393e+00 
# 1.000000e-10   <=  X3   <=    3.977366e+00 
# 1.000000e-10   <=  X4   <=    3.946013e+00 
# 1.000000e-10   <=  X5   <=    3.935601e+00 
# 1.000000e-10   <=  X6   <=    3.983994e+00 
# 1.000000e-08   <=  X7   <=    1.000000e+00 
# 
# Data Type: Floating Point
# Operators (code number, name, population) 
# (1) Cloning........................... 	15
# (2) Uniform Mutation.................. 	12
# (3) Boundary Mutation................. 	12
# (4) Non-Uniform Mutation.............. 	12
# (5) Polytope Crossover................ 	12
# (6) Simple Crossover.................. 	12
# (7) Whole Non-Uniform Mutation........ 	12
# (8) Heuristic Crossover............... 	12
# (9) Local-Minimum Crossover........... 	0
# 
# HARD Maximum Number of Generations: 12
# Maximum Nonchanging Generations: 5
# Population size       : 100
# Convergence Tolerance: 1.000000e-03
# 
# Using the BFGS Derivative Based Optimizer on the Best Individual Each Generation.
# Checking Gradients before Stopping.
# Not Using Out of Bounds Individuals and Not Allowing Trespassing.
# 
# Minimization Problem.
# GENERATION: 0 (initializing the population)
# Fitness value... 2.208561e+03
# mean............ 2.553946e+03
# variance........ 6.099376e+03
# #unique......... 100, #Total UniqueCount: 100
# var 1:
#   best............ 2.593664e+00
# mean............ 1.994317e+00
# variance........ 1.490608e+00
# var 2:
#   best............ 1.610852e+00
# mean............ 2.129391e+00
# variance........ 1.341718e+00
# var 3:
#   best............ 2.119373e+00
# mean............ 1.688947e+00
# variance........ 1.421736e+00
# var 4:
#   best............ 5.653348e-01
# mean............ 1.870641e+00
# variance........ 1.113832e+00
# var 5:
#   best............ 4.552013e-01
# mean............ 2.135965e+00
# variance........ 1.176490e+00
# var 6:
#   best............ 1.632898e+00
# mean............ 2.065864e+00
# variance........ 1.105075e+00
# var 7:
#   best............ 9.991401e-01
# mean............ 4.837713e-01
# variance........ 7.515206e-02
# 
# GENERATION: 1
# Fitness value... 1.774978e+03
# mean............ 2.360734e+03
# variance........ 1.538829e+04
# #unique......... 76, #Total UniqueCount: 176
# var 1:
#   best............ 2.595000e+00
# mean............ 2.025973e+00
# variance........ 7.838548e-01
# var 2:
#   best............ 1.592819e+00
# mean............ 1.687170e+00
# variance........ 1.054473e+00
# var 3:
#   best............ 2.136189e+00
# mean............ 2.026349e+00
# variance........ 1.178836e+00
# var 4:
#   best............ 5.419129e-01
# mean............ 1.224347e+00
# variance........ 9.397616e-01
# var 5:
#   best............ 4.586553e-01
# mean............ 8.802844e-01
# variance........ 8.634770e-01
# var 6:
#   best............ 1.635353e+00
# mean............ 1.917801e+00
# variance........ 5.823167e-01
# var 7:
#   best............ 1.000000e+00
# mean............ 9.263670e-01
# variance........ 6.957531e-03
# 
# GENERATION: 2
# Fitness value... 1.774978e+03
# mean............ 2.142269e+03
# variance........ 2.989773e+04
# #unique......... 60, #Total UniqueCount: 236
# var 1:
#   best............ 2.595000e+00
# mean............ 2.545538e+00
# variance........ 8.435389e-02
# var 2:
#   best............ 1.592819e+00
# mean............ 1.693762e+00
# variance........ 2.146937e-01
# var 3:
#   best............ 2.136189e+00
# mean............ 2.100809e+00
# variance........ 6.186262e-02
# var 4:
#   best............ 5.419129e-01
# mean............ 5.996654e-01
# variance........ 9.978496e-02
# var 5:
#   best............ 4.586553e-01
# mean............ 5.265053e-01
# variance........ 7.645452e-02
# var 6:
#   best............ 1.635353e+00
# mean............ 1.658050e+00
# variance........ 9.385026e-02
# var 7:
#   best............ 1.000000e+00
# mean............ 9.875691e-01
# variance........ 1.690494e-03
# 
# GENERATION: 3
# Fitness value... 1.774978e+03
# mean............ 1.903669e+03
# variance........ 6.128888e+04
# #unique......... 71, #Total UniqueCount: 307
# var 1:
#   best............ 2.595000e+00
# mean............ 2.557314e+00
# variance........ 4.366996e-02
# var 2:
#   best............ 1.592819e+00
# mean............ 1.568811e+00
# variance........ 2.259391e-01
# var 3:
#   best............ 2.136189e+00
# mean............ 2.101644e+00
# variance........ 1.287423e-01
# var 4:
#   best............ 5.419129e-01
# mean............ 5.773002e-01
# variance........ 3.480214e-02
# var 5:
#   best............ 4.586553e-01
# mean............ 5.656168e-01
# variance........ 1.308815e-01
# var 6:
#   best............ 1.635353e+00
# mean............ 1.703793e+00
# variance........ 1.062260e-01
# var 7:
#   best............ 1.000000e+00
# mean............ 9.686757e-01
# variance........ 1.480581e-02
# 
# GENERATION: 4
# Fitness value... 1.774978e+03
# mean............ 1.847758e+03
# variance........ 4.469708e+04
# #unique......... 67, #Total UniqueCount: 374
# var 1:
#   best............ 2.595000e+00
# mean............ 2.523455e+00
# variance........ 9.061796e-02
# var 2:
#   best............ 1.592819e+00
# mean............ 1.561050e+00
# variance........ 2.175498e-01
# var 3:
#   best............ 2.136189e+00
# mean............ 2.138717e+00
# variance........ 1.320554e-01
# var 4:
#   best............ 5.419129e-01
# mean............ 5.717601e-01
# variance........ 5.169354e-02
# var 5:
#   best............ 4.586553e-01
# mean............ 6.137220e-01
# variance........ 2.158122e-01
# var 6:
#   best............ 1.602258e+00
# mean............ 1.730038e+00
# variance........ 1.267838e-01
# var 7:
#   best............ 1.000000e+00
# mean............ 9.740201e-01
# variance........ 1.384998e-02
# 
# GENERATION: 5
# Fitness value... 1.774978e+03
# mean............ 1.860929e+03
# variance........ 4.681286e+04
# #unique......... 66, #Total UniqueCount: 440
# var 1:
#   best............ 2.595000e+00
# mean............ 2.493798e+00
# variance........ 1.043616e-01
# var 2:
#   best............ 1.592819e+00
# mean............ 1.494919e+00
# variance........ 3.102498e-01
# var 3:
#   best............ 2.136189e+00
# mean............ 2.199162e+00
# variance........ 1.689590e-01
# var 4:
#   best............ 5.419129e-01
# mean............ 5.399758e-01
# variance........ 2.531071e-03
# var 5:
#   best............ 4.586553e-01
# mean............ 6.152082e-01
# variance........ 1.948393e-01
# var 6:
#   best............ 1.602258e+00
# mean............ 1.873460e+00
# variance........ 2.015827e-01
# var 7:
#   best............ 1.000000e+00
# mean............ 9.836327e-01
# variance........ 7.173093e-03
# 
# GENERATION: 6
# Fitness value... 1.774978e+03
# mean............ 1.842332e+03
# variance........ 3.553003e+04
# #unique......... 64, #Total UniqueCount: 504
# var 1:
#   best............ 2.538416e+00
# mean............ 2.525160e+00
# variance........ 6.934279e-02
# var 2:
#   best............ 1.469857e+00
# mean............ 1.571532e+00
# variance........ 3.016951e-01
# var 3:
#   best............ 2.224111e+00
# mean............ 2.194503e+00
# variance........ 1.531624e-01
# var 4:
#   best............ 5.419129e-01
# mean............ 5.540816e-01
# variance........ 4.550167e-02
# var 5:
#   best............ 5.174133e-01
# mean............ 5.988571e-01
# variance........ 1.835780e-01
# var 6:
#   best............ 1.673623e+00
# mean............ 1.821700e+00
# variance........ 1.328276e-01
# var 7:
#   best............ 1.000000e+00
# mean............ 9.920564e-01
# variance........ 9.689292e-04
# 
# GENERATION: 7
# Fitness value... 1.774978e+03
# mean............ 1.843281e+03
# variance........ 3.461266e+04
# #unique......... 65, #Total UniqueCount: 569
# var 1:
#   best............ 2.784493e+00
# mean............ 2.515187e+00
# variance........ 1.059677e-01
# var 2:
#   best............ 2.004604e+00
# mean............ 1.600837e+00
# variance........ 2.285066e-01
# var 3:
#   best............ 1.841748e+00
# mean............ 2.143966e+00
# variance........ 8.379601e-02
# var 4:
#   best............ 5.419129e-01
# mean............ 5.785999e-01
# variance........ 5.333056e-02
# var 5:
#   best............ 2.618810e-01
# mean............ 5.575399e-01
# variance........ 1.385410e-01
# var 6:
#   best............ 1.363264e+00
# mean............ 1.767144e+00
# variance........ 2.033988e-01
# var 7:
#   best............ 1.000000e+00
# mean............ 9.867399e-01
# variance........ 8.636347e-03
# 
# 'wait.generations' limit reached.
# No significant improvement in 5 generations.
# 
# Solution Fitness Value: 1.774978e+03
# 
# Parameters at the Solution (parameter, gradient):
#   
#   X[ 1] :	2.784493e+00	G[ 1] :	0.000000e+00
# X[ 2] :	2.004604e+00	G[ 2] :	0.000000e+00
# X[ 3] :	1.841748e+00	G[ 3] :	0.000000e+00
# X[ 4] :	5.419129e-01	G[ 4] :	0.000000e+00
# X[ 5] :	2.618810e-01	G[ 5] :	0.000000e+00
# X[ 6] :	1.363264e+00	G[ 6] :	0.000000e+00
# X[ 7] :	1.000000e+00	G[ 7] :	0.000000e+00
# 
# Solution Found Generation 1
# Number of Generations Run 7
# 
# Wed Sep 09 11:19:48 2015
# Total run time : 0 hours 8 minutes and 52 seconds

load("result2.RData")

result2$nugget
# 13.26516

### not working
km(~1, design = data.frame(result2$designs[[1]]), response = result2$responses[[1]])
### Error in t.default(T) : argument is not a matrix

km(~1, design = data.frame(result2$designs[[1]]), response = result2$responses[[1]], noise.var = result2$diagonal.elements[[1]] * result2$nugget)
### Error in t.default(T) : argument is not a matrix

