source("shrinkDesign.R")
source("addUncertainty.R")

### example based on "ExampleIshigami"-Demo in fanovaGraph
library(fanovaGraph)

### definition of the underlying function:

d = 3
domain = c(-pi, pi)

fun = ishigami.fun


### maximin design via package 'lhs'

data(L)
x = -pi*L[,1:3]

### (black-box) function values

y = fun(x)

### kriging model via package 'DiceKriging'

KM = km(~1, design = data.frame(x), response = y)

### estimation of total interaction indices via fixing method

g = estimateGraph(f.mat = kmPredictWrapper, d = d, n.tot = 30000, q.arg = 
                     list(min = domain[1], max = domain[2]), km.object = KM) 

### threshold inactive edges and plot graph

g.cut = threshold(g, delta = 0.03, scale = TRUE)
plot(g.cut)

### estimate new model

Cliques = g.cut$cliques

### compute shrinked design and responses for each clique


result = shrinkDesign(fun = fun, cliques = Cliques, initial.design = x,
                      initial.values = y, n.new = 20)

### comparison of computed response and real evaluation
all.equal(as.matrix(fun(result$designs[[2]])), result$responses[[2]])


### addUncertainty (provisional version) very time-consuming beacuse of genetic optimization algorithm

result2 = addUncertainty(fun = fun, cliques = Cliques, initial.design = x,
                        initial.values = y)

# Wed Sep 09 14:03:36 2015
# Domains:
#   1.000000e-10   <=  X1   <=    1.248854e+01 
# 1.000000e-10   <=  X2   <=    1.246393e+01 
# 1.000000e-10   <=  X3   <=    1.249526e+01 
# 1.000000e-08   <=  X4   <=    1.000000e+00 
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
# Fitness value... 1.572831e+03
# mean............ 1.788423e+03
# variance........ 6.519480e+03
# #unique......... 100, #Total UniqueCount: 100
# var 1:
#   best............ 5.541824e+00
# mean............ 6.520622e+00
# variance........ 1.207145e+01
# var 2:
#   best............ 7.387199e+00
# mean............ 5.699038e+00
# variance........ 1.150021e+01
# var 3:
#   best............ 1.126848e+01
# mean............ 6.243857e+00
# variance........ 1.212331e+01
# var 4:
#   best............ 9.916157e-01
# mean............ 5.427355e-01
# variance........ 7.014721e-02
# 
# GENERATION: 1
# Fitness value... 1.347642e+03
# mean............ 1.635460e+03
# variance........ 7.133220e+03
# #unique......... 70, #Total UniqueCount: 170
# var 1:
#   best............ 5.541824e+00
# mean............ 5.476854e+00
# variance........ 7.322761e+00
# var 2:
#   best............ 7.387199e+00
# mean............ 6.850445e+00
# variance........ 5.856765e+00
# var 3:
#   best............ 1.126848e+01
# mean............ 8.173626e+00
# variance........ 1.390442e+01
# var 4:
#   best............ 1.000000e+00
# mean............ 9.048861e-01
# variance........ 2.064912e-02
# 
# GENERATION: 2
# Fitness value... 1.347642e+03
# mean............ 1.523329e+03
# variance........ 1.494651e+04
# #unique......... 68, #Total UniqueCount: 238
# var 1:
#   best............ 5.541824e+00
# mean............ 5.700847e+00
# variance........ 1.511944e+00
# var 2:
#   best............ 7.387199e+00
# mean............ 7.297365e+00
# variance........ 7.535447e-01
# var 3:
#   best............ 1.126848e+01
# mean............ 1.098223e+01
# variance........ 2.263035e+00
# var 4:
#   best............ 1.000000e+00
# mean............ 9.573305e-01
# variance........ 1.961783e-02
# 
# GENERATION: 3
# Fitness value... 1.347642e+03
# mean............ 1.411034e+03
# variance........ 1.621709e+04
# #unique......... 59, #Total UniqueCount: 297
# var 1:
#   best............ 5.541824e+00
# mean............ 5.489120e+00
# variance........ 1.455527e+00
# var 2:
#   best............ 7.387199e+00
# mean............ 7.281588e+00
# variance........ 1.757825e+00
# var 3:
#   best............ 1.126848e+01
# mean............ 1.097685e+01
# variance........ 1.380024e+00
# var 4:
#   best............ 1.000000e+00
# mean............ 9.750055e-01
# variance........ 1.155680e-02
# 
# GENERATION: 4
# Fitness value... 1.347642e+03
# mean............ 1.406442e+03
# variance........ 1.723531e+04
# #unique......... 57, #Total UniqueCount: 354
# var 1:
#   best............ 5.541824e+00
# mean............ 5.439483e+00
# variance........ 1.508055e+00
# var 2:
#   best............ 7.387199e+00
# mean............ 6.882247e+00
# variance........ 2.639399e+00
# var 3:
#   best............ 1.126848e+01
# mean............ 1.089038e+01
# variance........ 1.658621e+00
# var 4:
#   best............ 1.000000e+00
# mean............ 9.679693e-01
# variance........ 1.887237e-02
# 
# GENERATION: 5
# Fitness value... 1.347642e+03
# mean............ 1.397292e+03
# variance........ 1.444436e+04
# #unique......... 63, #Total UniqueCount: 417
# var 1:
#   best............ 5.541824e+00
# mean............ 5.613110e+00
# variance........ 1.413427e+00
# var 2:
#   best............ 7.387199e+00
# mean............ 6.555082e+00
# variance........ 3.117409e+00
# var 3:
#   best............ 1.126848e+01
# mean............ 1.083587e+01
# variance........ 2.369154e+00
# var 4:
#   best............ 1.000000e+00
# mean............ 9.715787e-01
# variance........ 1.333711e-02
# 
# GENERATION: 6
# Fitness value... 1.347642e+03
# mean............ 1.382348e+03
# variance........ 8.851921e+03
# #unique......... 59, #Total UniqueCount: 476
# var 1:
#   best............ 5.541824e+00
# mean............ 5.637526e+00
# variance........ 1.143052e+00
# var 2:
#   best............ 7.387199e+00
# mean............ 6.800494e+00
# variance........ 3.540116e+00
# var 3:
#   best............ 1.126848e+01
# mean............ 1.089467e+01
# variance........ 2.526657e+00
# var 4:
#   best............ 1.000000e+00
# mean............ 9.896484e-01
# variance........ 3.298192e-03
# 
# GENERATION: 7
# Fitness value... 1.347642e+03
# mean............ 1.378604e+03
# variance........ 9.047656e+03
# #unique......... 55, #Total UniqueCount: 531
# var 1:
#   best............ 5.541824e+00
# mean............ 5.682610e+00
# variance........ 8.410368e-01
# var 2:
#   best............ 7.387199e+00
# mean............ 6.741538e+00
# variance........ 2.630000e+00
# var 3:
#   best............ 1.126848e+01
# mean............ 1.083467e+01
# variance........ 2.716102e+00
# var 4:
#   best............ 1.000000e+00
# mean............ 9.869317e-01
# variance........ 3.600027e-03
# 
# 'wait.generations' limit reached.
# No significant improvement in 5 generations.
# 
# Solution Fitness Value: 1.347642e+03
# 
# Parameters at the Solution (parameter, gradient):
#   
#   X[ 1] :	5.541824e+00	G[ 1] :	0.000000e+00
# X[ 2] :	7.387199e+00	G[ 2] :	0.000000e+00
# X[ 3] :	1.126848e+01	G[ 3] :	0.000000e+00
# X[ 4] :	1.000000e+00	G[ 4] :	0.000000e+00
# 
# Solution Found Generation 1
# Number of Generations Run 7
# 
# Wed Sep 09 14:04:07 2015
# Total run time : 0 hours 0 minutes and 31 seconds

result2$nugget
# 3.699946

### not working
km(~1, design = data.frame(result2$designs[[1]]), response = result2$responses[[1]])
### Error in t.default(T) : argument is not a matrix
km(~1, design = data.frame(result2$designs[[1]]), response = result2$responses[[1]], noise.var = result2$diagonal.elements[[1]] * result2$nugget)
### Error in t.default(T) : argument is not a matrix