library(pryr)
library(gpuR)

# Initially points to same object
x = gpuMatrix(rnorm(1000000), 1000, 1000)
y = x


vclA = vclMatrix(rnorm(1000000), nrow = 1000, ncol = 1000)
vclB = vclMatrix(rnorm(1000000), nrow = 1000, ncol = 1000)
vclC = vclMatrix(rnorm(1000000), nrow = 1000, ncol = 1000)

# GEMM
vclD = vclA %*% vclB
system.time(vclA %*% vclB) 

# Element-wise addition
vclD = vclD + vclC
#vclD[]

A = matrix(rnorm(1000000), nrow = 1000, ncol = 1000)
B = matrix(rnorm(1000000), nrow = 1000, ncol = 1000)
system.time(A %*% B) 
