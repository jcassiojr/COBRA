# testes
# produto cartesiano
library(data.table)
A <- c(1,2,3)
B <- factor(c('x','y'))
C <- c(0.1,0.5)
d <- CJ(x = A, y = B, z = C)
d[,w:=f(x,y,z)]

f <- function(x,y,z) paste(x,y,z,sep = "+")

d <- CJ(x = A, y = B, z = C)[,w:=f(x, y, z)]
