# teste de time series
library("dlm")
buildFun <- function(x) {
    dlmModPoly(1, dV = exp(x[1]), dW = exp(x[2])) 
}
fit <- dlmMLE(Nile, parm = c(0,0), build = buildFun)
fit$conv
dlmNile <- buildFun(fit$par)
V(dlmNile)
W(dlmNile)
StructTS(Nile, "level") # estimated variances
# erro na funcao abaixo
buildFun <- function(x) {
    m <- dlmModPoly(1, dV = exp(x[1]))
    m$JW <- matrix(1)
    m$X <- matrix(exp(x[2]), nc = 1, nr = length(Nile))
    j <- which(time(Nile) == 1899)
    m$X[j,1] <- m$X[j,1] * (1 + exp(x[3]))
    return(m)
    }
fit <- dlmMLE(Nile, parm = c(0,0,0), build = buildFun) > fit$conv
dlmNileJump <- buildFun(fit$par)
V(dlmNileJump)
dlmNileJump$X[c(1, which(time(Nile) == 1899)), 1]
# filtering
nileJumpFilt <- dlmFilter(Nile, dlmNileJump)
plot(Nile, type = 'o', col = "seagreen") 
lines(dropFirst(nileJumpFilt$m), type = 'o',pch = 20, col = "brown")
attach(nileJumpFilt)
v <- unlist(dlmSvd2var(U.C, D.C))
pl <- dropFirst(m) + qnorm(0.05, sd = sqrt(v[-1]))
pu <- dropFirst(m) + qnorm(0.95, sd = sqrt(v[-1]))
detach()
lines(pl, lty = 2, col = "brown")
lines(pu, lty = 2, col = "brown")
# smoothing
nileJumpSmooth <- dlmSmooth(nileJumpFilt) 
plot(Nile, type = 'o', col = "seagreen")
attach(nileJumpSmooth)
lines(dropFirst(s), type = 'o', pch = 20, col = "brown")
v <- unlist(dlmSvd2var(U.S, D.S))
pl <- dropFirst(s) + qnorm(0.05, sd = sqrt(v[-1]))
pu <- dropFirst(s) + qnorm(0.95, sd = sqrt(v[-1]))
detach()
lines(pl, lty = 2, col = "brown")
lines(pu, lty = 2, col = "brown")

# EXEMPLO DE CONSUMO DE GAS
lGas <- log(UKgas)
dlmGas <- dlmModPoly() + dlmModSeas(4)
buildFun <- function(x) {
    diag(W(dlmGas))[2:3] <- exp(x[1:2])
    V(dlmGas) <- exp(x[3])
    return(dlmGas)
    }
(fit <- dlmMLE(lGas, parm = rep(0, 3), build = buildFun))$conv
dlmGas <- buildFun(fit$par)
drop(V(dlmGas))
diag(W(dlmGas))[2:3]

gasSmooth <- dlmSmooth(lGas, mod = dlmGas)
x <- cbind(lGas, dropFirst(gasSmooth$s[,c(1,3)])) 
colnames(x) <- c("Gas", "Trend", "Seasonal")
plot(x, type = 'o', main = "UK Gas Consumption")
    