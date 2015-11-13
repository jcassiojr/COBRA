# optimal cut point for ROC curve
# getting optimal cut-point (melhor balanço entre TPR = max and FPR = min)
f_opt_cut <- function(perf, pred) {
    cut.ind = mapply(FUN=function(x, y, p) {
        d = (x - 0)^2 + (y-1)^2
        ind = which(d == min(d))
        c(sensitivity = y[[ind]], specificity = 1-x[[ind]], 
          cutoff = p[[ind]])
    }, perf@x.values, perf@y.values, pred@cutoffs)
}