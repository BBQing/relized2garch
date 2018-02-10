###
# stealrugarch test
###
## Rule #1
require(stealrugarch)
data(sp500ret)
# create a cluster object to be used as part of this demonstration
cluster = makePSOCKcluster(15)


###
# rule 2#

spec = stealugarchspec()
show(spec)

##rule #3

row(expand.grid(GARCH = 1:14, VEX = 0:1, VT = 0:1, Mean = 0:1, ARCHM = 0:2, ARFIMA = 0:1, MEX = 0:1, DISTR = 1:10))


spec = stealugarchspec(variance.model = list(model = 'eGARCH', garchOrder = c(2, 1)), distribution = 'std')
rugarch::setstart(spec) <- list(shape = 5)
rugarch::setbounds(spec)

spec@model$modeldesc$vmodel
