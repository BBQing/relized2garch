library(stealrugarch)
require(xts)
library(testthat)
data(spyreal)
RS <- list(RSVp =  spyreal[,2] * 100 * 0.45, RSVm = spyreal[,2] * 100 * 0.55)
spec = stealugarchspec(mean.model = list(armaOrder = c(0, 0), include.mean = FALSE), variance.model = list(model = 'realGARCH', garchOrder = c(2, 1)))
setbounds(spec)<-list(alpha2=c(-1,1))
spec
spec2
fit = stealugarchfit(spec, spyreal[, 1] * 100, solver = 'hybrid', realizedVol = spyreal[,2] * 100)
fit
spec2 = stealugarchspec(mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
                        variance.model = list(model = 'real2GARCH', garchOrder = c(4,1)))
fit2 = stealugarchfit(spec2, spyreal[, 1] * 100, solver = 'hybrid', realizedVol = RS)
fit3 = stealugarchfit(spec2, spyreal[, 1] * 100, solver = 'hybrid', realizedVol = RS2)
fit4 = stealugarchfit(spec2, spyreal[, 1] * 100, solver = 'hybrid', realizedVol = RS3)
fit4
class(spyreal)
class(spyreal[,2] * 100 * 0.45)
is.xts(spyreal[,2] * 100 * 0.45)
is.xts(RS$RSVp)
for(i in names(RS)){
  print(is.xts(RS[[i]]))

}


RS$names(RS)
RS2 <- data.frame(RSVp = spyreal[,2] * 100 * 0.45,RSVm = spyreal[,2] * 100 * 0.55)
head(RS2)
names(RS2) <- c("RSVp","RSVm")
class(RS2$RSVp)
RS2$RSVp
RS3 <- cbind(spyreal[,2] * 100 * 0.45,spyreal[,2] * 100 * 0.55)
names(RS3) <- c("RSVp","RSVm")
class(RS3)
names(RS3)
RS3[,"RSVp"]
head(RS2)
RS3$"RSVp"
a <-names(RS3)[1]
RS3$a
subset(RS3,select = "RSVp")
RS3[,'a'] <- subset(RS3,select = "RSVp")
names(RS3)
RS3
log(RS3)
RS3[,1:10]
colnames(RS3) <- c("RSVm","RSVp")
colnames(RS3)
rownames(RS3)
length(merge.xts(RS3, spyreal[,2] * 100 * 0.45))
colnames(colnames((merge.xts(RS3, spyreal[,2] * 100 * 0.45))<-c("RSVm","RSVp","Sqr") ))
?`$`
getElement(RS3,name = )
length(RS3)
length(index(RS3))
class(merge.xts(RS3, spyreal[,2] * 100 * 0.45))
dim(merge.xts(spyreal[,2] * 100 * 0.45,spyreal[,2] * 100 * 0.55))
dim(cbind(spyreal[,2] * 100 * 0.45,spyreal[,2] * 100 * 0.55))

length(spyreal[,2] * 100 * 0.45)

merge.xts()
logRS3 <-log(RS3)
class(logRS3)
subset(logRS3,is.numeric(RSVm))
subset(!is.finite(logRS3),RSVp = FALSE)



stealrugarch:::.arfimaxfilter()
fnc(20)
