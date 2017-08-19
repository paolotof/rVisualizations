# source(file = "~/github/rVisualizations/multipleMediatorExtractOutput.R")
load(file = "~/Dropbox/mPlusTests/modelFit.RData")
ls()
if(!require("lavaan")) install.packages("lavaan",
    repos="http://cran.rstudio.com/", dependencies=TRUE)
summary(fit)

dat <- read.delim2('~/Dropbox/mPlusTests/PerfectMediation2.dat', as.is = TRUE)
names(dat) <- c('fear', 'hope', 'diet', 'bmi', 'age', 'pd', 'ps', 'Porder')
sapply(dat, function(x) sum(is.na(x)))
dim(dat)
dat <- na.omit(dat)
dim(dat)

summary(fit, fit.measures=TRUE,
  	standardize=TRUE,
  	rsquare=TRUE,
  	estimates = TRUE,
  	ci = TRUE)

tmp <- parameterEstimates(fit, standardized=TRUE, ci=TRUE,
                          boot.ci.type="bca.simple")

data.frame(round(tmp[,8:10], 2),
	ciSig = ((tmp[,9] * tmp[,10]) > 0),
	pSign = (tmp[,8] < .05),
	ciVsP = ((tmp[,9] * tmp[,10]) > 0) == (tmp[,8] < .05))

tableValues = data.frame(tmp[ ,1:3], round(tmp[,c(5,9:10)], 2),
	ciSig = ifelse((tmp[,9] * tmp[,10]) > 0, '*', ''))
tableValues$ciSig[tmp$op == '~~'] = ''

if(!require("xtable")) install.packages("xtable",
    repos="http://cran.rstudio.com/", dependencies=TRUE)
xtable(tableValues,	caption="Multiple-mediator analysis example.")

cat('\\documentclass{article}\n', file = "table.tex")
cat('\\begin{document}\n', file = "table.tex", append = TRUE)
resultsTable <- xtable(tableValues,
	caption="Multiple-mediator analysis.")
print(resultsTable, file="table.tex", append = TRUE)
cat('\\end{document}', file = "table.tex", append = TRUE)
