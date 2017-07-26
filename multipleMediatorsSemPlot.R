# source('multipleMediatorsSemPlot.R')
dat <- read.delim2('~/Dropbox/mPlusTests/PerfectMediation2.dat', as.is = TRUE)
names(dat) <- c('fear', 'hope', 'diet', 'bmi', 'age', 'pd', 'ps', 'Porder')
myModel <- readLines("~/Dropbox/mPlusTests/multipleMediatorCoded_simple.lav")
require("lavaan")
set.seed(42)
fit <- sem(myModel,
    data=dat,
    se = "bootstrap",
    bootstrap = 5000)
summary(fit, fit.measures=TRUE,
  	standardize=TRUE,
  	rsquare=TRUE,
  	estimates = TRUE,
  	ci = TRUE)
save(fit, file = "~/Dropbox/mPlusTests/modelFit.RData")
load(file = "~/Dropbox/mPlusTests/modelFit.RData")
require("semPlot")
pdf(file = 'models.pdf')
par(mfrow = c(1, 2))
fit <- sem(myModel, data=Data)
semPaths(fit, "model", "est", layout = 'spring',
	node.label.cex=5, edge.label.cex=1.25, fade=FALSE)
title("non-standardized coefficients")
fit <- sem(myModel, data=Data, std.lv=TRUE)
semPaths(fit, "model", "std", layout = 'spring',
	node.label.cex=5, edge.label.cex=1.25, fade=FALSE)
title("Standardized coefficients")
par(mfrow = c(1,1))
dev.off()
