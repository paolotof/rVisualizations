# install lavaan if it isn't already installed
if(!require("lavaan")) install.packages("lavaan", repos="http://cran.rstudio.com/")

# make up some data with one mediator
set.seed(06052017)
X <- rnorm(100)
M <- 0.5*X + rnorm(100)
Y <- 0.7*M + rnorm(100)
Data <- data.frame(X = X, Y = Y, M = M)

simpleMediation <- '
	Y ~ b * M + c * X
	M ~ a * X
	indirect := a * b
	total    := c + (a * b)
'
require("lavaan")
fit <- sem(model = simpleMediation,	data  = Data)
summary(fit)

## multiple mediator example
# add a second mediator to the dataset
M2 <- -0.35*X + rnorm(100)
Y <- 0.7*M2 + 0.48*-M + rnorm(100)
Data <- data.frame(X = X, Y = Y, M1 = M, M2 = M2)
multipleMediation <- '
	Y ~ b1 * M1 + b2 * M2 + c * X
	M1 ~ a1 * X
	M2 ~ a2 * X
	indirect1 := a1 * b1
	indirect2 := a2 * b2
	contrast := indirect1 - indirect2
	total    := c + (a1 * b1) + (a1 * b1)
	M1 ~~ M2
'
fit <- sem(model = multipleMediation, data = Data)
summary(fit)
# obtaining more output
summary(fit, fit.measures=TRUE, standardize=TRUE, rsquare=TRUE,
 	estimates = TRUE, ci = TRUE)
# comparison of paths through model comparison with anova()
constrainedMediation <- '
	Y ~ b1 * M1 + b2 * M2 + c * X
	M1 ~ a1 * X
	M2 ~ a2 * X
	indirect1 := a1 * b1
	indirect2 := a2 * b2
	total    := c + (a1 * b1) + (a1 * b1)
	# covariances
	M1 ~~ M2
	# constrain
	indirect1 == indirect2
'
noConstrFit <- sem(model = multipleMediation, data = Data)
constrFit <- sem(model = constrainedMediation, data = Data)
anova(noConstrFit, constrFit)
# obtaining (5000) bootstrap estimates
fit <- sem(
	model = contrastsMediation,
	data  = Data,
 	se = "bootstrap",
 	bootstrap = 5000 # 1000 is the default
)
# OR
parameterEstimates(fit, boot.ci.type="bca.simple")
# OR
bootstrapLavaan(fit, R=5000, type="bollen.stine",
		FUN=fitMeasures, fit.measures="chisq")
