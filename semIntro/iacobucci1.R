print("Import data")
#rm(list = ls()) # uncomment to clear all
# to run all the script at once copy line below from after '#' and paste to console 
# source('iacobucci1.R') # DO NOT UNCOMMENT, will make your file recursive and
# you will end up with a infite loop.
covMat <- read.table('iacobucci2.txt')

require(lavaan)
costumerQuality <- lav_matrix_lower2full(t(covMat))
rownames(costumerQuality) <- colnames(costumerQuality) <- c('q1', 'q2', 'q3', 
  'c1', 'c2', 'c3', 'v1', 'v2', 'v3', 'cs1', 'cs2', 'cs3', 'r1', 'r2', 'r3')
n <- 100 # number of respondents

print("Measurement model")

mdlStx <- "
quality =~ q1 + q2+ q3
cost =~ c1 + c2 + c3
value =~ v1 + v2 + v3
csat =~ cs1 + cs2 + cs3
repeaT =~ r1 + r2 + r3"

mdlFit <- cfa(mdlStx, sample.cov=costumerQuality, sample.nobs=n)

print("Fit measurement model")
iacobucciOutput <- function(mdlFit){
# this is a function wrapping the commands to print the output a la Iacobucci
# it's only function is to avoid typing the same commands 3 times
	print(paste(
		"chi^2_{", fitMeasures(mdlFit, "df"), 
		"} =", format(fitMeasures(mdlFit, "chisq"), nsmall = 2, digits = 2), 
		", (p =", format(fitMeasures(mdlFit, "pvalue"), digits = 1), 
		"), CFI =", format(fitMeasures(mdlFit, "cfi"), digits = 2), 
		", SRMR =", format(fitMeasures(mdlFit, "srmr"), digits = 2)
	))
}
iacobucciOutput(mdlFit)

print("Factor loadings and significance")
estVals <- parameterEstimates(mdlFit, standardized = TRUE)
estVals[estVals$op == "=~", "est"]
estVals[estVals$op == "=~", "pvalue"]

na.omit(estVals[estVals$op == "=~", "pvalue"])

na.omit(estVals[estVals$op == "=~", "pvalue"]) < .05

print(
  paste0(
    if(sum(na.omit(estVals[estVals$op == "=~", "pvalue"]) < .05) 
      != length(na.omit(estVals[estVals$op == "=~", "pvalue"]))){
      "NOT "}, "all factor loadings were significant"))

print("Non-significant factor loadings")
estVals[estVals$op == "=~", "rhs"][estVals[estVals$op == "=~", "pvalue"] > .05]


print("Range of factor loadings")
print(range(estVals[estVals$op == "=~", "est"]))
range(estVals[estVals$op == "=~", "std.all"])

print("Factors intercorrelation")
inspect(mdlFit, "cor.lv")

print("Plot path diagram")
#     rotation = 2 # observed variables on left
#     nCharNodes = 0 # do not trim latent variable names on nodes' labels
#     residuals = FALSE # remove recurrent loop on node itself
#     exoCov = FALSE # do not print the covariances among latent variables
#     whatLabels = 'est' # print estimates on paths
if (require('semPlot')){
  print("please install semPlot to plot this graph")
  }else{
  semPaths(mdlFit, rotation = 2, whatLabels = 'est', 
    nCharNodes = 0, residuals = FALSE, exoCov = FALSE)
  title("Fig. 1. Confirmatory factor analysis.")}
  
#     replace labels in paths with standardized estimates
# semPaths(mdlFit, rotation = 2, whatLabels = 'std', nCharNodes = 0, residuals = FALSE, exoCov = FALSE)

print("Structural equation modeling")
semStx <- "
repeaT ~ csat 
value ~ quality 
csat ~ quality
value ~ cost
repeaT ~ cost
csat ~ value 
"

semFit <- sem(semStx, sample.cov=inspect(mdlFit, "cor.lv"), 
  sample.nobs=n, std.lv=FALSE)
iacobucciOutput(semFit)

print("## xtract model output and print as Iacobucci")
estVals <- parameterEstimates(semFit)
require(xtable)
xtab2 <- xtable(
  # extract paths estimate by indexing the 'op' column with the tilde symbol
  # e.g.: estVals[estVals$op == '~', ]
  # create a new data.frame containing in the column:
  data.frame(
  # 1 the predicted construct
    b = estVals[estVals$op == '~', 'rhs'],
  # 2 the arrow relating the constucts
    rel = "->",
  # 3 the construct used as predictors
    e = estVals[estVals$op == '~', 'lhs'],
  # 4 the estimate of the path with a star if significant
    vals = paste0(
      format(estVals[estVals$op == '~', 'est'], digits = 1), 
      ifelse(estVals[estVals$op == '~', 'pvalue'] < .5, '*', ''))
  ))
print(xtab2, include.rownames = FALSE)

print("## plot path diagram as Iacobucci")
# save path diagram to an object so that we can access its attributes
mdlPlot <- semPaths(semFit, whatLabels = 'std', nCharNodes = 0, 
  layout = 'tree2', edge.label.cex = 1.2)
# as the positioning of the boxes  
mdlPlot$layout
# and their name 
mdlPlot$Arguments$label
# specify new x-y position for variables
ly<-matrix(c(1, 0,
  -0.33, 0,
  0.33, 0,
  -1, 1,
  -1, -1), ncol=2, byrow=TRUE)
# assign new positions to the function generating the path diagram 
semPaths(semFit, whatLabels = 'std', nCharNodes = 0, layout = ly, 
  edge.label.cex = 1.2, residuals=FALSE, sizeMan=8)
# colored graph
semPaths(semFit, what = "std",layout=ly, residuals = FALSE, 
  nCharNodes = 0, edge.label.cex = 1.2, sizeMan = 8)

# source(iacobucci1.R)
