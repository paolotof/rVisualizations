print("Import data")

covMat <- read.table('~/Dropbox/R/sem/iacobucci/iacobucci2.txt')

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
print(paste(
  "chi^2_{", fitMeasures(mdlFit, "df"), 
  "} =", format(fitMeasures(mdlFit, "chisq"), nsmall = 2, digits = 2), 
  ", (p =", format(fitMeasures(mdlFit, "pvalue"), digits = 1), 
  "), CFI =", format(fitMeasures(mdlFit, "cfi"), digits = 2), 
  ", SRMR =", format(fitMeasures(mdlFit, "srmr"), digits = 2)
))

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
semPaths(mdlFit, rotation = 2, whatLabels = 'est', 
  nCharNodes = 0, residuals = FALSE, exoCov = FALSE)
title("Fig. 1. Confirmatory factor analysis.")
#     replace labels in paths with standardized estimates
# semPaths(mdlFit, rotation = 2, whatLabels = 'std', nCharNodes = 0, residuals = FALSE, exoCov = FALSE)
