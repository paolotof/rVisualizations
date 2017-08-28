# source("streamGraphs.R")
rm(list = ls())

bump <- function (a) {
  x <- 1 / (.1 + runif(1, min=0, max=1))
  y <- 2 * runif(1, min=0, max=1) - .5
  z <- 10 / (.1 + runif(1, min=0, max=1))
  n <- length(a) - 1
	i <- 0 : n
  w <- (i / n - y) * z
  a <- a + x * exp(-w * w)
}

set.seed(09022017)
nStreams = 20
timePoints = 200

values = matrix(0, timePoints, nStreams)
for (icol in 1 : nStreams)
{
	a = matrix(0, 1, timePoints)
	for (i in 1:5)
		a <- bump(a)
	values[, icol] <- a
}

colorPalette <- colorRampPalette(c("#aaaadd", "#555566"), space = "rgb")
cols <- colorPalette(nStreams)
cols <- cols[sample(nStreams)]

streamGraph <- function(yy, cols, plotTitle = "Streamgraph"){
  timePoints <- dim(yy)[1]
  nStreams <- dim(yy)[2] / 2
	xx <- c(1:timePoints, timePoints:1)
	plot (xx, xx, type = "n", main = plotTitle,
		xlab = "Time",
		ylab = "Amplitude", ylim = range(yy),
		bty = 'n')
	for (iStream in 1 : nStreams)
	{
		y <- c(yy[, iStream * 2], rev(yy[, iStream * 2 - 1]))
    polygon(xx, y, col = cols[iStream], border = NA)
	}
}

computeStacks <- function(values, method = 'ThemeRiver'){
	timePoints <- dim(values)[1]
	nStreams <- dim(values)[2]
	if (method == "newWiggle"){
		thin2large <- sort(apply(values, 2, FUN=var),
			decreasing = FALSE, index.return = TRUE)$ix
		idxStreams <- c(thin2large[seq(1, length(thin2large), 2)],
						thin2large[seq(length(thin2large), 2, -2)])
	}
	yy <- matrix(0, timePoints, (nStreams * 2))
	for (iStream in 1 : nStreams){
    tmpVals <- values[, iStream]
		if (method == "newWiggle")
			tmpVals <- values[, idxStreams[iStream]]
		if (iStream > 1){
			yy[, iStream * 2 - 1] <- yy[, (iStream - 1) * 2]
			yy[, iStream * 2] <- yy[, iStream * 2 - 1] + tmpVals
		} else {
			switch(method,
				ThemeRiver = {
          yy[, 1] <- -(1/2) * rowSums(values)
					yy[, 2] <- yy[, iStream * 2 - 1] + tmpVals},
				zero = {
					yy[, 2] <- tmpVals},
				minimizedWiggle = {
					baseline <- array(0, timePoints)
					for (ipoint in 1 : timePoints) {
					  for (jStream in 1 : nStreams) {
						  baseline[ipoint] = baseline[ipoint] +
						  + (nStreams - jStream - .5) * values[ipoint, jStream]}
					  baseline[ipoint] = baseline[ipoint] / nStreams}
          yy[, 1] <- - baseline
					yy[, 2] <- yy[, iStream * 2 - 1] + tmpVals},
				newWiggle = {
				    baseline <- rowSums(matrix((nStreams - 1 : nStreams - .5),
						nrow = timePoints, ncol = nStreams, byrow = TRUE) * values)
          yy[, 1] <- - (baseline / nStreams)
					yy[, 2] <- yy[, iStream * 2 - 1] + tmpVals},
				{ # default
					print(paste0(baseline, 'not recognized'))
					print('baseline value can be zero, ThemeRiver, minimizedWiggle or newWiggle')}
			)
		}# end: if (iStream > 1){
	}# end:
	return(yy)
}

jpeg(filename = "Rplot%03d.jpeg", width = 800, height = 800)
par(mfrow = c(2, 2))
streamGraph(computeStacks(values, 'zero'), cols, 'zero')
streamGraph(computeStacks(values), cols, 'Theme river')
streamGraph(computeStacks(values, 'minimizedWiggle'), cols, 'minimized Wiggle')
streamGraph(computeStacks(values, 'newWiggle'), cols, 'newWiggle')
par(mfrow = c(1,1))
dev.off()
# source("streamGraphs.R")
