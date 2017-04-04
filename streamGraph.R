# source('streamGraph.R')
rm(list = ls())
timePoints <- 100
nStreams <- 4
set.seed(09022017)
values <- rnorm(timePoints*nStreams)
values <- abs(values)
# values <- values
dim(values) <- c(timePoints, nStreams)

yy <- matrix(0, timePoints, nStreams)
yy[, 1] <- values[,1]
for (iStream in 2 : nStreams)
	yy[, iStream] <- rowSums(values[,1 : iStream])

matplot(yy, type = 'l', lty = 1)

# smoothed terms
yy <- matrix(0, timePoints, nStreams)
yy[, 1] <- predict(smooth.spline(values[,1]))$y
for (iStream in 2 : nStreams)
	yy[, iStream] <- predict(smooth.spline(rowSums(values[,1 : iStream])))$y

matplot(yy, type = 'l', lty = 1)

yy <- matrix(0, timePoints, (nStreams * 2))
halfPanel <- (nStreams/2)
for (iStream in 1 : halfPanel)
{
	if (iStream == 1)
		yy[, iStream * 2] <- predict(smooth.spline(values[, iStream]))$y
	else
	{
		yy[, iStream * 2 - 1] <- yy[, (iStream - 1) * 2]
		yy[, iStream * 2] <- predict(smooth.spline(values[, iStream]))$y + 
												yy[, iStream * 2 - 1]
	}	
}
halfPanel <- halfPanel + 1
for (iStream in halfPanel : nStreams)
{
	if (iStream == halfPanel)
		yy[, iStream * 2] <- - predict(smooth.spline(values[, iStream]))$y
	else
	{
		yy[, iStream * 2 - 1] <- yy[, (iStream - 1) * 2]
		yy[, iStream * 2] <- (-predict(smooth.spline(values[, iStream]))$y) + 
													yy[, (iStream - 1) * 2]
	}	
}

x11()
xx <- c(1:timePoints, timePoints:1)
plot (xx, xx, type = "n", main = "Streamgraph",
	xlab = "Time", 
	ylab = "Amplitude", ylim = range(yy),
	bty = 'n')
for (iStream in 1 : nStreams)
{
	y <- c(yy[, iStream * 2], rev(yy[, iStream * 2 - 1]))
	polygon(xx, y, col = iStream + 1, border = NA)
}
