# source(streamGraph_realData.R')
rm(list = ls())
dat <- read.table(file = "test.txt", header = TRUE)
# bring target first for consistencies among the three graphs
dat$item <- relevel(dat$item, ref = 'ta') 
rgbCol <- c('#7fc97f',
	'#beaed4',
	'#fdc086',
	'#ffff99')
condition <- levels(dat$cond)
oldMai <- par()$mai
newMai <- c(.5, 0.5, 0.5, 0.1)
par(mfrow = c(2,2), mai = newMai, mgp=c(2,1,0))
require(splines)
splinesDF = 9
x11()
for (icond in condition)
{
	plot(1, type="n", xlab="Time (ms)", ylab="Proportion Fixations", 
			 xlim=c(0, max(dat$bin)), ylim=c(0, 1), bty = 'n')
	itemFixated <- levels(dat$item)
	counter = 1
	for (item in itemFixated)
	{	
		lines(
			dat$propFix[dat$cond == icond & dat$item == item], 
			col=rgbCol[counter], lwd = 2)
		counter = counter + 1
	}
	conditionLabel <- 'Neutral' 
	if (icond == 'c')
		conditionLabel <- 'Context'
	title(conditionLabel)
}
plot(1, type="n", xlab="Time (ms)", ylab="Proportion Fixations", 
		 xlim=c(0, max(dat$bin)), ylim=c(-1, 1), yaxt = 'n', bty = 'n')
for (icond in condition)
{
	itemFixated <- levels(dat$item)
	counter = 1
	flip = 1
	if (icond == 'c')
		flip = -1
	for (item in itemFixated)
	{	
		data2plot <- flip * dat$propFix[dat$cond == icond & dat$item == item]
		lines(data2plot, 
			 col=rgbCol[counter], lwd = 2)
		counter = counter + 1
	}
	title('Neutral & Context')
}
yAxisLocation <- 2
axis(yAxisLocation,
	at = seq(-1, 1, length.out = 5),
	labels = abs(seq(-1, 1, length.out = 5))
)
itemFixated <- levels(dat$item)
condition <- levels(dat$cond)
dims <- length(itemFixated) * length(condition)
maxBins <- max(dat$bin)
nPoints <- 100
iCol <- 1
yy <- matrix(0, maxBins, dims * 2)
for (icond in condition)
{
	upLowPanel <- 1
	if(icond == 'c')
		upLowPanel <- -1
	for (item in itemFixated)
	{
		tmpVals <- predict(
			lm(dat$propFix[dat$cond == icond & dat$item == item] ~
			ns(dat$bin[dat$cond == icond & dat$item == item], splinesDF)) 
		)
		if (iCol %% length(itemFixated) == 1)
		{
			yy[, iCol * 2] <- upLowPanel * tmpVals
		}
		else
		{
			yy[, iCol * 2 - 1] <- yy[, (iCol - 1) * 2]
			yy[, iCol * 2] <- upLowPanel * tmpVals +
				rowSums(yy[, seq((iCol - 1) * 2, iCol * 2, 2)])
		}
		iCol <- iCol + 1
	}
}
xx <- c(1:maxBins, maxBins:1)
par(xpd=T, mai = oldMai, mar=par()$mar+c(0,0,0,6), mgp=c(2,1,0))
plot (xx, xx,
			type = "n", xlab = "Time (ms)",
			ylab = "Proportion of fixations", yaxt = 'n', ylim = range(yy),
			bty = 'n')
for (iCol in 1 : dims)
{
	y <- c(yy[, iCol * 2 - 1], rev(yy[, iCol * 2]))
	polygon(xx, y, col = rgbCol[(iCol %% 4) + 1], border = "red")
}
title("Streamgraph")
legend(1300, 1, c("Target", "Competitor", "D1", "D2"),
	pch = 15,
	col = rgbCol[(1:4 %% 4) + 1],
	bty = 'n')
yAxisLocation <- 2
axis(yAxisLocation,
	at = seq(-1, 1, length.out = 5),
	labels = abs(seq(-1, 1, length.out = 5))
)
mtext(c('Neutral', 'Context'), side = 4, at = c(.5, -.5))
par(mar=c(5, 4, 4, 2) + 0.1, mfrow = c(1,1))

