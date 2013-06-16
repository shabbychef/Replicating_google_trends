##### 
#
# Quantifying Trading Behavior in Financial Markets Using Google Trends 
# 
# Copyright (C) 2013 Tobias Preis and Helen Susannah Moat
# http://www.tobiaspreis.de
# http://www.suzymoat.co.uk
#
# This program is free software; you can redistribute it and/or 
# modify it under the terms of the GNU General Public License 
# as published by the Free Software Foundation; either version 
# 3 of the License, or (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful, 
# but WITHOUT ANY WARRANTY; without even the implied warranty of 
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the 
# GNU General Public License for more details. 
# 
# You should have received a copy of the GNU General Public 
# License along with this program; if not, see 
# http://www.gnu.org/licenses/. 
# 
# Related publication: 
# 
# Tobias Preis,	 Helen Susannah Moat, and H. Eugene Stanley,
# Scientific Reports 3, 1684 (2013) 
# doi:10.1038/srep01684 
# 
#

require(xts)

# Read data file
dat <- read.csv(sep=" ","PreisMoatStanley_ScientificReports_3_1684_2013.dat")

# peel off DJIA data, and make into an xts
DJIA.data <- dat[,names(dat) %in% c("DJIA.Date","DJIA.Closing.Price")]
dat <- dat[,!(names(dat) %in% names(DJIA.data))]
# add +1 day to the date to round up to midnight.
DJIA.xts <- xts(DJIA.data[,"DJIA.Closing.Price"],
								order.by=as.POSIXct(DJIA.data[,"DJIA.Date"])+86400)

# peel off dates and make into an xts
google.dates <- dat[,names(dat) %in% c("Google.Start.Date","Google.End.Date")]
dat <- dat[,!(names(dat) %in% names(google.dates))]
search.terms.xts <- xts(dat,order.by=as.POSIXct(google.dates[,"Google.End.Date"]))

# clean up
rm(list=c("DJIA.data","google.dates","dat"))

# now running averages 
delta.t <- 3

# this function takes a vector, and returns the difference
# between a value and the mean value over the previous
# boxwin values.
running.center <- function(x,lag=10) {
	x.cum <- cumsum(x)
	x.dif <- c(x.cum[1:lag],diff(x.cum,lag))
	x.df  <- pmin(1:length(x.cum),lag)
	x.mu  <- x.dif / x.df
	x.ret <- c(NaN,x[2:length(x)] - x.mu[1:(length(x)-1)])
	return(x.ret)
}
# make the detrended 'signal'
signal.xts <- xts(apply(search.terms.xts,2,running.center,lag=delta.t),
						 order.by=time(search.terms.xts))
# at this point, do a spot check to make sure our function
# worked OK
my.err <- signal.xts[delta.t+5,10] - 
					(search.terms.xts[delta.t+5,10] - mean(search.terms.xts[5:(delta.t+4),10]))
if (abs(my.err) > 1e-8)
	stop("fancy function miscomputes the running mean")
# chop off the first delta.t rows
signal.xts <- signal.xts[-c(1:delta.t)]
mkt.xts <- DJIA.xts[-c(1:delta.t)]  # and for the market

# trading signal; the original authors 'short' the trend:
trade.xts <- - signal.xts
# break ties arbitrarily. anything smaller than a certain absolute 
# value gets moved to the tie-breaker. 
TIE.BREAKER <- 1e-5
TIE.LIMIT <- abs(TIE.BREAKER)
trade.xts[abs(trade.xts) <= TIE.LIMIT] <- TIE.BREAKER
# take the sign
sign.trade.xts <- sign(trade.xts)

# braindead 'backtest' function
dumb.bt <- function(sig.xts,mkt.xts) {
	if (dim(sig.xts)[1] != dim(mkt.xts)[1])
		stop("wrong row sizes")
	mkt.lret <- diff(log(mkt.xts),lag=1)
	mkt.rret <- exp(mkt.lret) - 1
	mkt.rret <- as.matrix(mkt.rret[-1])  # chop the first
	sig.xts  <- sig.xts[-dim(sig.xts)[1]]  # chop the last
	bt.rets <- xts(apply(sig.xts,2,function(v) { v * mkt.rret }),
								 order.by=time(sig.xts))
	return(bt.rets)
}
bt.rets <- dumb.bt(sign.trade.xts,mkt.xts)
bt.lrets <- log(1 + bt.rets)  # compute log returns

# first: apply a t-test to every column, get the p-values
ttest.pvals <- apply(bt.lrets,2,function(x) { 
						t.res <- t.test(x,alternative="two.sided")
						p.v <- t.res$p.value
								 })
# function for Q-Q plot against uniformity
qqunif <- function(x,xlab="Theoretical Quantiles under Uniformity",
									 ylab=NULL,...) {
	if (is.null(ylab))
		ylab=paste("Sample Quantiles (",deparse(substitute(x)),")",sep="")
	qqplot(qunif(ppoints(length(x))),x,xlab=xlab,ylab=ylab,...)
	abline(0,1,col='red')
}
qqunif(ttest.pvals)


require(SharpeR)

# under the new version of the package, this is legit,
# but bonks under CRAN version:
srs <- as.sr(bt.lrets)
sharpe.test <- sr_test(srs)
qqunif(sharpe.test$p.value)

# these do *not* have equal SR; but the test
# can reject for weird reasons...
all.eq <- sr_equality_test(as.matrix(bt.lrets),type="F")
all.eq <- sr_equality_test(as.matrix(bt.lrets),type="chisq")

# Hotelling's test
big.sr <- as.sropt(bt.lrets)
print(confint(big.sr))
print(big.sr)

inf <- inference(big.sr,type="KRS") 

