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

# Read data file
dat <- read.csv(sep=" ","PreisMoatStanley_ScientificReports_3_1684_2013.dat")

google.dates <- dat[,names(dat) %in% c("Google.Start.Date","Google.End.Date")]
dat <- dat[,!(names(dat) %in% names(google.dates))]
DJIA.data <- dat[,names(dat) %in% c("DJIA.Date","DJIA.Closing.Price")]
dat <- dat[,!(names(dat) %in% names(DJIA.data))]

# now running averages 
delta.t <- 3

cumul.dat <- as.data.frame(lapply(dat,cumsum),col.names=names(dat))
ok.diff <- function(x,lag=1) {
	c(x[1:lag],diff(x,lag))
}
run.sum <- lapply(cumul.dat,ok.diff,lag=delta.t)
run.sum <- as.data.frame(run.sum,col.names=names(cumul.dat),row.names=rownames(dat))
run.mean <- run.sum / delta.t

trend <- dat[(delta.t+1):dim(dat)[1],] - run.mean[(delta.t):(dim(run.mean)[1] - 1),]
# break ties arbitrarily
trend[trend == 0] <- 1e-9

# for some reason, do this:
trade.sign <- - sign(trend)

mean.exposure <- apply(trade.sign,2,mean)

# now the corresponding returns
DJIA.lret <- diff(log(DJIA.data[(delta.t+1):dim(DJIA.data)[1],"DJIA.Closing.Price"]),lag=1)
DJIA.lret <-
	as.data.frame(DJIA.lret,col.names=colnames(DJIA.data),row.names=rownames(DJIA.data)[(delta.t+2):dim(DJIA.data)[1]])
DJIA.rret <- exp(DJIA.lret) - 1

# braindead 'backtest'
bt.rets <- lapply(trade.sign[1:(dim(trade.sign)[1]-1),],function(v) { v *
									DJIA.rret })
bt.rets <- as.data.frame(bt.rets)
colnames(bt.rets) <- colnames(trade.sign)
rownames(bt.rets) <- rownames(DJIA.rret)
bt.lrets <- log(1 + bt.rets)

require(SharpeR)

srs <- lapply(bt.lrets,as.sr,ope=252,na.rm=TRUE)
# this is fucked!

foo <- unlist(lapply(srs,function(x) { x$sr }))
qqnorm(foo)

