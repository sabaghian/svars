setwd("C:/Users/moham/Documents/MBA/SS2020/Economics/assignment/svars/working_directrory")

### Read Data###
# data in csv should be formated wuch that header and freq matches
library(xts)
library(tsbox)
start_year <- 2000;
#interest rate
r<-read.csv("C:/Users/moham/Documents/MBA/SS2020/Economics/assignment/data/r.csv", header = TRUE, stringsAsFactors = FALSE)
r<-r$value
r.ts = ts(data=r, frequency = 1,
             start=c(2000))
#gdp adjusted
ry<-read.csv("C:/Users/moham/Documents/MBA/SS2020/Economics/assignment/data/ry.csv", header = TRUE, stringsAsFactors = FALSE)
ry<-ry$value
ry.ts = ts(data=ry,  frequency = 1,
          start=c(2000))
#inflation index pi
pi<-read.csv("C:/Users/moham/Documents/MBA/SS2020/Economics/assignment/data/pi.csv", header = TRUE, stringsAsFactors = FALSE)
pi<-pi$value
pi.ts = ts(data=pi, frequency = 1,
          start=c(2000))
#government expenditure
rg<-read.csv("C:/Users/moham/Documents/MBA/SS2020/Economics/assignment/data/rg.csv", header = TRUE, stringsAsFactors = FALSE)
rg<-rg$value
rg.ts = ts(data=rg, frequency = 1,
           start=c(2000))
#construction price index
mc<-read.csv("C:/Users/moham/Documents/MBA/SS2020/Economics/assignment/data/mc.csv", header = TRUE, stringsAsFactors = FALSE)
mc<-mc$value
mc.ts = ts(data=mc, frequency = 1,
           start=c(2000,12))
#residential property price index
rhp<-read.csv("C:/Users/moham/Documents/MBA/SS2020/Economics/assignment/data/rhp.csv", header = TRUE, stringsAsFactors = FALSE)
rhp<-rhp$value
rhp.ts = ts(data=rhp, frequency = 1,
           start=c(2000))
#new houses supply
h<-read.csv("C:/Users/moham/Documents/MBA/SS2020/Economics/assignment/data/h.csv", header = TRUE, stringsAsFactors = FALSE)
h<-h$value
h.ts = ts(data=h, frequency = 1,
            start=c(2000))
# prepare svar time series input
svr_data = data.frame(r,pi,  rg, mc,  log(rhp), log(ry))
svr.ts = ts(svr_data,  frequency = 1,
           start=c(1))

### SVAR Identification###
library(svars)
# data contains quarterly observations from 1965Q1 to 2008Q3
# x = output gap
# pi = inflation
# i = interest rates
set.seed(23211)
v1 <- vars::VAR(svr.ts, lag.max = 2, ic = "AIC" )
x1 <- id.chol(v1)
#x1 <- id.chol(v1, order_k = c(7,6,5,4,3,2,1)) ## order_k = c(2,1,3)
summary(x1)
# impulse response analysis
i1 <- irf(x1, n.ahead = 10)
#i2 <- irf(x2, n.ahead = 2)
plot(i1, scales = 'free_y')
#plot(i2, scales = 'free_y')

###Plots###
library(ggplot2)
library(reshape2)
date<-seq(as.Date('2000-12-30'),as.Date('2019-12-30'),by = 365)
df = data.frame(date, r,rg, h, pi, mc,rhp)
df_melt = melt(df, id.vars = 'date')
ggplot(df_melt, aes(x = date, y = value)) +
  geom_line() +
  facet_wrap(~variable, scales = 'free_y', ncol = 1)

