## ----Q2, eval=TRUE, echo=TRUE, results=TRUE, collapse=FALSE-------------------

dat<- read.csv("./data/dolphin.csv", stringsAsFactors= T)

dat$fTime6<- factor(dat$Time6, levels= c("MNight", "AM1", "AM2", "MDay", "PM1", "PM2"))
# reordering chronologically

dat$fTide4<- factor(dat$Tide4)

dat$fMonth<- factor(dat$mon)

str(dat)


## ----Q3, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE, fig.show= ifelse(SOLUTIONS, "asis", "hide")----
# count observations per year/month combination and represent as mosaicplot
plot(table(dat$year, dat$mon))
# CPOD failure in Feb-April 2012 and Dec 2012-March 2013

plot(table(dat$julianday))
# broadly even coverage of days of the year - no major gap

plot(table(dat$fTime6))
# fairly even representation of times of the day
# we should have no problem using 'fTime6' as a predictor in the model

plot(table(dat$fTide4, dat$fTime6))
# even representation of tides
# time of day and tidal phase not entirely independent (but not concerning)
# This is balanced enough that we should have no problem using 'fTime6', fTide4
# or their interaction as predictors in the model.

#### Now, investigating variation in probability of encounter:
#
# presence in relation to time of day
mean.per.fTime6<- tapply(dat$presence, list(dat$fTime6), mean)
plot(mean.per.fTime6, type= "l", ylim= c(0, 1),
		 xlab= "time of day", ylab= "proportion of hours present")
# Probability slightly lower in the middle of the day

# are there seasonal patterns?
mean.per.mon<- tapply(dat$presence, list(dat$mon), mean)
plot(mean.per.mon, type= "l",
				ylim= c(0, 1), xlab= "month", ylab= "proportion of hours present")
# Probability of presence lower in Jan-March?

# presence in relation to tide
mean.per.fTide4<- tapply(dat$presence, list(dat$fTide4), mean)
plot(mean.per.fTide4, type= "b", ylim= c(0, 1),
     xlab= "tidal phase",
		 ylab= "proportion of hours present")
# No obvious effect of tidal phase on average?


# If interested, we could also ask more complex questions, involving interactions between predictors, for example:

# are seasonal patterns similar between years?
# let's calculate the mean per month for each year,
# and plot the seasonal pattern lines for individual years together
mean.per.month.year<- tapply(dat$presence, list(dat$mon, dat$year), mean)
# (month in rows, years in columns)

# matplot draws one line per column (year)
matplot(mean.per.month.year, type= "l",
				ylim= c(0, 1), xlab= "month", ylab= "proportion of hours present")

legend(x= "topleft", legend= colnames(mean.per.month.year),
       bty= "n", # no bounding box for the legend
       col= 1:ncol(mean.per.month.year),
       lty= 1:ncol(mean.per.month.year),
       title= "Year")

# This suggests broadly similar seasonal patterns of variation across years,
# with very low probability of presence from Jan to March

# We could also explore if the effect of some predictors changes between seasons:
# Seasonal variation in diel pattern
mean.per.month.fTime6<- tapply(dat$presence, list(dat$mon, dat$fTime6), mean)
matplot(mean.per.month.fTime6, type= "l", 
        ylim= c(0, 1), 
				xlab= "month", ylab= "proportion of hours present", lty= 1)

legend(x= "topleft", legend= colnames(mean.per.month.fTime6),
       bty= "n", # no bounding box for the legend
       col= 1:ncol(mean.per.month.fTime6),
       lty= 1:ncol(mean.per.month.fTime6),
       title= "Time6")

# stronger diel pattern in later part of the year:
# the lines for different parts of the day diverge quite
# strongly from Sept to Jan.

# are seasonal patterns similar between fTide4 levels?
# let's calculate the mean per month for each tidal stage,
mean.per.month.fTide4<- tapply(dat$presence, list(dat$mon, dat$fTide4), mean)
matplot(mean.per.month.fTide4, type= "l",
       ylim= c(0, 1),
			 xlab= "month", ylab= "proportion of hours present", lty= 1)

legend(x= "topleft", legend= colnames(mean.per.month.fTide4),
       bty= "n", # no bounding box for the legend
       col= 1:ncol(mean.per.month.fTide4),
       lty= 1:ncol(mean.per.month.fTide4),
       title= "Tide4")

# no dramatic change in pattern of tide use across seasons, 
# as all the lines follow a broadly similar trajectory:
# the probability of sighting is mostly affected by season.
# There are variations among tide levels but more subtle.
# Would such an interaction turn out to be significant in a model?



## ----Q4, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE----------
PA1<- glm(presence ~ julianday + fTide4 + fTime6, family= binomial, data= dat)


## ----Q5, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE----------
summary(PA1)

# "(Intercept)" is the predicted value on the link (logit) scale for julianday = 0, tidal
# state 1 around midnight ("fTime6MNight")

# "julianday" is the slope of day of year, assumes a linear increase 
# (on the logit scale) from 1st Jan to 31st Dec

# "fTide4X" is the estimated difference (on the logit scale) between
# tidal states 1 and X

# "fTime6X" is the estimated difference (on the logit scale) between
# periods of the day 1 and X

# A mathematical description of the model
# (more or less how I would present it in the methods section of a paper):
# presence ~ Bernoulli(p)  or presence ~ Binomial(N= 1, p)
# log(p / (1-p)) = -0.999 + 0.0022*julianday
#    -0.269 * fTide42 -0.185 * fTide43 -0.162 * fTide44   
#    -0.099 * fTime6AM1 -0.514 * fTime6AM2
#    -0.644 * fTime6MDay -0.648 * fTime6PM1 
#    -0.054 * fTime6PM2 


## ----Q6, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE----------

drop1(PA1, test= "Chisq")
# all terms significant; nothing to drop

# out of interest, the total proportion of deviance explained is 
(PA1$null.deviance - PA1$deviance) / PA1$null.deviance
# 2.2%, very low!


## ----Q7, eval=TRUE, echo=TRUE, results=SOLUTIONS, collapse=TRUE, fig.show= ifelse(SOLUTIONS, "asis", "hide"), fig.height=8, fig.width=8----
par(mfrow= c(2, 2))
plot(PA1) # not very useful

# to make sense of what we are seeing, we can add colors: red
# for residuals of presence data, black for residuals of absence data
plot(PA1, col= dat$presence + 1)
# Not very telling either

# let's plot against predictors:
res1.p<- resid(PA1, type= "pearson")

par(mfrow= c(2, 2))
plot(res1.p ~ dat$fTide4) # boxplot (x axis is a factor)

plot(res1.p ~ dat$fTime6) # boxplot

plot(res1.p ~ dat$julianday, col= dat$presence + 1) # scatterplot

# Can't see anything useful.

# Use arm if you can:
# computes the mean of residuals per bin of numerical 
# variables (should be randomly distributed around zero).
# You need to convert categorical variables into 
# numerical for the function to work
library(arm)
par(mfrow= c(2, 2))
binnedplot(x= as.numeric(dat$fTide4), y= res1.p, xlab= "Tidal state")
binnedplot(x= as.numeric(dat$Time6), y= res1.p, xlab= "Time of day")
binnedplot(x= dat$julianday, y= res1.p, xlab= "Day of the year", nclass= 100)




## ----Q9a, eval=TRUE, echo=TRUE, results=SOLUTIONS, collapse=TRUE--------------
PA1.dat4pred<- data.frame(fTime6= levels(dat$fTime6),
                                julianday= 180, fTide4= "1")

PA1.pred<- predict(PA1, PA1.dat4pred, type= "link", se.fit= T)

# Convert predictions to the response (probability) scale.
# And add them to the prediction data frame (that bit is optional)
PA1.dat4pred$fit.resp<- exp(PA1.pred$fit)/(1+exp(PA1.pred$fit)) 
# or plogis(PA1.pred$fit)

# lower 95% CI
PA1.dat4pred$LCI<- plogis(PA1.pred$fit - 1.96*PA1.pred$se.fit)
# upper 95% CI
PA1.dat4pred$UCI<- plogis(PA1.pred$fit + 1.96*PA1.pred$se.fit)


## ----Q9b, eval=TRUE, echo=TRUE, results=SOLUTIONS, collapse=TRUE, fig.show= ifelse(SOLUTIONS, "asis", "hide"), fig.height=5----
par(mfrow= c(1, 1))
plot(x= 1:6, y= PA1.dat4pred$fit.resp, 
      pch= 16, cex= 1.4, xlab= "Section of day",
		  ylab= "Fitted probability", ylim= c(0, 1),
      main= "Predictions for time of day\n(assuming Tidal state 1 and day 180)")

arrows(x0= 1:6, x1= 1:6,
          y0= PA1.dat4pred$LCI, y1= PA1.dat4pred$UCI,
          length= 0.02, angle= 90, code= 3)



## ----Q10, eval=TRUE, echo=TRUE, results=SOLUTIONS, collapse=TRUE, fig.show= ifelse(SOLUTIONS, "asis", "hide"), fig.height=8, fig.width=8----
par(mfrow= c(2, 2)) # we will need 3 plots

# repeat plotting of predictions for fTime6
PA1.dat4pred<- data.frame(fTime6= levels(dat$fTime6),
                                julianday= 180, fTide4= "1")

PA1.pred<- predict(PA1, PA1.dat4pred, type= "link", se.fit= T)

PA1.dat4pred$fit.resp<- plogis(PA1.pred$fit) 
# lower 95% CI
PA1.dat4pred$LCI<- plogis(PA1.pred$fit - 1.96*PA1.pred$se.fit)
# upper 95% CI
PA1.dat4pred$UCI<- plogis(PA1.pred$fit + 1.96*PA1.pred$se.fit)

plot(x= 1:6, y= PA1.dat4pred$fit.resp, 
      pch= 16, cex= 1.4, xlab= "Section of day",
		  ylab= "Fitted probability", ylim= c(0, 1),
      main= "Predictions for time of day\n(assuming Tidal state 1 and day 180)",
      xaxt= "n") # supress automatic x axis (we will draw our own improved axis)

arrows(x0= 1:6, x1= 1:6,
          y0= PA1.dat4pred$LCI, y1= PA1.dat4pred$UCI,
          length= 0.02, angle= 90, code= 3)

axis(side= 1, at= 1:6, label= levels(dat$fTime6))

# plotting of predictions for julianday
julianday.seq<- 1:365
PA1.dat4pred<- data.frame(julianday= julianday.seq,
                              fTime6 = "PM2", fTide4= "1")

PA1.pred<- predict(PA1, PA1.dat4pred, type= "link", se.fit= T)

PA1.dat4pred$fit.resp<- plogis(PA1.pred$fit) 
# lower 95% CI
PA1.dat4pred$LCI<- plogis(PA1.pred$fit - 1.96*PA1.pred$se.fit)
# upper 95% CI
PA1.dat4pred$UCI<- plogis(PA1.pred$fit + 1.96*PA1.pred$se.fit)

plot(x= julianday.seq, PA1.dat4pred$fit.resp, 
      pch= 16, cex= 1.4, xlab= "Day of the year",
		  ylab= "Fitted probability", ylim= c(0, 1),
      main= "Predictions per day\n(assuming Time = PM2 and Tidal state 1)", type= "l", lwd= 1.5)

lines(x= julianday.seq, y= PA1.dat4pred$LCI, lty= 3)
lines(x= julianday.seq, y= PA1.dat4pred$UCI, lty= 3)

# plotting of predictions for fTide4
PA1.dat4pred<- data.frame(fTide4= levels(dat$fTide4),
                              fTime6 = "PM2", julianday= 180)

PA1.pred<- predict(PA1, PA1.dat4pred, type= "link", se.fit= T)

PA1.dat4pred$fit.resp<- plogis(PA1.pred$fit) 
# lower 95% CI
PA1.dat4pred$LCI<- plogis(PA1.pred$fit - 1.96*PA1.pred$se.fit)
# upper 95% CI
PA1.dat4pred$UCI<- plogis(PA1.pred$fit + 1.96*PA1.pred$se.fit)

plot(1:4, PA1.dat4pred$fit.resp, 
      pch= 16, cex= 1.4, xlab= "Tidal state",
		  ylab= "Fitted probability", ylim= c(0, 1),
      main= "Predictions for tide\n(assuming Time = PM2 and day 180)",
      xaxt= "n") # supress x axis (we will draw our own)

axis(side= 1, at= 1:4, label= levels(dat$fTide4))

arrows(x0= 1:4, x1= 1:4,
          y0= PA1.dat4pred$LCI, y1= PA1.dat4pred$UCI,
          length= 0.02, angle= 90, code= 3)





## ----binnedplot_alternative, eval=TRUE, echo=TRUE, results=SOLUTIONS, collapse=TRUE, fig.show= ifelse(SOLUTIONS, "asis", "hide")----
par(mfrow= c(1, 1))
# plot the residuals against julianday
plot(res1.p ~ dat$julianday, col= dat$presence + 1)
# get the mean of the residuals for each 1 day of julianday
day.means<- tapply(res1.p, list(dat$julianday), mean)
# convert ordered bin labels into numbers (1 to 365)
day.vals<- as.numeric(names(day.means))
lines(day.means ~ day.vals, col= 3)
abline(h= 0, lty= 3, col= grey(0.5))

