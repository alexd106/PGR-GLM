## ----Q2, eval=TRUE, echo=TRUE, results=TRUE, collapse=FALSE-------------------

dat<- read.csv("./data/dolphin.csv", stringsAsFactors= T)

# re-ordering factor levels for convenience:
dat$Per2<- factor(dat$Per2, levels= c("RestOfYear", "MayJun"))
# (making "RestOfYear" the reference level)
dat$Per4<- factor(dat$Per4, levels= c("RestOfYear", "MayJun1", "MayJun2", "MayJun3"))
dat$Time6<- factor(dat$Time6, levels= c("MNight", "AM1", "AM2", "MDay", "PM1", "PM2"))
# reordering chronologically

str(dat)


## ----Q3, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE, fig.show= ifelse(SOLUTIONS, "asis", "hide")----
# count observations per year/month combination and represent as mosaicplot
plot(table(dat$year, dat$mon))
# CPOD failure in Feb-April 2012 and Dec 2012-March 2013

plot(table(dat$mh))
# fairly even representation of hours
# (that's on the random sample; Almost perfectly balanced on the full dataset)
# we should have no problem using 'mh' as a predictor in the model

plot(table(dat$Tide4, dat$mh))
# even representation of tides
# time of day and tidal phase not independent (but not a linear correlation)
# This is balanced enough that we should have no problem using 'mh', Tide4
# or their interaction as predictors in the model.

#### Now, investigating variation in probability of encounter:
# presence in relation to time of day
plot(tapply(dat$presence, list(dat$mh), mean), type= "l", ylim= c(0, 1),
		 xlab= "time of day", ylab= "proportion of hours present")
# Probability slightly lower in the middle of the day

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

# This suggests similar seasonal patterns of variation across years,
# with very low probability of presence from Jan to March

# Presence in relation to tide
mean.per.Tide4<- tapply(dat$presence, list(dat$Tide4), mean)
plot(mean.per.Tide4, type= "b", ylim= c(0, 1),
     xlab= "tidal phase",
		 ylab= "proportion of hours present")
# No obvious effect of tidal phase on average?

# are seasonal patterns similar between Tide4 levels?
# let's calculate the mean per month for each tidal stage,
mean.per.month.Tide4<- tapply(dat$presence, list(dat$mon, dat$Tide4), mean)
matplot(mean.per.month.Tide4, type= "l",
       ylim= c(0, 1),
			 xlab= "month", ylab= "proportion of hours present", lty= 1)

legend(x= "topleft", legend= colnames(mean.per.month.Tide4),
       bty= "n", # no bounding box for the legend
       col= 1:ncol(mean.per.month.Tide4),
       lty= 1:ncol(mean.per.month.Tide4),
       title= "Tide4")

# no dramatic change in pattern of tide use across seasons, 
# as all the lines follow a broadly similar trajectory:
# the probability of sighting is mostly affected by season.
# There are variations among tide levels but more subtle.
# Would such an interaction turn out to be significant in a model?

# Seasonal variation in diel pattern
mean.per.month.Time6<- tapply(dat$presence, list(dat$mon, dat$Time6), mean)
matplot(mean.per.month.Time6, type= "l", 
        ylim= c(0, 1), 
				xlab= "month", ylab= "proportion of hours present", lty= 1)

legend(x= "topleft", legend= colnames(mean.per.month.Time6),
       bty= "n", # no bounding box for the legend
       col= 1:ncol(mean.per.month.Time6),
       lty= 1:ncol(mean.per.month.Time6),
       title= "Time6")

# stronger diel pattern in later part of the year:
# the lines for different parts of the day diverge quite
# strongly from Sept to Jan.

# Variation in diel pattern between May-June (red) and the rest of the year
mean.mh.Per2<- tapply(dat$presence, list(dat$mh, dat$Per2), mean)

matplot(mean.mh.Per2, type= "l",
        ylim= c(0, 1), 
				xlab= "time of day", ylab= "proportion of hours present", lty= 1)

legend(x= "topleft", legend= colnames(mean.mh.Per2),
       bty= "n", # no bounding box for the legend
       col= 1:ncol(mean.mh.Per2),
       lty= 1:ncol(mean.mh.Per2))

# less nocturnal in spring?



## ----Q4, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE----------
PA1<- glm(presence ~ tideangle_deg + mh + julianday, family= binomial, data= dat)


## ----Q5, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE----------
summary(PA1)

# Model description:
# presence ~ Bernoulli(p)  or presence ~ Binomial(N= 1, p)
# log(p / (1-p)) = 
#       -1.40*(Intercept) - 0.00044*tideangle_deg + 0.0012*mh 
#       + 0.0022*julianday

# "(Intercept)" general intercept

# "tideangle_deg" main effect of tide angle. Hypothesis: there is a monotonic
# increase or decrease of the probability of presence with tidal state

# "mh" main effect of time of day
# "julianday" main effect of day of year. Hypothesis: there is a monotonic
# increase or decrease of the probability of presence with day of the year



## ----Q6, eval=TRUE, echo=TRUE, results=SOLUTIONS, collapse=TRUE, fig.show= ifelse(SOLUTIONS, "asis", "hide")----
library(car)
vif(PA1)
# No concern.

par(mfrow= c(2, 2))
plot(PA1, col= dat$presence + 1) # red is presence, black is absence
# Not very useful or pretty statistical art. Not worth framing.

# plot against predictors:
res.PA1.p<- resid(PA1, type= "pearson")

par(mfrow= c(2, 2))
plot(res.PA1.p ~ dat$tideangle_deg, col= dat$presence + 1)

plot(res.PA1.p ~ dat$mh, col= dat$presence + 1)

plot(res.PA1.p ~ dat$julianday, col= dat$presence + 1)

# Can't see anything useful.

# Use arm if you can:
library(arm)
par(mfrow= c(2, 2))
binnedplot(x= dat$tideangle_deg, y= res.PA1.p, xlab= "Tide angle", nclass= 100)
binnedplot(x= dat$mh, y= res.PA1.p, xlab= "hour")
binnedplot(x= dat$julianday, y= res.PA1.p, xlab= "Day of the year", nclass= 100)


## ----Q6b, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE---------
# clearly some unwanted patterns, especially in mh and julianday
# but possibly in tide angle, too
# all pointing at non-linear effects of the predictors on the response



## ----Q6c, eval=TRUE, echo=TRUE, results=SOLUTIONS, collapse=TRUE, fig.show= ifelse(SOLUTIONS, "asis", "hide")----
par(mfrow= c(2, 2))
# plot the residuals against tideangle_deg
plot(res.PA1.p ~ dat$tideangle_deg, col= dat$presence + 1)
# get the mean of the residuals for each 1 degree bin of tideangle_deg
tide.means<- tapply(res.PA1.p, list(dat$tideangle_deg), mean)
# convert ordered bin labels into numbers (1 to 360)
tide.vals<- as.numeric(names(tide.means))
# plot residual means against bin number
lines(tide.means ~ tide.vals, col= 3)
# add horizontal line at y= 0 for reference
abline(h= 0, lty= 3, col= grey(0.5))

# same idea for hour of the day:
plot(res.PA1.p ~ dat$mh, col= dat$presence + 1)
hour.means<- tapply(res.PA1.p, list(dat$mh), mean)
lines(hour.means ~ as.numeric(names(hour.means)), col= 3)
abline(h= 0, lty= 3, col= grey(0.5))

# same for julianday:
plot(res.PA1.p ~ dat$julianday, col= dat$presence + 1)
day.means<- tapply(res.PA1.p, list(dat$julianday), mean)
lines(day.means ~ as.numeric(names(day.means)), col= 3)
abline(h= 0, lty= 3, col= grey(0.5))

# Same story.


## ----Q7a, eval=TRUE, echo=TRUE, results=SOLUTIONS, collapse=TRUE--------------
# Please take the time to think before unfolding the next code chunk



## ----Q7b, eval=TRUE, echo=TRUE, results=SOLUTIONS, collapse=TRUE--------------
# The issue is that the effects of these predictors are not linear
# on the logit (link) scale.

# There are several ways the non-linearity could be addressed. 
# one of the most straightforward with glm() is to discretize
# continuous predictors into bins and to treat them as factors.
# In this way, a mean is estimated per category of the variable,
# and no assumption is made about the shape of the relationship.

# Each of the predictors we started with already has one or more 
# categorical counterpart in the data set.
# I suggest you try fTide4 + Per2 + Time6, 
# with fTide4 being the factor version of Tide4 (needs creating).

# Then, we are also interested in interactions between these predictors.
# Some of the more biologically relevant interactions could include
#  + fTide4:Per2 + fTide4:Time6 + Per2:Time6
# You can choose something else or cut the predictors into different bin 
# definitions, too. Example code for this is given in the appendix at
# the end of the practical



## ----Q8, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE----------
# convert numerically coded categorical variables into factors:
dat$fTide4<- factor(dat$Tide4)

PA10<- glm(presence ~ fTide4 + Per2 + Time6 + fTide4:Per2 + fTide4:Time6 +
					 	Per2:Time6, family= binomial, data= dat)

require(car)
vif(PA10)
# No substantial concern.



## ----Q9, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE----------
summary(PA10)

# "(Intercept)" general intercept
# "fTide4" main categorical effect of tide angle (4 categories, thus 3 coefficients)
# "Time6" main effect of time of day (6 categories, thus 5 coefficients)
# "Per2MayJun" main effect of period of the year
# "fTide4:Per2" (interaction) does effect of tide angle change with period of the year?
# "fTide4:Time6" (interaction) does effect of tide angle change with time of day?
# "Per2:Time6" (interaction) does effect of time of day change with period of the year?



## ----Q10, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE---------
drop1(PA10, test= "Chisq")
# drop fTide4:Time6

PA11<- glm(presence ~ fTide4 + Per2 + Time6 + fTide4:Per2 + Per2:Time6,
					 family= binomial, data= dat)

drop1(PA11, test= "Chisq")
# drop fTide4:Per2

PA12<- glm(presence ~ fTide4 + Per2 + Time6 + Per2:Time6,
					 family= binomial, data= dat)

drop1(PA12, test= "Chisq")
# nothing else to drop

summary(PA12)
anova(PA12, test= "Chisq")
# fTide4 contributes minimally

# Total proportion of deviance explained is 
(PA12$null.deviance - PA12$deviance) / PA12$null.deviance # 3%


## ----Q11, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE, fig.show= ifelse(SOLUTIONS, "asis", "hide")----
# plot against predictors:
 res.PA12.p<- resid(PA12, type= "pearson")

library(arm)
par(mfrow= c(2, 2))
binnedplot(x= dat$tideangle_deg, y=  res.PA12.p, xlab= "Tide angle", nclass= 100)
# okay
binnedplot(x= dat$mh, y=  res.PA12.p, xlab= "hour")
# okay
binnedplot(x= dat$julianday, y=  res.PA12.p, xlab= "Day of the year", nclass= 100)
# julianday is not strictly a predictor in the model, 
# but is a more informative version of Per2:
# while Per 2 is the predictor used, there is no certainty that it's a good way
# to describe temporal variation (the cut is quite arbitrary). 
# Plotting against julian day allows to check this further.

# Residuals look less than good. Not too surprising, because the
# model only allows for difference between May-June
# and rest of the year, since predictor 'Per2' lumps everything
# from July to April in the same category)

# Check seasonal variation in diel pattern again ("time by season" interaction):
# This calculates the mean of the residuals per period of the day and
# per month (and plots one line for each period of the day against month)
matplot(tapply(res.PA12.p, list(dat$mon, dat$Time6), mean), type= "l", 
        xlab= "month", ylab= "proportion of hours present", lty= 1)
# more residual variation in diel pattern in later part of the year
# (not surprising as there is nothing in the model aiming at capturing this)

# residuals suggest that a finer binning of time of the year
# is required for the predictors. Or an approach that circumvents the issues
# with binning (see reference at the end for an alternative approach).



## ----Q12a, eval=TRUE, echo=TRUE, results=SOLUTIONS, collapse=TRUE-------------
PA12.dat4pred<- expand.grid(Time6= levels(dat$Time6),
                                Per2= levels(dat$Per2),
								fTide4= "1")


## ----Q12b, eval=TRUE, echo=TRUE, results=SOLUTIONS, collapse=TRUE-------------
PA12.pred<- predict(PA12, PA12.dat4pred, type= "link", se.fit= T)

PA12.dat4pred$fit.resp<- plogis(PA12.pred$fit)
# or exp(PA12.pred$fit)/(1+exp(PA12.pred$fit)) for the long version

# lower 95% CI
PA12.dat4pred$LCI<- plogis(PA12.pred$fit - 1.96*PA12.pred$se.fit)
# upper 95% CI
PA12.dat4pred$UCI<- plogis(PA12.pred$fit + 1.96*PA12.pred$se.fit)


## ----Q12c, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE, fig.height=5----
par(mfrow= c(1, 1))
plot(as.numeric(PA12.dat4pred$Time6), PA12.dat4pred$fit.resp, pch= 16, cex= 1.4,
      col= PA12.dat4pred$Per2, xlab= "Section of day",
		  ylab= "Fitted probability (assuming Tide = 1)", ylim= c(0, 1))

arrows(x0= as.numeric(PA12.dat4pred$Time6), x1= as.numeric(PA12.dat4pred$Time6),
          y0= PA12.dat4pred$LCI, y1= PA12.dat4pred$UCI,
		  col= PA12.dat4pred$Per2, length= 0.02, angle= 90, code= 3)

legend(x= "topright", legend= c("RestOfYear", "MayJun"), col= c(1, 2), lty= 1, pch= 16)


## ----Q13, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE, fig.height=8, , fig.width=8----
# dolphins have a weak but apparently stable preference for certain tidal states in Sutors.

# According to model PA12, they are more likely to be seen during the day in
# May/June than in other months where they are more nocturnal 

# There are few assumptions for the Bernoulli distribution other than 
# observations being zeros and ones.
# Some assumptions valid for all models still apply here, such as: model 
# correctly specified; independent
# residuals. The latter is violated in this data set due to consecutive 
# measurements in time. This issue
# is explored in the linked paper, using mixed models for non-independent data 
# The paper also uses GAMs for avoiding the discretization of 
# continuous variables, and for
# accounting for the cyclicity of the preditors (estimates at each end should 
# match, e.g. 31st Dec-1st Jan, or 23:59 - 00:00)

# Of note is the extremely low proportion of deviance explained by the model:
(PA12$null.deviance - PA12$deviance) /
	PA12$null.deviance 
# 3%. 
# Models for Bernoulli data rarely explain a large proportion of the deviance
# because Bernouilli data (0/1) are quite crude and thus, noisy.
# But 3% is particularly low, suggesting that the trends identified here are
# biologically weak ones albeit statistically significant.



## ----QA1, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE---------
PA10.MAM.stepAIC<- step(PA10)

anova(PA10.MAM.stepAIC, test= "Chisq")
# same model as PA13

# convert 'mon' into factor
dat$fMonth<- factor(dat$mon)
# fit new model
PA20<- glm(presence ~ fTide4 + fMonth + Time6 + fTide4:fMonth + fTide4:Time6 +
					 	fMonth:Time6, family= binomial, data= dat)

PA20.MAM.stepAIC<- step(PA20)

anova(PA20.MAM.stepAIC, test= "Chisq")
# Same structure selected: tide, plus season (month) by time of day
AIC(PA10.MAM.stepAIC)
AIC(PA20.MAM.stepAIC)
# Monthly model vastly favoured despite the 60 extra parameters

anova(PA10.MAM.stepAIC, PA20.MAM.stepAIC, test= "Chisq")
# also clearly favoured by likelihood ratio test

# constructing the AIC table by hand:
Model.formulas<- c("fTide4 + Per2 + Time6 + fTide4:Per2 + fTide4:Time6 + Per2:Time6",
	"fTide4 + Per2 + Time6 + fTide4:Per2 + Per2:Time6",
	"fTide4 + Per2 + Time6 + Per2:Time6",
  "fTide4 + fMonth + Time6 + fTide4:fMonth + fTide4:Time6 + fMonth:Time6",
  "fTide4 + fMonth + Time6 + fTide4:fMonth + fMonth:Time6",
	"fTide4 + fMonth + Time6 + fMonth:Time6")

M.start<- glm(presence ~ fTide4 + Per2 + Time6 + fTide4:Per2 + fTide4:Time6 + 
              Per2:Time6, family= binomial, data= dat)
M.step2<- glm(presence ~ fTide4 + Per2 + Time6 + fTide4:Per2 + Per2:Time6, 
              family= binomial, data= dat)
M.step3<- glm(presence ~ fTide4 + Per2 + Time6 + Per2:Time6, 
              family= binomial, data= dat)
M.step4<- glm(presence ~ fTide4 + fMonth + Time6 + fTide4:fMonth + fTide4:Time6 +
              fMonth:Time6, family= binomial, data= dat)
M.step5<- glm(presence ~ fTide4 + fMonth + Time6 + fTide4:fMonth + 
              fMonth:Time6, family= binomial, data= dat)
M.step6<- glm(presence ~ fTide4 + fMonth + Time6 + fMonth:Time6, 
              family= binomial, data= dat)

Model.AIC<- c(AIC(M.start),
	AIC(M.step2),
	AIC(M.step3),
	AIC(M.step4),
	AIC(M.step5),
	AIC(M.step6))

summary.table<- data.frame(Model= Model.formulas,
	AIC= round(Model.AIC, 2))

# Sorting models from lowest AIC (preferred) to highest (least preferred):
summary.table<- summary.table[order(summary.table$AIC), ]

# Adding AIC differences with respect to best model:
summary.table$deltaAIC<- summary.table$AIC - summary.table$AIC[1]

# summary.table
# All the models including fMonth are preferred to any model that doesn't


## ----QA1b, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE--------
require(knitr)
kable(summary.table)


## ----QA1c, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE, fig.show= ifelse(SOLUTIONS, "asis", "hide")----
# residual analysis:
res20.d<- resid(PA20.MAM.stepAIC, type= "pearson")
library(arm)
par(mfrow= c(2, 2))
binnedplot(x= dat$tideangle_deg, y=   res20.d, xlab= "Tide angle", nclass= 100)
binnedplot(x= dat$mh, y=   res20.d, xlab= "hour")
binnedplot(x= dat$julianday, y=   res20.d, xlab= "Day of the year", nclass= 100)
# Check seasonal variation in diel pattern again ("time by season" interaction):
matplot(tapply( res20.d, list(dat$mon, dat$Time6), mean), type= "l",
        xlab= "month", 
				ylab= "proportion of hours present", lty= 1)
# much improved


# More detail on the variation of the daily pattern along the year
# could be obtained by plotting the predicted hour of day effect per month
# from PA20.MAM.stepAIC), like this:

par(mfrow= c(3, 4))
for(month in 1:12){
	dat4pred<- expand.grid(Time6= levels(dat$Time6),
                                fMonth= as.character(month),
								fTide4= "1")
	PA20.pred<- predict(PA20.MAM.stepAIC, dat4pred, type= "link", se.fit= T)
	dat4pred$fit.resp<- plogis(PA20.pred$fit)
	dat4pred$LCI<- plogis(PA20.pred$fit - 1.96*PA20.pred$se.fit)
	dat4pred$UCI<- plogis(PA20.pred$fit + 1.96*PA20.pred$se.fit)
	plot(as.numeric(dat4pred$Time6), dat4pred$fit.resp, pch= 16, 
		  cex= 1.4, main= paste("Month =", month),
          col= 1, xlab= "Section of day",
		  ylab= "Fitted probability (assuming Tide = 1)", ylim= c(0, 1))
	arrows(x0= as.numeric(dat4pred$Time6), x1= as.numeric(dat4pred$Time6),
          y0= dat4pred$LCI, y1= dat4pred$UCI,
		  col= grey(0.5), length= 0.02, angle= 90, code= 3)
}

# According to this better supported model, they tend to use the site more
#  in May, June and 
# July day and night, visit mostly by night from October to December,
#  and seldom from Jan to March.

# Of note is the low proportion of deviance explained by the model,
# despite its complexity (75 parameters):
(PA20.MAM.stepAIC$null.deviance - PA20.MAM.stepAIC$deviance) /
	PA20.MAM.stepAIC$null.deviance 
# 10%. This is quite normal with Bernoulli data.



## ----Appendix, eval=FALSE, echo=TRUE, results=FALSE, collapse=FALSE-----------
## fulldat<- read.delim("./data/FineScale_Dataset_GAMM_OFB2019.txt")
## 
## str(fulldat)
## 
## dat<- fulldat[fulldat$site == "Sutors", c("presence", "year", "julianday", "tideangle_deg", "mh")]
## 
## dat$mon<- as.numeric(cut(dat$julianday, seq(1, 370, by= 30.5)))
## 
## dat$tideangle_deg<- round(dat$tideangle_deg)
## # count number of data per year/month combination and represent as mosaicplot
## plot(table(dat$year, dat$mon))
## 
## # remove 2016
## dat<- dat[dat$year != 2016, ]
## 
## # Bin year into two periods (May+June vs rest of year)
## dat$Per2<- cut(dat$julianday, breaks= c(-1, 120, 180, 400),
## 			labels= c("RestOfYear", "MayJun", "RestOfYear"))
## dat$Per2<- factor(dat$Per2, levels= c("RestOfYear", "MayJun"))
## # (making "RestOfYear" the reference level)
## 
## # check this is working as intended:
## plot(as.numeric(dat$Per2) ~ dat$julianday)
## 
## # Bin year into 4 periods:
## # 3 periods of 20 days from early May to end of June vs rest of the year
## dat$Per4<- cut(dat$julianday, breaks= c(0, 120, 140, 160, 180, 400),
## 			labels= c("RestOfYear", "MayJun1", "MayJun2", "MayJun3", "RestOfYear"))
## dat$Per4<- factor(dat$Per4, levels= c("RestOfYear", "MayJun1", "MayJun2", "MayJun3"))
## # (reordering levels)
## 
## # check this is working as intended:
## plot(as.numeric(dat$Per4) ~ dat$julianday)
## 
## # Bin time of day into 6 4h periods (first centered on midnight)
## dat$Time6<- cut(dat$mh, breaks= c(-1, seq(2, 22, by= 4), 24),
## 			labels= c("MNight", "AM1", "AM2", "MDay", "PM1", "PM2", "MNight"))
## dat$Time6<- factor(dat$Time6, levels= c("MNight", "AM1", "AM2", "MDay", "PM1", "PM2"))
## # reordering chronologically
## 
## # check this is working as intended:
## table(dat$Time6, dat$mh)
## 
## # Bin tide angle into 4 quadrants with peaks in middle of respective bin
## dat$Tide4<- cut(dat$tideangle_deg, breaks= c(-1, 45, 135, 225, 315, 360),
## 			labels= c(1:4, 1))
## 
## # check this is working as intended:
## plot(as.numeric(dat$Tide4) ~ dat$tideangle_deg)
## 
## # unless you desperately want to test the performance of your computer,
## # play safe and reduce the size of the data set from 50000 to 5000:
## set.seed(74) # makes the random sampling reproducible
## # This means you will get the same random sample as the solutions to
## # the exercises and the same results.
## dat<- dat[sample(1:nrow(dat), size= 5000), ] # random subset or rows
## 
## write.csv(dat, "dolphin.csv")

