## ----Q2, eval=TRUE, echo=TRUE, results=TRUE, collapse=FALSE-------------------

dat<- read.csv("./data/dolphin.csv", stringsAsFactors= T)

# re-ordering factor levels for convenience:
dat$Per2<- factor(dat$Per2, levels= c("RestOfYear", "MayJun"))
# (making "RestOfYear" the reference level)
dat$Per4<- factor(dat$Per4, levels= c("RestOfYear", "MayJun1", "MayJun2", "MayJun3"))
dat$Time6<- factor(dat$Time6, levels= c("MNight", "AM1", "AM2", "MDay", "PM1", "PM2"))
# reordering chronologically

str(dat)




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









## ----Q7a, eval=TRUE, echo=TRUE, results=SOLUTIONS, collapse=TRUE--------------
# Please take the time to think before unfolding the next code chunk



## ----Q7b, eval=TRUE, echo=TRUE, results=SOLUTIONS, collapse=TRUE--------------
# there are several ways the non-linearity could be addressed. 
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

