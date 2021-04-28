library(ggplot2)

df <- read.csv("football.csv")

fit <- lm(y~x2+x7+x8, data = df) 

#a
#i & ii
library(MASS)
stdres(fit)
barplot(height = studres(fit), names.arg = 1:28, 
        main = "Studentized Residuals", xlab = "Index", 
        ylab = "Studentized Resid", ylim=c(-5,5))
abline(h=3, col = "Red", lwd=3)
abline(h=-3, col = "Red", lwd=3)

studres(fit)
barplot(height = studres(fit), names.arg = 1:28, 
        main = "Studentized Residuals", xlab = "Index", 
        ylab = "Studentized Resid", ylim=c(-5,5))
abline(h=3, col = "Red", lwd=3)
abline(h=-3, col = "Red", lwd=3)


rstudent(fit)
barplot(height = rstudent(fit), names.arg = 1:28, 
        main = "R Student Residuals", xlab = "Index", 
        ylab = "R Student Resid", ylim=c(-5,5))

#iii
# we can see from these graphs that there are no real outliers. All of the residuals are 
# within +-3 and most are within +-2. From this can tell that the data is stable
# however if I had to pick out some oddities, I would say the first few residuals seems to 
# have the highest values

#b
#i
influence.measures(fit)
#ii
dfbetaPlots(fit, intercept = TRUE)
influenceIndexPlot(fit)
# iii
# we clearly see from the DFBETA graphs that only the intercept
# observations has real influence on the regression coefficient.
# and from the cook's D plot, we can see that all the points are below 1, so
# it does not require any more attention

#c
library(car)
vif(fit)
# there doesnt seem to be any multicollinearity as the values are all low(<10)

#d
par(mfrow=c(1,2))
hist(studres(fit), breaks=10, freq=F, col="cornflowerblue", cex.axis=1.5, cex.lab=1.5, cex.main=2)
qqPlot(fit)
# we can see that point 1 and 21 are leverage points, but not in any super significant way, as 
# the distribution is well spread overall with a slight concentration in the middle. 
# I would also add that the plot of residuals seems to be pointing to a nonlinearity, as the bow is prevelant in the graph. 

# e
residualPlots(fit, type = "rstudent", fitted = F, quadratic = F, col = "dodgerblue", pch=16, cex=1.5, cex.axis=1.5, cex.lab=1.5)
# From these graphs, it seems like the x2 residuals seem normal and good, but x7 and x8 residuals will definetly need transformations
# as the x7 graph shows a thin left side and a growing right side, while the x8 graph shows a thin left and right side with a 
# thick middle. 
