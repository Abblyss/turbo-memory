#Product Choice: Multinomial Logit Model
#load required package 
install.packages("mlogit")
install.packages("dfidx")
library(mlogit)
library(dfidx)
data("Heating", package = "mlogit")
summary(Heating)
head(Heating)
View(Heating)
##ic installion cost,oc operation cost
heatingdata <- dfidx(Heating,choice = "depvar", varying = c(3:12))
print(heatingdata, n = 10)
View(heatingdata)
# build multinomial logit model without intercept
heating_m1 <- mlogit(depvar ~ 0 + ic + oc, heatingdata)
summary(heating_m1)
# Both estimated coefficients are negative. This suggests that as the cost of a 
# heating system rises (and the costs of the alternatives remain the same), the 
# probability of that system being chosen decreases. Both coefficients are 
# statistically significantly different to 0.
prop.table(table(Heating$depvar))
#gc is the most popular heating choice.
apply(fitted(heating_m1, outcome = FALSE), MARGIN = 2, FUN = mean)
# Observed frequencies and predicted shares do not match well.

wtp <- coef(heating_m1)["oc"]/coef(heating_m1)["ic"]
wtp
# Individuals are willing to pay $0.73 higher installation costs to have $1 lower
# annual operating costs.

#discount rate r
r <- 1/wtp
r
# A discount rate > 1 is not reasonable.

## model improvement 1(assume r=0.12)
#current value of operating costs cv approaches oc/r for an increasing lifetime.
heatingdata$tc <- heatingdata$ic + (heatingdata$oc / 0.12)
heating_m2 <- mlogit(depvar ~ tc | 0, heatingdata)
summary(heating_m2)

# The estimate is negative and statistically significant, suggesting higher total
# costs of a heating system are related to a lower likelihood of its purchase.

#using a likelihood ratio test to find the model with better performance
lrtest(heating_m1, heating_m2)
##the p-value is significant,so we reject H0,accept H1,m1 perform better than m2.

## model improvement 2
heating_m3 <- mlogit(depvar ~ ic + oc, heatingdata, reflevel = "hp")
summary(heating_m3)
### model evalutation
prop.table(table(Heating$depvar))
apply(fitted(heating_m3, outcome = FALSE), MARGIN = 2, FUN = mean)
##The probabilities are a perfect match. Alternative-specific constants ensure
# average probabilities equal observed shares in logit models.
wtp <- coef(heating_m3)["oc"]/coef(heating_m3)["ic"]
r <- 1/wtp
# The corresponding discount rate is 0.22,which is reasonable.

##model improvement 3
heating_m4 <- mlogit(depvar ~ oc + I(ic / income), heatingdata)
summary(heating_m4)

##model improvement 4
heating_m5 <- mlogit(depvar ~ oc + ic | income, heatingdata, reflevel = "hp")
summary(heating_m5)
lrtest(heating_m3, heating_m5)
## p value=0.32,not significant,m3 performs better than m4.


##prediction with model 3
## what if government offer a 15% rebate on the installation cost of 
# heat pumps

heatingdata_new <- heatingdata
head(heatingdata)
heatingdata_new[idx(heatingdata_new, 2) == "hp", "ic"] <- 
  0.85 * heatingdata_new[idx(heatingdata_new, 2) == "hp", "ic"]
apply(fitted(heating_m3, outcome = FALSE), 2, mean)
apply(predict(heating_m3, newdata = heatingdata_new), 2, mean)
##15% rebate on installation cost of heat pump raise 1.4% .

