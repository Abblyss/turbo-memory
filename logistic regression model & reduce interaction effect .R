install.packages("vcd")
install.packages("gnm")
install.packages("vcdExtra")
install.packages("vcd")
install.packages("mlogit")
install.packages("dfidx")
install.packages("lmtest")
install.packages("expm")
library(vcdExtra) # to expand data tables
library(vcd) # for mosaic plot
library(mlogit) # for multinomial logit models & heating data set
library(dfidx) # to index a data set
library(lmtest) # for likelihood ratio test
library(expm)  # for matrix exponentiation
##logistic regression 1
# We will work with sales data on season passes to an amusement park. Thereby, 
# the sales differ according to two factors: the channel (email, postal mail, in-
# person) used to deliver the promotion and whether the promotion included the 
# ticket in a bundle with free parking or not.
# Bought season pass (count): 
#      Bundle No Bundle
# Mail    242       359
# Park    639       284 
# Email    38        27

# Did not buy season pass (count): 
#      Bundle No Bundle
# Mail    449       278
# Park    223        49 
# Email    83       485
#generate data
pass_tab <- c(242, 639, 38, 359, 284, 27, 449, 223, 83, 278, 49, 485)
dim(pass_tab) <- c(3, 2, 2)
dimnames(pass_tab) <- list(Channel = c("Mail", "Park", "Email"),
                           Offer = c("Bundle", "NoBundle"),
                           Ticket = c("Pass", "NoPass") )
class(pass_tab) <- "table"
pass_df <- expand.dft(pass_tab)
str(pass_df)
View(pass_df)
pass_df2 <- pass_df[sample(nrow(pass_df)),]
View(pass_df2)
table(pass_df$Ticket, pass_df$Offer)
#	Using logistic regression, estimate whether the promotion of the bundle 
# (season pass + free parking) has an effect on the sales of season passes to 
# the amusement park. 
str(pass_df$Offer)
pass_df$Offer <- factor(pass_df$Offer , levels = c("NoBundle", "Bundle"))
str(pass_df$Ticket)
pass_df$Ticket<-factor(pass_df$Ticket,levels=c("NoPass","Pass"))
table(pass_df$Ticket, pass_df$Offer)
str(pass_df$Offer)
pass_m1 <- glm(Ticket ~ Offer, data = pass_df, family = binomial) 
summary(pass_m1)
#conclusion: 
#Individuals that receive the bundled offer are more likely to purchase the 
# season pass, since the offer coefficient is positive (0.389) and statistically 
# significant.
exp(0.389)
# The effect of the bundle equals an estimated odds ratio of 1.476. This suggests 
# individuals are 1.476 times more likely to purchase the season pass if it is 
# offered in a bundle with free parking.
pass_m2 <- glm(Ticket ~ Offer + Channel, data = pass_df, family = binomial)
summary(pass_m2)
exp(coef(pass_m2))
# A calculation of the odds ratios and the corresponding confidence intervals 
# suggest that individuals are 41 times more likely to purchase a season 
# pass at the park directly. Offering the bundle to individuals is associated 
# with a 32% - 52% lower likelihood of purchase. we need to find out the reason
# for the change of sign.
doubledecker(table(pass_df))
##This chart shows that there are interaction effect between Channel and Offer.
#This may be the reason for the change of sign.We need to remove the effect.
pass_m3 <- glm(Ticket ~ Offer*Channel, 
                   data = pass_df, family = binomial)
summary(pass_m3)
##  Offer now have a positive impact on Ticket and the decreas of AIC(3498->3394) indicates
# that the model now have a better fit.
exp(confint(pass_m3))
