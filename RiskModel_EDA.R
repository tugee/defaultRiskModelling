testSet <- readRDS("CardT.rds")
validationSet <- readRDS("CardV.rds")

summary(testSet)

# Analyse missing values

testSet <- testSet[!is.na(testSet$EDUCATION)&!is.na(testSet$MARRIAGE),]
validationSet <- validationSet[!is.na(validationSet$EDUCATION)&!is.na(validationSet$MARRIAGE),]
attach(testSet)
ggplot(testSet) + geom_bar(aes(x = PAY_1))

# Study the univariate distribution of continuous variables
comparison <- par(mfrow = c(2,2))
par(comparison)
hist(testSet$PAY_AMT1,col="blue",main="Histogram of amount paid in SEPT",xlab="Amount paid in September",breaks = 40,xlim=c(0,300000))
hist(rowMeans(testSet[,18:23]),main="Histogram of mean amount paid",xlab="Mean amount paid",col = "blue",breaks = 40)

hist(testSet$BILL_AMT1,col="blue",main="Histogram of bill statement in SEPT",xlab="Bill statement in September",breaks = 40)
hist(rowMeans(testSet[,12:17]) ,col="blue",main="Histogram of mean bill statement",xlab="Mean bill statement",breaks = 50)

## Initial plot of all continuous variables

hist(testSet$LIMIT_BAL,col = "blue", main ="Histogram of credit limit", xlab ="Credit limit (NT$)",breaks = 30)
hist(testSet$PAY_AMT1,col="blue",main="Histogram of amount paid (SEPT)",xlab ="Amount paid in September (NT$)",breaks = 40,xlim=c(0,300000))
hist(testSet$BILL_AMT1,col="blue",main="Histogram of amount of bill statement (SEPT)",xlab ="Bill statement in April (NT$)",breaks = 40)
hist(testSet$AGE, col="blue",main="Histogram of AGE", xlab = "Years", breaks = 10)

barplot(prop.table(summary(EDUCATION)),col = rainbow(4),ylab="Total proportion",main = "Proportion of level of education")
barplot(prop.table(summary(SEX)),col=c("blue","red"), main = "Sex of customers",ylab = "Total proportion")
barplot(prop.table(summary(MARRIAGE)), col = rainbow(3), main = "Distribution of marital status")
barplot(prop.table(summary(PAY_1)), main = "Repayment status in SEPT", col = rainbow(5), xlab= "Category of repayment status")

##
# Univariate analysis for outliers
##
par(mfrow=c(1,3))
Boxplot(PAY_AMT1,col = "blue",ylab = "Amount paid in September (NT$)")
Boxplot(BILL_AMT1,col = "blue",ylab = )
Boxplot(LIMIT_BAL)

##
# Bivariate plots
##

barplot(prop.table(summary(default)),col = c("green","red"),names.arg = c("No","Yes"),main = "Proportion of clients who default")

# Since we know the different months are highly co-linear, we will replace them with mean variables, the mean amount paid per month and maximum paid per month

## Pipeline for transformed credit card information

cards <- testSet
## Remove lagged variables
cards <- cards[,-(12:23)]

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

gt <- function(x){
  any(x>0)
}

temp <- cbind(as.numeric(levels(testSet$PAY_1))[testSet$PAY_1],as.numeric(levels(testSet$PAY_2))[testSet$PAY_2])
temp <- cbind(temp,as.numeric(levels(testSet$PAY_3))[testSet$PAY_3])
temp <- cbind(temp,as.numeric(levels(testSet$PAY_4))[testSet$PAY_4])
temp <- cbind(temp,as.numeric(levels(testSet$PAY_5))[testSet$PAY_5])
temp <- cbind(temp,as.numeric(levels(testSet$PAY_6))[testSet$PAY_6])
paymentDelay <- apply(temp,1,gt)


dueMean <- rowMeans(testSet[,12:17])
paidMean <- rowMeans(testSet[,18:23])
dueMeanNormed <- dueMean/LIMIT_BAL
paidMeanNormed <- paidMean/LIMIT_BAL

paymentMode <- apply(testSet[,6:11],1,getmode)
library(forcats)
paymentMode<-fct_collapse(paymentMode, "-1" = c("-1"), "-2" = c("-2"), "0" = c("0"), "Overdue" = c("1","2","3","4","5","6","7","8","9"))

cards <- cbind(cards,paymentDelay)
cards <- cbind(cards,paymentMode)
cards <- cbind(cards,dueMean)
cards <- cbind(cards,paidMean)
cards <- cbind(cards,dueMeanNormed)
cards <- cbind(cards,paidMeanNormed)

cards <- cards[,-c(6:11)]
attach(cards)
cards <- cards[,c(1:5,7:12,6)]

ggpairs(testSet[,c(12:14,19:21,24)],mapping=aes(color=default,alpha=0.4),
        lower = list(continuous= mod_points,combo=mod_bihist),
        diag=list(continuous=modified_density,discrete=mod_bar),
        upper=list(continuous=mod_cor,combo=mod_box))

ggpairs(testSet[,c(18:23,24)],mapping=aes(color=default,alpha=0.4),
        lower = list(continuous= mod_points,combo=mod_bihist),
        diag=list(continuous=modified_density,discrete=mod_bar),
        upper=list(continuous=mod_cor,combo=mod_box))

ggpairs(cards[,c(1,8:12)],mapping=aes(color=default,alpha=0.4),
        lower = list(continuous= mod_points,combo=mod_bihist),
        diag=list(continuous=modified_density,discrete=mod_bar),
        upper=list(continuous=mod_cor,combo=mod_box))

##
# Empirical logit plots
##

myemplogit <- function(yvar=y,xvar=x,maxbins=10,sc=1,line=TRUE,...){
  breaks  <<- unique(quantile(xvar, probs=0:maxbins/maxbins))
  levs  <<- (cut(xvar, breaks, include.lowest=FALSE))
  num <<- as.numeric(levs)
  c.tab <- count(num,'levs')
  c.tab$levs <- factor(c.tab$levs, levels = levels(addNA(c.tab$levs)), labels = c(levels(c.tab$levs),
                                                                                  paste("[",min(xvar),"]",sep="")), exclude = NULL)
  c.tab <- c.tab[c(nrow(c.tab),1:nrow(c.tab)-1),]
  sc <- (max(c.tab$freq)/min(c.tab$freq)/sc)^2
  zcex <<- sqrt(c.tab$freq/pi)/sc
  print(c.tab);print(zcex);print(sc)
  emplogitplot1(yvar~xvar,breaks=breaks,cex=zcex,showline=line,...)
}

par(mfrow=c(2,2),mgp=c(1.5,0.5,0),mar=c(3,3,1.2,0.5))
myemplogit(default,(LIMIT_BAL),100,sc=20,xlab="Credit limit",line=TRUE)
myemplogit(default,AGE,30,sc=50,xlab="Age",line=TRUE)
myemplogit(default,dueMeanNormed,100,sc=100,xlab="Mean bill statement per month as % of credit limit",line=TRUE)
myemplogit(default,paidMeanNormed,100,sc=5,xlab="Mean amount paid per month as % of credit limit",line=TRUE)
myemplogit(default,log((paidMean+1)),40,sc=1,xlab="Mean amount paid per month as % of credit limit",line=TRUE)

cards <- transform(cards, log_paidMean = log(paidMean+1))
cards$paidMean <- NULL
cards <- cards[,c(1:10,12,11)]

##
# Fitting the model
##


library(glmnet)

fit1 <- glm(default~.,family=binomial,data = cards)
summary(fit1)
Anova(fit1)

## Backwards elimination of the least significant term

fit2 <- glm(default~. -LIMIT_BAL, family = binomial, data = cards)
Anova(fit2)

fit3 <- glm(default~. -LIMIT_BAL - paidMeanNormed, family = binomial, data = cards)
Anova(fit3)

fit4 <- glm(default~. -LIMIT_BAL - paidMeanNormed -MARRIAGE, family = binomial, data = cards)
Anova(fit4)

fit5 <- glm(default~. -LIMIT_BAL - paidMeanNormed -MARRIAGE - dueMean, family = binomial, data = cards)
Anova(fit5)

fit6 <- glm(default~. -LIMIT_BAL - paidMeanNormed -MARRIAGE - dueMean -AGE, family = binomial, data = cards)
Anova(fit6)

fit7 <- glm(default ~ SEX+paymentDelay+paymentMode+log_paidMean+dueMeanNormed, family = binomial,data = cards)
#DO we remove sex?
fit8 <- glm(default ~ EDUCATION+paymentDelay+paymentMode+log_paidMean+dueMeanNormed,family = binomial, data = cards)
anova(fit8,fit7,test="Chisq") #Choose model with SEX, doesnt have a massive coefficient for "EDUCATION" other, which might be caused by an anomaly in the training data
Anova(fit7)
summary(fit7)

# Group LASSO

x_train <- model.matrix( ~ .-1, cards[,-12])
group1 <- c(1,2,2,3,3,3,4,4,5,6,7,7,7,8,9,10,11)

lso1 <- oem(x = x_train, y = as.numeric(as.character(cards$default)), family="binomial",
                   penalty = c("lasso", "grp.lasso"), 
                   groups = group1)

groupLSO <- cv.oem(x = x_train, y = as.numeric(as.character(cards$default)), family="binomial",
                penalty = c("lasso", "grp.lasso"), 
                groups = group1,nfolds = 10)

predict(lso1, s = groupLSO$lambda.min.models[2], which=2,type="coefficients")
predict(lso1, s = groupLSO$lambda.1se.models[2], which=2,type="coefficients")

plot(lso1, which.model = 2, xvar = "loglambda", main="Group lasso", ylim = c(-2,1.5))
abline(v=log(groupLSO$lambda.1se.models[2]),col="red")
abline(v=log(groupLSO$lambda.min.models[2]),col="blue")
legend("topleft",legend=c("Minimum lambda", "1 standard error larger lambda"),lty=c(1,1),col=c("blue","red"), ins=0.05)

##
# Diagnostic plots
##

finalModel <- fit7

confint(finalModel)

# VIF
vif(finalModel)

# Influence
influenceIndexPlot(finalModel)

residualPlots(finalModel)
crPlots(finalModel)
marginalModelPlots(finalModel)

plot(allEffects(finalModel))

arm::binnedplot(x=predict(finalModel,type="response"),y=rstudent(finalModel,type="pearson"),nclass=40,col.int=NA, main ="Binned Student Pearson Residuals") # student residual and 40 bins
arm::binnedplot(predict(finalModel),rstandard(finalModel,type="pearson"),nclass=60,col.int=NA, main ="Binned Standardised Pearson Residuals") # student residual and 40 bins
arm::binnedplot(predict(finalModel),rstandard(finalModel,type="deviance"),nclass=40,col.int=NA, main ="Binned Standardised Deviance Residuals") # student residual and 40 bins
arm::binnedplot(x=log_paidMean,y=rstudent(finalModel,type="deviance"),nclass=100,col.int=NA,xlab="Glucose Concentration", main ="Binned Student Deviance Residuals against glucose")
arm::binnedplot(x=dueMean,y=rstudent(finalModel,type="deviance"),nclass=100,col.int=NA,xlab="Triceps Skin Fold Thickness (mm)", main ="Binned Student Deviance Residuals against skinthickness")

## HNP

par(mfrow=c(1,1),mgp=c(1.7,0.5,0),mar=c(3.5,3.5,3,0.5)) 

hnp(finalModel,resid.type="deviance",ylab="Deviance Residuals")
hnp(finalModel,resid.type="pearson", ylab="Pearson Residuals")

## dfbetas
dfbetasPlots(finalModel,intercept = TRUE,id.n=3)


par(mfrow=c(1,1),mgp=c(1.7,0.5,0),mar=c(3.5,3.5,3,0.5)) 
hist(predict(finalModel))
hist(rstudent(finalModel))

## Validation set pipeline

cardsV <- validationSet
cardsV <- cardsV[,-(12:23)]


temp <- cbind(as.numeric(levels(validationSet$PAY_1))[validationSet$PAY_1],as.numeric(levels(validationSet$PAY_2))[validationSet$PAY_2])
temp <- cbind(temp,as.numeric(levels(validationSet$PAY_3))[validationSet$PAY_3])
temp <- cbind(temp,as.numeric(levels(validationSet$PAY_4))[validationSet$PAY_4])
temp <- cbind(temp,as.numeric(levels(validationSet$PAY_5))[validationSet$PAY_5])
temp <- cbind(temp,as.numeric(levels(validationSet$PAY_6))[validationSet$PAY_6])

paymentDelay <- apply(temp,1,gt)
cardsV <- cbind(cardsV,paymentDelay)

dueMean <- rowMeans(validationSet[,12:17])
paidMean <- rowMeans(validationSet[,18:23])
paymentMode <- apply(validationSet[,6:11],1,getmode)
dueMeanNormed <- dueMean/validationSet$LIMIT_BAL
paidMeanNormed <- paidMean/validationSet$LIMIT_BAL
library(forcats)
paymentMode<-fct_collapse(paymentMode, "-1" = c("-1"), "-2" = c("-2"), "0" = c("0"), "Overdue" = c("1","2","3","4","5","6","7","8","9"))


cardsV <- cbind(cardsV,paymentMode)
cardsV <- cbind(cardsV,dueMean)
cardsV <- cbind(cardsV,paidMean)
cardsV <- cbind(cardsV,paidMeanNormed)
cardsV <- cbind(cardsV,dueMeanNormed)
cardsV <- cardsV[,-c(6:11)]

cardsV <- transform(cardsV, log_paidMean = log(paidMean+1))
cardsV <- cardsV[,-c(1:5)]

## Validation 

predictedV <- predict(finalModel, type='response',newdata = cardsV)
boxplot(predictedV~cardsV$default, col="blue")

ypred <- predictedV > 0.10
addmargins(table(cardsV$default, ypred))
ylassopred <- predictedV > 0.10
addmargins(table(cardsV$default,ylassopred))

# Exactly the same margins here, therefore same FPR and FNR

## Accuracy 
(1072+233)/2051

#FNR
68/(68+233)
#FPR
688/(1072+688)
x_trainV <- model.matrix( ~ .-1, cardsV[,-6])

predictedLassoValidation <- predict(lso1, s = groupLSO$lambda.1se.models[2], which=2,newx = x_trainV,type="response")
predictedLassoTrain <- predict(lso1, s = groupLSO$lambda.1se.models[2], which=2,type="response",newx = x_train)

#PLOT MULTI ROC
plot.roc(cardsV$default,predict(finalModel,type="response",newdata=cardsV),ci=TRUE,of="thresholds",ci.type="shape",print.auc=TRUE, main=" Both ROC")
plot.roc(cards$default,predict(finalModel, type='response'),add=TRUE,col="red",print.auc=TRUE, print.auc.x= 0.5, print.auc.y=0.3,print.auc.col="red")
plot.roc(cardsV$default,predictedLassoValidation,add=TRUE,col="green",print.auc=TRUE, print.auc.x= 0.9, print.auc.y=0.3,print.auc.col="green")
plot.roc(cards$default,predictedLassoTrain,add=TRUE,col="blue",print.auc=TRUE, print.auc.x= 0.5, print.auc.y=0.1,print.auc.col="blue")
legend("bottomright",legend=c("Training","Validation","LASSO Training","LASSO Validation"),col=c("red","black","green","blue"),lty=c(1,1),inset=c(0.05,0.05)) # Add a legend

