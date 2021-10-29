testSet <- readRDS("CardT.rds")
validationSet <- readRDS("CardV.rds")

## Univariate EDA
summary(testSet)

hist(testSet$LIMIT_BAL)
hist(testSet)

summary(testSet[is.na(testSet$EDUCATION),])
boxplot(testSet[is.na(testSet$EDUCATION),1])
boxplot(testSet[-is.na(testSet$EDUCATION),1])

for(nam in 12:23) {
  y <- testSet[, nam]
  plot(density(y), main="", xlab=nam)
}

mask <- testSet$default == 0

library(reshape)
defaults <- testSet[, c("default","BILL_AMT1","PAY_AMT1")]
as.data.frame(defaults)
melt(defaults, id = "BILL_AMT1")
library(ggplot2)
ggplot(aes(x=value, group="default", col="default"), data=defaults) +
  geom_density() 

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

gt <- function(x){
  any(x>0)
}

cards <- testSet
cards <- cards[,-(12:23)]

cardsV <- validationSet
cardsV <- cardsV[,-(12:23)]



temp <- cbind(as.numeric(levels(testSet$PAY_1))[testSet$PAY_1],as.numeric(levels(testSet$PAY_2))[testSet$PAY_2])
temp <- cbind(temp,as.numeric(levels(testSet$PAY_3))[testSet$PAY_3])
temp <- cbind(temp,as.numeric(levels(testSet$PAY_4))[testSet$PAY_4])
temp <- cbind(temp,as.numeric(levels(testSet$PAY_5))[testSet$PAY_5])
temp <- cbind(temp,as.numeric(levels(testSet$PAY_6))[testSet$PAY_6])
paymentDelay <- apply(temp,1,gt)
cards <- cbind(cards,paymentDelay)

temp <- cbind(as.numeric(levels(validationSet$PAY_1))[validationSet$PAY_1],as.numeric(levels(validationSet$PAY_2))[validationSet$PAY_2])
temp <- cbind(temp,as.numeric(levels(validationSet$PAY_3))[validationSet$PAY_3])
temp <- cbind(temp,as.numeric(levels(validationSet$PAY_4))[validationSet$PAY_4])
temp <- cbind(temp,as.numeric(levels(validationSet$PAY_5))[validationSet$PAY_5])
temp <- cbind(temp,as.numeric(levels(validationSet$PAY_6))[validationSet$PAY_6])
paymentDelay <- apply(temp,1,gt)
cardsV <- cbind(cardsV,paymentDelay)


dueMean <- rowMeans(testSet[,12:17])
paidMean <- rowMeans(testSet[,18:23])
dueMax <- apply(testSet[,12:17],1,max)
paidMax <- apply(testSet[,18:23],1,max)
paymentMode <- apply(testSet[,6:11],1,getmode)

cards <- cbind(cards,paymentMode)
cards <- cbind(cards,dueMean)
cards <- cbind(cards,dueMax)
cards <- cbind(cards,paidMean)
cards <- cbind(cards,paidMax)


dueMean <- rowMeans(validationSet[,12:17])
paidMean <- rowMeans(validationSet[,18:23])
dueMax <- apply(validationSet[,12:17],1,max)
paidMax <- apply(validationSet[,18:23],1,max)
paymentMode <- apply(validationSet[,6:11],1,getmode)

cardsV <- cbind(cardsV,paymentMode)
cardsV <- cbind(cardsV,dueMean)
cardsV <- cbind(cardsV,dueMax)
cardsV <- cbind(cardsV,paidMean)
cardsV <- cbind(cardsV,paidMax)

cardsT <- cards[,-c(6:11)]

## Boxplots for days
plot(default,col="blue", xlab="Diagnosed with Diabetes")
Boxplot(log(PAY_AMT1+2)~default,
        xlab = "Default on bills",
        ylab = "Number of Pregnacies", col="blue")

Boxplot(cards$paidMean~default,
        xlab = "Default on bills",
        ylab = "Amount paid", col="blue")

Boxplot(cards$dueMean~default,
        xlab = "Default on bills",
        ylab = "mean due bills", col="blue")

Boxplot(cards$paidMax~default,
        xlab = "Default on bills",
        ylab = "max Amount paid", col="blue")

Boxplot(cards$dueMax~default,
        xlab = "Default on bills",
        ylab = "max due bills", col="blue")


## Figure 3 - Bivariate relationships
library(gclus)
dta <- cardsT[,c(1,5,9,10,11,12)] # get data
dta.r <- abs(cor(dta, method="spearman")) # get correlations
dta.col <- dmat.color(dta.r) # get colors
## reorder variables so those with highest correlation are closest to the diagonal
dta.o <- order.single(dta.r)
cpairs(dta, dta.o, panel.colors=dta.col, gap=.5, )


################################
#                              #
#       BLOCK XIII             #
#       fit Model              #
#                              #
################################
fit0 <- glm(default~(1),family=binomial)
summary(fit0)
fit1 <- glm(default~paymentDelay+paymentMode+dueMean+dueMax+paidMean+paidMax+AGE+LIMIT_BAL,family=binomial,data = cards)
summary(fit1)

