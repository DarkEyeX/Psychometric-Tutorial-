library(psych)
data<-read.csv("C:/Users/madha/OneDrive/Desktop/Psychometrics/Tutorial/psych_data.csv")
#Removing unnecessary columns
data1<-data[,-(2:5)]
##Items:
#S1 When I'm around native German speakers, I try to speak in German
#S2 I'm often needed to be able to converse in German
#S3 I will need to study German for my planned profession/ studies
#S4 I'm investing time into learning German each week
#S5 Learning German is one of my highest priorities
#S6 I want to learn German so that I can understand German media
#S7 I want to learn German to hone my hearing and speaking skills for everyday life
#S8 I want to learn German to better understand the German culture
#S9 I want to learn German for professional competency
##Covariates:
#C1 What is your age?
#C2 What is your gender?
#C3 How long have you been living in Germany?
#C4 What do you ideally intend to do after your studies?
#C5 What is your current level of German?

colnames(data1) <-c("ID","S1","S2","S3","S4","S5","S6","S7","S8","S9","C1","C2","C3","C4","C5")
titles<-paste0("S",1:9)
data2<-data1[,2:10]
cor(data2)
#no need for rev

##ITEM DIFFICULTY
range(data2[1:9])

itemdiff <- apply(data2[1:9]-1,2,mean)
itemdiff[1:9] <- itemdiff[1:9]/(5-1)
range(itemdiff)

plot(apply(data2[1:9],2,mean),ylim=c(0,7))

##ITEM VARIANCE
# this provides information about the variation in the sample
# this should be larger than zero
itemvar <- apply(data2,2,var)
#All larger than 0

##Item-Scale Correlation
itemcor <- c()
for(j in 1:length(titles)){#j<-1
  scale_no_j <- apply(data2[,titles[-j]],1,mean)
  itemcor[j] <- cor(data2[,titles[j]],scale_no_j)
}

#illustrate in a table
library(xtable)
tab1 <- data.frame(itemdiff,itemvar,itemcor)
colnames(tab1) <- c("item difficulty","item variance", "item scale correlation")
rownames(tab1) <- titles

xtable(tab1)
#for latex
round(tab1,2)

#illustrate with a plot
par(mfrow=c(3,1))
plot(itemdiff[1:9],axes=F,type="b",ylim=c(0,1.5),xlab="item",ylab="item difficulty")
axis(2)
axis(1,1:9,titles)
abline(h=c(.1,.5,.9),lty=2)

plot(itemvar[1:9],axes=F,type="b",ylim=c(0,5),xlab="item",ylab="item variance")
axis(2)
axis(1,1:9,titles)
#check 7 and 8: low variance, most people might just agree and could be less informative
plot(itemcor,axes=F,type="b",ylim=c(0,1),xlab="item",ylab="item scale correlation")
axis(2)
axis(1,1:9,titles)
abline(h=c(.3),lty=2)
#check 2, not very correlated to the rest

###############################################################
# Part 1: Exploratory factor analysis for scale 1 and 2
###############################################################

###############################################################
# (a) number of factors
###############################################################

###############################################################
fa1 <- fa.parallel(data2[,titles],fa="fa")
abline(h=mean(fa1$fa.values),lty=2,col="purple")
#KG:2
#PA:1
#Elbow:1/2

fa1.ml1 <- fa(data2[,titles],nfactors=1,rotate="none",covar=F,fm="ml")
fa1.ml2 <- fa(data2[,titles],nfactors=2,rotate="none",covar=F,fm="ml")
anova(fa1.ml1,fa1.ml2)
# conclusion: one factor model is preferred over two factor model (chisq/df and BIC)

# check normality
library(MVN)
mvn(data2[,titles],multivariatePlot="qq", showOutliers = TRUE)
#ask how to analyse
md <- mahalanobis(data2,apply(data2,2,mean),cov(data2))
#checking outliers
which(md>16) #2
which(md<4.5) #12, 24

##########################################################################
# (b) residual matrix: S-Sigma
##########################################################################
# run relevant models with principal axis "pa"
fa1.pfa1 <- fa(data2[,titles],nfactors=1,rotate="none",covar=F,fm="pa")
fa1.pfa2 <- fa(data2[,titles],nfactors=2,rotate="none",covar=F,fm="pa")

round(fa1.pfa1$residual,2) # diagonal elements are 1-h (==unique variance, lower is better)
round(fa1.pfa2$residual,2)
# two factors better?

###############################################################
# (c) rotation of factors to simple structure
###############################################################
library(GPArotation) # there are different options to rotate with fa, but I prefer this one here
#####################################
################################
# Rotation plots
# this is a helpful plot for 2-factor solutions
################################

pl.fa2 <- function(dat,main,fa2,col="black"){
  # plot the original factor loadings
  plot(fa2$loadings[,1],fa2$loadings[,2],axes=F,xlim=c(-1,1),ylim=c(-1,1),
       xlab="",ylab="",main=main)
  # this adds the number of the items
  for(j in 1:length(fa2$loadings[,1])){
    text(fa2$loadings[j,1],fa2$loadings[j,2]+.075,paste0(j))
  }
  
  # these are the unrotated factors 
  abline(h=0,lty=2)
  abline(v=0,lty=2)
  
  # these are the rotated factors
  lines(c(-dat$Th[1,1],dat$Th[1,1]),c(-dat$Th[2,1],dat$Th[2,1]),col=col)
  lines(c(-dat$Th[1,2],dat$Th[1,2]),c(-dat$Th[2,2],dat$Th[2,2]),col=col)
  
  # this adds some text
  text(dat$Th[1,1],dat$Th[2,1],"Factor 1")
  text(dat$Th[1,2],dat$Th[2,2],"Factor 2")
  
}
####################################################################
# no rotation: Just as a starting value
fa1.start <- fa(data2[,titles],nfactors=2,rotate="none",fm="pa")
# extract factor loadings
L1 <- fa1.start$loadings


# orthogonal rotations
vmax1 <- Varimax(L=L1)
# direct oblique: delta is here gam
obli <- oblimin(L=L1)

# plot to easier interpret the solutions
par(mfrow=c(1,2))
pl.fa2(vmax1,"Varimax",fa2=fa1.start)
pl.fa2(obli,"Oblimin",fa2=fa1.start)
#maybe oblimin is better
# 2(?),3,9 -> factor 1
# 1,4,6 -> factor 2
# 5,7,8,(2) -> loading on both factors

###############################################################
# Part 2: Confirmatory factor analysis for scale 1 and 2
# note: typically one should cross-validate with different samples
# because we use the results from the efa above to define the model below
###############################################################
library(lavaan)
library(semPlot)

# simplify the names of the variables for easier coding
colnames(data2) <- c(paste0("y",1:9))

# scale 1
# one factor model
model1 <- '
f1 =~ y1 + y2 + y3 + y4 + y5 + y6 + + y7 + y8 + y9
'

# two factor model
model2 <- '
f1 =~ y2 + y3 +y9   
f2 =~ y1 + y4 + y5 + y6 +y7 +y8 +y2
'

##########################################################
# run model 1
cfa1 <- cfa(model1,data2,std.lv=T)

# output
summary(cfa1,standardized=T)

# plot with different options
semPaths(cfa1, "std")

# standardized version with z-values (Check for > +1.96 or < -1.96)
cov1 <- round(resid(cfa1,type="standardized")$cov,2)
cov1
# there is some unwanted residual correlation, e.g., between items 4 and 5

##########################################################
# run model 2
cfa2 <- cfa(model2,data2,std.lv=T)

# output
summary(cfa2,standardized=T)

# plot with different options
semPaths(cfa2, "std")

# standardized version with z-values (Check for > +1.96 or < -1.96)
cov2 <- round(resid(cfa2,type="standardized")$cov,2)
cov2
#no unwanted residuals

# model comparison to 1-factor model
anova(cfa2,cfa1)
#aic and bic lower of model 1 (one factor model)

# mod indices
mi <-modindices(cfa1)
# show all MI>3.84 (significant)
mi[mi$mi>3.84,]
# only y1~~y4
#not sure if this is required because:
#y1: When I'm around native German speakers, I try to speak in German
#y4: I'm investing time into learning German each week

