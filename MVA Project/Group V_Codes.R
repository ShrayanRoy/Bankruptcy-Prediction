#==============================================================================================
#Statistical Analysis of Bankrupt firms
#Based on Financial Indicators
#Group- V: Adrija Saha, Sampurna Mondal, Shrayan Roy
#Date: 12/04/2023

#==============================================================================================


rm(list = ls(all = T))  #removes all objects

#required packages !
library(ggplot2)
library(lattice)
library(car)
library(Matrix)
library(reshape2)
library(ggside)
library(tidyquant)
library(wesanderson)
library(aplpack)
library(GGally)
library(biotools)
library(klaR)
library(psych)

#==============================================================================================

#ploting theme
defined_theme <- theme(plot.subtitle = element_text(family = "mono",size = 11,
                                                    face = "bold",hjust = 0.01),axis.title = element_text(family = "serif"),
                       axis.text = element_text(size = 10),plot.title = element_text(family = "serif",
                      colour = "red", hjust = -0.01),legend.text = element_text(size = 10,family = "serif"), 
                       legend.title = element_text(family = "serif"),legend.background = element_blank(),
                       legend.box.background = element_rect(colour = "black")) + 
    theme(strip.background = element_rect(fill = "#FFE5B4"))


#==============================================================================================

setwd("C:/Users/hp/Desktop/College_ISI/Multivariate Analysis Project")

My.data <- read.table("bankruptcy.txt")
#cash flow to total debt - CFTD
#net income to total assets - NITA
#current assets to total liabilities - CATL
#current assets to net sales - CANS
#Bankruptcy Status - y (Response)
colnames(My.data) <- c("CFTD","NITA","CATL","CANS","y")
str(My.data)
summary(My.data)

sum(My.data$y) #Number of not Bankrupt Firms
sum(!My.data$y) #Number of Bankrupt Firms

#===============================================================================================
My.data0 <- My.data[My.data$y == 0,]  #Data for Bankrupt Firms
My.data1 <- My.data[My.data$y == 1,]  #Data for Not Bankrupt Firms

#===============================================================================================

graph.data <- as.data.frame(cbind(melt(My.data[,-5]),y = My.data$y,Index = ifelse(My.data$y == 0,"Bankrupt","Non-Bankrupt")))
graph.data0 <- as.data.frame(cbind(melt(My.data0[,-5])))
graph.data1 <- as.data.frame(cbind(melt(My.data1[,-5])))
Index <- ifelse(My.data$y == 0,"Bankrupt","Non-Bankrupt")


#y vs. Covariates
ggplot(graph.data,aes(y = y,x = value,col = variable)) + geom_point(size = 2.5) + 
  geom_smooth(se = FALSE) + scale_y_discrete(limits = c(0,1)) + 
  labs(x = "",y = "Bankruptcy Status",title = "Scatterplot of y vs. Covarites") + 
  facet_wrap(.~variable,scales = "free") + 
  theme_bw(14) + defined_theme 

#Boxplot
ggplot(graph.data,aes(x = value)) + geom_boxplot(aes(fill = Index),outlier.shape = 4,alpha = 0.7,
                                                 outlier.colour = "red",outlier.size = 2,outlier.stroke = 1.5) + 
  facet_wrap(.~variable,scales = "free") + labs(y = "",title = "BoxPlot of Co-variates of Bankruptcy Data") + 
  theme_bw(14) + defined_theme +   scale_fill_manual(values = c("hotpink","skyblue1"))


#Histogram
ggpairs(My.data[,-5], 
        mapping = ggplot2::aes(color = factor(My.data[,5])),
        lower = list(continuous = wrap("points",size = 2.5)),
        diag = list(continuous = wrap("barDiag", bins = 10,alpha = 0.7,position = "identity",col = "black")),
        upper = list(continuous = wrap("cor",size = 6.5) )) +
  theme_bw(14) + defined_theme + scale_color_manual(values = c("hotpink","deepskyblue3")) + 
  scale_fill_manual(values = c("hotpink","skyblue1")) 


#Density
ggpairs(My.data[,-5], 
        mapping = ggplot2::aes(color = factor(My.data[,5])),
        lower = list(continuous = wrap("points",size = 2.5)),
        diag = list(continuous = wrap("densityDiag",alpha = 0.7)),
        upper = list(continuous = wrap("cor",size = 6.5) )) +
  theme_bw(14) + defined_theme + scale_color_manual(values = c("hotpink","deepskyblue3")) + 
  scale_fill_manual(values = c("hotpink","skyblue1")) 


#Correlation Plot
corrplot::corrplot(cor(My.data0[,-5]))
corrplot::corrplot(cor(My.data1[,-5]))

#qqplot
par(mfrow = c(2,2))

#Bankrupt
qqPlot(My.data0$CFTD,xlab = "Normal(0,1) Quantiles",ylab = "Observed Quantiles of CFTD",
       main = "QQ Plot of CFTD",col = "red",pch = 19)
qqPlot(My.data0$NITA,xlab = "Normal(0,1) Quantiles",ylab = "Observed Quantiles of NITA",
       main = "QQ Plot of NITA",col = "red",pch = 19)
qqPlot(My.data0$CATL,xlab = "Normal(0,1) Quantiles",ylab = "Observed Quantiles of CATL",
       main = "QQ Plot of CATL",col = "red",pch = 19)
qqPlot(My.data0$CANS,xlab = "Normal(0,1) Quantiles",ylab = "Observed Quantiles of CANS",
       main = "QQ Plot of CANS",col = "red",pch = 19)

#Non-Bankrupt
qqPlot(My.data1$CFTD,xlab = "Normal(0,1) Quantiles",ylab = "Observed Quantiles of CFTD",
       main = "QQ Plot of CFTD",pch = 19)
qqPlot(My.data1$NITA,xlab = "Normal(0,1) Quantiles",ylab = "Observed Quantiles of NITA",
       main = "QQ Plot of NITA",pch = 19)
qqPlot(My.data1$CATL,xlab = "Normal(0,1) Quantiles",ylab = "Observed Quantiles of CATL",
       main = "QQ Plot of CATL",pch = 19)
qqPlot(My.data1$CANS,xlab = "Normal(0,1) Quantiles",ylab = "Observed Quantiles of CANS",
       main = "QQ Plot of CANS",pch = 19)

par(mfrow = c(1,1))

#Shapiro Wilk Test for Normality

apply(My.data0[,-5], 2, function(data){shapiro.test(data)})
apply(My.data1[,-5], 2, function(data){shapiro.test(data)})


#Let's check for Multivariate Normality using chi-square plot
md.1 <- mahalanobis(My.data0[,-5],colMeans(My.data0[,-5]),cov(My.data0[,-5]))
md.2 <- mahalanobis(My.data1[,-5],colMeans(My.data1[,-5]),cov(My.data1[,-5]))

par(mfrow = c(2,1))
qqPlot(md.1,"chisq",df = 4,main = "QQ Plot of Sqaured Mahalanobis Distance for Bankrupt Firms",
       pch = 19,col = 'red',ylab = "Mahalanobis Distance",xlab = "Chisquare(4) Quantiles")

qqPlot(md.2,"chisq",df = 4,main = "QQ Plot of Sqaured Mahalanobis Distance for Non-Bankrupt Firms",
       pch = 19,col = 'red',ylab = "Mahalanobis Distance",xlab = "Chisquare(4) Quantiles")
par(mfrow = c(1,1))

#=====================================================================================
#Checking Multivariate Normality using royston test

par(mfrow = c(1,2))
r1 <- MVN::mvn(data = My.data0[,-5], mvnTest = "royston",univariateTest = "SW",
               desc = FALSE,multivariateOutlierMethod = "adj",showOutliers = TRUE)
r1$multivariateNormality
r1$multivariateOutliers

r2 <- MVN::mvn(data = My.data1[,-5], mvnTest = "royston",univariateTest = "SW",
         desc = FALSE,multivariateOutlierMethod = "adj",showOutliers = TRUE)
r2$multivariateNormality
r2$multivariateOutliers

#In the 1st population, we have multivariate normality,but in the second population we don't have
#multivariate normality, so we need to use transformation for that

#======================================================================================

#Taking two Variables at a time

#Removing 1,2 variable only - Normal
MVN::mvn(My.data[My.data$y == 0,-c(1,2,5)],mvnTest = "royston",univariateTest = "SW", 
         multivariateOutlierMethod = "adj",showOutliers = TRUE)$multivariateNormality
MVN::mvn(My.data[My.data$y == 1,-c(1,2,5)],mvnTest = "royston",univariateTest = "SW",
         multivariateOutlierMethod = "adj",showOutliers = TRUE)$multivariateNormality

#Removing 1,3 variable only - Normal
MVN::mvn(My.data[My.data$y == 0,-c(1,3,5)],mvnTest = "royston",univariateTest = "SW", 
         multivariateOutlierMethod = "adj",showOutliers = TRUE)$multivariateNormality
MVN::mvn(My.data[My.data$y == 1,-c(1,3,5)],mvnTest = "royston",univariateTest = "SW",
         multivariateOutlierMethod = "adj",showOutliers = TRUE)$multivariateNormality

#Removing 1,4 variable only - Not Normal
MVN::mvn(My.data[My.data$y == 0,-c(1,4,5)],mvnTest = "royston",univariateTest = "SW", 
         multivariateOutlierMethod = "adj",showOutliers = TRUE)$multivariateNormality
MVN::mvn(My.data[My.data$y == 1,-c(1,4,5)],mvnTest = "royston",univariateTest = "SW",
         multivariateOutlierMethod = "adj",showOutliers = TRUE)$multivariateNormality

#Removing 2,3 variable only - Normal
MVN::mvn(My.data[My.data$y == 0,-c(2,3,5)],mvnTest = "royston",univariateTest = "SW", 
         multivariateOutlierMethod = "adj",showOutliers = TRUE)$multivariateNormality
MVN::mvn(My.data[My.data$y == 1,-c(2,3,5)],mvnTest = "royston",univariateTest = "SW", 
         multivariateOutlierMethod = "adj",showOutliers = TRUE)$multivariateNormality

#Removing 2,4 variable only - Not Normal
MVN::mvn(My.data[My.data$y == 0,-c(2,4,5)],mvnTest = "royston",univariateTest = "SW",
         multivariateOutlierMethod = "adj",showOutliers = TRUE)$multivariateNormality
MVN::mvn(My.data[My.data$y == 1,-c(2,4,5)],mvnTest = "royston",univariateTest = "SW",
         multivariateOutlierMethod = "adj",showOutliers = TRUE)$multivariateNormality

#Removing 3,4 variable only - Not Normal
MVN::mvn(My.data[My.data$y == 0,-c(3,4,5)],mvnTest = "royston",univariateTest = "SW", 
         multivariateOutlierMethod = "adj",showOutliers = TRUE)$multivariateNormality
MVN::mvn(My.data[My.data$y == 1,-c(3,4,5)],mvnTest = "royston",univariateTest = "SW", 
         multivariateOutlierMethod = "adj",showOutliers = TRUE)$multivariateNormality

#==== Thus, (1,4),(2,4) are bivariate normal pairs =====

#Let's check for Multivariate Normality using chi-square plot
md.1 <- mahalanobis(My.data[My.data$y == 0,-c(3,2,5)],colMeans(My.data[My.data$y == 0,-c(3,2,5)]),
                     cov(My.data[My.data$y == 0,-c(3,2,5)]))
md.2 <- mahalanobis(My.data[My.data$y == 1,-c(3,2,5)],colMeans(My.data[My.data$y == 1,-c(3,2,5)]),
                     cov(My.data[My.data$y == 1,-c(3,2,5)]))

#QQ Plot of Sqaured Mahalanobis Distance
par(mfrow = c(1,2))
qqPlot(md.1,"chisq",df = 2,main = "Bankrupt Firms With CFTD & CANS ",
       pch = 19,col = 'red',ylab = "Mahalanobis Distance")
qqPlot(md.2,"chisq",df = 2,main = "Non-Bankrupt Firms With CFTD & CANS",
       pch = 19,col = 'red',ylab = "Mahalanobis Distance")

#Let's check for Multivariate Normality using chi-square plot
md.3 <- mahalanobis(My.data[My.data$y == 0,-c(1,3,5)],colMeans(My.data[My.data$y == 0,-c(1,3,5)]),
                     cov(My.data[My.data$y == 0,-c(1,3,5)]))
md.4 <- mahalanobis(My.data[My.data$y == 1,-c(1,3,5)],colMeans(My.data[My.data$y == 1,-c(1,3,5)]),
                     cov(My.data[My.data$y == 1,-c(1,3,5)]))

par(mfrow = c(1,2))
qqPlot(md.3,"chisq",df = 2,main = "Bankrupt Firms With  NITA & CANS",
       pch = 19,col = 'red',ylab = "Mahalanobis Distance")
qqPlot(md.4,"chisq",df = 2,main = "Non-Bankrupt Firms With  NITA & CANS",
       pch = 19,col = 'red',ylab = "Mahalanobis Distance")


#==================================================
#====== (1,4) =======

heplots::boxM(as.matrix(My.data[,-c(2,3,5)]) ~ as.factor(y),data = My.data)

#covariance equality accepted

model.manova <- manova(cbind(CFTD,CANS)~y,data = My.data)
summary(model.manova)

#mean equality rejected ! 

#What if we use LDA?
Lda_Model.1 <- lda(My.data[,-c(2,3,5)],My.data$y,CV = T)
Lda_Model.1
klaR::partimat(as.factor(y) ~., method = "lda",data = My.data[,-c(2,3)])

table(Actual = My.data[,5], Predicted = Lda_Model.1$class)
aer(My.data[,5], Lda_Model.1$class)


#Diagram of LDA Scores

Lda_Model.1_scores.df <- data.frame(Scores = as.matrix(My.data[,-c(2,3,5)])%*%as.vector(lda(My.data[,-c(2,3,5)],My.data$y)$scaling),
                                    y = My.data$y)

ggplot(Lda_Model.1_scores.df,aes(y = jitter(rep(1,46)),x = Scores,col = Index)) + geom_point(size = 3) + 
  labs(y = "",x = "LDA Score",title = "LDA Scores") + theme_bw(14)+ defined_theme + ylim(0.95,1.05)
  

#=============================================================================================

#===== (2,4) =======

heplots::boxM(as.matrix(My.data[,-c(1,3,5)]) ~ as.factor(y),data = My.data)

#============ QDA =============
Qda_Model.2 <- MASS::qda(My.data[,-c(1,3,5)],My.data$y,CV = T)
Qda_Model.2

klaR::partimat(as.factor(y) ~., method = "qda",data = My.data[,-c(1,3)])

table(Actual = My.data[,5], Predicted = Qda_Model.2$class)
aer(My.data[,5], Qda_Model.2$class)

#===================================================================================

#Taking three observations at a time 

#Removing 1st variable only - (2,3,4)
MVN::mvn(My.data[My.data$y == 0,-c(1,5)],mvnTest = "royston",univariateTest = "SW",multivariatePlot = "qq", 
         multivariateOutlierMethod = "adj",showOutliers = TRUE)$multivariateNormality
MVN::mvn(My.data[My.data$y == 1,-c(1,5)],mvnTest = "royston",univariateTest = "SW",multivariatePlot = "qq", 
         multivariateOutlierMethod = "adj",showOutliers = TRUE)$multivariateNormality

#Removing 2nd variable only - (1,3,4)
MVN::mvn(My.data[My.data$y == 0,-c(2,5)],mvnTest = "royston",univariateTest = "SW",multivariatePlot = "qq", 
         multivariateOutlierMethod = "adj",showOutliers = TRUE)$multivariateNormality
MVN::mvn(My.data[My.data$y == 1,-c(2,5)],mvnTest = "royston",univariateTest = "SW",multivariatePlot = "qq", 
         multivariateOutlierMethod = "adj",showOutliers = TRUE)$multivariateNormality

#Removing 3rd variable only - (1,2,4)
MVN::mvn(My.data[My.data$y == 0,-c(3,5)],mvnTest = "royston",univariateTest = "SW",multivariatePlot = "qq", 
         multivariateOutlierMethod = "adj",showOutliers = TRUE)$multivariateNormality
MVN::mvn(My.data[My.data$y == 1,-c(3,5)],mvnTest = "royston",univariateTest = "SW",multivariatePlot = "qq", 
         multivariateOutlierMethod = "adj",showOutliers = TRUE)$multivariateNormality

#Removing 4th variable only - (1,2,3)
MVN::mvn(My.data[My.data$y == 0,-c(4,5)],mvnTest = "royston",univariateTest = "SW",multivariatePlot = "qq", 
         multivariateOutlierMethod = "adj",showOutliers = TRUE)$multivariateNormality
MVN::mvn(My.data[My.data$y == 1,-c(4,5)],mvnTest = "royston",univariateTest = "SW",multivariatePlot = "qq", 
         multivariateOutlierMethod = "adj",showOutliers = TRUE)$multivariateNormality


#So, (1,2,4) is multivariate normal

#checking using chi-sq plot
#Let's check for Multivariate Normality using chi-square plot
#QQ Plot of Sqaured Mahalanobis Distance for 
md.5 <- mahalanobis(My.data[My.data$y == 0,-c(3,5)],colMeans(My.data[My.data$y == 0,-c(3,5)]),
                    cov(My.data[My.data$y == 0,-c(3,5)]))
md.6 <- mahalanobis(My.data[My.data$y == 1,-c(3,5)],colMeans(My.data[My.data$y == 1,-c(3,5)]),
                    cov(My.data[My.data$y == 1,-c(3,5)]))

par(mfrow = c(1,2))
qqPlot(md.5,"chisq",df = 3,main = "Bankrupt Firms  With CFTD,NITA,CANS",
       pch = 19,col = 'red',ylab = "Sqaured Mahalanobis Distance")
qqPlot(md.6,"chisq",df = 3,main = "Non-Bankrupt Firms With CFTD,NITA,CANS",
       pch = 19,col = 'red',ylab = "Sqaured Mahalanobis Distance")

#=============== (1,2,4) =====================

#Test for Homogenity

heplots::boxM(as.matrix(My.data[,-c(3,5)]) ~ as.factor(y),data = My.data)

#Thus we reject H0 of homogeneity of Covariances
#So, we will use QDA 

#Before that we will do scalling !
#My.data[,-5] <- apply(My.data[,-5],2,FUN = scale)

#============ QDA =============
Qda_Model.3 <- MASS::qda(My.data[,-c(3,5)],My.data$y,CV = T)
Qda_Model.3

table(Actual = My.data[,5], Predicted = Qda_Model.3$class)
aer(My.data[,5], Qda_Model.3$class)


#========================================================================================
par(mfrow = c(1,1))
#For univariate normality ! 

gbc <- function(x,lambda){
  if(lambda != 0 & x >= 0){
    s = ((x+1)^lambda - 1)/lambda
  }else if(x >= 0 & lambda == 0){
    s = log(x+1)  
  }else if( x < 0 & lambda != 2){
    s = -((-x + 1)^(2 - lambda) - 1)/(2 - lambda)
  }else if( x < 0 &  lambda == 2){
    s = - log( - x + 1)
  }
  return(s)
}

#Likelihood boxcox

l.gbc <- function(data,lambda.0){
  n <- length(data)
  transformed.data <- vapply(data, FUN = function(x){gbc(x,lambda.0)}, FUN.VALUE = 2)
  mu.est <- mean(transformed.data)
  sigma.sq.est <- var(transformed.data)*(n-1)/n
  
  a <- (-n/2)*log(2*pi) - (n/2)*log(sigma.sq.est) - (1/(2*sigma.sq.est))*sum((transformed.data - mu.est)^2) 
  b <- (lambda.0 - 1)*sum(sign(data)*log(abs(data) + 1))
  
  return(a + b)
}

g.boxcox <- function(data0,data1,lambda.seq){
  
  lbc.mv <- function(lambda.1){
    l.gbc(data0,lambda.1) + l.gbc(data1,lambda.1) 
  }
  
  plot(lambda.seq,vapply(lambda.seq, FUN = lbc.mv, FUN.VALUE = 2),
       col = "red",type = "l",xlab = "Lambda",ylab = "Log Likelihood",
       main = "Plot of Log Likelihood vs. Lambda",lwd = 3)
  abline(h = max(vapply(lambda.seq, FUN = lbc.mv, FUN.VALUE = 2)) - 0.5,lty = 2,col = "blue",lwd = 2)
  abline(v = lambda.seq[which.max(vapply(lambda.seq, FUN = lbc.mv, FUN.VALUE = 2))],lty = 2,col = "blue",lwd = 2)
  print(c("Optimal Lambda" = lambda.seq[which.max(vapply(lambda.seq, FUN = lbc.mv, FUN.VALUE = 2))],
          "Likelihood Value" = max(vapply(lambda.seq, FUN = lbc.mv, FUN.VALUE = 2))))
  
}
g.boxcox0 <- function(data0,lambda.seq){
  
  lbc.mv <- function(lambda.1){
    l.gbc(data0,lambda.1)  
  }
  
  plot(lambda.seq,vapply(lambda.seq, FUN = lbc.mv, FUN.VALUE = 2),
       col = "red",type = "l",xlab = "Lambda",ylab = "Log Likelihood",
       main = "Plot of Log Likelihood vs. Lambda",lwd = 3)
  abline(h = max(vapply(lambda.seq, FUN = lbc.mv, FUN.VALUE = 2)) - 0.5,lty = 2,col = "blue",lwd = 2)
  abline(v = lambda.seq[which.max(vapply(lambda.seq, FUN = lbc.mv, FUN.VALUE = 2))],lty = 2,col = "blue",lwd = 2)
  print(c("Optimal Lambda" = lambda.seq[which.max(vapply(lambda.seq, FUN = lbc.mv, FUN.VALUE = 2))],
          "Likelihood Value" = max(vapply(lambda.seq, FUN = lbc.mv, FUN.VALUE = 2))))
  
}
g.boxcox0(My.data1[,3],seq(-5,7,by = 0.01))
My.data_trans0 <- My.data
My.data_trans0[,3] <- yjPower(My.data[,3],0.41)
MVN::mvn(data = My.data_trans0[My.data_trans0$y == 1,-5], mvnTest = "royston",
         univariateTest = "SW", desc = FALSE)

g.boxcox(My.data0[,3],My.data1[,3],seq(-5,7,by = 0.01))

My.data_trans2 <- My.data
My.data_trans2[,3] <- yjPower(My.data[,3],0.72)

#===========================


MVN::mvn(data = My.data_trans2[My.data_trans2$y == 0,-5], mvnTest = "royston",
    univariateTest = "SW", desc = FALSE)

MVN::mvn(data = My.data_trans2[My.data_trans2$y == 1,-5], mvnTest = "royston",
    univariateTest = "SW", desc = FALSE)

#So,No Multivariate Normality ! 

#===========================

lambda.1 <- powerTransform(My.data[My.data$y == 1,3],family = "yjPower")
My.data_trans3 <- My.data
My.data_trans3[,3] <- yjPower(My.data_trans3[,3],coef(lambda.1))

MVN::mvn(data = My.data_trans3[My.data_trans2$y == 0,-5], mvnTest = "royston",
         univariateTest = "SW", desc = FALSE)

MVN::mvn(data = My.data_trans3[My.data_trans2$y == 1,-5], mvnTest = "royston",
         univariateTest = "SW", desc = FALSE)

#Still No MVN ! 
# Loading Motivation for Multivariate Normality ! ')

#===========================
#yeo-Johnson family of transformation !

lambda.2 <- car::powerTransform(as.matrix(My.data1[,-5]),family = "yjPower")
lambda.2
MVN::mvn(yjPower(with(My.data0,cbind(CFTD,NITA,CATL,CANS)),coef(lambda.2)),mvnTest = "royston",
         univariateTest = "SW",desc = F)

MVN::mvn(yjPower(with(My.data1,cbind(CFTD,NITA,CATL,CANS)),coef(lambda.2)),mvnTest = "royston",
         univariateTest = "SW",desc = F)


#Transformation
My.data_trans4 <- as.data.frame(cbind(yjPower(with(My.data,cbind(CFTD,NITA,CATL,CANS)),coef(lambda.2)),y = My.data$y))
colnames(My.data_trans4) <- c(paste(colnames(My.data)[1:4],"Trans",sep = "_"),"y")

#Let's check for Multivariate Normality using chi-square plot
md.7 <- mahalanobis(My.data_trans4[My.data_trans4$y == 0,-5],colMeans(My.data_trans4[My.data_trans4$y == 0,-5]),
                    cov(My.data_trans4[My.data_trans4$y == 0,-5]))
md.8 <- mahalanobis(My.data_trans4[My.data_trans4$y == 1,-5],colMeans(My.data_trans4[My.data_trans4$y == 1,-5]),
                    cov(My.data_trans4[My.data_trans4$y == 1,-5]))

#QQ Plot of Sqaured Mahalanobis Distance for 
par(mfrow = c(1,2))
qqPlot(md.7,"chisq",df = 4,main = "Bankrupt Firms - Transformed Data",
       pch = 19,col = 'red',ylab = "Sqaured Mahalanobis Distance")
qqPlot(md.8,"chisq",df = 4,main = "Non-Bankrupt Firms - Transformed Data",
       pch = 19,col = 'red',ylab = "Sqaured Mahalanobis Distance")
par(mfrow = c(1,1))

# Yayy, Multivariate Normal ! 

#Test for Homogenity

heplots::boxM(as.matrix(My.data_trans4[,-5]) ~ as.factor(y),data = My.data_trans4)

#Thus we reject H0 of homogeneity of Covariances
#So, we will use QDA 

Qda_Model.4 <- qda(My.data_trans4[,-5], My.data_trans4[,5], CV = T)
table(Actual = My.data_trans4[,5], Predicted = Qda_Model.4$class)

aer(My.data_trans4[,5], Qda_Model.4$class)

#============================

s1 <- cov(My.data_trans4[My.data$y == 0,-5])
s2 <- cov(My.data_trans4[My.data$y == 1,-5])
a <- min(c(range(s1),range(s2))) - 0.1
b <- max(c(range(s1),range(s2))) + 0.1

s1.star <- (s1 - a)/(b-a)
s2.star <- (s2 - a)/(b-a)

#corrplot::corrplot(s1.star,col = rainbow(10),type = "upper",method = "square")
#corrplot::corrplot(s2.star,col = rainbow(10),type = "upper",method = "sqaure")



#==============================================================
#=== For (1,2) ===

lambda.3 <- powerTransform(My.data_trans2[My.data_trans2$y == 1,c(1,2)],family = "yjPower")


MVN::mvn(with(My.data_trans2[My.data_trans2$y == 0,],yjPower(cbind(CFTD,NITA),coef(lambda.3))), mvnTest = "royston",
         univariateTest = "SW", desc = FALSE)

MVN::mvn(with(My.data_trans2[My.data_trans2$y == 1,],yjPower(cbind(CFTD,NITA),coef(lambda.3))), mvnTest = "royston",
         univariateTest = "SW", desc = FALSE)

#Thus multivariate Normality !

My.data_trans5 <- My.data
My.data_trans5[,c(1,2)] <- with(My.data_trans2,yjPower(cbind(CFTD,NITA),coef(lambda.3)))

#Let's check for homogenity for variance

heplots::boxM(as.matrix(My.data_trans5[,c(1,2)]) ~ as.factor(y),data = My.data_trans4)

#Thus we reject H0 of homogeneity of Covariances
#So, we will use QDA 

Qda_Model.5 <- qda(My.data_trans5[,c(1,2)], My.data_trans5[,5], CV = T)
table(Actual = My.data_trans5[,5], Predicted = Qda_Model.5$class)

aer(My.data_trans5[,5], Qda_Model.5$class)

#==========================================================
#(1,3)

lambda.4 <- powerTransform(My.data[My.data$y == 1,c(1,3)],family = "yjPower")


MVN::mvn(with(My.data[My.data$y == 0,],yjPower(cbind(CFTD,CATL),coef(lambda.4))), mvnTest = "royston",
         univariateTest = "SW", desc = FALSE)

MVN::mvn(with(My.data[My.data$y == 1,],yjPower(cbind(CFTD,CATL),coef(lambda.4))), mvnTest = "royston",
         univariateTest = "SW", desc = FALSE)

#No multivariate Normality ! 

#What if we use consider that transformed variable, lambda -> 0.72 
#Removing 2,4 variable only 
MVN::mvn(My.data_trans2[My.data_trans2$y == 0,-c(2,4,5)],mvnTest = "royston",univariateTest = "SW",
         multivariateOutlierMethod = "adj",showOutliers = F,desc = F)
MVN::mvn(My.data_trans2[My.data_trans2$y == 1,-c(2,4,5)],mvnTest = "royston",univariateTest = "SW", 
         multivariateOutlierMethod = "adj",showOutliers = F,desc = F)

#=========
#(3,4)
lambda.5 <- powerTransform(My.data[My.data$y == 1,c(3,4)],family = "yjPower")

MVN::mvn(with(My.data[My.data$y == 0,],yjPower(cbind(CATL,CANS),coef(lambda.5))), mvnTest = "royston",
         univariateTest = "SW", desc = FALSE)

MVN::mvn(with(My.data[My.data$y == 1,],yjPower(cbind(CATL,CANS),coef(lambda.5))), mvnTest = "royston",
         univariateTest = "SW", desc = FALSE)

#No multivariate Normality ! 

#What if we use consider that transformed variable, lambda -> 0.72
#Removing 1,2 variable only - Normal
MVN::mvn(My.data_trans2[My.data_trans2$y == 0,-c(1,2,5)],mvnTest = "royston",univariateTest = "SW",multivariatePlot = "qq", 
         multivariateOutlierMethod = "adj",showOutliers = F,desc = F)
MVN::mvn(My.data_trans2[My.data_trans2$y == 1,-c(1,2,5)],mvnTest = "royston",univariateTest = "SW",multivariatePlot = "qq", 
         multivariateOutlierMethod = "adj",showOutliers = F,desc = F)

#========
#(2,3)

#Removing 1,4 variable only - Not Normal
MVN::mvn(My.data_trans2[My.data_trans2$y == 0,-c(1,4,5)],mvnTest = "royston",univariateTest = "SW")
MVN::mvn(My.data_trans2[My.data_trans2$y == 1,-c(1,4,5)],mvnTest = "royston",univariateTest = "SW")

lambda.51 <- powerTransform(My.data[My.data$y == 1,c(2,3)],family = "yjPower")
lambda.51

My.data_trans51 <- My.data
My.data_trans51[,c(2,3)] <- with(My.data,yjPower(cbind(NITA,CATL),coef(lambda.51)))

MVN::mvn(My.data_trans51[My.data_trans51$y == 0,-c(1,4,5)],mvnTest = "royston",univariateTest = "SW")
MVN::mvn(My.data_trans51[My.data_trans51$y == 1,-c(1,4,5)],mvnTest = "royston",univariateTest = "SW")

#Let's check for homogenity for variance

heplots::boxM(as.matrix(My.data_trans51[,c(2,3)]) ~ as.factor(y),data = My.data_trans51)

#Thus we reject H0 of homogeneity of Covariances
#So, we will use QDA 

Qda_Model.51 <- qda(My.data_trans51[,c(2,3)], My.data_trans51[,5], CV = T)
table(Actual = My.data_trans51[,5], Predicted = Qda_Model.51$class)

aer(My.data_trans51[,5], Qda_Model.51$class)

#training set performance ! 
table(Actual = My.data_trans51[,5], Predicted = predict(qda(My.data_trans51[,c(2,3)], My.data_trans51[,5]))$class)
klaR::partimat(as.factor(y) ~., method = "qda",data = My.data_trans51[,c(2,3,5)])


#=========
#(1,2,3)

lambda.6 <- powerTransform(My.data[My.data$y == 1,c(1,2,3)],family = "yjPower")

MVN::mvn(with(My.data[My.data$y == 0,],yjPower(cbind(CFTD,NITA,CATL),coef(lambda.6))), mvnTest = "royston",
         univariateTest = "SW", desc = FALSE)

MVN::mvn(with(My.data[My.data$y == 1,],yjPower(cbind(CFTD,NITA,CATL),coef(lambda.6))), mvnTest = "royston",
         univariateTest = "SW", desc = FALSE)

#Thus multivariate Normality !

My.data_trans6 <- My.data
My.data_trans6[,c(1,2,3)] <- with(My.data_trans6,yjPower(cbind(CFTD,NITA,CATL),coef(lambda.6)))

#Let's check for homogenity for variance

heplots::boxM(as.matrix(My.data_trans6[,c(1,2,3)]) ~ as.factor(y),data = My.data_trans6)

#Thus we reject H0 of homogeneity of Covariances
#So, we will use QDA 

Qda_Model.6 <- qda(My.data_trans6[,c(1,2,3)], My.data_trans6[,5], CV = T)
table(Actual = My.data_trans6[,5], Predicted = Qda_Model.6$class)

aer(My.data_trans6[,5], Qda_Model.6$class)

#==============================

#(1,3,4)

lambda.7 <- powerTransform(My.data[My.data$y == 1,c(1,3,4)],family = "yjPower")

MVN::mvn(with(My.data[My.data$y == 0,],yjPower(cbind(CFTD,CATL,CANS),coef(lambda.7))), mvnTest = "royston",
         univariateTest = "SW", desc = FALSE)

MVN::mvn(with(My.data[My.data$y == 1,],yjPower(cbind(CFTD,CATL,CANS),coef(lambda.7))), mvnTest = "royston",
         univariateTest = "SW", desc = FALSE)

#No multivariate Normality ! 

#What if we use consider that transformed variable, lambda -> 0.72
#Removing 2nd variable only
MVN::mvn(My.data_trans2[My.data_trans2$y == 0,-c(2,5)],mvnTest = "royston",univariateTest = "SW",multivariatePlot = "qq", 
         multivariateOutlierMethod = "adj",showOutliers = F,desc = F)
MVN::mvn(My.data_trans2[My.data_trans2$y == 1,-c(2,5)],mvnTest = "royston",univariateTest = "SW",multivariatePlot = "qq", 
         multivariateOutlierMethod = "adj",showOutliers = F,desc = F)

#Test for Homogenity

heplots::boxM(as.matrix(My.data_trans2[,-c(2,5)]) ~ as.factor(y),data = My.data_trans2)

#Thus we reject H0 of homogeneity of Covariances
#So, we will use QDA 

#Before that we will do scalling !
My.data_trans2[,-5] <- apply(My.data_trans2[,-5],2,FUN = scale)

#============ QDA =============
Qda_Model.7 <- MASS::qda(My.data_trans2[,-c(2,5)],My.data_trans2$y,CV = T)
table(Actual = My.data_trans2[,5], Predicted = Qda_Model.7$class)
aer(My.data_trans2[,5], Qda_Model.7$class)

#===================================================
#(2,3,4)

lambda.8 <- powerTransform(My.data[My.data$y == 1,c(2,3,4)],family = "yjPower")

MVN::mvn(with(My.data[My.data$y == 0,],yjPower(cbind(NITA,CATL,CANS),coef(lambda.8))), mvnTest = "royston",
         univariateTest = "SW", desc = FALSE)

MVN::mvn(with(My.data[My.data$y == 1,],yjPower(cbind(NITA,CATL,CANS),coef(lambda.8))), mvnTest = "royston",
         univariateTest = "SW", desc = FALSE)

# Not Multivariate Normal

#what if we use lambda <- 0.72
#Removing 1st variable only
MVN::mvn(My.data_trans2[My.data_trans2$y == 0,-c(1,5)],mvnTest = "royston",univariateTest = "SW",multivariatePlot = "qq", 
         showOutliers = F,desc = F)
MVN::mvn(My.data_trans2[My.data_trans2$y == 1,-c(1,5)],mvnTest = "royston",univariateTest = "SW",multivariatePlot = "qq", 
         showOutliers = F,desc = F)

#=======================================================================================

#Principle Component Analysis ! 
#PCA on whole data to understand the separation ---

color <- ifelse(My.data[,5] == 0, "deeppink1", "deepskyblue3")
pca <- prcomp(My.data[,-5],scale = T)
data_pca <- as.data.frame(cbind(pca$x, My.data[,5]))
colnames(data_pca) <- c("PC1", "PC2", "PC3", "PC4", "y")
data_pca[,5] <- ifelse(data_pca[,5] == 1, "Bankrupt", "Non Bankrupt")
data_pca[,5] <- as.factor(data_pca[,5])

library(rgl)
plot3d(x = data_pca$PC1, y = data_pca$PC2, z = data_pca$PC3,
       col = color, xlab = "PC1", ylab = "PC2", zlab = "PC3",
       type = "s", radius = .1)

s = pca$sdev^2
plot(1:4,cumsum(s)/sum(s),type = "o",main = "Proportion of Variablity Explained by PC's",
     col = "red",pch= 19,lwd = 3,xlab = "Index",ylab = "Proportion")
points(1:4,cumsum(s)/sum(s),col = "blue",pch =19,cex = 2)
mtext(paste("Cumulative Proportion : 0.57 0.83 0.96 1"),side = 3,)

screeplot(pca,col = 2:5,main = "ScreePlot",ylim =  c(0,2.5))
mtext(paste("Cumulative Proportion : 0.57 0.83 0.96 1"),side = 3,)
box()
#===============================================
#What if we use LDA in multivariate case?
Lda_Model.9 <- MASS::lda(My.data[,-5],My.data$y,CV = T)
table(Actual = My.data[,5], Predicted = Lda_Model.9$class)
aer(My.data[,5], Lda_Model.9$class)

#Diagram of LDA Scores

Lda_Model.9_scores.df <- data.frame(Scores = as.matrix(My.data_trans4[,-5])%*%as.vector(lda(My.data[,-5],My.data$y)$scaling),
                                    y = My.data_trans4$y)

ggplot(Lda_Model.9_scores.df,aes(y = jitter(rep(1,46)),x = Scores,col = Index)) + geom_point(size = 3) + 
  labs(y = "",x = "LDA Score",title = "LDA Scores") + theme_bw(14)+ defined_theme + ylim(0.95,1.05)


#=================
#What if we use QDA in multivariate case?
Qda_Model.91 <- MASS::qda(My.data[,-5],My.data$y,CV = T)
table(Actual = My.data[,5], Predicted = Qda_Model.91$class)
aer(My.data[,5], Qda_Model.91$class)


#========================================
#Logistic Regression 

Logistic_Model.10 <- glm(y ~.,data = My.data,family = binomial(link = "logit"))
summary(Logistic_Model.10)

APER.10 <- NULL
for(i in 1:nrow(My.data)){
  Logistic_Model.i <- glm(y ~.,data = My.data[-i,],family = binomial(link = "logit"))
  APER.10[i] <- ifelse(predict(Logistic_Model.i,newdata = My.data[i,],type = "response") > 0.5,1,0)
}
APER.10 <- mean(APER.10 != My.data$y)
APER.10

#Confusion matrix
Actual= My.data$y
Predicted =ifelse(predict.glm(Logistic_Model.10,type = "response") > 0.5,1,0)
table(Actual,Predicted) 

#training set misclassification error
mean(ifelse(predict.glm(Logistic_Model.10,type = "response") > 0.5,1,0) != My.data$y)

#==================================
library(GPArotation) ##factor analysis

cortest.bartlett(My.data[,-5])

fc <- fa((My.data_trans4[,-5]), nfactors = 2, rotate = "varimax", fm = "pa")
fc$loadings
fc$communality
fa.diagram(fc$loadings)

fc_n <- fa(My.data_trans4[,-5], nfactors = 2, rotate = "varimax", fm = "ml")
fc_n$loadings
fc_n$communality
fa.diagram(fc_n$loadings)

#Finding Residual Matrix
lamda.var <- fc_n$loadings
psi.var <- diag(fc_n$uniquenesses)
sigma.var <- lamda.var%*%t(lamda.var) + psi.var
sigma.var

#With No rotation !
fc_none <- fa((My.data_trans4[,-5]), nfactors = 2, rotate = "none", fm = "ml")
lamda.none <- fc_none$loadings
psi.none <- diag(fc_none$uniquenesses)
sigma.none <- lamda.none%*%t(lamda.none) + psi.none
sigma.none 

#They are equal, Thus fitted matrix doesnot change under factor rotation.
round(sigma.none,2)
round(sigma.var,2)

#====================

#Rotation of Loadings ! 
fun <-function(x,main,col){
  plot(x[,1],x[,2],xlab=paste('Factor 1'),ylab=paste('Factor 2'),
       xlim=c(-1,1),ylim=c(-1,1),main=main,col=col,pch=19,cex = 1.2)
  abline(h=0,v=0)
}
par(mfrow = c(1,2))
fun(fc_none$loadings,main="No Rotation",col="blue")
fun(fc_n$loadings,main="Varimax Rotation",col="red")

#==========================================

#=================================================================================





