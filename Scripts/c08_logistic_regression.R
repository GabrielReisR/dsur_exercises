#Chapter 8 - Logistic Regression#
setwd("C:\\Users\\Marco2\\Desktop\\Gabriel\\Mestrado\\Estatística\\Data Files")
getwd()
library(car); library(mlogit)


#EEL
#

eelData<-read.delim("eel.dat", header = TRUE)
View(eelData)
head(eelData)
names(eelData)

#Releveling the contrasts automatically put by R#
eelData$Cured<-relevel(eelData$Cured, ref = "Not Cured")
eelData$Intervention<-relevel(eelData$Intervention, ref = "No Treatment")

##Basic Logistic Regression##

#Calculating Logistic Regression#
eelModel.1<-glm(Cured ~ Intervention, data = eelData, family = binomial())
summary(eelModel.1)

#Indices#
#Chi-square#
modelChi.1<-eelModel.1$null.deviance - eelModel.1$deviance
modelChi.1
chiDf<-eelModel.1$df.null - eelModel.1$df.residual
chiDf #the result is the number of variables added in the model compared only to the intercept#
#Calculating the probability associated to the chi-square#
chiProb<- 1 - pchisq(modelChi.1, chiDf)
chiProb
#Report that "including Intervention produced a significant improvement
#in the fit of the model, ??2(1) = 9.93, p= .002"

#R-squared by Hosmer & Lemeshow (1989) - chi-square divided by null deviance
model.1R2<-modelChi.1/eelModel.1$null.deviance
model.1R2
#Cox and Snell's R (1989)
model.1R2cs<-1 - exp ((eelModel.1$deviance - eelModel.1$null.deviance)/113)
model.1R2cs
#Nagelkerske (1991) - mais apropriado de todos, utiliza o valor de Cox and Snell's
model.1R2N<-model.1R2cs/(1-exp(-(eelModel.1$null.deviance/113)))
model.1R2N
#Function for all Rs:
logisticPseudoR2s <- function(LogModel) {
  dev <- LogModel$deviance 
  nullDev <- LogModel$null.deviance 
  modelN <- length(LogModel$fitted.values)
  R.l <- 1 - dev / nullDev
  R.cs <- 1- exp ( -(nullDev - dev) / modelN)
  R.n <- R.cs / ( 1 - ( exp (-(nullDev / modelN))))
  cat("Pseudo R^2 for logistic regression\n")
  cat("Hosmer and Lemeshow R^2 ", round(R.l, 3), "\n")
  cat("Cox and Snell R^2 ", round(R.cs, 3), "\n")
  cat("Nagelkerke R^2 ", round(R.n, 3), "\n")
}

#Odds ratio - the exponential of the b coefficient for the intervention variable
exp(eelModel.1$coefficients) # <<<- 3.416667
exp(confint(eelModel.1))

#another model
eelModel.2<-glm(Cured ~ Intervention + Duration, data = eelData, family = binomial())
summary(eelModel.2)
#Comparando o modelo 1 ao modelo 2
modelChi<-eelModel.1$deviance - eelModel.2$deviance
modelDf<-eelModel.1$df.residual - eelModel.2$df.residual
probChi<-1 - pchisq(modelChi, modelDf)
modelChi; modelDf; probChi
#Comparando com a anova() function - obs.:não fornece p
anova(eelModel.1, eelModel.2) #o resultado (deviance) é igual a modelChi calculado acima; ou seja, deviance do primeiro modelo menos deviance do segundo modelo

#Diagnostics - "há algum caso que comprometa o modelo?" - criar novas variáveis no data.frame com os valores do modelo
eelData$predicted.probabilities<-fitted(eelModel.1)
eelData$standardized.residuals<-rstandard(eelModel.1)
eelData$student.residuals<-rstudent(eelModel.1)
eelData$dfbeta<-dfbeta(eelModel.1)
eelData$dffit<-dffits(eelModel.1)
eelData$leverage<-hatvalues(eelModel.1)
head(eelData)
View(eelData)

eelData[, c("leverage", "standardized.residuals", "student.residuals")]

summary(eelModel.1)

#PENALTY
#

penaltyData<-read.delim("penalty.dat", header = TRUE)
names(penaltyData)
penaltyData$Scored<-relevel(penaltyData$Scored, ref = "Missed Penalty")

penaltyModel1<-glm(Scored ~ PSWQ + Previous, data = penaltyData, family = binomial())
penaltyModel2<-glm(Scored ~ PSWQ + Previous + Anxious, data = penaltyData, family = binomial())

summary(penaltyModel1)
summary(penaltyModel2)
names(penaltyModel1)

#chi
chi1<-penaltyModel1$null.deviance - penaltyModel1$deviance
df1<-penaltyModel1$df.null - penaltyModel1$df.residual
probChi1<- 1 - pchisq(chi1, df1)
chi1; df1; probChi1

chi2<-penaltyModel1$deviance - penaltyModel2$deviance
df2<-penaltyModel1$df.residual - penaltyModel2$df.residual
probChi2<- 1 - pchisq(chi2, df2)
chi2; df2; probChi2

#Rs
logisticPseudoR2s(penaltyModel1)
logisticPseudoR2s(penaltyModel2)

#odds
exp(penaltyModel1$coefficients)
exp(confint(penaltyModel1))

exp(penaltyModel2$coefficients)
exp(confint(penaltyModel2))

#variance inflation factor - must be  <10
vif(penaltyModel1)
1/vif(penaltyModel1) #this is called tolerance - must be >~ .1,.2
#ok!

vif(penaltyModel2)
1/vif(penaltyModel2)
#Previous + Anxious parecem compartilharem variância, logo, exploraremos isso
names(penaltyData)
#cor method 1
penaltyMatrix<-as.matrix(penaltyData[,c("PSWQ", "Anxious", "Previous")])
library(Hmisc)
rcorr(penaltyMatrix)
#cor method 2
cor(penaltyData[,c("PSWQ", "Anxious", "Previous")])

#linearity assumption between log of independent variables and log odds of dependent variables
#OU SEJA, logx e logy são correlacionados linearmente

#análise inicial p/ ver se há um 0
#não existe log de 0, então precisaremos adicionar um B0 (constante) a essas vars
names(penaltyData)
penaltyData[,"PSWQ"]
penaltyData[,"Anxious"]
penaltyData[,"Previous"] #previous possui variáveis 0, então precisaremos adicionar um constante ao resultado de log
penaltyData$logPSWQ.int<-log(penaltyData$PSWQ)*penaltyData$PSWQ
penaltyData$logAnxious.int<-log(penaltyData$Anxious)*penaltyData$Anxious
penaltyData$logPrevious.int<-log(penaltyData$Previous + 1)*penaltyData$Previous
penaltyData #agora com as variáveis acimas

penaltyTest.1<-glm(Scored ~ PSWQ + Anxious + Previous + Scored + logPSWQ.int + logAnxious.int + logPrevious.int, data = penaltyData, family=binomial())
summary(penaltyTest.1)

#Multinomial Logistic Regression
chatData<-read.delim("Chat-Up Lines.dat", header = TRUE)
head(chatData)
is.factor(chatData$Success) #TRUE
is.factor(chatData$Gender) #TRUE

#relevelling Gender
contrasts(chatData$Gender)
chatData$Gender<-relevel(chatData$Gender, ref = "Male")
contrasts(chatData$Gender)

#restructuring the data.frame considering the outcome variable
mlChat<-mlogit.data(chatData, choice = "Success", shape = "wide")
mlChat

#running the analysis
chatModel<-mlogit(Success ~ 1 | Good_Mate + Funny + Gender + Sex + Gender:Sex+ Funny:Gender, data = mlChat, reflevel = 3)
summary(chatModel)

#Coefficients
m1Odds<-data.frame(exp(chatModel$coefficients))
m2OddsP<-data.frame(exp(confint(chatModel)))

#evaluating it all
summary(chatModel)
m1Odds
m2OddsP

#diagnostics of multicollinearity and linearity of logit
corChat<-chatData[,c("Funny", "Sex", "Good_Mate")]
cor(corChat)
#create log objects of independent variables directly into dataframe
mlChat$logGood<-log(mlChat$Good_Mate + 1)*mlChat$Good_Mate
mlChat$logFunny<-log(mlChat$Funny + 1)*mlChat$Funny
mlChat$logSex<-log(mlChat$Sex + 1)*mlChat$Sex

conf.chatModel<-mlogit(Success ~ 1 | Good_Mate + Funny + Gender + Sex + Gender:Sex + Funny:Gender + logGood + logFunny + logSex, data = mlChat, reflevel = 3)
summary(conf.chatModel) #all three predictors violated the assumption of linearity of logit

#checking vifs
#first create a generalized linear model with glm() and the original data.frame
chatData
chatData$Gender<-relevel(chatData$Gender, ref = "Female")
contrasts(chatData$Gender)
VIF.model<-glm(Success ~ Funny + Good_Mate + Sex + Gender, data = chatData, family = binomial())
vif(VIF.model) #VIF. Must be < 10
1/vif(VIF.model) #Tolerance. Must be > .1/.2
