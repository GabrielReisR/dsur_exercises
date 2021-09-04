# Chapter 7 - Regression ====

# Initializing ====
load_libraries <- function(){
  if (!require("boot"))
    install.packages("boot"); library(boot)
  if (!require("car"))
    install.packages("car"); library(car)
  if (!require("dplyr"))
    install.packages("dplyr"); library(dplyr)
  if (!require("ggplot2"))
    install.packages("ggplot2"); library(ggplot2)
  if(!require("magrittr"))
    install.packages("magrittr"); library(magrittr)
  if(!require("psych"))
    install.packages("psych"); library(psych)
  if(!require("QuantPsyc"))
    install.packages("QuantPsyc"); library(QuantPsyc) # lm.beta()
}

load_libraries()

# Running a simple linear regression ====
# Reading data
df <- read.delim(".\\Data Files\\Album Sales 1.dat")

# Model 1
model_1 <- lm(sales ~ adverts, data = df)
summary(model_1)

# Running a multiple regression ====
# Reading data
df <- read.delim(".\\Data Files\\Album Sales 2.dat")

# Running simple regression
model_1 <- lm(sales ~ adverts, data = df)

# Running multiple regression
model_2 <- lm(sales ~ adverts + airplay + attract, data = df)

# Summarizing models
summary(model_1)
summary(model_2)

# Getting standardized beta-values ====
# That is, when we add a SD to X, how many SDs does Y change?

lm.beta(model_2)

# Getting confidence interval for the parameters ====
confint(model_2)

# Comparing models ====
anova(model_1, model_2)

# Checking outliers and influential cases ====
# Criar vari?veis no dataframe e comparar no final#
album2$residuals<-resid(albumSales.3)
album2$standardized.residuals<-rstandard(albumSales.3)
album2$studentized.residuals<-rstudent(albumSales.3)
album2$cooks.distance<-cooks.distance(albumSales.3)
album2$dfbeta<-dfbeta(albumSales.3)
album2$dffits<-dffits(albumSales.3)
album2$hatvalues<-hatvalues(albumSales.3)
album2$covariance.ratios<-covratio(albumSales.3)
#Salvar uma tabela#
round(album2, digits = 3)
names(album2)
write.table(album2, "Album Sales with Diagnostics.xls", sep = "\t", row.names = FALSE)

#Verificar dados segundo um ponto de corte, e salv?-los como outra vari?vel#
album2$standardized.residuals > 2 | album2$standardized.residuals < -2
album2$large.residuals<-album2$standardized.residuals > 2 | album2$standardized.residuals < -2
#Quantos casos?#
sum(album2$large.residuals)
#Quais casos? - rodar o dataframe especificando quais casos e quais vari?veis ver#
album2[album2$large.residuals == TRUE, c("sales", "airplay", "attract", "adverts", "standardized.residuals")]
album2[album2$large.residuals == TRUE, c("cooks.distance", "hatvalues", "covariance.ratios")]

#Assessing the assumption of independence - ser? que h? correla??o entre os erros do modelo? (1<x<3)#
dwt(albumSales.3)

#Assessing the assumption of multicollinearity - ser? que h? correla??o entre os preditores do modelo?#
vifA3<-vif(albumSales.3)
#tolerance - problemas quando 1/vif < 0.2#
1/vifA3
mean(vifA3)

#Assessing dispersion of residuals#
plot(albumSales.3)
hist(album2$studentized.residuals)

#Bootstrapping a regression#
library(boot)
bootReg <- function (formula, data, indices)
{
  fit <- lm(formula, data = data[indices,])
  return(coef(fit))
}
bootResults<-boot(statistic =bootReg, formula = sales ~ adverts +airplay +
                    attract, data =album2, R =2000)
bootResults
boot.ci(bootResults, type ="bca", index =1)
boot.ci(bootResults, type ="bca", index =2)
boot.ci(bootResults, type ="bca", index =3)
boot.ci(bootResults, type ="bca", index =4)

#Dummy coding categorical variables#
gfr<-read.delim("GlastonburyFestivalRegression.dat", header = TRUE)
names(gfr)
#Setting it almost aumotically#
contrasts(gfr$music)<-contr.treatment(4, base = 4)
gfr$music
#Giving helpful names#
music_Crusty<-c(1,0,0,0)
music_Indie<-c(0,1,0,0)
music_Metaller<-c(0,0,1,0)
contrasts(gfr$music)<-cbind(music_Crusty, music_Indie, music_Metaller)
glastonburyModel<-lm(change ~ music, data = gfr)
summary(glastonburyModel)
