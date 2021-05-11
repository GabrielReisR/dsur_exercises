#Chapter 7 - Regression#
setwd("C:\\Users\\Gabriel\\Documents\\Mestrado\\Estat?stica\\Data Files")
getwd()
library(car)
library(ggplot2)
#Simple regression#
album1<-read.delim("Album Sales 1.dat", header = TRUE)
head(album1)
albumSales.1<-lm(sales ~ adverts, data = album1)
summary(albumSales.1)

##Multiple regression##
#Running a multiple regression#
album2<-read.delim("Album Sales 2.dat", header = TRUE)
head(album2)
albumSales.2<-lm(sales ~ adverts, data = album2)
albumSales.3<-lm(sales ~ adverts + airplay + attract, data = album2)
summary(albumSales.2)
summary(albumSales.3)
#Standardized beta-values (quantos desvios-padr?o em y ocorrer?o com o aumento de 1 desvio-padr?o em B?)#
library(QuantPsyc)
lm.beta(albumSales.3)
#Confidence interval for the parameters#
confint(albumSales.3)

#Comparing models (o modelo 2 ? realmente melhor que o modelo 1? - apenas hier?rquico)#
anova(albumSales.1, albumSales.3)

##Outliers and influential cases##
#Criar vari?veis no dataframe e comparar no final#
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
