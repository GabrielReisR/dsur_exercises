
df <- read.delim(".\\Data Files\\Exam Anxiety.dat", header = TRUE)
head(examData)
library(boot);library(ggplot2)
library(Hmisc)
library(ggm)
library(polycor)

#Simple correlations#
examData2<-examData[,c("Revise", "Exam", "Anxiety")]
head(examData2)
cor(examData2)
examMatrix<-as.matrix(examData2[, c("Exam", "Revise", "Anxiety")])
rcorr(examMatrix)
cor.test(examData2$Revise, examData$Anxiety)
cor.test(examData2$Exam, examData$Anxiety)
cor.test(examData2$Exam, examData$Revise)

#Bootstrap#
liarData<-read.delim("The Biggest Liar.dat", header = TRUE)
head(liarData)
cor.test(liarData$Creativity, liarData$Position, method = "kendall")
#cria??o da fun??o#
bootTau<-function(liarData, i)cor(liarData$Creativity[i], liarData$Position[i], use = "complete.obs", method = "kendall")
#execu??o do bootstrap#
boot_kendall<-boot(liarData, bootTau, 2000)
boot_kendall
boot.ci(boot_kendall)
bootExam<-function(examData2, i)cor(examData2$Revise[i], examData2$Exam[i], use = "complete.obs", method = "pearson")
bootExamP<-boot(examData2, bootExam, 2000)
bootExamP
boot.ci(bootExamP)

#Point-biserial and biserial correlation#
catData<-read.csv("pbcorr.csv", header = TRUE)
head(catData)
#Point-biserial - correla??o de pearson comum com a vari?vel categ?rica numerada "0" e "1"#
cor.test(catData$time, catData$recode, method = "pearson")

#Biserial - polyserial correlation, primeiro deve vir a vari?vel cont?nua, depois a categ?rica#
catFrequencies<-table(catData$gender)
catFrequencies
prop.table(catFrequencies)
polyserial(catData$time, catData$gender)
?polyserial


#Partial correlations#
pcor(c("Exam", "Anxiety", "Revise"), var(examData2))
pc<-pcor(c("Exam", "Anxiety", "Revise"), var(examData2))
pc
pc^2*100
nrow(examData2)
pcor.test(pc, 1, 103)

#Comparing correlations#

#independent rs#
examDataM<-subset(examData, Gender == "Male")
examDataF<-subset(examData, Gender == "Female")
cor.test(examDataM$Anxiety, examDataM$Exam)
cor.test(examDataF$Anxiety, examDataF$Exam)
#use function on page 238 or see the additional material#

#dependent rs#
#use function on page 239 or see the additional material#

#tasks#

#task0 = labcoat leni#
study1<-read.delim("Chamorro-Premuzic.dat", header = TRUE)
head(study1)
study12<-study1[, c("studentN", "studentE", "studentO", "studentA", "studentC", "lectureN", "lecturE", "lecturO", "lecturA", "lecturC")]
head(study12)
cor(study12, method = "pearson", use = "pairwise.complete.obs")
round(cor(study12, use = "pairwise.complete.obs"), 2)


#task 1#
essayMarks<-read.delim("EssayMarks.dat", header = TRUE)
order(essayMarks$grade)
cor.test(essayMarks$grade, essayMarks$hours)
essayScatter<-ggplot(essayMarks, aes(hours, essay))
essayScatter + geom_point() + geom_smooth(method="lm")
shapiro.test(essayMarks$hours)
cor.test(essayMarks$essay, essayMarks$hours)
cor.test(essayMarks$hours, essayMarks$grade, alternative = "less", method = "kendall")


#task 2#
chickFlick<-read.delim("ChickFlick.dat", header=TRUE)
names(chickFlick)
chickFlick
cor.test(chickFlick$arousal, as.numeric(chickFlick$gender))
levels(chickFlick$gender)
cor.test(as.numeric(chickFlick$film), chickFlick$arousal)
contrasts(chickFlick$film)


#task 3#
grades<-read.csv("grades.csv", sep = ",")
grades  
cor.test(grades$stats, grades$gcse, method = "kendall")
  
  

