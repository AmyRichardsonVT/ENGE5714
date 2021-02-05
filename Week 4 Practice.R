#Loading appropriate packages and libraries.
##These are needed to follow along with the book tutorial.
install.packages("car")
install.packages("ggplot2")
install.packages("pastecs")
install.packages("psych")

library(car)
library(ggplot2)
library(pastecs)
library(psych)
library(Rcmdr)

#Read in Download Festival Data

dlf <- read.delim("DownloadFestivalNoOutlier.dat", header = TRUE)

#Create a histogram

hist.day1 <- ggplot(dlf, aes(day1)) + theme(legend.position = "none")+
  geom_histogram(aes(y = ..density..), color = "black", fill = "white") +
  labs(x = "Hygiene score on Day 1", y = "Density")
hist.day1

hist.day1 + 
  stat_function(fun = dnorm, args = list(mean = mean(dlf$day1, na.rm = TRUE), sd = sd(dlf$day1, na.rm = TRUE)), color = "black", size = 1) +


#Self-Test Page 171 
hist.day2 <- ggplot(dlf, aes(day2)) + theme(legend.position = "none")+
  geom_histogram(aes(y = ..density..), color = "black", fill = "white", binwidth = 0.01) +
  labs(x = "Hygiene score on Day 2", y = "Density")
hist.day2

hist.day2 + stat_function(fun = dnorm, args = list(mean = mean(dlf$day2, na.rm = TRUE), sd = sd(dlf$day2, na.rm = TRUE)), color = "black", size = 1) 



hist.day3 <- ggplot(dlf, aes(day3)) + theme(legend.position = "none")+
  geom_histogram(aes(y = ..density..), color = "black", fill = "white", binwidth = 0.01) +
  labs(x = "Hygiene score on Day 3", y = "Density")
hist.day3

hist.day3 + 
  stat_function(fun = dnorm, args = list(mean = mean(dlf$day3, na.rm = TRUE), sd = sd(dlf$day3, na.rm = TRUE)), color = "black", size = 1) 

#draw a Q-Q plot for Day 1

qqplot.day1 <- qplot(sample = dlf$day1, stat="qq")
qqplot.day1  

#Self-Test Page 171

qqplot.day2 <- qplot(sample = dlf$day2, stat="qq")
qqplot.day2 

qqplot.day3 <- qplot(sample = dlf$day3, stat="qq")
qqplot.day3 

##5.5.2 Quantifying Normality with Numbers
###this is from the psych package

describe(dlf$day1)

###or this from pastecs package

stat.desc(dlf$day1, basic = FALSE, norm = TRUE)

#to look at more than one variable

stat.desc(dlf[, c("day1", "day2", "day3")], basic = FALSE, norm = TRUE)

##5.5.3 Exploring groups of Data
 rexam <- read.delim("rexam.dat", header = TRUE)

 #change 'uni' column from numbers to text
  rexam$uni <- factor(rexam$uni, level = c(0:1), labels = c("Duncetown University", "Sussex University"))
 
 #Self-Test Page 177
 hist.exam <- ggplot(rexam, aes(exam)) + theme(legend.position = "none")+
   geom_histogram(aes(y = ..density..), color = "black", fill = "white") +
   labs(x = "1st Year R Exam Score", y = "Density")
 hist.exam
 
 hist.exam + 
   stat_function(fun = dnorm, args = list(mean = mean(rexam$exam, na.rm = TRUE), sd = sd(rexam$exam, na.rm = TRUE)), color = "black", size = 1) 
  
 
 hist.computer <- ggplot(rexam, aes(computer)) + theme(legend.position = "none")+
   geom_histogram(aes(y = ..density..), color = "black", fill = "white") +
   labs(x = "Measure of Computer Literacy", y = "Density")
 hist.computer
 
 hist.computer+ 
   stat_function(fun = dnorm, args = list(mean = mean(rexam$computer, na.rm = TRUE), sd = sd(rexam$computer, na.rm = TRUE)), color = "black", size = 1) 
 
 
 hist.lectures <- ggplot(rexam, aes(lectures)) + theme(legend.position = "none")+
   geom_histogram(aes(y = ..density..), color = "black", fill = "white", binwidth = 1) +
   labs(x = "Percentage of Lectures Attended", y = "Density")
 hist.lectures
 
 hist.lectures + 
   stat_function(fun = dnorm, args = list(mean = mean(rexam$lectures, na.rm = TRUE), sd = sd(rexam$lectures, na.rm = TRUE)), color = "black", size = 1)
 
 
 hist.numeracy <- ggplot(rexam, aes(numeracy)) + theme(legend.position = "none")+
   geom_histogram(aes(y = ..density..), color = "black", fill = "white") +
   labs(x = "Measure of Numerical Ability", y = "Density")
 hist.numeracy
 
 hist.numeracy + 
   stat_function(fun = dnorm, args = list(mean = mean(rexam$numeracy, na.rm = TRUE), sd = sd(rexam$numeracy, na.rm = TRUE)), color = "black", size = 1) 
 

 round(stat.desc(rexam[, c("exam", "computer", "numeracy", "lectures")], basic = FALSE, norm = TRUE), digits = 3)
 
 #Obtain separate descriptive statistics for each of the Universities
 
 by(rexam[, c("exam", "numeracy")], rexam$uni, stat.desc, basic = FALSE, norm = TRUE) 
 
 dunceData<-subset(rexam, rexam$uni=="Duncetown University")
 sussexData<-subset(rexam, rexam$uni=="Sussex University")
 
 hist.numeracy.duncetown <- ggplot(dunceData, aes(numeracy)) + theme(legend.position = "none") + 
   geom_histogram(aes(y = ..density..), fill = "white", colour = "black", binwidth = 1) + 
   labs(x = "Numeracy Score", y = "Density") + 
   stat_function(fun=dnorm, args=list(mean = mean(dunceData$numeracy, na.rm = TRUE), sd = sd(dunceData$numeracy, na.rm = TRUE)), colour = "red", size=1)
 hist.numeracy.duncetown
 ggsave(file = paste(imageDirectory,"05 dunce numeracy Hist.png",sep="/"))
 
 
 
 hist.exam.duncetown <- ggplot(dunceData, aes(exam)) + theme(legend.position = "none") + 
   geom_histogram(aes(y = ..density..), fill = "white", colour = "black") + 
   labs(x = "First Year Exam Score", y = "Density") + 
   stat_function(fun=dnorm, args=list(mean = mean(dunceData$exam, na.rm = TRUE), sd = sd(dunceData$exam, na.rm = TRUE)), colour = "red", size=1)
 hist.exam.duncetown
 ggsave(file = paste(imageDirectory,"05 dunce exam Hist.png",sep="/"))
 
 
 hist.numeracy.sussex <- ggplot(sussexData, aes(numeracy)) + theme(legend.position = "none") + 
   geom_histogram(aes(y = ..density..), fill = "white", colour = "black", binwidth = 1) + 
   labs(x = "Numeracy Score", y = "Density") + 
   stat_function(fun=dnorm, args=list(mean = mean(sussexData $numeracy, na.rm = TRUE), sd = sd(sussexData $numeracy, na.rm = TRUE)), colour = "blue", size=1)
 hist.numeracy.sussex
 ggsave(file = paste(imageDirectory,"05 sussex numeracy Hist.png",sep="/"))
 
 hist.exam.sussex <- ggplot(sussexData, aes(exam)) + theme(legend.position = "none") + 
   geom_histogram(aes(y = ..density..), fill = "white", colour = "black") + 
   labs(x = "First Year Exam Score", y = "Density") + 
   stat_function(fun=dnorm, args=list(mean = mean(sussexData$exam, na.rm = TRUE), sd = sd(sussexData$exam, na.rm = TRUE)), colour = "blue", size=1)
 hist.exam.sussex
 ggsave(file = paste(imageDirectory,"05 sussex exam Hist.png",sep="/")) 
 
 
 
 
 #Self-Test Page 182
 hist.computer.duncetown <- ggplot(dunceData, aes(computer)) + theme(legend.position = "none") + 
   geom_histogram(aes(y = ..density..), fill = "white", colour = "black", binwidth = 1) + 
   labs(x = "Computer Score", y = "Density") + 
   stat_function(fun=dnorm, args=list(mean = mean(dunceData$computer, na.rm = TRUE), sd = sd(dunceData$computer, na.rm = TRUE)), colour = "red", size=1)
 hist.computer.duncetown
 ggsave(file = paste(imageDirectory,"05 dunce computer Hist.png",sep="/"))
 
 
 
 hist.lectures.duncetown <- ggplot(dunceData, aes(lectures)) + theme(legend.position = "none") + 
   geom_histogram(aes(y = ..density..), fill = "white", colour = "black") + 
   labs(x = "Lecture Attendance", y = "Density") + 
   stat_function(fun=dnorm, args=list(mean = mean(dunceData$lectures, na.rm = TRUE), sd = sd(dunceData$lectures, na.rm = TRUE)), colour = "red", size=1)
 hist.lectures.duncetown
 ggsave(file = paste(imageDirectory,"05 dunce lectures Hist.png",sep="/"))
 
 
 hist.computer.sussex <- ggplot(sussexData, aes(computer)) + theme(legend.position = "none") + 
   geom_histogram(aes(y = ..density..), fill = "white", colour = "black", binwidth = 1) + 
   labs(x = "ComputerScore", y = "Density") + 
   stat_function(fun=dnorm, args=list(mean = mean(sussexData$computer, na.rm = TRUE), sd = sd(sussexData$computer, na.rm = TRUE)), colour = "blue", size=1)
 hist.computer.sussex
 ggsave(file = paste(imageDirectory,"05 sussex computer Hist.png",sep="/"))
 
 hist.lectures.sussex <- ggplot(sussexData, aes(lectures)) + theme(legend.position = "none") + 
   geom_histogram(aes(y = ..density..), fill = "white", colour = "black") + 
   labs(x = "Lecture Attendance", y = "Density") + 
   stat_function(fun=dnorm, args=list(mean = mean(sussexData$lectures, na.rm = TRUE), sd = sd(sussexData$lectures, na.rm = TRUE)), colour = "blue", size=1)
 hist.lectures.sussex
 ggsave(file = paste(imageDirectory,"05 sussex lectures Hist.png",sep="/")) 
 
 
#5.6 Testing whether a distribution is Normal.
 ## Shapiro-Wilk
 shapiro.test(rexam$exam)
shapiro.test(rexam$numeracy)
    ##  These results p = 0.005 and p = 0.00002424 are < 0.05 so distribution is not normal

    ##  Break up by instittuion to compare Shapiro-Wilk scores
by(rexam$exam, rexam$uni, shapiro.test)
by(rexam$numeracy, rexam$uni, shapiro.test)
    
  ##drow Q-Qplots for each variable
qplist.film.duncetownot(sample = rexam$exam, stat = "qq")
qplot(sample = rexam$numeracy, stat = "qq")


#5.7 testing for Homogeneity of Variance
###Levene's Test (can do in RCommander or in console)
leveneTest(rexam$exam, rexam$uni)
leveneTest(rexam$exam, rexam$uni, center = mean)
leveneTest(rexam$numeracy, rexam$uni)


#Smart Alex Exercises Page 204
#Check the assumptions of normality and homogeneity of variance for the two films (ignore gender):  are the assumptions met?

cflick <- read.delim("ChickFlick.dat", header = TRUE)

#create DF for each film in original dataset
MementoData<-subset(cflick, cflick$film =="Memento")
BJDData<-subset(cflick, cflick$film =="Bridget Jones' Diary")

#create histograms for each film with smooth distribution 
hist.memento <- ggplot(MementoData, aes(arousal)) + theme(legend.position = "none") + 
   geom_histogram(aes(y = ..density..), fill = "white", colour = "black") + 
   labs(x = "Arousal - Memento", y = "Density") + 
   stat_function(fun=dnorm, args=list(mean = mean(MementoData$arousal, na.rm = TRUE), sd = sd(MementoData$arousal, na.rm = TRUE)), colour = "red", size=1)
hist.memento


hist.BJD <- ggplot(cflick, aes(arousal)) + theme(legend.position = "none") + 
   geom_histogram(aes(y = ..density..), fill = "white", colour = "black") + 
   labs(x = "Arousal -  BJD", y = "Density") + 
   stat_function(fun=dnorm, args=list(mean = mean(BJDData$arousal, na.rm = TRUE), sd = sd(BJDData$arousal, na.rm = TRUE)), colour = "red", size=1)
hist.BJD

#produce descriptive statistics
round(stat.desc(MementoData$arousal, basic = FALSE, norm = TRUE), digits = 3)
round(stat.desc(BJDData$arousal, basic = FALSE, norm = TRUE), digits = 3)

shapiro.test(MementoData$arousal)
shapiro.test(BJDData$arousal)

qplot(sample = MementoData$arousal, stat = "qq")
qplot(sample = BJDData$arousal, stat = "qq")

leveneTest(cflick$arousal, cflick$film)


#6.4 Correlation Analysis

advertData <- read.delim("Advert.dat", header = TRUE)

#Self-Test Page 213

scatter <- ggplot(advertData, (aes(adverts, packets)))
scatter + geom_point(size = 3) + labs(x = "Adverts", y = "Packets") +
   scale_y_continuous(limits=c(0, 15), breaks=0:15) + scale_x_continuous(limits=c(0, 9), breaks=0:9)

#6.5 Bivariate Correlation

install.packages("Hmisc")
install.packages("polycor")
install.packages("ggm")

#Initiate packages
library(Hmisc)
library(ggplot2)
library(boot)
library(polycor)
library(ggm)
library(Rcmdr)

#can use Rcmdr to calculate Correlation Matrix
   ##Statistics -> summary -> correlation matrix

examData <- read.delim("Exam Anxiety.dat", header = TRUE)

#Pearson's r

   cor(examData$Exam, examData$Anxiety, use = "complete.obs", method = 'pearson')
   examData2 <- examData[, c("Exam", "Anxiety", "Revise")]
   cor(examData2)
   
   # or
   cor(examData[, c("Exam", "Anxiety", "Revise")])
   
   
   
   # yields same matrix as before but includes sample size and p-values
   examMatrix<-as.matrix(examData[, c("Exam", "Anxiety", "Revise")])
   Hmisc::rcorr(examMatrix)
   Hmisc::rcorr(as.matrix(examData[, c("Exam", "Anxiety", "Revise")]))
   
   #confidence intervals for correlation coefficients
   cor.test(examData$Anxiety, examData$Exam)
   cor.test(examData$Revise, examData$Exam)
   cor.test(examData$Anxiety, examData$Revise)
   
   
   #to calculate coefficient of determination - R^2 and make it a percent
   cor(examData2)^2 * 100


   #Spearman's Rho
   
      liarData <- read.delim("The Biggest Liar.dat", header = TRUE)
      
      cor(liarData$Position, liarData$Creativity, use = "complete.obs", method = 'spearman')
      liarMatrix <- as.matrix(liarData[, c("Position", "Creativity")])
      rcorr(liarMatrix)
      
      
      #using "less" since the lower the place (1st, 2nd, 3rd) the better the result
      cor.test(liarData$Position, liarData$Creativity, alternative = "less", method = "spearman")
      

   #Kendall's tau
      cor(liarData$Position, liarData$Creativity, use = "complete.obs", method = 'kendall')
      #or
      cor.test(liarData$Position, liarData$Creativity, alternative = "less", method = "kendall")

   
   #Self-test Page 226

      cor.test(advertData$adverts, advertData$packets)
  
      
   #6.5.7 Bootstrapping
      library(boot)
      
      bootTau <- function(liarData,i)cor(liarData$Position[i], liarData$Creativity[i], use = "complete.obs", method = "kendall")
      boot_kendall <- boot(liarData, bootTau, 2000)      
      boot_kendall      
      
      #to get the 95% confidence interval
      boot.ci(boot_kendall)

      #Self-test Page 227
      
      bootPearson <- function(examData2,i)cor(examData2$Exam[i], examData2$Anxiety[i], use = "complete.obs", method = "pearson")
      boot_Pearson <- boot(examData2, bootPearson, 2000)      
      boot_Pearson    
      boot.ci(boot_Pearson)
      
      
      bootRho<-function(examData2,i) cor(examData2$Exam[i], examData2$Anxiety[i], use = "complete.obs", method = "spearman")
      boot_spearman<-boot(examData2, bootRho, 2000)
      boot_spearman
      boot.ci(boot_spearman)
      
      
      #Partial correlation
       library(ggm)
      
         pc <- pcor(c("Exam", "Anxiety", "Revise"), var(examData2))
         pc
         pc^2

      
      
      
      
      
      
      
      
      
      
      
      
      
      
      














