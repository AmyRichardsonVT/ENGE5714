#Loading appropriate packages and libraries.
## Not sure if all are needed for this problem set.
install.packages("car")
install.packages("ggplot2")
install.packages("pastecs")
install.packages("psych")
install.packages("tidyverse")
install.packages("tidyr")
 
library(car)
library(ggplot2)
library(pastecs)
library(psych)
library(Rcmdr)
library(tidyverse)
library(tidyr)
library(stringr)
library(dplyr)

#The original files were downloaded from  https://data.lib.vt.edu/files/vd66w001w 

#Read in Principal Salaries from .csv files

prinData <- read.csv("Principal_Salaries.csv", header = TRUE)

#check to see the types of variables
str(prinData)

#Need to change columns containing salary to integers
  
prinData <- prinData%>%
    mutate_at(vars(starts_with("FY20")), as.numeric)
  
str(prinData) 
  

#Read in Teacher Salaries from .csv files
teachData <- read.csv("Teacher_Salaries.csv", header = TRUE)

#check to see the types of variables

str(teachData)

#Clean both DFs so that we have only the data we need

prinData_sub <- prinData %>%
  select(div_name, starts_with("FY20")) 
  
teachData_sub <- teachData %>%
  select(div_name, starts_with("FY20")) 
 
# reshape data wide to long 
prinData_long <- gather(prinData_sub, Year, Salary,  FY2005P:FY2016P)
teachData_long <- gather(teachData_sub, Year, Salary,  FY2005T:FY2016T)

# some columns have salaries < 0, replace them with NA
prinData_long [prinData_long  < 0] <- NA
teachData_long [teachData_long  < 0] <- NA


#Create a histogram for principals salaries

hist_prinData <- ggplot(prinData_long, aes (Salary))+ theme(legend.position = "none") +
  geom_histogram(aes(y = ..density..), color = "black", fill = "white", binwidth = 3000) +
  labs(title = "Histograpm of Principal Salaries", x = "Principal Salaries", y = "Density")
hist_prinData

hist_prinData +
  stat_function(fun = dnorm, args = list(mean = mean(prinData_long$Salary, na.rm = TRUE), sd = sd(prinData_long$Salary, na.rm = TRUE)), color = "black", size = 1)


#Descriptive Statistics for Principal Salaries

round(stat.desc(prinData_long$Salary, basic = FALSE, norm = TRUE), digits = 3)

#Q-QPlot of Principal Salaries DOES NOT WORK

str(prinData_long)

qqplot.prinData <- qplot(sample = prinData_long$Salary, stat="qq")
qqplot.PrinData

#Shapiro-Wilk Test Principal
shapiro.test(prinData_long$Salary)

#Create a histogram for teachers salaries

hist_teachData <- ggplot(teachData_long, aes (Salary))+ theme(legend.position = "none") +
  geom_histogram(aes(y = ..density..), color = "black", fill = "white", binwidth = 1000) +
  labs(title = "Histogram of Teacher Salaries", x = "Teacher Salaries", y = "Density")
hist_teachData

hist_teachData +
  stat_function(fun = dnorm, args = list(mean = mean(teachData_long$Salary, na.rm = TRUE), sd = sd(teachData_long$Salary, na.rm = TRUE)), color = "black", size = 1)


#Descriptive Statistics for Teacher Salaries

round(stat.desc(teachData_long$Salary, basic = FALSE, norm = TRUE), digits = 3)

#Q-QPlot of Teacher Salaries 

qqplot.teachData <- qplot(sample = teachData_long$Salary, stat="qq")
qqplot.teachData + labs(title = "QQPlot for Teacher Salaries", x = "Thoeretical", y = "Sample")

#Shapiro-Wilk Test Teacher
shapiro.test(teachData_long$Salary)


#calculate the mean salaries over the 12 years 
prinData_Tmean <- aggregate(list(PAvgSalary=prinData_long$Salary), list(County=prinData_long$div_name), mean, na.rm=TRUE)

#calculate the mean teacher salaries over the 12 years 
teachData_Tmean <- aggregate(list(TAvgSalary=teachData_long$Salary), list(County=teachData_long$div_name), mean, na.rm=TRUE)

#join prin and teach average data by county
salary_data <- full_join(prinData_Tmean,teachData_Tmean)











