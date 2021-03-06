#Loading appropriate packages and libraries.
## Not sure if all are needed for this problem set.
#install.packages("car")
#install.packages("ggplot2")
#install.packages("pastecs")
#install.packages("psych")
#install.packages("tidyverse")
#install.packages("tidyr")
 
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

#----------------Part 1----------------------
#For the public school systems, describe (via reported descriptive statistics) and plot the 
#distribution of principal salaries, the distribution of teacher salaries, and make a third 
#plot to visually inspect if there seems to be a relationship between the two. 


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
prinData_long [prinData_long  <= 0] <- NA
teachData_long [teachData_long  <= 0] <- NA


#Create a histogram for principals salaries

hist_prinData <- ggplot(prinData_long, aes (Salary))+ theme(legend.position = "none") +
  geom_histogram(aes(y = ..density..), color = "black", fill = "white", binwidth = 3000) +
  labs(title = "Histogram of Principal Salaries", x = "Principal Salaries", y = "Density")
hist_prinData

hist_prinData +
  stat_function(fun = dnorm, args = list(mean = mean(prinData_long$Salary, na.rm = TRUE), sd = sd(prinData_long$Salary, na.rm = TRUE)), color = "black", size = 1)


#Descriptive Statistics for Principal Salaries

round(stat.desc(prinData_long$Salary, basic = FALSE, norm = TRUE), digits = 3)

#Q-QPlot of Principal Salaries

qqplot.prinData <- qplot(sample = prinData_long$Salary, stat="qq")
qqplot.prinData + labs(title = "QQPlot for Principal Salaries", x = "Thoeretical", y = "Sample")

#Shapiro-Wilk Test Principal
shapiro.test(prinData_long$Salary)

#Create a histogram for teachers salaries

hist_teachData <- ggplot(teachData_long, aes (Salary))+ theme(legend.position = "none") +
  geom_histogram(aes(y = ..density..), color = "black", fill = "white", binwidth = 2000) +
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

#plot both Principle and Teacher Salaries on one graph

scatter_both <- ggplot(salary_data, aes(TAvgSalary,PAvgSalary))
scatter_both + geom_point() +
  labs(title = "Compare Teacher and Principal Salaries", x = "Teacher Salaries", y = "Principal Salaries")


#----------------Part 3----------------------
#Compute a rank ordering for teacher salaries (e.g., #1 is the school system paying teachers 
#the most on average) and plot this compared to the rank ordering in the VA County Health 
#Rankings for both Health Factors and Health Outcomes. Just do this for 2016.
                                              
#Read in Principal Salaries from .csv files
healthData <- read.csv("VA_County_Health.csv", header = TRUE)


#Isolate the 2016 data for both Teachers and Health Factors & Outcomes
teachData16 <- teachData_sub %>%
    select(div_name, "FY2016T" )

teachData16[teachData16  < 0] <- NA

healthData16 <- healthData %>%
    select(div_name, HR_outcomes_2016, HR_factors_2016)

#Join Teacher and Health Data - only joining rows that have all data associated.  
#Excluded schools that did not have health data.
teach_health_data <- inner_join(teachData16,healthData16)

#Adding Rank Column to Joined Data Frame
teach_health_data <- transform(teach_health_data, 
          Srank = ave(FY2016T, FUN = function(x) rank(-x, ties.method = "first")))

teach_health_data <- mutate(teach_health_data = dense_rank(-FY2016T))


#Create a Scatter Plot for Teacher Salary Ranking vs. Health Outcome Ranking
scatter_factor <- ggplot(teach_health_data, aes(Srank,HR_factors_2016))
scatter_factor+ geom_point() +
  geom_smooth() +
  labs(title = "Compare Teacher Salary Ranking and Health Factor Ranking", 
       x = "Teacher Salary Ranking", y = "Health Factor Ranking")


#Create a Scatter Plot for Teacher Salary Ranking vs. Health Outcome Ranking
scatter_outcome <- ggplot(teach_health_data, aes(Srank,HR_outcomes_2016))
scatter_outcome+ geom_point() +
  geom_smooth() +
  labs(title = "Compare Teacher Salary Ranking and Health Outccomes Ranking", 
       x = "Teacher Salary Ranking", y = "Health Outcomes Ranking")


#----------------Part 5----------------------
#Describe (via reported descriptive statistics) and plot the distribution of Percent 
#Engineering Degrees awarded by different school systems.  Note that one of the sheets 
#in this file produces a nice summary table of these percentages so use that sheet to 
#make things easier on yourself. Plot the 2Y and 4Y percentages on the same plot as 
#two different series of data. Make a statement of interpretation about what you see.

#Read in Percentage of Engineering Degrees from .csv files
all_degrees <- read.csv("HE_Institutions.csv", header = TRUE)

#Rename first column to match with Teachers.
names(all_degrees)[1] <- "div_name"

#Isolate the 2-year and 4-year columns and convert to percentage
egr_degrees <- all_degrees %>%
  select("div_name", "X.EngTot2yr", "X.EngTot4yr" ) %>%
  transform(X.EngTot2yr = X.EngTot2yr * 100) %>%
  transform(X.EngTot4yr = X.EngTot4yr * 100)


#Create a histogram for 2Y Engineering
hist_2y<- ggplot(egr_degrees, aes (X.EngTot2yr))+ theme(legend.position = "none") +
  geom_histogram(aes(y = ..density..), color = "black", fill = "white") +
  labs(title = "Histogram of 2 Year Engineering Degrees", x = "Percentange of 2 Year Engineering Degrees", y = "Density")
hist_2y

hist_2y +
  stat_function(fun = dnorm, args = list(mean = mean(egr_degrees$X.EngTot2yr, na.rm = TRUE), sd = sd(egr_degrees$X.EngTot2yr, na.rm = TRUE)), color = "black", size = 1)

#Descriptive Statistics for 2Y Engineering
round(stat.desc(egr_degrees$X.EngTot2yr, basic = FALSE, norm = TRUE), digits = 3)

#Q-QPlot of 2Year 
qqplot.2Yegr_degrees<- qplot(sample = egr_degrees$X.EngTot2yr, stat="qq")
qqplot.2Yegr_degrees+ labs(title = "QQPlot for 2 Year Engineering Degrees", x = "Thoeretical", y = "Sample")

#Shapiro-Wilk Test 2 Year
shapiro.test(egr_degrees$X.EngTot2yr)

#Create a histogram for 4Y Engineering
hist_4y<- ggplot(egr_degrees, aes (X.EngTot4yr))+ theme(legend.position = "none") +
  geom_histogram(aes(y = ..density..), color = "black", fill = "white") +
  labs(title = "Histogram of 4 Year Engineering Degrees", x = "Percentange of 4 Year Engineering Degrees", y = "Density")
hist_4y

hist_4y +
  stat_function(fun = dnorm, args = list(mean = mean(egr_degrees$X.EngTot4yr, na.rm = TRUE), sd = sd(egr_degrees$X.EngTot4yr, na.rm = TRUE)), color = "black", size = 1)

#Descriptive Statistics for 4Y Engineering
round(stat.desc(egr_degrees$X.EngTot4yr, basic = FALSE, norm = TRUE), digits = 3)

#Q-QPlot of 4Year 
qqplot.4Yegr_degrees<- qplot(sample = egr_degrees$X.EngTot4yr, stat="qq")
qqplot.4Yegr_degrees+ labs(title = "QQPlot for 4 Year Engineering Degrees", x = "Thoeretical", y = "Sample")

#Shapiro-Wilk Test 4 Year
shapiro.test(egr_degrees$X.EngTot4yr)

#graph 2Y and 4Y data on one graph
#scatter
scatter_degrees <- ggplot(egr_degrees, aes(X.EngTot2yr, X.EngTot4yr))
scatter_degrees + geom_point() +
  geom_smooth() +
  labs(title = "Compare 2Year and 4Year Degrees", 
       x = "2Year Engineering Degrees", y = "4year Engineering Degrees")

# reshape data wide to long 
egr_degrees_long <- gather(egr_degrees, Year, Percentage,X.EngTot2yr:X.EngTot4yr)

#histogram
hist_degrees <- ggplot(egr_degrees_long, aes(x=Percentage, color = Year, fill = Year)) + 
  geom_histogram(position="identity", alpha = 0.2, bins = 50)+
  theme(legend.position = "top")+
  labs(x = "Percenage of Engineering Degrees", y = "Count")
hist_degrees 


#----------------Part 7----------------------
#Devise a plot which lets you investigate if there is a relationship 
#between public salaries and percent degrees awarded.



#Create DF with teacher and principal salaries by Division Number to Join with Degrees

prinData_sub2 <- prinData %>%
  select(div_num,div_name, starts_with("FY20")) 

teachData_sub2 <- teachData %>%
  select(?...div_num, div_name, starts_with("FY20")) 

# reshape data wide to long 
prinData_long2 <- gather(prinData_sub2, Year, Salary,  FY2005P:FY2016P)
teachData_long2 <- gather(teachData_sub2, Year, Salary,  FY2005T:FY2016T)

# some columns have salaries < 0, replace them with NA
prinData_long2 [prinData_long2  <= 0] <- NA
teachData_long2 [teachData_long2  <= 0] <- NA

#calculate the mean salaries over the 12 years 
prinData_Tmean2 <- aggregate(list(PAvgSalary=prinData_long2$Salary), list(Div_num = prinData_long2$?..div_num), mean, na.rm=TRUE)

#calculate the mean teacher salaries over the 12 years 
teachData_Tmean2 <- aggregate(list(TAvgSalary=teachData_long2$Salary), list(Div_num=teachData_long2$?...div_num), mean, na.rm=TRUE)

#join prin and teach average data by county
salary_data2 <- full_join(prinData_Tmean2,teachData_Tmean2)




#Create DF with engineering degrees by Division Number to Join with Degrees

egr_degrees2 <- all_degrees %>%
  select("Div_num", "X.EngTot2yr", "X.EngTot4yr" ) %>%
  transform(X.EngTot2yr = X.EngTot2yr * 100) %>%
  transform(X.EngTot4yr = X.EngTot4yr * 100)


#Need to change columns containing salary to integers

salary_data2 <- salary_data2 %>%
  mutate_at(vars(starts_with("Div_num")), as.numeric)
str(salary_data2) 

#Join Salaries and Engineering degrees
salary_egr_data <- inner_join(egr_degrees2, salary_data2, by = "Div_num")


#Plot Scatter Plot of 4y Percentage vs. Teacher Salary
scatter_4y_teach<- ggplot(salary_egr_data, aes(X.EngTot4yr, TAvgSalary))
scatter_4y_teach + geom_point() +
  geom_smooth() +
  labs(title = "Compare 4Year Degree Percentage with Teacher Salaries", 
       x = "4Year Engineering Degrees", y = "Teacher Salary")

#Plot Scatter Plot of 4y Percentage vs. Principal Salary
scatter_4y_prin<- ggplot(salary_egr_data, aes(X.EngTot4yr, PAvgSalary))
scatter_4y_prin + geom_point() +
  geom_smooth() +
  labs(title = "Compare 4Year Degree Percentage with Principal Salaries", 
       x = "4Year Engineering Degrees", y = "Principal Salary")


#Plot Scatter Plot of 2y Percentage vs. Teacher Salary
scatter_2y_teach<- ggplot(salary_egr_data, aes(X.EngTot2yr, TAvgSalary))
scatter_2y_teach + geom_point() +
  geom_smooth() +
  labs(title = "Compare 2Year Degree Percentage with Teacher Salaries", 
       x = "2Year Engineering Degrees", y = "Teacher Salary")

#Plot Scatter Plot of 4y Percentage vs. Principal Salary
scatter_2y_prin<- ggplot(salary_egr_data, aes(X.EngTot2yr, PAvgSalary))
scatter_2y_prin + geom_point() +
  geom_smooth() +
  labs(title = "Compare 2Year Degree Percentage with Principal Salaries", 
       x = "2Year Engineering Degrees", y = "Principal Salary")






