#Smart Alex's Tasks 
#Chapter 4 - Page 164

#Task 1
#will need to read in /RPractice/Week 3/Lecturer Data.dat
lecturer_Data <- read.delim("Lecturer Data.dat", header = TRUE)

#Error bar chart showing the mean number of friends for students and lecturers.
lecturer_Data$job_text[lecturer_Data$job==1] <- "Lecturer"
lecturer_Data$job_text[lecturer_Data$job==2] <- "Student"
bar <- ggplot(lecturer_Data, aes (job, friends))
bar + stat_summary(fun = mean, geom = "bar", fill = "White", color = "Black") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2)
  
#Error bar chart showing the mean alcohol consumption for students and lecturers
bar2 <- ggplot(lecturer_Data, aes(job_text, alcohol))
lecturer_Data$job_text[lecturer_Data$job==1] <- "Lecturer"
lecturer_Data$job_text[lecturer_Data$job==2] <- "Student"
bar2 + stat_summary(fun = mean, geom = "bar", fill = "White", color = "Black") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2)


#Error line chart showing the mean income for students and lecturers
line <- ggplot(lecturer_Data, aes(job_text, income))
line + stat_summary(fun = mean, geom = "point") +
  stat_summary(fun = mean, geom = "line", aes(group = "Lecturer"), color = "Blue", linetype = "dashed") +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2)


#An error line chart showing the mean neuroticism for students and lecturers.
line2 <- ggplot(lecturer_Data, aes(job_text, neurotic))
line2 + stat_summary(fun = mean, geom = "point") +
  stat_summary(fun = mean, geom = "line", aes(group = "Neuroticism"), color = "Blue", linetype = "dashed") +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2)

#A scatter plot with regression lines of alcohol consumption and neuroticism group by lecturer/student
plot <- ggplot(lecturer_Data, aes(neurotic, alcohol))               
plot + geom_point(aes(color = job_text)) + 
  geom_smooth(method = "lm", aes(fill = job_text, color = job_text), alpha = 0.1) +
  labs(x = "Neuroticism", y = "Alcohol Consumption", color = "job_text")

#Finishing Classwork - Which county has the highest percentage of students on free and reduced lunch?  The lowest?
# install necessary packages
install.packages("tidyverse")
install.packages("tidyr")
library(tidyverse)
library(tidyr)

# read dataset in
VApublic <- read.csv("Copy of Free Reduced Lunch.csv", header = TRUE)

# create DF with salient columns
FRL <- select(VApublic, "div_name", starts_with("totalper_"))  

# convert character percentage columns to numeric columns, replace columns in original DF
by_county[-1] <- data.frame(apply(by_county[-1], 2, function(x) as.numeric(sub("%","",as.character(x)))))

# some columns have percentages < 0, replace them with NA
by_county[by_county < 0] <- NA

# aggregate by county and calcualte mean for each column/year
by_county_agg <- aggregate(by_county[, -1], list(County=by_county$div_name), mean, na.rm = TRUE)

# reshape data wide to long
by_county_agg_long <- gather(by_county_agg, Year, Percentage, totalper_0809:totalper_1718)

# aggregate by county across all years
by_county_agg_long_counties <- aggregate(list(Percentage=by_county_agg_long$Percentage), list(County=by_county_agg_long$County), mean, na.rm = TRUE)

# find minimum value
by_county_agg_long_counties[which.min(by_county_agg_long_counties$Percentage),]

# find maximum value
by_county_agg_long_counties[which.max(by_county_agg_long_counties$Percentage),]




