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
VApublic <- read.csv("Copy of Free Reduced Lunch.csv", header = TRUE)
FRL <- select(VApublic, "div_name", starts_with("totalper_"))  
mutate(FRL, )

by_county <- group_by(FRL, div_name)

data.frame(apply(by_county[-1], 2, function(x) as.numeric(sub("%","",as.character(x)))))
#this is still terrible

