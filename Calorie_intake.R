
# Reading the data from CSV
calorie <- read.csv("/Users/shivaniparab/Downloads/calorie.csv")
head(calorie)

# Q1 
#Summarize the data by whether children participated in the meal preparation or not
summary(calorie)

# graphical presentation showing the calorie distribution for participants vs. non-participants
hist_of_participants <- hist(calorie$Calorie.Intake.for.participants, 
                          main="Calorie intake for Participants",
                          xlab='Calorie Intake', 
                          ylab='Number of Participants',
                          col = "pink",
                          ylim=c(0,8))

summary(calorie$Calorie.Intake.for.participants)


hist_non_participants <- hist(calorie$Calorie.intake.for.non.participants, 
                              main="Calorie intake for Non-Participants",
                              xlab='Calorie Intake', 
                              ylab='Number of Participants',
                              col = "royalblue",
                              ylim=c(0,8))
summary(calorie$Calorie.Intake.for.non.participants)

# Q2 
#Checking mean calorie consumption for those who participated in the meal preparation differ from 425
t.test(x = calorie$Calorie.Intake.for.participants, mu = 425, alternative = "two.sided", conf.level = 0.95)
abs(qt(0.975, df = 24))

# Q3 
#Calculate a 90% confidence interval for the mean calorie intake for participants in the meal preparation

# t-value for a 90% confidence interval with 24 degrees of freedom
t_val <- qt(0.95, df = 24)

sample_mean <- mean(calorie$Calorie.Intake.for.participants)
sample_sd <- sd(calorie$Calorie.Intake.for.participants)

standard_error <- sample_sd / sqrt(length(calorie$Calorie.Intake.for.participants))

margin_of_error <- t_val * standard_error

lower_conf_interval <- sample_mean - margin_of_error
upper_conf_interval <- sample_mean + margin_of_error

cat("90% Confidence Interval for Mean Calorie Intake for Participants: [", round(lower_conf_interval, 2), ",", round(upper_conf_interval, 2), "]")

# Q4 
#Whether or not participants consumed more calories than non-participants at the alpha = 0.05
t.test(calorie$Calorie.Intake.for.participants, calorie$Calorie.intake.for.non.participants, alternative = "greater", conf.level = 0.95)
#df = min(participants-1,non-participants-1) = min (24-1 , 22-1) = min(23,21) = 21
abs(qt(0.95, 21))


