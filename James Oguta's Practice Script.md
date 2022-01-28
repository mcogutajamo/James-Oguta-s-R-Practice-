# James Oguta R-Practicals-Latest
 My current R practicals
 ### Upload the heart dataset-Ensure it is saved in the same repository
Heart <- read.csv ("Heart(3).csv", header=TRUE)
## Step 4: Use the 'structure' command to see and overview of the data
str(Heart)
### Step 5: Use the 'summary' command to see statistical summary of different data 
The summarize command provides a statistical summary of your data. It includes the mean, median, minimum and maximum values and centiles of your data.
summary(Heart$Leg) 
summary(Heart$Heart) 
###Step 6: There has been a data entry error for patient ID 2, blood alcohol level from leg.  
#See the value: 
Heart[Heart$PatientID==2,"Leg"]
#Correct the value: change it from 3000 to 265: 
Heart[Heart$PatientID==2,"Leg"] <- 265 

#Step 7: Create a new variable to record the difference between blood alcohol levels from leg and from heart. We want to investigate whether we think the blood alcohol levels are similar between levels taken in the leg and heart by looking at the difference in the levels, if levels are similar you would expect the difference to be zero. 
Heart$diff <- Heart$Leg - Heart$Heart
summary(Heart$diff)
#Step 8: See the number of males and females in the dataset 
table(Heart$Gender)
#Recode the value to make it more readable 
Heart$Sex <- ifelse(Heart$Gender == 1, "Male", "Female")
table(Heart$Sex)
#Step 9: Summarise blood alcohol separately for males and females 
summary(Heart$Leg[Heart$Sex =="Male"])
summary(Heart$Leg[Heart$Sex =="Female"])
summary(Heart$Heart[Heart$Sex =="Male"])
summary(Heart$Heart[Heart$Sex =="Female"])
# Do you notice a difference in levels for males and females?
#Step 10: plot a histogram of blood alcohol levels taken from the leg 
hist(Heart$Leg)
#Do you think the data is normally distributed (symmetrical)?
#Step 11: do a log transformation of blood alcohol levels taken from the leg 
Heart$lleg10 <- log10(Heart$Leg)
hist(Heart$lleg10)
#Does the log transformed data appear to be more normally distributed?

