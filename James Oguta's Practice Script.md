# James Oguta R-Practicals-Latest
My current R practicals-Based on Tutorial File shared by Tracey Young
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

####PLEASE REVIEW THE CODES FROM THIS SECTION-YOUR FEEDBACK IS MUCH APPRECIATED

###Microcosting Tutorial###

###SCENARIO

###2.	Work with the microcosting dataset 
#In Tutorial 2 you completed a micro costing exercise using an EXCEL spreadsheet. Repeat the same exercise using the file Microcosting.csv.  A reminder of the tutorial is set out below.
#In a trial of cardiogenic pulmonary oedema patients were randomised to three treatment alternatives on arrival at the Emergency Deaprtment (ED).  These were:
#Standard care, which is typically oxygen
#Continuous positive airway pressure (CPAP)
#Non-invasive positive pressure ventilation (NIPPV)

#CPAP and NIPPV involve the purchase of an expensive piece of machinery which which forces air into the lungs.  With CPAP the pressure is constant, with NIPPV the pressure varies depending on whether the patient is breathing in or out; the same machine is used for both although it is thought that the greater number of adjustments required for NIPPV mean that more staff time is required.
#Reference costs do not distinguish between these three treatments, therefore a microcosting study was undertaken to assess accurately the difference in costs.  The dataset is available on MOLE in Excel.
#The treatment groups are labelled 1, 2 and 3 for standard care, CPAP and NIPPV, respectively.  The other variables/labels are attached.
#The machine (including maintenance, consumables, etc) costs £52,065 and has a life expectancy of 5 years at which point it will have no resale value.

#QUESTIONS
#1.	Calculate the cost per patient for the machine assuming r=3.5% and an expected annual workload of 130.
#2.	Identify unit costs for each of the other units of resource and state where they have been taken from.  Assume that for the tests and investigations, direct access services are the most appropriate.  All costs are available from NHS Reference Costs or Unit Costs of Health and Social Care (copies within MOLE).
#3.	Calculate the following costs for each patient
#	machine costs
#	staff costs
#	investigations
#	total costs including overheads (which have been calculated elsewhere at £21.52 per patient
#4.	What is the mean cost for each of the three types of treatment?

#SOLUTIONS#

###Importing microcosting CSV file into R###
Microcosting <- read.csv ("Microcosting.csv", header=TRUE)
#Question 1- Requires computing the Equivalent Annual Cost of the Machine per patient

#Equivalent Annual Cost = (Asset Price x Discount Rate) / (1 - (1 + Discount Rate)^-n)

EAC <-(52605*0.035)/(1-(1.035^-5))
EAC
###EAC per patient-Assuming that the machine serves 130 patients
EAC_unit<-EAC/130
###Computing costs for different interventions###
###Adding cost variables to the dataset###
Microcosting$ctscancost<- Microcosting$ctscans*78
Microcosting$artbloodcost<-Microcosting$artblood*2.80
Microcosting$fbccost<- Microcosting$fbc*2.80
Microcosting$ureacost<-Microcosting$urea*1.10
Microcosting$bloodsugcost<-Microcosting$bloodsug*1.10
Microcosting$livercost<-Microcosting$liver*1.10
Microcosting$creatinicost<-Microcosting$creatini*1.10
Microcosting$troponincost<-Microcosting$troponin*1.10
Microcosting$thyroidcost<-Microcosting$thyroid*1.10
Microcosting$ccost<-Microcosting$cmins*1.82
Microcosting$mgdcost<-Microcosting$mgdmins*0.67
Microcosting$jdcost<-Microcosting$jdmins*0.47
Microcosting$sncost<-Microcosting$snmins*0.92
Microcosting$mncost<-Microcosting$mnmins*0.78
Microcosting$lncost<-Microcosting$lnmins*0.63

#Computing Machine costs###
#These are costs of CT scan and the computed EAC-But the EAC will not be for standard patients
Microcosting$newmachine<-EAC_unit
###Replacing new machine cost for standard patients with zero###
Microcosting$newmachine<-ifelse(Microcosting$tx_rand=="Standard", EAC_unit*0,EAC_unit)
##Computing total machine costs
Microcosting$machinecost<- Microcosting$newmachine+Microcosting$ctscancost
###Computing Staff Costs
Microcosting$staffcost<-Microcosting$ccost+Microcosting$jdcost+Microcosting$mgdcost+Microcosting$sncost+Microcosting$mncost+Microcosting$lncost
Microcosting$investigationcost<-Microcosting$artbloodcost+Microcosting$fbccost+Microcosting$ureacost+Microcosting$bloodsugcost+Microcosting$livercost+Microcosting$creatinicost+Microcosting$troponincost+Microcosting$thyroidcost
##Adding overhead cost###
Microcosting$overheadcost<-21.52
#Computing total costs per patient
Microcosting$totalcost<-Microcosting$machinecost+Microcosting$staffcost+Microcosting$investigationcost+Microcosting$overheadcost
###Summary by the three types of treatment###
summary(Microcosting$totalcost[Microcosting$tx_rand=="Standard"])
summary(Microcosting$totalcost[Microcosting$tx_rand=="CPAP"])
summary(Microcosting$totalcost[Microcosting$tx_rand=="NIPPV"])
###End of R Script###
