
#**********************************************************************************************************
#              HR ANALYTICS CASE STUDY-LOGISTIC REGRESSION MODEL                                          *                                                                             *
#**********************************************************************************************************                                                                                                      *

# Group Members:                                                                                         *
# Ria Nag,Priya Chopra,Piyush Gaur and  Sanajana S Rao
###################################################################################################
#Business Understanding
#Data Understanding
#Data Preparation & EDA
#Model Building 
#Model Evaluation
#################################################################################################

# Business objective:                                                                                     *
#1)A large company named XYZ that employs around 4000 employees, wants to know what changes
#they should make to their workplace, in order to get most of their employees to stay. 
#2)Also, they want to know which of these variables is most important 
#and needs to be addressed right away.

#Goal of this assignment

#We are required to model the probability of attrition using  logistic regression with the available independent variables.
#It will be used by the management to understand what changes they should make to their workplace, in order to get most of their employees to stay.


#**********************************************************************************************************                                                                                                      *


#** Install all Packages if packages are not already installed **
#install.packages("tidyr")
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("corrplot")
#install.packages("stringr")
#install.packages("ade4")
#install.packages("MASS")
#install.packages("car")
#install.packages("e1071')
#install.packages("caret")
#install.packages("cowplot")
#install.packages("caTools")
#install.packages("ROCR")
#install.packages("lift")
#install.packages("stringr")
#install.packages("plyr")
#install.packages("AUC")

#loading all required libraries
library(MASS)
library(car)
library(e1071)
library(caret)
library(ggplot2)
library(cowplot)
library(caTools)
library(ROCR)
library(ade4)
library(dplyr)
library(tidyr)
library(ggplot2)
library(corrplot)
library(stringr)
library(MASS)
library(car)
library(lift)
library(stringr)
library(plyr)
library(AUC)

## define working directory using the command setwd to the directory where your input files are located
##** Clear environment variables **
rm(list=ls())

#loading all input files
out_time<-read.csv("out_time.csv",stringsAsFactors=FALSE)
in_time<-read.csv("in_time.csv",stringsAsFactors=FALSE)
emp<-read.csv("employee_survey_data.csv",stringsAsFactors=FALSE)
manager<-read.csv("manager_survey_data.csv",stringsAsFactors=FALSE)
general_data<-read.csv("general_data.csv",stringsAsFactors=FALSE)


##################################################################################################
######DATA CLEANING OF IN-TIME OR OUT TIME FILES#####################################################3
colnames(in_time)[1]<-"EmployeeID"
colnames(out_time)[1]<-"EmployeeID"
#checking which employee id does not match in in_time,, out_time and emp
length(in_time[which(in_time$EmployeeID!=out_time$EmployeeID),1])
length(emp[which(in_time$EmployeeID!=emp$EmployeeID),1])
#all employee ids match
#checking for which colnames do not match in in_time and out_time
which(!colnames(in_time)%in% colnames(out_time))


out_time[,2:ncol(out_time)]<-sapply(out_time[,2:ncol(out_time)], function(x) as.POSIXct(x,format="%Y-%m-%d %H:%M:%S"))
in_time[,2:ncol(in_time)]<-sapply(in_time[,2:ncol(in_time)], function(x) as.POSIXct(x,format="%Y-%m-%d %H:%M:%S"))


#derived metrics-average time spent by each employee at work per day in hrs

for(j in 1:nrow(in_time))
  {
     for (i in 2:ncol(in_time))
            {
                
            out_time[j,i]<-(out_time[j,i]-in_time[j,i])
         i<-i+1
         }
      j<-j+1
    }
emp$average_time<-apply(out_time[,-1],1,mean,na.rm=TRUE)

#calculating average time spent by each employee at work per day in hrs
emp$average_time<-emp$average_time/3600

#check for NA
colSums(is.na(emp))
#EnvironmentSatisfaction, JobSatisfaction, WorkLifeBalance have less than 50 NA Values
colSums(is.na(general_data))
#NumCompaniesWorked  and TotalWorkingYears have less than 20 NA
colSums(is.na(manager))
#no NA in manager dataframe
#check for blanks
which(sapply(emp, function(x) length(which(x == ""))>0)==T)
which(sapply(manager, function(x) length(which(x == ""))>0)==T)
which(sapply(general_data, function(x) length(which(x == ""))>0)==T)
#no blanks were present
#merging emp,manager and general_data into master file.
master<-merge(emp,manager,"EmployeeID")
master<-merge(master,general_data,"EmployeeID")
#imputing NA with median values in master dataframe
master$EnvironmentSatisfaction[which(is.na(master$EnvironmentSatisfaction))]<-median(master$EnvironmentSatisfaction,na.rm=TRUE)
master$JobSatisfaction[which(is.na(master$JobSatisfaction))]<-median(master$JobSatisfaction,na.rm=TRUE)
master$WorkLifeBalance[which(is.na(master$WorkLifeBalance))]<-median(master$WorkLifeBalance,na.rm=TRUE)
master$NumCompaniesWorked[which(is.na(master$NumCompaniesWorked))]<-median(master$NumCompaniesWorked,na.rm=TRUE)
master$TotalWorkingYears[which(is.na(master$TotalWorkingYears))]<-median(master$TotalWorkingYears,na.rm=TRUE)
colSums(is.na(master))
#no NA in manager dataframe
#DERIVED METRICS- leaves taken per year
#calculating number of NAs in in_time as number of leaves taken by each employee
master$leaves<-apply(in_time,1,function(x)sum(is.na(x)))
######################################################################################################

###########separating numeric and character variables in master############################################

master_chr<-master[,sapply(master,function(x) is.character(x))]
master_num<-master[,sapply(master,function(x) !is.character(x))]
#converting to lowercase all characters
master_chr<-data.frame(apply(master_chr,2,function(x) tolower(x)))
#spell checks were done and no spelling mistakes observed in charater variables
#####checking if employee if is unique and then removing employee id,sandard hours and employee count column from master_num dataframe
#and Over18  from master_chr dataframe as they are not required for analysis
length(unique(master_num$EmployeeID))#<-4410 that is all employee ids are unique
master_num<-master_num[,-c(1,11,16)]
master_chr<-master_chr[,-8]
summary(master_num)
#############DISCRETE AND CONTINUOUS NUMERIC VARIABLE DATA PREPARATION
con_colnames<-c("Age","average_time", "DistanceFromHome","MonthlyIncome","PercentSalaryHike","TotalWorkingYears","YearsAtCompany","YearsSinceLastPromotion","YearsWithCurrManager","leaves")
master_con<-master_num[,con_colnames]
master_dis<-master_num[,!colnames(master_num)%in%colnames(master_con)]

################OUTLIER TREATMENT OF CONTINOUS VARIABLES 
quantile(master_con$Age,seq(0,1,0.01)) 
quantile(master_con$average_time,seq(0,1,0.01)) 
quantile(master_con$DistanceFromHome,seq(0,1,0.01)) 
quantile(master_con$MonthlyIncome,seq(0,1,0.01)) 
#outlier treatment of monthly income variable
master_con$MonthlyIncome[which(master_con$MonthlyIncome<18590.0)]<-18590.0
quantile(master_con$PercentSalaryHike,seq(0,1,0.01)) 
quantile(master_con$TotalWorkingYears,seq(0,1,0.01)) 
#outlier treatment ofTotalWorkingYears variable
master_con$TotalWorkingYears[which(master_con$TotalWorkingYears>32.00)]<-32.00
quantile(master_con$YearsAtCompany,seq(0,1,0.01)) 
#outlier treatment of YearsAtCompany variable
master_con$YearsAtCompany[which(master_con$YearsAtCompany>24)]<-24
quantile(master_con$YearsSinceLastPromotion,seq(0,1,0.01)) 
quantile(master_con$YearsWithCurrManager,seq(0,1,0.01)) 
#outlier treatment of YearsWithCurrManager variable
master_con$YearsWithCurrManager[which(master_con$YearsWithCurrManager>14)]<-14


############ Univariate, segmentation, bivariate analysis of CONTINIOUS VARIABLES ######################
## Correlation in continious variables
corr<- cor( master_con)
corrplot(corr, method="number",order="AOE", outline=TRUE)
View(master_con)
# Years at Company high correlation with YearswithCurrManager
# While other contionious variables Age, TotalWorking Years, Years at Company
# Years with current manager, Years since last promotion seems to be correlated as well


#UNIVARIATE ANALYSIS AND SEGMENTATION
#median monthly income of employees is Rs.50000
boxplot(master_con$MonthlyIncome,ylab="Monthly income in rupees per month")


boxplot(master_con[,-4], las=3)
#Most employees have a age around 35
#Most employees live at less than distance of 10KM from home
#Most employees take average 8 mins to reach office
#Average Percent Salary hike is 15%
#Average Working years of Employees is around 11 years 
#Most employees have 6 years of working expereince in company
#It has been average 2 years since last promtion for employees
#Most employees have been 4 years with current manager
#most emplyoyees take 25 days of leaves per year outside weekend
###############SEGMENTATION AND BIVARIATE ANALYSIS
#LETS make a new dataframe with continous variable and attrition
master_con_plot<-cbind(master_con,master_chr$Attrition)
colnames(master_con_plot)[11]<-"Attrition"


#Lets create a bin for Age 
master_con_plot<- mutate(master_con_plot,age_segments=ifelse(master_con_plot$Age <30, "Young",
                         ifelse(master_con_plot$Age <45, "Middle", "Senior")))
ggplot(master_con_plot,aes(age_segments,fill=factor(Attrition)))+geom_bar(position="fill")
##There is higher level of attrition among young people age group.

## lets create a bin for average time spent at work less than 8 and greater than or equal to 8
master_con_plot<-mutate(master_con_plot, 
                        avg_timesegment= ifelse(master_con_plot$average_time >8, "long_hours"," average hours"))
ggplot(master_con_plot,aes(avg_timesegment,fill=factor(Attrition)))+geom_bar(position="fill")

## The attrition is higher for people working  more than 8 hours  per day

## lets create a bin for number of leaves taken by each employee per year
master_con_plot<-mutate(master_con_plot, 
                        avg_leaves_segment= ifelse(master_con_plot$leaves <=20, "low_leaves",
ifelse(master_con_plot$leaves <=30,"avg_leaves"," high_leaves")))
ggplot(master_con_plot,aes(avg_leaves_segment,fill=factor(Attrition)))+geom_bar(position="fill")
#attrition seems to be marginally decreasing as number of leaves taken by employee increases.
## The attrition is higher for people working  8 hours or less per day
#lets create a bin from Distancefrom home
master_con_plot<- mutate(master_con_plot,distance_home=ifelse(master_con_plot$DistanceFromHome <10, "Near","Far"))
ggplot(master_con_plot,aes(distance_home,fill=factor(Attrition)))+geom_bar(position="fill")
## There is pretty low differenc in attrition for people living less than 10 km or more

#lets create a bin for monthly income
master_con_plot<-mutate(master_con_plot, 
                        income= ifelse(master_con_plot$MonthlyIncome < 50000, "low Salary",
                                       ifelse(master_con_plot$MonthlyIncome < 80000, "Reasonable Salary", "Well Paid" )))
ggplot(master_con_plot,aes(income,fill=factor(Attrition)))+geom_bar(position="fill")
# There is marginally low  difference in attrition based on salary 


# Lets create a bin for salary hike
master_con_plot<-mutate(master_con_plot, 
                        hike= ifelse(master_con_plot$PercentSalaryHike < 12, "Less Hike", "Good Hike" ))
ggplot(master_con_plot,aes(hike,fill=factor(Attrition)))+geom_bar(position="fill")
# There is marginally any difference in attrition due to salary hike

# lets create a bin for total working years
master_con_plot<-mutate(master_con_plot, 
                        workyears= ifelse(master_con_plot$TotalWorkingYears < 5, "Less Exp", 
                                    ifelse(master_con_plot$TotalWorkingYears < 10, "Medium Exp", "High Exp" )))
ggplot(master_con_plot,aes(workyears,fill=factor(Attrition)))+geom_bar(position="fill")
# Attrition seems to be higher for low working years i.e. less than 5

#lets create a bin for association with company
master_con_plot<-mutate(master_con_plot, 
                        comp_association= ifelse(master_con_plot$YearsAtCompany < 5, "Less 5", 
                                           "More than 5" ))
ggplot(master_con_plot,aes(comp_association,fill=factor(Attrition)))+geom_bar(position="fill")
# There is a higher attrition among people who are with company for less than 5 years 

#lets create a histogram for YearsSince last promotion
ggplot(master_con_plot,aes(master_con_plot$YearsSinceLastPromotion,fill=factor(Attrition)))+xlab("Years Since last promotion")+geom_histogram(position="fill",binwidth=3)

#Observation
#Attrition slightly increases after a dip with increase in YearsSince last promotion after 10 years


# lets create a bin for association with company
master_con_plot<-mutate(master_con_plot, 
                        YearswithManager= ifelse(master_con_plot$YearsWithCurrManager  < 5, "Low Association", 
                                             "High Association" ))
ggplot(master_con_plot,aes(YearswithManager,fill=factor(Attrition)))+geom_bar(position="fill")
# There is a higher attrition for low association with manager <5 years


###########SCALING OF CONTINOUS VARIABLES###########################################################33
master_con<-data.frame(scale(master_con))

#the continuous variables were not transformed to WOE to avoid data clumping and loss of predictive power
#number of NA were also very less in the data and random hence they were imputed by median values and not WOE
####################CHARACTER AD DISCRETE NUMERIC VARIABLE DATA PREPARATION
master_chr<- master_chr%>% mutate_if(is.character,as.factor)
master_dis<- master_dis%>% mutate_if(is.numeric,as.factor)
##########################COMBINING ALL DATAFRAMES OF MASTER LIST
master_final<-cbind(master_chr,master_con)
master_final<-cbind(master_final,master_dis)
##############################EDA##################################################################


#EDA FOR CHARACTER VARIABLES
#univariate analysis
#calculating percentage of attrition
length(which(master_final$Attrition=="yes"))/4410*100#->16.12%
ggplot(master_final,aes(x=JobRole))+geom_bar(fill="red")+ theme(axis.text.x=element_text(angle=90, hjust=1))
ggplot(master_final,aes(x=Attrition))+geom_bar(fill="red")
ggplot(master_final,aes(x=BusinessTravel))+geom_bar(fill="red")+ theme(axis.text.x=element_text(angle=90, hjust=1))
ggplot(master_final,aes(x=Department))+geom_bar(fill="red")+ theme(axis.text.x=element_text(angle=90, hjust=1))
ggplot(master_final,aes(x=EducationField))+geom_bar(fill="red")+ theme(axis.text.x=element_text(angle=90, hjust=1))
ggplot(master_final,aes(x=Gender))+geom_bar(fill="red")+ theme(axis.text.x=element_text(angle=90, hjust=1))
ggplot(master_final,aes(x=MaritalStatus))+geom_bar(fill="red")+ theme(axis.text.x=element_text(angle=90, hjust=1))


#conclusion- 
#1)most common job role is o sales excecutive
#2)most employees traveled rarely for business
#3)percentage of attrition among employees is 16.12%
#4)most employees are from research and development department
#5)educational feild of most employees is life sciences followed by medical
#6)there are more male than female employees and more married than single or divorced employees.

#Bivariate analysis

ggplot(master_final,aes(x=JobRole ,fill=Attrition))+geom_bar(position="fill")+ theme(axis.text.x=element_text(angle=90, hjust=1))
ggplot(master_final,aes(x=EducationField ,fill=Attrition))+geom_bar(position="fill")+ theme(axis.text.x=element_text(angle=90, hjust=1))
ggplot(master_final,aes(x=BusinessTravel ,fill=Attrition))+geom_bar(position="fill")+ theme(axis.text.x=element_text(angle=90, hjust=1))
ggplot(master_final,aes(x=Department,fill=Attrition))+geom_bar(position="fill")+ theme(axis.text.x=element_text(angle=90, hjust=1))
ggplot(master_final,aes(x=Gender ,fill=Attrition))+geom_bar(position="fill")+ theme(axis.text.x=element_text(angle=90, hjust=1))
ggplot(master_final,aes(x=MaritalStatus ,fill=Attrition))+geom_bar(position="fill")+ theme(axis.text.x=element_text(angle=90, hjust=1))
ggplot(master_final,aes(x=JobRole ,fill=Attrition))+geom_bar(position="fill")+ theme(axis.text.x=element_text(angle=90, hjust=1))

#CONCLUSIONS:
# higher percentage of attrition is among people whose educational field  is human resources.
# higher percentage of attrition is among people whose job role is research director
# higher percentage of attrition is among people who travel frequently for business
# higher percentage of attrition is among people whose department is human resources
# higher percentage of attrition is among people who are single
#No significant difference in attrition among male and female employees.

#####################################################################################################
#EDA FOR DISCRETE VARIABLES
######################################################################
#univariate analysis
ggplot(master_final,aes(x=TrainingTimesLastYear))+geom_bar(fill="red")
ggplot(master_final,aes(x=NumCompaniesWorked))+geom_bar(fill="red")
ggplot(master_final,aes(x=EnvironmentSatisfaction))+geom_bar(fill="red")
ggplot(master_final,aes(x=Education))+geom_bar(fill="red")
ggplot(master_final,aes(x=JobLevel))+geom_bar(fill="red")
ggplot(master_final,aes(x=JobSatisfaction))+geom_bar(fill="red")
ggplot(master_final,aes(x=WorkLifeBalance))+geom_bar(fill="red")
ggplot(master_final,aes(x=JobInvolvement))+geom_bar(fill="red")
ggplot(master_final,aes(x=PerformanceRating))+geom_bar(fill="red")
ggplot(master_final,aes(x=StockOptionLevel))+geom_bar(fill="red")

#CONCLUSIONS:
#most employees have taken 2 to 3 training last year.
#most employees have worked for 1 company previously.
#Higher number of employees have environment satisfaction at the level 3 or 4 than 1 or 2.
#highest number of emplyoees have education of the level 3 that is bachelor's level.
#most employees have job level 1 or 2
#Most employees have expressed job satisfaction of level 3 or 4 that high level of job satisfaction.
#most employees have expressed better of work life balance of  level 3
#most employees have high job involvement of level 3
#most employees have received a performance rating of 3 instead of 4
#most employees have stock option level of 0 or 1.


#bivariate analysis
ggplot(master_final,aes(x=TrainingTimesLastYear,fill=Attrition))+geom_bar(position="fill")
#it shows there is very low percentage of attrition with people taking 6 TrainingTimesLastYear 
ggplot(master_final,aes(x=NumCompaniesWorked ,fill=Attrition))+geom_bar(position="fill")
#attrition is higher for employees with total number of companies that they have worked for is greater than 4 upto 9 except when it is 8.
Discrete_plot_data <- master_final

Discrete_plot_data$EnvironmentSatisfaction <- ifelse(Discrete_plot_data$EnvironmentSatisfaction==1, "Low",
                                                     
                                                     ifelse(Discrete_plot_data$EnvironmentSatisfaction==2, "Medium", 
                                                            
                                                            ifelse(Discrete_plot_data$EnvironmentSatisfaction==3, "High", "Very High")))

ggplot(Discrete_plot_data,aes(x=EnvironmentSatisfaction ,fill=Attrition))+geom_bar(position="fill")
# attritions are little higher when Environment satifaction is low

Discrete_plot_data$Education <- ifelse(Discrete_plot_data$Education==1, "Below College",
                                       
                                       ifelse(Discrete_plot_data$Education==2, "College", 
                                              
                                              ifelse(Discrete_plot_data$Education==3, "Bachelor",
                                                     
                                                     ifelse(Discrete_plot_data$Education==4, "Masters", "Doctors"))))

ggplot(Discrete_plot_data,aes(x=Education ,fill=Attrition))+geom_bar(position="fill")

# Attrition is little higher for people who college education



ggplot(Discrete_plot_data,aes(x=JobLevel ,fill=Attrition))+geom_bar(position="fill")
# No Significant Information found

Discrete_plot_data$JobSatisfaction <- ifelse(Discrete_plot_data$JobSatisfaction==1, "Low",
                                             
                                             ifelse(Discrete_plot_data$JobSatisfaction==2, "Medium", 
                                                    
                                                    ifelse(Discrete_plot_data$JobSatisfaction==3, "High", "Very High")))


ggplot(Discrete_plot_data,aes(x=JobSatisfaction ,fill=Attrition))+geom_bar(position="fill")
# Attrition is little higher where Job Satisfaction is low.


Discrete_plot_data$WorkLifeBalance <- ifelse(Discrete_plot_data$WorkLifeBalance==1, "Bad",
                                             
                                             ifelse(Discrete_plot_data$WorkLifeBalance==2, "Good", 
                                                    
                                                    ifelse(Discrete_plot_data$WorkLifeBalance==3, "Better", "Best")))

ggplot(Discrete_plot_data,aes(x=WorkLifeBalance ,fill=Attrition))+geom_bar(position="fill")
# Attrition is little higher where WorkLife Balance is bad.



Discrete_plot_data$JobInvolvement <- ifelse(Discrete_plot_data$JobInvolvement==1, "Low",
                                            
                                            ifelse(Discrete_plot_data$JobInvolvement==2, "Medium", 
                                                   
                                                   ifelse(Discrete_plot_data$JobInvolvement==3, "High", "Very High")))


ggplot(Discrete_plot_data,aes(x=JobInvolvement ,fill=Attrition))+geom_bar(position="fill")

# ATTRITION is marginally higher for low job involvement



Discrete_plot_data$PerformanceRating <- ifelse(Discrete_plot_data$PerformanceRating==1, "Low",
                                               
                                               ifelse(Discrete_plot_data$PerformanceRating==2, "Good", 
                                                      
                                                      ifelse(Discrete_plot_data$PerformanceRating==3, "Excellent", "Outstanding")))

ggplot(Discrete_plot_data,aes(x=PerformanceRating ,fill=Attrition))+geom_bar(position="fill")


# No Significant Informatiion found


ggplot(Discrete_plot_data,aes(x=StockOptionLevel ,fill=Attrition))+geom_bar(position="fill")

# No Significant Informatiion found

################################################################################################
#Creating Dummy variables
# creating dummy variables for factor attributes
dummy_dis<- data.frame(sapply(master_dis, 
                            function(x) data.frame(model.matrix(~x,data =master_dis))[,-1]))
dummy_chr<- data.frame(sapply(master_chr, 
                              function(x) data.frame(model.matrix(~x,data =master_chr))[,-1]))
###########CREATING FINAL DATA FRAME FOR MODEL PREPARATION
final<-cbind(dummy_dis,dummy_chr)
final<-cbind(final,master_con)


#lets look at correlation among variables
corr<- cor(final)

#model_preparation
# splitting the data between train and test

set.seed(100)

indices = sample.split(final$Attrition, SplitRatio = 0.7)

train = final[indices,]

test = final[!(indices),]
which(colnames(test)=="Attrition")
#40

########################################################################
# Logistic Regression: 

#Initial model
model_1 = glm(Attrition ~ ., data = train, family = "binomial")
summary(model_1)
# Stepwise selection
model_2<- stepAIC(model_1, direction="both")

summary(model_2)
model2_table<-data.frame(vif(model_2))
#############3##################################################################
#removing MaritalStatus.xmarried as it has vif above 2 and low p values
model_2<-glm(formula = Attrition ~ EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
               WorkLifeBalance.x4 + JobInvolvement.x3 + Education.x3 + Education.x4 + 
               Education.x5 + JobLevel.x2 + JobLevel.x5 + NumCompaniesWorked.x1 + 
               NumCompaniesWorked.x4 + NumCompaniesWorked.x5 + NumCompaniesWorked.x6 + 
               NumCompaniesWorked.x7 + NumCompaniesWorked.x8 + NumCompaniesWorked.x9 + 
               StockOptionLevel.x1 + TrainingTimesLastYear.x1 + TrainingTimesLastYear.x3 + 
               TrainingTimesLastYear.x4 + TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + 
               BusinessTravel.xtravel_frequently + BusinessTravel.xtravel_rarely + 
               Department.xresearch...development + Department.xsales + 
               EducationField.xmarketing + EducationField.xother + Gender + 
               JobRole.xlaboratory.technician + JobRole.xresearch.director + 
               JobRole.xresearch.scientist + JobRole.xsales.executive + MaritalStatus.xsingle + Age + average_time + 
               DistanceFromHome + MonthlyIncome + TotalWorkingYears + YearsSinceLastPromotion + 
               YearsWithCurrManager, family = "binomial", data = train)
summary(model_2)
model2_table<-data.frame(vif(model_2))

######################################################################################
#removing BusinessTravel.xtravel_frequently  as it has vif above 2 
model_3<-glm(formula = Attrition ~ EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
               WorkLifeBalance.x4 + JobInvolvement.x3 + Education.x3 + Education.x4 + 
               Education.x5 + JobLevel.x2 + JobLevel.x5 + NumCompaniesWorked.x1 + 
               NumCompaniesWorked.x4 + NumCompaniesWorked.x5 + NumCompaniesWorked.x6 + 
               NumCompaniesWorked.x7 + NumCompaniesWorked.x8 + NumCompaniesWorked.x9 + 
               StockOptionLevel.x1 + TrainingTimesLastYear.x1 + TrainingTimesLastYear.x3 + 
               TrainingTimesLastYear.x4 + TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + 
               BusinessTravel.xtravel_rarely+
               Department.xsales+Department.xresearch...development +
               EducationField.xmarketing + EducationField.xother + Gender + 
               JobRole.xlaboratory.technician + JobRole.xresearch.director + 
               JobRole.xresearch.scientist + JobRole.xsales.executive + MaritalStatus.xsingle + Age + average_time + 
               DistanceFromHome + MonthlyIncome + TotalWorkingYears + YearsSinceLastPromotion + 
               YearsWithCurrManager, family = "binomial", data = train)
summary(model_3)
model3_table<-data.frame(vif(model_3))
#AIC changes by less than 100 so it is acceptable
######################################################################################
#removing Department.xsales  as it has vif above 2 
model_3<-glm(formula = Attrition ~ EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
               WorkLifeBalance.x4 + JobInvolvement.x3 + Education.x3 + Education.x4 + 
               Education.x5 + JobLevel.x2 + JobLevel.x5 + NumCompaniesWorked.x1 + 
               NumCompaniesWorked.x4 + NumCompaniesWorked.x5 + NumCompaniesWorked.x6 + 
               NumCompaniesWorked.x7 + NumCompaniesWorked.x8 + NumCompaniesWorked.x9 + 
               StockOptionLevel.x1 + TrainingTimesLastYear.x1 + TrainingTimesLastYear.x3 + 
               TrainingTimesLastYear.x4 + TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + 
               BusinessTravel.xtravel_rarely+
               Department.xresearch...development +
               EducationField.xmarketing + EducationField.xother + Gender + 
               JobRole.xlaboratory.technician + JobRole.xresearch.director + 
               JobRole.xresearch.scientist + JobRole.xsales.executive + MaritalStatus.xsingle + Age + average_time + 
               DistanceFromHome + MonthlyIncome + TotalWorkingYears + YearsSinceLastPromotion + 
               YearsWithCurrManager, family = "binomial", data = train)
summary(model_3)
model3_table<-data.frame(vif(model_3))
#AIC changes by less than 100 so it is acceptable

######################################################################################
######################################################################################
#removing WorkLifeBalance.x3  as it has vif above 2 
model_3<-glm(formula = Attrition ~ EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4 + WorkLifeBalance.x2 +  
               WorkLifeBalance.x4 + JobInvolvement.x3 + Education.x3 + Education.x4 + 
               Education.x5 + JobLevel.x2 + JobLevel.x5 + NumCompaniesWorked.x1 + 
               NumCompaniesWorked.x4 + NumCompaniesWorked.x5 + NumCompaniesWorked.x6 + 
               NumCompaniesWorked.x7 + NumCompaniesWorked.x8 + NumCompaniesWorked.x9 + 
               StockOptionLevel.x1 + TrainingTimesLastYear.x1 + TrainingTimesLastYear.x3 + 
               TrainingTimesLastYear.x4 + TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + 
               BusinessTravel.xtravel_rarely+
               Department.xresearch...development +
               EducationField.xmarketing + EducationField.xother + Gender + 
               JobRole.xlaboratory.technician + JobRole.xresearch.director + 
               JobRole.xresearch.scientist + JobRole.xsales.executive + MaritalStatus.xsingle + Age + average_time + 
               DistanceFromHome + MonthlyIncome + TotalWorkingYears + YearsSinceLastPromotion + 
               YearsWithCurrManager, family = "binomial", data = train)
summary(model_3)
model3_table<-data.frame(vif(model_3))
#AIC changes by less than 100 so it is acceptable

######################################################################################
######################################################################################
#removing TotalWorkingYears  as it has vif above 2 
model_3<-glm(formula = Attrition ~ EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4 + WorkLifeBalance.x2 +  
               WorkLifeBalance.x4 + JobInvolvement.x3 + Education.x3 + Education.x4 + 
               Education.x5 + JobLevel.x2 + JobLevel.x5 + NumCompaniesWorked.x1 + 
               NumCompaniesWorked.x4 + NumCompaniesWorked.x5 + NumCompaniesWorked.x6 + 
               NumCompaniesWorked.x7 + NumCompaniesWorked.x8 + NumCompaniesWorked.x9 + 
               StockOptionLevel.x1 + TrainingTimesLastYear.x1 + TrainingTimesLastYear.x3 + 
               TrainingTimesLastYear.x4 + TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + 
               BusinessTravel.xtravel_rarely+
               Department.xresearch...development +
               EducationField.xmarketing + EducationField.xother + Gender + 
               JobRole.xlaboratory.technician + JobRole.xresearch.director + 
               JobRole.xresearch.scientist + JobRole.xsales.executive + MaritalStatus.xsingle + Age + average_time + 
               DistanceFromHome + MonthlyIncome +  + YearsSinceLastPromotion + 
               YearsWithCurrManager, family = "binomial", data = train)
summary(model_3)
model3_table<-data.frame(vif(model_3))
#AIC changes by less than 100 so it is acceptable
#now all variables have vif less than 2
#############################################################################################
#removing Gender  as it has high p values
model_4<-glm(formula = Attrition ~ EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4 + WorkLifeBalance.x2 +  
               WorkLifeBalance.x4 + JobInvolvement.x3 + Education.x3 + Education.x4 + 
               Education.x5 + JobLevel.x2 + JobLevel.x5 + NumCompaniesWorked.x1 + 
               NumCompaniesWorked.x4 + NumCompaniesWorked.x5 + NumCompaniesWorked.x6 + 
               NumCompaniesWorked.x7 + NumCompaniesWorked.x8 + NumCompaniesWorked.x9 + 
               StockOptionLevel.x1 + TrainingTimesLastYear.x1 + TrainingTimesLastYear.x3 + 
               TrainingTimesLastYear.x4 + TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + 
               BusinessTravel.xtravel_rarely+
               Department.xresearch...development +
               EducationField.xmarketing + EducationField.xother +  
               JobRole.xlaboratory.technician + JobRole.xresearch.director + 
               JobRole.xresearch.scientist + JobRole.xsales.executive + MaritalStatus.xsingle + Age + average_time + 
               DistanceFromHome + MonthlyIncome +  + YearsSinceLastPromotion + 
               YearsWithCurrManager, family = "binomial", data = train)
summary(model_4)
model4_table<-data.frame(vif(model_4))
#AIC did not change much so it is acceptable.
#############################################################################################
#removing WorkLifeBalance.x2  as it has high p values
model_4<-glm(formula = Attrition ~ EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4  +  
               WorkLifeBalance.x4 + JobInvolvement.x3 + Education.x3 + Education.x4 + 
               Education.x5 + JobLevel.x2 + JobLevel.x5 + NumCompaniesWorked.x1 + 
               NumCompaniesWorked.x4 + NumCompaniesWorked.x5 + NumCompaniesWorked.x6 + 
               NumCompaniesWorked.x7 + NumCompaniesWorked.x8 + NumCompaniesWorked.x9 + 
               StockOptionLevel.x1 + TrainingTimesLastYear.x1 + TrainingTimesLastYear.x3 + 
               TrainingTimesLastYear.x4 + TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + 
               BusinessTravel.xtravel_rarely+
               Department.xresearch...development +
               EducationField.xmarketing + EducationField.xother +  + 
               JobRole.xlaboratory.technician + JobRole.xresearch.director + 
               JobRole.xresearch.scientist + JobRole.xsales.executive + MaritalStatus.xsingle + Age + average_time + 
               DistanceFromHome + MonthlyIncome +  + YearsSinceLastPromotion + 
               YearsWithCurrManager, family = "binomial", data = train)
summary(model_4)
model4_table<-data.frame(vif(model_4))
#AIC did not change much so it is acceptable
##################################################################################################
#removing JobInvolvement.x3  as it has high p values
model_4<-glm(formula = Attrition ~ EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4  +  
               WorkLifeBalance.x4  + Education.x3 + Education.x4 + 
               Education.x5 + JobLevel.x2 + JobLevel.x5 + NumCompaniesWorked.x1 + 
               NumCompaniesWorked.x4 + NumCompaniesWorked.x5 + NumCompaniesWorked.x6 + 
               NumCompaniesWorked.x7 + NumCompaniesWorked.x8 + NumCompaniesWorked.x9 + 
               StockOptionLevel.x1 + TrainingTimesLastYear.x1 + TrainingTimesLastYear.x3 + 
               TrainingTimesLastYear.x4 + TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + 
               BusinessTravel.xtravel_rarely+
               Department.xresearch...development +
               EducationField.xmarketing + EducationField.xother +  + 
               JobRole.xlaboratory.technician + JobRole.xresearch.director + 
               JobRole.xresearch.scientist + JobRole.xsales.executive + MaritalStatus.xsingle + Age + average_time + 
               DistanceFromHome + MonthlyIncome +  + YearsSinceLastPromotion + 
               YearsWithCurrManager, family = "binomial", data = train)
summary(model_4)
model4_table<-data.frame(vif(model_4))
#AIC did not change much so it is acceptable

#################################################################################################
#removing WorkLifeBalance.x4  as it has high p values
model_4<-glm(formula = Attrition ~ EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4  +  
                 Education.x3 + Education.x4 + 
               Education.x5 + JobLevel.x2 + JobLevel.x5 + NumCompaniesWorked.x1 + 
               NumCompaniesWorked.x4 + NumCompaniesWorked.x5 + NumCompaniesWorked.x6 + 
               NumCompaniesWorked.x7 + NumCompaniesWorked.x8 + NumCompaniesWorked.x9 + 
               StockOptionLevel.x1 + TrainingTimesLastYear.x1 + TrainingTimesLastYear.x3 + 
               TrainingTimesLastYear.x4 + TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + 
               BusinessTravel.xtravel_rarely+
               Department.xresearch...development +
               EducationField.xmarketing + EducationField.xother +  + 
               JobRole.xlaboratory.technician + JobRole.xresearch.director + 
               JobRole.xresearch.scientist + JobRole.xsales.executive + MaritalStatus.xsingle + Age + average_time + 
               DistanceFromHome + MonthlyIncome +  + YearsSinceLastPromotion + 
               YearsWithCurrManager, family = "binomial", data = train)
summary(model_4)
model4_table<-data.frame(vif(model_4))
#AIC did not change much so it is acceptable
###################################################################################################
#removing JobLevel.x5   as it has high p values
model_4<-glm(formula = Attrition ~ EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4  +  
                Education.x3 + Education.x4 + 
               Education.x5 + JobLevel.x2  + NumCompaniesWorked.x1 + 
               NumCompaniesWorked.x4 + NumCompaniesWorked.x5 + NumCompaniesWorked.x6 + 
               NumCompaniesWorked.x7 + NumCompaniesWorked.x8 + NumCompaniesWorked.x9 + 
               StockOptionLevel.x1 + TrainingTimesLastYear.x1 + TrainingTimesLastYear.x3 + 
               TrainingTimesLastYear.x4 + TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + 
               BusinessTravel.xtravel_rarely+
               Department.xresearch...development +
               EducationField.xmarketing + EducationField.xother +  + 
               JobRole.xlaboratory.technician + JobRole.xresearch.director + 
               JobRole.xresearch.scientist + JobRole.xsales.executive + MaritalStatus.xsingle + Age + average_time + 
               DistanceFromHome + MonthlyIncome +  + YearsSinceLastPromotion + 
               YearsWithCurrManager, family = "binomial", data = train)
summary(model_4)
model4_table<-data.frame(vif(model_4))
#AIC did not change much so it is acceptable
#################################################################################################
#removing Education.x3   as it has high p values
model_4<-glm(formula = Attrition ~ EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4  +  
               Education.x4 + 
               Education.x5 + JobLevel.x2  + NumCompaniesWorked.x1 + 
               NumCompaniesWorked.x4 + NumCompaniesWorked.x5 + NumCompaniesWorked.x6 + 
               NumCompaniesWorked.x7 + NumCompaniesWorked.x8 + NumCompaniesWorked.x9 + 
               StockOptionLevel.x1 + TrainingTimesLastYear.x1 + TrainingTimesLastYear.x3 + 
               TrainingTimesLastYear.x4 + TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + 
               BusinessTravel.xtravel_rarely+
               Department.xresearch...development +
               EducationField.xmarketing + EducationField.xother +  + 
               JobRole.xlaboratory.technician + JobRole.xresearch.director + 
               JobRole.xresearch.scientist + JobRole.xsales.executive + MaritalStatus.xsingle + Age + average_time + 
               DistanceFromHome + MonthlyIncome +  + YearsSinceLastPromotion + 
               YearsWithCurrManager, family = "binomial", data = train)
summary(model_4)
model4_table<-data.frame(vif(model_4))
#AIC did not change much so it is acceptable
###################################################################################################
#removing DistanceFromHome   as it has high p values
model_4<-glm(formula = Attrition ~ EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4  +  
               Education.x4 + 
               Education.x5 + JobLevel.x2  + NumCompaniesWorked.x1 + 
               NumCompaniesWorked.x4 + NumCompaniesWorked.x5 + NumCompaniesWorked.x6 + 
               NumCompaniesWorked.x7 + NumCompaniesWorked.x8 + NumCompaniesWorked.x9 + 
               StockOptionLevel.x1 + TrainingTimesLastYear.x1 + TrainingTimesLastYear.x3 + 
               TrainingTimesLastYear.x4 + TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + 
               BusinessTravel.xtravel_rarely+
               Department.xresearch...development +
               EducationField.xmarketing + EducationField.xother +  + 
               JobRole.xlaboratory.technician + JobRole.xresearch.director + 
               JobRole.xresearch.scientist + JobRole.xsales.executive + MaritalStatus.xsingle + Age + average_time + 
                MonthlyIncome +  + YearsSinceLastPromotion + 
               YearsWithCurrManager, family = "binomial", data = train)
summary(model_4)
model4_table<-data.frame(vif(model_4))
#AIC did not change much so it is acceptable
#############################################################################################



#removing Education.x4   as it has high p values

model_4<-glm(formula = Attrition ~ EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4  +  
               Education.x5 + JobLevel.x2  + NumCompaniesWorked.x1 + 
               NumCompaniesWorked.x4 + NumCompaniesWorked.x5 + NumCompaniesWorked.x6 + 
               NumCompaniesWorked.x7 + NumCompaniesWorked.x8 + NumCompaniesWorked.x9 + 
               StockOptionLevel.x1 + TrainingTimesLastYear.x1 + TrainingTimesLastYear.x3 + 
               TrainingTimesLastYear.x4 + TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + 
               BusinessTravel.xtravel_rarely+
               Department.xresearch...development +
               EducationField.xmarketing + EducationField.xother +  + 
               JobRole.xlaboratory.technician + JobRole.xresearch.director + 
               JobRole.xresearch.scientist + JobRole.xsales.executive + MaritalStatus.xsingle + Age + average_time + 
                MonthlyIncome +  + YearsSinceLastPromotion + 
               YearsWithCurrManager, family = "binomial", data = train)
summary(model_4)
model4_table<-data.frame(vif(model_4))
#AIC did not change much so it is acceptable.



#################################################################################################

#removing TrainingTimesLastYear.x3   as it has high p values
model_4<-glm(formula = Attrition ~ EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4  +  
               Education.x5 + JobLevel.x2  + NumCompaniesWorked.x1 + 
               NumCompaniesWorked.x4 + NumCompaniesWorked.x5 + NumCompaniesWorked.x6 + 
               NumCompaniesWorked.x7 + NumCompaniesWorked.x8 + NumCompaniesWorked.x9 + 
                TrainingTimesLastYear.x1  + 
               TrainingTimesLastYear.x4 + TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + 
               BusinessTravel.xtravel_rarely+
               Department.xresearch...development +
               EducationField.xmarketing + EducationField.xother +  + 
               JobRole.xlaboratory.technician + JobRole.xresearch.director + 
               JobRole.xresearch.scientist + JobRole.xsales.executive + MaritalStatus.xsingle + Age + average_time + 
               MonthlyIncome +  + YearsSinceLastPromotion + 
               YearsWithCurrManager, family = "binomial", data = train)
summary(model_4)
model4_table<-data.frame(vif(model_4))
#AIC did not change much so it is acceptable.

#################################################################################################

#removing TrainingTimesLastYear.x1   as it has high p values
model_4<-glm(formula = Attrition ~ EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4  +  
               Education.x5 + JobLevel.x2  + NumCompaniesWorked.x1 + 
               NumCompaniesWorked.x4 + NumCompaniesWorked.x5 + NumCompaniesWorked.x6 + 
               NumCompaniesWorked.x7 + NumCompaniesWorked.x8 + NumCompaniesWorked.x9 + 
               TrainingTimesLastYear.x4 + TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + 
               BusinessTravel.xtravel_rarely+
               Department.xresearch...development +
               EducationField.xmarketing + EducationField.xother +  + 
               JobRole.xlaboratory.technician + JobRole.xresearch.director + 
               JobRole.xresearch.scientist + JobRole.xsales.executive + MaritalStatus.xsingle + Age + average_time + 
               MonthlyIncome +  + YearsSinceLastPromotion + 
               YearsWithCurrManager, family = "binomial", data = train)
summary(model_4)
model4_table<-data.frame(vif(model_4))
#AIC did not change much so it is acceptable.


#################################################################################################

#removing Education.x5   as it has high p values
model_4<-glm(formula = Attrition ~ EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4  +  
                JobLevel.x2  + NumCompaniesWorked.x1 + 
               NumCompaniesWorked.x4 + NumCompaniesWorked.x5 + NumCompaniesWorked.x6 + 
               NumCompaniesWorked.x7 + NumCompaniesWorked.x8 + NumCompaniesWorked.x9 + 
               TrainingTimesLastYear.x4 + TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + 
               Department.xresearch...development +
               EducationField.xmarketing + EducationField.xother +  + 
               JobRole.xlaboratory.technician + JobRole.xresearch.director + 
               JobRole.xresearch.scientist + JobRole.xsales.executive + MaritalStatus.xsingle + Age + average_time + 
               MonthlyIncome +  + YearsSinceLastPromotion + 
               YearsWithCurrManager, family = "binomial", data = train)
summary(model_4)
model4_table<-data.frame(vif(model_4))
#AIC did not change much so it is acceptable.



#################################################################################################

#removing TrainingTimesLastYear.x5   as it has high p values
model_4<-glm(formula = Attrition ~ EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4  +  
               JobLevel.x2  + NumCompaniesWorked.x1 + 
               NumCompaniesWorked.x4 + NumCompaniesWorked.x5 + NumCompaniesWorked.x6 + 
               NumCompaniesWorked.x7 + NumCompaniesWorked.x8 + NumCompaniesWorked.x9 + 
               TrainingTimesLastYear.x4  + TrainingTimesLastYear.x6 + 
               Department.xresearch...development +
               EducationField.xmarketing + EducationField.xother +  + 
               JobRole.xlaboratory.technician + JobRole.xresearch.director + 
               JobRole.xresearch.scientist + JobRole.xsales.executive + MaritalStatus.xsingle + Age + average_time + 
               MonthlyIncome +  + YearsSinceLastPromotion + 
               YearsWithCurrManager, family = "binomial", data = train)
summary(model_4)
model4_table<-data.frame(vif(model_4))
#AIC did not change much so it is acceptable.


#################################################################################################

#removing NumCompaniesWorked.x4   as it has high p values
model_4<-glm(formula = Attrition ~ EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4  +  
               JobLevel.x2  + NumCompaniesWorked.x1 + 
                NumCompaniesWorked.x5 + NumCompaniesWorked.x6 + 
               NumCompaniesWorked.x7 + NumCompaniesWorked.x8 + NumCompaniesWorked.x9 + 
               TrainingTimesLastYear.x4  + TrainingTimesLastYear.x6 + 
               Department.xresearch...development +
               EducationField.xmarketing + EducationField.xother +  + 
               JobRole.xlaboratory.technician + JobRole.xresearch.director + 
               JobRole.xresearch.scientist + JobRole.xsales.executive + MaritalStatus.xsingle + Age + average_time + 
               MonthlyIncome +  + YearsSinceLastPromotion + 
               YearsWithCurrManager, family = "binomial", data = train)
summary(model_4)
model4_table<-data.frame(vif(model_4))
#AIC did not change much so it is acceptable.
#################################################################################################

#removing EducationField.xother   as it has high p values
model_4<-glm(formula = Attrition ~ EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4  +  
               JobLevel.x2  + NumCompaniesWorked.x1 + 
               NumCompaniesWorked.x5 + NumCompaniesWorked.x6 + 
               NumCompaniesWorked.x7 + NumCompaniesWorked.x8 + NumCompaniesWorked.x9 + 
               TrainingTimesLastYear.x4  + TrainingTimesLastYear.x6 + 
               Department.xresearch...development +
               EducationField.xmarketing + +
               JobRole.xlaboratory.technician + JobRole.xresearch.director + 
               JobRole.xresearch.scientist + JobRole.xsales.executive + MaritalStatus.xsingle + Age + average_time + 
               MonthlyIncome +  + YearsSinceLastPromotion + 
               YearsWithCurrManager, family = "binomial", data = train)
summary(model_4)
model4_table<-data.frame(vif(model_4))
#AIC did not change much so it is acceptable.

#################################################################################################

#removing MonthlyIncome   as it has high p values
model_4<-glm(formula = Attrition ~ EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4  +  
               JobLevel.x2  + NumCompaniesWorked.x1 + 
               NumCompaniesWorked.x5 + NumCompaniesWorked.x6 + 
               NumCompaniesWorked.x7 + NumCompaniesWorked.x8 + NumCompaniesWorked.x9 + 
               TrainingTimesLastYear.x4  + TrainingTimesLastYear.x6 + 
               Department.xresearch...development +
               EducationField.xmarketing + +
               JobRole.xlaboratory.technician + JobRole.xresearch.director + 
               JobRole.xresearch.scientist + JobRole.xsales.executive + MaritalStatus.xsingle + Age + average_time + 
                 YearsSinceLastPromotion + 
               YearsWithCurrManager, family = "binomial", data = train)
summary(model_4)
model4_table<-data.frame(vif(model_4))
#AIC did not change much so it is acceptable.
#################################################################################################

#removing NumCompaniesWorked.x8   as it has high p values
model_4<-glm(formula = Attrition ~ EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4  +  
               JobLevel.x2  + NumCompaniesWorked.x1 + 
               NumCompaniesWorked.x5 + NumCompaniesWorked.x6 + 
               NumCompaniesWorked.x7  + NumCompaniesWorked.x9 + 
               TrainingTimesLastYear.x4  + TrainingTimesLastYear.x6 + 
               Department.xresearch...development +
               EducationField.xmarketing + +
               JobRole.xlaboratory.technician + JobRole.xresearch.director + 
               JobRole.xresearch.scientist + JobRole.xsales.executive + MaritalStatus.xsingle + Age + average_time + 
               YearsSinceLastPromotion + 
               YearsWithCurrManager, family = "binomial", data = train)
summary(model_4)
model4_table<-data.frame(vif(model_4))
#AIC did not change much so it is acceptable.
#################################################################################################

#removing TrainingTimesLastYear.x4   as it has high p values
model_4<-glm(formula = Attrition ~ EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4  +  
               JobLevel.x2  + NumCompaniesWorked.x1 + 
               NumCompaniesWorked.x5 + NumCompaniesWorked.x6 + 
               NumCompaniesWorked.x7  + NumCompaniesWorked.x9 + 
                TrainingTimesLastYear.x6 + 
               Department.xresearch...development +
               EducationField.xmarketing + +
               JobRole.xlaboratory.technician + JobRole.xresearch.director + 
               JobRole.xresearch.scientist + JobRole.xsales.executive + MaritalStatus.xsingle + Age + average_time + 
               YearsSinceLastPromotion + 
               YearsWithCurrManager, family = "binomial", data = train)
summary(model_4)
model4_table<-data.frame(vif(model_4))
#AIC did not change much so it is acceptable.
###################################################################################################

#removing EducationField.xmarketing   as it has high p values
model_4<-glm(formula = Attrition ~ EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4  +  
               JobLevel.x2  + NumCompaniesWorked.x1 + 
               NumCompaniesWorked.x5 + NumCompaniesWorked.x6 + 
               NumCompaniesWorked.x7  + NumCompaniesWorked.x9 + 
               TrainingTimesLastYear.x6 + 
               Department.xresearch...development +
               JobRole.xlaboratory.technician + JobRole.xresearch.director + 
               JobRole.xresearch.scientist + JobRole.xsales.executive + MaritalStatus.xsingle + Age + average_time + 
               YearsSinceLastPromotion + 
               YearsWithCurrManager, family = "binomial", data = train)
summary(model_4)
model4_table<-data.frame(vif(model_4))
#AIC did not change much so it is acceptable.

###################################################################################################

#removing Department.xresearch...development   as it has high p values
model_4<-glm(formula = Attrition ~ EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4  +  
               JobLevel.x2  + NumCompaniesWorked.x1 + 
               NumCompaniesWorked.x5 + NumCompaniesWorked.x6 + 
               NumCompaniesWorked.x7  + NumCompaniesWorked.x9 + 
               TrainingTimesLastYear.x6 + 
               JobRole.xlaboratory.technician + JobRole.xresearch.director + 
               JobRole.xresearch.scientist + JobRole.xsales.executive + MaritalStatus.xsingle + Age + average_time + 
               YearsSinceLastPromotion + 
               YearsWithCurrManager, family = "binomial", data = train)
summary(model_4)
model4_table<-data.frame(vif(model_4))
#AIC did not change much so it is acceptable.

###################################################################################################

#removing JobLevel.x2   as it has high p values
model_4<-glm(formula = Attrition ~ EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4  +   NumCompaniesWorked.x1 + 
               NumCompaniesWorked.x5 + NumCompaniesWorked.x6 + 
               NumCompaniesWorked.x7  + NumCompaniesWorked.x9 + 
               TrainingTimesLastYear.x6 + 
               JobRole.xlaboratory.technician + JobRole.xresearch.director + 
               JobRole.xresearch.scientist + JobRole.xsales.executive + MaritalStatus.xsingle + Age + average_time + 
               YearsSinceLastPromotion + 
               YearsWithCurrManager, family = "binomial", data = train)
summary(model_4)
model4_table<-data.frame(vif(model_4))
#AIC did not change much so it is acceptable.

###################################################################################################

#removing JobRole.xlaboratory.technician   as it has high p values
model_4<-glm(formula = Attrition ~ EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4  +   NumCompaniesWorked.x1 + 
               NumCompaniesWorked.x5 + NumCompaniesWorked.x6 + 
               NumCompaniesWorked.x7  + NumCompaniesWorked.x9 + 
               TrainingTimesLastYear.x6  + JobRole.xresearch.director + 
               JobRole.xresearch.scientist + JobRole.xsales.executive + MaritalStatus.xsingle + Age + average_time + 
               YearsSinceLastPromotion + 
               YearsWithCurrManager, family = "binomial", data = train)
summary(model_4)
model4_table<-data.frame(vif(model_4))
#AIC did not change much so it is acceptable.


###################################################################################################

#removing JobRole.xresearch.scientist   as it has high p values
model_4<-glm(formula = Attrition ~ EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4  +   NumCompaniesWorked.x1 + 
               NumCompaniesWorked.x5 + NumCompaniesWorked.x6 + 
               NumCompaniesWorked.x7  + NumCompaniesWorked.x9 + 
               TrainingTimesLastYear.x6  + JobRole.xresearch.director + JobRole.xsales.executive + MaritalStatus.xsingle + Age + average_time + 
               YearsSinceLastPromotion + 
               YearsWithCurrManager, family = "binomial", data = train)
summary(model_4)
model4_table<-data.frame(vif(model_4))
#AIC did not change much so it is acceptable.



###################################################################################################

#removing JobRole.xresearch.director  as it has high p values
model_4<-glm(formula = Attrition ~ EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4  +   NumCompaniesWorked.x1 + 
               NumCompaniesWorked.x5 + NumCompaniesWorked.x6 + 
               NumCompaniesWorked.x7  + NumCompaniesWorked.x9 + 
               TrainingTimesLastYear.x6   + JobRole.xsales.executive + MaritalStatus.xsingle + Age + average_time + 
               YearsSinceLastPromotion + 
               YearsWithCurrManager, family = "binomial", data = train)
summary(model_4)
model4_table<-data.frame(vif(model_4))
#AIC did not change much so it is acceptable.



###################################################################################################

#removing NumCompaniesWorked.x1  as it has high p values
model_4<-glm(formula = Attrition ~ EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4  + 
               NumCompaniesWorked.x5 + NumCompaniesWorked.x6 + 
               NumCompaniesWorked.x7  + NumCompaniesWorked.x9 + 
               TrainingTimesLastYear.x6   + JobRole.xsales.executive + MaritalStatus.xsingle + Age + average_time + 
               YearsSinceLastPromotion + 
               YearsWithCurrManager, family = "binomial", data = train)
summary(model_4)
model4_table<-data.frame(vif(model_4))
#AIC did not change much so it is acceptable.


###################################################################################################

#removing JobRole.xsales.executive  as it has high p values
model_4<-glm(formula = Attrition ~ EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4  + 
               NumCompaniesWorked.x5 + NumCompaniesWorked.x6 + 
               NumCompaniesWorked.x7  + NumCompaniesWorked.x9 + 
               TrainingTimesLastYear.x6  + MaritalStatus.xsingle + Age + average_time + 
               YearsSinceLastPromotion + 
               YearsWithCurrManager, family = "binomial", data = train)
summary(model_4)
model4_table<-data.frame(vif(model_4))
#AIC did not change much so it is acceptable.



###################################################################################################

#removing NumCompaniesWorked.x6  as it has high p values
model_4<-glm(formula = Attrition ~ EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4  + 
               NumCompaniesWorked.x5  + 
               NumCompaniesWorked.x7  + NumCompaniesWorked.x9 + 
               TrainingTimesLastYear.x6  + MaritalStatus.xsingle + Age + average_time + 
               YearsSinceLastPromotion + 
               YearsWithCurrManager, family = "binomial", data = train)
summary(model_4)
model4_table<-data.frame(vif(model_4))
#AIC did not change much so it is acceptable.



###################################################################################################

#removing JobSatisfaction.x3  as it has high p values
model_4<-glm(formula = Attrition ~ EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2  + 
               JobSatisfaction.x4  + 
               NumCompaniesWorked.x5  + 
               NumCompaniesWorked.x7  + NumCompaniesWorked.x9 + 
               TrainingTimesLastYear.x6  + MaritalStatus.xsingle + Age + average_time + 
               YearsSinceLastPromotion + 
               YearsWithCurrManager, family = "binomial", data = train)
summary(model_4)
model4_table<-data.frame(vif(model_4))
#AIC did not change much so it is acceptable.



###################################################################################################

#removing JobSatisfaction.x2  as it has high p values
model_4<-glm(formula = Attrition ~ EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 +   + 
               JobSatisfaction.x4  + 
               NumCompaniesWorked.x5  + 
               NumCompaniesWorked.x7  + NumCompaniesWorked.x9 + 
               TrainingTimesLastYear.x6  + MaritalStatus.xsingle + Age + average_time + 
               YearsSinceLastPromotion + 
               YearsWithCurrManager, family = "binomial", data = train)
summary(model_4)
model4_table<-data.frame(vif(model_4))
#AIC did not change much so it is acceptable.



###################################################################################################

#removing JobSatisfaction.x2  as it has high p values
model_4<-glm(formula = Attrition ~ EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 +   + 
               JobSatisfaction.x4  + 
               NumCompaniesWorked.x5  + 
               NumCompaniesWorked.x7  + NumCompaniesWorked.x9 + 
               TrainingTimesLastYear.x6  + MaritalStatus.xsingle + Age + average_time + 
               YearsSinceLastPromotion + 
               YearsWithCurrManager, family = "binomial", data = train)
summary(model_4)
model4_table<-data.frame(vif(model_4))
#AIC did not change much so it is acceptable.

##############################################################################################
#now all variables are significant in the model

###################################################################################################

#removing TrainingTimesLastYear.x6  as it has higher p value compared to others
model_4<-glm(formula = Attrition ~ EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 +   + 
               JobSatisfaction.x4  + 
               NumCompaniesWorked.x5  + 
               NumCompaniesWorked.x7  + NumCompaniesWorked.x9 + 
               MaritalStatus.xsingle + Age + average_time + 
               YearsSinceLastPromotion + 
               YearsWithCurrManager, family = "binomial", data = train)
summary(model_4)
model4_table<-data.frame(vif(model_4))
#AIC did not change much so it is acceptable.

###################################################################################################

#removing NumCompaniesWorked.x9  as it has higher p value compared to others
model_4<-glm(formula = Attrition ~ EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 +   + 
               JobSatisfaction.x4  + 
               NumCompaniesWorked.x5  + 
               NumCompaniesWorked.x7   + 
               MaritalStatus.xsingle + Age + average_time + 
               YearsSinceLastPromotion + 
               YearsWithCurrManager, family = "binomial", data = train)
summary(model_4)
model4_table<-data.frame(vif(model_4))
#AIC did not change much so it is acceptable.

###################################################################################################

#removing NumCompaniesWorked.x7  as it has higher p value compared to others
model_4<-glm(formula = Attrition ~ EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 +   + 
               JobSatisfaction.x4  + 
               NumCompaniesWorked.x5  + 
               MaritalStatus.xsingle + Age + average_time + 
               YearsSinceLastPromotion + 
               YearsWithCurrManager, family = "binomial", data = train)
summary(model_4)
model4_table<-data.frame(vif(model_4))
#AIC did not change much so it is acceptable.


###################################################################################################

#removing NumCompaniesWorked.x5  as it has higher p value compared to others
model_4<-glm(formula = Attrition ~ EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 +   + 
               JobSatisfaction.x4  + 
               MaritalStatus.xsingle + Age + average_time + 
               YearsSinceLastPromotion + 
               YearsWithCurrManager, family = "binomial", data = train)
summary(model_4)
model4_table<-data.frame(vif(model_4))
#AIC did not change much so it is acceptable.


###################################################################################################

#removing NumCompaniesWorked.x5  as it has higher p value compared to others
model_4<-glm(formula = Attrition ~ EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 +   + 
               JobSatisfaction.x4  + 
               MaritalStatus.xsingle + Age + average_time + 
               YearsSinceLastPromotion + 
               YearsWithCurrManager, family = "binomial", data = train)
summary(model_4)
model4_table<-data.frame(vif(model_4))
#AIC did not change much so it is acceptable.

###################################################################################################


###################################################################################################

#removing EnvironmentSatisfaction.x2
model_4<-glm(formula = Attrition ~  + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 +   + 
               JobSatisfaction.x4  + 
               MaritalStatus.xsingle + Age + average_time + 
               YearsSinceLastPromotion + 
               YearsWithCurrManager, family = "binomial", data = train)
summary(model_4)
model4_table<-data.frame(vif(model_4))
#AIC did not change much so it is acceptable.

###################################################################################################

#removing EnvironmentSatisfaction.x3
model_4<-glm(formula = Attrition ~  
               EnvironmentSatisfaction.x4 +   + 
               JobSatisfaction.x4  + 
               MaritalStatus.xsingle + Age + average_time + 
               YearsSinceLastPromotion + 
               YearsWithCurrManager, family = "binomial", data = train)
summary(model_4)
model4_table<-data.frame(vif(model_4))
#AIC did not change much so it is acceptable.

final_model<- model_4

#######################################################################

### Model Evaluation

### Test Data ####

#predicted probabilities of attrition for test data

test_pred = predict(final_model, type = "response", 
                    newdata = test[,-40])

summary(test_pred)

pred<-prediction(test_pred,test$Attrition)
eva<-performance(pred,"sens","spec")
evaA<-performance(pred,'acc')

plot(evaA)
sensitivity <- eva@y.values[[1]]
cutoff <- eva@alpha.values[[1]]
specificity<- eva@x.values[[1]]
accuracy<-evaA@y.values[[1]]
plot(cutoff,sensitivity,col="red")
lines(cutoff,specificity,col="green")
lines(cutoff,accuracy,col="blue")
abline(v=0.16)
legend("bottomright", legend=c("sensitivity","accuracy","specificity"),
       col=c("red","blue","green"), lty=1:2, cex=0.8)
#making a matrix with sensitivity,specificity,accuracy and cutoff values
matrix<-data.frame(cbind(sensitivity,specificity,accuracy,cutoff))
matrix$cutoff[ which(abs(matrix$sensitivity-matrix$specificity)<0.00045)]
#0.1598 which is almost equal to 0.16
#lets use 0.16 as the cutoff as its the point close to where accuracy, sensitivity  and specificity curves  meets

matrix[ which(abs(matrix$sensitivity-matrix$specificity)<0.00045),]
# sensitivity specificity  accuracy    cutoff
#   0.6995305   0.6990991 0.6991686 0.15987

#Lets find cofusion matrix for this model
test_cutoff_attrition <- factor(ifelse(test_pred >=0.16, "Yes", "No"))
test_actual_attrition <- factor(ifelse(test$Attrition==1, "Yes", "No"))
conf_final <- confusionMatrix(test_cutoff_attrition , test_actual_attrition, positive = "Yes")
conf_final
#          Reference
#Prediction  No Yes
#        No  777  64
#       Yes  333 149

#lets find ks statistic for this model
perf<-performance(pred,'tpr','fpr')
ks<-max(perf@y.values[[1]]-perf@x.values[[1]])
# ks statistic for this model is 0.42
#gain and lift chart
gain<-performance(pred,'tpr','rpp')
deciles<-performance(pred,'rpp')
#ks chart
k_stat_matrix<-data.frame(10*(deciles@y.values[[1]]),10*(perf@y.values[[1]]-perf@x.values[[1]]))
colnames(k_stat_matrix)[1]<-"deciles"
colnames(k_stat_matrix)[2]<-"k_statistic"
k_stat_matrix$k_statistic<-round(k_stat_matrix$k_statistic,2)
k_stat_matrix[which(k_stat_matrix$k_statistic==4.2),]
plot(k_stat_matrix)
abline(h=4.2,v=3.4)
#ks statistic lies withinin first 4 deciles 
plot(gain)
plot(perf)
plotLift(test_pred,test$Attrition)
plot(roc(test_pred,factor(test$Attrition)))
auc(roc(test_pred,factor(test$Attrition)))
# area under ROC CURVE is 0.7693186
###################################################################################################
#BOOTSTRAPPING
#FIRST ITERATION

set.seed(10)

indices = sample.split(final$Attrition, SplitRatio = 0.7)

train = final[indices,]

test = final[!(indices),]

model_4<-glm(formula = Attrition ~  
               EnvironmentSatisfaction.x4 +   + 
               JobSatisfaction.x4  + 
               MaritalStatus.xsingle + Age + average_time + 
               YearsSinceLastPromotion + 
               YearsWithCurrManager, family = "binomial", data = train)
summary(model_4)
model4_table<-data.frame(vif(model_4))
#AIC did not change much so it is acceptable.

final_model<- model_4

#######################################################################

### Model Evaluation

### Test Data ####

#predicted probabilities of attrition 1 for test data

test_pred = predict(final_model, type = "response", 
                    newdata = test[,-40])


# Let's see the summary 

summary(test_pred)

test$prob <- test_pred
View(test)

pred<-prediction(test_pred,test$Attrition)
eva<-performance(pred,"sens","spec")
evaA<-performance(pred,'acc')

plot(evaA)
sensitivity <- eva@y.values[[1]]
cutoff <- eva@alpha.values[[1]]
specificity<- eva@x.values[[1]]
accuracy<-evaA@y.values[[1]]
plot(cutoff,sensitivity,col="red")
lines(cutoff,specificity,col="green")
lines(cutoff,accuracy,col="blue")
abline(v =0.16)
legend("bottomright", legend=c("sensitivity","accuracy","specificity"),
       col=c("red","blue","green"), lty=1:2, cex=0.8)
matrix<-data.frame(cbind(sensitivity,specificity,accuracy,cutoff))
matrix$cutoff[which(matrix$accuracy==matrix$specificity)]
matrix[ which(abs(matrix$sensitivity-matrix$specificity)<0.00045),]
#sensitivity specificity accuracy    cutoff
#   0.7183099    0.718018 0.718065 0.1767336
perf<-performance(pred,'tpr','fpr')
ks<-max(perf@y.values[[1]]-perf@x.values[[1]])
# 0.45
#####################################################################################################
#BOOTSTRAPPING
#SECOND ITERATION

set.seed(20)

indices = sample.split(final$Attrition, SplitRatio = 0.7)

train = final[indices,]

test = final[!(indices),]

model_4<-glm(formula = Attrition ~  
               EnvironmentSatisfaction.x4 +   + 
               JobSatisfaction.x4  + 
               MaritalStatus.xsingle + Age + average_time + 
               YearsSinceLastPromotion + 
               YearsWithCurrManager, family = "binomial", data = train)
summary(model_4)
model4_table<-data.frame(vif(model_4))
#AIC did not change much so it is acceptable.

final_model<- model_4

#######################################################################

### Model Evaluation

### Test Data ####

#predicted probabilities of attrition 1 for test data

test_pred = predict(final_model, type = "response", 
                    newdata = test[,-40])


# Let's see the summary 

summary(test_pred)

test$prob <- test_pred
View(test)
#calculating sensitivity,specificity,accuracy for different cutoff values for probability of attrition
pred<-prediction(test_pred,test$Attrition)
eva<-performance(pred,"sens","spec")
evaA<-performance(pred,'acc')

sensitivity <- eva@y.values[[1]]
cutoff <- eva@alpha.values[[1]]
specificity<- eva@x.values[[1]]
accuracy<-evaA@y.values[[1]]
#making a plot for sensitivity,specificity,accuracy
plot(cutoff,sensitivity,col="red",ylab="sensitivity,accuracy,specificity")
lines(cutoff,specificity,col="green")
lines(cutoff,accuracy,col="blue")
abline(v =0.16)
legend("bottomright", legend=c("sensitivity","accuracy","specificity"),
       col=c("red","blue","green"), lty=1:2, cex=0.8)

#making a matrix with sensitivity,specificity,accuracy and cutoff values
matrix<-data.frame(cbind(sensitivity,specificity,accuracy,cutoff))
matrix$cutoff[which(matrix$accuracy==matrix$specificity)]
matrix[ which(abs(matrix$sensitivity-matrix$specificity)<0.00045),]
#sensitivity specificity  accuracy    cutoff
# 0.685446   0.6855856 0.6855631 0.1624628
perf<-performance(pred,'tpr','fpr')
ks<-max(perf@y.values[[1]]-perf@x.values[[1]])
# 0.41