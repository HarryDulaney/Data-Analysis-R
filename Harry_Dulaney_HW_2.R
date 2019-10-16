###########################  FI 4090 #####################################################################
######################### HW ASSIGNMENT 2 #################################################
######################## CREATED BY: HARRY DULANEY #######################################################

###########################################################################################
################################ PART 1 ################################################################
#########################################################################################

	#HousePrices data set is a cross-sectional data set on house prices and other 
	#features, e.g., number of bedroom, of houses in Windsor, Ontario. The data were 
	#gathered during the summer of 1987


setwd("C:/RData")	#Set and confirm the working directory
getwd()

	###i.CONSTRUCT A SUMMARY STAT FOR ALL VARIABLES IN HOUSEPRICES DATA

	#Create a refrence to the houseprices.csv file and inspect it

houPr = read.csv("HousePrices.csv",header=TRUE)
fix(houPr)


	#Create a summary stat with all variables

library(fBasics)

names(houPr)
attach(houPr)

housePricesSS=cbind(price, lotsize, bedrooms, bathrooms, stories, driveway,
		recreation, fullbase, gasheat, aircon, garage, prefer)

mHP=apply(housePricesSS,2,mean) 		#mean
varHP=apply(housePricesSS,2,var)		#variance
stdHP=apply(housePricesSS,2,stdev) 		#standard deviation
summaryHP=apply(housePricesSS,2,summary)	#summary
cor1HP=cor(housePricesSS) 			#correlation matrix 

	#View Summary

cor1HP	 #Correlation Matrix
mHP 		 #means
stdHP		 #standard deviations
summaryHP	 #Statistical summary



	###ii. Find the percentage of the houses with a driveway,
	#gasheat, and airconditioning

	#Attach data to R path in order to 
	#easily access attributes e.g.(price,garage,prefer..)

attach(houPr)

	#Create dummy variables for each attribute

	#Check size of original data

dim(houPr)

	#Create reference to number of rows in the dataset

numrows = nrow(houPr)

	#Instantiate the dummy variables and set to 
	#where yes=1 and no=0

driveway_dummy <- matrix(nrow = numrows,ncol=1)
gasheat_dummy <- matrix(nrow = numrows,ncol=1)
ac_dummy <- matrix(nrow = numrows,ncol=1)

for(i in 1:numrows) {

	if(driveway[i] == 'yes'){
	driveway_dummy[i,] = 1
	}else{
	driveway_dummy[i,] = 0
	}

	if(gasheat[i] == 'yes'){
	gasheat_dummy[i,] = 1
	}else{
	gasheat_dummy[i,] = 0
	}
	
	if(aircon[i] == 'yes'){
	ac_dummy[i,] = 1
	}else{
	ac_dummy[i,] = 0
	}

}

	#Rename dummy columns

colnames(driveway_dummy)= "driveway_dummy"
colnames(gasheat_dummy)="gasheat_dummy"
colnames(ac_dummy)="aircon_dummy"

	#Confirm create dummy variables 

head(driveway_dummy)
head(driveway)

head(gasheat_dummy)
head(gasheat)

head(ac_dummy)
head(aircon)

	#Use the mean function and multiply by 100
	#to find the percentage houses with driveways, gasheat, and ac

percentWithDriveways <- mean(driveway_dummy)*100
percentWithDriveways

percentWithGasheat <- mean(gasheat_dummy)*100
percentWithGasheat

percentWithAc <- mean(ac_dummy)*100
percentWithAc


	#iii.CONSTRUCT A LINEAR REGRESSION MODEL TO TEST HOW THE PRICE IS AFFECTED BY 
	#NUMBER OF BEDROOMS


houPr2 = subset(houPr,select = c(bedrooms,price))
head(houPr2)
dim(houPr2)

	#Linear regression model and summary

lm.fit=lm(price~bedrooms,data=houPr2)
summary(lm.fit)

	#Multiple linear regression model. Includes all variales from houseprices.csv
	#Observe how do they affect price... Followed by summary

lm.fit2=lm(price~lotsize+bedrooms+bathrooms+stories+driveway+recreation
		+fullbase+gasheat+aircon+garage+prefer,data=houPr)
summary(lm.fit2)

###########################################################################################
################################ PART 2################################################################
#########################################################################################

	##Part A.
	###Use the Credit data to perform the following tests using 
	###Linear Regression settings:

	#Check working directory

getwd()

	#Import and check credit.csv

credit = read.csv("Credit.csv",header=TRUE)
head(credit)
fix(credit)

	#Remove the X column and confirm

credit_clean = subset(credit, select = -c(X))
head(credit_clean)

	#i.ATTACH CREDIT DATA TO THE R ENVIRONMENT. 

attach(credit_clean)

	#ii.OBSERVE THE NUMBER OF ROWS IN THE CREDIT DATA. OBSERVE THE DIMENSION OF THE CREDIT DATA

numrows = nrow(credit_clean)
numrows

dim(credit_clean)

	#iii.PROVIDE A SUMMARYT STAT FOR THE VARIABLES IN CREDIT DATA.

summary(credit_clean)	#Full summary stat at END of exercise 

	#iv.WHAT IS THE PERCENTAGE OF STUDENT AND FEMALE IN CREDIT DATA.

	#Confirm rows is acurate

numrows = nrow(credit_clean)
numrows

	#Instantiate dummy variables
Student_dummy <- matrix(nrow = numrows,ncol=1)
Gender_dummy <- matrix(nrow = numrows,ncol=1)

	#Give binary values to dummy variables 

for(i in 1:numrows) {

	if(Student[i] == 'Yes'){
	Student_dummy[i,] = 1
	}else{
	Student_dummy[i,] = 0
	}

	if(Gender[i] == 'Female'){
	Gender_dummy[i,] = 1
	}else{
	Gender_dummy[i,] = 0
	}
}
	#Check
head(Gender_dummy)
head(Gender)
head(Student_dummy)
head(Student)

	#Rename dummy columns
colnames(Gender_dummy)= "gender_dummy"
colnames(Student_dummy)="student_dummy"

	# Use Mean Function times 100 to find percentage
	# of Students in the credit data and percentage of females 
	# in the credit data

percentStudent <- mean(Student_dummy)* 100
percentStudent
percentFemale <- mean(Gender_dummy) * 100
percentFemale

	#SUMMARY STAT FOR CREDIT DATA (All Variables)

library(fBasics)
names(credit_clean)
attach(credit_clean)

sum_credit=cbind(Income, Limit, Rating, Cards,
		 Age, Education,Gender_dummy, Student_dummy,Balance,Ethnicity)


mCredit=apply(sum_credit,2,mean) 				#mean
varCredit=apply(sum_credit,2,var) 				#variance
stdCredit=apply(sum_credit,2,stdev) 			#standard deviation
summaryCredit=apply(sum_credit,2,summary)	#summary
cor1Credit=cor(sum_credit) 					#correlation matrix 

	#View the summary of stats

cor1Credit
mCredit
stdCredit
summaryCredit

	#Plot some relationships
plot(Income,Balance)
plot(Age, Income)
plot(Age, Balance)
plot(Rating, Limit)



	####Part B.
	###Construct a linear regression model as follows: 
	###Response variable: Credit Card Balance 
	###dictors: Credit Rating, Student, Credit Rating * Student (interaction terms)   
	###Provide a summary of the model using summary() function. 

names(credit_clean) #Check header names
lm.fit1 = lm(Balance~Rating+Student_dummy+(Rating*Student_dummy))

summary(lm.fit1)

lm.fit4 = lm(Balance~Student)	#Compare student status to CC Balance
summary(lm.fit4)

 

	#Create statistics

sum_rateStudent = cbind(Balance,Student_dummy,Rating)
mean12 = apply(sum_rateStudent,2,mean)
var12 = apply(sum_rateStudent,2,var)
stdev12 = apply(sum_rateStudent,2,stdev)
summary12 = apply(sum_rateStudent,2,summary)
cor12 = cor(sum_rateStudent)

	#View statistics

mean12
var12
stdev12
summary12
cor12


###########################################################################################
################################ PART 3###############################################################
#########################################################################################

	##i.	Test whether Age influence Credit Card Balance on the basis of 
	##simple linear regression.Provide a summary of the model using the summary()function. 


summary(Age) 	#Get the simple stat summmary on Age variable

lm.fit9 = lm(Balance~Age) #Simple linear regression for Balance vs Age
summary(lm.fit9)

	##ii.	Use Age and Credit Rating as predictors of Credit Card Balance 
	##(response variable) in a multiple linear regression setting. 
	##(Provide a summary of the model using summary() function). 
	##Interpret the effects of both the predictors.  


lm.fit7=lm(Balance~Age+Rating,data=credit_clean) 	#Multi-linear regression model
summary(lm.fit7) 							#Summary of effect of Age and Rating on Balance



				#Answer ii.
#The effect of Age on credit card balance is negative and significant(p-valuev < 0.001)
#A 1% increase in Age will result in a 2.35% decrease in card balance
#The effect of Rating on credit card balance is positive and significant(p-value < 0.001)
#A 1% increase in Rating will result in a 2.59% increase in card balance




##iii.Compare effect of Age from part (i) and (ii). 	

#In part i, the simple linear regression function returns a relationship
#between Age and Balance with a p-value > 0.1 and almost so 
#based on this the model the coefficient would NOT be significant.
#However, when we run a multiple linear regression model and include the Rating
#variable with Age, we now have a coefficient that has a p-value < 0.001 
#making it definitly significant.
#This means that Age on its own does not provide enough information to 
#establish a significant effect on Balance, but when we include 
#the influence of the Credit Rating variable, we can establish a significant
#effect on balance by both Age and Credit Rating.