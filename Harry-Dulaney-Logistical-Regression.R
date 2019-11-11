############################  FI 4090 #####################################################################
########################## HW ASSIGNMENT 3 #################################################
######################### CREATED BY: HARRY DULANEY #######################################################

###########################################################################################
################################ PART 1 ################################################################
#########################################################################################

			###Analyze the data in the CreditCard dataset in AER package.


install.packages("AER")	#Install and load the AER packages
library(AER)		

			####Get the data "CreditCard" and check it is actually attatched

data("CreditCard")
attach(CreditCard)
names(CreditCard)
head(CreditCard)
dim(CreditCard)	#check number of rows and columns


			#Use variables reports,age,income,share,owner,dependents,months,to 
			#determine which of the predictors influence
			#the probability that an application is accepted ie (card). 

			########	A. Provide Sum Stat of the predictors 

library("fBasics")		#Load fBasics statistics library



numrows = nrow(CreditCard)		#Create reference to number of rows in the dataset


card_dummy <- matrix(nrow = numrows,ncol=1)		#Instantiate the dummy variables
owner_dummy <- matrix(nrow = numrows,ncol=1)			

				#Set yes=1 and no=0 for 'card' and 'owner'

for(i in 1:numrows) {

	if(card[i] == 'yes'){
	card_dummy[i,] = 1
	}else{
	card_dummy[i,] = 0
	}

	if(owner[i] == 'yes'){
	owner_dummy[i,] = 1
	}else{
	owner_dummy[i,] = 0
	}
	
}
colnames(card_dummy)="card_dummy"
colnames(owner_dummy)="owner_dummy"


			
					#Create an object with the desired variables

CCData <- subset(CreditCard,select=c(reports,age,income,share,dependents,months))
CCData
					#attatch the dummy variables in place of card and owner

CCData <- cbind(CCData,card_dummy)
CCData <- cbind(CCData,owner_dummy)

head(CCData)			#Check
tail(CCData)	




			########	B. There are some values of variable age under one year. 
			########	Consider data with age > 18 for your analysis for the rest of the questions.


			######	Remove data row if age is not greater than 18 i.e.(age > 18)

new_CCData <- subset(CCData, age>18)


head(new_CCData) 			#Check operation was successful
tail(new_CCData)
dim(new_CCData)

new_CCData$income = new_CCData$income * 10000


write.csv(new_CCData,"C:/RData/new_CCData.R")

basicStats(new_CCData) 		#Finanly Get the Summary Stat
cor(new_CCData)				#And the Correlation Matrix



		######	C. Plot of income vs. reports (Number of major derogatory reports): 
		######	mark individuals with card application accepted as blue, and not
		###### 	accepted as red.  

par(mfcol=c(1,1)) 	
plot(new_CCData$reports,new_CCData$income,
xlab='Derogatory Reports',
ylab='Income Level', 
main='Income vs Reports',
col=ifelse(new_CCData$card_dummy==0, 'red', 'blue'))


		##	D. Boxplots of income as a function of card acceptance status. 
		##	Boxplots of reports as a function of card acceptance status 
		##	(mark card application accepted as blue, and not accepted as red). 
		##	(Display two boxplots in same page). 


par(mfcol=c(1,2))
boxplot(income~card_dummy,data=new_CCData, xlab='card', ylab='income', col=(c("blue","red"))) 

par(mfcol=c(1,2))
boxplot(reports~card_dummy,data=new_CCData, xlab='card', ylab='reports', col=(c("blue","red"))) 
reports


				######	E.	Construct the histogram for the predictors.  	(5 points)
				#	Note that share is highly right-skewed, so log(share) will be used in the analysis. 
				#	reports is also extremely right skewed (most values of reports are 0 or 1, 
				#	but the maximum value is 14. To reduce the skewness, log(reports+1) will be used for your analysis. 
				#	Highly skewed predictors have high leverage points and are less likely to be linearly related to the response.
				#	variables --->>> reports,age,income,share,dependents,months,card,owner

tempShare <- cbind(log(new_CCData$share)) 			#convert to log(share), store in temp object
tempReports <- cbind(log(new_CCData$reports +1)) 		#convert to log(reports+1), store in temp object


hist(new_CCData$income)

hist(new_CCData$age)

hist(tempReports)

hist(tempShare)

hist(new_CCData$dependents)

hist(new_CCData$months)

hist(new_CCData$card_dummy)

hist(new_CCData$owner)

					# F.	Use variables 2 to 8 to determine which of the predictors influence 
					#the probability that an application is accepted. Use the summary function 
					#to print the results. (10 points)
					# reports,age,income,share,dependents,months,owner


							#Use Logistical Regression
							# to test the effect of
							# the variables on 
							# card application accepted 

fitAll=glm(card~reports+age+income+share+dependents+months+owner, family="binomial", data=CreditCard)			
summary(fitAll)




	#####	G. To predict whether the application will be accepted or not, 
	####	convert the predicted probabilities into class labels yes or no with the following 
	####	condition: probs >.5="yes". Compute the confusion matrix and overall fraction of 
	####	correct predictions.  (30 points)



pred_prob=predict(fitAll,type="response") 	#Create prediction of card application acceptance on scale of 0 to 1
head(pred_prob)						#Check values were created
length(pred_prob)						#Confirm correct number of predictions for available data



glm.pred=rep("no",1319)					#Vector where all values are No

glm.pred[pred_prob>.5]="yes"				#Where predicted probablitiy is greater than 0.5 change to Yes

head(glm.pred)						#Check
tail(glm.pred)						#Check
head(CreditCard)						#Compare prediction to actuals

table(glm.pred,card)					#Display the confusion matrix

new_card <- as.character(card)	#Convert card to a character class to match the glm.pred variables
class(new_card)		#Confirm card variables are characters

mean(glm.pred==new_card)					#Determine the average number of correct predictions






								#####  H. Now fit the logistic regression model using a training data for 
								#####	 observations 1 to 1000. Compute the confusion matrix and the 
								#####	 overall fraction of correct predictions for the test data (that is,
								#####	 the data for observations 1001 to end of data
		

			####If needed re-import the data and the library
library(AER)

data("CreditCard")
attach(CreditCard)
names(CreditCard)
head(CreditCard)



tempDf = c(1:1319)

train=(tempDf<1001)

CC.test=CreditCard[!train,]

card.test = card[!train]

correct_card <- as.character(card.test)		#Instantiate new variable to match 'glm.pred' data type

				#Set yes=1 and no=0 for 'card' and 'owner'


glm.fit=glm(card~reports+age+income+share+dependents+months+owner,data=CreditCard,family=binomial,subset=train)	

glm.probs=predict(glm.fit,CC.test,type="response")			#Create prediction of card application acceptance on scale of 0 to 1

glm.pred=rep("no",nrow(CC.test))

glm.pred[glm.probs>.5]="yes"

table(glm.pred,card.test)		#Generate the confusion matrix

new_glm.pred <- glm.pred
			
mean(glm.pred==correct_card) 

