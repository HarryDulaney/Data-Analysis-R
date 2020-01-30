############################  FI 4090 ################################################################################
########################## HW ASSIGNMENT 4 ###########################################################################
################ LASSO, Tree Regression, and Cross Validation ########################################################
######################### CREATED BY: HARRY DULANEY ##################################################################

#######################################################################################################################
################################ PART 1: LASSO ########################################################################
###############################################################################################################
### Predict the number of applications received Apps
### using all other variables in the College data set 
### using LASSO model for variable selection:  

setwd("C:/RData")
getwd()
		 #INSTALL ISLR PACKAGE AND INITIALIZE

install.packages("ISLR")
library(ISLR)


		#EXAMINE, ATTACH AND CHECK SIZE OF College DATA SET

fix(College) 
attach(College)  	
dim(College)
names(College)	

################ a. Split the data set randomly into training and test data set. ####################################

		### CREATE A MATRIX FOR PREDICTORS, REMOVE Apps, 
		### AND CREATE DUMMY VARIABLES FOR QUALITATIVE OBSERVATIONS

p=model.matrix(Apps~.,College)[,-1] 	#CREATE MATRIX FOR PREDICTOR VARIABLES NAMES P

class(p) 						#CONFIRM MATRIX CREATED

r=College$Apps   					#CREATE VECTOR FOR RESPONSE VARIABLE NAMED R

set.seed(1)  					#SET THE SEED FOR R'S RANDOM NUMBER GENERATOR

train=sample(1:nrow(p), nrow(p)/2)		#RANDOMLY SELECT SAMPLE FOR TRAINING DATA SET

train							#CHECK TRAINING SAMPLE

test=(-train)					#SET THE TEST 

r.test=r[test]					#DEFINE R.TEST

 
################### b. Fit Lasso model using glmnet() function on the training data set.#######################  

install.packages("glmnet") 						#LOAD THE glmnet PACKAGE FOR LASSO
library(glmnet)

grid=10^seq(10,-2,length=100)						#CREATE GRID

lasso.mod=glmnet(p[train,],r[train],alpha=1,lambda=grid)	#FIT LASSO MODEL USING glmnet


################### c. Perform cross-validation on the training data set to choose the best lambda.#################################

set.seed(1)							#SET THE SEED R'S RANDOM NUMBER GENERATOR

cv.out=cv.glmnet(p[train,],r[train],alpha=1)	#PERFORM CROSS VALIDATION

plot(cv.out)						#CREATE VISUAL AID

bestLambda=cv.out$lambda.min				#FIND BEST LAMBDA

bestLambda							#VIEW BEST LAMBDA (bestLambda = 24.62086)


################## d. Estimate the predicted values using the best lambda obtained in part (c) on ###############################
##################	 the test data (using the predict() function) and compute test MSE. ####################################


# Estimate the predicted values by using predict() function on the test set using the bestlam 

lasso.pred=predict(lasso.mod,s=bestLambda,newx=p[test,])  #ESTIMATE PREDICTED VALUES USING BEST LAMBDA

head(lasso.pred)					#CHECK 
tail(lasso.pred)					#CHECK 
mean((lasso.pred-r.test)^2)			#COMPUTER THE TEST MSE


	
####################### e. Compare the Lasso predicted test MSE with the null model (lambda=infinity)##################### 
	
######################		test MSE and least square regression model (lambda=0) test MSE.      #####################

nullModelMSE=mean((mean(r[train])-r.test)^2)   			#CALCULATE THE TEST MSE OF THE NULL MODEL
nullModelMSE

lasso.pred=predict(lasso.mod,s=0,newx=p[test,])			#LEAST SQUARE REGRESSION (LAMBDA=0) TEST MSE
lsrTestMSE=mean((lasso.pred-r.test)^2)					
lsrTestMSE									

######################	 f. Now construct the Lasso model for the entire data set and obtain the Lasso ######################	
########################   coefficients using the best lambda obtained in part (c) and report the number ######################	
##########################################	of non-zero coefficient estimates. 	###############################################################			


allData=glmnet(p,r,alpha=1,lambda=grid)    	#LASSO MODEL FOR ENTIRE DATA SET
plot(allData)						#OBSERVE AND CHECK


					
lasso.coef=predict(allData,type="coefficients",s=bestLambda)[1:18,]  #PREDICIT COEFFICIENTS AT BEST LAMBDA

lasso.coef								#DISPLAY PREDICTED COEFFICIENTS 
nonZeroCOEst = lasso.coef[lasso.coef!=0] 		 	#FIND AND REPORT NUMBER OF NON-ZERO COEFFICIENT ESTIMATES
nonZeroCOEst



############## g. Now use the Lasso predictors obtained in part (f) to fit the Linear Regression ###################################
############################	Model and report the summary of the linear model. ##################################################

attach(College)				#ATTACH COLLEGE IF NOT ALREADY ATTATCHED
names(College)				#VISUAL GUIDE FOR EXACT COLUMN HEADINGS

						#FIT THE LINEAR REGRESSION MODEL WITH LASSO PREDICTORS 
lm.fit.lasso=lm(r~Private+Accept+Enroll+Top10perc+Top25perc+F.Undergrad+P.Undergrad+Outstate+Room.Board+Books+Personal+PhD+Terminal+S.F.Ratio+perc.alumni+Expend+Grad.Rate)

summary(lm.fit.lasso)			#REPORT THE SUMMARY


#######################################################################################################################
################################ PART 2: REGRESSION TREE ####################################################################
#######################################################################################################################
### Predict the number of applications received 
### Apps using all other variables in the College 
### data set based on a Regression Tree:  

############### 	Perform the following tasks: Use the training and test #############################################################################
################		 data set that you created in Part 1(a). 		###################################################################################
		######################################################################
		### a.	Fit a Regression Tree to the training data, with Apps  ###
		### as the response and the all other variables as predictors.	 ###
		### Use the summary() function to produce summary statistics about ###
		### the tree. Note how many terminal nodes the tree has.           ###

install.packages("tree")		#INSTALL THE TREE PACKAGE
library(tree)				#LOAD TREE PACKAGE		


tree.College = tree(Apps~.,College,subset=train)		#FIT REGRESSION TREE TO TRAINING DATA
summary(tree.College)							#PROVIDE SUMMARY STATS OF TREE
										#NODES = 9

############## b.Type in the name of the tree object in order to get a detailed text output. ####################################  


tree.College		#DETAILED TEXT OUTPUT


################ c. Create a plot of the tree. (Hint: use plot() and text() functions)##################################

plot(tree.College)			#PLOT THE TREE
text(tree.College,pretty=0)		#ADD THE ACCEPTANCE INFO



################### d.	Now use cross validation function cv.tree() to the training data set ################################  
################ 		 to see whether pruning the tree will improve performance (to determine ################################  
################ 				 the optimal tree size)			 ######################################################### 
	
cv.College=cv.tree(tree.College)		#RUN CROSS VALIDATION FUNCTION 
names(cv.College)					#DISPLAY HEADERS 
cv.College						#DISPLAY CROSSVALIDATION DATA


################### e. Produce a plot with tree size on the x-axis and cross-validated classification ################################################################
################### error on the y-axis. (Hint: use the plot() function) ################################################################


plot(cv.College$size,cv.College$dev,type='b')	#PLOT WITH TREE SIZE ON X-AXIS AND CV CLASSIFICATION ERR ON Y


################### f. Produce a pruned tree corresponding to the optimal tree size obtained using ################################
####################### cross-validation in parts (d) and (e). If cross-validation does not lead to ################################
##################### selection of a pruned tree, then create a pruned tree with eight terminal nodes. ################################	

prune.College=prune.tree(tree.College,best=8)	#PRUNE TO 8 NODES
summary(prune.College)					#DISPLAY SUMMARY


################### g. Compute the test error rates (test MSE) between the pruned and unpruned trees  ################


yHat=predict(tree.College,newdata=College[-train,])  		#UNPRUNED TREE PREDICTION WITH TEST DATA
College.test=College[-train,"Apps"]					#CREATE COLLEGE.TEST
plot(yHat,College.test)							#OBSERVE PLOT WITH AB LINE
abline(0,1)

### The test set MSE associated with the regression tree is:

mean((yHat-College.test)^2)						#FIND THE TEST MSE FOR UNPRUNED TREE


##### TEST MSE WITH THE PRUNED TREE #######################################

yHat2=predict(prune.College,newdata=College[-train,])  	#PRUNED TREE PREDICTION WITH TEST DATA
plot(yHat2,College.test)						#OBSERVE PLOT WITH AB LINE
abline(0,1)

mean((yHat2-College.test)^2)						#FIND THE TEST MSE FOR PRUNED TREE (HIGHER)

################### h. Compare the above two test error rates in part (g) (pruned and unpruned trees) ################################
###################    with the one obtained using LASSO regression (test MSE) in Part 1(d). ################################

mean((lasso.pred-r.test)^2)	  #LASSO regression test MSE (1107427)
mean((yHat-College.test)^2)	  #UNPRUNED TREE WITH 9 NODES (2742553)
mean((yHat2-College.test)^2)	#PRUNED TREE WITH 8 NODES (2847350)

##   ANSWER TO h.) Between the two test error rates in part g of the pruned and unpruned trees the
##   most accurate model came from the unpruned tree, with a slightly lower MSE
##   If you consider the LASSO regression test error rate from Part 1(d), it shows that 
##   the LASSO regression produced the most accurate model because it has a considerably lower mean squared error
##   compared to the two tree based models.



