
###########################  FI 4090 #####################################################################
######################### ASSIGNMENT 1 #################################################
######################## CREATED BY: HARRY DULANEY #######################################################

###########################################################################################
################################ PART 1 ################################################################
#########################################################################################

### LOAD QUANTMOD PACKAGE ###

library(quantmod) 

###  DOWNLOAD MICROSOFT STOCK DATA FROM YAHOO(DEFAULT SRC)  ###
###  		FOR PERIOD JAN. 2ND, 2001 to DEC. 31ST, 2016    ###

getSymbols("MSFT",from="2001-01-02", to="2016-12-31")

###   SEE THE SIZE OF THE DATA   ###

dim(MSFT)

### OBSERVE THE FIRST 6 ROWS OF DATA ###

head(MSFT)   

### TIME PLOT DAILY PRICE AND VOLUME ###

chartSeries(MSFT,theme="white") 

### COMPUTE LOG RETURNS ###

MSFT.logReturn = diff(log(MSFT$MSFT.Adjusted))

### REMOVE BLANK OBSERVATIONS ###

MSFT.logReturn = na.omit(MSFT.logReturn)

### CONSTRUCT A TIME PLOT OF DAILY LOG RETURNS FOR MICROSOFT STOCK DATA ###

chartSeries(MSFT.logReturn,theme="white")

### COMPUTE THE SAMPLE MEAN,STD,MIN,MAX ###
###		 LOAD FBASICS LIBRARY 	    ###

library(fBasics)

### 	SUMMARY (includes MIN and MAX)     ###
###   MIN = -0.1245782, MAX = 0.1706256  ###

summary(MSFT.logReturn)

### 	COMPUTE MEAN 	###
### MEAN = 0.0003577994 ###

mean(MSFT.logReturn)

### COMPUTE STANDARD DEVIATION ###
###	  STD = 0.01829244       ###

stdev(MSFT.logReturn) 


			###   ANALYSIS OF FINDINGS FOR PART 1  ###

# The time plot of the daily log returns for Microsoft between Jan. 2nd 2001 and  		  #
# Dec. 31st 2016 shows that returns were more volatile in some periods 		    		  #
# compared to others. The highest volatility in Jan 2nd 2009 is consistent with   		  #
# the market volatility at that time, as this was the height of the financial crisis.	  #
# However, for investors who held on through the ups and downs, Microsoft paid good returns #
# tripling in price between Jan 2001 and Dec 2016.							  #

##############################################################################################
###########################################################################################
################################ Part 2 ################################################################
#########################################################################################

### LOAD LIBRARY: IF NEW SESSION ###

library(quantmod) 


### 	DOWNLOAD NVIDIA HISTORICAL PRICE AND VOLUME DATA	 ###
###			 FROM YAHOO(DEFAULT Src)	     		 ###
###   FOR TIME PERIOD JAN. 2ND, 2001 to DEC. 31ST, 2016 	 ###

getSymbols("NVDA",from="2001-01-02", to="2016-08-20")

### CONFIRM DOWNLOAD ###

head(NVDA)

### TIME PLOT PRICE AND VOLUME FOR NVIDIA ###

chartSeries(NVDA,theme="black") 

### COMPUTE LOG RETURNS ###

NVDA.LOG = diff(log(NVDA$NVDA.Adjusted))

### REMOVE BLANK OBSERVATIONS ###

NVDA.LOG = na.omit(NVDA.LOG)

### CHECK RESULTS ###

head(NVDA.LOG)
tail(NVDA.LOG)

### CONSTRUCT TIME PLOT OF OF DAILY LOG RETURNS ###

chartSeries(NVDA.LOG,theme="white")

###   COMPUTE THE SAMPLE MEAN, MIN, AND MAX OF LOG RETURN SERIES	###
  #  		MEAN = 0.0006593, MIN = -0.4343819, MAX = 0.2856566   #

summary(NVDA.LOG)

### STANDARD DEVIATION ###
  #  STD = 0.03640378  #

stdev(NVDA.LOG)



			###   ANALYSIS OF FINDINGS FOR PART 2  ###

# The time plot of daily log returns for NVIDIA between Jan. 2nd, 2001    #
# and Aug. 20th, 2016 show some areas of high volatility and others of    #
# low volatility. The areas of high volatility are more extreme then we   #	 
# observed for Microsoft stock, over a similar period. As well, NVIDIA's  #
# returns were much more volatile, heavy tails, before the great  	  #
# recession than they were afterward, when they became much less volatile.#
# While NVIDIAs share price suffered greatly leading up to the recession, #
# the stock overcame this to show excellent returns between Jan 2009 and  #
# Aug 2016. This is likley do to the rapid growth of demand for their 	  #
# graphics cards, as they became the most popular graphics cards for 	  #
# mining crypto currency, which exploded in popularity during this time.  #






