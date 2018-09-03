#---------------------------------- Import required packages ----------------------------------
library(tidyr)
library(dplyr)
library(MASS)
library(car)
library(ggplot2)
library(stringr)
#library(fuzzywuzzyR)
#---------------------------------------------------------------------------------------------



#---------------------------------- Business understanding & Problem statement ----------------------------------
# GOAL is to identify -  
#       Which variables are significant in predicting the price of a car?
#       How well those variables describe the price of a car?
#---------------------------------------------------------------------------------------------



#---------------------------------- Data Dictonary ----------------------------------
# Car_ID	 	Unique id of each observation (Interger)
# Symboling 	Its assigned insurance risk rating, A value of +3 indicates that the auto is risky, -3 that it is probably pretty safe.(Categorical) 
# carCompany	Name of car company (Categorical)
# fueltype	Car fuel type i.e gas or diesel (Categorical)
# aspiration	Aspiration used in a car (Categorical)
# doornumber	Number of doors in a car (Categorical)
# carbody	body of car (Categorical)
# drivewheel	type of drive wheel (Categorical)
# enginelocation	Location of car engine (Categorical)
# wheelbase	Weelbase of car (Numeric)
# carlength	Length of car (Numeric)
# carwidth	Width of car (Numeric)
# carheight	height of car (Numeric)
# curbweight	The weight of a car without occupants or baggage. (Numeric)
# enginetype	Type of engine. (Categorical)
# cylindernumber	cylinder placed in the car (Categorical)
# enginesize	Size of car (Numeric)
# fuelsystem	Fuel system of car (Categorical)
# boreratio	Boreratio of car (Numeric)
# stroke	Stroke or volume inside the engine (Numeric)
# compressionratio	compression ratio of car (Numeric)
# horsepower	Horsepower (Numeric)
# peakrpm	car peak rpm (Numeric)
# citympg	Mileage in city (Numeric)
# highwaympg	Mileage on highway (Numeric)
# price(Dependent variable)	Price of car (Numeric)
#---------------------------------------------------------------------------------------------



#---------------------------------- Load the input dataset ----------------------------------
master <- read.csv("CarPrice_Assignment.csv",stringsAsFactors = FALSE)
master_backup <- master #For backup
#View the data
View(master)
str(master)
summary(master)

#As per the input #CarName is comprised of two parts - 'car company' and 'car model'
#Seperate them and exclude 'car model' from the analysys
master$carcompany <- str_split_fixed(master$CarName, " ", 2)[,1] #Don't include [,2] which is 'car model'
#---------------------------------------------------------------------------------------------



#---------------------------------- TODO:(Code refactor) Write all hepler functions here ----------------------------------
# TODO: Write common function to remove outliers in numeric variables
#---------------------------------------------------------------------------------------------





#---------------------------------- Data cleaning and preparation for modeling ----------------------------------
#---------------------------------- Analys each independent metric ----------------------------------

#Check or NA values
sum(is.na(master)) # 0 NA values - No issues

#Duplicate rows
sum(duplicated(master)) # 0 No duplicates

#Column by column analysis
#car_ID - Remove this metric since it is uniuew ID
master <- master[, -c(1)]

#symboling - Since it is categorical variable lets convert it into factors - Categorical
#Using -3 to 3 representation would lead to incorrect mathematical model.
master$symboling <- as.factor(master$symboling)
levels(master$symboling) <- c(0:6)
master$symboling <- as.numeric(master$symboling)

#Remove CarName from the model and check for carcompany instead - Categorical
master <- master[, -c(2)]
levels(as.factor(master$carcompany))
#Fix the spelling mistakes in the company names
master$carcompany[master$carcompany == "Nissan"] <- "nissan"
master$carcompany[master$carcompany == "vokswagen"] <- "volkswagen"
master$carcompany[master$carcompany == "maxda"] <- "mazda"
master$carcompany[master$carcompany == "porcshce"] <- "porsche"
master$carcompany[master$carcompany == "toyouta"] <- "toyota"
master$carcompany[master$carcompany == "vw"] <- "volkswagen"
levels(as.factor(master$carcompany))
summary(as.factor(master$carcompany))
num_carcompany <- model.matrix(~carcompany, master)
num_carcompany <- num_carcompany[,-1]
master <- cbind(master[,-which(colnames(master)=='carcompany')],num_carcompany)


#fueltype - OK - Categorical
levels(as.factor(master$fueltype))
summary(as.factor(master$fueltype))
master$fueltype <- ifelse(master$fueltype == "gas",0,1) #Convert to numerical, Boolean type

#aspiration - OK - Categorical
levels(as.factor(master$aspiration))
summary(as.factor(master$aspiration))
master$aspiration <- ifelse(master$aspiration == "std",0,1) #Convert to numerical, Boolean type

#doornumber - OK - Categorical
levels(as.factor(master$doornumber))
summary(as.factor(master$doornumber))
master$doornumber <- ifelse(master$doornumber == "doornumber",0,1) #Convert to numerical, Boolean type

#carbody - OK - Categorical - Expand factors into dummy variables
levels(as.factor(master$carbody))
summary(as.factor(master$carbody))
num_carbody <- model.matrix(~carbody, master)
num_carbody <- num_carbody[,-1]
master <- cbind(master[,-which(colnames(master)=='carbody')],num_carbody)


#drivewheel - OK - Categorical - Expand factors into dummy variables
levels(as.factor(master$drivewheel))
summary(as.factor(master$drivewheel))
num_drivewheel <- model.matrix(~drivewheel, master)
num_drivewheel <- num_drivewheel[,-1]
master <- cbind(master[,-which(colnames(master)=='drivewheel')],num_drivewheel)

#enginelocation - OK - Categorica
levels(as.factor(master$enginelocation))
summary(as.factor(master$enginelocation))
master$enginelocation <- ifelse(master$enginelocation == "front",0,1) #Convert to numerical, Boolean type

#enginetype - Not OK - Categorical - Has some invalid engine types & Expand factors into dummy variables
levels(as.factor(master$enginetype))
summary(as.factor(master$enginetype))
#As per google search - ohcf & ohcv are same as ohc and dohcv is same as dohc
master$enginetype[master$enginetype == "dohcv"] <- "dohc"
master$enginetype[master$enginetype == "ohcf"] <- "ohc"
master$enginetype[master$enginetype == "ohcv"] <- "ohc"
summary(as.factor(master$enginetype))
num_enginetype <- model.matrix(~enginetype, master)
num_enginetype <- num_enginetype[,-1]
master <- cbind(master[,-which(colnames(master)=='enginetype')],num_enginetype)


#cylindernumber - OK - Categorical - Expand factors into dummy variables
levels(as.factor(master$cylindernumber))
summary(as.factor(master$cylindernumber))
num_cylindernumber <- model.matrix(~cylindernumber, master)
num_cylindernumber <- num_cylindernumber[,-1]
master <- cbind(master[,-which(colnames(master)=='cylindernumber')],num_cylindernumber)


#fuelsystem - OK - Categorical - Expand factors into dummy variables
levels(as.factor(master$fuelsystem))
summary(as.factor(master$fuelsystem))
num_fuelsystem <- model.matrix(~fuelsystem, master)
num_fuelsystem <- num_fuelsystem[,-1]
master <- cbind(master[,-which(colnames(master)=='fuelsystem')],num_fuelsystem)


#All - Numeric
par(mfrow=c(3,5))
boxplot(master$wheelbase, plot=TRUE)
boxplot(master$carlength, plot=TRUE)
boxplot(master$carwidth, plot=TRUE)
boxplot(master$carheight, plot=TRUE)
boxplot(master$curbweight, plot=TRUE)
boxplot(master$enginesize, plot=TRUE)
boxplot(master$boreratio, plot=TRUE)
boxplot(master$stroke, plot=TRUE)
boxplot(master$compressionratio, plot=TRUE)
boxplot(master$horsepower, plot=TRUE)
boxplot(master$peakrpm, plot=TRUE)
boxplot(master$citympg, plot=TRUE)
boxplot(master$highwaympg, plot=TRUE)

#Handle outliers for each numeric metric
out <- boxplot(master$wheelbase, plot=FALSE)$out
if(length(out) > 0){
  val1 <- quantile(master$wheelbase, probs=c(.05, .95), na.rm = T,  names = FALSE)[1]
  val2 <- quantile(master$wheelbase, probs=c(.05, .95), na.rm = T,  names = FALSE)[2]
  master$wheelbase[(which(master$wheelbase < val1))] <- val1
  master$wheelbase[(which(master$wheelbase > val2))] <- val2
}

out <- boxplot(master$carlength, plot=FALSE)$out
if(length(out) > 0){
  val1 <- quantile(master$carlength, probs=c(.05, .95), na.rm = T,  names = FALSE)[1]
  val2 <- quantile(master$carlength, probs=c(.05, .95), na.rm = T,  names = FALSE)[2]
  master$carlength[(which(master$carlength < val1))] <- val1
  master$carlength[(which(master$carlength > val2))] <- val2
}

out <- boxplot(master$carwidth, plot=FALSE)$out
if(length(out) > 0){
  val1 <- quantile(master$carwidth, probs=c(.05, .95), na.rm = T,  names = FALSE)[1]
  val2 <- quantile(master$carwidth, probs=c(.05, .95), na.rm = T,  names = FALSE)[2]
  master$carwidth[(which(master$carwidth < val1))] <- val1
  master$carwidth[(which(master$carwidth > val2))] <- val2
}

out <- boxplot(master$carheight, plot=FALSE)$out
if(length(out) > 0){
  val1 <- quantile(master$carheight, probs=c(.05, .95), na.rm = T,  names = FALSE)[1]
  val2 <- quantile(master$carheight, probs=c(.05, .95), na.rm = T,  names = FALSE)[2]
  master$carheight[(which(master$carheight < val1))] <- val1
  master$carheight[(which(master$carheight > val2))] <- val2
}

out <- boxplot(master$curbweight, plot=FALSE)$out
if(length(out) > 0){
  val1 <- quantile(master$curbweight, probs=c(.05, .95), na.rm = T,  names = FALSE)[1]
  val2 <- quantile(master$curbweight, probs=c(.05, .95), na.rm = T,  names = FALSE)[2]
  master$curbweight[(which(master$curbweight < val1))] <- val1
  master$curbweight[(which(master$curbweight > val2))] <- val2
}

out <- boxplot(master$enginesize, plot=FALSE)$out
if(length(out) > 0){
  val1 <- quantile(master$enginesize, probs=c(.05, .95), na.rm = T,  names = FALSE)[1]
  val2 <- quantile(master$enginesize, probs=c(.05, .95), na.rm = T,  names = FALSE)[2]
  master$enginesize[(which(master$enginesize < val1))] <- val1
  master$enginesize[(which(master$enginesize > val2))] <- val2
}

out <- boxplot(master$boreratio, plot=FALSE)$out
if(length(out) > 0){
  val1 <- quantile(master$boreratio, probs=c(.05, .95), na.rm = T,  names = FALSE)[1]
  val2 <- quantile(master$boreratio, probs=c(.05, .95), na.rm = T,  names = FALSE)[2]
  master$boreratio[(which(master$boreratio < val1))] <- val1
  master$boreratio[(which(master$boreratio > val2))] <- val2
}

out <- boxplot(master$stroke, plot=FALSE)$out
if(length(out) > 0){
  val1 <- quantile(master$stroke, probs=c(.05, .95), na.rm = T,  names = FALSE)[1]
  val2 <- quantile(master$stroke, probs=c(.05, .95), na.rm = T,  names = FALSE)[2]
  master$stroke[(which(master$stroke < val1))] <- val1
  master$stroke[(which(master$stroke > val2))] <- val2
}

out <- boxplot(master$compressionratio, plot=FALSE)$out
if(length(out) > 0){
  val1 <- quantile(master$compressionratio, probs=c(.05, .95), na.rm = T,  names = FALSE)[1]
  val2 <- quantile(master$compressionratio, probs=c(.05, .95), na.rm = T,  names = FALSE)[2]
  master$compressionratio[(which(master$compressionratio < val1))] <- val1
  master$compressionratio[(which(master$compressionratio > val2))] <- val2
}

out <- boxplot(master$horsepower, plot=FALSE)$out
if(length(out) > 0){
  val1 <- quantile(master$horsepower, probs=c(.05, .95), na.rm = T,  names = FALSE)[1]
  val2 <- quantile(master$horsepower, probs=c(.05, .95), na.rm = T,  names = FALSE)[2]
  master$horsepower[(which(master$horsepower < val1))] <- val1
  master$horsepower[(which(master$horsepower > val2))] <- val2
}

out <- boxplot(master$peakrpm, plot=FALSE)$out
if(length(out) > 0){
  val1 <- quantile(master$peakrpm, probs=c(.05, .95), na.rm = T,  names = FALSE)[1]
  val2 <- quantile(master$peakrpm, probs=c(.05, .95), na.rm = T,  names = FALSE)[2]
  master$peakrpm[(which(master$peakrpm < val1))] <- val1
  master$peakrpm[(which(master$peakrpm > val2))] <- val2
}

out <- boxplot(master$citympg, plot=FALSE)$out
if(length(out) > 0){
  val1 <- quantile(master$citympg, probs=c(.05, .95), na.rm = T,  names = FALSE)[1]
  val2 <- quantile(master$citympg, probs=c(.05, .95), na.rm = T,  names = FALSE)[2]
  master$citympg[(which(master$citympg < val1))] <- val1
  master$citympg[(which(master$citympg > val2))] <- val2
}

out <- boxplot(master$highwaympg, plot=FALSE)$out
if(length(out) > 0){
  val1 <- quantile(master$highwaympg, probs=c(.05, .95), na.rm = T,  names = FALSE)[1]
  val2 <- quantile(master$highwaympg, probs=c(.05, .95), na.rm = T,  names = FALSE)[2]
  master$highwaympg[(which(master$highwaympg < val1))] <- val1
  master$highwaympg[(which(master$highwaympg > val2))] <- val2
}

#After handling Outliers
par(mfrow=c(3,5))
boxplot(master$wheelbase, plot=TRUE)
boxplot(master$carlength, plot=TRUE)
boxplot(master$carwidth, plot=TRUE)
boxplot(master$carheight, plot=TRUE)
boxplot(master$curbweight, plot=TRUE)
boxplot(master$enginesize, plot=TRUE)
boxplot(master$boreratio, plot=TRUE)
boxplot(master$stroke, plot=TRUE)
boxplot(master$compressionratio, plot=TRUE)
boxplot(master$horsepower, plot=TRUE)
boxplot(master$peakrpm, plot=TRUE)
boxplot(master$citympg, plot=TRUE)
boxplot(master$highwaympg, plot=TRUE)
#---------------------------------------------------------------------------------------------




#---------------------------------- Create Model ----------------------------------
set.seed(100)
numOfRows <- nrow(master)
trainIds <- sample(1:numOfRows, 0.7 * numOfRows)
trainData <- master[trainIds,]
testData <- master[-trainIds,]

model1 <- lm(price ~ ., data = trainData)
summary(model1)
#Multiple R-squared:  0.9779,	Adjusted R-squared:  0.9648 

step <- stepAIC(model1, direction = 'both')
step

#stepAIC suggested dependent metrics
model2 <- lm(formula = price ~ symboling + fueltype + aspiration + enginelocation + 
               wheelbase + curbweight + boreratio + stroke + compressionratio + 
               peakrpm + citympg + carcompanyaudi + carcompanybmw + carcompanybuick + 
               carcompanychevrolet + carcompanydodge + carcompanyhonda + 
               carcompanyisuzu + carcompanyjaguar + carcompanymazda + carcompanymercury + 
               carcompanymitsubishi + carcompanynissan + carcompanypeugeot + 
               carcompanyplymouth + carcompanyrenault + carcompanysaab + 
               carcompanysubaru + carcompanytoyota + carcompanyvolkswagen + 
               carcompanyvolvo + carbodywagon + drivewheelfwd + enginetyperotor + 
               cylindernumberfive + cylindernumberfour + cylindernumbersix + 
               fuelsystem2bbl + fuelsystemmpfi, data = trainData)
summary(model2)
#Multiple R-squared:  0.9769,	Adjusted R-squared:  0.9681 
#carcompanyjaguar pVal is 0.194917 > 0.05, SO remove from the model

model3 <- lm(formula = price ~ symboling + fueltype + aspiration + enginelocation + 
               wheelbase + curbweight + boreratio + stroke + compressionratio + 
               peakrpm + citympg + carcompanyaudi + carcompanybmw + carcompanybuick + 
               carcompanychevrolet + carcompanydodge + carcompanyhonda + 
               carcompanyisuzu + carcompanymazda + carcompanymercury + 
               carcompanymitsubishi + carcompanynissan + carcompanypeugeot + 
               carcompanyplymouth + carcompanyrenault + carcompanysaab + 
               carcompanysubaru + carcompanytoyota + carcompanyvolkswagen + 
               carcompanyvolvo + carbodywagon + drivewheelfwd + enginetyperotor + 
               cylindernumberfive + cylindernumberfour + cylindernumbersix + 
               fuelsystem2bbl + fuelsystemmpfi, data = trainData)
summary(model3)
#Multiple R-squared:  0.9765,	Adjusted R-squared:  0.9679
#carcompanybuick  pVal is 0.074676 > 0.05, SO remove from the model

model4 <- lm(formula = price ~ symboling + fueltype + aspiration + enginelocation + 
               wheelbase + curbweight + boreratio + stroke + compressionratio + 
               peakrpm + citympg + carcompanyaudi + carcompanybmw + 
               carcompanychevrolet + carcompanydodge + carcompanyhonda + 
               carcompanyisuzu + carcompanymazda + carcompanymercury + 
               carcompanymitsubishi + carcompanynissan + carcompanypeugeot + 
               carcompanyplymouth + carcompanyrenault + carcompanysaab + 
               carcompanysubaru + carcompanytoyota + carcompanyvolkswagen + 
               carcompanyvolvo + carbodywagon + drivewheelfwd + enginetyperotor + 
               cylindernumberfive + cylindernumberfour + cylindernumbersix + 
               fuelsystem2bbl + fuelsystemmpfi, data = trainData)
summary(model4)
#Multiple R-squared:  0.9758,	Adjusted R-squared:  0.9672
#boreratio 0.284703 > 0.05, So Remove it

model5 <- lm(formula = price ~ symboling + fueltype + aspiration + enginelocation + 
               wheelbase + curbweight + stroke + compressionratio + 
               peakrpm + citympg + carcompanyaudi + carcompanybmw + 
               carcompanychevrolet + carcompanydodge + carcompanyhonda + 
               carcompanyisuzu + carcompanymazda + carcompanymercury + 
               carcompanymitsubishi + carcompanynissan + carcompanypeugeot + 
               carcompanyplymouth + carcompanyrenault + carcompanysaab + 
               carcompanysubaru + carcompanytoyota + carcompanyvolkswagen + 
               carcompanyvolvo + carbodywagon + drivewheelfwd + enginetyperotor + 
               cylindernumberfive + cylindernumberfour + cylindernumbersix + 
               fuelsystem2bbl + fuelsystemmpfi, data = trainData)
summary(model5)
#Multiple R-squared:  0.9755,	Adjusted R-squared:  0.9672
#fuelsystem2bbl 0.072006 >0.05, SO remove it

model6 <- lm(formula = price ~ symboling + fueltype + aspiration + enginelocation + 
               wheelbase + curbweight + stroke + compressionratio + 
               peakrpm + citympg + carcompanyaudi + carcompanybmw + 
               carcompanychevrolet + carcompanydodge + carcompanyhonda + 
               carcompanyisuzu + carcompanymazda + carcompanymercury + 
               carcompanymitsubishi + carcompanynissan + carcompanypeugeot + 
               carcompanyplymouth + carcompanyrenault + carcompanysaab + 
               carcompanysubaru + carcompanytoyota + carcompanyvolkswagen + 
               carcompanyvolvo + carbodywagon + drivewheelfwd + enginetyperotor + 
               cylindernumberfive + cylindernumberfour + cylindernumbersix + 
               fuelsystemmpfi, data = trainData)
summary(model6)
#Multiple R-squared:  0.9747,	Adjusted R-squared:  0.9665 
#peakrpm 0.133045 > 0.05 remove this metric

model7 <- lm(formula = price ~ symboling + fueltype + aspiration + enginelocation + 
               wheelbase + curbweight + stroke + compressionratio + 
               citympg + carcompanyaudi + carcompanybmw + 
               carcompanychevrolet + carcompanydodge + carcompanyhonda + 
               carcompanyisuzu + carcompanymazda + carcompanymercury + 
               carcompanymitsubishi + carcompanynissan + carcompanypeugeot + 
               carcompanyplymouth + carcompanyrenault + carcompanysaab + 
               carcompanysubaru + carcompanytoyota + carcompanyvolkswagen + 
               carcompanyvolvo + carbodywagon + drivewheelfwd + enginetyperotor + 
               cylindernumberfive + cylindernumberfour + cylindernumbersix + 
               fuelsystemmpfi, data = trainData)
summary(model7)
#Multiple R-squared:  0.9742,	Adjusted R-squared:  0.966
#fueltype 0.069349>0.05, remove it

model8 <- lm(formula = price ~ symboling + aspiration + enginelocation + 
               wheelbase + curbweight + stroke + compressionratio + 
               citympg + carcompanyaudi + carcompanybmw + 
               carcompanychevrolet + carcompanydodge + carcompanyhonda + 
               carcompanyisuzu + carcompanymazda + carcompanymercury + 
               carcompanymitsubishi + carcompanynissan + carcompanypeugeot + 
               carcompanyplymouth + carcompanyrenault + carcompanysaab + 
               carcompanysubaru + carcompanytoyota + carcompanyvolkswagen + 
               carcompanyvolvo + carbodywagon + drivewheelfwd + enginetyperotor + 
               cylindernumberfive + cylindernumberfour + cylindernumbersix + 
               fuelsystemmpfi, data = trainData)
summary(model8)
#Multiple R-squared:  0.9734,	Adjusted R-squared:  0.9653
#fuelsystemmpfi 0.242217  >0.05

model9 <- lm(formula = price ~ symboling + aspiration + enginelocation + 
               wheelbase + curbweight + stroke + compressionratio + 
               citympg + carcompanyaudi + carcompanybmw + 
               carcompanychevrolet + carcompanydodge + carcompanyhonda + 
               carcompanyisuzu + carcompanymazda + carcompanymercury + 
               carcompanymitsubishi + carcompanynissan + carcompanypeugeot + 
               carcompanyplymouth + carcompanyrenault + carcompanysaab + 
               carcompanysubaru + carcompanytoyota + carcompanyvolkswagen + 
               carcompanyvolvo + carbodywagon + drivewheelfwd + enginetyperotor + 
               cylindernumberfive + cylindernumberfour + cylindernumbersix , data = trainData)
summary(model9)
#Multiple R-squared:  0.9731,	Adjusted R-squared:  0.9652
#citympg  0.288387 > 0.05

model10 <- lm(formula = price ~ symboling + aspiration + enginelocation + 
                wheelbase + curbweight + stroke + compressionratio + 
                carcompanyaudi + carcompanybmw + 
                carcompanychevrolet + carcompanydodge + carcompanyhonda + 
                carcompanyisuzu + carcompanymazda + carcompanymercury + 
                carcompanymitsubishi + carcompanynissan + carcompanypeugeot + 
                carcompanyplymouth + carcompanyrenault + carcompanysaab + 
                carcompanysubaru + carcompanytoyota + carcompanyvolkswagen + 
                carcompanyvolvo + carbodywagon + drivewheelfwd + enginetyperotor + 
                cylindernumberfive + cylindernumberfour + cylindernumbersix , data = trainData)
summary(model10)
#Multiple R-squared:  0.9728,	Adjusted R-squared:  0.9652
#compressionratio 0.155164   > 0.05

model11 <- lm(formula = price ~ symboling + aspiration + enginelocation + 
                wheelbase + curbweight + stroke + 
                carcompanyaudi + carcompanybmw + 
                carcompanychevrolet + carcompanydodge + carcompanyhonda + 
                carcompanyisuzu + carcompanymazda + carcompanymercury + 
                carcompanymitsubishi + carcompanynissan + carcompanypeugeot + 
                carcompanyplymouth + carcompanyrenault + carcompanysaab + 
                carcompanysubaru + carcompanytoyota + carcompanyvolkswagen + 
                carcompanyvolvo + carbodywagon + drivewheelfwd + enginetyperotor + 
                cylindernumberfive + cylindernumberfour + cylindernumbersix , data = trainData)
summary(model11)
#Multiple R-squared:  0.9723,	Adjusted R-squared:  0.9648
#drivewheelfwd pVal 0.064500

model12 <- lm(formula = price ~ symboling + aspiration + enginelocation + 
                wheelbase + curbweight + stroke + 
                carcompanyaudi + carcompanybmw + 
                carcompanychevrolet + carcompanydodge + carcompanyhonda + 
                carcompanyisuzu + carcompanymazda + carcompanymercury + 
                carcompanymitsubishi + carcompanynissan + carcompanypeugeot + 
                carcompanyplymouth + carcompanyrenault + carcompanysaab + 
                carcompanysubaru + carcompanytoyota + carcompanyvolkswagen + 
                carcompanyvolvo + carbodywagon + enginetyperotor + 
                cylindernumberfive + cylindernumberfour + cylindernumbersix , data = trainData)
summary(model12)
#Multiple R-squared:  0.9714,	Adjusted R-squared:  0.9641
#carcompanybmw pval 0.078157

model13 <- lm(formula = price ~ symboling + aspiration + enginelocation + 
                wheelbase + curbweight + stroke + 
                carcompanyaudi + 
                carcompanychevrolet + carcompanydodge + carcompanyhonda + 
                carcompanyisuzu + carcompanymazda + carcompanymercury + 
                carcompanymitsubishi + carcompanynissan + carcompanypeugeot + 
                carcompanyplymouth + carcompanyrenault + carcompanysaab + 
                carcompanysubaru + carcompanytoyota + carcompanyvolkswagen + 
                carcompanyvolvo + carbodywagon + enginetyperotor + 
                cylindernumberfive + cylindernumberfour + cylindernumbersix , data = trainData)
summary(model13)
#Multiple R-squared:  0.9706,	Adjusted R-squared:  0.9634
#symboling pVal 0.025342>0.05

model14 <- lm(formula = price ~  aspiration + enginelocation + 
                wheelbase + curbweight + stroke + 
                carcompanyaudi + 
                carcompanychevrolet + carcompanydodge + carcompanyhonda + 
                carcompanyisuzu + carcompanymazda + carcompanymercury + 
                carcompanymitsubishi + carcompanynissan + carcompanypeugeot + 
                carcompanyplymouth + carcompanyrenault + carcompanysaab + 
                carcompanysubaru + carcompanytoyota + carcompanyvolkswagen + 
                carcompanyvolvo + carbodywagon + enginetyperotor + 
                cylindernumberfive + cylindernumberfour + cylindernumbersix , data = trainData)
summary(model14)
#Multiple R-squared:  0.9693,	Adjusted R-squared:  0.9621
#stroke pVal 0.017887>0.05

model15 <-lm(formula = price ~  aspiration + enginelocation + 
               wheelbase + curbweight +
               carcompanyaudi + 
               carcompanychevrolet + carcompanydodge + carcompanyhonda + 
               carcompanyisuzu + carcompanymazda + carcompanymercury + 
               carcompanymitsubishi + carcompanynissan + carcompanypeugeot + 
               carcompanyplymouth + carcompanyrenault + carcompanysaab + 
               carcompanysubaru + carcompanytoyota + carcompanyvolkswagen + 
               carcompanyvolvo + carbodywagon + enginetyperotor + 
               cylindernumberfive + cylindernumberfour + cylindernumbersix , data = trainData)
summary(model15)
#Multiple R-squared:  0.9677,	Adjusted R-squared:  0.9605
vif(model15)
#cylindernumberfour vif 15.007344

model16 <-lm(formula = price ~  aspiration + enginelocation + 
               wheelbase + curbweight +
               carcompanyaudi + 
               carcompanychevrolet + carcompanydodge + carcompanyhonda + 
               carcompanyisuzu + carcompanymazda + carcompanymercury + 
               carcompanymitsubishi + carcompanynissan + carcompanypeugeot + 
               carcompanyplymouth + carcompanyrenault + carcompanysaab + 
               carcompanysubaru + carcompanytoyota + carcompanyvolkswagen + 
               carcompanyvolvo + carbodywagon + enginetyperotor + 
               cylindernumberfive + cylindernumbersix , data = trainData)
summary(model16)
#Multiple R-squared:  0.9426,	Adjusted R-squared:  0.9304
# There is a big change in Adjusted R-squared after removing cylindernumberfour
# So Lets evaluate model15

#Multiple R-squared & Adjusted R-squared close enough each other and pVal is least for all
#Dependent metrics. Lets evaluate this model.
#---------------------------------------------------------------------------------




#---------------------------------- Final Model stats ----------------------------------
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-4300.8  -937.4     0.0   807.7  6885.7 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)          -1.803e+04  5.349e+03  -3.371 0.001019 ** 
#  aspiration            1.569e+03  4.772e+02   3.288 0.001335 ** 
#  enginelocation        1.453e+04  1.272e+03  11.422  < 2e-16 ***
#  wheelbase             3.334e+02  6.058e+01   5.503 2.26e-07 ***
#  curbweight            5.904e+00  7.473e-01   7.900 1.76e-12 ***
#  carcompanyaudi       -2.901e+03  1.021e+03  -2.841 0.005306 ** 
#  carcompanychevrolet  -1.661e+04  2.280e+03  -7.289 4.12e-11 ***
#  carcompanydodge      -7.663e+03  1.111e+03  -6.896 2.99e-10 ***
#  carcompanyhonda      -5.819e+03  8.996e+02  -6.468 2.46e-09 ***
#  carcompanyisuzu      -8.553e+03  1.786e+03  -4.788 5.01e-06 ***
#  carcompanymazda      -6.572e+03  9.237e+02  -7.114 9.96e-11 ***
#  carcompanymercury    -6.588e+03  1.789e+03  -3.682 0.000353 ***
#  carcompanymitsubishi -7.624e+03  9.386e+02  -8.123 5.49e-13 ***
#  carcompanynissan     -5.680e+03  8.611e+02  -6.596 1.31e-09 ***
#  carcompanypeugeot    -1.062e+04  1.033e+03 -10.282  < 2e-16 ***
#  carcompanyplymouth   -7.376e+03  1.009e+03  -7.310 3.70e-11 ***
#  carcompanyrenault    -6.635e+03  1.370e+03  -4.845 3.95e-06 ***
#  carcompanysaab       -4.183e+03  9.923e+02  -4.216 4.95e-05 ***
#  carcompanysubaru     -7.140e+03  1.004e+03  -7.109 1.02e-10 ***
#  carcompanytoyota     -7.584e+03  7.392e+02 -10.260  < 2e-16 ***
#  carcompanyvolkswagen -6.780e+03  8.657e+02  -7.832 2.51e-12 ***
#  carcompanyvolvo      -5.888e+03  9.417e+02  -6.252 6.95e-09 ***
#  carbodywagon         -1.571e+03  4.583e+02  -3.429 0.000840 ***
#  enginetyperotor      -9.085e+03  1.587e+03  -5.726 8.22e-08 ***
#  cylindernumberfive   -1.223e+04  1.224e+03  -9.990  < 2e-16 ***
#  cylindernumberfour   -1.187e+04  1.248e+03  -9.506 3.41e-16 ***
#  cylindernumbersix    -9.340e+03  1.040e+03  -8.979 5.84e-15 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 1623 on 116 degrees of freedom
#Multiple R-squared:  0.9677,	Adjusted R-squared:  0.9605 
#F-statistic: 133.9 on 26 and 116 DF,  p-value: < 2.2e-16

#VIF
#aspiration       enginelocation            wheelbase           curbweight 
#1.892654             1.803361             5.351426             8.369227 
#carcompanyaudi  carcompanychevrolet      carcompanydodge      carcompanyhonda 
#1.907818             1.957810             1.821948             3.375795 
#carcompanyisuzu      carcompanymazda    carcompanymercury carcompanymitsubishi 
#1.202307             3.558764             1.206093             2.524664 
#carcompanynissan    carcompanypeugeot   carcompanyplymouth    carcompanyrenault 
#3.325196             2.328135             2.221015             1.403396 
#carcompanysaab     carcompanysubaru     carcompanytoyota carcompanyvolkswagen 
#1.802687             2.199937             4.540606             2.397938 
#carcompanyvolvo         carbodywagon      enginetyperotor   cylindernumberfive 
#2.240153             1.313035             2.805476             4.792614 
#cylindernumberfour    cylindernumbersix 
#15.007344             5.834300 
#---------------------------------------------------------------------------------

#-------------------------------------Evaluate the model--------------------------------------------
predictedPrice <- predict(model15, testData[,-which(colnames(testData)=='price')])
testData$testPrice <- predictedPrice
rVal <- cor(testData$price,testData$testPrice)
rsquared <- cor(testData$price,testData$testPrice)^2
# 0.8145126
#---------------------------------------------------------------------------------

#-------------------------------------SUMMARY--------------------------------------------
#0.8145126 - OUR MODEL IS 81% ACCURATE IN PREDECTING THE PRICE.

# CarCompany, EngineLocation, Aspiration, WheelBasem CurbWeight, cylindernumber, enginetyperotor
# CarBodyWagor pays key roles in deciding the price of the car
# Look at the weights to understand the detailed impact

#Metrics which effects the car price are the following
#  aspiration            
#  enginelocation       
#  wheelbase            
#  curbweight           
#  carcompanyaudi       
#  carcompanychevrolet  
#  carcompanydodge      
#  carcompanyhonda      
#  carcompanyisuzu      
#  carcompanymazda      
#  carcompanymercury    
#  carcompanymitsubishi 
#  carcompanynissan     
#  carcompanypeugeot    
#  carcompanyplymouth   
#  carcompanyrenault    
#  carcompanysaab       
#  carcompanysubaru     
#  carcompanytoyota     
#  carcompanyvolkswagen 
#  carcompanyvolvo      
#  carbodywagon         
#  enginetyperotor      
#  cylindernumberfive   
#  cylindernumberfour   
#  cylindernumbersix    
#---------------------------------------------------------------------------------