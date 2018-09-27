# Loading car pice data set
car <- read.csv("CarPrice_Assignment.csv", stringsAsFactors = TRUE)
str(car)


# Step 0: Preprocessing and deriving new metrics from "CarName". "companyName" can be used in creating models
# Step 0.1: Separating car names from model in column "CarName"
library(tidyr)
carPrice <- separate(car, CarName, into=c("companyName", "modelName"), sep=" ")

# Step 1: Cleaning the data set

# Step 1.1: Check for NA Values
sum(is.na(carPrice))
sum(is.na(carPrice$modelName))
# We can see there are 2 NA values in newly created "modelName". But as mentioned in the requirements, we need to consider only "companyName" as independent varibale for model building, hence we will ignore this column while model building.
# So effectively now there are no NA values. Even now we can can remove this column as this will not be used in model building as stated above.
carPrice <- carPrice[,-4]


# Step 1.2: Check for duplicate Values
sum(duplicated(carPrice))
# There are no duplicate values in the dataset. However we have seen where all column values are same but only differ in model name and hence we cannot treat these as duplicate entries. Since there can be variation across different models.
# Now Although there are no duplicate entries visible from these commands. But in "companyName" we can see some names being differently spelled. Like "maxda", "Nissan", "vw", "vokswagen", "porcshce". We can replace with one correct name for these.
carPrice$companyName[which(carPrice$companyName == "Nissan")] <- "nissan"
carPrice$companyName[which(carPrice$companyName == "porcshce")] <- "porsche"
carPrice$companyName[which(carPrice$companyName == "maxda")] <- "mazda"
carPrice$companyName[which(carPrice$companyName == "vokswagen")] <- "volkswagen"
carPrice$companyName[which(carPrice$companyName == "vw")] <- "volkswagen"
carPrice$companyName[which(carPrice$companyName == "toyouta")] <- "toyota"
carPrice$companyName <- as.factor(carPrice$companyName)

# Step 1.3: Checking for outliers. We will check for outliers at every 1% step
quantile(carPrice$wheelbase, seq(0,1,0.01))
carPrice$wheelbase[which(carPrice$wheelbase > 115.544)] <- 115.544 # There is a jump from 99% to 100% and hence capping the valuess

quantile(carPrice$carlength, seq(0,1,0.01))
carPrice$carlength[which(carPrice$carlength > 199.568)] <- 199.568 # Capping for more than 99%
carPrice$carlength[which(carPrice$carlength < 155.9)] <- 155.9 # Capping for less than 3%

quantile(carPrice$curbweight, seq(0,1,0.01))
carPrice$curbweight[which(carPrice$curbweight < 1488.00)] <- 1488.00 # Capping for less than 1%

quantile(carPrice$enginesize, seq(0,1,0.01))
carPrice$enginesize[which(carPrice$enginesize > 209.00)] <- 209.00 # Capping for more than 97%
carPrice$enginesize[which(carPrice$enginesize < 90.00)] <- 90.00 # Capping for less than 3%

quantile(carPrice$horsepower, seq(0,1,0.01)) # This is fine and there are no outliers
carPrice$horsepower[which(carPrice$horsepower > 207.00)] <- 207.00 # Capping for more than 99%

quantile(carPrice$citympg, seq(0,1,0.01)) # This is fine and there are no outliers
carPrice$citympg[which(carPrice$citympg > 38.00)] <- 38.00 # Capping for more than 98%

quantile(carPrice$highwaympg, seq(0,1,0.01)) # This is fine and there are no outliers
carPrice$citympg[which(carPrice$citympg > 49.88)] <- 49.88 # Capping for more than 99%

quantile(carPrice$carwidth, seq(0,1,0.01)) # This is fine and there are no outliers
quantile(carPrice$carheight, seq(0,1,0.01)) # This is fine and there are no outliers
quantile(carPrice$boreratio, seq(0,1,0.01)) # This is fine and there are no outliers
quantile(carPrice$stroke, seq(0,1,0.01)) # This is fine and there are no outliers
quantile(carPrice$compressionratio, seq(0,1,0.01)) # This is fine and there are no outliers
quantile(carPrice$peakrpm, seq(0,1,0.01)) # This is fine and there are no outliers


# Step 1.4: Converting categorical variable having different levels into numeric types and multiple dummy variables.
# First converting the variables with only 2 levels like "fueltype", "aspiration", "doornumber", "enginelocation"

# 1 for gas & 0 for diesel
levels(carPrice$fueltype)<-c(1,0)
carPrice$fueltype <- as.numeric(levels(carPrice$fueltype))[carPrice$fueltype]

# 1 for std & 0 for turbo
levels(carPrice$aspiration)<-c(1,0)
carPrice$aspiration <- as.numeric(levels(carPrice$aspiration))[carPrice$aspiration]

# 1 for two & 0 for four
levels(carPrice$doornumber)<-c(1,0)
carPrice$doornumber <- as.numeric(levels(carPrice$doornumber))[carPrice$doornumber]

# 1 for front & 0 for rear
levels(carPrice$enginelocation)<-c(1,0)
carPrice$enginelocation <- as.numeric(levels(carPrice$enginelocation))[carPrice$enginelocation]

# Now creating dummy variables for more than 2 levels "carbody", "drivewheel", "enginetype", 
# "cylindernumber", "fuelsystem", "companyName"
dummyVariable <- model.matrix( ~carbody, data = carPrice)
dummyVariable <- dummyVariable[,-1]
carPrice <- cbind(carPrice[,-7], dummyVariable)

dummyVariable <- model.matrix( ~drivewheel, data = carPrice)
dummyVariable <- dummyVariable[,-1]
carPrice <- cbind(carPrice[,-7], dummyVariable)

dummyVariable <- model.matrix( ~enginetype, data = carPrice)
dummyVariable <- dummyVariable[,-1]
carPrice <- cbind(carPrice[,-13], dummyVariable)

dummyVariable <- model.matrix( ~cylindernumber, data = carPrice)
dummyVariable <- dummyVariable[,-1]
carPrice <- cbind(carPrice[,-13], dummyVariable)

dummyVariable <- model.matrix( ~fuelsystem, data = carPrice)
dummyVariable <- dummyVariable[,-1]
carPrice <- cbind(carPrice[,-14], dummyVariable)

dummyVariable <- model.matrix( ~companyName, data = carPrice)
dummyVariable <- dummyVariable[,-1]
carPrice <- cbind(carPrice[,-3], dummyVariable)


#Step 1.5: Deriving new metrics
# We can create a total dimensions from "carheight", "carlength" & "carwidth". This can be a good dimension as total dimensions of car sometimes matters to individuals.
carPrice$totalDimensions <- carPrice$carlength + carPrice$carwidth + carPrice$carheight

# We can again average mileage of cars on highways and city. This again can be a deciding factor for buying a car.
carPrice$averageMileage <- (carPrice$highwaympg + carPrice$citympg) / 2

# If we don't find these variables significant we will remove them subsequently in the model creations.

# Step 2: Model Building
carPrice <- carPrice
# Now we have cleaned the datasets and we can go ahead building with models. For this we will divide our test and train sets in 75% and 25%.
# Setting seed to 100 to make it reproducible
set.seed(100)

# Randomly generate 75% rows for Train dataset 
trainIndices= sample(1:nrow(carPrice), 0.70*nrow(carPrice))
# Generate the train data set
train = carPrice[trainIndices,]

# Similarly generate the Test data set
test = carPrice[-trainIndices,]

# Loading libraries "car" and "MASS" for "VIF" and "stepAIC" respectively
library(car)
library(MASS)

# Since everythign column is either integer and numeric we can go ahead and start creating the ML models
# Model 1:
model1 <-lm(price~.,data=train)
summary(model1)

# As we can see there many variables for which coefficients are not defined. It can be because of the fact that these variables can be collinear with other inddependent variables.
# This may mean that these variables are not being used anywhere. Going ahead with stepAIC to see if they are removed or not.
step <- stepAIC(model1, direction = "both")

# To get the model parameters we can do "step"
step

# Now creating a model with the equations we got above. From now on we will verifying "VIF's" also for models.
model2 <- lm(formula = price ~ car_ID + aspiration + enginelocation + wheelbase + 
               carwidth + curbweight + stroke + carbodyhardtop + carbodyhatchback + 
               carbodysedan + carbodywagon + enginetypedohcv + enginetypel + 
               enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberfour + 
               cylindernumbersix + companyNamebmw + companyNamedodge + companyNamehonda + 
               companyNameisuzu + companyNamejaguar + companyNamemazda + 
               companyNamemercury + companyNamemitsubishi + companyNamenissan + 
               companyNameplymouth + companyNamerenault + companyNamesaab + 
               companyNametoyota + companyNamevolkswagen + companyNamevolvo + 
               horsepower, data = train)
summary(model2)
vif(model2)

# As we can see "companyNametoyota" & "car_ID" has one of the highest VIF's and but they are also very significant with 3 stars. So we have to remove some other variable. As we can see that "horsepower" has largest "P-Value" and is therefore most insignificant. Hence removing this.
model3 <- lm(formula = price ~ car_ID + aspiration + enginelocation + wheelbase + 
               carwidth + curbweight + stroke + carbodyhardtop + carbodyhatchback + 
               carbodysedan + carbodywagon + enginetypedohcv + enginetypel + 
               enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberfour + 
               cylindernumbersix + companyNamebmw + companyNamedodge + companyNamehonda + 
               companyNameisuzu + companyNamejaguar + companyNamemazda + 
               companyNamemercury + companyNamemitsubishi + companyNamenissan + 
               companyNameplymouth + companyNamerenault + companyNamesaab + 
               companyNametoyota + companyNamevolkswagen + companyNamevolvo, data = train)
summary(model3)
vif(model3)

# After removing "horsepower" we can see that the "Adjusted R-squared" didn't change much. Hence this can be done.
# Now going ahead, again "companyNametoyota" & "car_ID" has one of the highest VIF's. 
# Also almost every variable has 3 start. Now to decide which variable will be removed, we will pick the one with highest VIF's among the variables with less no. of stars.
# So removing "wheelbase" as it has VIF 8.469678 among "stroke" & "carbodyhardtop"
model4 <- lm(formula = price ~ car_ID + aspiration + enginelocation + 
               carwidth + curbweight + stroke + carbodyhardtop + carbodyhatchback + 
               carbodysedan + carbodywagon + enginetypedohcv + enginetypel + 
               enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberfour + 
               cylindernumbersix + companyNamebmw + companyNamedodge + companyNamehonda + 
               companyNameisuzu + companyNamejaguar + companyNamemazda + 
               companyNamemercury + companyNamemitsubishi + companyNamenissan + 
               companyNameplymouth + companyNamerenault + companyNamesaab + 
               companyNametoyota + companyNamevolkswagen + companyNamevolvo, data = train)
summary(model4)
vif(model4)

# Again the variables with highest VIF's remains unchanged and now removing "carbodyhardtop" as it has no stars in p-value.
model5 <- lm(formula = price ~ car_ID + aspiration + enginelocation + 
               carwidth + curbweight + stroke + carbodyhatchback + 
               carbodysedan + carbodywagon + enginetypedohcv + enginetypel + 
               enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberfour + 
               cylindernumbersix + companyNamebmw + companyNamedodge + companyNamehonda + 
               companyNameisuzu + companyNamejaguar + companyNamemazda + 
               companyNamemercury + companyNamemitsubishi + companyNamenissan + 
               companyNameplymouth + companyNamerenault + companyNamesaab + 
               companyNametoyota + companyNamevolkswagen + companyNamevolvo, data = train)
summary(model5)
vif(model5)

# Now removing "carbodyhatchback" as it has highest VIF among "stroke" & "carbodysedan" with 1 stars.
model6 <- lm(formula = price ~ car_ID + aspiration + enginelocation + 
               carwidth + curbweight + stroke + 
               carbodysedan + carbodywagon + enginetypedohcv + enginetypel + 
               enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberfour + 
               cylindernumbersix + companyNamebmw + companyNamedodge + companyNamehonda + 
               companyNameisuzu + companyNamejaguar + companyNamemazda + 
               companyNamemercury + companyNamemitsubishi + companyNamenissan + 
               companyNameplymouth + companyNamerenault + companyNamesaab + 
               companyNametoyota + companyNamevolkswagen + companyNamevolvo, data = train)
summary(model6)
vif(model6)

# Removing "carbodysedan" as it has no stars.
model7 <- lm(formula = price ~ car_ID + aspiration + enginelocation + 
               carwidth + curbweight + stroke + carbodywagon + enginetypedohcv + enginetypel + 
               enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberfour + 
               cylindernumbersix + companyNamebmw + companyNamedodge + companyNamehonda + 
               companyNameisuzu + companyNamejaguar + companyNamemazda + 
               companyNamemercury + companyNamemitsubishi + companyNamenissan + 
               companyNameplymouth + companyNamerenault + companyNamesaab + 
               companyNametoyota + companyNamevolkswagen + companyNamevolvo, data = train)
summary(model7)
vif(model7)

# Removing "stroke" as it has highest VIF among "carbodywagon"
model8 <- lm(formula = price ~ car_ID + aspiration + enginelocation + 
               carwidth + curbweight + carbodywagon + enginetypedohcv + enginetypel + 
               enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberfour + 
               cylindernumbersix + companyNamebmw + companyNamedodge + companyNamehonda + 
               companyNameisuzu + companyNamejaguar + companyNamemazda + 
               companyNamemercury + companyNamemitsubishi + companyNamenissan + 
               companyNameplymouth + companyNamerenault + companyNamesaab + 
               companyNametoyota + companyNamevolkswagen + companyNamevolvo, data = train)
summary(model8)
vif(model8)

# Removing "carbodywagon" with 1 star
model9 <- lm(formula = price ~ car_ID + aspiration + enginelocation + 
               carwidth + curbweight + enginetypedohcv + enginetypel + 
               enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberfour + 
               cylindernumbersix + companyNamebmw + companyNamedodge + companyNamehonda + 
               companyNameisuzu + companyNamejaguar + companyNamemazda + 
               companyNamemercury + companyNamemitsubishi + companyNamenissan + 
               companyNameplymouth + companyNamerenault + companyNamesaab + 
               companyNametoyota + companyNamevolkswagen + companyNamevolvo, data = train)
summary(model9)
vif(model9)

# Removing "companyNamejaguar" as it as highest VIF among "companyNameisuzu" and both of them have 2 stars.
model10 <- lm(formula = price ~ car_ID + aspiration + enginelocation + 
               carwidth + curbweight + enginetypedohcv + enginetypel + 
               enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberfour + 
               cylindernumbersix + companyNamebmw + companyNamedodge + companyNamehonda + 
               companyNameisuzu + companyNamemazda + 
               companyNamemercury + companyNamemitsubishi + companyNamenissan + 
               companyNameplymouth + companyNamerenault + companyNamesaab + 
               companyNametoyota + companyNamevolkswagen + companyNamevolvo, data = train)
summary(model10)
vif(model10)

# Removing "aspiration" as it is the only one with 2 stars.
model11 <- lm(formula = price ~ car_ID + enginelocation + 
                carwidth + curbweight + enginetypedohcv + enginetypel + 
                enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + companyNamebmw + companyNamedodge + companyNamehonda + 
                companyNameisuzu + companyNamemazda + 
                companyNamemercury + companyNamemitsubishi + companyNamenissan + 
                companyNameplymouth + companyNamerenault + companyNamesaab + 
                companyNametoyota + companyNamevolkswagen + companyNamevolvo, data = train)
summary(model11)
vif(model11)

# Now we have reached a point where all the variables have 3 stars. So we can check the r-square for our predictions
# Predict the car prices from the testing dataset
# Getting column number of "price" in "car" data set
columnNumberOfPrice = which( colnames(carPrice)=="price")
predict1 <- predict(model11,test[,-columnNumberOfPrice])
test$testPrice <- predict1
# Now calculating the accuracy of the predictions
# Calculate correlation
rValue <- cor(test$price,test$testPrice)
# calculate R squared by squaring correlation
rSquared <- cor(test$price,test$testPrice)^2
# check R-squared
rSquared

# Now if we see the value of "rSquared" is "0.86" which is very less from Adjusted R-squared:0.9556. Therefore we cannot stop here and must go ahead to remove some more variables to get these two values more near to each other.
# Continuing on building model
summary(model11)
vif(model11)

# Since all the variables have 3 stars now we will removed on the basis of High VIF's. So removing "companyNametoyota" as it has highest VIF = 35
model12 <- lm(formula = price ~ car_ID + enginelocation + carwidth + curbweight + enginetypedohcv + 
                enginetypel + enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + companyNamebmw + companyNamedodge + companyNamehonda + companyNameisuzu + 
                companyNamemazda + companyNamemercury + companyNamemitsubishi + companyNamenissan + 
                companyNameplymouth + companyNamerenault + companyNamesaab + companyNamevolkswagen + 
                companyNamevolvo, data = train)
summary(model12)
vif(model12)

# After removing "companyNametoyota" many variables have became insignificant and mostly related to companyName.
# Now we can again start removing on the basis of high VIF's. "cylindernumberfour" has highest VIF but it is very significant.
# Searching in this pattern we get "enginetypeohcf" with VIF = 1.641082 and 0 star in p-value. So removing this.
model13 <- lm(formula = price ~ car_ID + enginelocation + carwidth + curbweight + enginetypedohcv + 
                enginetypel + enginetyperotor + cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                companyNamebmw + companyNamedodge + companyNamehonda + companyNameisuzu + companyNamemazda + 
                companyNamemercury + companyNamemitsubishi + companyNamenissan + companyNameplymouth + 
                companyNamerenault + companyNamesaab + companyNamevolkswagen + companyNamevolvo, data = train)
summary(model13)
vif(model13)

# Now almost all the variables with high VIF's are very significant and hence removing on the basis of high p-value or less no. of stars.
# We get "companyNameplymouth" as with highest p-value. So removing this.
model14 <- lm(formula = price ~ car_ID + enginelocation + carwidth + curbweight + enginetypedohcv + 
                enginetypel + enginetyperotor + cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                companyNamebmw + companyNamedodge + companyNamehonda + companyNameisuzu + companyNamemazda + 
                companyNamemercury + companyNamemitsubishi + companyNamenissan + 
                companyNamerenault + companyNamesaab + companyNamevolkswagen + companyNamevolvo, data = train)
summary(model14)
vif(model14)

# Again on the same reasoning as above, removing highest p-value variable. Removing "companyNamenissan".
model15 <- lm(formula = price ~ car_ID + enginelocation + carwidth + curbweight + enginetypedohcv + 
                enginetypel + enginetyperotor + cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                companyNamebmw + companyNamedodge + companyNamehonda + companyNameisuzu + companyNamemazda + 
                companyNamemercury + companyNamemitsubishi + companyNamerenault + companyNamesaab + 
                companyNamevolkswagen + companyNamevolvo, data = train)
summary(model15)
vif(model15)

# Removing "companyNameisuzu" with highest p-value.
model16 <- lm(formula = price ~ car_ID + enginelocation + carwidth + curbweight + enginetypedohcv + 
                enginetypel + enginetyperotor + cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                companyNamebmw + companyNamedodge + companyNamehonda + companyNamemazda + 
                companyNamemercury + companyNamemitsubishi + companyNamerenault + companyNamesaab + 
                companyNamevolkswagen + companyNamevolvo, data = train)
summary(model16)
vif(model16)

# Removing "companyNamevolkswagen" with highest p-value.
model17 <- lm(formula = price ~ car_ID + enginelocation + carwidth + curbweight + enginetypedohcv + 
                enginetypel + enginetyperotor + cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                companyNamebmw + companyNamedodge + companyNamehonda + companyNamemazda + companyNamemercury + 
                companyNamemitsubishi + companyNamerenault + companyNamesaab + companyNamevolvo, data = train)
summary(model17)
vif(model17)

# Removing "companyNamevolvo" with highest p-value.
model18 <- lm(formula = price ~ car_ID + enginelocation + carwidth + curbweight + enginetypedohcv + 
                enginetypel + enginetyperotor + cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                companyNamebmw + companyNamedodge + companyNamehonda + companyNamemazda + companyNamemercury + 
                companyNamemitsubishi + companyNamerenault + companyNamesaab, data = train)
summary(model18)
vif(model18)

# Removing "companyNamesaab" with highest p-value.
model19 <- lm(formula = price ~ car_ID + enginelocation + carwidth + curbweight + enginetypedohcv + 
                enginetypel + enginetyperotor + cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                companyNamebmw + companyNamedodge + companyNamehonda + companyNamemazda + companyNamemercury + 
                companyNamemitsubishi + companyNamerenault, data = train)
summary(model19)
vif(model19)

# Removing "companyNamemercury" with highest p-value. Although it has lower VIF than "companyNamehonda"
model20 <- lm(formula = price ~ car_ID + enginelocation + carwidth + curbweight + enginetypedohcv + 
                enginetypel + enginetyperotor + cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                companyNamebmw + companyNamedodge + companyNamehonda + companyNamemazda + 
                companyNamemitsubishi + companyNamerenault, data = train)
summary(model20)
vif(model20)

# Removing "companyNamehonda" with highest p-value.
model21 <- lm(formula = price ~ car_ID + enginelocation + carwidth + curbweight + enginetypedohcv + 
                enginetypel + enginetyperotor + cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                companyNamebmw + companyNamedodge + companyNamemazda + companyNamemitsubishi + 
                companyNamerenault, data = train)
summary(model21)
vif(model21)

# Removing "companyNamedodge" with highest p-value and higher VIF than "companyNamerenault" with 1 stars.
model22 <- lm(formula = price ~ car_ID + enginelocation + carwidth + curbweight + enginetypedohcv + 
                enginetypel + enginetyperotor + cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                companyNamebmw + companyNamemazda + companyNamemitsubishi + 
                companyNamerenault, data = train)
summary(model22)
vif(model22)

# Removing "companyNamemazda" with highest p-value and higher VIF than "companyNamemitsubishi" & "companyNamerenault" with 1 stars.
model23 <- lm(formula = price ~ car_ID + enginelocation + carwidth + curbweight + enginetypedohcv + 
                enginetypel + enginetyperotor + cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                companyNamebmw + companyNamemitsubishi + 
                companyNamerenault, data = train)
summary(model23)
vif(model23)

# Removing "companyNamerenault" with highest p-value among 1 stars.
model24 <- lm(formula = price ~ car_ID + enginelocation + carwidth + curbweight + enginetypedohcv + 
                enginetypel + enginetyperotor + cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                companyNamebmw + companyNamemitsubishi, data = train)
summary(model24)
vif(model24)

# Removing "companyNamemitsubishi" with highest p-value among 1 stars.
model25 <- lm(formula = price ~ car_ID + enginelocation + carwidth + curbweight + enginetypedohcv + 
                enginetypel + enginetyperotor + cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                companyNamebmw, data = train)
summary(model25)
vif(model25)

# Removing "enginetypedohcv" with highest p-value and 1 star.
model26 <- lm(formula = price ~ car_ID + enginelocation + carwidth + curbweight + enginetypel + 
                enginetyperotor + cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                companyNamebmw, data = train)
summary(model26)
vif(model26)

# Again we are at a stage where all the variables have 3 stars. Let's calculate r-square for predicted test data.
# Predict the car prices from the testing dataset
# Getting column number of "price" in "car" data set
columnNumberOfPrice = which( colnames(carPrice)=="price")
predict1 <- predict(model26,test[,-columnNumberOfPrice])
test$testPrice <- predict1
# Now calculating the accuracy of the predictions
# Calculate correlation
rValue <- cor(test$price,test$testPrice)
# calculate R squared by squaring correlation
rSquared <- cor(test$price,test$testPrice)^2
# check R-squared
rSquared

# Here the r-square for the predicted from test is only "0.875" compared to "Adjusted R-squared":  0.9336.
# So we will go ahead and remove some more variable.
summary(model26)
vif(model26)

# Now based on highest VIF we will remove, "cylindernumberfour", since all the variables have 3 stars.
model27 <- lm(formula = price ~ car_ID + enginelocation + carwidth + curbweight + enginetypel + 
                enginetyperotor + cylindernumberfive + cylindernumbersix + 
                companyNamebmw, data = train)
summary(model27)
vif(model27)

# Now we have again got 3 very insignificant variables and hence removing them. Removing "cylindernumbersix".
model28 <- lm(formula = price ~ car_ID + enginelocation + carwidth + curbweight + enginetypel + 
                enginetyperotor + cylindernumberfive + companyNamebmw, data = train)
summary(model28)
vif(model28)

# Removing "enginetyperotor".
model29 <- lm(formula = price ~ car_ID + enginelocation + carwidth + curbweight + enginetypel + 
                cylindernumberfive + companyNamebmw, data = train)
summary(model29)
vif(model29)

# Removing "cylindernumberfive".
model30 <- lm(formula = price ~ car_ID + enginelocation + carwidth + curbweight + enginetypel + 
                companyNamebmw, data = train)
summary(model30)
vif(model30)

# Again we have all the variables as 3 stars. Let's calculate r-square for predicted test data set.
# Predict the car prices from the testing dataset
# Getting column number of "price" in "car" data set
columnNumberOfPrice = which( colnames(carPrice)=="price")
predict1 <- predict(model30,test[,-columnNumberOfPrice])
test$testPrice <- predict1
# Now calculating the accuracy of the predictions
# Calculate correlation
rValue <- cor(test$price,test$testPrice)
# calculate R squared by squaring correlation
rSquared <- cor(test$price,test$testPrice)^2
# check R-squared
rSquared

# It is "0.8636321" compared to "Adjusted R-squared:0.874". Now both of them seems to be almost same.
# Hence this can be one of our models. (FINAL-MODEL30)

# But let's go ahead and make some more changes.
# In "model12" when we removed "companyNametoyota", we decided to go ahead with the priciple that highest p-value variable should be removed first.
# Although at that time "cylindernumberfour" had very high VIF = 9.275459. As we saw in "model27" the "Adjusted R-squared" dropped from "0.9336" to "0.8824".
# This is very high drop. So let's try removing "cylindernumberfour" after "model12"
model31 <- lm(formula = price ~ car_ID + enginelocation + carwidth + curbweight + enginetypedohcv + 
                enginetypel + enginetypeohcf + enginetyperotor + cylindernumberfive + 
                cylindernumbersix + companyNamebmw + companyNamedodge + companyNamehonda + companyNameisuzu + 
                companyNamemazda + companyNamemercury + companyNamemitsubishi + companyNamenissan + 
                companyNameplymouth + companyNamerenault + companyNamesaab + companyNamevolkswagen + 
                companyNamevolvo, data = train)
summary(model31)
vif(model31)

# As we can see after removing "cylindernumberfour" the "Adjusted R-squared" dropped to "0.9076" from "0.9336" in "model12" which is again a significant drop.
# Let's go ahead and remove some more variables in this new route. Let's remove "enginetypeohcf" with next highest VIF and also insignificant.
model32 <- lm(formula = price ~ car_ID + enginelocation + carwidth + curbweight + enginetypedohcv + 
                enginetypel + enginetyperotor + cylindernumberfive + 
                cylindernumbersix + companyNamebmw + companyNamedodge + companyNamehonda + companyNameisuzu + 
                companyNamemazda + companyNamemercury + companyNamemitsubishi + companyNamenissan + 
                companyNameplymouth + companyNamerenault + companyNamesaab + companyNamevolkswagen + 
                companyNamevolvo, data = train)
summary(model32)
vif(model32)

# Removing "cylindernumbersix" with next highest VIF and insignificant.
model33 <- lm(formula = price ~ car_ID + enginelocation + carwidth + curbweight + enginetypedohcv + 
                enginetypel + enginetyperotor + cylindernumberfive + companyNamebmw + companyNamedodge + 
                companyNamehonda + companyNameisuzu + companyNamemazda + companyNamemercury + 
                companyNamemitsubishi + companyNamenissan + companyNameplymouth + companyNamerenault + 
                companyNamesaab + companyNamevolkswagen + companyNamevolvo, data = train)
summary(model33)
vif(model33)

# Removing "companyNamevolvo" with next highest VIF and insignificant.
model34 <- lm(formula = price ~ car_ID + enginelocation + carwidth + curbweight + enginetypedohcv + 
                enginetypel + enginetyperotor + cylindernumberfive + companyNamebmw + companyNamedodge + 
                companyNamehonda + companyNameisuzu + companyNamemazda + companyNamemercury + 
                companyNamemitsubishi + companyNamenissan + companyNameplymouth + companyNamerenault + 
                companyNamesaab + companyNamevolkswagen, data = train)
summary(model34)
vif(model34)

# Removing "companyNamebmw" with next highest VIF and insignificant.
model35 <- lm(formula = price ~ car_ID + enginelocation + carwidth + curbweight + enginetypedohcv + 
                enginetypel + enginetyperotor + cylindernumberfive + companyNamedodge + 
                companyNamehonda + companyNameisuzu + companyNamemazda + companyNamemercury + 
                companyNamemitsubishi + companyNamenissan + companyNameplymouth + companyNamerenault + 
                companyNamesaab + companyNamevolkswagen, data = train)
summary(model35)
vif(model35)
# This decrease the Adjusted r-squared and hence we will revert this.

# Removing "enginetyperotor" with next highest VIF and insignificant.
model36 <- lm(formula = price ~ car_ID + enginelocation + carwidth + curbweight + enginetypedohcv + 
                enginetypel + cylindernumberfive + companyNamebmw + companyNamedodge + 
                companyNamehonda + companyNameisuzu + companyNamemazda + companyNamemercury + 
                companyNamemitsubishi + companyNamenissan + companyNameplymouth + companyNamerenault + 
                companyNamesaab + companyNamevolkswagen, data = train)
summary(model36)
vif(model36)

# Removing "companyNamevolkswagen" with next highest VIF and insignificant.
model37 <- lm(formula = price ~ car_ID + enginelocation + carwidth + curbweight + enginetypedohcv + 
                enginetypel + cylindernumberfive + companyNamebmw + companyNamedodge + 
                companyNamehonda + companyNameisuzu + companyNamemazda + companyNamemercury + 
                companyNamemitsubishi + companyNamenissan + companyNameplymouth + companyNamerenault + 
                companyNamesaab, data = train)
summary(model37)
vif(model37)

# Removing "companyNamesaab" with next highest VIF and insignificant.
model38 <- lm(formula = price ~ car_ID + enginelocation + carwidth + curbweight + enginetypedohcv + 
                enginetypel + cylindernumberfive + companyNamebmw + companyNamedodge + 
                companyNamehonda + companyNameisuzu + companyNamemazda + companyNamemercury + 
                companyNamemitsubishi + companyNamenissan + companyNameplymouth + companyNamerenault, data = train)
summary(model38)
vif(model38)

# Removing "enginetypedohcv" with next highest VIF and insignificant.
model39 <- lm(formula = price ~ car_ID + enginelocation + carwidth + curbweight + 
                enginetypel + cylindernumberfive + companyNamebmw + companyNamedodge + 
                companyNamehonda + companyNameisuzu + companyNamemazda + companyNamemercury + 
                companyNamemitsubishi + companyNamenissan + companyNameplymouth + companyNamerenault, data = train)
summary(model39)
vif(model39)

# Removing "companyNameisuzu" with next highest VIF and insignificant.
model40 <- lm(formula = price ~ car_ID + enginelocation + carwidth + curbweight + 
                enginetypel + cylindernumberfive + companyNamebmw + companyNamedodge + 
                companyNamehonda + companyNamemazda + companyNamemercury + 
                companyNamemitsubishi + companyNamenissan + companyNameplymouth + companyNamerenault, data = train)
summary(model40)
vif(model40)

# Removing "companyNameplymouth" with next highest VIF and insignificant.
model41 <- lm(formula = price ~ car_ID + enginelocation + carwidth + curbweight + 
                enginetypel + cylindernumberfive + companyNamebmw + companyNamedodge + 
                companyNamehonda + companyNamemazda + companyNamemercury + 
                companyNamemitsubishi + companyNamenissan + companyNamerenault, data = train)
summary(model41)
vif(model41)

# Removing "companyNamenissan" with next highest VIF and 1 star.
model42 <- lm(formula = price ~ car_ID + enginelocation + carwidth + curbweight + 
                enginetypel + cylindernumberfive + companyNamebmw + companyNamedodge + 
                companyNamehonda + companyNamemazda + companyNamemercury + 
                companyNamemitsubishi + companyNamerenault, data = train)
summary(model42)
vif(model42)

# Removing "companyNamemercury" with next highest VIF and 1 star.
model43 <- lm(formula = price ~ car_ID + enginelocation + carwidth + curbweight + 
                enginetypel + cylindernumberfive +companyNamebmw + companyNamedodge + 
                companyNamehonda + companyNamemazda + 
                companyNamemitsubishi + companyNamerenault, data = train)
summary(model43)
vif(model43)

# Removing "companyNamerenault" with next highest VIF and 1 star.
model44 <- lm(formula = price ~ car_ID + enginelocation + carwidth + curbweight + 
                enginetypel + cylindernumberfive + companyNamebmw + companyNamedodge + 
                companyNamehonda + companyNamemazda + 
                companyNamemitsubishi, data = train)
summary(model44)
vif(model44)

# Removing "companyNamehonda" since this is only one with 2 star.
model45 <- lm(formula = price ~ car_ID + enginelocation + carwidth + curbweight + 
                enginetypel + cylindernumberfive + companyNamebmw + companyNamedodge + companyNamemazda + 
                companyNamemitsubishi, data = train)
summary(model45)
vif(model45)

# Removing "cylindernumberfive" with next highest VIF and 2 star.
model46 <- lm(formula = price ~ car_ID + enginelocation + carwidth + curbweight + 
                enginetypel + companyNamedodge + companyNamebmw + companyNamemazda + 
                companyNamemitsubishi, data = train)
summary(model46)
vif(model46)

# Removing "companyNamemazda" with next highest VIF and 2 star.
model47 <- lm(formula = price ~ car_ID + enginelocation + carwidth + curbweight + 
                enginetypel + companyNamedodge + companyNamebmw +
                companyNamemitsubishi, data = train)
summary(model47)
vif(model47)

# Removing "companyNamedodge" since this is only one with 2 star.
model48 <- lm(formula = price ~ car_ID + enginelocation + carwidth + curbweight + companyNamebmw +
                enginetypel + companyNamemitsubishi, data = train)
summary(model48)
vif(model48)

# Removing "companyNamemitsubishi" since this is only one with 2 star.
model49 <- lm(formula = price ~ car_ID + enginelocation + carwidth + curbweight + companyNamebmw +
                enginetypel, data = train)
summary(model49)
vif(model49)

# Again all the variables have 3 stars and our model is same as "MODEL30".

# So overall MODEL30 (Line No. 484) & MODEl49 is the final model with following indepedent variables:
# "car_ID", "enginelocation", "carwidth", "curbweight", "enginetypel" and "companyNamebmw".
# The Adjusted R-Squared is "0.874" and "r-square" on predicted data set is "0.8636321" which is almost same.

######################################################## END OF CASE STUDY ###########################################################