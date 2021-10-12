library(tidyverse)
library(caret)
library(olsrr)
library(car)
df_house_price <- read.csv("train.csv", header = TRUE)
View(df_house_price)
str(df_house_price)
#Checking for NA values
is.na(df_house_price)
#Replacing NA values with values mentioned in the Description Doc
df_house_price$Alley[is.na(df_house_price$Alley)] <- "No alley access"
df_house_price$MiscFeature[is.na(df_house_price$MiscFeature)] <- "None"
df_house_price$Fence[is.na(df_house_price$Fence)] <- "No Fence"
df_house_price$PoolQC[is.na(df_house_price$PoolQC)] <- "No Pool"
df_house_price$FireplaceQu[is.na(df_house_price$FireplaceQu)] <- "No Fireplace"
df_house_price$GarageType[is.na(df_house_price$GarageType)] <- "No Garage"
df_house_price$GarageFinish[is.na(df_house_price$GarageFinish)] <- "No Garage"
df_house_price$GarageQual[is.na(df_house_price$GarageQual)] <- "No Garage"
df_house_price$GarageCond[is.na(df_house_price$GarageCond)] <- "No Garage"
df_house_price$BsmtQual[is.na(df_house_price$BsmtQual)] <- "No Basement"
df_house_price$BsmtCond[is.na(df_house_price$BsmtCond)] <- "No Basement"
df_house_price$BsmtExposure[is.na(df_house_price$BsmtExposure)] <- "No Basement"
df_house_price$BsmtFinType1[is.na(df_house_price$BsmtFinType1)] <- "No Basement"
df_house_price$BsmtFinType2[is.na(df_house_price$BsmtFinType2)] <- "No Basement"
df_house_price$LotFrontage[is.na(df_house_price$LotFrontage)] <- 0
view(df_house_price)

 

#checking to verify
is.na(df_house_price)

lm_area <- lm(SalePrice ~ MSSubClass + MSZoning + LotFrontage + LotArea + Street + Alley + LotShape + LandContour + LotConfig + LandSlope, data = df_house_price)
summary(lm_area) 

ols_plot_resid_fit(lm_area)
ols_plot_resid_lev(lm_area)
ols_plot_resid_stud(lm_area)
summary(lm_encoded_area)

vif(lm_area)

#removing Landslope and fitting the model again
lm_area <- lm(SalePrice ~  MSSubClass + MSZoning + LotFrontage + LotArea + Street + Alley + LotShape + LandContour + LotConfig, data = df_house_price)
summary(lm_area) 
vif(lm_area)

#Trying with new Feature 'Neighbourhood'
lm_area <- lm(SalePrice ~  MSSubClass + MSZoning + LotFrontage + LotArea + Street + Alley + LotShape + LandContour + LotConfig + Neighborhood, data = df_house_price)
summary(lm_area) 
#We can observe that the r-square has drastically increased

#Plotting Residuals plot
ols_plot_resid_stud(lm_area)
#We can observe quiet a few outliers

#We will try and compare the r-squared by grouping features.


#grouping for house features

#Grouping for Quality
lm_house_quality <- lm(SalePrice ~ OverallQual + ExterQual + BsmtQual + HeatingQC + KitchenQual + FireplaceQu + GarageQual + PoolQC, data = df_house_price)
summary(lm_house_quality)
#the pvalues show more significance towards OverallQual and ExterQual

#checking VIF
vif(lm_house_quality)
#Vif tends to fall the in sufficient tolerance for OverallQual and ExterQual

#fitting the model with these two features
lm_house_quality <- lm(SalePrice ~ OverallQual + ExterQual, data = df_house_price)
summary(lm_house_quality)
#we observe a minimal drop in the r-squared.



#Grouping for condition
lm_condition <- lm(SalePrice ~ OverallCond + ExterCond + BsmtCond + GarageCond, data = df_house_price)
summary(lm_condition)

vif(lm_condition)

# the r -squared value which we are getting witht his model is very low. This indicates that these features have less impact on the SalePrice.


#Grouping for Property info
lm_prop_info <- lm(SalePrice ~ HouseStyle + YearBuilt + YearRemodAdd + RoofStyle + MasVnrArea + Foundation + TotalBsmtSF + Fireplaces + GarageArea + MiscFeature, data = df_house_price )
summary(lm_prop_info)

vif(lm_prop_info)


#fitting the model without the least significant variables
lm_prop_info <- lm(SalePrice ~  YrSold + Neighborhood + YearRemodAdd  + MasVnrArea + TotalBsmtSF + Fireplaces + GarageArea + OverallQual + ExterQual , data = df_house_price )
summary(lm_prop_info)
#We have ended up with a very good r-squared, however we will still try to find features which can increase this more.




#Grouping for Sale conditions
lm_sale_cond <- lm(SalePrice ~ MiscVal + MoSold + YrSold + SaleType + SaleCondition, data = df_house_price)
summary(lm_sale_cond)
#r-square is very low



#Grouping for Porch tyoe and Area
lm_porch_area <- lm(SalePrice ~ LotArea + Street + WoodDeckSF + OpenPorchSF + ScreenPorch , data = df_house_price)
summary(lm_porch_area)
#r-squared is too low 


#Grouping by rooms
lm_house_rooms <- lm(SalePrice ~  LowQualFinSF + GrLivArea + BsmtFullBath + FullBath + HalfBath  + BedroomAbvGr + TotRmsAbvGrd + Functional, data = df_house_price )
summary(lm_house_rooms)

#removing least significant variables and check p-values again
lm_house_rooms <- lm(SalePrice ~  LowQualFinSF + GrLivArea + BsmtFullBath + FullBath + BedroomAbvGr  , data = df_house_price )
summary(lm_house_rooms)

vif(lm_house_rooms)
#getting a substantial r-squared value 



#Hence, using variables from this model in our property info model
lm_prop_info <- lm(SalePrice ~  Neighborhood + YearRemodAdd + TotalBsmtSF + Fireplaces + GarageArea + OverallQual + ExterQual + GrLivArea   , data = df_house_price )
summary(lm_prop_info)
vif
(lm_prop_info)

#able to get an r-squared of 0.8208, now checking for any outliers using Residuals plot
ols_plot_resid_fit(lm_prop_info)
ols_plot_resid_stud(lm_prop_info)


#removing extreme outliers at 1299 and 1325 index
lm_prop_info <- lm(SalePrice ~  Neighborhood + YearRemodAdd + TotalBsmtSF + Fireplaces + GarageArea + OverallQual + ExterQual + GrLivArea   , data = df_house_price[-c(1299, 1325), ] )
summary(lm_prop_info)
#able to get an r-squared of 0.8483 after removing the extreme outliers, now checking for any outliers using Residuals plot
ols_plot_resid_fit(lm_prop_info)
ols_plot_resid_stud(lm_prop_info)

#Now checking while removing the extreme outlier at 524
lm_prop_info <- lm(SalePrice ~  Neighborhood + YearRemodAdd + TotalBsmtSF + Fireplaces + GarageArea + OverallQual + ExterQual + GrLivArea   , data = df_house_price[-c(1299, 1325, 524), ] )
summary(lm_prop_info)
#able to get an r-squared of 0.8612 after removing the extreme outliers, now checking for any outliers using Residuals plot
ols_plot_resid_fit(lm_prop_info)
ols_plot_resid_stud(lm_prop_info)

#Finally checking while removing the extreme outliers at 1169 898 1046 581
lm_prop_info <- lm(SalePrice ~  Neighborhood + YearRemodAdd + TotalBsmtSF + Fireplaces + GarageArea + OverallQual + ExterQual + GrLivArea   , data = df_house_price[-c(1325, 1299 ,1169, 1166,1046,  898,581, 524  ), ] )
summary(lm_prop_info)
#able to get an r-squared of 0.8613 after removing the extreme outliers, now checking for any outliers using Residuals plot
ols_plot_resid_fit(lm_prop_info)
ols_plot_resid_stud(lm_prop_info)
ols_plot_cooksd_chart(lm_prop_info)
ols_plot_dffits(lm_prop_info)
ols_plot_hadi(lm_prop_info)

#Removing these outlier didn't affect the r-squared. Our Final r-squared remains = 0.8613

test <- read.csv("test.csv", header = TRUE)
test$Alley[is.na(test$Alley)] <- "No alley access"
test$MiscFeature[is.na(test$MiscFeature)] <- "None"
test$Fence[is.na(test$Fence)] <- "No Fence"
test$PoolQC[is.na(test$PoolQC)] <- "No Pool"
test$FireplaceQu[is.na(test$FireplaceQu)] <- "No Fireplace"
test$GarageType[is.na(test$GarageType)] <- "No Garage"
test$GarageFinish[is.na(test$GarageFinish)] <- "No Garage"
test$GarageQual[is.na(test$GarageQual)] <- "No Garage"
test$GarageCond[is.na(test$GarageCond)] <- "No Garage"
test$BsmtQual[is.na(test$BsmtQual)] <- "No Basement"
test$BsmtCond[is.na(test$BsmtCond)] <- "No Basement"
test$BsmtExposure[is.na(test$BsmtExposure)] <- "No Basement"
test$BsmtFinType1[is.na(test$BsmtFinType1)] <- "No Basement"
test$BsmtFinType2[is.na(test$BsmtFinType2)] <- "No Basement"
test$LotFrontage[is.na(test$LotFrontage)] <- 0
view(test)

#Print the predictions of the test data
predictions <- predict(lm_prop_info, test, interval = "prediction")
print(predictions)

#save the predictions as a csv file
write.csv(predictions, "predictions.csv")
