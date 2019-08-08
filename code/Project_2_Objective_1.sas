/* Tom Gianelle - Project 2 - Q1 */

FILENAME REFFILE '/home/tomgianelle0/Stats 2/Project 2 - Logistic Regression/framingham.xlsx';
 

PROC IMPORT DATAFILE=REFFILE
	DBMS=XLSX
	OUT=CHD;
	GETNAMES=YES;
RUN;

proc print data=CHD;
run;

/* Q1 - EDA  */



data CHD_clean;
SET CHD;
run;


/* Clean the data replace missing values with mean or median*/
proc hpimpute data = CHD out = CHD_clean;
input education cigsPerDay BPMeds totChol BMI heartRate glucose;
impute education / method=pmedian;
impute cigsPerDay / method=pmedian;
impute BPMeds / method=pmedian;
impute totChol / method=mean;
impute BMI / method=mean;
impute heartRate / method=mean;
impute glucose / method=mean;
ID male	age	currentSmoker prevalentStroke prevalentHyp diabetes sysBP diaBP TenYearCHD;


/* Evaluate CHD out data*/
Proc freq data=CHD_clean;
run;


/* NOTE --- Glucose has a high number of missing values.   Recommend excluding from study  */

/* simple logistic regression  */
proc logistic data=CHD_clean;
class male IM_education currentSmoker  IM_BPMeds prevalentStroke prevalentHyp diabetes/ param=ref;
model TenYearCHD(event='1')= male age IM_education currentSmoker IM_cigsPerDay IM_BPMeds prevalentStroke prevalentHyp diabetes IM_totChol sysBP diaBP IM_BMI IM_heartRate IM_glucose/ scale=none aggregate lackfit influence;
effectplot;
effectplot slicefit(sliceby=male plotby=IM_BPMeds) / noobs;
run;

/* LASSO logistic regression  --- excluding glucose because 20% of rows missing values*/
PROC HPGENSELECT Data=CHD_clean
LASSORHO=.95 /* default */ LASSOSTEPS=100; /* default */
class male IM_education currentSmoker  IM_BPMeds prevalentStroke prevalentHyp diabetes/ param=ref;
model TenYearCHD(event='1')= male age IM_education currentSmoker IM_cigsPerDay IM_BPMeds prevalentStroke prevalentHyp diabetes IM_totChol sysBP diaBP IM_BMI IM_heartRate / DISTRIBUTION=BINARY;
 SELECTION METHOD=LASSO (CHOOSE=SBC STOP=NONE) DETAILS=ALL;
run;

/* FORWARD logistic regression using LASSO selections as response variables --- excluding glucose because 20% of rows missing values */
proc logistic data=CHD_clean;
class male IM_education currentSmoker  IM_BPMeds prevalentStroke prevalentHyp diabetes/ param=ref;
model TenYearCHD(event='1')= age IM_cigsPerDay IM_totChol sysBP diaBP IM_BMI IM_heartRate /  selection=forward scale=none aggregate lackfit influence;
effectplot;
effectplot slicefit(sliceby=male X=age) / noobs;
run;


/* FORWARD logistic regression using LASSO selections as response variables --- excluding glucose because 20% of rows missing values  */
proc logistic data=CHD_clean;
class male IM_education currentSmoker  IM_BPMeds prevalentStroke prevalentHyp diabetes/ param=ref;
model TenYearCHD(event='1')= male age IM_education currentSmoker IM_cigsPerDay IM_BPMeds prevalentStroke prevalentHyp diabetes IM_totChol sysBP diaBP IM_BMI IM_heartRate/  selection=forward scale=none aggregate lackfit influence;
effectplot;
effectplot slicefit(sliceby=male X=age) / noobs;
run;

/*                         */
/*  STAT 1 - Final Project */
/*                         */

data train_clean;
SET train;
logSalePrice = log(SalePrice);
logGrLIvArea = log(GrLIvArea);
run;

/* Adding SalePrice Column To Test Set*/
data test;
set test;
SalePrice = .;
run;

/* Evaluate missing values (train)*/
proc means data=train_clean NMISS N; run;

/* Clean the data (in train: replace missing values with mean)*/
proc hpimpute data = CHD out = CHD_clean;
input MasVnrArea GarageYrBlt;
impute MasVnrArea / method=mean;
impute GarageYrBlt / method=mean;
ID 

/* Reasses missing values (train)*/
proc means data=train_clean NMISS N; run;

/* Appending the test and train set*/
data complete;
set train test;
run;

/* Cleaning the Appended Data set */
proc HPImpute data = complete out = complete_clean;
input MasVnrArea BsmtFinSF1 BsmtFinSF2 BsmtUnfSF TotalBsmtSF BsmtFullBath 
BsmtHalfBath GarageYrBlt GarageCars GarageArea;
impute MasVnrArea / method=mean;
impute BsmtFinSF1 / method=mean;
impute BsmtFinSF2 / method=mean;
impute BsmtUnfSF / method=mean;
impute TotalBsmtSF / method=mean;
impute BsmtFullBath / method=mean;
impute BsmtHalfBath / method=mean;
impute GarageYrBlt / method=mean;
impute GarageCars / method=mean;
impute GarageArea / method=mean;
ID ID SalePrice MsSubClass MSZoning Street Alley LotShape LandContour Utilities LotConfig LotFrontage LandSlope Neighborhood Condition1 Condition2
BldgType HouseStyle RoofStyle RoofMatl Exterior1st Exterior2nd ExterQual ExterCond Foundation BsmtQual BsmtCond BsmtExposure BsmtFinType1 BsmtFinType2
Heating HeatingQC CentralAir Electrical KitchenQual Functional FireplaceQu GarageType GarageFinish GarageQual GarageCond PavedDrive PoolQC Fence MiscFeature
SaleType SaleCondition LotArea OverallQual OverallCond YearBuilt YearRemodAdd FirstFloorSF SecondFloorSF LowQualFinSF GrLivArea FullBath HalfBath BedroomAbvGr 
KitchenAbvGr TotRmsAbvGrd Fireplaces WoodDeckSF OpenPorchSF EnclosedPorch ThreeSsnPorch ScreenPorch PoolArea MiscVal MoSold YrSold MsSubClass
MSZoning Street Alley LotShape LandContour Utilities LotConfig LotFrontage LandSlope Neighborhood Condition1 Condition2 BldgType HouseStyle
RoofStyle RoofMatl Exterior1st Exterior2nd ExterQual ExterCond Foundation BsmtQual BsmtCond BsmtExposure BsmtFinType1 BsmtFinType2 Heating
HeatingQC CentralAir Electrical KitchenQual Functional FireplaceQu GarageType GarageFinish GarageQual GarageCond PavedDrive PoolQC Fence MiscFeature SaleType SaleCondition;
proc means data = complete_clean mean; run;

/* Forward Selection (All  Variables from Training Data Set) */
proc glmselect data = train_clean seed=555;
class MsSubClass MSZoning Street Alley LotShape LandContour Utilities LotConfig LotFrontage LandSlope Neighborhood Condition1 Condition2 
BldgType HouseStyle RoofStyle RoofMatl Exterior1st Exterior2nd ExterQual ExterCond Foundation BsmtQual BsmtCond BsmtExposure 
BsmtFinType1 BsmtFinType2 Heating HeatingQC CentralAir Electrical KitchenQual Functional FireplaceQu GarageType GarageFinish
GarageQual GarageCond PavedDrive PoolQC Fence MiscFeature SaleType SaleCondition;
model SalePrice = LotArea OverallQual OverallCond YearBuilt YearRemodAdd M_MasVnrArea BsmtFinSF1 BsmtFinSF2
BsmtUnfSF TotalBsmtSF FirstFloorSF SecondFloorSF LowQualFinSF GrLivArea BsmtFullBathBsmtHalfBath FullBath HalfBath
BedroomAbvGr KitchenAbvGr TotRmsAbvGrd Fireplaces M_GarageYrBlt GarageCars GarageArea WoodDeckSF OpenPorchSF 
EnclosedPorch ThreeSsnPorch ScreenPorch PoolArea MiscVal MoSold YrSoldMsSubClass MSZoning Street Alley LotShape
LandContour Utilities LotConfig LotFrontage LandSlope Neighborhood Condition1 Condition2 BldgType HouseStyle
RoofStyle RoofMatl Exterior1st Exterior2nd ExterQual ExterCond Foundation BsmtQual BsmtCond BsmtExposure
BsmtFinType1 BsmtFinType2 Heating HeatingQC CentralAir Electrical KitchenQual Functional FireplaceQu GarageType
GarageFinish GarageQual GarageCond PavedDrive PoolQC Fence MiscFeature SaleType SaleCondition / selection = forward (select=cv choose=cv stop=cv) CVDETAILS;
run;

/* 2 A */

/* Backward Selection (All  Variables from Training Data Set) */
proc glmselect data = train_clean seed=333;
class MsSubClass MSZoning Street Alley LotShape LandContour Utilities LotConfig 
LotFrontage LandSlope Neighborhood Condition1 Condition2 BldgType HouseStyle RoofStyle RoofMatl Exterior1st Exterior2nd ExterQual ExterCond 
Foundation BsmtQual BsmtCond BsmtExposure BsmtFinType1 BsmtFinType2 Heating HeatingQC CentralAir Electrical KitchenQual 
Functional FireplaceQu GarageType GarageFinish GarageQual GarageCond PavedDrive PoolQC Fence MiscFeature SaleType SaleCondition;
model SalePrice = LotArea OverallQual OverallCond YearBuilt YearRemodAdd M_MasVnrArea BsmtFinSF1 BsmtFinSF2
BsmtUnfSF TotalBsmtSF FirstFloorSF SecondFloorSF LowQualFinSF GrLivArea BsmtFullBath BsmtHalfBath FullBath HalfBath BedroomAbvGr KitchenAbvGr TotRmsAbvGrd 
Fireplaces M_GarageYrBlt GarageCars GarageArea WoodDeckSF OpenPorchSF EnclosedPorch ThreeSsnPorch ScreenPorch PoolArea MiscVal MoSold YrSold
MsSubClass MSZoning Street Alley LotShape LandContour Utilities LotConfig LotFrontage LandSlope Neighborhood Condition1 Condition2 
BldgType HouseStyle RoofStyle RoofMatl Exterior1st Exterior2nd ExterQual ExterCond Foundation BsmtQual BsmtCond BsmtExposure 
BsmtFinType1 BsmtFinType2 Heating HeatingQC CentralAir Electrical KitchenQual Functional FireplaceQu GarageType GarageFinish 
GarageQual GarageCond PavedDrive PoolQC Fence MiscFeature SaleType SaleCondition / selection = backward (select=cv choose=cv stop=cv) CVDETAILS;
run;

/* 2 C */

/* StepWise Selection (All  Variables from Training Data Set) */
proc glmselect data = train_clean seed=333;
class MsSubClass MSZoning Street Alley LotShape LandContour Utilities LotConfig LotFrontage LandSlope Neighborhood Condition1 Condition2 
BldgType HouseStyle RoofStyle RoofMatl Exterior1st Exterior2nd ExterQual ExterCond Foundation BsmtQual BsmtCond BsmtExposure 
BsmtFinType1 BsmtFinType2 Heating HeatingQC CentralAir Electrical KitchenQual Functional FireplaceQu GarageType GarageFinish 
GarageQual GarageCond PavedDrive PoolQC Fence MiscFeature SaleType SaleCondition;
model SalePrice = LotArea OverallQual OverallCond YearBuilt YearRemodAdd M_MasVnrArea BsmtFinSF1 BsmtFinSF2
BsmtUnfSF TotalBsmtSF FirstFloorSF _SecondFloorSF LowQualFinSF GrLivArea BsmtFullBath BsmtHalfBath FullBath
HalfBath BedroomAbvGr KitchenAbvGr TotRmsAbvGrd Fireplaces M_GarageYrBlt GarageCars GarageArea WoodDeckSF OpenPorchSF 
EnclosedPorch ThreeSsnPorch ScreenPorch PoolArea MiscVal MoSold YrSold MsSubClass MSZoning Street Alley LotShape LandContour Utilities LotConfig LotFrontage
LandSlope Neighborhood Condition1 Condition2BldgType HouseStyle RoofStyle RoofMatl Exterior1st Exterior2nd ExterQual ExterCond Foundation BsmtQual BsmtCond BsmtExposure 
BsmtFinType1 BsmtFinType2 Heating HeatingQC CentralAir Electrical KitchenQual Functional FireplaceQu GarageType GarageFinish 
GarageQual GarageCond PavedDrive PoolQC Fence MiscFeature SaleType SaleCondition / selection = stepwise (select=cv choose=cv stop=cv) CVDETAILS;
run;

/* 2 D */
/* Analysis 2: Submitting Kaggle Scores */


/* Using Forward Selection Model for SalePrice Prediction/Kaggle Submission */
proc glm data=complete_clean plots(unpack)=all;
class LandContour Neighborhood RoofMatl BsmtQual BsmtExposure BsmtFinType1 KitchenQual;
model SalePrice= MsSubClass LandContour Neighborhood RoofMatl BsmtQual BsmtExposure BsmtFinType1 KitchenQual
OverallQual GrLivArea IM_GarageCars OverallCond YearBuilt LotArea ScreenPorch / cli solution;
/* Output the Results */
output out= results_f p=Predict; 
run;
proc print data=results_f; run;

/* Creating CSV file with predictions */
data results_f2;
set results_f;
if Predict > 0 then SalePrice = Predict;
/* Set any intelligent guess for the SalePrice */
if Predict < 0 then SalePrice = 181000; 
/* Retain only the SalePrice */
keep ID SalePrice; 
/* Separate only values that appear in the test data */
where ID > 1460; 
run;
proc print data=results_f2;run;

/* Exporting CSV File for Submission */
proc export data=results_f2 DBMS=csv outfile="/folders/myfolders/sasuser.v94/results_f2.csv"; run;

/* Using Backward Elimination Model for SalePrice Prediction/Kaggle Submission */
proc glm data=complete_clean;
class MsSubClass MSZoning Street Alley LotShape LandContour Utilities LotConfig LotFrontage LandSlope Neighborhood Condition1 Condition2 
BldgType HouseStyle RoofStyle RoofMatl Exterior1st Exterior2nd ExterQual ExterCond 
Foundation BsmtQual BsmtCond BsmtExposure BsmtFinType1 BsmtFinType2 Heating HeatingQC CentralAir Electrical KitchenQual
Functional FireplaceQu GarageType GarageFinish GarageQual GarageCond PavedDrive PoolQC Fence MiscFeature SaleType SaleCondition;
model SalePrice = LotArea OverallQual OverallCond YearBuilt YearRemodAdd IM_MasVnrArea IM_BsmtFinSF1 IM_BsmtFinSF2
IM_BsmtUnfSF FirstFloorSF SecondFloorSF LowQualFinSF IM_BsmtFullBath IM_BsmtHalfBath FullBath HalfBath BedroomAbvGr KitchenAbvGr TotRmsAbvGrd 
Fireplaces IM_GarageYrBlt IM_GarageCars IM_GarageArea WoodDeckSF OpenPorchSF EnclosedPorch ThreeSsnPorch ScreenPorch PoolArea MiscVal MoSold YrSold
MsSubClass MSZoning Street Alley LotShape LandContour Utilities LotConfig LotFrontage LandSlope Neighborhood Condition1 Condition2 
BldgType HouseStyle RoofStyle RoofMatl Exterior1st Exterior2nd ExterQual ExterCond Foundation BsmtQual BsmtCond BsmtExposure 
BsmtFinType1 BsmtFinType2 Heating HeatingQC CentralAir Electrical KitchenQual Functional FireplaceQu GarageType GarageFinish 
GarageQual GarageCond PavedDrive PoolQC Fence MiscFeature SaleType SaleCondition / cli solution;
/* Output the Results */
output out= results_b p=Predict;
run;

/* Create CSV File with Predictions */
proc print data=results_b; run;

data results_b2;
set results_b;
if Predict > 0 then SalePrice = Predict;
/* Set any intelligent guess for the SalePrice */
if Predict < 0 then SalePrice = 181000; 
/* Retain only the SalePrice */
keep ID SalePrice; 
/* Separate only values that appear in the test data */
where ID > 1460; 
run;
proc print data=results_b2; run;

/* Exporting CSV File for Submission */
proc export data=results_b2 DBMS=csv outfile="/folders/myfolders/sasuser.v94/results_b2.csv"; run;

/* Predictions from Stepwise Elimination Model */
proc glm data=complete_clean;
class MsSubClass LandContour LotConfig Neighborhood Condition1 BldgType Foundation BsmtQual BsmtExposure BsmtFinType1 KitchenQual Functional Fireplaces GarageQual GarageCond PavedDrive SaleType GarageFinish;
model SalePrice = OverallQual GrLivArea OverallCond YearBuilt YearRemodAdd IM_BsmtFullBath FullBath HalfBath KitchenAbvGr Fireplaces IM_GarageCars WoodDeckSF ScreenPorch MsSubClass LandContour LotConfig Neighborhood Condition1 BldgType BsmtQual BsmtExposure BsmtFinType1 KitchenQual Functional EnclosedPorch GarageFinish GarageQual PavedDrive SaleType / cli solution;
/* Output the Results */
output out= results_sw p=Predict; 
run;
proc print data=results_sw; run;

/* creating CSV file with predictions */
data results_sw2;
set results_sw;
if Predict > 0 then SalePrice = Predict;
/* Set any intelligent guess for the SalePrice */
if Predict < 0 then SalePrice = 181000; 
/* Retain only the SalePrice */
keep ID SalePrice; 
/* Separate only values that appear in the test data */
where ID > 1460; 
run;

/* Exporting CSV File for Submission */
proc print data=results_sw2; run;
proc export data=results_sw2 DBMS=csv outfile="/folders/myfolders/sasuser.v94/results_sw2.csv";run;

/* Analysis 2: Final Model (Using Forward Pred. Model) */

/* Predictions from Forward Selection Model */
/* before transform (Matrix Scatterplot)*/
proc sgscatter data=complete_clean;
 title "Scatterplot Matrix for Forward Predictive Model (No Transform)";
 matrix SalePrice MsSubClass OverallQual GrLivArea IM_GarageCars OverallCond YearBuilt LotArea ScreenPorch;
 run;
       
/*Proc GLM before transform */
proc glm data=complete_clean plots(unpack)=all;
class LandContour Neighborhood RoofMatl BsmtQual BsmtExposure BsmtFinType1 KitchenQual;
model SalePrice = MsSubClass LandContour Neighborhood RoofMatl BsmtQual BsmtExposure BsmtFinType1 KitchenQual OverallQual GrLivArea IM_GarageCars OverallCond YearBuilt LotArea ScreenPorch / cli solution;
/* Output the Results After Transformation (Log-Log)  */
output out= results_f p=Predict; run;

/* 2E */

data complete_clean;
set complete_clean;
logSalePrice =log(SalePrice);
logMsSubClass = log(MsSubClass);
logOverallQual = log(OverallQual);
logGrLivArea = log(GrLivArea);
logIM_GarageCars = log(IM_GarageCars);
logOverallCond = log(OverallCond);
logYearBuilt = log(YearBuilt);
logLotArea = log(LotArea);
logScreenPorch = log(ScreenPorch);
run;

/* Scatterplot Matrix After Log-Log Transform*/
proc sgscatter data=complete_clean;
title "Scatterplot Matrix for Forward Predictive Model(Log-Log)";
matrix logSalePrice logMsSubClass logOverallQual logGrLivArea logIM_GarageCars logOverallCond logYearBuilt logLotArea logScreenPorch;
run;

/* Final model after Log-Log Transform */
proc glm data=complete_clean plots=all;
class LandContour Neighborhood RoofMatl BsmtQual BsmtExposure BsmtFinType1 KitchenQual;
model logSalePrice = logMsSubClass LandContour Neighborhood RoofMatl BsmtQual BsmtExposure BsmtFinType1 KitchenQual logOverallQual logGrLivArea logIM_GarageCars logOverallCond logYearBuilt logLotArea logScreenPorch / cli solution;
run;

proc glm data=complete_clean plots=all;
class LandContour Neighborhood RoofMatl BsmtQual BsmtExposure BsmtFinType1 KitchenQual;
model logSalePrice = logMsSubClass LandContour Neighborhood RoofMatl BsmtQual BsmtExposure BsmtFinType1 KitchenQual logOverallQual logGrLivArea logIM_GarageCars logOverallCond logYearBuilt logLotArea logScreenPorch / cli solution;
run;

/* Predictions from Forward Selection Model (Our Best Predictive Model) */
/* before transform (Matrix Scatterplot) */

proc sgscatter data=complete_clean;
title "Scatterplot Matrix for Forward Predictive Model (No Transform)";
matrix SalePrice MsSubClass OverallQual GrLivArea IM_GarageCars OverallCond YearBuilt LotArea ScreenPorch;
run;
proc print data=results_f; run;

/*  from video -- need to fix alpha and numeric in same columns */


data results;
set test;
SalePrice = .;
run;


data train2_clean;
set train_clean;
logSalePrice =log(SalePrice);
run;

/* Scatterplot Matrix After Log-Log Transform*/
proc sgscatter data=train2_clean;
title "Scatterplot Matrix for custom model";
matrix logSalePrice GrLivArea  OverallQual OverallCond LotArea;
run;


/* custom model */
proc glm data = train2_clean plots=all;
  class LotArea  GarageType GarageFinish GarageQual; 
  Model logSalePrice = GrLivArea  OverallQual OverallCond LotArea  GarageType GarageFinish GarageQual   / cli solution;
  Output out = results p = predict;
run;

data results2;
set results;
if Predict > 0 then SalePrice = Predict;
if Predict <= 0 then SalePrice = 10000;
keep id SalePrice;
where id > 1460;

Proc means data = results2;
  var SalePrice;
run;

/* Exporting Custom CSV File for Submission */
proc print data=results2; run;
proc export data=results2 DBMS=csv outfile="/home/tomgianelle0/Project 3/results_c.csv";run;

/*  data CHD_out;
SET CHD_clean;
male = male;
age = age;
education = IM_education;
currentSmoker = currentSmoker;
cigsPerDay = IM_cigsPerDay;
BPMeds = IM|_BPMeds;
prevalentStroke = prevalentStroke;
prevalentHyp = prevalentHyp;
diabetes = diabetes;
totChol = IM_totChol;
sysBP = sysBP;
diaBP = diaBP;
BMI = IM_BMI;
heartRate = IM_heartRate;
glucose = IM_glucose;
TenYearCHD = TenYearCHD;
run;
*/
  
  
  