##Load data
library(caret)
library(tidyverse)

train <- read.csv("train.csv")



# Check which columns have NAs
na_cols <- colSums(is.na(train)) > 0
na_cols_names <- names(na_cols[na_cols])
na_cols_names
train<-train %>% dplyr::select(-na_cols_names)


## Split train and cross validation set
library(MASS)
data(train)
n= dim(train)[1]
set.seed(1251)
train.index = sample(n, 0.8*n)
train = train[train.index, ]
train_V = train[-train.index, ]
Actuals = train_V$SalePrice
train_V <-train_V %>% dplyr::select(-SalePrice)

#Summary of Final Price
sd(Actuals)
summary(Actuals)
cat("Standard deviation: ", sd(Actuals))
var(Actuals)

##1 Histograms for each variable

#Saleprice
train$SalePrice <- as.numeric(train$SalePrice)
options(scipen=999)
hist(train$SalePrice, main = "Sale Price Distribution", xlab = "Sale Price", ylab = "Frequency", breaks = 20)

# LotArea
hist(train$LotArea, main = "LotArea Distribution", xlab = "LotArea", ylab = "Frequency", breaks = 20)

# OverallQual
hist(train$OverallQual, main = "OverallQual Distribution", xlab = "OverallQual", ylab = "Frequency", breaks = 10)

# OverallCond
hist(train$OverallCond, main = "OverallCond Distribution", xlab = "OverallCond", ylab = "Frequency", breaks = 10)

# YearBuilt
hist(train$YearBuilt, main = "YearBuilt Distribution", xlab = "YearBuilt", ylab = "Frequency", breaks = 20)

# Histogram for YearRemodAdd
hist(train$YearRemodAdd, main = "Year Remodeled Distribution", xlab = "Year Remodeled", ylab = "Frequency", breaks = 20)

# Histogram for BsmtFinSF1
hist(train$BsmtFinSF1, main = "Type 1 Finished Basement Area Distribution", xlab = "Type 1 Finished Basement Area", ylab = "Frequency", breaks = 20)

# Histogram for BsmtFinSF2
hist(train$BsmtFinSF2, main = "Type 2 Finished Basement Area Distribution", xlab = "Type 2 Finished Basement Area", ylab = "Frequency", breaks = 20)

# Histogram for BsmtUnfSF
hist(train$BsmtUnfSF, main = "Unfinished Basement Area Distribution", xlab = "Unfinished Basement Area", ylab = "Frequency", breaks = 20)

# Histogram for TotalBsmtSF
hist(train$TotalBsmtSF, main = "Total Basement Area Distribution", xlab = "Total Basement Area", ylab = "Frequency", breaks = 20)

# Histogram for 1stFlrSF
hist(train$X1stFlrSF, main = "First Floor Area Distribution", xlab = "First Floor Area", ylab = "Frequency", breaks = 20)

hist(train$X2ndFlrSF, main = "Second Floor Square Feet Distribution", xlab = "Square Feet", ylab = "Frequency", breaks = 20)

hist(train$LowQualFinSF, main = "Low Quality Finished Square Feet Distribution", xlab = "Square Feet", ylab = "Frequency", breaks = 20)

hist(train$GrLivArea, main = "Above Ground Living Area Distribution", xlab = "Square Feet", ylab = "Frequency", breaks = 20)

hist(train$BsmtFullBath, main = "Basement Full Bathrooms Distribution", xlab = "Number of Bathrooms", ylab = "Frequency", breaks = 5)

hist(train$BsmtHalfBath, main = "Basement Half Bathrooms Distribution", xlab = "Number of Bathrooms", ylab = "Frequency", breaks = 5)

hist(train$FullBath, main = "Full Bathrooms Above Grade Distribution", xlab = "Number of Bathrooms", ylab = "Frequency", breaks = 5)

hist(train$HalfBath, main = "Half Baths Above Grade Distribution", xlab = "Number of Baths", ylab = "Frequency", breaks = 5)

hist(train$Bedroom, main = "Number of Bedrooms Above Basement Level Distribution", xlab = "Number of Bedrooms", ylab = "Frequency", breaks = 10)

hist(train$KitchenAbvGr, main = "Number of Kitchens Distribution", xlab = "Number of Kitchens", ylab = "Frequency", breaks = 5)

hist(train$TotRmsAbvGrd, main = "Total Rooms Above Grade Distribution", xlab = "Number of Rooms", ylab = "Frequency", breaks = 10)

hist(train$Fireplaces, main = "Number of Fireplaces Distribution", xlab = "Number of Fireplaces", ylab = "Frequency", breaks = 5)

hist(train$GarageCars, main = "Garage Car Capacity Distribution", xlab = "Number of Cars", ylab = "Frequency", breaks = 5)

hist(train$GarageArea, main = "Garage Area Distribution", xlab = "Square Feet", ylab = "Frequency", breaks = 20)

hist(train$WoodDeckSF, main = "Wood Deck Area Distribution", xlab = "Square Feet", ylab = "Frequency", breaks = 20)

hist(train$OpenPorchSF, main = "Open Porch Area Distribution", xlab = "Square Feet", ylab = "Frequency", breaks = 20)

hist(train$EnclosedPorch, main = "Enclosed Porch Area Distribution", xlab = "Square Feet", ylab = "Frequency", breaks = 20)

hist(train$X3SsnPorch, main = "Three Season Porch Area Distribution", xlab = "Square Feet", ylab = "Frequency", breaks = 20)

hist(train$ScreenPorch, main = "Screen Porch Area Distribution", xlab = "Square Feet", ylab = "Frequency", breaks = 20)

hist(train$PoolArea, main = "Pool Area Distribution", xlab = "Square Feet", ylab = "Frequency", breaks = 20)

hist(train$MiscVal, main = "Miscellaneous Feature Value Distribution", xlab = "MiscVal", ylab = "Frequency", breaks = 20)



##2 Frequency of different levels, pie charts pie4
#Frequency of MSZoning
freq_table <- table(train$MSZoning)
pie(freq_table, labels = names(freq_table), main = "Frequency of MSZoning Categories")

# Frequency of MSSubClass
freq_table2 <- table(train$MSSubClass)
pie(freq_table2, labels = names(freq_table2), main = "Frequency of MSSubclass Categories")

# Frequency of Street
freq_table3 <- table(train$Street)
pie(freq_table3, labels = names(freq_table3), main = "Frequency of Street Categories")

# Frequency of LotShape
freq_table4 <- table(train$LotShape)
pie(freq_table4, labels = names(freq_table4), main = "Frequency of LotShape Categories")

# Frequency of LandContour
freq_table6 <- table(train$LandContour)
pie(freq_table6, labels = names(freq_table6), main = "Frequency of LandContour Categories")

# Frequency of Utilities
freq_table7 <- table(train$Utilities)
pie(freq_table7, labels = names(freq_table7), main = "Frequency of Utilities Categories")

# Frequency of LotConfig
freq_table8 <- table(train$LotConfig)
pie(freq_table8, labels = names(freq_table8), main = "Frequency of LotConfig Categories")

# Frequency of LandSlope
freq_table9 <- table(train$LandSlope)
pie(freq_table9, labels = names(freq_table9), main = "Frequency of LandSlope Categories")

# Frequency of Neighborhood
freq_table10 <- table(train$Neighborhood)
pie(freq_table10, labels = names(freq_table10), main = "Frequency of Neighborhood Categories")

# Frequency of Condition1
freq_table11 <- table(train$Condition1)
pie(freq_table, labels = names(freq_table11), main = "Frequency of Condition1 Categories")

# Frequency of Condition2
freq_table12 <- table(train$Condition2)
pie(freq_table, labels = names(freq_table12), main = "Frequency of Condition2 Categories")

# Frequency of BldgType
freq_table13 <- table(train$BldgType)
pie(freq_table13, labels = names(freq_table13), main = "Frequency of BldgType Categories")

# Frequency of HouseStyle
table(train$HouseStyle)
pie(table(train$HouseStyle), main="House Style")

# Frequency of RoofStyle
table(train$RoofStyle)
pie(table(train$RoofStyle), main="Roof Style")

# Frequency of RoofMatl
table(train$RoofMatl)
pie(table(train$RoofMatl), main="Roof Material")

# Frequency of Exterior1st
table(train$Exterior1st)
pie(table(train$Exterior1st), main="Exterior Covering (1st)")

# Frequency of Exterior2nd
table(train$Exterior2nd)
pie(table(train$Exterior2nd), main="Exterior Covering (2nd)")

# Frequency of ExterQual
table(train$ExterQual)
pie(table(train$ExterQual), main="Exterior Material Quality")

# Frequency of ExterCond
table(train$ExterCond)
pie(table(train$ExterCond), main="Exterior Material Condition")

# Frequency of Foundation
table(train$Foundation)
pie(table(train$Foundation), main="Foundation Type")

# Frequency of heating
heating_freq <- table(train$Heating)
pie(heating_freq, main="Heating Type")

# Frequency of HeatingQC
heatingqc_freq <- table(train$HeatingQC)
pie(heatingqc_freq, main="Heating Quality and Condition")

# Frequency of CentralAir
centralair_freq <- table(train$CentralAir)
pie(centralair_freq, main="Central Air Conditioning")

# Frequency of KitchenQual
kitchenqual_freq <- table(train$KitchenQual)
pie(kitchenqual_freq, main="Kitchen Quality")

# Frequency of Functional
functional_freq <- table(train$Functional)
pie(functional_freq, main="Home Functionality Rating")

# Frequency of PavedDrive
paveddrive_freq <- table(train$PavedDrive)
pie(paveddrive_freq, main="Paved Driveway")

# Frequency of SaleType
saletype_freq <- table(train$SaleType)
pie(saletype_freq, main="Type of Sale")

# Frequency of SaleCondition
salecondition_freq <- table(train$SaleCondition)
pie(salecondition_freq, main="Condition of Sale")


##3 Relationships between Sale Price and each predictor variables
library(tidyverse)

# Select the quantitative predictor variables
quant_vars <- c("LotArea","OverallQual", "OverallCond", "YearBuilt", "YearRemodAdd","BsmtFinSF1","BsmtFinSF2","BsmtUnfSF", "TotalBsmtSF", "X1stFlrSF","X2ndFlrSF","LowQualFinSF", "GrLivArea","BsmtFullBath","BsmtHalfBath", "FullBath", "HalfBath", "BedroomAbvGr", "KitchenAbvGr", "TotRmsAbvGrd", "Fireplaces","GarageCars", "GarageArea", "WoodDeckSF", "OpenPorchSF", "EnclosedPorch", "X3SsnPorch", "ScreenPorch", "PoolArea", "MiscVal")
print(quant_vars)
library(dplyr)
library(corrplot)

# Create a correlation matrix
corr_matrix <- cor(train[, quant_vars])

# Visualize the correlation matrix using corrplot package
library(corrplot)
corrplot(corr_matrix, method = "color", type = "lower", order = "hclust", tl.col = "black", tl.cex = 0.5)



##4 Create scatter plots for each quantitative predictor variable
library(ggplot2)


# Create the scatter plot

ggplot(train, aes(x = LotArea, y = SalePrice)) +
  geom_point(alpha = 0.5) +
  labs(title = "Scatter Plot of Sale Price vs. Lot Area",
       x = "Lot Area", y = "Sale Price") +
  theme_bw()

# OverallQual vs. SalePrice
ggplot(train, aes(x = OverallQual, y = SalePrice)) +
  geom_point(alpha = 0.5) +
  labs(title = "Scatter Plot of Sale Price vs. Overall Quality",
       x = "Overall Quality", y = "Sale Price") +
  theme_bw()

# OverallCond vs. SalePrice
ggplot(train, aes(x = OverallCond, y = SalePrice)) +
  geom_point(alpha = 0.5) +
  labs(title = "Scatter Plot of Sale Price vs. Overall Condition",
       x = "Overall Condition", y = "Sale Price") +
  theme_bw()

# YearBuilt vs. SalePrice
ggplot(train, aes(x = YearBuilt, y = SalePrice)) +
  geom_point(alpha = 0.5) +
  labs(title = "Scatter Plot of Sale Price vs. Year Built",
       x = "Year Built", y = "Sale Price") +
  theme_bw()

# YearRemodAdd vs. SalePrice
ggplot(train, aes(x = YearRemodAdd, y = SalePrice)) +
  geom_point(alpha = 0.5) +
  labs(title = "Scatter Plot of Sale Price vs. Year Remodeled",
       x = "Year Remodeled", y = "Sale Price") +
  theme_bw()

# BsmtFinSF1 vs. SalePrice
ggplot(train, aes(x = BsmtFinSF1, y = SalePrice)) +
  geom_point(alpha = 0.5) +
  labs(title = "Scatter Plot of Sale Price vs. Type 1 Finished Basement Area",
       x = "Type 1 Finished Basement Area", y = "Sale Price") +
  theme_bw()

# BsmtFinSF2 vs. SalePrice
ggplot(train, aes(x = BsmtFinSF2, y = SalePrice)) +
  geom_point(alpha = 0.5) +
  labs(title = "Scatter Plot of Sale Price vs. Type 2 Finished Basement Area",
       x = "Type 2 Finished Basement Area", y = "Sale Price") +
  theme_bw()

# BsmtUnfSF vs. SalePrice
ggplot(train, aes(x = BsmtUnfSF, y = SalePrice)) +
  geom_point(alpha = 0.5) +
  labs(title = "Scatter Plot of Sale Price vs. Unfinished Basement Area",
       x = "Unfinished Basement Area", y = "Sale Price") +
  theme_bw()

# TotalBsmtSF vs. SalePrice
ggplot(train, aes(x = TotalBsmtSF, y = SalePrice)) +
  geom_point(alpha = 0.5) +
  labs(title = "Scatter Plot of Sale Price vs. Total Basement Area",
       x = "Total Basement Area", y = "Sale Price") +
  theme_bw()

# 1stFlrSF vs. SalePrice
ggplot(train, aes(x = `X1stFlrSF`, y = SalePrice)) +
  geom_point(alpha = 0.5) +
  labs(title = "Scatter Plot of Sale Price vs. First Floor Area",
       x = "First Floor Area", y = "Sale Price") +
  theme_bw()

# 2ndFlrSF vs. SalePrice
ggplot(train, aes(x = `X2ndFlrSF`, y = SalePrice)) +
  geom_point(alpha = 0.5) +
  labs(title = "Scatter Plot of Sale Price vs. Second Floor Area",
       x = "Second Floor Area", y = "Sale Price") +
  theme_bw()

# LowQualFinSF vs. SalePrice
ggplot(train, aes(x = LowQualFinSF, y = SalePrice)) +
  geom_point(alpha = 0.5) +
  labs(title = "Scatter Plot of Sale Price vs. Low Quality Finished Area",
       x = "Low Quality Finished Area", y = "Sale Price") +
  theme_bw()

# GrLivArea vs. SalePrice
ggplot(train, aes(x = GrLivArea, y = SalePrice)) +
  geom_point(alpha = 0.5) +
  labs(title = "Scatter Plot of Sale Price vs. Above Grade Living Area",
       x = "Above Grade Living Area", y = "Sale Price") +
  theme_bw()

# BsmtFullBath vs. SalePrice
ggplot(train, aes(x = BsmtFullBath, y = SalePrice)) +
  geom_point(alpha = 0.5) +
  labs(title = "Scatter Plot of Sale Price vs. Basement Full Bathrooms",
       x = "Basement Full Bathrooms", y = "Sale Price") +
  theme_bw()

# BsmtHalfBath vs. SalePrice
ggplot(train, aes(x = BsmtHalfBath, y = SalePrice)) +
  geom_point(alpha = 0.5) +
  labs(title = "Scatter Plot of Sale Price vs. Basement Half Bathrooms",
       x = "Basement Half Bathrooms", y = "Sale Price") +
  theme_bw()

# FullBath vs. SalePrice
ggplot(train, aes(x = FullBath, y = SalePrice)) +
  geom_point(alpha = 0.5) +
  labs(title = "Scatter Plot of Sale Price vs. Full Bathrooms Above Grade",
       x = "Full Bathrooms Above Grade", y = "Sale Price") +
  theme_bw()

# HalfBath vs. SalePrice
ggplot(train, aes(x = HalfBath, y = SalePrice)) +
  geom_point(alpha = 0.5) +
  labs(title = "Scatter Plot of Sale Price vs. Half Baths Above Grade",
       x = "Half Baths Above Grade", y = "Sale Price") +
  theme_bw()

# BedroomAbvGr vs. SalePrice
ggplot(train, aes(x = BedroomAbvGr, y = SalePrice)) +
  geom_point(alpha = 0.5) +
  labs(title = "Scatter Plot of Sale Price vs. Bedrooms Above Basement Level",
       x = "Bedrooms Above Basement Level", y = "Sale Price") +
  theme_bw()

# KitchenAbvGr vs. SalePrice
ggplot(train, aes(x = KitchenAbvGr, y = SalePrice)) +
  geom_point(alpha = 0.5) +
  labs(title = "Scatter Plot of Sale Price vs. Number of Kitchens",
       x = "Number of Kitchens", y = "Sale Price") +
  theme_bw()

# TotRmsAbvGrd vs. SalePrice
ggplot(train, aes(x = TotRmsAbvGrd, y = SalePrice)) +
  geom_point(alpha = 0.5) +
  labs(title = "Scatter Plot of Sale Price vs. Total Rooms Above Grade",
       x = "Total Rooms Above Grade", y = "Sale Price") +
  theme_bw()

# Fireplaces vs. SalePrice
ggplot(train, aes(x = Fireplaces, y = SalePrice)) +
  geom_point(alpha = 0.5) +
  labs(title = "Scatter Plot of Sale Price vs. Number of Fireplaces",
       x = "Number of Fireplaces", y = "Sale Price") +
  theme_bw()

# GarageCars vs. SalePrice
ggplot(train, aes(x = GarageCars, y = SalePrice)) +
  geom_point(alpha = 0.5) +
  labs(title = "Scatter Plot of Sale Price vs. Garage Size in Car Capacity",
       x = "Garage Size in Car Capacity", y = "Sale Price") +
  theme_bw()

# GarageArea vs. SalePrice
ggplot(train, aes(x = GarageArea, y = SalePrice)) +
  geom_point(alpha = 0.5) +
  labs(title = "Scatter Plot of Sale Price vs. Garage Size in Square Feet",
       x = "Garage Size in Square Feet", y = "Sale Price") +
  theme_bw()


ggplot(train, aes(x = WoodDeckSF, y = SalePrice)) +
  geom_point(alpha = 0.5) +
  labs(title = "Scatter Plot of Sale Price vs. Wood Deck Area",
       x = "Wood Deck Area in Square Feet", y = "Sale Price") +
  theme_bw()

ggplot(train, aes(x = OpenPorchSF, y = SalePrice)) +
  geom_point(alpha = 0.5) +
  labs(title = "Scatter Plot of Sale Price vs. Open Porch Area",
       x = "Open Porch Area in Square Feet", y = "Sale Price") +
  theme_bw()

ggplot(train, aes(x = EnclosedPorch, y = SalePrice)) +
  geom_point(alpha = 0.5) +
  labs(title = "Scatter Plot of Sale Price vs. Enclosed Porch Area",
       x = "Enclosed Porch Area in Square Feet", y = "Sale Price") +
  theme_bw()

ggplot(train, aes(x = `X3SsnPorch`, y = SalePrice)) +
  geom_point(alpha = 0.5) +
  labs(title = "Scatter Plot of Sale Price vs. Three Season Porch Area",
       x = "Three Season Porch Area in Square Feet", y = "Sale Price") +
  theme_bw()

ggplot(train, aes(x = ScreenPorch, y = SalePrice)) +
  geom_point(alpha = 0.5) +
  labs(title = "Scatter Plot of Sale Price vs. Screen Porch Area",
       x = "Screen Porch Area in Square Feet", y = "Sale Price") +
  theme_bw()

ggplot(train, aes(x = PoolArea, y = SalePrice)) +
  geom_point(alpha = 0.5) +
  labs(title = "Scatter Plot of Sale Price vs. Pool Area",
       x = "Pool Area in Square Feet", y = "Sale Price") +
  theme_bw()

ggplot(train, aes(x = MiscVal, y = SalePrice)) +
  geom_point(alpha = 0.5) +
  labs(title = "Scatter Plot of Sale Price vs. Miscellaneous Feature Value",
       x = "Value of Miscellaneous Feature in Dollars", y = "Sale Price") +
  theme_bw()



##5 Create box plots for qualitative variables
# load required packages
library(ggplot2)

# create box plots for each categorical variable
ggplot(train, aes(x = factor(MSSubClass), y = SalePrice)) +
  geom_boxplot(alpha = 0.5) +
  labs(title = "Box Plot of Sale Price vs. Building Class",
       x = "Building Class", y = "Sale Price") +
  theme_bw()

ggplot(train, aes(x = factor(MSZoning), y = SalePrice)) +
  geom_boxplot(alpha = 0.5) +
  labs(title = "Box Plot of Sale Price vs. General Zoning Classification",
       x = "General Zoning Classification", y = "Sale Price") +
  theme_bw()

ggplot(train, aes(x = factor(Street), y = SalePrice)) +
  geom_boxplot(alpha = 0.5) +
  labs(title = "Box Plot of Sale Price vs. Type of Road Access",
       x = "Type of Road Access", y = "Sale Price") +
  theme_bw()

ggplot(train, aes(x = factor(LotShape), y = SalePrice)) +
  geom_boxplot(alpha = 0.5) +
  labs(title = "Box Plot of Sale Price vs. General Shape of Property",
       x = "General Shape of Property", y = "Sale Price") +
  theme_bw()

ggplot(train, aes(x = factor(LandContour), y = SalePrice)) +
  geom_boxplot(alpha = 0.5) +
  labs(title = "Box Plot of Sale Price vs. Flatness of Property",
       x = "Flatness of Property", y = "Sale Price") +
  theme_bw()

ggplot(train, aes(x = factor(Utilities), y = SalePrice)) +
  geom_boxplot(alpha = 0.5) +
  labs(title = "Box Plot of Sale Price vs. Type of Utilities Available",
       x = "Type of Utilities Available", y = "Sale Price") +
  theme_bw()


ggplot(train, aes(x = LotConfig, y = SalePrice)) +
  geom_boxplot(alpha = 0.5) +
  labs(title = "Box Plot of Sale Price vs. Lot Configuration",
       x = "Lot Configuration", y = "Sale Price") +
  theme_bw()

ggplot(train, aes(x = LandSlope, y = SalePrice)) +
  geom_boxplot(alpha = 0.5) +
  labs(title = "Box Plot of Sale Price vs. Slope of Property",
       x = "Slope of Property", y = "Sale Price") +
  theme_bw()

ggplot(train, aes(x = Neighborhood, y = SalePrice)) +
  geom_boxplot(alpha = 0.5) +
  labs(title = "Box Plot of Sale Price vs. Neighborhood",
       x = "Neighborhood", y = "Sale Price") +
  theme_bw() +
  theme(text = element_text(size = 8))


ggplot(train, aes(x = Condition1, y = SalePrice)) +
  geom_boxplot(alpha = 0.5) +
  labs(title = "Box Plot of Sale Price vs. Proximity to Main Road or Railroad (Condition 1)",
       x = "Proximity to Main Road or Railroad", y = "Sale Price") +
  theme_bw()

ggplot(train, aes(x = Condition2, y = SalePrice)) +
  geom_boxplot(alpha = 0.5) +
  labs(title = "Box Plot of Sale Price vs. Proximity to Main Road or Railroad (Condition 2)",
       x = "Proximity to Main Road or Railroad", y = "Sale Price") +
  theme_bw()

ggplot(train, aes(x = BldgType, y = SalePrice)) +
  geom_boxplot(alpha = 0.5) +
  labs(title = "Box Plot of Sale Price vs. Type of Dwelling",
       x = "Type of Dwelling", y = "Sale Price") +
  theme_bw()

ggplot(train, aes(x = HouseStyle, y = SalePrice)) +
  geom_boxplot(alpha = 0.5) +
  labs(title = "Box Plot of Sale Price vs. Style of Dwelling",
       x = "Style of Dwelling", y = "Sale Price") +
  theme_bw()

ggplot(train, aes(x = RoofStyle, y = SalePrice)) +
  geom_boxplot(alpha = 0.5) +
  labs(title = "Box Plot of Sale Price vs. Type of Roof",
       x = "Type of Roof", y = "Sale Price") +
  theme_bw()

ggplot(train, aes(x = RoofMatl, y = SalePrice)) +
  geom_boxplot(alpha = 0.5) +
  labs(title = "Box Plot of Sale Price vs. Roof Material",
       x = "Roof Material", y = "Sale Price") +
  theme_bw()

ggplot(train, aes(x = factor(Exterior1st), y = SalePrice)) +
  geom_boxplot(alpha = 0.5) +
  labs(title = "Box Plot of Sale Price vs. Exterior Covering on House",
       x = "Exterior Covering on House", y = "Sale Price") +
  theme_bw()

ggplot(train, aes(x = factor(Exterior2nd), y = SalePrice)) +
  geom_boxplot(alpha = 0.5) +
  labs(title = "Box Plot of Sale Price vs. Exterior Covering on House (If More Than One Material)",
       x = "Exterior Covering on House (If More Than One Material)", y = "Sale Price") +
  theme_bw()

ggplot(train, aes(x = factor(ExterQual), y = SalePrice)) +
  geom_boxplot(alpha = 0.5) +
  labs(title = "Box Plot of Sale Price vs. Exterior Material Quality",
       x = "Exterior Material Quality", y = "Sale Price") +
  theme_bw()

ggplot(train, aes(x = factor(ExterCond), y = SalePrice)) +
  geom_boxplot(alpha = 0.5) +
  labs(title = "Box Plot of Sale Price vs. Present Condition of the Material on the Exterior",
       x = "Present Condition of the Material on the Exterior", y = "Sale Price") +
  theme_bw()

ggplot(train, aes(x = factor(Foundation), y = SalePrice)) +
  geom_boxplot(alpha = 0.5) +
  labs(title = "Box Plot of Sale Price vs. Type of Foundation",
       x = "Type of Foundation", y = "Sale Price") +
  theme_bw()

ggplot(train, aes(x = factor(HeatingQC), y = SalePrice)) +
  geom_boxplot(alpha = 0.5) +
  labs(title = "Box Plot of Sale Price vs. Heating Quality and Condition",
       x = "Heating Quality and Condition", y = "Sale Price") +
  theme_bw()

ggplot(train, aes(x = factor(CentralAir), y = SalePrice)) +
  geom_boxplot(alpha = 0.5) +
  labs(title = "Box Plot of Sale Price vs. Central Air Conditioning",
       x = "Central Air Conditioning", y = "Sale Price") +
  theme_bw()


ggplot(train, aes(x = factor(KitchenQual), y = SalePrice)) +
  geom_boxplot(alpha = 0.5) +
  labs(title = "Box Plot of Sale Price vs. Kitchen Quality",
       x = "Kitchen Quality", y = "Sale Price") +
  theme_bw()

ggplot(train, aes(x = factor(Functional), y = SalePrice)) +
  geom_boxplot(alpha = 0.5) +
  labs(title = "Box Plot of Sale Price vs. Home Functionality Rating",
       x = "Home Functionality Rating", y = "Sale Price") +
  theme_bw()

ggplot(train, aes(x = factor(PavedDrive), y = SalePrice)) +
  geom_boxplot(alpha = 0.5) +
  labs(title = "Box Plot of Sale Price vs. Paved Driveway",
       x = "Paved Driveway", y = "Sale Price") +
  theme_bw()

ggplot(train, aes(x = factor(SaleType), y = SalePrice)) +
  geom_boxplot(alpha = 0.5) +
  labs(title = "Box Plot of Sale Price vs. Type of Sale",
       x = "Type of Sale", y = "Sale Price") +
  theme_bw()

ggplot(train, aes(x = factor(SaleCondition), y = SalePrice)) +
  geom_boxplot(alpha = 0.5) +
  labs(title = "Box Plot of Sale Price vs. Condition of Sale",
       x = "Condition of Sale", y = "Sale Price") +
  theme_bw()

ggplot(train, aes(x = factor(MoSold), y = SalePrice)) +
  geom_boxplot(alpha = 0.5) +
  labs(title = "Box Plot of Sale Price vs. Month Sold",
       x = "Month Sold", y = "Sale Price") +
  theme_bw()

ggplot(train, aes(x = factor(YrSold), y = SalePrice)) +
  geom_boxplot(alpha = 0.5) +
  labs(title = "Box Plot of Sale Price vs. Year Sold",
       x = "Year Sold", y = "Sale Price") +
  theme_bw()


## 6 Predicting using 3 Models
##Linear Regression Model
# Define the predictor variables
predictors <- c("LotArea", "OverallQual", "OverallCond", "YearBuilt", "BsmtFinSF1", 
                "BsmtFinSF2", "X1stFlrSF", "X2ndFlrSF", "TotRmsAbvGrd", "PoolArea","Neighborhood",
                 "RoofMatl", "ExterQual", "KitchenQual")

# Fit the linear regression model
model1 <- aov(SalePrice ~ ., data = train %>% dplyr::select(SalePrice, all_of(predictors)))

# Summarize the model results
summary(model1)

# Make predictions
predictions <- model1 %>% predict(train_V)
# Model performance
# (a) Compute the prediction error, RMSE
RMSE(predictions, Actuals)


##Model 2

# Define the response variable
response <- "SalePrice"

# Build the decision tree model
# Fit the decision tree model
library(tree)
model2 <- tree(formula = as.formula(paste(response, "~", paste(predictors, collapse = "+"))),
                   data = train)
model2

# Print the summary of the model
summary(model2)

# Plot the decision tree
plot(model2)
text(model2)

# Make predictions
predictions2 <- model2 %>% predict(train_V)
# Model performance
# (a) Compute the prediction error, RMSE
RMSE(predictions2, Actuals)
 
#Model 3
# Load the randomForest package
library(randomForest)

# Build the random forest model
model3 <- randomForest(formula = SalePrice ~ .,
                       data = train %>% dplyr::select(SalePrice, all_of(predictors)),
                       ntree = 500, mtry = 4, importance = TRUE)

# Print the model summary
print(model3)
plot(model3)

#Variable Importance
varImpPlot(model3)

#Predictions
predictions3 <- predict(model3, train_V)

# Compute the model performance (RMSE)
RMSE(predictions3, Actuals)




