library(moments)
library(plyr)
library(data.table)
library(lmtest)
library(sandwich)
library(stargazer)
library(ggplot2)
library(SparseM)
library(caret)
library(stats)


# Create new function for RMSE calculation in levels 
## (if RMSE from caret package doesn't work)
RMSE_Lev <- function(pred, obs, na.rm = FALSE){
  sqrt(mean((obs - pred)^2, na.rm = na.rm))
}
setwd("C:\\Users\\csorn\\Documents\\edu\\ceu\\da\\da4\\term")

source('da_helper_functions.R')

## Reading data file 
property_df <- read.csv("da4_property_aug.csv", header = TRUE)

## ordering property_df data frame by the 13th, 16th, 2nd, 39th, 26th, 25th, 21st, 4th, 22nd and 19th parameter
#13: price in thousands
#16: number of half rooms
#2 : floors
#39: heating
#26: lift dummy
#25: airconditioning dummy
#21: hasbalcony
#4 : balcony size
#22: concrete blockflat dummy
#19: id 
property_df <- property_df[order(property_df[,13], property_df[, 16], property_df[,2], property_df[, 39], 
                                 property_df[,26], property_df[, 25], property_df[,21], property_df[, 4], 
                                 property_df[, 22], property_df[, 19]),]


##flagging those observations with occurrence count, 
#where price, sqm, floor and heating are the same
property_df <- ddply(property_df,.(p, sqm, floor, heating), transform, multiple = NROW(piece))
##frequency of different occurrence flags
count(property_df$multiple)

#transforming data frame to data table
property_df <- data.table(property_df)

#TODO counting how many observations we have from the same price-sqm-floor-heating quartet
property_df <- property_df[ , temp := 1:.N , by = c("p" , "sqm", "floor", "heating") ]
#transforming back to tata.frame
property_df <- as.data.frame(property_df)
#generating frequency table
count(property_df$temp)

#checking for duplicates
property_df$duplicate1 <- as.numeric(property_df$temp != 1)
#we have 4546 unique observations and 2201 duplicates on different levels
count(property_df$duplicate1)

#dropping temp variable
property_df <- property_df[, -49]

#checking for duplicates now with 9 variables 
#original 4 + lift dummy, aircondition dummy, balcony flag, balcony size, whether concrete block or not)
property_df <- ddply(property_df,.(p, sqm, floor, heating, lift_d, aircond_d, hasbalcony, balcony, concrete_blockflat_d), transform, multiple2 = NROW(piece))
#frequency table of the multples
count(property_df$multiple2)

#todo
property_df <- data.table(property_df)
property_df <- property_df[ , temp := 1:.N , by = c("p" , "sqm", "floor", "heating", "lift_d", "aircond_d", "hasbalcony", "balcony", "concrete_blockflat_d") ]
property_df <- as.data.frame(property_df)
count(property_df$temp)


property_df$duplicate2 <- as.numeric(property_df$temp != 1)
count(property_df$duplicate2)

#######################################
#NEW VARS: view, orientation, size of balcony, parking
# balcony has already been included
# there are seamingly cleaned/factorized variables in the dataset:
# view_str - view
# parking_str - parking
# orientation_str - orientation

property_df <- ddply(property_df,.(p, sqm, floor, heating, lift_d, aircond_d, hasbalcony, balcony, 
                                   concrete_blockflat_d, 
                                   view_str, parking_str, orientation_str), 
                     transform, multiple2 = NROW(piece))
#frequency table of the multples
# cool thing, that we have 5546 unique
count(property_df$multiple2)

#todo
property_df <- data.table(property_df)
property_df <- property_df[ , temp3 := 1:.N , 
                            by = c("p" , "sqm", "floor", "heating", "lift_d", "aircond_d", "hasbalcony", 
                                   "balcony", "concrete_blockflat_d",
                                   "view_str", "parking_str", "orientation_str") ]
property_df <- as.data.frame(property_df)
count(property_df$temp3)


property_df$duplicate2 <- as.numeric(property_df$temp3 != 1)
count(property_df$duplicate2)

#keeping only the first 51 columns
property_df <- property_df[, -51]

#creating new variable, with default value 0
property_df$dup_m <- 0
#if in  4 feature setup flat does not have 
#but in  12 feature setup it does have  has duplicate flag, value is 1
property_df$dup_m[property_df$duplicate1 == 1 & property_df$duplicate2 == 0] <- 1
#...otherwise 2
property_df$dup_m[property_df$duplicate2 == 1] <- 2
#creating text label, filling it up with numeric lable first
property_df$dup_m_labels <- property_df$dup_m 
#if in neither listing it got flag, unique flat identified
property_df$dup_m_labels[property_df$dup_m == 0] <- "unique"
#if in 4 feature it got a flag, there is a chance for being duplicate
property_df$dup_m_labels[property_df$dup_m == 1] <- "maybe duplicate"
#if in 12 feature it got a flag, it is truly duplicate
property_df$dup_m_labels[property_df$dup_m == 2] <- "duplicate"
#factorizing the text label
property_df$dup_m_labels <- as.factor(property_df$dup_m_labels)
#frequency table
count(property_df$dup_m_labels)

#table
#count, mean sqm, mean psqm, mean floor, mean whether it is a new flat
ddply(property_df, 
      .(dup_m_labels), 
      summarize, 
      freq = length(dup_m_labels), 
      mean_sqm = round(mean(sqm), digits = 2), 
      mean_psqm = round(mean(psqm), digits = 2),
      mean_floor2 = round(mean(floor2), digits = 2), 
      mean_new_flat = round(mean(new_flat), digits = 2))

#keeping only the uniqe and potentially duplicates
property_df <- property_df[property_df$duplicate2 == 0,]


############# Describe raw data ####
# Look at descriptive statistics - 1

#creating own summary statistics function
summary_stat <- function(x) {
  c(mean = mean(x, na.rm = TRUE), median = median(x, na.rm = TRUE), min = min(x, na.rm = TRUE), max = max(x, na.rm = TRUE), N = length(x))
}

#summary stats for price, sqm, psqm, floor2, number of floor, lift dummy, airconditioning dummy,
#whether it has balcony, balcony size, whether concrete blockflat
for(i in c(13, 16, 20, 23, 3, 26, 25, 21, 4, 22)) {
  print(names(property_df[i]))
  print(round(summary_stat(property_df[,i]), digits = 3))
}  

#frequency table for conditions
ddply(property_df, .(condition), summarize, freq = length(condition), mean_psqm = round(mean(psqm), digits = 3))
#frequency table for heating
ddply(property_df, .(heating), summarize, freq = length(heating), mean_psqm = round(mean(psqm), digits = 3))
#frequency table for view
ddply(property_df, .(view), summarize, freq = length(view), mean_psqm = round(mean(psqm), digits = 3))
#frequency table for view_str
ddply(property_df, .(view_str), summarize, freq = length(view_str), mean_psqm = round(mean(psqm), digits = 3))
#frequency table for parking_str
orientation_str
ddply(property_df, .(parking_str), summarize, freq = length(parking_str), mean_psqm = round(mean(psqm), digits = 3))


#printing the scatterplot distribution of psqm over sqm to a png file generated by ggplot
#levels with linear regression trendline
png(filename="Graph_sqm_psqm3.png", 
    res = 200, width = 800, height = 800)
ggplot(data = property_df, aes(x = sqm, y = psqm)) + 
  labs(
    title='Dependency between flat size\nand price per squaremeter',
    y='Price per squaremeter\n(thousand Forint)',
    x='Area (squaremeter)',
    caption='Flat sizes under 8 sqm are excluded from analysis'
  )+
  theme_bw()+
  geom_vline(xintercept=8, colour = "deeppink4") +
  geom_point(size = 1, colour = "indianred") +
  geom_smooth(method = "lm", colour = "darkred", 
              se = FALSE, size = 1)
dev.off()

#printing the scatterplot distribution of log psqm over log sqm to a png file generated by ggplot
#with linear regression trendline
png(filename="Graph_sc2.png", res = 200, width = 1200, height = 800)
ggplot(data = property_df, aes(x = ln_sqm, y = ln_psqm)) +
  geom_point(size = 2.5, colour = "grey42") +
  geom_smooth(method = "lm", colour = "blue", se = FALSE)
dev.off()

#plotting the linear smoothing trendline of the log psqm over log sqm
png(filename="Graph_lp1.png", res = 200, width = 1200, height = 800)
ggplot(data = property_df, aes(x = ln_sqm, y = ln_psqm)) + geom_smooth()
dev.off()

#plotting the linear smoothing trendline of the  psqm over sqm
png(filename="Graph_lp2.png", res = 200, width = 1200, height = 800)
ggplot(data = property_df, aes(x = sqm, y = psqm)) + geom_smooth()
dev.off()

#plotting the linear smoothing trendline log psqm over log sqm 
#for only those flats, where size is below 180 sqm
png(filename="Graph_lp3.png", res = 200, width = 1200, height = 800)
ggplot(data = property_df[property_df$sqm < 180,], 
       aes(x = ln_sqm, y = ln_psqm)) + geom_smooth()
dev.off()

#plotting the linear smoothing trendline  psqm over  sqm 
#for only those flats, where size is below 180 sqm
png(filename="Graph_lp4.png", res = 200, width = 1200, height = 800)
ggplot(data = property_df[property_df$sqm < 180,], aes(x = sqm, y = psqm)) + geom_smooth() 
dev.off()

#dropping all observations, where the flat size is smaller than 180 sqm
property_df <- property_df[property_df$sqm <= 180,]



###########
# features
###########

#eliminating 2.5 floor and normalizing it to 3 for floor feature
property_df$floor[property_df$floor > 2 & property_df$floor <3] <- 3
#eliminating 2.5 floor and normalizing it to 3 for floor2 feature
property_df$floor2[property_df$floor2 > 2 & property_df$floor2 <3] <- 3
#eliminating proportional number of floors as well
property_df$number_of_floor[property_df$number_of_floor > 5 & property_df$number_of_floor < 6] <- 5

#new variable
property_df$sqm_sp2060 <- property_df$sqm
#using 60 as baseline, if bigger than 60, 60, if less, actual value
property_df$sqm_sp2060[property_df$sqm_sp2060 > 60] <- 60
#new variable: how bigger than 60 sqm, if not bigger, 0
property_df$sqm_sp60p <- property_df$sqm - property_df$sqm_sp2060
#frequency table for how bigger than 60
count(property_df$sqm_sp60p)

#similar trick with log sqm
property_df$lnsqm_sp2060 <- property_df$ln_sqm
#but here the baseline is log 60 ~ 4.1
property_df$lnsqm_sp2060[property_df$lnsqm_sp2060 > 4.1] <- 4.1
#how bigger is the log than log 60
property_df$lnsqm_sp60p <- property_df$ln_sqm - property_df$lnsqm_sp2060


#manual binning sqm
property_df$sqmcut <- cut(property_df$sqm, breaks = c(19,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100,105,110,115,120,125,130,140,150,160,170, 181), right = FALSE)

#normalizing log variable with multiplication with 10
property_df$ln_sqm10 <- property_df$ln_sqm * 10
#manual binning log sqm
property_df$lnsqmcut <- cut(property_df$ln_sqm10, breaks = c(29,  31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52), right = FALSE)

#frequency table for floors
ddply(property_df, .(floor2), summarize, freq = length(floor2), mean_psqm = round(mean(psqm), digits = 3))

#keeping only non-suterens
property_df <- property_df[which(property_df$suter == 0 | is.na(property_df$suter)),]

#where floor2 value is missing we assign the mean
property_df$floor2[is.na(property_df$floor2)] <- 3
#new variable for floor baseline
property_df$floor_sp05 <- property_df$floor2
#wherever floor is bigger than 5, 5 is the baseline 
property_df$floor_sp05[property_df$floor_sp05 > 5] <- 5
#how bigger the floor is than 5, if any bigger
property_df$floor_sp6p <- property_df$floor2 - property_df$floor_sp05

#new variable, flag whether ground floor
property_df$floor0 <- as.numeric(property_df$floor == 0)

#interacting lift with floor
property_df$liftXfloor2 <- property_df$lift_d * property_df$floor2
#interacting balcony flag with floor
property_df$balconyXfloor2 <- property_df$hasbalcony * property_df$floor2

#frequency and mean psqm by condition
ddply(property_df, .(condition), summarize, freq = length(condition), mean_psqm = round(mean(psqm), digits = 3))

#new helper var
property_df$condition_broad <- as.character(property_df$condition)
#replacing <NA> sign for NA
property_df$condition_broad[which(is.na(property_df$condition_broad))] <- "NA"
#collapsing 2 newly build subtypes together
property_df$condition_broad[which(property_df$condition_broad == "just opened" | property_df$condition == "under construction")] <- "newly built"
#factorizing not newly built
property_df$condition_broad <- factor(property_df$condition_broad, levels = c("NA", "in good condition", "in medium condition", "newly built", "recently built", "renovated"))

#frequency table of conditions with mean psqms
ddply(property_df, .(condition_broad), summarize, freq = length(condition_broad), mean_psqm = round(mean(psqm), digits = 3))

#turning back condition to character
property_df$condition <- as.character(property_df$condition)
#assigning NA char to NA values
property_df$condition[which(is.na(property_df$condition))] <- "NA"
#factorizing again
property_df$condition <- factor(property_df$condition, levels = c("NA", "in good condition", "in medium condition", "just opened", "newly built", "recently built", "renovated", "under construction"))

#frequency table for heating, with avg psqm
ddply(property_df, .(heating), summarize, freq = length(heating), mean_psqm = round(mean(psqm), digits = 3))

#helper dummy for heating
property_df$heating_broad <- as.character(property_df$heating)
#NA strings for NA values
property_df$heating_broad[which(is.na(property_df$heating_broad))] <- "NA"
#grouping non-usual heating to other category
property_df$heating_broad[which(property_df$heating_broad == "fan coil " | property_df$heating_broad == "electric" | property_df$heating_broad == "NA" | property_df$heating_broad == "geothermal" | property_df$heating_broad == "stove")] <- "other"
#grouping gas convector heating types together
property_df$heating_broad[which(property_df$heating_broad == "gas Héra boiler" | property_df$heating_broad == "gas boiler")] <- "gas convector heating"
#shortening district heating name
property_df$heating_broad[which(property_df$heating_broad == "district heating with heat meter")] <- "district heating"
#shortening central heating name
property_df$heating_broad[which(property_df$heating_broad == "central heating with heat meter")] <- "central heating"
#factorizing heating
property_df$heating_broad <- factor(property_df$heating_broad)

#frequency table of the output
ddply(property_df, .(heating_broad), summarize, freq = length(heating_broad), mean_psqm = round(mean(psqm), digits = 3))

#changing NA to char NA and creating factor of it
property_df$heating <- as.character(property_df$heating)
property_df$heating[which(is.na(property_df$heating))] <- "NA"
property_df$heating <- factor(property_df$heating, levels = c("NA", "central heating", "central heating with heat meter", "district heating", "district heating with heat meter", "electric", "fan coil ", "gas boiler", "gas circo heating", "gas convector heating", "gas Héra boiler", "geothermal", "other", "stove"))

#frequency table of view
ddply(property_df, .(view), summarize, freq = length(view), mean_psqm = round(mean(psqm), digits = 3))

#here the only thing to be done is factoring in NA
property_df$view <- as.character(property_df$view)
property_df$view[which(is.na(property_df$view))] <- "NA"
property_df$view <- factor(property_df$view, levels = c("NA", "court view", "garden view", "panorama", "street view"))

#...successfully
ddply(property_df, .(view), summarize, freq = length(view), mean_psqm = round(mean(psqm), digits = 3))


#################################################
# Create test and train samples, cross-validation
#################################################

#creating sample size
smp_size <- floor(0.8 * nrow(property_df))

#randomization seed
set.seed(201703)

#creating random nimber in the size of 80% our dataset size
train_ids <- sample(seq_len(nrow(property_df)), size = smp_size)

#default value is 0
property_df$train <- 0
#for the randomly selected rows it is 1, training and test set separated
property_df$train[train_ids] <- 1


# regressions 


#linear regression: LHS: ln pqsm, RHSs: the log size until 60 sqm, log size over 60 sqm and heating with 5 levels
heat_reg_1 <- lm(data = property_df, ln_psqm ~ lnsqm_sp2060 + lnsqm_sp60p + factor(heating_broad))
#linear regression: LHS: ln pqsm, RHSs: the log size until 60 sqm, log size over 60 sqm and heating with 14 levels
heat_reg_2 <- lm(data = property_df, ln_psqm ~ lnsqm_sp2060 + lnsqm_sp60p + factor(heating))

#error rate estimated with RMSE for regression1 
heat_reg_1_rmse1 <- round(RMSE_Lev(pred = predict(heat_reg_1, newdata = property_df[property_df$train == 0,]), obs = property_df$ln_psqm[property_df$train == 0], na.rm = TRUE), digits = 3)
#error rate estimated with BIC for regression2
heat_reg_1_bic1  <- round(BIC(heat_reg_1), digits = 3)

#error rate estimated with RMSE for regression2
heat_reg_2_rmse1 <- round(RMSE_Lev(pred = predict(heat_reg_2, newdata = property_df[property_df$train == 0,]), obs = property_df$ln_psqm[property_df$train == 0], na.rm = TRUE), digits = 3)
#error rate estimated with BIC for regression2
heat_reg_2_bic1  <- round(BIC(heat_reg_2), digits = 3)

#show the models with the 2 error measures added to the table
#note: adding rows throw error, RODO check why
stargazer_r(list_of_models = c(list(heat_reg_1), list(heat_reg_2)), 
            digits = 3, 
            add.lines = list(
              c("BIC", heat_reg_1_bic1, heat_reg_2_bic1), 
              c("RMSE_Lev", heat_reg_1_rmse1, heat_reg_2_rmse1)),
            out = "heating_reg.html")



#linear regression: LHS: ln pqsm, RHSs: the log size until 60 sqm, log size over 60 sqm and condition with 6 levels
cond_reg_1 <- lm(data = property_df, ln_psqm ~ lnsqm_sp2060 + lnsqm_sp60p + factor(condition_broad))
#linear regression: LHS: ln pqsm, RHSs: the log size until 60 sqm, log size over 60 sqm and condition with 7 levels
cond_reg_2 <- lm(data = property_df, ln_psqm ~ lnsqm_sp2060 + lnsqm_sp60p + factor(condition))

#calculating BIC and RMSE for model 2.1
cond_reg_1_rmse1 <- round(RMSE_Lev(pred = predict(cond_reg_1, newdata = property_df[property_df$train == 0,]), obs = property_df$ln_psqm[property_df$train == 0], na.rm = TRUE), digits = 3)
cond_reg_1_bic1  <- round(BIC(cond_reg_1), digits = 3)

#calculating BIC and RMSE for model 2.1
cond_reg_2_rmse1 <- round(RMSE_Lev(pred = predict(cond_reg_2, newdata = property_df[property_df$train == 0,]), obs = property_df$ln_psqm[property_df$train == 0], na.rm = TRUE), digits = 3)
cond_reg_2_bic1  <- round(BIC(cond_reg_2), digits = 3)

#show the models with the 2 error measures added to the table, for models #2
stargazer_r(list_of_models = c(list(cond_reg_1), list(cond_reg_2)), digits = 3, add.lines = list(c("BIC", cond_reg_1_bic1, cond_reg_2_bic1), c("RMSE_Lev", cond_reg_1_rmse1, cond_reg_2_rmse1)), out = "condition_reg.txt")

############################
### Define prediction models
############################

#defining outcome variable with pqsm level
property_df$Ylev <- property_df$psqm
#defining outcome variable with pqsm log
property_df$Ylog <- property_df$ln_psqm

# models in levels
# 1
#predicting square meter from floor and sqm with linear regression
modellev1 <- lm(data = property_df[property_df$train == 1,], Ylev ~ sqm + floor2)
#train error RMSE
modellev1_rmse_train <- round(RMSE_Lev(pred = predict(modellev1, newdata = property_df[property_df$train == 1,]), obs = property_df$Ylev[property_df$train == 1], na.rm = TRUE), digits = 3)
#test error RMSE
modellev1_rmse_test <- round(RMSE_Lev(pred = predict(modellev1, newdata = property_df[property_df$train == 0,]), obs = property_df$Ylev[property_df$train == 0], na.rm = TRUE), digits = 3)
#BIC error 
modellev1_bic  <- round(BIC(modellev1), digits = 3)
#where are the coefficeints are not NA
modellev1_n <- length(which(is.na(coefficients(modellev1)) == FALSE)) - 1

# 2
#predicting square meter from variables:
# size under 60sqm, size over 60sqm, whether ground floor, whether until 6th floor, whether over 6th
# calculating RMSE for train and test set
# calculating BIC
# significance check
modellev2 <- lm(data = property_df[property_df$train == 1,], Ylev ~ sqm_sp2060 + sqm_sp60p + floor0 + floor_sp05 + floor_sp6p)
modellev2_rmse_train <- round(RMSE_Lev(pred = predict(modellev2, newdata = property_df[property_df$train == 1,]), obs = property_df$Ylev[property_df$train == 1], na.rm = TRUE), digits = 3)
modellev2_rmse_test <- round(RMSE_Lev(pred = predict(modellev2, newdata = property_df[property_df$train == 0,]), obs = property_df$Ylev[property_df$train == 0], na.rm = TRUE), digits = 3)
modellev2_bic  <- round(BIC(modellev2), digits = 3)
modellev2_n <- length(which(is.na(coefficients(modellev2)) == FALSE)) - 1

# 3
#predicting square meter from variables:
# - size under 60sqm, size over 60sqm, whether ground floor, whether until 6th floor, whether over 6th
# - number of floors, condition(collapsed), whether has lift, balcony, air conditioning, heating (collapsed), whether concrete block
# calculating RMSE for train and test set
# calculating BIC
# significance check
modellev3 <- lm(data = property_df[property_df$train == 1,], Ylev ~ sqm_sp2060 + sqm_sp60p + floor0 + floor_sp05 + floor_sp6p + number_of_floor + factor(condition_broad) + lift_d + hasbalcony + aircond_d + factor(heating_broad) + concrete_blockflat_d)
modellev3_rmse_train <- round(RMSE_Lev(pred = predict(modellev3, newdata = property_df[property_df$train == 1,]), obs = property_df$Ylev[property_df$train == 1], na.rm = TRUE), digits = 3)
modellev3_rmse_test <- round(RMSE_Lev(pred = predict(modellev3, newdata = property_df[property_df$train == 0,]), obs = property_df$Ylev[property_df$train == 0], na.rm = TRUE), digits = 3)
modellev3_bic  <- round(BIC(modellev3), digits = 3)
modellev3_n <- length(which(is.na(coefficients(modellev3)) == FALSE)) - 1

# 4
#predicting square meter from variables:
# - size under 60sqm, size over 60sqm, whether ground floor, whether until 6th floor, whether over 6th
# - number of floors, condition(collapsed), whether has lift, balcony, air conditioning, heating (collapsed), whether concrete block
# - adding interactions with floor
# calculating RMSE for train and test set
# calculating BIC
# significance check
modellev4 <- lm(data = property_df[property_df$train == 1,], Ylev ~ sqm_sp2060 + sqm_sp60p + floor0 + floor_sp05 + floor_sp6p + number_of_floor + factor(condition_broad) + lift_d + hasbalcony + aircond_d + factor(heating_broad) + concrete_blockflat_d + balconyXfloor2 + liftXfloor2 + factor(floor)*concrete_blockflat_d + factor(floor)*number_of_floor + factor(floor)*heating_broad)
modellev4_rmse_train <- round(RMSE_Lev(pred = predict(modellev4, newdata = property_df[property_df$train == 1,]), obs = property_df$Ylev[property_df$train == 1], na.rm = TRUE), digits = 3)
modellev4_rmse_test <- round(RMSE_Lev(pred = predict(modellev4, newdata = property_df[property_df$train == 0,]), obs = property_df$Ylev[property_df$train == 0], na.rm = TRUE), digits = 3)
modellev4_bic  <- round(BIC(modellev4), digits = 3)
modellev4_n <- length(which(is.na(coefficients(modellev4)) == FALSE)) - 1

# 5
#predicting square meter from variables:
# - size under 60sqm, size over 60sqm, whether ground floor, whether until 6th floor, whether over 6th
# - number of floors, condition(collapsed), whether has lift, balcony, air conditioning, heating (collapsed), whether concrete block
# - adding interactions with new flat dummy
# calculating RMSE for train and test set
# calculating BIC
# significance check
modellev5 <- lm(data = property_df[property_df$train == 1,], Ylev ~ new_flat*sqm_sp2060 + new_flat*sqm_sp60p + new_flat*floor0 + new_flat*floor_sp05 + new_flat*floor_sp6p + number_of_floor + factor(condition_broad) + lift_d + hasbalcony  + aircond_d + factor(heating_broad) + concrete_blockflat_d + new_flat*number_of_floor + new_flat*lift_d + new_flat*liftXfloor2 + new_flat*hasbalcony + new_flat*balconyXfloor2 + new_flat*aircond_d + new_flat*factor(heating_broad) + new_flat*concrete_blockflat_d)
modellev5_rmse_train <- round(RMSE_Lev(pred = predict(modellev5, newdata = property_df[property_df$train == 1,]), obs = property_df$Ylev[property_df$train == 1], na.rm = TRUE), digits = 3)
modellev5_rmse_test <- round(RMSE_Lev(pred = predict(modellev5, newdata = property_df[property_df$train == 0,]), obs = property_df$Ylev[property_df$train == 0], na.rm = TRUE), digits = 3)
modellev5_bic  <- round(BIC(modellev5), digits = 3)
modellev5_n <- length(which(is.na(coefficients(modellev5)) == FALSE)) - 1


# 6
#predicting square meter from variables:
# - size under 60sqm, size over 60sqm, whether ground floor, whether until 6th floor, whether over 6th
# - number of floors, condition(collapsed), whether has lift, balcony, air conditioning, heating (collapsed), whether concrete block
# - adding even more interactions
# calculating RMSE for train and test set
# calculating BIC
# significance check
modellev6 <- lm(data = property_df[property_df$train == 1,], Ylev ~ new_flat*sqm_sp2060 + new_flat*sqm_sp60p + new_flat*floor0 + new_flat*floor_sp05 + new_flat*floor_sp6p + number_of_floor + factor(condition_broad) + lift_d + hasbalcony  + aircond_d + factor(heating_broad) + concrete_blockflat_d + factor(condition_broad)*factor(floor2) + factor(condition_broad)*number_of_floor + factor(condition_broad)*lift_d + factor(condition_broad):liftXfloor2 + factor(condition_broad)*hasbalcony + factor(condition_broad):balconyXfloor2 + factor(condition_broad)*aircond_d + factor(condition_broad)*factor(heating_broad) + factor(condition_broad)*concrete_blockflat_d + factor(condition_broad)*factor(floor2)*number_of_floor + factor(condition_broad)*factor(floor2)*factor(heating_broad) + factor(condition_broad)*factor(floor2)*concrete_blockflat_d)
modellev6_rmse_train <- round(RMSE_Lev(pred = predict(modellev6, newdata = property_df[property_df$train == 1,]), obs = property_df$Ylev[property_df$train == 1], na.rm = TRUE), digits = 3)
modellev6_rmse_test <- round(RMSE_Lev(pred = predict(modellev6, newdata = property_df[property_df$train == 0,]), obs = property_df$Ylev[property_df$train == 0], na.rm = TRUE), digits = 3)
modellev6_bic  <- round(BIC(modellev6), digits = 3)
modellev6_n <- length(which(is.na(coefficients(modellev6)) == FALSE)) - 1
#stargazer(modellev6, out = "model6.txt")

# 7
#predicting square meter from variables:
# - here we use only the manually binned sqm interacted wurg new dummy , number of floors, condition, lift, balcony... and interactions
# calculating RMSE for train and test set
# calculating BIC
# significance check
modellev7 <- lm(data = property_df[property_df$train == 1,], Ylev ~ new_flat*factor(sqmcut) + number_of_floor + factor(condition_broad) + lift_d + hasbalcony  + aircond_d + factor(heating_broad) + concrete_blockflat_d + factor(condition_broad)*factor(floor2) + factor(condition_broad)*number_of_floor + factor(condition_broad)*lift_d + factor(condition_broad)*liftXfloor2 + factor(condition_broad)*hasbalcony + factor(condition_broad)*balconyXfloor2 + factor(condition_broad)*aircond_d + factor(condition_broad)*factor(heating_broad) + factor(condition_broad)*concrete_blockflat_d + factor(condition_broad)*factor(floor2)*number_of_floor + factor(condition_broad)*factor(floor2) * factor(heating_broad) + factor(condition_broad)*factor(floor2)*concrete_blockflat_d)
modellev7_rmse_train <- round(RMSE_Lev(pred = predict(modellev7, newdata = property_df[property_df$train == 1,]), obs = property_df$Ylev[property_df$train == 1], na.rm = TRUE), digits = 3)
modellev7_rmse_test <- round(RMSE_Lev(pred = predict(modellev7, newdata = property_df[property_df$train == 0,]), obs = property_df$Ylev[property_df$train == 0], na.rm = TRUE), digits = 3)
modellev7_bic  <- round(BIC(modellev7), digits = 3)
modellev7_n <- length(which(is.na(coefficients(modellev7)) == FALSE)) - 1

# 8
#predicting square meter the same way as 7, but now with using floor instead of floor2
modellev8 <- lm(data = property_df[property_df$train == 1,], Ylev ~ new_flat*factor(sqmcut) + number_of_floor + factor(condition_broad) + lift_d + hasbalcony  + aircond_d + factor(heating_broad) + concrete_blockflat_d + factor(condition_broad)*factor(floor) + factor(condition_broad)*number_of_floor + factor(condition_broad)*lift_d + factor(condition_broad)*liftXfloor2 + factor(condition_broad)*hasbalcony + factor(condition_broad)*balconyXfloor2 + factor(condition_broad)*aircond_d + factor(condition_broad)*factor(heating_broad) + factor(condition_broad)*concrete_blockflat_d + factor(condition_broad)*factor(floor)*number_of_floor + factor(condition_broad)*factor(floor) * factor(heating_broad) + factor(condition_broad)*factor(floor)*concrete_blockflat_d)
modellev8_rmse_train <- round(RMSE_Lev(pred = predict(modellev8, newdata = property_df[property_df$train == 1,]), obs = property_df$Ylev[property_df$train == 1], na.rm = TRUE), digits = 3)
modellev8_rmse_test <- round(RMSE_Lev(pred = predict(modellev8, newdata = property_df[property_df$train == 0,]), obs = property_df$Ylev[property_df$train == 0], na.rm = TRUE), digits = 3)
modellev8_bic  <- round(BIC(modellev8), digits = 3)
modellev8_n <- length(which(is.na(coefficients(modellev8)) == FALSE)) - 1

#visualizing, add lines still not work
stargazer_r(list_of_models = c(list(modellev1), 
                               list(modellev2),
                               list(modellev3), 
                               list(modellev4), 
                               list(modellev5), 
                               list(modellev6), 
                               list(modellev7), 
                               list(modellev8)), digits = 3, 
            add.lines = list(c("BIC", modellev1_bic, modellev2_bic, modellev3_bic, 
                               modellev4_bic, modellev5_bic, modellev6_bic, modellev7_bic, modellev8_bic), 
                             c("RMSE_train", modellev1_rmse_train, modellev2_rmse_train, modellev3_rmse_train, 
                               modellev4_rmse_train, modellev5_rmse_train, modellev6_rmse_train, modellev7_rmse_train, modellev8_rmse_train), 
                             c("RMSE_test", modellev1_rmse_test, modellev2_rmse_test, modellev3_rmse_test, 
                               modellev4_rmse_test, modellev5_rmse_test, modellev6_rmse_test, modellev7_rmse_test, modellev8_rmse_test), 
                             c("Number of controls", modellev1_n, modellev2_n, modellev3_n, modellev4_n, 
                               modellev5_n, modellev6_n, modellev7_n, modellev8_n)), 
            out = "models_lev_results.html")

# models in logs

#creating appropriate RMSE function for training set log values
rmse_train_log <- function (x){
  sqrt(mean((property_df$Ylev[property_df$train == 1] - exp(predict(x, newdata = property_df[property_df$train == 1,])) * exp(resid(x)^2/2))^2))
}

#creating appropriate RMSE function for training set log values
rmse_test_log <- function (x){
  sqrt(mean((property_df$Ylev[property_df$train == 0] - exp(predict(x, newdata = property_df[property_df$train == 0,])) * exp((property_df$Ylog[property_df$train == 0] - predict(x, newdata = property_df[property_df$train == 0,]))^2/2))^2))
}

# 1
# linear prediction: LHS log psqm, RHS log sqm and floor2
# calculating RMSE for training set, test set, bic, variable count
modellog1 <- lm(data = property_df[property_df$train == 1,], Ylog ~ ln_sqm + floor2)
modellog1_rmse_train <- round(rmse_train_log(modellog1), digits = 3)
modellog1_rmse_test <- round(rmse_test_log(modellog1), digits = 3)
modellog1_bic  <- round(BIC(modellog1), digits = 3)
modellog1_n <- length(which(is.na(coefficients(modellog1)) == FALSE)) - 1

# 2
# linear prediction: LHS log psqm, 
#RHS log sqm below 60 part, log sqm over 60 part and whether ground floor, whether until 5th floor, whether over 6th
# calculating RMSE for training set, test set, bic, variable count
modellog2 <- lm(data = property_df[property_df$train == 1,], Ylog ~ lnsqm_sp2060 + lnsqm_sp60p + floor0 + floor_sp05 + floor_sp6p)
modellog2_rmse_train <- round(rmse_train_log(modellog2), digits = 3)
modellog2_rmse_test <- round(rmse_test_log(modellog2), digits = 3)
modellog2_bic  <- round(BIC(modellog2), digits = 3)
modellog2_n <- length(which(is.na(coefficients(modellog2)) == FALSE)) - 1

# 3 
# linear prediction: LHS log psqm, 
# RHS log sqm below 60 part, log sqm over 60 part and whether ground floor, whether until 5th floor, whether over 6th
# also heating, condition, airconditioning, ...
# calculating RMSE for training set, test set, bic, variable count
modellog3 <- lm(data = property_df[property_df$train == 1,], Ylog ~ lnsqm_sp2060 + lnsqm_sp60p + floor0 + floor_sp05 + floor_sp6p + number_of_floor + factor(condition_broad) + lift_d + hasbalcony + aircond_d + factor(heating_broad) + concrete_blockflat_d)
modellog3_rmse_train <- round(rmse_train_log(modellog3), digits = 3)
modellog3_rmse_test <- round(rmse_test_log(modellog3), digits = 3)
modellog3_bic  <- round(BIC(modellog3), digits = 3)
modellog3_n <- length(which(is.na(coefficients(modellog3)) == FALSE)) - 1

# 4 
# linear prediction: LHS log psqm, 
# RHS log sqm below 60 part, log sqm over 60 part and whether ground floor, whether until 5th floor, whether over 6th
# also heating, condition, airconditioning, ...
# interactions with floor2
# calculating RMSE for training set, test set, bic, variable count
modellog4 <- lm(data = property_df[property_df$train == 1,], Ylog ~ lnsqm_sp2060 + lnsqm_sp60p + floor0 + floor_sp05 + floor_sp6p + number_of_floor + factor(condition_broad) + lift_d + hasbalcony + aircond_d + factor(heating_broad) + concrete_blockflat_d + balconyXfloor2 + liftXfloor2 + factor(floor)*concrete_blockflat_d + factor(floor)*number_of_floor + factor(floor)*heating_broad)
modellog4_rmse_train <- round(rmse_train_log(modellog4), digits = 3)
modellog4_rmse_test <- round(rmse_test_log(modellog4), digits = 3)
modellog4_bic  <- round(BIC(modellog4), digits = 3)
modellog4_n <- length(which(is.na(coefficients(modellog4)) == FALSE)) - 1

# 5
# linear prediction: LHS log psqm, 
# RHS log sqm below 60 part, log sqm over 60 part and whether ground floor, whether until 5th floor, whether over 6th
# also heating, condition, airconditioning, ...
# interactions with new flat 
# calculating RMSE for training set, test set, bic, variable count
modellog5 <- lm(data = property_df[property_df$train == 1,], Ylog ~ new_flat*lnsqm_sp2060 + new_flat*lnsqm_sp60p + new_flat*floor0 + new_flat*floor_sp05 + new_flat*floor_sp6p + number_of_floor + factor(condition_broad) + lift_d + hasbalcony + aircond_d + factor(heating_broad) + concrete_blockflat_d + new_flat:number_of_floor + new_flat:lift_d + new_flat*liftXfloor2 + new_flat:hasbalcony + new_flat*balconyXfloor2 + new_flat:aircond_d + new_flat:factor(heating_broad) + new_flat:concrete_blockflat_d)
modellog5_rmse_train <- round(rmse_train_log(modellog5), digits = 3)
modellog5_rmse_test <- round(rmse_test_log(modellog5), digits = 3)
modellog5_bic  <- round(BIC(modellog5), digits = 3)
modellog5_n <- length(which(is.na(coefficients(modellog5)) == FALSE)) - 1

# 6
# linear prediction: LHS log psqm, 
# RHS log sqm below 60 part, log sqm over 60 part and whether ground floor, whether until 5th floor, whether over 6th
# also heating, condition, airconditioning, ...
# interactions
# calculating RMSE for training set, test set, bic, variable count
modellog6 <- lm(data = property_df[property_df$train == 1,], Ylog ~ new_flat*lnsqm_sp2060 + new_flat*lnsqm_sp60p + new_flat*floor0 + new_flat*floor_sp05 + new_flat*floor_sp6p + number_of_floor + factor(condition_broad) + lift_d + hasbalcony + aircond_d + factor(heating_broad) + concrete_blockflat_d + factor(condition_broad):factor(floor2) + factor(condition_broad):number_of_floor + factor(condition_broad):lift_d + factor(condition_broad):liftXfloor2 + factor(condition_broad):hasbalcony + factor(condition_broad):balconyXfloor2 + factor(condition_broad):aircond_d + factor(condition_broad):factor(heating_broad) + factor(condition_broad):concrete_blockflat_d + factor(condition_broad):factor(floor2):number_of_floor + factor(condition_broad):factor(floor2):factor(heating_broad) + factor(condition_broad):factor(floor2):concrete_blockflat_d)
modellog6_rmse_train <- round(rmse_train_log(modellog6), digits = 3)
modellog6_rmse_test <- round(rmse_test_log(modellog6), digits = 3)
modellog6_bic  <- round(BIC(modellog6), digits = 3)
modellog6_n <- length(which(is.na(coefficients(modellog6)) == FALSE)) - 1


# 7
# linear prediction: LHS log psqm, 
# RHS log sqm below 60 part, log sqm over 60 part and whether ground floor, whether until 5th floor, whether over 6th
# also heating, condition, airconditioning, ...
# using manuall sqm binning logs and interactions, floor2
# calculating RMSE for training set, test set, bic, variable count
modellog7 <- lm(data = property_df[property_df$train == 1,], Ylog ~ new_flat*lnsqmcut + number_of_floor + factor(condition_broad) + lift_d + hasbalcony + aircond_d + factor(heating_broad) + concrete_blockflat_d + factor(condition_broad):factor(floor2) + factor(condition_broad):number_of_floor + factor(condition_broad):lift_d + factor(condition_broad):liftXfloor2 + factor(condition_broad):hasbalcony + factor(condition_broad):balconyXfloor2 + factor(condition_broad):aircond_d + factor(condition_broad):factor(heating_broad) + factor(condition_broad):concrete_blockflat_d + factor(condition_broad):factor(floor2):number_of_floor + factor(condition_broad):factor(floor2):factor(heating_broad) + factor(condition_broad):factor(floor2):concrete_blockflat_d)
modellog7_rmse_train <- round(rmse_train_log(modellog7), digits = 3)
modellog7_rmse_test <- round(rmse_test_log(modellog7), digits = 3)
modellog7_bic  <- round(BIC(modellog7), digits = 3)
modellog7_n <- length(which(is.na(coefficients(modellog7)) == FALSE)) - 1

# 8
# linear prediction: LHS log psqm, 
# RHS log sqm below 60 part, log sqm over 60 part and whether ground floor, whether until 5th floor, whether over 6th
# also heating, condition, airconditioning, ...
# using manuall sqm binning logs and interactions, floor
# calculating RMSE for training set, test set, bic, variable count
modellog8 <- lm(data = property_df[property_df$train == 1,], Ylog ~ new_flat*lnsqmcut + number_of_floor + factor(condition_broad) + lift_d + hasbalcony + aircond_d + factor(heating_broad) + concrete_blockflat_d + factor(condition_broad)*factor(floor) + factor(condition_broad)*number_of_floor + factor(condition_broad)*lift_d + factor(condition_broad)*liftXfloor2 + factor(condition_broad)*hasbalcony + factor(condition_broad)*balconyXfloor2 + factor(condition_broad)*aircond_d + factor(condition_broad)*factor(heating_broad) + factor(condition_broad)*concrete_blockflat_d + factor(condition_broad)*factor(floor)*number_of_floor + factor(condition_broad)*factor(floor)*factor(heating_broad) + factor(condition_broad)*factor(floor)*concrete_blockflat_d)
modellog8_rmse_train <- round(rmse_train_log(modellog8), digits = 3)
modellog8_rmse_test <- round(rmse_test_log(modellog8), digits = 3)
modellog8_bic  <- round(BIC(modellog8), digits = 3)
modellog8_n <- length(which(is.na(coefficients(modellog8)) == FALSE)) - 1

#visualizing with stargazer
stargazer_r(list_of_models = c(list(modellog1), list(modellog2), list(modellog3), list(modellog4), list(modellog5), list(modellog6), list(modellog7), list(modellog8)), digits = 3, add.lines = list(c("BIC", modellog1_bic, modellog2_bic, modellog3_bic, modellog4_bic, modellog5_bic, modellog6_bic, modellog7_bic, modellog8_bic), c("RMSE_train", modellog1_rmse_train, modellog2_rmse_train, modellog3_rmse_train, modellog4_rmse_train, modellog5_rmse_train, modellog6_rmse_train, modellog7_rmse_train, modellog8_rmse_train), c("RMSE_test", modellog1_rmse_test, modellog2_rmse_test, modellog3_rmse_test, modellog4_rmse_test, modellog5_rmse_test, modellog6_rmse_test, modellog7_rmse_test, modellog8_rmse_test), c("Number of controls", modellog1_n, modellog2_n, modellog3_n, modellog4_n, modellog5_n, modellog6_n, modellog7_n, modellog8_n)), out = "models_log_results.txt")
stargazer(list(modellog1), list(modellog2), list(modellog3), list(modellog4), list(modellog5), list(modellog6), list(modellog7), list(modellog8), digits = 3, add.lines = list(c("BIC", modellog1_bic, modellog2_bic, modellog3_bic, modellog4_bic, modellog5_bic, modellog6_bic, modellog7_bic, modellog8_bic), c("RMSE_train", modellog1_rmse_train, modellog2_rmse_train, modellog3_rmse_train, modellog4_rmse_train, modellog5_rmse_train, modellog6_rmse_train, modellog7_rmse_train, modellog8_rmse_train), c("RMSE_test", modellog1_rmse_test, modellog2_rmse_test, modellog3_rmse_test, modellog4_rmse_test, modellog5_rmse_test, modellog6_rmse_test, modellog7_rmse_test, modellog8_rmse_test), c("Number of controls", modellog1_n, modellog2_n, modellog3_n, modellog4_n, modellog5_n, modellog6_n, modellog7_n, modellog8_n)), out = "models_log_results_starg.txt")


# CV
# cross validation
# set seed of randomization
set.seed(201703)
# using 5-fold crossvalidation
k <- 5
# creating folds
folds <- sample(rep(1:k, nrow(property_df)/k))
# sneak peak into folds (only 1 to 5 here)
folds
# number of observations in folds (equal, all 1013)
table(folds)

# dropping RMSE values for level models
rmse_mlev1 <- NULL
rmse_mlev2 <- NULL
rmse_mlev3 <- NULL
rmse_mlev4 <- NULL
rmse_mlev5 <- NULL
rmse_mlev6 <- NULL
rmse_mlev7 <- NULL
rmse_mlev8 <- NULL

# Create empty vectors for RSMEs (lev train)
rmse_train_mlev1 <- NULL
rmse_train_mlev2 <- NULL
rmse_train_mlev3 <- NULL
rmse_train_mlev4 <- NULL
rmse_train_mlev5 <- NULL
rmse_train_mlev6 <- NULL
rmse_train_mlev7 <- NULL
rmse_train_mlev8 <- NULL

# dropping RMSE values for log models
rmse_mlog1 <- NULL
rmse_mlog2 <- NULL
rmse_mlog3 <- NULL
rmse_mlog4 <- NULL
rmse_mlog5 <- NULL
rmse_mlog6 <- NULL
rmse_mlog7 <- NULL
rmse_mlog8 <- NULL

# Create empty vectors for RSMEs (log train)
rmse_train_mlog1 <- NULL
rmse_train_mlog2 <- NULL
rmse_train_mlog3 <- NULL
rmse_train_mlog4 <- NULL
rmse_train_mlog5 <- NULL
rmse_train_mlog6 <- NULL
rmse_train_mlog7 <- NULL
rmse_train_mlog8 <- NULL

#dropping bic values for levels
bic_mlev1  <- NULL
bic_mlev2  <- NULL
bic_mlev3  <- NULL
bic_mlev4  <- NULL
bic_mlev5  <- NULL
bic_mlev6  <- NULL
bic_mlev7  <- NULL
bic_mlev8  <- NULL


#RMSE calculator function for levels
rmse_log <- function (x){
  sqrt(
    mean(
      (property_df_test$Ylev - exp(predict(x, newdata = property_df_test)) * 
         exp((property_df_test$Ylog - predict(x, newdata = property_df_test))^2/2))
      ^2)
  )
}

#RMSE for log values in training set
rmse_train_log <- function (x){
  sqrt(
    mean(
      (property_df_train$Ylev - exp(predict(x, newdata = property_df_train)) * 
         exp((property_df_train$Ylog - predict(x, newdata = property_df_train))^2/2))
      ^2)
  )
}

#dropping bic values for levels
bic_mlog1  <- NULL
bic_mlog2  <- NULL
bic_mlog3  <- NULL
bic_mlog4  <- NULL
bic_mlog5  <- NULL
bic_mlog6  <- NULL
bic_mlog7  <- NULL
bic_mlog8  <- NULL

#for each eand every fold (out of k=5)
for(i in 1:k){
  #... the training set is every fold, which is not the actual
  property_df_train <- property_df[folds!=i,]
  #... and the test set is the actual
  property_df_test <- property_df[folds == i,]
  
  
  # linear model with LHS is the predicted price per squaremeter level
  # RHS is sqm and floor2
  # RMSE is added to a vector on the i-th index for test set for model 1
  # RMSE is added to a vector on the i-th index for training set for model 1
  # bic is also added to a vector i-th index for model 1
  mlev1 <- lm(data = property_df_train, Ylev ~ sqm + floor2)
  #this assignment does not work: object 'rmse_mlev1' not found
  rmse_mlev1[i] <- RMSE_Lev(pred = predict(mlev1, newdata = property_df_test), obs = property_df_test$Ylev, na.rm = TRUE)
  rmse_train_mlev1[i] <- RMSE_Lev(pred = predict(mlev1, newdata = property_df_train), obs = property_df_train$Ylev, na.rm = TRUE)
  bic_mlev1[i] <- BIC(mlev1)
  
  # linear model with LHS is the predicted price per squaremeter level
  # RHS is sqm below and over 60 sqm, whwther ground floor, under 6th floor or over 6th floor
  # RMSE is added to a vector on the i-th index for test set for model 2
  # RMSE is added to a vector on the i-th index for training set for model 2
  # bic is also added to a vector i-th index for model 2
  mlev2 <- lm(data = property_df_train, Ylev ~ sqm_sp2060 + sqm_sp60p + floor0 + floor_sp05 + floor_sp6p)
  rmse_mlev2[i] <- RMSE_Lev(pred = predict(mlev2, newdata = property_df_test), obs = property_df_test$Ylev, na.rm = TRUE)
  rmse_train_mlev2[i] <- RMSE_Lev(pred = predict(mlev2, newdata = property_df_train), obs = property_df_train$Ylev, na.rm = TRUE)
  bic_mlev2[i] <- BIC(mlev2)
  
  
  # linear model with LHS is the predicted price per squaremeter level 
  # RHS is sqm below and over 60 sqm, whwther ground floor, under 6th floor or over 6th floor
  # also RHS is number of floors, condition (collapsed), lift dummy, balcony flag, airconditioning dummy, heating (collapsed) 
  # and whether this is a concrete flat
  # RMSE is added to a vector on the i-th index for test set for model 3
  # RMSE is added to a vector on the i-th index for training set for model 3
  # bic is also added to a vector i-th index for model 3
  mlev3 <- lm(data = property_df_train, Ylev ~ sqm_sp2060 + sqm_sp60p + floor0 + floor_sp05 + floor_sp6p + 
                number_of_floor + factor(condition_broad) + lift_d + hasbalcony + aircond_d + 
                factor(heating_broad) + concrete_blockflat_d)
  rmse_mlev3[i] <- RMSE_Lev(pred = predict(mlev3, newdata = property_df_test), obs = property_df_test$Ylev, na.rm = TRUE)
  rmse_train_mlev3[i] <- RMSE_Lev(pred = predict(mlev3, newdata = property_df_train), obs = property_df_train$Ylev, na.rm = TRUE)
  bic_mlev3[i] <- BIC(mlev3)
  
  # linear model with LHS is the predicted price per squaremeter level
  # RHS is sqm below and over 60 sqm, whwther ground floor, under 6th floor or over 6th floor
  # also RHS is number of floors, condition (collapsed), lift dummy, balcony flag, airconditioning dummy, heating (collapsed) 
  # and whether this is a concrete flat
  # plus interactions with floor
  # RMSE is added to a vector on the i-th index for test set for model 4
  # RMSE is added to a vector on the i-th index for training set for model 4
  # bic is also added to a vector i-th index for model 4
  mlev4 <- lm(data = property_df_train, Ylev ~ sqm_sp2060 + sqm_sp60p + floor0 + floor_sp05 + floor_sp6p + 
                number_of_floor + factor(condition_broad) + lift_d + hasbalcony + aircond_d + 
                factor(heating_broad) + concrete_blockflat_d + 
                balconyXfloor2 + liftXfloor2 + factor(floor)*concrete_blockflat_d + 
                factor(floor)*number_of_floor + factor(floor)*heating_broad)
  rmse_mlev4[i] <- RMSE_Lev(pred = predict(mlev4, newdata = property_df_test), obs = property_df_test$Ylev, na.rm = TRUE)
  rmse_train_mlev4[i] <- RMSE_Lev(pred = predict(mlev4, newdata = property_df_train), obs = property_df_train$Ylev, na.rm = TRUE)
  bic_mlev4[i] <- BIC(mlev4)
  
  
  # linear model with LHS is the predicted price per squaremeter level
  # RHS is sqm below and over 60 sqm, whwther ground floor, under 6th floor or over 6th floor
  # also RHS is number of floors, condition (collapsed), lift dummy, balcony flag, airconditioning dummy, heating (collapsed) 
  # and whether this is a concrete flat
  # plus interactions with new_flat flag
  # RMSE is added to a vector on the i-th index for test set for model 5
  # RMSE is added to a vector on the i-th index for training set for model 5
  # bic is also added to a vector i-th index for model 5
  mlev5 <- lm(data = property_df_train, Ylev ~ new_flat*sqm_sp2060 + new_flat*sqm_sp60p + new_flat*floor0 + 
                new_flat*floor_sp05 + new_flat*floor_sp6p + number_of_floor + 
                factor(condition_broad) + lift_d + hasbalcony  + 
                aircond_d + factor(heating_broad) + concrete_blockflat_d + 
                new_flat*number_of_floor + new_flat*lift_d + new_flat*liftXfloor2 + 
                new_flat*hasbalcony + new_flat*balconyXfloor2 + new_flat*aircond_d + 
                new_flat*factor(heating_broad) + new_flat*concrete_blockflat_d)
  rmse_mlev5[i] <- RMSE_Lev(pred = predict(mlev5, newdata = property_df_test), obs = property_df_test$Ylev, na.rm = TRUE)
  rmse_train_mlev5[i] <- RMSE_Lev(pred = predict(mlev5, newdata = property_df_train), obs = property_df_train$Ylev, na.rm = TRUE)
  bic_mlev5[i] <- BIC(mlev5)
  
  # linear model with LHS is the predicted price per squaremeter level
  # RHS is sqm below and over 60 sqm, whwther ground floor, under 6th floor or over 6th floor
  # also RHS is number of floors, condition (collapsed), lift dummy, balcony flag, airconditioning dummy, heating (collapsed) 
  # and whether this is a concrete flat
  # plus interactions with condition
  # RMSE is added to a vector on the i-th index for test set for model 6
  # RMSE is added to a vector on the i-th index for training set for model 6
  # bic is also added to a vector i-th index for model 6
  mlev6 <- lm(data = property_df_train, Ylev ~ new_flat*sqm_sp2060 + new_flat*sqm_sp60p + 
                new_flat*floor0 + new_flat*floor_sp05 + new_flat*floor_sp6p + number_of_floor + 
                factor(condition_broad) + lift_d + hasbalcony  + aircond_d + factor(heating_broad) + 
                concrete_blockflat_d + factor(condition_broad)*factor(floor2) + 
                factor(condition_broad)*number_of_floor + factor(condition_broad)*lift_d + 
                factor(condition_broad):liftXfloor2 + factor(condition_broad)*hasbalcony + 
                factor(condition_broad):balconyXfloor2 + factor(condition_broad)*aircond_d + 
                factor(condition_broad)*factor(heating_broad) + 
                factor(condition_broad)*concrete_blockflat_d + 
                factor(condition_broad)*factor(floor2)*number_of_floor + 
                factor(condition_broad)*factor(floor2)*factor(heating_broad) + 
                factor(condition_broad)*factor(floor2)*concrete_blockflat_d)
  rmse_mlev6[i] <- RMSE_Lev(pred = predict(mlev6, newdata = property_df_test), obs = property_df_test$Ylev, na.rm = TRUE)
  rmse_train_mlev6[i] <- RMSE_Lev(pred = predict(mlev6, newdata = property_df_train), obs = property_df_train$Ylev, na.rm = TRUE)
  bic_mlev6[i] <- BIC(mlev6)
  
  
  # linear model with LHS is the predicted price per squaremeter level
  # RHS is using sqm manual bins instead of the values, whwther ground floor, under 6th floor or over 6th floor
  # also RHS is number of floors, condition (collapsed), lift dummy, balcony flag, airconditioning dummy, heating (collapsed) 
  # and whether this is a concrete flat
  # plus interactions with condition
  # RMSE is added to a vector on the i-th index for test set for model 7
  # RMSE is added to a vector on the i-th index for training set for model 7
  # bic is also added to a vector i-th index for model 7
  mlev7 <- lm(data = property_df_train, Ylev ~ new_flat*factor(sqmcut) + number_of_floor + 
                factor(condition_broad) + lift_d + hasbalcony  + aircond_d + 
                factor(heating_broad) + concrete_blockflat_d + 
                factor(condition_broad)*factor(floor2) + 
                factor(condition_broad)*number_of_floor + factor(condition_broad)*lift_d + 
                factor(condition_broad)*liftXfloor2 + factor(condition_broad)*hasbalcony + 
                factor(condition_broad)*balconyXfloor2 + factor(condition_broad)*aircond_d + 
                factor(condition_broad)*factor(heating_broad) + factor(condition_broad)*concrete_blockflat_d + 
                factor(condition_broad)*factor(floor2)*number_of_floor + factor(condition_broad)*factor(floor2) * factor(heating_broad) + factor(condition_broad)*factor(floor2)*concrete_blockflat_d)
  rmse_mlev7[i] <- RMSE_Lev(pred = predict(mlev7, newdata = property_df_test), obs = property_df_test$Ylev, na.rm = TRUE)
  rmse_train_mlev7[i] <- RMSE_Lev(pred = predict(mlev7, newdata = property_df_train), obs = property_df_train$Ylev, na.rm = TRUE)
  bic_mlev7[i] <- BIC(mlev7)
  
  
  # linear model with LHS is the predicted price per squaremeter level
  # RHS is using sqm manual bins instead of the values, whwther ground floor, under 6th floor or over 6th floor
  # also RHS is number of floors, condition (collapsed), lift dummy, balcony flag, airconditioning dummy, heating (collapsed) 
  # and whether this is a concrete flat
  # plus interactions with condition
  # interactions with floor instead of floor2
  # RMSE is added to a vector on the i-th index for test set for model 8
  # RMSE is added to a vector on the i-th index for training set for model 8
  # bic is also added to a vector i-th index for model 8
  mlev8 <- lm(data = property_df_train, Ylev ~ new_flat*factor(sqmcut) + number_of_floor + 
                factor(condition_broad) + lift_d + hasbalcony  + aircond_d + factor(heating_broad) + 
                concrete_blockflat_d + factor(condition_broad)*factor(floor) + 
                factor(condition_broad)*number_of_floor + factor(condition_broad)*lift_d + 
                factor(condition_broad)*liftXfloor2 + factor(condition_broad)*hasbalcony + 
                factor(condition_broad)*balconyXfloor2 + factor(condition_broad)*aircond_d + 
                factor(condition_broad)*factor(heating_broad) + 
                factor(condition_broad)*concrete_blockflat_d + 
                factor(condition_broad)*factor(floor)*number_of_floor + 
                factor(condition_broad)*factor(floor) * factor(heating_broad) + 
                factor(condition_broad)*factor(floor)*concrete_blockflat_d)
  rmse_mlev8[i] <- RMSE_Lev(pred = predict(mlev8, newdata = property_df_test), obs = property_df_test$Ylev, na.rm = TRUE)
  rmse_train_mlev8[i] <- RMSE_Lev(pred = predict(mlev8, newdata = property_df_train), obs = property_df_train$Ylev, na.rm = TRUE)
  bic_mlev8[i] <- BIC(mlev8) 
  
  # linear model with LHS is the predicted price per squaremeter log
  # RHS is log sqm and floor2
  # RMSE is added to a vector on the i-th index for test set for model 1
  # RMSE is added to a vector on the i-th index for training set for model 1
  # bic is also added to a vector i-th index for model 1
  mlog1 <- lm(data = property_df_train, Ylog ~ ln_sqm + floor2)
  rmse_mlog1[i] <- rmse_log(mlog1)
  rmse_train_mlog1[i] <- rmse_train_log(mlog1)
  bic_mlog1[i]  <- BIC(mlog1)
  
  # linear model with LHS is the predicted price per squaremeter log
  # RHS is log sqm below and log over 60 sqm, whwther ground floor, under 6th floor or over 6th floor
  # RMSE is added to a vector on the i-th index for test set for model 2
  # RMSE is added to a vector on the i-th index for training set for model 2
  # bic is also added to a vector i-th index for model 2
  mlog2 <- lm(data = property_df_train, Ylog ~ lnsqm_sp2060 + lnsqm_sp60p + floor0 + floor_sp05 + floor_sp6p)
  rmse_mlog2[i] <- rmse_log(mlog2)
  rmse_train_mlog2[i] <- rmse_train_log(mlog2)
  bic_mlog2[i]  <- BIC(mlog2)
  
  # linear model with LHS is the predicted price per squaremeter log
  # RHS is log sqm below and log over 60 sqm, whwther ground floor, under 6th floor or over 6th floor
  # also RHS is number of floors, condition (collapsed), lift dummy, balcony flag, airconditioning dummy, heating (collapsed) 
  # and whether this is a concrete flat
  # plus interactions with floor
  # RMSE is added to a vector on the i-th index for test set for model 4
  # RMSE is added to a vector on the i-th index for training set for model 4
  # bic is also added to a vector i-th index for model 4
  mlog3 <- lm(data = property_df_train, Ylog ~ lnsqm_sp2060 + lnsqm_sp60p + floor0 + floor_sp05 + floor_sp6p + number_of_floor + factor(condition_broad) + lift_d + hasbalcony + aircond_d + factor(heating_broad) + concrete_blockflat_d)
  rmse_mlog3[i] <- rmse_log(mlog3)
  rmse_train_mlog3[i] <- rmse_train_log(mlog3)
  bic_mlog3[i]  <- BIC(mlog3)
  
  # linear model with LHS is the predicted price per squaremeter log
  # RHS is log sqm below and log over 60 sqm, whether ground floor, under 6th floor or over 6th floor
  # also RHS is number of floors, condition (collapsed), lift dummy, balcony flag, airconditioning dummy, heating (collapsed) 
  # and whether this is a concrete flat
  # plus interactions with floor
  # RMSE is added to a vector on the i-th index for test set for model 4
  # RMSE is added to a vector on the i-th index for training set for model 4
  # bic is also added to a vector i-th index for model 4
  mlog4 <- lm(data = property_df_train, Ylog ~ lnsqm_sp2060 + lnsqm_sp60p + floor0 + floor_sp05 + 
                floor_sp6p + number_of_floor + factor(condition_broad) + 
                lift_d + hasbalcony + aircond_d + factor(heating_broad) + 
                concrete_blockflat_d + balconyXfloor2 + 
                liftXfloor2 + factor(floor)*concrete_blockflat_d + factor(floor)*number_of_floor + 
                factor(floor)*heating_broad)
  rmse_mlog4[i] <- rmse_log(mlog4)
  rmse_train_mlog4[i] <- rmse_train_log(mlog4)
  bic_mlog4[i]  <- BIC(mlog4)
  
  # linear model with LHS is the predicted price per squaremeter log
  # RHS is log sqm below and log over 60 sqm, whwther ground floor, under 6th floor or over 6th floor
  # also RHS is number of floors, condition (collapsed), lift dummy, balcony flag, airconditioning dummy, heating (collapsed) 
  # and whether this is a concrete flat
  # plus interactions with new_flat flag
  # RMSE is added to a vector on the i-th index for test set for model 5
  # RMSE is added to a vector on the i-th index for training set for model 5
  # bic is also added to a vector i-th index for model 5
  mlog5 <- lm(data = property_df_train, Ylog ~ new_flat*lnsqm_sp2060 + new_flat*lnsqm_sp60p + new_flat*floor0 + new_flat*floor_sp05 + new_flat*floor_sp6p + number_of_floor + factor(condition_broad) + lift_d + hasbalcony + aircond_d + factor(heating_broad) + concrete_blockflat_d + new_flat:number_of_floor + new_flat:lift_d + new_flat*liftXfloor2 + new_flat:hasbalcony + new_flat*balconyXfloor2 + new_flat:aircond_d + new_flat:factor(heating_broad) + new_flat:concrete_blockflat_d)
  rmse_mlog5[i] <- rmse_log(mlog5)
  rmse_train_mlog5[i] <- rmse_train_log(mlog5)
  bic_mlog5[i]  <- BIC(mlog5)
  
  # linear model with LHS is the predicted price per squaremeter log
  # RHS is log sqm below and log over 60 sqm, whwther ground floor, under 6th floor or over 6th floor
  # also RHS is number of floors, condition (collapsed), lift dummy, balcony flag, airconditioning dummy, heating (collapsed) 
  # and whether this is a concrete flat
  # plus interactions with condition
  # RMSE is added to a vector on the i-th index for test set for model 6
  # RMSE is added to a vector on the i-th index for training set for model 6
  # bic is also added to a vector i-th index for model 6
  mlog6 <- lm(data = property_df_train, Ylog ~ new_flat*lnsqm_sp2060 + new_flat*lnsqm_sp60p + 
                new_flat*floor0 + new_flat*floor_sp05 + new_flat*floor_sp6p + 
                number_of_floor + factor(condition_broad) + lift_d + hasbalcony + 
                aircond_d + factor(heating_broad) + concrete_blockflat_d + 
                factor(condition_broad):factor(floor2) + 
                factor(condition_broad):number_of_floor + 
                factor(condition_broad):lift_d + factor(condition_broad):liftXfloor2 + 
                factor(condition_broad):hasbalcony + factor(condition_broad):balconyXfloor2 + 
                factor(condition_broad):aircond_d + factor(condition_broad):factor(heating_broad) + 
                factor(condition_broad):concrete_blockflat_d + 
                factor(condition_broad):factor(floor2):number_of_floor + 
                factor(condition_broad):factor(floor2):factor(heating_broad) + 
                factor(condition_broad):factor(floor2):concrete_blockflat_d)
  rmse_mlog6[i] <- rmse_log(mlog6)
  rmse_train_mlog6[i] <- rmse_train_log(mlog6)
  bic_mlog6[i]  <- BIC(mlog6)
  
  # linear model with LHS is the predicted price per squaremeter log
  # RHS is using log sqm manual bins instead of the values, whwther ground floor, under 6th floor or over 6th floor
  # also RHS is number of floors, condition (collapsed), lift dummy, balcony flag, airconditioning dummy, heating (collapsed) 
  # and whether this is a concrete flat
  # plus interactions with condition
  # RMSE is added to a vector on the i-th index for test set for model 7
  # RMSE is added to a vector on the i-th index for training set for model 7
  # bic is also added to a vector i-th index for model 7
  mlog7 <- lm(data = property_df_train, Ylog ~ new_flat*lnsqmcut + number_of_floor + factor(condition_broad) + lift_d + hasbalcony + aircond_d + factor(heating_broad) + concrete_blockflat_d + factor(condition_broad):factor(floor2) + factor(condition_broad):number_of_floor + factor(condition_broad):lift_d + factor(condition_broad):liftXfloor2 + factor(condition_broad):hasbalcony + factor(condition_broad):balconyXfloor2 + factor(condition_broad):aircond_d + factor(condition_broad):factor(heating_broad) + factor(condition_broad):concrete_blockflat_d + factor(condition_broad):factor(floor2):number_of_floor + factor(condition_broad):factor(floor2):factor(heating_broad) + factor(condition_broad):factor(floor2):concrete_blockflat_d)
  rmse_mlog7[i] <- rmse_log(mlog7)
  rmse_train_mlog7[i] <- rmse_train_log(mlog7)
  bic_mlog7[i]  <- BIC(mlog7)
  
  
  # linear model with LHS is the predicted price per squaremeter log
  # RHS is using log sqm manual bins instead of the values, whwther ground floor, under 6th floor or over 6th floor
  # also RHS is number of floors, condition (collapsed), lift dummy, balcony flag, airconditioning dummy, heating (collapsed) 
  # and whether this is a concrete flat
  # plus interactions with condition
  # interactions with floor instead of floor2
  # RMSE is added to a vector on the i-th index for test set for model 8
  # RMSE is added to a vector on the i-th index for training set for model 8
  # bic is also added to a vector i-th index for model 8
  mlog8 <- lm(data = property_df_train, Ylog ~ new_flat*lnsqmcut + number_of_floor + factor(condition_broad) + lift_d + hasbalcony + aircond_d + factor(heating_broad) + concrete_blockflat_d + factor(condition_broad)*factor(floor) + factor(condition_broad)*number_of_floor + factor(condition_broad)*lift_d + factor(condition_broad)*liftXfloor2 + factor(condition_broad)*hasbalcony + factor(condition_broad)*balconyXfloor2 + factor(condition_broad)*aircond_d + factor(condition_broad)*factor(heating_broad) + factor(condition_broad)*concrete_blockflat_d + factor(condition_broad)*factor(floor)*number_of_floor + factor(condition_broad)*factor(floor)*factor(heating_broad) + factor(condition_broad)*factor(floor)*concrete_blockflat_d)
  rmse_mlog8[i] <- rmse_log(mlog8)
  rmse_train_mlog8[i] <- rmse_train_log(mlog8)
  bic_mlog8[i]  <- BIC(mlog8)
}


#organizing RMSE into a data frame
#here the code is broken, most possibly because of the extra comma
models_lev_rmse <- data.frame(matrix(, nrow=5, ncol=8))
#assigning colnames
colnames(models_lev_rmse) <- c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5", "Model 6", "Model 7", "Model 8")
#assigning rownames
rownames(models_lev_rmse) <- c("BIC", "RMSEin", "RMSEtestCV", "RMSEtest1", "RMSE_max")

#calculating RMSE from the mean of the bic and rmse vectors for all 8 level models
models_lev_rmse[,1] <- c(mean(bic_mlev1), mean(rmse_train_mlev1), mean(rmse_mlev1), rmse_mlev1[1], max(rmse_mlev1))
models_lev_rmse[,2] <- c(mean(bic_mlev2), mean(rmse_train_mlev2), mean(rmse_mlev2), rmse_mlev2[1], max(rmse_mlev2))
models_lev_rmse[,3] <- c(mean(bic_mlev3), mean(rmse_train_mlev3), mean(rmse_mlev3), rmse_mlev3[1], max(rmse_mlev3))
models_lev_rmse[,4] <- c(mean(bic_mlev4), mean(rmse_train_mlev4), mean(rmse_mlev4), rmse_mlev4[1], max(rmse_mlev4))
models_lev_rmse[,5] <- c(mean(bic_mlev5), mean(rmse_train_mlev5), mean(rmse_mlev5), rmse_mlev5[1], max(rmse_mlev5))
models_lev_rmse[,6] <- c(mean(bic_mlev6), mean(rmse_train_mlev6), mean(rmse_mlev6), rmse_mlev6[1], max(rmse_mlev6))
models_lev_rmse[,7] <- c(mean(bic_mlev7), mean(rmse_train_mlev7), mean(rmse_mlev7), rmse_mlev7[1], max(rmse_mlev7))
models_lev_rmse[,8] <- c(mean(bic_mlev8), mean(rmse_train_mlev8), mean(rmse_mlev8), rmse_mlev8[1], max(rmse_mlev8))

#checking on the matrix
models_lev_rmse

#organizing results to another matrix, error here as well, hopefully clarified on seminar
models_log_rmse <- data.frame(matrix(, nrow=5, ncol=9)) 
#assigning column names
colnames(models_log_rmse) <- c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5", "Model 6", "Model 7", "Model 8", "Model 8 (w/o 1st set)")
#assigning row names
rownames(models_log_rmse) <- c("BIC", "RMSEin", "RMSEtestCV", "RMSEtest1", "RMSE_max")

#calculating RMSE from the mean of the bic and rmse vectors for all 8 log models
models_log_rmse[,1] <- c(mean(bic_mlog1), mean(rmse_train_mlog1), mean(rmse_mlog1), rmse_mlog1[1], max(rmse_mlog1))
models_log_rmse[,2] <- c(mean(bic_mlog2), mean(rmse_train_mlog2), mean(rmse_mlog2), rmse_mlog2[1], max(rmse_mlog2))
models_log_rmse[,3] <- c(mean(bic_mlog3), mean(rmse_train_mlog3), mean(rmse_mlog3), rmse_mlog3[1], max(rmse_mlog3))
models_log_rmse[,4] <- c(mean(bic_mlog4), mean(rmse_train_mlog4), mean(rmse_mlog4), rmse_mlog4[1], max(rmse_mlog4))
models_log_rmse[,5] <- c(mean(bic_mlog5), mean(rmse_train_mlog5), mean(rmse_mlog5), rmse_mlog5[1], max(rmse_mlog5))
models_log_rmse[,6] <- c(mean(bic_mlog6), mean(rmse_train_mlog6), mean(rmse_mlog6), rmse_mlog6[1], max(rmse_mlog6))
models_log_rmse[,7] <- c(mean(bic_mlog7), mean(rmse_train_mlog7), mean(rmse_mlog7), rmse_mlog7[1], max(rmse_mlog7))
models_log_rmse[,8] <- c(mean(bic_mlog8), mean(rmse_train_mlog8), mean(rmse_mlog8), rmse_mlog8[1], max(rmse_mlog8))
models_log_rmse[,9] <- c(mean(bic_mlog8), mean(rmse_train_mlog8), mean(rmse_mlog8[2:5]), rmse_mlog8[2], max(rmse_mlog8[2:5]))

#checking matrix
models_log_rmse

#summary statistics for the values of predicsted price per squarementer for 5th fold
summary(property_df[folds == 5,"Ylev"])

#linear model for the other 4 folds, with log price for sqm, and log sqm values for below and over 60sqm
logmod4 <- lm(data = property_df[folds != 5,], Ylog ~ lnsqm_sp2060 + lnsqm_sp60p + floor0 + floor_sp05 + floor_sp6p + number_of_floor + factor(condition_broad) + lift_d + hasbalcony + aircond_d + factor(heating_broad) + concrete_blockflat_d + balconyXfloor2 + liftXfloor2 + factor(floor)*concrete_blockflat_d + factor(floor)*number_of_floor + factor(floor)*heating_broad)
#linear model for the other 4 folds, with level price for sqm, and level sqm values for below and over 60sqm
levmod4 <- lm(data = property_df[folds != 5,], Ylev ~ sqm_sp2060 + sqm_sp60p + floor0 + floor_sp05 + floor_sp6p + number_of_floor + factor(condition_broad) + lift_d + hasbalcony + aircond_d + factor(heating_broad) + concrete_blockflat_d + balconyXfloor2 + liftXfloor2 + factor(floor)*concrete_blockflat_d + factor(floor)*number_of_floor + factor(floor)*heating_broad)

#predicted values on fold 5 for log
ylog_m4_t5 <- exp(predict(logmod4, newdata = property_df[folds == 5,]))
#error of the prediction on 5th fold
elog_m4_t5 <- ylog_m4_t5 - property_df[folds == 5, "Ylev"]
#predicted values on fold 5 for level model
ylev_m4_t5 <- predict(levmod4, newdata = property_df[folds == 5,])

#input for model error visualization
data_graphs_results <- as.data.frame(cbind(ylog_m4_t5, 
                                           elog_m4_t5, 
                                           ylev_m4_t5, 
                                           Ylev = property_df[folds == 5, "Ylev"]))

#visualizing model error on a scatterplot, x=predicted value for logs, y residual error
png(filename="scatter_res4.png", res = 200, width = 1200, height = 800)
ggplot(data = data_graphs_results, aes(x = ylog_m4_t5, y =  elog_m4_t5)) +
  geom_point(size = 2.5, colour = "black")
dev.off()

#visualizing model error on a scatterplot, x=predicted value for logs, 
#only for y residual error > -500 values
png(filename="scatter_res4_upd.png", res = 200, width = 1200, height = 800)
ggplot(data = data_graphs_results[elog_m4_t5 > -500,], aes(x = ylog_m4_t5, y =  elog_m4_t5)) +
  geom_point(size = 2.5, colour = "black")
dev.off()

#plotting true vs predicted values, prediction linear fit with green line, using log model
g <- ggplot(data = data_graphs_results, aes(x = Ylev, y =  ylog_m4_t5)) +
  geom_point(shape = 1, colour = "blue") + 
  geom_smooth(method=lm, se=FALSE, colour = "green", size = 3.5, linetype = 2) 

#plotting true vs predicted values, prediction linear fit with green line, using log model
# and adding level model points and line
g <- g + geom_point(data = data_graphs_results, aes(x = Ylev, y =  ylev_m4_t5), shape = 4, colour = "red") +
  geom_smooth(method=lm, se=FALSE, colour = "orange") + geom_vline(xintercept = mean(data_graphs_results$Ylev)) +
  ylab("Fitted values") + xlab("True values")

png(filename="log_vs_lin_all.png", res = 200, width = 1200, height = 800)
plot(g)
dev.off()

#prediction standard deviation
YlevSD <- sd(data_graphs_results$Ylev)
#prediction mean
YlevMEAN <- mean(data_graphs_results$Ylev)

#plotting only observations where the predicted level value is within the plusminus
# 2SE distance from mean (CI=95%) on the ylevel predictions on x axis and log model prediction on y
g <- ggplot(data = data_graphs_results[data_graphs_results$Ylev >= YlevMEAN - 2*YlevSD & 
                                         data_graphs_results$Ylev <= YlevMEAN + 2*YlevSD,], 
            aes(x = Ylev, y =  ylog_m4_t5)) +
  geom_point(shape = 1, colour = "blue") + 
  geom_smooth(method=lm, se=FALSE, colour = "green", size = 3.5, linetype = 2) 

#plotting only observations where the predicted level value is within the plusminus
# 2SE distance from mean (CI=95%) on the ylevel predictions on x axis and log model prediction on y
# and adding the pointd of the level model with red, plus fitted line with orange
g <- g + geom_point(data = data_graphs_results[data_graphs_results$Ylev >= YlevMEAN - 2*YlevSD & 
                                                 data_graphs_results$Ylev <= YlevMEAN + 2*YlevSD,], 
                    aes(x = Ylev, y =  ylev_m4_t5), shape = 4, colour = "red") +
  geom_smooth(method=lm, se=FALSE, colour = "orange") + 
  geom_vline(xintercept = mean(data_graphs_results$Ylev)) +
  ylab("Fitted values") + xlab("True values")

png(filename="log_vs_lin.png", res = 200, width = 1200, height = 800)
plot(g)
dev.off()

#plotting only observations where the predicted level value is within the plusminus
# 2SE distance from mean (CI=95%) on the ylevel predictions on x axis and log model prediction on y
# additionally plotting the 45ndegree with label
png(filename="log_vs_45.png", res = 200, width = 1500, height = 1000)
ggplot(data = data_graphs_results[data_graphs_results$Ylev >= YlevMEAN - 2*YlevSD & 
                                    data_graphs_results$Ylev <= YlevMEAN + 2*YlevSD,], 
       aes(x = Ylev, y =  ylog_m4_t5)) +
  geom_point(shape = 1, colour = "blue") + 
  geom_smooth(method=lm, se=FALSE, colour = "green", size = 2, linetype = 2) + geom_line(aes(x = Ylev, y =  Ylev), colour = "red") +
  annotate("text", x = 740, y = 800, label = "45 degree line", colour = "red", size = 3) +
  annotate("text", x = 850, y = 700, label = "Fitted values", colour = "green", size = 3)
dev.off() 

