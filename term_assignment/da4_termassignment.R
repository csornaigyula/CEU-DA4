rm(list=ls())
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
png(filename="Graph_logprice_logarea.png", res = 200, width = 800, height = 800)
ggplot(data = property_df[property_df$sqm < 180,], 
       aes(x = ln_sqm, y = ln_psqm)) + 
    labs(
      title='Dependency between log flat size\nand log price per squaremeter',
      y='Log price per squaremeter',
      x='Log area (squaremeter)',
      caption='Flat sizes over 180 sqm are excluded from analysis'
    )+
    theme_bw()+
    geom_smooth(colour = "darkred")
dev.off()

#plotting the linear smoothing trendline  psqm over  sqm 
#for only those flats, where size is below 180 sqm
png(filename="Graph_lev_lev.png", res = 200, width = 800, height = 800)
ggplot(data = property_df[property_df$sqm < 180,], aes(x = sqm, y = psqm))+
  labs(
    title='Dependency between flat size\nand price per squaremeter',
    y='price per squaremeter',
    x='area (squaremeter)',
    caption='Flat sizes over 180 sqm are excluded from analysis'
  )+
  theme_bw()+
  geom_smooth(colour = "darkred")
dev.off()

png(filename="Graph_floor_lev_lev.png", res = 200, width = 800, height = 800)
ggplot(data = property_df[property_df$sqm < 180,], aes(x = floor, y = psqm))+
  labs(
    title='Dependency between flat location(floor)\nand price per squaremeter',
    y='price per squaremeter',
    x='which floor flat is on',
    caption='Flat sizes over 180 sqm are excluded from analysis'
  )+
  theme_bw()+
  geom_smooth(colour = "darkred")
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

####################################################
##Engineering new variables

#frequency table of orientation
ddply(property_df, .(orientation), summarize, freq = length(orientation), 
      mean_psqm = round(mean(psqm), digits = 3))

#here the only thing to be done is factoring in NA
property_df$orientation <- as.character(property_df$orientation)

property_df$orientation[is.na(property_df$orientation)] <- "unknown"
property_df$orientation <- factor(property_df$orientation, levels = 
                                    c("unknown", "East", "North", "North-East", "North-West",
                                      "South","South-East", "South-West", "West"))

#...successfully
ddply(property_df, .(orientation), summarize, freq = length(orientation), 
      mean_psqm = round(mean(psqm), digits = 3))

#frequency table of parking
ddply(property_df, .(parking), summarize, freq = length(parking), 
      mean_psqm = round(mean(psqm), digits = 3))

#here the only thing to be done is factoring in NA
property_df$parking <- as.character(property_df$parking)

property_df$parking[is.na(property_df$parking)] <- "unknown"
property_df$parking <- factor(property_df$parking, levels = 
                                    c("unknown", "garage- for sale", "garage- included in the price", 
                                      "on street parking - for pay", "on street parking -for free",
                                      "outdoor parking spot- for sale","outdoor parking spot- included in the price", 
                                      "parking space in undergound garage- for sale", 
                                      "parking space in underground garage- included in the price"))

#...successfully
ddply(property_df, .(parking), summarize, freq = length(parking), 
      mean_psqm = round(mean(psqm), digits = 3))

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


##########################################################
# REGRESSIONS
##########################################################

# SECTION01: in sample multiple regression baseline

osreg01 <- lm(data = property_df, 
               ln_psqm ~ lnsqm_sp2060 + lnsqm_sp60p +  heating_broad + 
                 condition_broad+
                 parking+  hasbalcony+ concrete_blockflat_d+
                 floor_sp05+floor_sp6p+ view+ balcony+lift_d+aircond_d)
osreg02 <- lm(data = property_df, 
              ln_psqm ~ lnsqm_sp2060 + lnsqm_sp60p +  heating_broad )
osreg03 <- lm(data = property_df, 
              psqm ~ sqm_sp2060 + sqm_sp60p +  heating_broad + 
                condition_broad+
                parking+  hasbalcony+ concrete_blockflat_d+
                floor_sp05+floor_sp6p+ view+ balcony+lift_d+aircond_d)
osreg04 <- lm(data = property_df, 
              psqm ~ sqm_sp2060 + sqm_sp60p +  heating_broad )
stargazer_r( list(osreg01, osreg02, osreg03, osreg04), 
             digits=2,single.row = TRUE, out="osreg0102.html")

RMSE_Lev <- function(pred, obs, na.rm = FALSE){
  sqrt(
    mean(
      (obs - pred)^2, 
      na.rm = na.rm
    )
  )
}

RMSE_Log <- function(pred, obs, na.rm = TRUE){
  sqrt(
    mean(
      (exp(obs) - exp(pred))^2, na.rm = na.rm
    )
  )
}




RMSE01 <- round(
  RMSE_Log(
    pred = predict(
      osreg01, newdata = property_df[property_df$train == 0,]
      ), 
    obs = property_df$ln_psqm[property_df$train == 0], 
    na.rm = TRUE), digits = 3)

RMSE02 <- round(
  RMSE_Log(
    pred = predict(
      osreg02, newdata = property_df[property_df$train == 0,]
    ), 
    obs = property_df$ln_psqm[property_df$train == 0], 
    na.rm = TRUE), digits = 3)

RMSE03 <- round(
  RMSE_Lev(
    pred = predict(
      osreg03, newdata = property_df[property_df$train == 0,]
    ), 
    obs = property_df$psqm[property_df$train == 0], 
    na.rm = TRUE), digits = 3)

sqmv <- property_df[property_df$train == 0,]$sqm
predv <- predict(
  osreg03, newdata = property_df[property_df$train == 0,]
)
obsv <- property_df[property_df$train == 0,]$psqm
sqerrv <- (predv - obsv)^2
errv <- (predv - obsv)

sqmvm <-matrix(sqmv, nrow=length(sqmv), ncol=1)
errvm <-matrix(errv, nrow=length(errv), ncol=1)

?matrix
vizmat <- cbind(sqmvm,errvm)
colnames(vizmat) <- c("sqm", "err")
vizmat <-data.frame(vizmat)


  

png(filename="Errdistr_insample.png", res = 200, width = 800, height = 800)
ggplot(data=vizmat)+
  aes( x=sqm,y=err)+
  geom_point(size = 1, colour = "indianred")+
  labs(
    title='Distribution of residual error\nper squaremeter',
    subtitle='Out of sample multiple regression',
    y='Difference between predicted\nand observed price',
    x='area (squaremeter)'
  )+
  theme_bw()
dev.off()


RMSE04 <- round(
  RMSE_Lev(
    pred = predict(
      osreg04, newdata = property_df[property_df$train == 0,]
    ), 
    obs = property_df$psqm[property_df$train == 0], 
    na.rm = TRUE), digits = 3)



#SECTION02: Out of sample multiple regression without crossvalidation and interactions
#Argument against log regression

osreg05 <- lm(data = property_df[property_df$train == 1,], 
              psqm ~ sqm_sp2060 + sqm_sp60p +  heating_broad + 
                condition_broad+
                parking+  hasbalcony+ concrete_blockflat_d+
                floor_sp05+floor_sp6p+ view+ balcony+lift_d+aircond_d)
stargazer_r( list(osreg05, osreg03), 
             digits=2,single.row = TRUE, out="osreg0503.html")


RMSE05 <- round(
  RMSE_Lev(
    pred = predict(
      osreg05, newdata = property_df[property_df$train == 0,]
    ), 
    obs = property_df$psqm[property_df$train == 0], 
    na.rm = TRUE), digits = 3)

predv <- predict(
  osreg05, newdata = property_df[property_df$train == 0,]
)
errv <- (predv - obsv)

sqmvm <-matrix(sqmv, nrow=length(sqmv), ncol=1)
errvm <-matrix(errv, nrow=length(errv), ncol=1)

vizmat <- cbind(sqmvm,errvm)
colnames(vizmat) <- c("sqm", "err")
vizmat <-data.frame(vizmat)

png(filename="Errdistr_oosample.png", res = 200, width = 800, height = 800)
ggplot(data=vizmat)+
  aes( x=sqm,y=err)+
  geom_point(size = 1, colour = "indianred")+
  labs(
    title='Distribution of residual error\nper squaremeter',
    subtitle='Out of sample multiple regression',
    y='Difference between predicted\nand observed price',
    x='area (squaremeter)'
  )+
  theme_bw()
dev.off()

#SECTION3: Out of sample multiple regression with interactions with
# 1- lift
# 2- new flat
# 3- using liftXfloor2 already interacted variable
osreg06 <- lm(data = property_df[property_df$train == 1,], 
              psqm ~ sqm_sp2060 + sqm_sp60p +  heating_broad + 
                condition_broad+
                parking+  hasbalcony+ concrete_blockflat_d+
                floor_sp05+floor_sp6p+ view+ balcony+lift_d+aircond_d+
                liftXfloor2+
                floor_sp05*lift_d+floor_sp6p*lift_d+
                new_flat*hasbalcony+new_flat*aircond_d+new_flat*view+
                  new_flat*sqm_sp2060+new_flat*sqm_sp60p)
stargazer_r( list(osreg06, osreg05, osreg03), 
             digits=2,single.row = TRUE, out="osreg060503.html")

RMSE06 <- round(
  RMSE_Lev(
    pred = predict(
      osreg06, newdata = property_df[property_df$train == 0,]
    ), 
    obs = property_df$psqm[property_df$train == 0], 
    na.rm = TRUE), digits = 3)

predv <- predict(
  osreg06, newdata = property_df[property_df$train == 0,]
)
errv <- (predv - obsv)

sqmvm <-matrix(sqmv, nrow=length(sqmv), ncol=1)
errvm <-matrix(errv, nrow=length(errv), ncol=1)

vizmat <- cbind(sqmvm,errvm)
colnames(vizmat) <- c("sqm", "err")
vizmat <-data.frame(vizmat)

png(filename="Errdistr_oosample_interact.png", res = 200, width = 800, height = 800)
ggplot(data=vizmat)+
  aes( x=sqm,y=err)+
  geom_point(size = 1, colour = "indianred")+
  labs(
    title='Distribution of residual error\nper squaremeter',
    subtitle='Out of sample multiple regression\n w/ interactions',
    y='Difference between predicted\nand observed price',
    x='area (squaremeter)'
  )+
  theme_bw()
dev.off()

mean(property_df$psqm)
property_df$lnpsqm <- log(property_df$psqm)

#SECTION4: Even more interactions, log function
osreg07 <- lm(data = property_df[property_df$train == 1,], 
              ln_psqm ~ new_flat*lnsqm_sp2060 + new_flat*lnsqm_sp60p + 
              new_flat*floor0 + new_flat*floor_sp05 + new_flat*floor_sp6p + 
              number_of_floor + factor(condition_broad) + lift_d + hasbalcony + 
              aircond_d + factor(heating_broad) + concrete_blockflat_d + 
              factor(orientation)+factor(view)+factor(parking)+
              factor(condition_broad):factor(floor2) + 
              factor(condition_broad):number_of_floor + 
              factor(condition_broad):lift_d + factor(condition_broad):liftXfloor2 + 
              factor(condition_broad):hasbalcony + factor(condition_broad):balconyXfloor2 + 
              factor(condition_broad):aircond_d + factor(condition_broad):factor(heating_broad) + 
              factor(condition_broad):concrete_blockflat_d + 
              factor(condition_broad):factor(floor2):number_of_floor + 
              factor(condition_broad):factor(floor2):factor(heating_broad) + 
              factor(condition_broad):factor(floor2):concrete_blockflat_d +
              factor(condition_broad):factor(view):factor(floor2)+
              factor(condition_broad):factor(floor2):factor(parking)+
              factor(condition_broad):factor(floor2):factor(orientation)
              )

stargazer_r( list(osreg07,osreg06, osreg05, osreg03), 
             digits=2,single.row = TRUE, out="osreg07060503.html")

RMSE07 <- round(
  RMSE_Log(
    pred = predict(
      osreg07, newdata = property_df[property_df$train == 0,]
    ), 
    obs = property_df$ln_psqm[property_df$train == 0], 
    na.rm = TRUE), digits = 3)


  

predv <- predict(
  osreg07, newdata = property_df[property_df$train == 0,]
)
errv <- (predv - obsv)

sqmvm <-matrix(sqmv, nrow=length(sqmv), ncol=1)
errvm <-matrix(errv, nrow=length(errv), ncol=1)

vizmat <- cbind(sqmvm,errvm)
colnames(vizmat) <- c("sqm", "err")
vizmat <-data.frame(vizmat)

png(filename="Errdistr_oosample_lotsofinteract.png", res = 200, width = 800, height = 800)
ggplot(data=vizmat)+
  aes( x=sqm,y=err)+
  geom_point(size = 1, colour = "indianred")+
  labs(
    title='Distribution of residual error\nper squaremeter',
    subtitle='Out of sample multiple regression\n w/ many interactions',
    y='Difference between predicted\nand observed price',
    x='area (squaremeter)'
  )+
  theme_bw()
dev.off()

#SECTION 05: Cross validated model with interaction 
#using level models
k <- 5
folds <- sample(rep(1:k, nrow(property_df)/k))
RMSE101 <-NULL
RMSE102 <-NULL
RMSE103 <-NULL
RMSE104 <-NULL
RMSE105 <-NULL

RMSE101_TST <-NULL
RMSE102_TST <-NULL
RMSE103_TST <-NULL
RMSE104_TST <-NULL
RMSE105_TST <-NULL

BIC101 <-NULL
BIC102 <-NULL
BIC103 <-NULL
BIC104 <-NULL
BIC105 <-NULL

for(i in 1:k){
  #... the training set is every fold, which is not the actual
  property_df_train <- property_df[folds!=i,]
  #... and the test set is the actual
  property_df_test <- property_df[folds == i,]
  
  cvreg101 <- lm(data = property_df_train, psqm ~ sqm_sp2060 + sqm_sp60p + floor0 + floor_sp05 + floor_sp6p + 
                number_of_floor + factor(condition_broad) + lift_d + hasbalcony + aircond_d + 
                factor(heating_broad) + concrete_blockflat_d + 
                balconyXfloor2 + liftXfloor2 + factor(floor)*concrete_blockflat_d + 
                factor(floor)*number_of_floor + factor(floor)*heating_broad+
                factor(view)+
                factor(parking)+
                factor(orientation))
  RMSE101_TST[i] <- RMSE_Lev(
    pred = predict(cvreg101, newdata = property_df_test), 
    obs = property_df_test$psqm, 
    na.rm = TRUE)
  RMSE101[i] <- RMSE_Lev(
    pred = predict(cvreg101, newdata = property_df_train), 
    obs = property_df_train$psqm, 
    na.rm = TRUE)
  BIC101[i] <- BIC(cvreg101)
  
  cvreg102 <- lm(data = property_df_train, psqm ~ new_flat*sqm_sp2060 + new_flat*sqm_sp60p + new_flat*floor0 + 
                new_flat*floor_sp05 + new_flat*floor_sp6p + number_of_floor + 
                factor(condition_broad) + lift_d + hasbalcony  + 
                aircond_d + factor(heating_broad) + concrete_blockflat_d + 
                new_flat*number_of_floor + new_flat*lift_d + new_flat*liftXfloor2 + 
                new_flat*hasbalcony + new_flat*balconyXfloor2 + new_flat*aircond_d + 
                new_flat*factor(heating_broad) + new_flat*concrete_blockflat_d+
                  factor(view)+
                  factor(parking)+
                  factor(orientation))
  RMSE102_TST[i] <- RMSE_Lev(
    pred = predict(cvreg102, newdata = property_df_test), 
    obs = property_df_test$psqm, 
    na.rm = TRUE)
  RMSE102[i] <- RMSE_Lev(
    pred = predict(cvreg102, newdata = property_df_train), 
    obs = property_df_train$psqm, 
    na.rm = TRUE)
  BIC102[i] <- BIC(cvreg102)
  
  cvreg103 <- lm(data = property_df_train, psqm ~ new_flat*sqm_sp2060 + new_flat*sqm_sp60p + 
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
                factor(condition_broad)*factor(floor2)*concrete_blockflat_d+
                  factor(view)+
                  factor(parking)+
                  factor(orientation))
  RMSE103_TST[i] <- RMSE_Lev(
    pred = predict(cvreg103, newdata = property_df_test), 
    obs = property_df_test$psqm, 
    na.rm = TRUE)
  RMSE103[i] <- RMSE_Lev(
    pred = predict(cvreg103, newdata = property_df_train), 
    obs = property_df_train$psqm, 
    na.rm = TRUE)
  BIC103[i] <- BIC(cvreg103)
  
  
  cvreg104 <- lm(data = property_df_train, psqm ~ new_flat*factor(sqmcut) + number_of_floor + 
                factor(condition_broad) + lift_d + hasbalcony  + aircond_d + 
                factor(heating_broad) + concrete_blockflat_d + 
                factor(condition_broad)*factor(floor2) + 
                factor(condition_broad)*number_of_floor + factor(condition_broad)*lift_d + 
                factor(condition_broad)*liftXfloor2 + factor(condition_broad)*hasbalcony + 
                factor(condition_broad)*balconyXfloor2 + factor(condition_broad)*aircond_d + 
                factor(condition_broad)*factor(heating_broad) + factor(condition_broad)*concrete_blockflat_d + 
                factor(condition_broad)*factor(floor2)*number_of_floor + 
                  factor(condition_broad)*factor(floor2) * factor(heating_broad) + 
                  factor(condition_broad)*factor(floor2)*concrete_blockflat_d+
                  factor(view)+
                  factor(parking)+
                  factor(orientation))
  RMSE104_TST[i] <- RMSE_Lev(
    pred = predict(cvreg104, newdata = property_df_test), 
    obs = property_df_test$psqm, 
    na.rm = TRUE)
  RMSE104[i] <- RMSE_Lev(
    pred = predict(cvreg104, newdata = property_df_train), 
    obs = property_df_train$psqm, 
    na.rm = TRUE)
  BIC104[i] <- BIC(cvreg104)
  
  
}


#organizing RMSE into a data frame
#here the code is broken, most possibly because of the extra comma
models_lev_rmse <- data.frame(matrix( nrow=5, ncol=4))
#assigning colnames
colnames(models_lev_rmse) <- c("Model 1", "Model 2", "Model 3", "Model 4")
#assigning rownames
rownames(models_lev_rmse) <- c("BIC", "RMSE insample", "RMSE test CV", "RMSE min", "RMSE max")

#calculating RMSE from the mean of the bic and rmse vectors for all 8 level models
models_lev_rmse[,1] <- c(mean(BIC101), mean(RMSE101), mean(RMSE101_TST),min(RMSE101_TST) , max(RMSE101_TST))
models_lev_rmse[,2] <- c(mean(BIC102), mean(RMSE102), mean(RMSE102_TST),min(RMSE102_TST) , max(RMSE102_TST))
models_lev_rmse[,3] <- c(mean(BIC103), mean(RMSE103), mean(RMSE103_TST),min(RMSE103_TST) , max(RMSE103_TST))
models_lev_rmse[,4] <- c(mean(BIC104), mean(RMSE104), mean(RMSE104_TST),min(RMSE104_TST) , max(RMSE104_TST))
models_lev_rmse

#6: Now random forest
library(randomForest)
library(ROCR)
library(pander)
rf_df<-property_df
rf_df$description <- NULL
rf_df_work <- rf_df[,c(1,2,3,15,19, 20,21,22,24,25,32,37,38,39,40,46)]
rf_df_work <- rf_df_work[!is.na(rf_df_work$psqm),]
rf_df_work$rnd <-runif(nrow(rf_df_work))
rf_df_work <- rf_df_work[order(rf_df_work$rnd),]
rf_df_work$rnd <- NULL
rf_df_work$liftxfloor <- NULL
#70% training set 30% test set
trainset <- rf_df_work[0:round( nrow(rf_df_work) * 0.7 ),]
testset <- rf_df_work[(round( nrow(rf_df_work) * 0.7 )+1) : nrow(rf_df_work),]
rfmod_t100 <- randomForest(psqm ~ .,data=trainset,ntree=100, importance=TRUE)
?randomForest
pander(rfmod_t100)
pred <- predict(rfmod_t100, testset)

RMSE_RFT <- RMSE_Lev(
  pred = predict(rfmod_t100, testset), 
  obs = testset$psqm, 
  na.rm = TRUE)




obsv <- testset$psqm
errv <- (pred - obsv)
sqvm <- testset$sqm

sqmvm <-matrix(sqmv, nrow=length(sqvm), ncol=1)
errvm <-matrix(errv, nrow=length(errv), ncol=1)

vizmat <- cbind(sqmvm,errvm)
colnames(vizmat) <- c("sqm", "err")
vizmat <-data.frame(vizmat)

png(filename="Random forest.png", res = 200, width = 800, height = 800)
ggplot(data=vizmat)+
  aes( x=sqm,y=err)+
  geom_point(size = 1, colour = "indianred")+
  labs(
    title='Distribution of residual error\nper squaremeter',
    subtitle='Random forest - 100 trees',
    y='Difference between predicted\nand observed price',
    x='area (squaremeter)'
  )+
  theme_bw()
dev.off()

png(filename="Random forest imp.png", res = 200, width = 1200, height = 800)
varImpPlot(rfmod_t100, type=2)
dev.off()


