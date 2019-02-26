#### WIFI INDOOR LOCATION ####

# Raúl Vázquez Méndez

#### LIBRARIES ####
pacman::p_load("pacman",
               "ggplot2",
               "dplyr",
               "tidyr",
               "anchors",
               "reshape",
               "tidyverse",
               "plyr",
               "stats",
               "esquisse",
               "plotly",
               "caret",
               "randomForest",
               "prob",
               "class",
               "e1071",
               "FNN",
               "lme4")

#### READ CSV ####
train <- read.csv("trainingData.csv")
test <- read.csv("ValidationData.csv")

#### MAIN PRE PROCESS ####

## Train Data Types ##
train$BUILDINGID <- as.factor(train$BUILDINGID)
levels(train$BUILDINGID) <- c("TI",
                                "TD",
                                "TC") #rename levels

train$FLOOR <- as.factor(train$FLOOR)
levels(train$FLOOR) <- c("Main Floor",
                           "1st Floor",
                           "2nd Floor",
                           "3rd Floor",
                           "4th Floor") #rename levels

train$RELATIVEPOSITION <- as.factor(train$RELATIVEPOSITION)
levels(train$RELATIVEPOSITION) <- c("Inside",
                                      "Outside") #rename levels

train$SPACEID <- as.factor(train$SPACEID)
train$USERID <- as.factor(train$USERID)
train$PHONEID <- as.factor(train$PHONEID)
train$TIMESTAMP <- as.POSIXct(train$TIMESTAMP, origin="1970-01-01") 

## Test Data Types ##
test$BUILDINGID <- as.factor(test$BUILDINGID)
levels(test$BUILDINGID) <- c("TI",
                                "TD",
                                "TC") #rename levels

test$FLOOR <- as.factor(test$FLOOR)
levels(test$FLOOR) <- c("Main Floor",
                           "1st Floor",
                           "2nd Floor",
                           "3rd Floor",
                           "4th Floor") #rename levels

test$RELATIVEPOSITION <- as.factor(test$RELATIVEPOSITION)
levels(test$RELATIVEPOSITION) <- c("Inside",
                                      "Outside")

test$SPACEID <- as.factor(test$SPACEID)
test$USERID <- as.factor(test$USERID)
test$PHONEID <- as.factor(test$PHONEID)
test$TIMESTAMP <- as.POSIXct(test$TIMESTAMP, origin="1970-01-01")

## Visualization ##
plot(train$LATITUDE, train$LONGITUDE) #where the WAPS are in Train
plot(test$LATITUDE, test$LONGITUDE) #where the WAPS are in Validation

## 100 values (Default with no Signal) comes to -105 ##
train[,1:520] <- apply(train[,1:520], 2, function(x) ifelse(x == 100, -105, x))
test[,1:520] <- apply(test[,1:520], 2, function(x) ifelse(x == 100, -105, x))

## Duplicates ##
train <- unique(train)
test <- unique(test)

## Checking variance ##
WAPS_train <- grep("WAP", names(train), value = TRUE) #saving total WAPS as a value carachter

train_variance <- nearZeroVar(train, freqCut = 80, uniqueCut = 10, saveMetrics = TRUE) #check variance frequency
rm(train_variance)

train_Zvar <- train #0 variance dataset
train_NZvar <- train #NZ variance dataset

train_Zvar <- train[-which(apply(train[WAPS_train], 2, var) == 0)] #deleting 0 variance WAPS
train_NZvar <- train[-which(apply(train[WAPS_train], 2, var) <= 1)] #delenting <= 1 variance WAPS

test_Zvar <- test #0 variance dataset
test_NZvar <- test #NZ variance dataset

test_Zvar <- test[-which(apply(test[WAPS_train], 2, var) == 0)] #deleting 0 variance WAPS

## Common Waps on Test and Train ##
WAPS_train_common <- names(train_Zvar[,1:465])
WAPS_test_common <- names(test_Zvar[,1:367])

WAPS_common <- intersect(WAPS_train_common, WAPS_test_common)

trainWaps <- train_Zvar[,c(WAPS_common)]
traininfo <- train_Zvar[,466:474] 

train_Zvar <- cbind(trainWaps, traininfo) #selecting common WAPS

testWaps <- test_Zvar[,c(WAPS_common)]
testinfo <- test_Zvar[,368:376]

test_Zvar <- cbind(testWaps, testinfo) #selecting common WAPS

WAPS_trainZvar <- grep("WAP", names(train_Zvar), value = TRUE) #saving 0 WAPS as a value carachter
WAPS_trainNZvar <- grep("WAP", names(train_NZvar), value = TRUE) #saving NZ WAPS as a value carachter
#### PATTERNS FOR STUDY ####

## Gather to visualize ##
dfTrain <- gather(train, "WAPid", "WAPvalue", 1:520) #gather table to better use
dfTrain <- dplyr::filter(dfTrain, WAPvalue != -105)

dfTest <- gather(test, "WAPid", "WAPvalue", 1:520) #gather table to better use
dfTest <- dplyr::filter(dfTest, WAPvalue != -105)

## Intensity Information ###
dfTrain$INTENSITY <- ifelse(dfTrain[,c("WAPvalue")] >= -30, "Outliers",
                            ifelse(dfTrain[,c("WAPvalue")] >= -66 , "Excelent",
                                   ifelse(dfTrain[,c("WAPvalue")] >= -69 , "VeryGood",
                                          ifelse(dfTrain[,c("WAPvalue")] >= -79 , "Good",
                                                 ifelse(dfTrain[,c("WAPvalue")] >= -89 , "NotGood", "Unusable")))))
                                                
dfTrain$INTENSITY <- as.factor(dfTrain$INTENSITY) #new column as factor

dfTest$INTENSITY <- ifelse(dfTest[,c("WAPvalue")] >= -30, "Outliers",
                            ifelse(dfTest[,c("WAPvalue")] >= -66 , "Excelent",
                                   ifelse(dfTest[,c("WAPvalue")] >= -69 , "VeryGood",
                                          ifelse(dfTest[,c("WAPvalue")] >= -79 , "Good",
                                                 ifelse(dfTest[,c("WAPvalue")] >= -89 , "NotGood", "Unusable")))))

dfTest$INTENSITY <- as.factor(dfTest$INTENSITY) #new column as factor

## Distribution ##
ggplot(data = dfTrain) +
  aes(x = WAPvalue, fill = FLOOR) +
  geom_histogram(bins = 30) +
  labs(title = "Distribution of WAPS per Building",
       subtitle = "On Train") +
  theme_minimal() +
  facet_wrap(vars(BUILDINGID)) #On Train per Building and Floor

ggplot(data = dfTest) +
  aes(x = WAPvalue, fill = FLOOR) +
  geom_histogram(bins = 30) +
  labs(title = "Distribution of WAPS per Building",
       subtitle = "On Test") +
  theme_minimal() +
  facet_wrap(vars(BUILDINGID)) #On Test per Building and Floor

train_TC <- dfTrain %>% dplyr::filter(BUILDINGID == "TC") #52% of Data
train_TD <- dfTrain %>% dplyr::filter(BUILDINGID == "TD") #23% of Data
train_TI <- dfTrain %>% dplyr::filter(BUILDINGID == "TI") #24% of Data

mean(train_TC[,c("WAPvalue")]) #-80,17 mean of WAP's Value's
mean(train_TD[,c("WAPvalue")]) #-77,42 mean of WAP's Value's
mean(train_TI[,c("WAPvalue")]) #-76,26 mean of WAP's Value's

## Intensity Distribution per Building ##
ggplot(data = train_TC) +
  aes(x = LONGITUDE, y = LATITUDE, color = INTENSITY) +
  geom_point() +
  theme_minimal() +
  facet_wrap(vars(FLOOR)) #train_TC

ggplot(data = train_TI) +
  aes(x = LONGITUDE, y = LATITUDE, color = INTENSITY) +
  geom_point() +
  theme_minimal() +
  facet_wrap(vars(FLOOR)) #TI

ggplot(data = train_TD) +
  aes(x = LONGITUDE, y = LATITUDE, color = INTENSITY) +
  geom_point() +
  theme_minimal() +
  facet_wrap(vars(FLOOR)) #TD

#### OUTLIERS  (-30 Intensity) ####
train_30 <- train %>% dplyr::filter(apply(train[,1:520], 1, function(x) any(x >= -30))) #selecting -30 intensity

outliers <- gather(train_30, "WAPid", "WAPvalue", 1:520) #gather to visualize it
outliers <- dplyr::filter(outliers, WAPvalue != -115) #for a good visualization

ggplot(data = outliers) +
  aes(x = WAPvalue, fill = FLOOR) +
  geom_histogram(bins = 30) +
  theme_minimal() +
  facet_wrap(vars(BUILDINGID)) #Per Building and Floor

ggplot(data = outliers) +
  aes(x = PHONEID, fill = FLOOR) +
  geom_bar() +
  theme_minimal() +
  facet_wrap(vars(BUILDINGID)) #Nexus 4 Phone ID 19

ggplot(data = outliers) +
  aes(x = USERID, fill = FLOOR) +
  geom_bar() +
  theme_minimal() +
  facet_wrap(vars(BUILDINGID)) #User 6 does all the 3rd and 4th floor

## User 6 ##
train_UID6 <- dplyr::filter(dfTrain, USERID == 6)
train30_UID6 <- train_30 %>% dplyr::filter(USERID == 6) #84% of the user 6 observations has a -30 value

ggplot(data = train_UID6) +
  aes(x = WAPvalue, fill = FLOOR) +
  geom_histogram(bins = 30) +
  theme_minimal() +
  facet_wrap(vars(BUILDINGID)) #what de User 6 does

## Mostly the outliers are on 3rd 4th Floor of TC ##
outliers_TC <- outliers %>% dplyr::filter(BUILDINGID == "TC" & FLOOR == "3rd Floor" | FLOOR == "4th Floor")

ggplot(data = outliers_TC) +
  aes(x = WAPvalue, fill = USERID) +
  geom_histogram(bins = 30) +
  theme_minimal() +
  facet_wrap(vars(FLOOR)) #Distribution of Outliers in 3rd and 4th Floor in TC Building

## Deleting -30 rows (Outliers) ##
train_no_30 <- train_Zvar %>% dplyr::filter(apply(train_Zvar[WAPS_trainZvar], 1, function(x) all(x < -30))) #deleting -30 intensity

#### MODELING ####

## SAMPLING ##
train_Zvarsample <- train_Zvar %>% group_by(BUILDINGID, FLOOR) %>% sample_n(150) #taking smart sample of 0 variance
train_NZvarsample <- train_NZvar %>% group_by(BUILDINGID, FLOOR) %>% sample_n(150) #taking smart sample of NZ variance
train_no30_sample <- train_no_30 %>% group_by(BUILDINGID, FLOOR) %>% sample_n(150) #taking smart sample of 0 variance

plot_ly(train_Zvarsample, x = ~LONGITUDE, y = ~LATITUDE, z = ~FLOOR, color = ~BUILDINGID) %>%
  add_markers() #checking distribution of sample1

## Understanding Samples ##
metrics_Building <- list()
samples <- list(train_Zvarsample, train_NZvarsample, train_no30_sample)
metric_comp <- c()
models <- list()

  for (i in  samples) {

    WAPS <- grep("WAP", names(i), value = TRUE) #saving total WAPS as a value carachter

    set.seed(777)
    model_RF <- randomForest(y = i$BUILDINGID,
                                x = i[WAPS],
                                importance = T,
                                method = "rf",
                                ntree = 100) #rf
    models <- list(models,model_RF)

    prediction_RF <- predict(model_RF, test) #prediction

    metric_RF <- postResample(prediction_RF, test$BUILDINGID)
    print(metric_RF)

    metric_comp <- cbind(metric_comp, metric_RF)

  }

metric_comp <- as.data.frame(metric_comp)
colnames(metric_comp) <- c("train_Zvarsample","train_NZvarsample","train_no30_sample")
rm(test_NZvar)

#### RANDOM FOREST ####

#### BUILDING ####
set.seed(777)
model_RF_Building <- randomForest(y = train_Zvar$BUILDINGID,
                         x = train_Zvar[WAPS_trainZvar],
                         importance = T,
                         method = "rf",
                         ntree = 100) #rf

prediction_RF_Building <- predict(model_RF_Building, test_Zvar) #prediction
confusionMatrix(prediction_RF_Building, test$BUILDINGID)

test_Zvar$BUILD <- prediction_RF_Building #99,82 Accuracy

saveRDS(model_RF_Building, file = "model_RF_building.rds")
readRDS(model_RF_Building, file = "model_RF_building.rds")

#### FLOOR ####
train_floor <- train_Zvar %>% select(c(1:312), BUILDINGID, FLOOR)
set.seed(777)

model_RF_Floor <- randomForest(y = train_floor$FLOOR,
                             x = train_floor[,1:313],
                             importance = T,
                             method = "rf",
                             ntree = 100) #rf

prediction_RF_Floor <- predict(model_RF_Floor, test_Zvar) #prediction
confusionMatrix(prediction_RF_Floor, test_Zvar$FLOOR)

test_Zvar$FLOOR_PREDICT <- prediction_RF_Floor #91,18% Acc
 
saveRDS(model_RF_Floor, file = "model_RF_Floor.rds")
readRDS(model_RF_Floor, file = "model_RF_Floor.rds")

#### LATITUDE ####
train_Regression <- train_Zvar %>% select(c(1:312), BUILDINGID, FLOOR, LONGITUDE, LATITUDE) #Train df for Regression
test_Regression <- test_Zvar %>% select(c(1:312), BUILDINGID, FLOOR, LONGITUDE, LATITUDE) #Test df for Regression

set.seed(777)

model_RF_Latitude <- randomForest(y = train_Regression$LATITUDE,
                             x = train_Regression[,1:315],
                             importance = T,
                             method = "rf",
                             ntree = 100) #rf

prediction_RF_Latitude <- predict(model_RF_Latitude, test_Regression) #prediction
postResample(prediction_RF_Latitude, test_Zvar$LATITUDE) #5,10%

test_Zvar$LATITUDE_PREDICT <- prediction_RF_Latitude

saveRDS(model_RF_Latitude, file = "model_RF_Latitude.rds")
readRDS(model_RF_Latitude, file = "model_RF_Latitude.rds")

#### LONGITUDE ####
set.seed(777)

train_Regression_Longitude <- train_Regression

train_Regression_Longitude <- train_Regression_Longitude[,c(1:312,313,314,316,315)] #reorder columns to predcit Longitude with Latitude

model_RF_Longitude <- randomForest(y = train_Regression_Longitude$LONGITUDE,
                                  x = train_Regression_Longitude[,1:315],
                                  importance = T,
                                  method = "rf",
                                  ntree = 100) #rf

prediction_RF_Longitude <- predict(model_RF_Longitude, test_Regression) #prediction
postResample(prediction_RF_Longitude, test_Regression$LONGITUDE) #5,51 MAE

test_Zvar$LONGITUDE_PREDICT <- prediction_RF_Longitude

saveRDS(model_RF_Longitude, file = "model_RF_Longitude.rds")
readRDS(model_RF_Longitude, file = "model_RF_Longitude.rds")

#### MODELING II (Predicting per Building to check perfromance of the models) With RANDOM FOREST ####

## Filter per Building ##
train_TC_floor <- train_Zvar %>% dplyr::filter(BUILDINGID == "TC") 
train_TD_floor <- train_Zvar %>% dplyr::filter(BUILDINGID == "TD") 
train_TI_floor <- train_Zvar %>% dplyr::filter(BUILDINGID == "TI")

train_TI_floor <- train_TI_floor %>% select(c(1:312), BUILDINGID, FLOOR)
train_TD_floor <- train_TD_floor %>% select(c(1:312), BUILDINGID, FLOOR)
train_TC_floor <- train_TC_floor %>% select(c(1:312), BUILDINGID, FLOOR)

## New levels ##
train_TC_floor$FLOOR <- as.factor(train_TC_floor$FLOOR)
train_TC_floor$FLOOR <- factor(train_TC_floor$FLOOR, levels = c("Main Floor",
                                                                "1st Floor",
                                                                "2nd Floor",
                                                                "3rd Floor",
                                                                "4th Floor"))


train_TD_floor$FLOOR <- as.character(train_TD_floor$FLOOR)
train_TD_floor$FLOOR <- as.factor(train_TD_floor$FLOOR)
train_TD_floor$FLOOR <- factor(train_TD_floor$FLOOR, levels = c("Main Floor",
                                                                "1st Floor",
                                                                "2nd Floor",
                                                                "3rd Floor"))

train_TI_floor$FLOOR <- as.character(train_TI_floor$FLOOR)
train_TI_floor$FLOOR <- as.factor(train_TI_floor$FLOOR)
train_TI_floor$FLOOR <- factor(train_TI_floor$FLOOR, levels = c("Main Floor",
                                                                "1st Floor",
                                                                "2nd Floor", 
                                                                "3rd Floor"))

## On test ##
test_TC_floor <- test_Zvar %>% dplyr::filter(BUILD == "TC") 
test_TD_floor <- test_Zvar %>% dplyr::filter(BUILD == "TD") 
test_TI_floor <- test_Zvar %>% dplyr::filter(BUILD == "TI")

test_TI_floor <- test_TI_floor %>% select(c(1:312), BUILDINGID, FLOOR)
test_TD_floor <- test_TD_floor %>% select(c(1:312), BUILDINGID, FLOOR)
test_TC_floor <- test_TC_floor %>% select(c(1:312), BUILDINGID, FLOOR)

test_TC_floor$FLOOR <- as.factor(test_TC_floor$FLOOR)
test_TC_floor$FLOOR <- factor(test_TC_floor$FLOOR, levels = c("Main Floor",
                                                                "1st Floor",
                                                                "2nd Floor",
                                                                "3rd Floor",
                                                                "4th Floor"))


test_TD_floor$FLOOR <- as.character(test_TD_floor$FLOOR)
test_TD_floor$FLOOR <- as.factor(test_TD_floor$FLOOR)
test_TD_floor$FLOOR <- factor(test_TD_floor$FLOOR, levels = c("Main Floor",
                                                                "1st Floor",
                                                                "2nd Floor",
                                                                "3rd Floor"))

test_TI_floor$FLOOR <- as.character(test_TI_floor$FLOOR)
test_TI_floor$FLOOR <- as.factor(test_TI_floor$FLOOR)
test_TI_floor$FLOOR <- factor(test_TI_floor$FLOOR, levels = c("Main Floor",
                                                                "1st Floor",
                                                                "2nd Floor", 
                                                                "3rd Floor"))
#### FLOOR II ####

## TC ##
set.seed(777)

model_RF_TC <- randomForest(y = train_TC_floor$FLOOR,
                         x = train_TC_floor[,1:313],
                         importance = T,
                         method = "rf",
                         ntree = 100) #rf

prediction_RF_TC <- predict(model_RF_TC, test_TC_floor) #prediction
confusionMatrix(prediction_RF_TC, test_TC_floor$FLOOR)

test_TC_floor$FLOOR_PREDICT <- prediction_RF_TC #91,64 Accuracy

saveRDS(model_RF_TC, file = "model_RF_TC.rds")
readRDS(model_RF_TC, file = "model_RF_TC.rds")


## TD ##
set.seed(777)

model_RF_TD <- randomForest(y = train_TD_floor$FLOOR,
                               x = train_TD_floor[,1:313],
                               importance = T,
                               method = "rf",
                               ntree = 100) #rf

prediction_RF_TD <- predict(model_RF_TD, test_TD_floor) #prediction
confusionMatrix(prediction_RF_TD, test_TD_floor$FLOOR)

test_TD_floor$FLOOR_PREDICT <- prediction_RF_TD #80,91 Accuracy

saveRDS(model_RF_TD, file = "model_RF_TD.rds")
readRDS(model_RF_TD, file = "model_RF_TD.rds")

## TI ##
set.seed(777)

model_RF_TI <- randomForest(y = train_TI_floor$FLOOR,
                            x = train_TI_floor[,1:313],
                            importance = T,
                            method = "rf",
                            ntree = 100) #rf

prediction_RF_TI <- predict(model_RF_TI, test_TI_floor) #prediction
confusionMatrix(prediction_RF_TI, test_TI_floor$FLOOR)

test_TI_floor$FLOOR_PREDICT <- prediction_RF_TI #97,44 Accuracy
 
saveRDS(model_RF_TI, file = "model_RF_TI.rds")
readRDS(model_RF_TI, file = "model_RF_TI.rds")

#### LATITUDE AND LONGITUDE II ####

## Filtering per Bulding on Regression in order to find a better performance ##

train_TC_Regression <- train_Zvar %>% dplyr::filter(BUILDINGID == "TC") 
train_TD_Regression <- train_Zvar %>% dplyr::filter(BUILDINGID == "TD") 
train_TI_Regression <- train_Zvar %>% dplyr::filter(BUILDINGID == "TI")

train_TI_Regression <- train_TI_Regression %>% select(c(1:312), BUILDINGID, FLOOR, LATITUDE, LONGITUDE)
train_TD_Regression <- train_TD_Regression %>% select(c(1:312), BUILDINGID, FLOOR, LATITUDE, LONGITUDE)
train_TC_Regression <- train_TC_Regression %>% select(c(1:312), BUILDINGID, FLOOR, LATITUDE, LONGITUDE)

## New levels ##
train_TC_Regression$FLOOR <- as.factor(train_TC_Regression$FLOOR)
train_TC_Regression$FLOOR <- factor(train_TC_Regression$FLOOR, levels = c("Main Floor",
                                                                "1st Floor",
                                                                "2nd Floor",
                                                                "3rd Floor",
                                                                "4th Floor"))


train_TD_Regression$FLOOR <- as.character(train_TD_Regression$FLOOR)
train_TD_Regression$FLOOR <- as.factor(train_TD_Regression$FLOOR)
train_TD_Regression$FLOOR <- factor(train_TD_Regression$FLOOR, levels = c("Main Floor",
                                                                "1st Floor",
                                                                "2nd Floor",
                                                                "3rd Floor"))

train_TI_Regression$FLOOR <- as.character(train_TI_Regression$FLOOR)
train_TI_Regression$FLOOR <- as.factor(train_TI_Regression$FLOOR)
train_TI_Regression$FLOOR <- factor(train_TI_Regression$FLOOR, levels = c("Main Floor",
                                                                "1st Floor",
                                                                "2nd Floor", 
                                                                "3rd Floor"))

## On test ##
test_TC_Regression <- test_Zvar %>% dplyr::filter(BUILD == "TC") 
test_TD_Regression <- test_Zvar %>% dplyr::filter(BUILD == "TD") 
test_TI_Regression <- test_Zvar %>% dplyr::filter(BUILD == "TI")

test_TI_Regression <- test_TI_Regression %>% select(c(1:465), BUILDINGID, FLOOR, LATITUDE, LONGITUDE)
test_TD_Regression <- test_TD_Regression %>% select(c(1:465), BUILDINGID, FLOOR, LATITUDE, LONGITUDE)
test_TC_Regression <- test_TC_Regression %>% select(c(1:465), BUILDINGID, FLOOR, LATITUDE, LONGITUDE)

test_TC_Regression$FLOOR <- as.factor(test_TC_Regression$FLOOR)
test_TC_Regression$FLOOR <- factor(test_TC_Regression$FLOOR, levels = c("Main Floor",
                                                              "1st Floor",
                                                              "2nd Floor",
                                                              "3rd Floor",
                                                              "4th Floor"))


test_TD_Regression$FLOOR <- as.character(test_TD_Regression$FLOOR)
test_TD_Regression$FLOOR <- as.factor(test_TD_Regression$FLOOR)
test_TD_Regression$FLOOR <- factor(test_TD_Regression$FLOOR, levels = c("Main Floor",
                                                              "1st Floor",
                                                              "2nd Floor",
                                                              "3rd Floor"))

test_TI_Regression$FLOOR <- as.character(test_TI_floor$FLOOR)
test_TI_Regression$FLOOR <- as.factor(test_TI_floor$FLOOR)
test_TI_Regression$FLOOR <- factor(test_TI_floor$FLOOR, levels = c("Main Floor",
                                                              "1st Floor",
                                                              "2nd Floor", 
                                                              "3rd Floor"))
## LONGITUDE ##

## TC ##
set.seed(777)

model_RF_TC_Longitude <- randomForest(y = train_TC_Regression$LONGITUDE,
                             x = train_TC_Regression[,1:315],
                             importance = T,
                             method = "rf",
                             ntree = 100) #rf

prediction_RF_TC_Longitude <- predict(model_RF_TC_Longitude, test_TC_Regression) #prediction 5,61 Acc
postResample(prediction_RF_TC_Longitude, test_TC_Regression$LONGITUDE)

## TD ##
set.seed(777)

model_RF_TD_Longitude <- randomForest(y = train_TD_Regression$LONGITUDE,
                                     x = train_TD_Regression[,1:315],
                                     importance = T,
                                     method = "rf",
                                     ntree = 100) #rf

prediction_RF_TD_Longitude <- predict(model_RF_TD_Longitude, test_TD_Regression) #prediction 6,90 Acc
postResample(prediction_RF_TD_Longitude, test_TD_Regression$LONGITUDE)

## TI ##
set.seed(777)

model_RF_TI_Longitude <- randomForest(y = train_TI_Regression$LONGITUDE,
                                     x = train_TI_Regression[,1:315],
                                     importance = T,
                                     method = "rf",
                                     ntree = 100) #rf

prediction_RF_TI_Longitude <- predict(model_RF_TI_Longitude, test_TI_Regression) #prediction 3,88 Acc
postResample(prediction_RF_TI_Longitude, test_TI_Regression$LONGITUDE)

## LATITUDE ##

# Prepare Data #
train_TC_Regression_Lat <- train_TC_Regression
train_TD_Regression_Lat <- train_TD_Regression
train_TI_Regression_Lat <- train_TI_Regression

train_TC_Regression_Lat <- train_TC_Regression_Lat[,c(1:314,316,315)]
train_TD_Regression_Lat <- train_TD_Regression_Lat[,c(1:314,316,315)]
train_TI_Regression_Lat <- train_TI_Regression_Lat[,c(1:314,316,315)]


## TC ##
set.seed(777)

model_RF_TC_Latitude <- randomForest(y = train_TC_Regression_Lat$LATITUDE,
                                     x = train_TC_Regression_Lat[,1:315],
                                     importance = T,
                                     method = "rf",
                                     ntree = 100) #rf

prediction_RF_TC_Latitude <- predict(model_RF_TC_Latitude, test_TC_Regression) #prediction 5,61 Acc
postResample(prediction_RF_TC_Latitude, test_TC_Regression$LATITUDE)

## TD ##
set.seed(777)

model_RF_TD_Latitude <- randomForest(y = train_TD_Regression_Lat$LATITUDE,
                                     x = train_TD_Regression_Lat[,1:315],
                                     importance = T,
                                     method = "rf",
                                     ntree = 100) #rf

prediction_RF_TD_Latitude <- predict(model_RF_TD_Latitude, test_TD_Regression) #prediction 6,90 Acc
postResample(prediction_RF_TD_Latitude, test_TD_Regression$LATITUDE)

## TI ##
set.seed(777)

model_RF_TI_Latitude <- randomForest(y = train_TI_Regression_Lat$LATITUDE,
                                     x = train_TI_Regression_Lat[,1:315],
                                     importance = T,
                                     method = "rf",
                                     ntree = 100) #rf

prediction_RF_TI_Latitude <- predict(model_RF_TI_Latitude, test_TI_Regression) #prediction 3,88 Acc
postResample(prediction_RF_TI_Latitude, test_TI_Regression$LATITUDE)

#### MODELING III  (Preprocess Distance Based Models) KNN and SVM ####

## On Train ##

## Dummy Vars ##
dumm <- dummyVars(" ~ .", data = train_floor[c("BUILDINGID")])
pred <- predict(dumm, newdata = train_floor)
train_floor_knn <- data.frame(pred)

train_floor_KNN <- train_floor
train_floor_KNN$BUILDTC <- train_floor_knn$BUILDINGID.TC
train_floor_KNN$BUILDTD <- train_floor_knn$BUILDINGID.TD
train_floor_KNN$BUILDTI <- train_floor_knn$BUILDINGID.TI

train_floor_KNN$BUILDINGID <- NULL
train_floor_KNN <- train_floor_KNN[,c(1:312,314,315,316,313)]

## Normalize WAPS ##
preprocessParams <- preProcess(train_floor[,1:312], method=c("range"))
transformed <- predict(preprocessParams, train_floor[,1:312])
train_floor_KNN[,1:312] <- transformed

## On test ##

## Dummy Vars ##
test_floor <- test_Zvar %>% select(c(1:312), BUILDINGID, FLOOR)

dumm_test <- dummyVars(" ~ .", data = test_floor[c("BUILDINGID")])
pred_test <- predict(dumm_test, newdata = test_floor)
test_floor_knn <- data.frame(pred_test)

test_floor_KNN <- test_floor
test_floor_KNN$BUILDTC <- test_floor_knn$BUILDINGID.TC
test_floor_KNN$BUILDTD <- test_floor_knn$BUILDINGID.TD
test_floor_KNN$BUILDTI <- test_floor_knn$BUILDINGID.TI

test_floor_KNN$BUILDINGID <- NULL
test_floor_KNN <- test_floor_KNN[,c(1:312,314,315,316,313)]

## Normalize WAPS ##
preprocessParams <- preProcess(test_floor[,1:312], method=c("range"))
transformed <- predict(preprocessParams, test_floor[,1:312])
test_floor_KNN[,1:312] <- transformed

#### KNN ####
model_knn_floor <- FNN::knn(train = train_floor_KNN, test = test_floor_KNN, cl = train_floor_KNN$FLOOR, k = 4)

#### ERROR VISUALIZATION ####

esquisser()

## BUILDING ##
ggplot(data = test_Zvar) +
  aes(x = LONGITUDE_PREDICT, y = LATITUDE_PREDICT, color = BUILD) +
  geom_point() +
  labs(title = "BUILDING",
    subtitle = "Predictied") +
  theme_minimal()

## FLOOR ##
plot_ly(test_Zvar, x = ~LONGITUDE_PREDICT, y = ~LATITUDE_PREDICT, z = ~FLOOR_PREDICT, color = ~BUILD) %>%
  add_markers(marker = list(sizeref = 230))


## LONGITUDE (Real vs Predicted) ##
ggplot(data = test_Zvar) +
  aes(x = LONGITUDE, y = LONGITUDE_PREDICT) +
  geom_point(color = "#0c4c8a") +
  labs(title = "LONGITUDE",
    subtitle = "Real vs Predicted") +
  theme_minimal()

## LATITUDE (Real vs Predicted) ##
ggplot(data = test_Zvar) +
  aes(x = LATITUDE, y = LATITUDE_PREDICT) +
  geom_point(color = "#0c4c8a") +
  labs(title = "LATITUDE",
    subtitle = "Real vs Predicted") +
  theme_minimal()



