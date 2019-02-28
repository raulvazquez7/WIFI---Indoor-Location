# WIFI - Indoor Location - Project Description

### Project Goal: 

Many real world applications need to know the localization of a user in the world to provide their services. Therefore, automatic user localization has been a hot research topic in the last years. Automatic user localization consists of estimating the position of the user (latitude, longitude and altitude) by using an electronic device, usually a mobile phone. Outdoor localization problem can be solved very accurately thanks to the inclusion of GPS sensors into the mobile devices. However, indoor localization is still an open problem mainly due to the loss of GPS signal in indoor environments. Although, there are some indoor positioning technologies and methodologies, this database is focused on WLAN fingerprint-based ones (also know as WiFi Fingerprinting).  

Although there are many papers in the literature trying to solve the indoor localization problem using a WLAN fingerprint-based method, there still exists one important drawback in this field which is the lack of a common database for comparison purposes. So, UJIIndoorLoc database is presented to overcome this gap. We expect that the proposed database will become the reference database to compare different indoor localization methodologies based on WiFi fingerprinting. 

### Data characteristics: 
The UJIIndoorLoc database covers three buildings of Universitat Jaume I with 4 or more floors and almost 110.000m2. It can be used for classification, e.g. actual building and floor identification, or regression, e.g. actual longitude and latitude estimation. It was created in 2013 by means of more than 20 different users and 25 Android devices. The database consists of 19937 training/reference records (trainingData.csv file) and 1111 validation/test records (validationData.csv file). 

The 529 attributes contain the WiFi fingerprint, the coordinates where it was taken, and other useful information. Each WiFi fingerprint can be characterized by the detected Wireless Access Points (WAPs) and the corresponding Received Signal Strength Intensity (RSSI). The intensity values are represented as negative integer values ranging -104dBm (extremely poor signal) to 0dbM. The positive value 100 is used to denote when a WAP was not detected. During the database creation, 520 different WAPs were detected. Thus, the WiFi fingerprint is composed by 520 intensity values.

Then the coordinates (latitude, longitude, floor) and Building ID are provided as the attributes to be predicted.

Data source: http://archive.ics.uci.edu/ml/datasets/UJIIndoorLoc

### Technical Analysis
Language used: R programming

### 1. PRE-PROCESSING
- Missing values check
- Duplicates check
- Data types & classes treatment
- Default 100 to -105
- Removing Zero Variance 
- Outliers Understanding
- Identify the most relevant predictors
- Dummify (Distance based models)
- Standardize (Distance based models)
- Normalize (Distance based models)
### 2. DATA PARTITION AND SPLITTING
- Create smart Samples in order to Train and Test faster
- Understanding Samples
### 3. APPROACHES
- Two Approaches to predict:
  - Hole Data set
  - Divide de Data per Building
- Predictions made on this order:
  1- Building
  2- Floor
  3- Latitude
  4- Longitude
### 4. MODELLING AND PREDICTING
- Random Forest
- Knn
- SVM
### 5. ERROR ANALYSIS
### 6. VALIDATION
Using a new test set to validate our results of our training in Data that has never seen our model in order to understand our performance and our continuous improvements to our model.


