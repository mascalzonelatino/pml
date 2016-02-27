# Practical Machine Learning course project

## Introduction

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect 
a large amount of data about personal activity relatively inexpensively. 
These type of devices are part of the quantified self movement – a group of enthusiasts who take 
measurements about themselves regularly to improve their health, to find patterns in their behavior, 
or because they are tech geeks. 
One thing that people regularly do is quantify how much of a particular activity they do, 
but they rarely quantify how well they do it. In this project, your goal will be to use data 
from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. 
They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. 

## Data
The training data for this project are available here: 
  https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv

The test data are available here: 
  https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv

The data for this project come from this source: 
  http://groupware.les.inf.puc-rio.br/har. 

## Goal
The goal of the project is to predict the manner in which they did the exercise. 
This is the “classe” variable in the training set. 

## 1. Loading the data

```r
all_data <- read.csv("pml-training.csv")
```

## 2. Data partitioning and cleaning 

```r
library(caret)
```

```
## Loading required package: lattice
```

```
## Loading required package: ggplot2
```

```r
set.seed(1234)
inTrain <- createDataPartition(y=all_data$classe,p=0.75,list=FALSE)
training <- all_data[inTrain,]
testing <- all_data[-inTrain,]
dim(training)
```

```
## [1] 14718   160
```

From manual exploration it emerges that there is a significant number of missing
values for some of the variables. We can remove the ones for which the number of 
missing values is 95%: 


```r
training[training==""] <- NA
training <- training[,colSums(is.na(training)) < (nrow(training) * 0.95)]
```

Now let's take a closer look at the type of variables in the training set 
in the hope that we will be able to remove some of them from the analysis: 


```r
head(training,5)
```

```
##   X user_name raw_timestamp_part_1 raw_timestamp_part_2   cvtd_timestamp
## 2 2  carlitos           1323084231               808298 05/12/2011 11:23
## 3 3  carlitos           1323084231               820366 05/12/2011 11:23
## 4 4  carlitos           1323084232               120339 05/12/2011 11:23
## 5 5  carlitos           1323084232               196328 05/12/2011 11:23
## 6 6  carlitos           1323084232               304277 05/12/2011 11:23
##   new_window num_window roll_belt pitch_belt yaw_belt total_accel_belt
## 2         no         11      1.41       8.07    -94.4                3
## 3         no         11      1.42       8.07    -94.4                3
## 4         no         12      1.48       8.05    -94.4                3
## 5         no         12      1.48       8.07    -94.4                3
## 6         no         12      1.45       8.06    -94.4                3
##   gyros_belt_x gyros_belt_y gyros_belt_z accel_belt_x accel_belt_y
## 2         0.02         0.00        -0.02          -22            4
## 3         0.00         0.00        -0.02          -20            5
## 4         0.02         0.00        -0.03          -22            3
## 5         0.02         0.02        -0.02          -21            2
## 6         0.02         0.00        -0.02          -21            4
##   accel_belt_z magnet_belt_x magnet_belt_y magnet_belt_z roll_arm
## 2           22            -7           608          -311     -128
## 3           23            -2           600          -305     -128
## 4           21            -6           604          -310     -128
## 5           24            -6           600          -302     -128
## 6           21             0           603          -312     -128
##   pitch_arm yaw_arm total_accel_arm gyros_arm_x gyros_arm_y gyros_arm_z
## 2      22.5    -161              34        0.02       -0.02       -0.02
## 3      22.5    -161              34        0.02       -0.02       -0.02
## 4      22.1    -161              34        0.02       -0.03        0.02
## 5      22.1    -161              34        0.00       -0.03        0.00
## 6      22.0    -161              34        0.02       -0.03        0.00
##   accel_arm_x accel_arm_y accel_arm_z magnet_arm_x magnet_arm_y
## 2        -290         110        -125         -369          337
## 3        -289         110        -126         -368          344
## 4        -289         111        -123         -372          344
## 5        -289         111        -123         -374          337
## 6        -289         111        -122         -369          342
##   magnet_arm_z roll_dumbbell pitch_dumbbell yaw_dumbbell
## 2          513      13.13074      -70.63751    -84.71065
## 3          513      12.85075      -70.27812    -85.14078
## 4          512      13.43120      -70.39379    -84.87363
## 5          506      13.37872      -70.42856    -84.85306
## 6          513      13.38246      -70.81759    -84.46500
##   total_accel_dumbbell gyros_dumbbell_x gyros_dumbbell_y gyros_dumbbell_z
## 2                   37                0            -0.02             0.00
## 3                   37                0            -0.02             0.00
## 4                   37                0            -0.02            -0.02
## 5                   37                0            -0.02             0.00
## 6                   37                0            -0.02             0.00
##   accel_dumbbell_x accel_dumbbell_y accel_dumbbell_z magnet_dumbbell_x
## 2             -233               47             -269              -555
## 3             -232               46             -270              -561
## 4             -232               48             -269              -552
## 5             -233               48             -270              -554
## 6             -234               48             -269              -558
##   magnet_dumbbell_y magnet_dumbbell_z roll_forearm pitch_forearm
## 2               296               -64         28.3         -63.9
## 3               298               -63         28.3         -63.9
## 4               303               -60         28.1         -63.9
## 5               292               -68         28.0         -63.9
## 6               294               -66         27.9         -63.9
##   yaw_forearm total_accel_forearm gyros_forearm_x gyros_forearm_y
## 2        -153                  36            0.02            0.00
## 3        -152                  36            0.03           -0.02
## 4        -152                  36            0.02           -0.02
## 5        -152                  36            0.02            0.00
## 6        -152                  36            0.02           -0.02
##   gyros_forearm_z accel_forearm_x accel_forearm_y accel_forearm_z
## 2           -0.02             192             203            -216
## 3            0.00             196             204            -213
## 4            0.00             189             206            -214
## 5           -0.02             189             206            -214
## 6           -0.03             193             203            -215
##   magnet_forearm_x magnet_forearm_y magnet_forearm_z classe
## 2              -18              661              473      A
## 3              -18              658              469      A
## 4              -16              658              469      A
## 5              -17              655              473      A
## 6               -9              660              478      A
```

The variables X, user_name, window type and timestamps are used purely to 
identify an experiment and can be safely removed as they do not influence the prediction model we are trying to build. 


```r
nonPredictors <- grep("name|timestamp|window|X", colnames(training), value=F) 
training <- training[,-nonPredictors]
ncol(training) # columns in our training set 
```

```
## [1] 53
```

## 3. Principal component analysis 

We now have a total of 53 variables in our training set; the 53th variable is 
the outcome variable "classe". 
We will use Principal component analysis (PCA) to identify whether some variables are highly correlated. 


```r
preProc <- preProcess(training[,1:52], method="pca",thresh=0.95) # retain 95% of the variance; printing preProc$rotation would show that 25 components are identified. 
trainingPCA <- predict(preProc,training[,1:52])
```

## 4. Prediction model and validation

We can now train a prediction model against the training set and then validate the model against the testing set. Given the non-linear nature of the training set and 
the considerable number of predictors, we will choose the Random Forest algorithm. 


```r
library(randomForest)
```

```
## randomForest 4.6-12
```

```
## Type rfNews() to see new features/changes/bug fixes.
```

```
## 
## Attaching package: 'randomForest'
```

```
## The following object is masked from 'package:ggplot2':
## 
##     margin
```

```r
fitRF <- randomForest(training$classe ~ ., data=trainingPCA,do.trace=F)
print(fitRF)
```

```
## 
## Call:
##  randomForest(formula = training$classe ~ ., data = trainingPCA,      do.trace = F) 
##                Type of random forest: classification
##                      Number of trees: 500
## No. of variables tried at each split: 5
## 
##         OOB estimate of  error rate: 2.3%
## Confusion matrix:
##      A    B    C    D    E class.error
## A 4150   10   16    7    2 0.008363202
## B   41 2769   36    0    2 0.027738764
## C    5   36 2496   28    2 0.027658746
## D    5    1   86 2315    5 0.040215589
## E    3   12   23   19 2649 0.021064302
```

We can now check our model against the testing set. The testing set will be "cleaned"
using the same steps performed for the training set above: 


```r
testing <- testing[,-nonPredictors]
testing[testing==""] <- NA
testing <- testing[,colSums(is.na(testing)) < (nrow(testing) * 0.95)]
testingPCA <- predict(preProc,testing[,1:52])
confusionMatrix(testing$classe,predict(fitRF,testingPCA))
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 1387    2    4    2    0
##          B   14  929    5    0    1
##          C    1   18  825    8    3
##          D    4    1   29  769    1
##          E    0    0    6    5  890
## 
## Overall Statistics
##                                           
##                Accuracy : 0.9788          
##                  95% CI : (0.9744, 0.9826)
##     No Information Rate : 0.2867          
##     P-Value [Acc > NIR] : < 2.2e-16       
##                                           
##                   Kappa : 0.9732          
##  Mcnemar's Test P-Value : NA              
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            0.9865   0.9779   0.9494   0.9809   0.9944
## Specificity            0.9977   0.9949   0.9926   0.9915   0.9973
## Pos Pred Value         0.9943   0.9789   0.9649   0.9565   0.9878
## Neg Pred Value         0.9946   0.9947   0.9891   0.9963   0.9988
## Prevalence             0.2867   0.1937   0.1772   0.1599   0.1825
## Detection Rate         0.2828   0.1894   0.1682   0.1568   0.1815
## Detection Prevalence   0.2845   0.1935   0.1743   0.1639   0.1837
## Balanced Accuracy      0.9921   0.9864   0.9710   0.9862   0.9958
```

The model is 98% accurate with average specificity and sensitivity of 99%. 

Finally, we can use our model to predict the result of the 20 experiments in our validation data set.   


```r
testdata <- read.csv("pml-testing.csv")
testdata <- testdata[,-nonPredictors]
testdata[testdata==""] <- NA
testdata <- testdata[,colSums(is.na(testdata)) < (nrow(testdata) * 0.95)]
testdataPCA <- predict(preProc,testdata[,1:52])
testdata$classe <- predict(fitRF,testdataPCA)
testdata$classe
```

```
##  [1] B A C A A E D B A A B C B A E E A B B B
## Levels: A B C D E
```
