# CLASSIFICATION-PROBLEM
Decision tree, knn and naive bayes

DT,knn,nb
================
Archit Rao
10 November 2017

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
#install.packages("corrplot")
library(corrplot)
```

    ## corrplot 0.84 loaded

``` r
library(ggplot2)
#install.packages("VIM")
library(VIM)
```

    ## Loading required package: colorspace

    ## Loading required package: grid

    ## Loading required package: data.table

    ## 
    ## Attaching package: 'data.table'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     between, first, last

    ## VIM is ready to use. 
    ##  Since version 4.0.0 the GUI is in its own package VIMGUI.
    ## 
    ##           Please use the package to use the new (and old) GUI.

    ## Suggestions and bug-reports can be submitted at: https://github.com/alexkowa/VIM/issues

    ## 
    ## Attaching package: 'VIM'

    ## The following object is masked from 'package:datasets':
    ## 
    ##     sleep

``` r
#install.packages("mice")
library(mice)
```

    ## Loading required package: lattice

``` r
library(tree)
#install.packages("class")
library(class)
#install.packages("e1071")
library(e1071)
```

Data exploration, imputation and visualization

``` r
#reading data
data_h<-read.csv("C:/Users/Administrator/Desktop/Machine Learning/Hackaton_1/Model_Data_updated.csv",header = FALSE)
summary(data_h)
```

    ##        V1                        V2              V3         
    ##  Min.   :17.00    Private         :26636   Min.   :  12285  
    ##  1st Qu.:28.00    Self-emp-not-inc: 3036   1st Qu.: 117525  
    ##  Median :37.00    Local-gov       : 2485   Median : 178037  
    ##  Mean   :38.67    ?               : 2185   Mean   : 189665  
    ##  3rd Qu.:48.00    State-gov       : 1543   3rd Qu.: 237993  
    ##  Max.   :90.00    Self-emp-inc    : 1329   Max.   :1490400  
    ##                  (Other)          : 1184                    
    ##              V4              V5                             V6       
    ##   HS-grad     :12338   Min.   : 1.00    Divorced             : 5293  
    ##   Some-college: 8564   1st Qu.: 9.00    Married-AF-spouse    :   32  
    ##   Bachelors   : 6335   Median :10.00    Married-civ-spouse   :17589  
    ##   Masters     : 2097   Mean   :10.08    Married-spouse-absent:  496  
    ##   Assoc-voc   : 1625   3rd Qu.:12.00    Never-married        :12609  
    ##   11th        : 1415   Max.   :16.00    Separated            : 1206  
    ##  (Other)      : 6024                    Widowed              : 1173  
    ##                 V7                      V8       
    ##   Prof-specialty : 4861    Husband       :15491  
    ##   Craft-repair   : 4790    Not-in-family : 9868  
    ##   Exec-managerial: 4779    Other-relative: 1189  
    ##   Adm-clerical   : 4437    Own-child     : 5939  
    ##   Sales          : 4295    Unmarried     : 4076  
    ##   Other-service  : 3879    Wife          : 1835  
    ##  (Other)         :11357                          
    ##                    V9             V10             V11       
    ##   Amer-Indian-Eskimo:  371    Female:12814   Min.   :    0  
    ##   Asian-Pac-Islander: 1177    Male  :25584   1st Qu.:    0  
    ##   Black             : 3683                   Median :    0  
    ##   Other             :  310                   Mean   : 1082  
    ##   White             :32857                   3rd Qu.:    0  
    ##                                              Max.   :99999  
    ##                                                             
    ##       V12               V13                   V14             V15       
    ##  Min.   :   0.00   Min.   : 1.0    United-States:34479    <=50K :19235  
    ##  1st Qu.:   0.00   1st Qu.:40.0    Mexico       :  750    <=50K.: 9937  
    ##  Median :   0.00   Median :40.0    ?            :  656    >50K  : 6147  
    ##  Mean   :  86.82   Mean   :40.4    Philippines  :  235    >50K. : 3079  
    ##  3rd Qu.:   0.00   3rd Qu.:45.0    Germany      :  161                  
    ##  Max.   :4356.00   Max.   :99.0    Puerto-Rico  :  144                  
    ##                                   (Other)       : 1973

``` r
sample_data_names<-read.csv("C:/Users/Administrator/Desktop/Machine Learning/Hackaton_1/Model_Data_Sample1.csv",header = FALSE)

#adding column names
#checking if the number of  col names a same
ncol(data_h)
```

    ## [1] 15

``` r
nrow(sample_data_names)#colum names are mentioned rowwise
```

    ## [1] 15

``` r
names=unlist(sample_data_names[,1])
colnames(data_h)<-names

#renaming names haing hyphen
names(data_h)[5]<-"educationnum"
names(data_h)[6]<-"maritalstatus"
names(data_h)[11]<-"capitalgain"      
names(data_h)[12]<-"capitalloss"
names(data_h)[13]<-"hoursperweek"
names(data_h)[14]<-"nativecountry"


str(data_h)
```

    ## 'data.frame':    38398 obs. of  15 variables:
    ##  $ age          : int  43 25 23 65 22 34 33 43 49 33 ...
    ##  $ workclass    : Factor w/ 9 levels " ?"," Federal-gov",..: 5 5 5 5 5 3 5 5 5 5 ...
    ##  $ fnlwgt       : int  346189 220931 134446 136384 54164 382078 269705 170482 171128 145402 ...
    ##  $ education    : Factor w/ 16 levels " 10th"," 11th",..: 13 10 12 13 12 10 12 12 6 12 ...
    ##  $ educationnum : int  14 13 9 14 9 13 9 9 4 9 ...
    ##  $ maritalstatus: Factor w/ 7 levels " Divorced"," Married-AF-spouse",..: 3 5 6 3 5 3 3 6 3 3 ...
    ##  $ occupation   : Factor w/ 15 levels " ?"," Adm-clerical",..: 5 11 8 11 9 5 7 8 8 8 ...
    ##  $ relationship : Factor w/ 6 levels " Husband"," Not-in-family",..: 1 2 5 1 2 1 1 2 1 6 ...
    ##  $ race         : Factor w/ 5 levels " Amer-Indian-Eskimo",..: 5 5 3 5 5 5 5 5 3 5 ...
    ##  $ sex          : Factor w/ 2 levels " Female"," Male": 2 2 2 2 2 2 2 1 2 1 ...
    ##  $ capitalgain  : int  0 0 0 0 14084 3103 0 0 0 0 ...
    ##  $ capitalloss  : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ hoursperweek : int  50 43 54 50 60 50 40 44 40 35 ...
    ##  $ nativecountry: Factor w/ 42 levels " ?"," Cambodia",..: 40 30 40 40 40 40 40 40 40 40 ...
    ##  $ compensation : Factor w/ 4 levels " <=50K"," <=50K.",..: 4 2 2 4 4 4 2 2 2 2 ...

``` r
summary(data_h)
```

    ##       age                    workclass         fnlwgt       
    ##  Min.   :17.00    Private         :26636   Min.   :  12285  
    ##  1st Qu.:28.00    Self-emp-not-inc: 3036   1st Qu.: 117525  
    ##  Median :37.00    Local-gov       : 2485   Median : 178037  
    ##  Mean   :38.67    ?               : 2185   Mean   : 189665  
    ##  3rd Qu.:48.00    State-gov       : 1543   3rd Qu.: 237993  
    ##  Max.   :90.00    Self-emp-inc    : 1329   Max.   :1490400  
    ##                  (Other)          : 1184                    
    ##          education      educationnum                  maritalstatus  
    ##   HS-grad     :12338   Min.   : 1.00    Divorced             : 5293  
    ##   Some-college: 8564   1st Qu.: 9.00    Married-AF-spouse    :   32  
    ##   Bachelors   : 6335   Median :10.00    Married-civ-spouse   :17589  
    ##   Masters     : 2097   Mean   :10.08    Married-spouse-absent:  496  
    ##   Assoc-voc   : 1625   3rd Qu.:12.00    Never-married        :12609  
    ##   11th        : 1415   Max.   :16.00    Separated            : 1206  
    ##  (Other)      : 6024                    Widowed              : 1173  
    ##             occupation             relationship  
    ##   Prof-specialty : 4861    Husband       :15491  
    ##   Craft-repair   : 4790    Not-in-family : 9868  
    ##   Exec-managerial: 4779    Other-relative: 1189  
    ##   Adm-clerical   : 4437    Own-child     : 5939  
    ##   Sales          : 4295    Unmarried     : 4076  
    ##   Other-service  : 3879    Wife          : 1835  
    ##  (Other)         :11357                          
    ##                   race            sex         capitalgain   
    ##   Amer-Indian-Eskimo:  371    Female:12814   Min.   :    0  
    ##   Asian-Pac-Islander: 1177    Male  :25584   1st Qu.:    0  
    ##   Black             : 3683                   Median :    0  
    ##   Other             :  310                   Mean   : 1082  
    ##   White             :32857                   3rd Qu.:    0  
    ##                                              Max.   :99999  
    ##                                                             
    ##   capitalloss       hoursperweek         nativecountry    compensation  
    ##  Min.   :   0.00   Min.   : 1.0    United-States:34479    <=50K :19235  
    ##  1st Qu.:   0.00   1st Qu.:40.0    Mexico       :  750    <=50K.: 9937  
    ##  Median :   0.00   Median :40.0    ?            :  656    >50K  : 6147  
    ##  Mean   :  86.82   Mean   :40.4    Philippines  :  235    >50K. : 3079  
    ##  3rd Qu.:   0.00   3rd Qu.:45.0    Germany      :  161                  
    ##  Max.   :4356.00   Max.   :99.0    Puerto-Rico  :  144                  
    ##                                   (Other)       : 1973

``` r
levels(data_h$occupation)
```

    ##  [1] " ?"                 " Adm-clerical"      " Armed-Forces"     
    ##  [4] " Craft-repair"      " Exec-managerial"   " Farming-fishing"  
    ##  [7] " Handlers-cleaners" " Machine-op-inspct" " Other-service"    
    ## [10] " Priv-house-serv"   " Prof-specialty"    " Protective-serv"  
    ## [13] " Sales"             " Tech-support"      " Transport-moving"

``` r
levels(data_h$nativecountry)
```

    ##  [1] " ?"                          " Cambodia"                  
    ##  [3] " Canada"                     " China"                     
    ##  [5] " Columbia"                   " Cuba"                      
    ##  [7] " Dominican-Republic"         " Ecuador"                   
    ##  [9] " El-Salvador"                " England"                   
    ## [11] " France"                     " Germany"                   
    ## [13] " Greece"                     " Guatemala"                 
    ## [15] " Haiti"                      " Holand-Netherlands"        
    ## [17] " Honduras"                   " Hong"                      
    ## [19] " Hungary"                    " India"                     
    ## [21] " Iran"                       " Ireland"                   
    ## [23] " Italy"                      " Jamaica"                   
    ## [25] " Japan"                      " Laos"                      
    ## [27] " Mexico"                     " Nicaragua"                 
    ## [29] " Outlying-US(Guam-USVI-etc)" " Peru"                      
    ## [31] " Philippines"                " Poland"                    
    ## [33] " Portugal"                   " Puerto-Rico"               
    ## [35] " Scotland"                   " South"                     
    ## [37] " Taiwan"                     " Thailand"                  
    ## [39] " Trinadad&Tobago"            " United-States"             
    ## [41] " Vietnam"                    " Yugoslavia"

``` r
levels(data_h$workclass)
```

    ## [1] " ?"                " Federal-gov"      " Local-gov"       
    ## [4] " Never-worked"     " Private"          " Self-emp-inc"    
    ## [7] " Self-emp-not-inc" " State-gov"        " Without-pay"

``` r
#"?" needs to be addres in Occupation and native country
data_h%>%filter(data_h$workclass==" ?" & occupation==" ?" )%>%nrow() #2185
```

    ## [1] 2185

``` r
data_h%>%group_by(occupation)%>%summarise(n()) #2195
```

    ## # A tibble: 15 x 2
    ##    occupation           `n()`
    ##    <fct>                <int>
    ##  1 " ?"                  2195
    ##  2 " Adm-clerical"       4437
    ##  3 " Armed-Forces"          9
    ##  4 " Craft-repair"       4790
    ##  5 " Exec-managerial"    4779
    ##  6 " Farming-fishing"    1180
    ##  7 " Handlers-cleaners"  1624
    ##  8 " Machine-op-inspct"  2354
    ##  9 " Other-service"      3879
    ## 10 " Priv-house-serv"     200
    ## 11 " Prof-specialty"     4861
    ## 12 " Protective-serv"     785
    ## 13 " Sales"              4295
    ## 14 " Tech-support"       1162
    ## 15 " Transport-moving"   1848

``` r
data_h%>%group_by(nativecountry)%>%summarise(n()) #656
```

    ## # A tibble: 42 x 2
    ##    nativecountry         `n()`
    ##    <fct>                 <int>
    ##  1 " ?"                    656
    ##  2 " Cambodia"              21
    ##  3 " Canada"               142
    ##  4 " China"                100
    ##  5 " Columbia"              69
    ##  6 " Cuba"                 102
    ##  7 " Dominican-Republic"    76
    ##  8 " Ecuador"               36
    ##  9 " El-Salvador"          122
    ## 10 " England"               95
    ## # ... with 32 more rows

``` r
2185/nrow(data_h)*100
```

    ## [1] 5.690401

``` r
#number of ? is approx 5% so they can be ignored
data_wh<-data_h%>%filter(!data_h$workclass==" ?" & !occupation==" ?")

#refactor compensation appropriatly
data_h$compensation<-as.character(data_h$compensation)
data_h$compensation[data_h$compensation==" <=50K."]=" <=50K"
data_h$compensation[data_h$compensation==" >50K." ]=" >50K"

data_h$compensation<-as.factor(data_h$compensation)
levels(data_h$compensation)
```

    ## [1] " <=50K" " >50K"

``` r
#test and train
set.seed(14)
samp<-sample.int(n=nrow(data_wh),size=floor(.80*nrow(data_wh)),replace =F)

train_data<-data_h[samp,]
test_data<-data_h[-samp,]
ggplot(data_h,aes(data_h$educationnum,data_h$education))+geom_point()
```

![](Hackaton_1_files/figure-markdown_github/data%20exploration,%20imputation%20and%20visualization%20approach-1.png)

``` r
#booth  are corelated
```

Building 3 Models, each using one of different type of algorithm.

``` r
tree.model<-tree(compensation~age+workclass+fnlwgt+education+maritalstatus+occupation+relationship+race+sex+capitalgain+capitalloss+hoursperweek,data=train_data)
#tree.model
summary(tree.model)
```

    ## 
    ## Classification tree:
    ## tree(formula = compensation ~ age + workclass + fnlwgt + education + 
    ##     maritalstatus + occupation + relationship + race + sex + 
    ##     capitalgain + capitalloss + hoursperweek, data = train_data)
    ## Variables actually used in tree construction:
    ## [1] "relationship" "capitalgain"  "education"    "occupation"  
    ## Number of terminal nodes:  8 
    ## Residual mean deviance:  0.6969 = 20180 / 28950 
    ## Misclassification error rate: 0.1555 = 4503 / 28962

``` r
plot(tree.model)
text(tree.model)
```

![](Hackaton_1_files/figure-markdown_github/decision%20tree%20model-1.png)

``` r
model_prediction<-predict(tree.model,test_data)


#maxidx gets  the maximum from each row of the model_prediction probabilities
maxidx<-function(arr){
  return(which(arr ==max(arr)))}

#applies maxidx on model_prediciton #c(1) specifes to rum on rows
idx=apply(model_prediction,c(1),maxidx)
modelprediction<-c(' <=50K',' >50K')[idx]

confmat=table(modelprediction,test_data$compensation)

accuracy=sum(diag(confmat))/sum(confmat)
accuracy
```

    ## [1] 0.8483468

``` r
ncol(data_h)
```

    ## [1] 15

``` r
#without labels
knn_train_data<-train_data[,-15]
knn_test_data<-test_data[,-15]

str(knn_train_data)
```

    ## 'data.frame':    28962 obs. of  14 variables:
    ##  $ age          : int  21 28 52 56 47 41 60 30 45 62 ...
    ##  $ workclass    : Factor w/ 9 levels " ?"," Federal-gov",..: 8 5 5 5 5 5 5 6 5 5 ...
    ##  $ fnlwgt       : int  41183 224964 165998 147202 187969 238384 389254 119422 190088 24515 ...
    ##  $ education    : Factor w/ 16 levels " 10th"," 11th",..: 16 12 16 16 2 12 12 16 12 7 ...
    ##  $ educationnum : int  10 9 10 10 7 9 9 10 9 5 ...
    ##  $ maritalstatus: Factor w/ 7 levels " Divorced"," Married-AF-spouse",..: 5 5 3 1 1 1 3 3 4 3 ...
    ##  $ occupation   : Factor w/ 15 levels " ?"," Adm-clerical",..: 11 9 8 2 9 14 2 5 2 5 ...
    ##  $ relationship : Factor w/ 6 levels " Husband"," Not-in-family",..: 4 4 1 2 2 5 1 1 5 1 ...
    ##  $ race         : Factor w/ 5 levels " Amer-Indian-Eskimo",..: 5 5 5 5 5 5 5 5 5 5 ...
    ##  $ sex          : Factor w/ 2 levels " Female"," Male": 1 1 2 1 1 1 2 2 1 2 ...
    ##  $ capitalgain  : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ capitalloss  : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ hoursperweek : int  20 25 40 45 38 40 40 70 30 40 ...
    ##  $ nativecountry: Factor w/ 42 levels " ?"," Cambodia",..: 40 40 40 12 40 40 40 40 40 40 ...

``` r
#Just choosing numerical colums
Numericcol_knn_train_data<-knn_train_data[,c(1,3,5,11,12,13)]
Numericcol_knn_test_data<-knn_test_data[,c(1,3,5,11,12,13)]

#with labels
#without labels
Labels_knn_train_data<-train_data[,15]
Labels_knn_test_data<-test_data[,15]
#Just numerical colums



for(k in 1:31){
knn.model<-knn(train=Numericcol_knn_train_data,test=Numericcol_knn_test_data,cl=Labels_knn_train_data,k)
confmat_knn<-table(Labels_knn_test_data,knn.model)
accuracy_knn=sum(diag(confmat_knn))/sum(confmat_knn)
cat(k," ",accuracy_knn,"\n")
}
```

    ## 1   0.7317719 
    ## 2   0.717677 
    ## 3   0.7616575 
    ## 4   0.7551929 
    ## 5   0.7740568 
    ## 6   0.7754345 
    ## 7   0.7888936 
    ## 8   0.7894235 
    ## 9   0.7955702 
    ## 10   0.796418 
    ## 11   0.7979017 
    ## 12   0.7974777 
    ## 13   0.7999152 
    ## 14   0.800869 
    ## 15   0.8021407 
    ## 16   0.8037304 
    ## 17   0.8034125 
    ## 18   0.8035184 
    ## 19   0.8041543 
    ## 20   0.8041543 
    ## 21   0.8041543 
    ## 22   0.8053201 
    ## 23   0.805638 
    ## 24   0.805744 
    ## 25   0.8058499 
    ## 26   0.8053201 
    ## 27   0.8060619 
    ## 28   0.805638 
    ## 29   0.805744 
    ## 30   0.8059559 
    ## 31   0.805638

``` r
naive.model<-naiveBayes(compensation~.,data=train_data)

pred_naive<-predict(naive.model,test_data[,-15])

confmat_naive<-table(pred_naive,test_data[,15])

accuracy_naive<-sum(diag(confmat_naive))/sum(confmat_naive)
print(accuracy_naive)
```

    ## [1] 0.8290589

Predicting model performance on each of the 3 models.

``` r
model1_accuracy=cat(accuracy)
```

    ## 0.8483468

``` r
model2_accuracy_for_k13 =0.8003391 
model2_accuracy_for_k30= 0.8061679 
modell3_accuracy=cat(accuracy_naive)
```

    ## 0.8290589
