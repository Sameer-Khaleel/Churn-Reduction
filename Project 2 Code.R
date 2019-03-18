#clean the workspace
rm(list=ls())

#set working directory
setwd("E:/study/data/Project#2")
getwd()
#load libraries
x=c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
    "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees','knitr','tidyr','xtable',"varhandle",'MASS','psych')
library(DMwR)
lapply(x, require, character.only = TRUE)
rm(x)

#load datasets
train =read.csv("Train.csv")
test=read.csv("Test.csv")
df=rbind(train,test)

#exploring the data: checking the data type of the variables and if there are any missing values
str(df)
table(is.na(df))

##changing the factor to catagorical factor
#for df
for(i in 1:ncol(df)){
  
  if(class(df[,i]) == 'factor'){
    
    df[i] = factor(df[,i], labels=(1:length(levels(factor(df[,i])))))
    
  }
}
#For train
for(i in 1:ncol(train)){
  
  if(class(train[,i]) == 'factor'){
    
    train[i] = factor(train[,i], labels=(1:length(levels(factor(train[,i])))))
    
  }
}
#For test
for(i in 1:ncol(test)){
  
  if(class(test[,i]) == 'factor'){
    
    test[i] = factor(test[,i], labels=(1:length(levels(factor(test[,i])))))
    
  }
}

#################exploratory analaysis############# removing the variables which are not important
df$phone.number=NULL
train$phone.number=NULL
test$phone.number=NULL
#changing the datatype for Factor variables
df$state=unfactor(df$state)
df$international.plan=unfactor(df$international.plan)
df$voice.mail.plan=unfactor(df$voice.mail.plan)
#selecting only numeric data for boxplot
numeric_index = sapply(df,is.numeric)
numeric_data = df[,numeric_index]
cnames = colnames(numeric_data)
#box plot analysis

for (i in 1:length(cnames))
{
  assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i]), x = "Churn"), data = subset(df))+ 
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=cnames[i],x="Churn")+
           ggtitle(paste("Box plot of Churn for",cnames[i])))
}
gridExtra::grid.arrange(gn1,gn4,gn5,gn7,ncol=4)
gridExtra::grid.arrange(gn15,gn19,gn8,gn9,ncol=4)
gridExtra::grid.arrange(gn10,gn11,gn12,gn13,ncol=4)
gridExtra::grid.arrange(gn14,gn18,gn16,gn17,ncol=4)




#Histograms

multi.hist(numeric_data,main=NA,dcol=c("blue","red"),dlty=c("solid","solid"),bcol="pink")
#histogram
hist(df$voice.mail.plan)
hist(df$international.plan)

# Correaltion analysis
corrgram(df[,cnames], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")


#Feature Selection
impvar=randomForest(Churn~.,data=df,ntree=100,keep.forest=F,importance=T)
importance(impvar,type=1) 

## based on the importance we remove area code and state from test and train dataset
df$area.code=NULL
df$state=NULL
test$account.length=NULL
test$state=NULL
train$state=NULL
train$account.length=NULL

# taking the histogram for target variable
df$Churn=unfactor(df$Churn)
h1=hist(df$Churn)

#model development model1
#Decision tree for classification
#Develop Model on training data
C50_model = C5.0(Churn ~., train, trials = 100, rules = TRUE)

#Summary of DT model
summary(C50_model)

#write rules into disk
write(capture.output(summary(C50_model)), "c50Rules.txt")

#Lets predict for test cases

C50_Predictions = predict(C50_model, test, type = "class")

##Evaluate the performance of classification model
ConfMatrix_C50 = table(test$Churn, C50_Predictions)
confusionMatrix(ConfMatrix_C50)

#False Negative rate
#FNR = FN/FN+TP 
#accuracy
#FNR
## Random Forest method
RF_model = randomForest(Churn ~ ., train, importance = TRUE, ntree = 500)

#Extract rules fromn random forest
#transform rf object to an inTrees' format
treeList = RF2List(RF_model)  
# 
# #Extract rules
exec = extractRules(treeList, train[-18])  # R-executable conditions
# 
# #Visualize some rules
exec[1:2,]
# 
# #Make rules more readable:
readableRules = presentRules(exec, colnames(train))
readableRules[1:2,]
# 
# #Get rule metrics
ruleMetric = getRuleMetric(exec, train[,-18], train$Churn)  # get rule metrics
# 
# #evaulate few rules
ruleMetric[1:2,]

#Presdict test data using random forest model
RF_Predictions = predict(RF_model, test[,-18],)

##Evaluate the performance of classification model
ConfMatrix_RF = table(test$Churn, RF_Predictions)
confusionMatrix(ConfMatrix_RF)
#Accuracy :96%
#FNR-26%
#False Negative rate
#FNR = FN/FN+TP 

#naive Bayes
library(e1071)

#Develop model
NB_model = naiveBayes(Churn ~ ., data = train)

#predict on test cases #raw
NB_Predictions = predict(NB_model, test[,1:17], type = 'class')

#Look at confusion matrix
Conf_matrix = table(observed = test[,18], predicted = NB_Predictions)
confusionMatrix(Conf_matrix)

#Accuracy: 89.32%
#FNR: 57.58


##**************************************************************************************************************
# #Model 2 create NA on  outliners and compute them with mean and compare the result
#with the previous random forest and select the best model

for(i in cnames){
  val = df[,i][df[,i] %in% boxplot.stats(df[,i])$out]
  print(length(val))
  df[,i][df[,i] %in% val] = NA
}

# 
df$number.customer.service.calls [is.na(df$number.customer.service.calls )] = mean(df$number.customer.service.calls, na.rm = T)
df$international.plan [is.na(df$international.plan )] = mean(df$international.plan, na.rm = T)
df$number.vmail.messages [is.na(df$number.vmail.messages )] = mean(df$number.vmail.messages, na.rm = T)
df$total.day.minutes [is.na(df$total.day.minutes )] = mean(df$total.day.minutes, na.rm = T)
df$total.day.calls [is.na(df$total.day.calls )] = mean(df$total.day.calls, na.rm = T)
df$total.day.charge [is.na(df$total.day.charge )] = mean(df$total.day.charge, na.rm = T)
df$total.eve.minutes [is.na(df$total.eve.minutes )] = mean(df$total.eve.minutes, na.rm = T)
df$total.eve.calls [is.na(df$total.eve.calls )] = mean(df$total.eve.calls, na.rm = T)
df$total.eve.charge [is.na(df$total.eve.charge )] = mean(df$total.eve.charge, na.rm = T)
df$total.night.minutes [is.na(df$total.night.minutes )] = mean(df$total.night.minutes, na.rm = T)
df$total.night.calls [is.na(df$total.night.calls )] = mean(df$total.night.calls, na.rm = T)
df$total.night.charge [is.na(df$total.night.charge )] = mean(df$total.night.charge, na.rm = T)
df$total.intl.minutes [is.na(df$total.intl.minutes )] = mean(df$total.intl.minutes, na.rm = T)
df$total.intl.calls [is.na(df$total.intl.calls )] = mean(df$total.intl.calls, na.rm = T)
df$total.intl.charge [is.na(df$total.intl.charge )] = mean(df$total.intl.charge, na.rm = T)
df$account.length [is.na(df$account.length  )] = mean(df$account.length , na.rm = T)
table(is.na(df))
## Random Forest method
train.index = createDataPartition(df$Churn, p = .80, list = FALSE)
train = df[ train.index,]
test  = df[-train.index,]

RF_model = randomForest(Churn ~ ., train, importance = TRUE, ntree = 500)

#Extract rules fromn random forest
#transform rf object to an inTrees' format
treeList = RF2List(RF_model)  
# 
# #Extract rules
exec = extractRules(treeList, train[-18])  # R-executable conditions
# 
# #Visualize some rules
exec[1:2,]
# 
# #Make rules more readable:
readableRules = presentRules(exec, colnames(train))
readableRules[1:2,]
# 
# #Get rule metrics
ruleMetric = getRuleMetric(exec, train[,-18], train$Churn)  # get rule metrics
# 
# #evaulate few rules
ruleMetric[1:2,]

#Presdict test data using random forest model
RF_Predictions = predict(RF_model, test[,-18],)

##Evaluate the performance of classification model
ConfMatrix_RF = table(test$Churn, RF_Predictions)
confusionMatrix(ConfMatrix_RF)
#accuracy=91
#FNR=0.531
## since model1 is better, we select model 1
