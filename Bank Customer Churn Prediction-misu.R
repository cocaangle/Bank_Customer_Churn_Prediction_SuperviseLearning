train=read.csv("train.csv")
test=read.csv("test.csv")
#Exploratory data
str(train)
str(test)
table(is.na(train))
table(is.na(test))
summary(train)#some missing values are not NA, use summary can detect them out
colSums(is.na(train))

#Gender has 13 missing values
#Married has 3 missing values
#dependentss has 15 missing values
#self_emloyed has 32 missing values
#Loan amount has 22 missing values
#laon_amount_term has 14 missing values

##Data visulisation
library(dplyr)
library(ggplot2)
#1. loan status VS gender
train%>%count(Loan_Status,Gender)
g1=train%>%filter(Gender!="")%>%count(Loan_Status,Gender)%>%ungroup%>%group_by(Loan_Status)%>%
  mutate(pct=round(n/sum(n),2))
g1
ggplot(g1,aes(x=Loan_Status,y=pct))+geom_bar(stat="identity",aes(fill=Loan_Status))+
  facet_grid(.~Gender)+geom_text(aes(label=round(pct,2),y=pct+0.03))+theme_bw()+
  theme(axis.title = element_text(size=12),axis.text = element_text(size=10))+scale_fill_brewer(palette = "Set2")
##scale_fill_brewer , quality
#Insight: there's no difference between female and male

#2. Married VS gender
train%>%count(Married,Loan_Status)
g2=train%>%filter(Married!="")%>%count(Loan_Status,Married)%>%ungroup%>%group_by(Married)%>%
  mutate(pct=round(n/sum(n),2))
ggplot(g2,aes(x=Loan_Status,y=pct))+geom_bar(stat="identity",aes(fill=Loan_Status))+
  facet_grid(.~Married)+geom_text(aes(label=round(pct,2),y=pct+0.03))+theme_bw()+
  theme(axis.title = element_text(size=12),axis.text = element_text(size=10))+scale_fill_brewer(palette = "Set2")
#Insight: Married people have a slight higher approve rate for loan

#3. Dependents VS loan status
train%>%count(Dependents,Loan_Status)
g3=train%>%filter(Dependents!="")%>%count(Loan_Status,Dependents)%>%ungroup%>%group_by(Dependents)%>%
  mutate(pct=round(n/sum(n),2))
ggplot(g3,aes(x=Loan_Status,y=pct))+geom_bar(stat="identity",aes(fill=Loan_Status))+
  facet_grid(.~Dependents)+geom_text(aes(label=round(pct,2),y=pct+0.03))+theme_bw()+
  theme(axis.title = element_text(size=12),axis.text = element_text(size=10))+scale_fill_brewer(palette = "Set2")
#People who have 2 dependents have the highest approve rate, which is 75%

#4.Education VS loan status
g4=train%>%filter(Education!="")%>%count(Loan_Status,Education)%>%ungroup%>%group_by(Education)%>%
  mutate(pct=round(n/sum(n),2))
ggplot(g4,aes(x=Loan_Status,y=pct))+geom_bar(stat="identity",aes(fill=Loan_Status))+
  facet_grid(.~Education)+geom_text(aes(label=round(pct,2),y=pct+0.03))+theme_bw()+
  theme(axis.title = element_text(size=12),axis.text = element_text(size=10))+scale_fill_brewer(palette = "Set1")
# Graduate people have higher approvement rate, which is 71%

#5 Self-employee VS loan status
g5=train%>%filter(Self_Employed!="")%>%count(Loan_Status,Self_Employed)%>%ungroup%>%group_by(Self_Employed)%>%
  mutate(pct=round(n/sum(n),2))
ggplot(g5,aes(x=Loan_Status,y=pct))+geom_bar(stat="identity",aes(fill=Loan_Status))+
  facet_grid(.~Self_Employed)+geom_text(aes(label=round(pct,2),y=pct+0.03))+theme_bw()+
  theme(axis.title = element_text(size=12),axis.text = element_text(size=10))+scale_fill_brewer()
# there's no significant relationship between self employed and loan_status

#6 Property area VS loan status
g6=train%>%filter(Property_Area!="")%>%count(Loan_Status,Property_Area)%>%ungroup%>%group_by(Loan_Status)%>%
  mutate(pct=round(n/sum(n),2))
g6
ggplot(g6,aes(x=Loan_Status,y=pct))+geom_bar(stat="identity",aes(fill=Loan_Status))+
  facet_grid(.~Property_Area)+geom_text(aes(label=round(pct,2),y=pct+0.03))+theme_bw()+
  theme(axis.title = element_text(size=12),axis.text = element_text(size=10))+scale_fill_brewer(palette = "Set2")
#Semiurban has the highest approvement rate

#7 appliantincome VS loan status
table(is.na(train$ApplicantIncome))
train%>%count(Loan_Status,ApplicantIncome)
#%>%group_by(Loan_Status)%>%summarise(mean(ApplicantIncome))
g7=train%>%group_by(Loan_Status)%>%summarise(mean(ApplicantIncome))
ggplot(train,aes(x=Loan_Status,y=ApplicantIncome))+geom_bar(stat="summary",fun.y="mean",aes(fill=Loan_Status))+
  theme_bw()
#There's no significant difference for loan status as for applicantincome
  
#8 credit history
str(train$Credit_History)
train%>%count(Loan_Status, Credit_History)
g8=train%>%filter(Credit_History!="")%>%count(Loan_Status, Credit_History)%>%ungroup%>%group_by(Loan_Status)%>%
  mutate(pct=round(n/sum(n),2))
g8
ggplot(g8,aes(x=Loan_Status,y=Credit_History))+geom_bar(stat="identity",aes(fill=Loan_Status))+
  facet_grid(.~Credit_History)+geom_text(aes(label=round(pct,2),y=pct+0.03))+theme_bw()+
  theme(axis.title = element_text(size=12),axis.text = element_text(size=10))
#Credit history meets guidelines is the very important element to be approved
#9 loan amount
str(train$LoanAmount)
table(is.na(train$LoanAmount))
train%>%count(LoanAmount,Loan_Status)
g9=train%>%filter(LoanAmount!="")%>%group_by(Loan_Status)%>%summarise(mean(LoanAmount))
g9
ggplot(train,aes(x=Loan_Status,y=LoanAmount))+geom_bar(stat="summary",fun.y="mean",aes(fill=Loan_Status))+
  theme_bw()+
  theme(axis.title=element_text(size=12),axis.text=element_text(size=10))
#The average LoanAmount for not approved is slightly higher than what approved

#10 coapplicant income
str(train$CoapplicantIncome)
table(is.na(train$CoapplicantIncome))
g10=train%>%group_by(Loan_Status)%>%mutate(mean=mean(CoapplicantIncome))
ggplot(train,aes(x=Loan_Status,y=CoapplicantIncome))+geom_bar(stat="summary",fun.y="mean",aes(fill=Loan_Status))+theme_bw()+
  theme(axis.title=element_text(size=12),axis.text=element_text(size=10))
ggplot(train, aes(x = Loan_Status, y = CoapplicantIncome)) +
  geom_bar(stat = 'summary', fun.y = 'mean', aes(fill = Loan_Status)) +
  theme_bw()
#When CoapplicantIncome is higher, it is harder to get the loan approvement.

##Data cleaning
#1. Impute missing value for gender
#1.1 Loan status=N
train%>%count(Loan_Status,Gender)%>%filter(Gender!=""&Loan_Status=="N")%>%
  ungroup%>%group_by(Loan_Status)%>%mutate(pct=round(n/sum(n),2))
##See the portions of male and female
df_n=train[train$Gender==""&train$Loan_Status=="N",]
##Choose the missing value rows
set.seed(123)
df_n_M=train%>%filter(Gender==""&Loan_Status=="N")%>%
  sample_n(5*0.8)
##Choose 80% missing value to assign to men
Male=df_n_M$Loan_ID
Male
Female=setdiff(df_n$Loan_ID,df_n_M$Loan_ID)
Female
train$Gender=ifelse(train$Loan_ID%in%Male,"Male",
                   ifelse(train$Loan_ID%in%Female,"Female",as.character(train$Gender)))
train%>%filter(Loan_Status=="N")%>%count(Gender)

#1.2 Loan_Status=Y
train%>%count(Loan_Status,Gender)%>%filter(Gender!=""&Loan_Status=="Y")%>%
  ungroup%>%group_by(Loan_Status)%>%mutate(pct=round(n/sum(n),2))
##See the portions of male and female
df_y=train[train$Gender==""&train$Loan_Status=="Y",]
##Choose the missing value rows
set.seed(123)
df_y_M=train%>%filter(Gender==""&Loan_Status=="Y")%>%
  sample_n(8*0.82)
##Choose 80% missing value to assign to men
Male=df_y_M$Loan_ID
Male
Female=setdiff(df_y$Loan_ID,df_y_M$Loan_ID)
Female
train$Gender=ifelse(train$Loan_ID%in%Male,"Male",
                    ifelse(train$Loan_ID%in%Female,"Female",as.character(train$Gender)))
train%>%filter(Loan_Status=="Y")%>%count(Gender)
summary(train)
##only character can be filled

#2 impute missing values for married
#2.1 Loan_Status=Y
train%>%count(Loan_Status,Married)%>%filter(Married!=""&Loan_Status=="Y")%>%ungroup%>%
  group_by(Loan_Status)%>%mutate(pct=round(n/sum(n),2))
df_y=train[train$Loan_Status=="Y"&train$Married=="",]
set.seed(123)
df_y_y=train%>%filter(Loan_Status=="Y"&Married=="")%>%
  sample_n(3*0.68)
yes=df_y_y$Loan_ID
no=setdiff(df_y$Loan_ID,df_y_y$Loan_ID)
yes
no
train$Married=ifelse(train$Loan_ID%in%yes,"yes",
                     ifelse(train$Loan_ID%in%no,"no",
                            as.character(train$Married)))
train%>%filter(Loan_Status=="Y")%>%count(Married)
table(is.na(train$Married))
#3. Imputing values for Dependents
#3.1Loan_status=Y
summary(train$Dependents)
str(train$Dependents)
as.character(train$Dependents)
train%>%count(Loan_Status,Dependents)%>%filter(Loan_Status=="Y"&Dependents!="")%>%ungroup%>%
  group_by(Loan_Status)%>%mutate(pct=round(n/sum(n),2))
df_Y=train[train$Loan_Status=="Y"&train$Dependents=="",]
set.seed(123)
df_Y_1=train%>%filter(Loan_Status=="Y"&Dependents=="")%>%sample_n(9*0.58)
one_two_three=setdiff(df_Y,df_Y_1)
df_Y_1=one_two_three%>%filter(Loan_Status=="Y"&Dependents=="")%>%sample_n(9*0.16)
one_two=setdiff(one_two_three,df_Y_1)
df_Y_2=one_two%>%filter(Loan_Status=="Y"&Dependents=="")%>%sample_n(9*0.18)
df_Y_3=setdiff(one_two,df_Y_2)
train$Dependents=ifelse(train$Loan_ID%in%df_Y_0,"0",
                        ifelse(train$Loan_ID%in%df_Y_1,"1",
                               ifelse(train$Loan_ID%in%df_Y_2,"2",
                                      ifelse(train$Loan_ID%in%df_Y_3,"3+",as.character(train$Dependents)))))
train%>%filter(Loan_Status=="Y")%>%count(Dependents)
summary(train$Dependents)
table(is.na(train$Dependents))
#4 Imputing values for self Employed
#4.1 Loan_status=Y
summary(train$Self_Employed)
train%>%count(Loan_Status,Self_Employed)%>%filter(Loan_Status=="Y"&Self_Employed!="")%>%
  ungroup%>%group_by(Loan_Status)%>%mutate(pct=round(n/sum(n),2))
df_y_se=train[train$Loan_Status=="Y"&train$Self_Employed=="",]
se_No=train%>%filter(Loan_Status=="Y"&train$Self_Employed=="")%>%sample_n(0.86*23)
se_No
se_Yes=setdiff(df_y_se,se_No)
train$Self_Employed=ifelse(train$Loan_ID%in%se_No,"No",
                           ifelse(train$Loan_ID%in%se_Yes,"Yes",as.character(train$Self_Employed)))
train%>%filter(Loan_Status=="Y")%>%count(Self_Employed)

#4.2 Loan_status=N
summary(train$Self_Employed)
train%>%count(Loan_Status,Self_Employed)%>%filter(Loan_Status=="N"&Self_Employed!="")%>%
  ungroup%>%group_by(Loan_Status)%>%mutate(pct=round(n/sum(n),2))
df_N_se=train[train$Loan_Status=="N"&train$Self_Employed=="",]
se_No=train%>%filter(Loan_Status=="N"&train$Self_Employed=="")%>%sample_n(0.86*23)
se_No
se_Yes=setdiff(df_y_se,se_No)
train$Self_Employed=ifelse(train$Loan_ID%in%se_No,"No",
                           ifelse(train$Loan_ID%in%se_Yes,"Yes",as.factor(train$Self_Employed)))
train%>%filter(Loan_Status=="Y")%>%count(Self_Employed)
#5. Imputing missing values for loanamount with mean value
table(is.na(train$LoanAmount))
mean=mean(train$LoanAmount[!is.na(train$LoanAmount)])
mean
train$LoanAmount=ifelse(is.na(train$LoanAmount),mean,train$LoanAmount)
table(is.na(train$LoanAmount))
#6. Imputing missing values for loanamount term with median value
summary(train$Loan_Amount_Term)
median=median(train$Loan_Amount_Term[!is.na(train$Loan_Amount_Term)])
train$Loan_Amount_Term=ifelse(is.na(train$Loan_Amount_Term),median,train$Loan_Amount_Term)
summary(train$Loan_Amount_Term)
summary(train)
#7. Imputing missing values for credit history
summary(train$Credit_History)
str(train$Credit_History)
#7.1 Loan_status=Y
train%>%count(Loan_Status,Credit_History)%>%filter(Credit_History!=""&Loan_Status=="Y")%>%ungroup%>%
  group_by(Loan_Status)%>%mutate(pct=round(n/sum(n),2))
df_Y=train[train$Loan_Status=="Y"&is.na(train$Credit_History),]
set.seed(123)
str(df_Y)
df_Y_1=train%>%filter(Loan_Status=="Y"&is.na(Credit_History))%>%
  sample_n(37*0.98)
one=df_Y_1$Loan_ID
zero=setdiff(df_Y$Loan_ID,df_Y_1$Loan_ID)
one
zero
train$Credit_History=ifelse(train$Loan_ID%in%one,"1",
                     ifelse(train$Loan_ID%in%zero,"0",
                            as.character(train$Credit_History)))
train%>%filter(Loan_Status=="Y")%>%count(Credit_History)
#7.2 Loan_status=N
train%>%count(Loan_Status,Credit_History)%>%filter(Credit_History!=""&Loan_Status=="N")%>%
  ungroup%>%group_by(Loan_Status)%>%mutate(pct=round(n/sum(n),2))
df_N=train[train$Loan_Status=="N"&is.na(train$Credit_History),]
df_N
set.seed(2)
str(df_N)
df_N_1=train%>%filter(Loan_Status=="N"&is.na(Credit_History))%>%sample_n(13*0.54)
one=df_N_1$Loan_ID
zero=setdiff(df_N$Loan_ID,df_N_1$Loan_ID)
train$Credit_History=ifelse(train$Loan_ID%in%one,"1",ifelse(train$Loan_ID%in%zero,"0",
                                                            as.character(train$Credit_History)))
train%>%filter(Loan_Status=="N")%>%count(Credit_History)
summary(train$Credit_History)
summary(train)
str(train)

summary(train)

#logistic regression model

#Build Model
train$Loan_Status = ifelse(train$Loan_Status == 'Y',1,0)
train2 =subset(train, select = -c(X,Loan_ID))
fit1 <- glm(Loan_Status~., family = binomial, data = train2)
summary(fit1)

#drop all insignificant attributes and rebuild model
keep = c("Credit_History", "Property_Area", "Loan_Status")
train3 = train[,names(train) %in% keep]
fit2 <- glm(Loan_Status ~., family = binomial, data = train3)
summary(fit2)

#Calculate Likelihood ration test(test of Independence)
anova(fit2,fit1, test = "Chisq")
#p=0.07586 which is greater than 0.05
#the performance of two models are not significant different


#Coefficients Interpretation
coef(fit2)
#the log odds increase by 3.84 if the applicant have the credit history
#the log odds increase by 0.92 if the applicant own property in the area of semi urban
#the log odds increase by 0.27 if the applciant own property in the area of urban

#exp(coefficients) Interpretation
#exp(coef) is the ratio of the odds
exp(coef(fit2))
#the odds of getting approval increase by a factor of 47 when applicants have credit history
#the odds of getting approval increase by a factor of 2.52 when applicants lives in the area of semi urban
#the odds of getting approval increase by a factor of 1.31 when applicants lives in the area of urban

##evaluation
#Miscalssification Rate
#Compare values in model with the orginial data 

pred_prob = predict(fit2, newdata = train, type = "response")
pred_prob
pred_yes = ifelse(pred_prob > 0.5,'Y','N')
table = table(pred_yes, train$Loan_Status) #confusion matrix
table
sum(diag(table))/sum(table) #Accurancy
rate = 1 - sum(diag(table))/sum(table)
rate
#misclassificaiton rate is 18%

#ROC
library(ROCR)
#install.packages("gplots")
pred = prediction(pred_prob, train$Loan_Status)
#true positive rate and faluse positive rate
roc = performance(pred, "tpr","fpr")
plot(roc, colorize = T)

#AUROC
#Error under ROC
auc = performance(pred, "auc")
auc = slot(auc, 'y.values')[[1]]
legend(0.6,0.3,round(auc,4),title="AUC",cex = 1)
#Normally, if it is greater or equals to 80%, it is a perfect model. 
#Area under ROC is 0.78, the performance of fit2 is ok

#Identify the best cutofff
# ACC (Accuracy): true Positive + True negative
eval <- performance(pred,"acc")
plot(eval)
max <- which.max(slot(eval,"y.values")[[1]])
acc <- max(slot(eval,"y.values")[[1]])
# Calculate the max in the ACC and choose out best cutoff
cutoff <- slot(eval,"x.values")[[1]][max]
# so the best cutoff is 0.727
abline(h = acc, v = cutoff) # add the cutoff line to the ACC plot


# Reset pred_yes with the cutoff '0.727'
pred_prob = predict(fit2, newdata = loan, type = "response")
pred_prob
pred_yes = ifelse(pred_prob > 0.727,'Y','N')
table = table(pred_yes, loan$Loan_Status) #confusion matrix
table
sum(diag(table))/sum(table) #Accurancy
rate = 1 - sum(diag(table))/sum(table)
rate

##Test prediction
test2 = subset(test, select = -c(X, Loan_ID))
pred_prob = predict(fit2, newdata = test2, type = 'response')
pred_prob
test$Loan_Status = ifelse(pred_prob > 0.5,'Y','N')
test$Loan_Status

output = test %>% select(X, Loan_ID, Loan_Status)
output

### Decision tree on Train data 
library(rpart)
library(rpart.plot)

#Build Model
train=read.csv("train.csv")
train$Loan_Status = ifelse(train$Loan_Status == 'Y',1,0)
train2 =subset(train, select = -c(X,Loan_ID))
#Grow a tree on train
par(mfrow=c(1,1))
fit3 <- rpart(Loan_Status ~., data = train2, method = 'class')
fit3
#plot the decision tree
rpart.plot(fit3)

#Check how the model is doing using train data
pred_prob2 = predict(fit3, train2, type = 'class')
pred_prob2
table = table(loan2$Loan_Status,pred_prob2) #cofusion metrix
table

1-sum(diag(table))/sum(table)
#the misclassification rate is about 19% which is near to logisitc regression
#so in this case we can either choose logisitc regression model or decision tree model'


#drop all insignificant attributes and rebuild model
keep = c("Credit_History", "Property_Area", "Loan_Status")
train3 = train[,names(train) %in% keep]
fit4 <- rpart(Loan_Status ~., data = train3, method = 'class')
summary(fit4)


#pruning the tree
plotcp(fit4)
#printcp(fit)
fit5 = prune.rpart(fit4, cp=0.062)
rpart.plot(fit5)
#Check how the model is doing using the train data
pred_prob3 = predict(fit5, train3, type = 'class')
table2 = table(train3$Loan_Status,pred_prob3)
table2

1-sum(diag(table2))/sum(table2)

#The misclassification rate is around 19% ,too . We can see the decision tree is such robust !



## Random Forest ##

# Fitting Random Forest Model 
Random_forest <- randomForest(Churn ~., data = training)
print(Random_forest) # we use default ntree = 500 and the overall error rate = 21.49 %
# The error rate is relatively low when predicting 'No', and the error rate is much higher when preidcting 'Yes'

# Confusion Matrix
predict_RF <- predict(Random_forest, testing)
caret::confusionMatrix(predict_RF, testing$Churn)
table4 <- table(Predicted = predict_RF, Actual = testing$Churn)
print(paste('Random Forest Accuracy', sum(diag(table4)/sum(table4))))
plot(Random_forest)
# We can tell that as the number trees increases, the OOB error rate decreases, and then becomes almost constant
# We are not able to continutious minimize OOB error rate after 100 trees, so ntree can determine as 100


#Tune Random Forest
tune <- tuneRF(training[,-18], training[,18],stepFactor = 0.5, plot = TRUE, ntreeTry = 100, trace = TRUE, improve = 0.05)
# OOB error rate is at the lowest when mtry is 2. Therefore, we choose mtry =2

# Fitting a new random forest model 
Random_forest_New <- randomForest(Churn ~., data = training, ntree = 100, mtry = 2, importance = TRUE, proximity = TRUE)
print(Random_forest_New)
# The oob error rate decrease drom 21.49% to 20.9%

# New random forest model predicitons and confuion matrix
predict_RF_new <- predict(Random_forest_New, testing)
table5 <- table(Predicted = predict_RF_new, Actual = testing$Churn)
print(paste('Random Forest Accuracy II', sum(diag(table5)/sum(table5))))
# Both Accuracy and Sensitivity increases

# We can use this new random forest model to detect feature imporatnce
varImpPlot(Random_forest_New, sort = TRUE, n.var = 10, main = "Top 10 Feature Importance")

# Summary 

# 1.From above, Logistic Regression, Decision Tree and Random Forest can be used for customer churn analysis for this dataset,
# After pruning model, random forest model can lead to best accuracy.
# 2.Through feature importance, we can tell that features such as tenure_group, Contract, Monthlycharges and InternetService appear to play a role in customer churn
# 3.There does not seem to be a relationship between gender, partner, dependents, PhoneService and churn 
# 4.Customers in a month-to-month contract, with PaperlessBilling and are within 12 months tenure, 
# are more likely to churn; On the other hand, customers with one or two year contract, with longer than 12 months tenure, 
# that are not using PaperlessBilling, are less likely to churn.