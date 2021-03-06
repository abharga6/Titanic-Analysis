# data wrangling
library(tidyverse)
library(forcats)
library(stringr)

#data imputation
library(mice)

# data assessment/visualizations
library(tm)
library(data.table)
library(pander)
library(ggplot2)
library(scales)
library(grid)
library(gridExtra)
library(corrplot)
library(VIM) 
library(knitr)
library(vcd)
library(caret)

# model
library(xgboost)
library(MLmetrics)
library('randomForest') 
library('rpart')
library('rpart.plot')
library('car')
library('e1071')

setwd("C:/Users/ichbi/Desktop/kaggle/Titanic")
train <- read_csv('train.csv')
str(train)
train <- train %>% setNames(tolower(names(.))) %>% mutate(survived=factor(survived, levels = c("0","1"), labels = c("0","1")),pclass=factor(pclass), sex=factor(sex),embarked=factor(embarked), set='train')
summary(train)
#percentage survived 
surv_pct <- train %>% count(survived) %>% mutate(pct=n/sum(n)) %>% setNames(c('survived','count','percentage')) %>% as.data.frame() %>% mutate(survived=ifelse(survived==0,'No','Yes'))

test <- read_csv('test.csv')
str(test)
test <- test %>% setNames(tolower(names(.))) %>% mutate(pclass=factor(pclass), sex=factor(sex),embarked=factor(embarked), set='test')
summary(test)
full <- bind_rows(train,test)
summary(full)
str(full)

#Missing value analysis
miss_values <- full %>% summarise_all(funs(count=sum(is.na(.)),percentage=sum(is.na(.))/n()))
miss_count <- miss_values[,1:12] %>% gather(variable, miss_count) 
miss_count %>% ggplot(aes(x=reorder(variable,-miss_count),y=miss_count)) + geom_bar(stat='identity',fill='red') + geom_text(aes(y=miss_count+30,label=miss_count))+coord_flip()
miss_percentage <- miss_values[,13:24] %>% gather(variable, miss_percentage)
miss_percentage %>% ggplot(aes(x=reorder(variable,-miss_percentage),y=miss_percentage)) + geom_bar(stat='identity',fill='red') +coord_flip()

#missing value imputation 
#embarkment : ovbious related variable is fare and pclass 
str(full %>% filter(is.na(embarked)))
embark_full <- full %>% filter(!is.na(embarked))
#1st class, females, 80 fair
ggplot(embark_full, aes(x=embarked,y=fare, fill=pclass))+geom_boxplot()+geom_hline(yintercept = 80)
full <- full %>% mutate(embarked=factor(ifelse(is.na(full$embarked),'C', as.character(embarked))))
                        
 #fare
str(full %>% filter(is.na(fare)))
#embarked = s, pclass=3, since only one value is missing lets plug with the median in this category
impte_value <- full %>% group_by(embarked, pclass) %>% summarise(mean_fare=median(fare,na.rm=T)) %>% filter(embarked=='S',pclass==3) %>% .$mean_fare
full <- full %>% mutate(fare=ifelse(is.na(full$fare),impte_value,fare))
                        
#age
#using mice imputation
 #creating dataset for mice imputation 
  mice_data <- full[,!names(full) %in% c('passengerid','name','ticket','cabin','survived')]
 set.seed(120)
 imputed_data <- mice(mice_data, method = 'rf')
imputed_data <- mice::complete(imputed_data)
                        
#comparing age distribution in original and imputed data 
 par_1 <- ggplot(full, aes(x=age)) + geom_histogram(fill='skyblue', binwidth = 2) + labs(title="Original data")
 par_2 <- ggplot(imputed_data, aes(x=age)) + geom_histogram(fill='darkgreen', binwidth = 2) + labs(title="imputed data")
grid.arrange(par_1, par_2, ncol=2)
                        
#finally replacing age variable in full data set 
full$age <- imputed_data$age

#Feature engineering
#Creating title
full$title <- gsub("^.*,[[:blank:]](.*?)\\..*$", "\\1", full$name)
table(full$title)
rare <- c('the Countess','Capt', 'Col', 'Don', 
            'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')
full$title[full$title == 'Mlle'] <- 'Miss' 
full$title[full$title == 'Ms']   <- 'Miss'
full$title[full$title == 'Mme']  <- 'Mrs' 
full$title[full$title == 'Lady'] <- 'Miss'
full$title[full$title == 'Dona'] <- 'Miss'
full$title[full$title %in% rare] <- "rare"
table(full$title)
full$title <- factor(full$title)

#family size 
full$familysize <- full$sibsp +full$parch +1
full$FamilySized[full$familysize == 1] <- 'Single' 
full$FamilySized[full$familysize < 5 & full$familysize >= 2] <- 'Small' 
full$FamilySized[full$familysize >= 5] <- 'Big' 
full$FamilySized=as.factor(full$FamilySized)

#age group
full <- full %>% mutate(agegroup=case_when(age<13 ~"children", (age >= 13 & age< 18) ~ "adolsents", (age >=18 & age<60) ~ "adults", age>=60 ~ "aged")) %>% mutate(agegroup=factor(agegroup))

#Ticket
full <- full %>% mutate(ticketgroup= tolower(ticket)) %>% mutate(ticketgroup=removePunctuation(ticketgroup)) %>% mutate(ticketgroup=gsub(" ","", ticketgroup)) %>% mutate(ticketgroup= case_when(grepl('^[0-9]', ticketgroup) & nchar(ticketgroup)==3 ~ "numeric3", grepl('^[0-9]', ticketgroup) & nchar(ticketgroup)==4 ~ "numeric4",grepl('^[0-9]', ticketgroup) & nchar(ticketgroup)==5 ~ "numeric5",grepl('^[0-9]', ticketgroup) & nchar(ticketgroup)==6 ~ "numeric6",grepl('^[0-9]', ticketgroup) & nchar(ticketgroup)==7 ~ "numeric7", grepl("^(a)", ticketgroup) ~ "a",grepl("^(c)", ticketgroup) ~ "c",grepl("^(f)", ticketgroup)|grepl("^(l)", ticketgroup) ~ "f",grepl("^(p)", ticketgroup)~ "p",grepl("^(s)", ticketgroup)~"s",grepl("^(w)", ticketgroup)~"w" ))%>% mutate(ticketgroup=factor(ticketgroup))


#exploratory analysis
#survival vs other relevant variables
#pclass
summary <- full %>% filter(set=='train') %>% group_by(pclass) %>% summarise(passenger=n(),survived=sum(as.numeric(as.character(survived))), survival_rate= round(sum(as.numeric(as.character(survived)))*100/n()))
kable(summary)
ggplot(full %>% mutate(survived=case_when(survived==0 ~ "No", survived==1 ~'yes'))  %>% filter(set=='train'), aes(x=pclass, fill=survived))+geom_bar(position = "fill")+scale_fill_brewer(palette="Set1")+ scale_y_continuous(labels = percent)+labs(x='Passenger class', y= "survival rate", title ="survival rate vs Passenger class")      #use RColorBrewer::display.brewer.all() to check pallet options
ggplot(full %>% mutate(survived=case_when(survived==0 ~ "No", survived==1 ~'yes'))  %>% filter(set=='train'), aes(x=pclass, fill=survived))+geom_bar()+scale_fill_brewer(palette="Set1")+labs(x='Passenger class', y= "Number of passenger", title ="Number of passenger vs Passenger class")      #use RColorBrewer::display.brewer.all() to check pallet options

#sex
summary <- full %>% filter(set=='train') %>% group_by(sex) %>% summarise(passenger=n(),survived=sum(as.numeric(as.character(survived))), survival_rate= round(sum(as.numeric(as.character(survived)))*100/n()))
kable(summary)
ggplot(full %>% mutate(survived=case_when(survived==0 ~ "No", survived==1 ~'yes'))  %>% filter(set=='train'), aes(x=sex, fill=survived))+geom_bar(position = "fill")+scale_fill_brewer(palette="Set1")+ scale_y_continuous(labels = percent)+labs(x='sex', y= "survival rate", title ="survival rate vs sex")      #use RColorBrewer::display.brewer.all() to check pallet options
ggplot(full %>% mutate(survived=case_when(survived==0 ~ "No", survived==1 ~'yes'))  %>% filter(set=='train'), aes(x=sex, fill=survived))+geom_bar()+scale_fill_brewer(palette="Set1")+labs(x='sex', y= "Number of passenger", title ="Number of passenger vs sex")      #use RColorBrewer::display.brewer.all() to check pallet options

#age 
ggplot(full %>% mutate(survived=case_when(survived==0 ~ "No", survived==1 ~'yes'))  %>% filter(set=='train'),aes(x=age, fill=survived))+geom_histogram(aes(y=..density..), alpha=0.5)+geom_density(alpha=0.2,aes(colour=survived))+scale_y_continuous(label=percent)+labs(x="age",y='density', title="age distribution")
ggplot(full %>% mutate(survived=case_when(survived==0 ~ "No", survived==1 ~'yes'))  %>% filter(set=='train'),aes(x=age, fill=survived))+geom_histogram(alpha=0.5)+labs(x="age",y='passengers', title="age distribution")

#agegroup
summary <- full %>% filter(set=='train') %>% group_by(agegroup) %>% summarise(passenger=n(),survived=sum(as.numeric(as.character(survived))), survival_rate= round(sum(as.numeric(as.character(survived)))*100/n()))
kable(summary)
ggplot(full %>% mutate(survived=case_when(survived==0 ~ "No", survived==1 ~'yes'))  %>% filter(set=='train'), aes(x=agegroup, fill=survived))+geom_bar(position = "fill")+scale_fill_brewer(palette="Set1")+ scale_y_continuous(labels = percent)+labs(x='age group', y= "survival rate", title ="survival rate vs age group")      #use RColorBrewer::display.brewer.all() to check pallet options
ggplot(full %>% mutate(survived=case_when(survived==0 ~ "No", survived==1 ~'yes'))  %>% filter(set=='train'), aes(x=agegroup, fill=survived))+geom_bar()+scale_fill_brewer(palette="Set1")+labs(x='age group', y= "Number of passenger", title ="Number of passenger vs age group")      #use RColorBrewer::display.brewer.all() to check pallet options


#sibsp
summary <- full %>% filter(set=='train') %>% group_by(sibsp) %>% summarise(passenger=n(),survived=sum(as.numeric(as.character(survived))), survival_rate= round(sum(as.numeric(as.character(survived)))*100/n()))
kable(summary)
ggplot(full %>% mutate(survived=case_when(survived==0 ~ "No", survived==1 ~'yes'))  %>% filter(set=='train'), aes(x=sibsp, fill=survived))+geom_bar(position = "fill")+scale_fill_brewer(palette="Set1")+ scale_y_continuous(labels = percent)+labs(x='number of siblings', y= "survival rate", title ="survival rate vs number of siblings")      #use RColorBrewer::display.brewer.all() to check pallet options
ggplot(full %>% mutate(survived=case_when(survived==0 ~ "No", survived==1 ~'yes'))  %>% filter(set=='train'), aes(x=sibsp, fill=survived))+geom_bar()+scale_fill_brewer(palette="Set1")+labs(x='number of siblings', y= "Number of passenger", title ="Number of passenger vs number of siblings")      #use RColorBrewer::display.brewer.all() to check pallet options

#parch
summary <- full %>% filter(set=='train') %>% group_by(parch) %>% summarise(passenger=n(),survived=sum(as.numeric(as.character(survived))), survival_rate= round(sum(as.numeric(as.character(survived)))*100/n()))
kable(summary)
ggplot(full %>% mutate(survived=case_when(survived==0 ~ "No", survived==1 ~'yes'))  %>% filter(set=='train'), aes(x=parch, fill=survived))+geom_bar(position = "fill")+scale_fill_brewer(palette="Set1")+ scale_y_continuous(labels = percent)+labs(x='number of parents', y= "survival rate", title ="survival rate vs number of parents")      #use RColorBrewer::display.brewer.all() to check pallet options
ggplot(full %>% mutate(survived=case_when(survived==0 ~ "No", survived==1 ~'yes'))  %>% filter(set=='train'), aes(x=parch, fill=survived))+geom_bar()+scale_fill_brewer(palette="Set1")+labs(x='number of parents', y= "Number of passenger", title ="Number of passenger vs number of parents")      #use RColorBrewer::display.brewer.all() to check pallet options

#fare
ggplot(full %>% mutate(survived=case_when(survived==0 ~ "No", survived==1 ~'yes'))  %>% filter(set=='train'),aes(x=fare, fill=survived))+geom_histogram(aes(y=..density..), alpha=0.5)+geom_density(alpha=0.2,aes(colour=survived))+scale_y_continuous(label=percent)+labs(x="fare",y='density', title="fare distribution")
ggplot(full %>% mutate(survived=case_when(survived==0 ~ "No", survived==1 ~'yes'))  %>% filter(set=='train'),aes(x=fare, fill=survived))+geom_histogram(alpha=0.5)+labs(x="fare",y='passengers', title="fare distribution")

#embarked
summary <- full %>% filter(set=='train') %>% group_by(embarked) %>% summarise(passenger=n(),survived=sum(as.numeric(as.character(survived))), survival_rate= round(sum(as.numeric(as.character(survived)))*100/n()))
kable(summary)
ggplot(full %>% mutate(survived=case_when(survived==0 ~ "No", survived==1 ~'yes'))  %>% filter(set=='train'), aes(x=embarked, fill=survived))+geom_bar(position = "fill")+scale_fill_brewer(palette="Set1")+ scale_y_continuous(labels = percent)+labs(x='embarkment', y= "survival rate", title ="survival rate vs embarkment")      #use RColorBrewer::display.brewer.all() to check pallet options
ggplot(full %>% mutate(survived=case_when(survived==0 ~ "No", survived==1 ~'yes'))  %>% filter(set=='train'), aes(x=embarked, fill=survived))+geom_bar()+scale_fill_brewer(palette="Set1")+labs(x='embarkment', y= "Number of passenger", title ="Number of passenger vs embarkment")      #use RColorBrewer::display.brewer.all() to check pallet options

#title
summary <- full %>% filter(set=='train') %>% group_by(title) %>% summarise(passenger=n(),survived=sum(as.numeric(as.character(survived))), survival_rate= round(sum(as.numeric(as.character(survived)))*100/n()))
kable(summary)
ggplot(full %>% mutate(survived=case_when(survived==0 ~ "No", survived==1 ~'yes'))  %>% filter(set=='train'), aes(x=title, fill=survived))+geom_bar(position = "fill")+scale_fill_brewer(palette="Set1")+ scale_y_continuous(labels = percent)+labs(x='title', y= "survival rate", title ="survival rate vs title")      #use RColorBrewer::display.brewer.all() to check pallet options
ggplot(full %>% mutate(survived=case_when(survived==0 ~ "No", survived==1 ~'yes'))  %>% filter(set=='train'), aes(x=title, fill=survived))+geom_bar()+scale_fill_brewer(palette="Set1")+labs(x='title', y= "Number of passenger", title ="Number of passenger vs title")      #use RColorBrewer::display.brewer.all() to check pallet options

#family size
summary <- full %>% filter(set=='train') %>% group_by(FamilySized) %>% summarise(passenger=n(),survived=sum(as.numeric(as.character(survived))), survival_rate= round(sum(as.numeric(as.character(survived)))*100/n()))
kable(summary)
ggplot(full %>% mutate(survived=case_when(survived==0 ~ "No", survived==1 ~'yes'))  %>% filter(set=='train'), aes(x=FamilySized, fill=survived))+geom_bar(position = "fill")+scale_fill_brewer(palette="Set1")+ scale_y_continuous(labels = percent)+labs(x='family size', y= "survival rate", title ="survival rate vs family size")      #use RColorBrewer::display.brewer.all() to check pallet options
ggplot(full %>% mutate(survived=case_when(survived==0 ~ "No", survived==1 ~'yes'))  %>% filter(set=='train'), aes(x=FamilySized, fill=survived))+geom_bar()+scale_fill_brewer(palette="Set1")+labs(x='family size', y= "Number of passenger", title ="Number of passenger vs family size")      #use RColorBrewer::display.brewer.all() to check pallet options

#Ticket Group
summary <- full %>% filter(set=='train') %>% group_by(ticketgroup) %>% summarise(passenger=n(),survived=sum(as.numeric(as.character(survived))), survival_rate= round(sum(as.numeric(as.character(survived)))*100/n()))
kable(summary)
ggplot(full %>% mutate(survived=case_when(survived==0 ~ "No", survived==1 ~'yes'))  %>% filter(set=='train'), aes(x=ticketgroup, fill=survived))+geom_bar(position = "fill")+scale_fill_brewer(palette="Set1")+ scale_y_continuous(labels = percent)+labs(x='ticket group', y= "survival rate", title ="survival rate vs ticket group")      #use RColorBrewer::display.brewer.all() to check pallet options
ggplot(full %>% mutate(survived=case_when(survived==0 ~ "No", survived==1 ~'yes'))  %>% filter(set=='train'), aes(x=ticketgroup, fill=survived))+geom_bar()+scale_fill_brewer(palette="Set1")+labs(x='ticket group', y= "Number of passenger", title ="Number of passenger vs ticket group")      #use RColorBrewer::display.brewer.all() to check pallet options


#feature correlation
coor_tbl <- full %>% filter(set=="train")%>%select(-passengerid,-name,-ticket,-cabin,-set) %>% mutate_all(as.numeric)%>%cor(use="complete.obs")%>%corrplot(type="lower", diag=FALSE)

#prep for prediction 
set.seed(120)
train_dev <- full %>% filter(set=="train") %>% select(survived,pclass,sex,agegroup,ticketgroup,FamilySized,title,fare, embarked)
data_partition <- createDataPartition(train_dev$survived, p=0.8, list=F)
train_final <- train_dev[data_partition,]
dev_final <- train_dev[-data_partition,]

#test set for final prediction
test_final <- full %>% filter(set=="test") %>% select(survived,pclass,sex,agegroup,ticketgroup,FamilySized,title,fare, embarked)


#decision tree
model_dt <- rpart(survived~., data=train_final,method='class')
rpart.plot(model_dt,extra =  3)
predict_train <- predict(model_dt,data=train_final,type = "class")
confusionMatrix(predict_train,train_final$survived)
predict_dev <- predict(model_dt, newdata = dev_final, type = "class")
confusionMatrix(predict_dev,dev_final$survived)

#crossvalidated decision tree
set.seed(120)
cv.10 <- createMultiFolds(train_final$survived,k=10,times=10)
ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 10, index=cv.10)
train_final <- as.data.frame(train_final)
set.seed(120)
model_cdt <- train(x=train_final[,-1],y=train_final[,1], method="rpart", trControl= ctrl)
rpart.plot(model_cdt$finalModel,extra =  3)
predict2_train <- predict(model_cdt$finalModel, data=train_final, type="class")
confusionMatrix(predict2_train,train_final$survived)
predict2_dev <- predict(model_cdt$finalModel, newdata=dev_final, type="class")
confusionMatrix(predict2_dev,dev_final$survived)


#Logistic regression
model_logit <- glm(survived~.,data = train_final, family = binomial)
predict_logit_train <- predict(model_logit, data=train_final, type='response')
table(train_final$survived, predict_logit_train>0.5)
accurcy <- (389+206)/(389+206+51+68)
accurcy
predict_logit_dev <- predict(model_logit, newdata=dev_final, type='response')
table(dev_final$survived, predict_logit_dev>0.5)
accurcy <- (95+51)/(95+51+17+14)
accurcy


#Random Forest
model_rf <- randomForest(x=train_final[,-1],y=train_final[,1], mtry = 3, ntree = 1000, importance=T)
model_rf
predict_train_rf <- predict(model_rf,data=train_final,type = "class")
confusionMatrix(predict_train_rf,train_final$survived)

predict_dev_rf <- predict(model_rf, newdata = dev_final, type = "class")
confusionMatrix(predict_dev_rf,dev_final$survived)

importance(model_rf)
varImpPlot(model_rf)

#cross validated RF
set.seed(120)
cv.10 <- createMultiFolds(train_final$survived,k=10,times=10)
ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 10, index=cv.10)
train_final <- as.data.frame(train_final)
set.seed(120)
model_crf <- train(x=train_final[,-1],y=train_final[,1], method="rf", trControl= ctrl, ntree=1000, importance=T)
model_crf
predict_train_crf <- predict(model_crf,data=train_final)
confusionMatrix(predict_train_crf,train_final$survived)

predict_dev_crf <- predict(model_crf, newdata = dev_final)
confusionMatrix(predict_dev_crf,dev_final$survived)

var_imp <- varImp(model_crf, scale=F)
var_imp
Plot(var_imp)


#final prediction on test 
prediction_test <- predict(model_cdt, newdata = test_final)


