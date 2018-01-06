train <- read_csv('train.csv')
str(train)
train <- train %>% setNames(tolower(names(.))) %>% mutate(survived=factor(survived),pclass=factor(pclass), sex=factor(sex),embarked=factor(embarked))
summary(train)
#percentage survived 
surv_pct <- train %>% count(survived) %>% mutate(pct=n/sum(n)) %>% setNames(c('survived','count','percentage')) %>% as.data.frame() %>% mutate(survived=ifelse(survived==0,'No','Yes'))

test <- read_csv('test.csv')
str(test)
test <- test %>% setNames(tolower(names(.))) %>% mutate(pclass=factor(pclass), sex=factor(sex),embarked=factor(embarked))
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
full <- full %>% mutate(embarked=factor(ifelse(is.na(full$embarked),'C', as.character(embarked)))

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
imputed_data <- complete(imputed_data)

#comparing age distribution in original and imputed data 
par_1 <- ggplot(full, aes(x=age)) + geom_histogram(fill='skyblue', binwidth = 2) + labs(title="Original data")
par_2 <- ggplot(imputed_data, aes(x=age)) + geom_histogram(fill='darkgreen', binwidth = 2) + labs(title="imputed data")
grid.arrange(par_1, par_2, ncol=2)

#finally replacing age variable in full data set 
full$age <- imputed_data$age



