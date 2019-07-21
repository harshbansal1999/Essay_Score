#Data PreProcessing

# Importing the dataset
library(readxl)
train=read_excel("training_set_rel3.xlsx")
test=read.delim("test_set.tsv",quote = '', stringsAsFactors = FALSE)
train_temp=train[,c(1,2,3)]
test_temp=test[,c(1,2,3)]

data=rbind(train_temp,test_temp)



# Cleaning the texts
library(tm)
library(SnowballC)
corpus = VCorpus(VectorSource(data$essay))
corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, removeNumbers)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords())
corpus = tm_map(corpus, stemDocument)
corpus = tm_map(corpus, stripWhitespace)


# Creating the Bag of Words model
dtm = DocumentTermMatrix(corpus)
dtm = removeSparseTerms(dtm, 0.999)
dataset = as.data.frame(as.matrix(dtm))



test_final=dataset[test$essay_id,]
train_final=dataset[train$essay_id,]


train_final_rater1=train_final
train_final_rater2=train_final
train_final_rater1$rater1=train$rater1_domain1
train_final_rater2$rater2=train$rater2_domain1
train_final_rater1$id=train$essay_id
train_final_rater1$rater1=ifelse(is.na(train_final_rater1$rater1),
                                 0,train_final_rater1$rater1)
train_final_rater2$id=train$essay_id
train_final_rater2$rater2=ifelse(is.na(train_final_rater2$rater2),0,train_final_rater2$rater2)
test_final$id=test$essay_id
train_final_rater1=train_final_rater1[complete.cases(train_final_rater1),]
train_final_rater2=train_final_rater2[complete.cases(train_final_rater2),]
test_final=test_final[complete.cases(test_final),]



# Fitting KNN Classification to the Training set
library(class)

y_pred_rater1 = knn(train = train_final_rater1[, -3489],
             test = test_final,
             cl = train_final_rater1[, 3489],
             k = 3,
             prob = TRUE)


y_pred_rater2 = knn(train = train_final_rater2[, -3489],
             test = test_final,
             cl = train_final_rater2[, 3489],
             k = 3,
             prob = TRUE)



#Writng of data
write.csv(y_pred_rater1,"rater1_pred.csv")
write.csv(y_pred_rater2,"rater2_pred.csv")
rater1_pred=read.csv("rater1_pred.csv")
rater1_pred$X=test_final$id
colnames(rater1_pred)=c("id","Rater1_Score")
rater2_pred=read.csv("rater2_pred.csv")
rater2_pred$X=test_final$id
colnames(rater2_pred)=c("id","Rater2_Score")




#Final Data
write.csv(rater1_pred,"rater1_pred_final.csv")
write.csv(rater2_pred,"rater2_pred_final.csv")