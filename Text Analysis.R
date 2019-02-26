#Assignment 4#
#Sentiment Analysis on Tweets based on Autonomous vehicle#
#Programmer: Jane Dsouza#

#Question 1#
#Setting the current working directory to the current file locations#
setwd("C:/CSV path")

#Loading the training data set#
mydata<- read.csv("online.csv", header = TRUE)

#Loading the tm package#
library("tm")

#Considering only the text portion of the dataset#
mydata <- paste(mydata$text, collapse=" ")

#Converting the data to a Vector source#
train.source <- VectorSource(mydata)

#Creating a corpus object#
corpus.train <- Corpus(train.source) 

#Performing the data preprocessing on the corpus#
corpus.train <- tm_map(corpus.train, tolower) 
corpus.train <- tm_map(corpus.train, removePunctuation)
corpus.train <- tm_map(corpus.train, removeNumbers)
corpus.train <- tm_map(corpus.train, removeWords, stopwords("english"))
corpus.train <- tm_map(corpus.train, stemDocument)

#Convverting the corpurs to a Document Term Matrix#
dtm.train <- DocumentTermMatrix(corpus.train)
dtm.train.1 <- as.matrix(dtm.train)

#Identifying the frequency#
frequency <- colSums(dtm.train.1)
frequency <- sort(frequency, decreasing =TRUE)[1:200]

#Creating a wordcloud of the top 5 frequent words#
#install.packages("wordcloud")
library(wordcloud)
words <- names(frequency)
wordcloud(words,frequency)


#Question2#
library(RTextTools)
trace("create_matrix", edit=T)
train.data <- read.csv("online.csv", header = TRUE)
test.data <- read.csv("STUFF.csv", header = TRUE)
class(x.text)
head(train.data)
head(test.data)
nrow(train.data)
nrow(test.data)
dtm.train <- create_matrix(train.data$X.text,removeNumbers=TRUE, removePunctuation =TRUE, removeStopwords =TRUE, stemWords = TRUE, toLower = TRUE,ngramLength = 5)
train.container <- create_container(dtm.train, train.data$X.sentiment,
                              trainSize=1:7126,virgin=FALSE)
model <- train_model(train.container,"SVM",kernel="linear",cost=1)


predictionData <-test.data$X.text
predSize <- length(predictionData)
dtm.test <- create_matrix(predictionData, originalMatrix=dtm.train)

predictionContainer <- create_container(dtm.test,
                                        test.data$X.sentiment., testSize=1:predSize, virgin=FALSE)
results <- classify_model(predictionContainer, model)
df <- data.frame(predictionData,test.data$X.sentiment,results)
write.csv(df,file = "afterstemming_svm2.csv")

library(caret)
xtab <- table(test.data$X.sentiment,  results$SVM_LABEL)
confusionMatrix(xtab)

#*****************************************************************************************#
library(sentimentr)

test1 <- read.csv("syuzhet.csv", header=TRUE)
class(test1$text)
test1$text <- as.character(test1$text)
#sentences <- get_sentences(test1$text)
head(sentences)

#Using afinn method#
afinn_vector <- get_sentiment(test1$text, method="afinn")
plot(afinn_vector, type="l", main="Plot Trajectory", xlab = "Narrative Time", ylab= "Emotional Valence")

ft_values <- get_transformed_values(afinn_vector, low_pass_size=3, x_reverse_len=100, scale_vals=TRUE, scale_range=FALSE)

plot(ft_values, type ="h", main ="Transformed data", xlab = "narrative time", ylab = "Emotional Valence", col = "red")

#Using bing
bing_vector <- get_sentiment(test1$text, method="bing")

#Using nrc
nrc_vector <- get_sentiment(test1$text, method="nrc")

#Using syuzhet
syuzhet_vector <- get_sentiment(test1$text, method="syuzhet")
plot(
  syuzhet_vector, 
  type="h", 
  main="Example Plot Trajectory", 
  xlab = "Narrative Time", 
  ylab= "Emotional Valence"
)

percent_vals <- get_percentage_values(syuzhet_vector, bins = 40)
plot(
  percent_vals, 
  type="l", 
  main="Opinions related to Self-Driving Cars", 
  xlab = "Count", 
  ylab= "Emotional Valence", 
  col="red"
)

#Transformed values
ft_values <- get_transformed_values(
  syuzhet_vector, 
  low_pass_size = 3, 
  x_reverse_len = 100,
  padding_factor = 2,
  scale_vals = TRUE,
  scale_range = FALSE
)
plot(
  ft_values, 
  type ="l", 
  main ="Opinions related to Self-Driving Cars", 
  xlab = "Narrative Time", 
  ylab = "Emotional Valence", 
  col = "red"
)


dct_values <- get_dct_transform(
  syuzhet_vector, 
  low_pass_size = 5, 
  x_reverse_len = 100,
  scale_vals = F,
  scale_range = T
)
plot(
  dct_values, 
  type ="l", 
  main ="Joyce's Portrait using Transformed Values", 
  xlab = "Narrative Time", 
  ylab = "Emotional Valence", 
  col = "red"
)

simple_plot(syuzhet_vector)


nrc_data <- get_nrc_sentiment(test1$text)

#Angry items
angry_items <- which(nrc_data$anger > 0)
test1$text[angry_items]

#Joyful items
joy_items <- which(nrc_data$joy > 0)
test1$text[joy_items]

pander::pandoc.table(nrc_data[, 1:8], split.table = Inf)

barplot(
  sort(colSums(prop.table(nrc_data[, 1:8]))), 
  horiz = TRUE, 
  cex.names = 0.7, 
  las = 1, 
  main = "Emotions in Sample text", xlab="Percentage"
)


#########################################################################
test1 <- read.csv("syuzhet.csv", header=TRUE)
library(ggmap)
map <- get_map(location = 'United States', zoom = 4)
mapPoints <- ggmap(map) + geom_point(aes(x = lon, y = lat,color = test1$sentiment,size = test1$sentiment), data = test1, alpha = .5)

testplot <- read.csv("syuzhet.csv",header=TRUE)




ggmap(map) + geom_point(data = test1, aes(x = Long,y = Lat,color = Freq,size = Freq))+ scale_color_gradient(low="Yellow",high="red")
