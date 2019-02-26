setwd("C:/CSV Path")

#Loading the training data set#
delaware <- read.csv("delaware.csv", header=TRUE)
text <- delaware$x
text<- gsub("@\\w+", " ", text) #Remove username mentions in tweets - they start with @ symbol.
text <- gsub("http.+ |http.+$", " ", text)
text <- gsub("rt", " ", text) 
text <- gsub("RT", " ", text)#Remove RT so that duplicates are easy to identify
text <- unique(text)
write.csv(text,'delaware_1.csv')
delaware1 <- read.csv('delaware_1.csv',header=TRUE)

#Emoticons
emoticons <- read.csv("emoticon.csv", header = F)

names(emoticons) <- c("unicode", "bytes","description")
emoji.frequency <- matrix(NA, nrow = nrow(delaware1), ncol = nrow(emoticons))

for(i in 1:nrow(emoticons))
{
  
  
  emoji.frequency[,i] <- regexpr(emoticons$bytes[i],delaware1$x, useBytes = T )
  
}

emoji.per.tweet <- rowSums(emoji.frequency > -1)

emoji.indexes <- which( emoji.per.tweet > 0) 
emoji.ds <- NULL

for(i in emoji.indexes){
  
  valid.cols <- which(emoji.frequency[i,]>-1)
  
  for(j in valid.cols){
    
    emoji.ds <- rbind(cbind(delaware1[i,], emoticons[j,]), emoji.ds)
    
  }
  
}

write.csv(emoji.ds,'delawareemoticons.csv')

delaware1 <- read.csv('delaware_1.csv',header=TRUE)
text<-delaware1$x
text <- gsub("[[:punct:]]", " ", text)
text <- gsub("^ ", "", text) #Leading blanks
text <- gsub(" $", "", text) #Trailing blanks
text <- gsub(" +", " ", text)
text <- iconv(text, to = "ASCII", sub = " ")
text <- gsub("^ ", "", text) #Leading blanks
text <- gsub(" $", "", text) #Trailing blanks
text <- gsub(" +", " ", text)
text <- unique(text)


write.csv(text,'delaware_2.csv')
