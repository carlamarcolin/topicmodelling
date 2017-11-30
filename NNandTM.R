#Code developed for paper in submission
####Neural Network Classifier#######
##Classification taks
#Adapted from: http://tjo-en.hatenablog.com/entry/2016/03/30/233848
#Adapted from: https://mxnet.incubator.apache.org/tutorials/r/fiveMinutesNeuralNetwork.html 
#Installation:
cran <- getOption("repos")
cran["dmlc"] <- "https://s3-us-west-2.amazonaws.com/apache-mxnet/R/CRAN/"
options(repos = cran)
install.packages("mxnet",dependencies = T)
require(mlbench)
require(mxnet)
require(tm)
require(lsa)
require(dplyr)

#Words removed from all corpus:
x <- stopwords("en")
x <- c(x, "more")

##For categories with few comments, mix with some machine annotated comments
#First, select the main words (with Topic Modeling) from each of the categories
#For that, corpus with only the comments that belongs to the category
categorie <- yourData
corpus <- Corpus(VectorSource(categorie$Comm))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removeWords, x)
TDMNew <- TermDocumentMatrix(corpus)
LSANew <- lsa(TDMNew,dimcalc_share())
#Order them to select top-n (function ordered.LSA available in root)
ordLSA <- ordered.lsa(LSANew$tk)
#(opcional)Visualization of the process
#()View(ordLSA[1:20,1:20])
#()findFreqTerms(TDMNew,lowfreq =20)
#DataFrame with top-n words from top-(n*2) topics
ft1 <- ordLSA[1:10,1:20]
#Just collumns with words (odd columns)
ft1<- ft1[,c(1,3,5,7,9,11,13,15,17,19)]
#Create a chr vector
ft1 <- c(ft1$`2`,ft1$`3`,ft1$`4`,ft1$`5`,ft1$`5`,ft1$`6`,ft1$`8`,ft1$`9`,ft1$`10`)
#Remove repeated words
ft1 <- unique(ft1)

#Now grab all unlabeled documents and run again to make a DTM with them
#HERE RUN AGAIN line 26 to 32 with categorie <- yourDataUnlabeled
TDMNewM <- as.matrix(TDMNew)
#Only comments that contains at least a % of this ft1 words
##Otherwise it would bring so many confusing things!
#Take those words because they don't appear in the new database
remove <- c("christmas","")
ft1 <- ft1 [! ft1 %in% remove]
Docs <- TDMNewM[ft1,]
#Make it a logical matrix so that the amount of each word won't affect
Docs <- sapply(as.data.frame(Docs), as.logical)
#Multiply by one so that you have 0's and 1's
Docs <- Docs * 1
#Choose only those columns where sum > n (at least n from the m words in the same document)
Docs <- which(colSums(Docs)>=6)
#Use this index to choose only the correspondent documents with those words
Docs <- NotClass[Docs,]

#Insert category columm (all 1) information
Docs$Cat <- as.numeric(1)
#Reorder to rbind
Docs <- Docs[c(2,1)]
#Put together with the existent CatnClass (0's and 1's comment)
#Select the data to work
categorie <- yourData
categorie <- rbind(CatnClass,Docs)
#mx.mlp requires the following parameters:
  #Training data and label
  #Number of hidden nodes in each hidden layer
  #Number of nodes in the output layer
  #Type of the activation
  #Type of the output loss
  #The device to train (GPU or CPU)
  #Other parameters for mx.model.FeedForward.create
mx.set.seed(0)
#Create a vector to keep track of differences in accuracy during the training process
accuracy <- as.vector(1:10)
##Neural Network Feed-Foward Classifier!
for (i in 1:10){
  #Randomize the data every time
  set.seed(12*i)
  categorieRnd = categorie[sample(nrow(categorie)),]
  #Build a Corpus
  corpus <- Corpus(VectorSource(categorieRnd$Comm))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, tolower)
  corpus <- tm_map(corpus, removeWords, x)
  #Build Document-Term Matrix Train and Test
  DTM <- DocumentTermMatrix(corpus)
  #Optional: Remove Sparse Terms
  #DTM <- removeSparseTerms(DTMC1,0.998)
  limTrain <- round(nrow(categorieRnd)*0.75)
  DTMTrain <- DTM[1:limTrain,] 
  DTMTrain <- as.matrix(DTMTrain)
  DTMTest <- DTM[(limTrain+1):nrow(categorieRnd),]
  DTMTest <- as.matrix(DTMTest)
  #Build labels Train and Test
  CatTrain <- categorieRnd$Cat[1:limTrain]
  CatTest <- categorieRnd$Cat[(limTrain+1):nrow(categorieRnd)]
  CatTrain <- as.numeric(CatTrain)
  CatTest <- as.numeric(CatTest)
  
  #Multi-layer Perceptron, train with Train data:
  model <- mx.mlp(DTMTrain, CatTrain, hidden_node=15, out_node=4, out_activation="softmax",
                  num.round=20, array.batch.size=15, learning.rate=0.07, momentum=0.9,
                  eval.metric=mx.metric.accuracy)
  
  #Predict Test data
  preds = predict(model, DTMTest)
  pred.label = max.col(t(preds))-1
  #Table with rights and weongs in Test Data
  table <- table(pred.label, CatTest)
  #Keep track of accuracy
  accuracy[i] <- (table[1,1]+table[2,2])/(sum(table))
}

plot(accuracy,type="b", ylim=c(0,1),xlab="",ylab="Accuracy")

#########Can you predict new data?
categorie <- yourUnlabeledData
#Since that in Docs we used some of this data, we have to remove now
#Otherwise it would be duplicated
#anti_join() return all rows from x where there are not matching values in y, keeping just x columns
test <- anti_join(yourUnlabeledData,categorie)
categorie <- test

corpus <- Corpus(VectorSource(categorie$Comm))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removeWords, x)
#Build DTM just to compare the vocabulary
NDTM <- DocumentTermMatrix(corpus)
#Demonstrate that the vocabulary is similar, so we can classify based on this classifier:
ft2 <- findFreqTerms(DTM)
ft1 <- findFreqTerms(NDTM)
#Common terms
common.c1c2 <- data.frame(term = character(0), freq = integer(0))
for(t in ft1){
  find <- agrep(t, ft2)
  if(length(find) != 0){
    common.c1c2 <- rbind(common.c1c2, data.frame(term = t, freq = length(find)))
  }
}
#Difference among them
same <- common.c1c2[,1]
allNDTM <- NDTM$dimnames$Terms
outsiders <- setdiff(allNDTM,same)
#Frequency of this outside terms
outsidersFreq <- NDTM[,intersect(colnames(NDTM), outsiders)]
#Percentage:
sum(outsidersFreq)
outsidersPerc <- (sum(outsidersFreq$v)/sum(NDTM$v))*100

#Now build DTM with the same vocabulary as the previous data
NDTM <- DocumentTermMatrix(corpus, 
                           control=list(dictionary=DTM$dimnames$Terms))
NDTM <- as.matrix(NDTM)
#Run the model in the unlabeled data
predsNew <- predict(model,NDTM)
predsNew.label <- max.col(t(predsNew))-1
Xx <- cbind(as.character(predsNew.label),as.character(categorie$Comm))
#Build the dataframe
Xx <- as.data.frame(Xx)
#Stay only with the "1"
CatnNew <- Xx[which(Xx$V1=="1"),]
#Coerce names otherwise rbind won't work
names(CatnNew) <- names(CatnClass)
#Join with CatNClass (That was labeled by humans)
CatnNew <- rbind(CatnNew,CatnClass[which(CatnClass$Cat==1),])

##LSA with new data to uncover the topics
categorie <- yourData
corpus <- Corpus(VectorSource(categorie$Comm))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removeWords, x)
TDMNew <- TermDocumentMatrix(corpus)
LSANew <- lsa(TDMNew,dimcalc_share())
ordLSA <- ordered.lsa(LSANew$tk)
View(ordLSA)

#Now create some wordclouds based on main topics!
library(wordcloud)
#Frequency of the words
freq <- rowSums(as.matrix(TDMNew))
#Name in rows
freq <- as.data.frame(freq,rownames(as.matrix(TDMNew)))
#Select main words from topics
ftw <- ordLSA[1:10,1:22]
#Just columns with words (odd columns)
ftw<- ftw[,c(1,3,5,7,9,11,13,15,17,19,21)]
#Create a chr vector
ftw <- c(ftw$`2`,ftw$`3`,ftw$`4`,ftw$`5`,ftw$`5`,ftw$`6`,ftw$`8`,ftw$`9`,ftw$`10`,ftw$`11`)
#Remove repeated words and put in exclusive variables (per category)
ftwn <- unique(ftw)
freqn <- freq
#Plot Word cloud, min n words (here 15)
WCCatn <- wordcloud(ftwn,freqn[ftwn,],min.freq = 15, random.color = TRUE)

#LSAAll to visualize singular values
corpus <- Corpus(VectorSource(yourUnlabeledData$Comm))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removeWords, x)
TDM <- TermDocumentMatrix(corpus)
#Generates raw = all singular values; and share = 50%, and plot both to compare
LSAAll <- lsa(TDM, dimcalc_raw())
LSA50 <- lsa(TDM,dimcalc_share())
plot(LSAAll$sk, type="l",xlab="",ylab="",main="Singular Values",ylim=c(0,100))
par(new=TRUE)
plot(LSA50$sk, type="l",xlab="",ylab="",xaxt="n",yaxt="n",col="blue",
     xlim=c(0,1582),ylim=c(0,100),lwd=3)

######PRCOMP Visualization######
#My Data: Hotels in line and category in collumns
##First: weight each hotel by each category by main topics word frequency
#Initialize a data.frame to keep the values:
##strongAsFactors = FALSE, otherwise problems to inser Hotel Name
CatHotel <- data.frame(Hotel=as.character(),Cat1=as.numeric(), Cat2=as.numeric(),Cat3=as.numeric(),
                       Cat4=as.numeric(),Cat5=as.numeric(),stringsAsFactors = FALSE)
#First, only counting the words and normalizing by the total comments from that hotel
hotel <- "H1"
#Here import data frame with all hotels, nicknames to preserve hotel real name, and comments
hotelComm <- All2017[which(All2017$Nickname==hotel),]
#Corpus with that hotel words
categorie <- hotelComm
corpus <- Corpus(VectorSource(categorie$Comm))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removeWords, x)
TDMHotel <- TermDocumentMatrix(corpus)
#See frequency of each word for this hotel
freqHotel <- rowSums(as.matrix(TDMHotel))
#Name in rows
freqHotel <- as.data.frame(freqHotel,rownames(as.matrix(TDMHotel)))
#Sum  words for each category, dividing by the number of comments
##Do it for all categories
CatnHotel <- sum(freqHotel[ftwn,],na.rm=TRUE)/nrow(hotelComm)
#Make it a row in a data.frame
CatRow <- c(hotel,CatnHotel,CatmHotel,CatxHotel) #and how many more you have
CatHotel[nrow(CatHotel)+1,] <- CatRow
#Hotels in rownames
library(tidyverse)
CatHotel <- CatHotel %>% remove_rownames %>% column_to_rownames(var="Hotel")
CatHotel <- as.data.frame(lapply(CatHotel, as.numeric))
##PCA plot to see where are the hotels with the categories
biplot(prcomp(CatHotel,scale = TRUE))
