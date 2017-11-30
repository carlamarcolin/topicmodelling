###############SVM Classification################
library(RTextTools)

#Take off zeros
#SentTest[is.na(SentTest)] <- 0
#SentTest$`1` <- as.factor(SentTest$`1`)

#Set dimension in analysis
original <- Cat1Class
original <- AllClass

##Create Document Term Matrix
DTM <- create_matrix(original$Comm, language="english", removeNumbers=TRUE, 
                     removePunctuation = TRUE, removeStopwords = TRUE, toLower = TRUE,
                     stemWords=TRUE, removeSparseTerms=.998)

freqs <- rowSums(as.matrix(DTM))
lower <- which(freqs>1)
Alower <- DTM[-lower,]
DTM <- DTM[lower,]
freqs <- rowSums(as.matrix(DTM))
upper <- which(freqs<ncol(DTM)/4)
DTM <- DTM[,upper]
empty <- as.integer(which(colSums(as.matrix(DTM))<14))
DTM <- DTM[,-empty]
DTM <- as.matrix(DTM)

#Parameter tunning:
tune_out <- tune.svm(x = container@training_matrix,
                     y = container@training_codes,
                     gamma = 10^(-10:10),
                     cost = 10^(-1:1)
)

##Create container object that prepares to train data in different algorithms
#Total Size = rows from DTM, i.e., the amount of documents
#virgin = false, we dont have virgin docs yet

#Object to store analytics values
analytics <- array(1:10,dim=c(10,6))
ensemble <- array(1:10,dim=c(10,4))
alg <- c('SVM','TREE')

for (i in 1:10){
  #Set seed to reproduce same example
  set.seed(100*i) 
  #Cross-fold validation
  mod <- original[sample(nrow(original)),]
  #mod <- mod[lower,]
  #Train size
  limTrain <- round(nrow(mod)*0.75)
  #Create object to train
  container <- create_container(DTM, mod$Cat, trainSize=1:limTrain, 
                                testSize = (limTrain+1):nrow(mod), virgin=FALSE)
  
  #Train SVM
  model <- train_models(container, alg, kernel="radial", cost=0.1, gamma = 1e-06)
  #Test
  multi_classify <- classify_models(container, model)
  #Get analytics and store in analytics array
  multi_analytics <- create_analytics(container,multi_classify)
  #[,1] = precision; [,2] = recall; [,3] = fscore
  analytics[i,] <- summary(multi_analytics)
  ensemble[i,]<- multi_analytics@ensemble_summary
  
}

###Test with new data
#New Unlabeled Data and Cleaning
virgin <- read_csv("your_path")

DTM_new <- create_matrix(virgin$Comm, language="english", removeNumbers=TRUE, 
                         removePunctuation = TRUE, removeStopwords = TRUE, toLower = TRUE,
                         stemWords=TRUE, removeSparseTerms=.998)

#trace("create_matrix",edit=T) Change line 42 to "acronym" instead of "Acronym" -- original package error fix
#Continer object with the unlabeled and labeled data
container_New1 <- create_container(DTM_new, Cat1Class$Cat, trainSize=NULL, testSize = 1:1581, 
                              virgin=TRUE)

#Choose algorithms to compare
alg <- c('SVM','MAXENT')
#Create model object with trainning data
model_multi <- train_models(container, alg)
#Perform classification
Multi_classify <- classify_models(container,model_multi)
#Analytis
Multi_Analytics <- create_analytics(container,Multi_classify)
summary(Multi_Analytics)
#Here we can compare both classifiers testd
Multi_Analytics_New@document_summary

#####Other SVM option from e1071 that allows to test with new data###############
##1. Data
#Set DataSet we are working
categorie <- yourData

#Randomize
set.seed(155)
categorieRnd = categorie[sample(nrow(categorie)),]

#Treat the corpus
x <- stopwords("en")
x <- c(x, "more")
corpus <- Corpus(VectorSource(categorie$Comm))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removeWords, x)

#DTM Categorie 1
DTMC1 <- DocumentTermMatrix(corpus)
DTMC1 <- removeSparseTerms(DTMC1,0.998)
DTMC1Train <- DTMC1[1:780,] 
DTMC1Train <- as.matrix(DTMC1Train)
DTMC1Test <- DTMC1[781:1045,]
DTMC1Test <- as.matrix(DTMC1Test)

#Use LSA as feature selection to improve SVM results.
LSAC1 <- lsa(DTMC1, dimcalc_share())
#Multiply back to use as the new DTM
DTMLSA <- as.textmatrix(LSAC1)
DTMLSATrain <- DTMLSA[1:780,] 
DTMLSATrain <- as.matrix(DTMLSATrain)
DTMLSATest <- DTMLSA[781:1045,]
DTMLSATest <- as.matrix(DTMLSATest)

categorieRnd$Cat <- as.factor(categorieRnd$Cat)
CatTrain <- categorieRnd[1:780,]
CatTest <- categorieRnd[781:1045,]

#For unbalanced categories, set weights
weights <- c("0"=0.7,"1"=0.7)
#Train Model
model <- svm(DTMC1Train,CatTrain$Cat, kernel="radial",cost = 0.1,type = "C-classification", gamma = 1e-06,
             scale = FALSE, probability = TRUE)
             #class.weights = weights, 
#Train with LSA
model <- svm(DTMLSATrain,CatTrain$Cat, kernel="radial",cost = 0.001,type = "C-classification", gamma = 1e-06, 
             probability = TRUE)
             #class.weights = weights)
#Test Model
predTest <- predict(model,DTMC1Test)
#Test Model with LSA
predTest <- predict(model,DTMLSATest)
#Model Performance
acc <- table(predTest, CatTest$Cat)
#Combine results
Xx <- cbind(as.character(predTest),as.character(CatTest$Cat),as.character(CatTest$Comm))

################Naive Bayes Classifier##################
library(e1071)
#create 75:25 partitions of the dataframe and document term matrix
##The label collum has to be factor otherwise naiveBayes won't get it!
#SentTestT <- yourData
#SentTestT$`1` <- as.factor(SentTestT$`1`)
#SentTest_train <- SentTestT[1:90,]
#SentTest_test <- SentTestT[91:98,]

DTMSent <- create_matrix(Cat1ClassMod$Comm, language="english", removeNumbers=TRUE, 
                         removePunctuation = TRUE, removeStopwords = TRUE, toLower = TRUE,
                         stemWords=TRUE, removeSparseTerms=.998, weighting = weightTfIdf)

#trainSize=1:753, testSize = 754:805
Cat1ClassMod$Cat <- as.factor(Cat1ClassMod$Cat)
Cat1ClassMod_train <- Cat1ClassMod[1:753,]
Cat1ClassMod_test <- Cat1ClassMod[754:805,]
mod <- Cat3Class
mod$Class <- as.factor(mod$Class)
modTrain <- mod[1:670,]
modTest <- mod[671:894,]

DTMSent_train <- DTMSent[1:753,]
DTMSent_train <- as.matrix(DTMSent_train)
DTMSent_test <- DTMSent[754:805,]
DTMSent_test <- as.matrix(DTMSent_test)

#trainSize=1:670, testSize = 671:894
DTMTrain <- as.matrix(DTM[1:670,])
DTMTest <- as.matrix(DTM[671:894,])
dim(DTMTrain)
dim(DTMSent_train)
#Second number = Amount of features (combination of possibilities)
#You have to define for each of the features how would be a condition True [DTP Training]
#For improve, can try to decrease the number of features (vocabulary)

##First parameter has to be a Matrix
system.time(classifier <- naiveBayes(DTMSent_train, Cat1ClassMod_train$Cat, laplace = 1))
system.time(pred <- predict(classifier, newdata=DTMSent_test) )

classifier <- naiveBayes(DTMTrain,modTrain$Class,laplace=1)
pred <- predict(classifier,newdata = DTMTest)

table("Predictions"= pred,  "Actual" = Cat1ClassMod_test$Cat)
library(caret)
#Confusion Matrix
conf_mat <- confusionMatrix(pred, modTest$Class)
conf_mat$byClass
conf_mat$overall
conf_mat$overall['Accuracy']

##Since the predictor variables here are all continuous, the Naïve Bayes classifier 
#generates three Gaussian (Normal) distributions for each predictor variable (each word)
classifier$tables$bad
#mean (first column) and standard deviation (second column) for each class
#also, how to access these four values
classifier$tables$good[1:4]

#Plotting word curves #shower, breakfast and bed (mean and SD pick up from previous line with the desired word
plot(function(x) dnorm(x, 0.17948718, 0.4514185), 0, 3, col="red", 
     main="Words distribution among 'Tangibles' Dimension", ylab="", xlab="")
curve(dnorm(x, 0.35897436, 0.7775528), add=TRUE, col="blue")
curve(dnorm(x, 0.4615385, 0.6002698), add=TRUE, col="green")
