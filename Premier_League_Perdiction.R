install.packages("jsonlite", repos="https://cran.rstudio.com/")
library("jsonlite")

json_file <- 'https://datahub.io/sports-data/english-premier-league/datapackage.json'
json_data <- fromJSON(paste(readLines(json_file), collapse=""))
View(json_data)

print(json_data$resources$name)

# print all tabular data(if exists any)
for(i in 1:length(json_data$resources$datahub$type)){
  if(json_data$resources$datahub$type[i]=='derived/csv'){
    path_to_file = json_data$resources$path[i]
    data <- read.csv(url(path_to_file))
    print(data)
  }
}

View(json_data)


path <- "C:/Users/Ankur/Desktop/EPL"
merge_file_name <- "C:/Users/Ankur/Desktop/EPL/EPL1.csv"

filenames <- list.files(path= path, full.names=TRUE)

All <- lapply(filenames,function(filename){
  print(paste("Merging",filename,sep = " "))
  read.csv(filename)
})

EPL <- do.call(rbind.data.frame, All)

write.csv(df,merge_file_name)
##########################################################
View(EPL)
EPL <- read.csv("C:/Users/Ankur/Desktop/EPL.csv")
names(EPL)
EPL <- EPL[ -c(1:6,11,27:65) ]

head(EPL)

names(EPL)

install.packages("caret")
install.packages("Boruta")
install.packages("numDeriv")

library(caret)
library(Boruta)

spliting <- createDataPartition(EPL$FTR, p=.70, list=FALSE)
TrainData <- EPL[ spliting,]
TestData <- EPL[-spliting,]

set.seed(123)

#Feature Selection
data.train <- Boruta(FTR~.,data=EPL,doTrace=2)
print(data.train)
plot(data.train,xlab="",xaxt="n")

lz<-lapply(1:ncol(data.train$ImpHistory),function(i)
  data.train$ImpHistory[is.finite(data.train$ImpHistory[,i]),i])

names(lz) <- colnames(data.train$ImpHistory)

Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(data.train$ImpHistory), cex.axis = 0.7
)
###################################################################

#install.packages("mlbench")
#install.packages("caret")

set.seed(234)

library(caret)
library(mlbench)

control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results <- rfe(EPL[,2:16], EPL[,1], sizes=c(1:8), rfeControl=control)
# summarize the results
print(results)

predictors(results)

plot(results, type=c("g", "o"))

#install.packages("rpart")

library(rpart)

fit <- rpart(FTR ~ HTR+HST+HTAG+HTHG+AST,
             method="class", data=EPL)

printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits

# plot tree 
plot(fit, uniform=TRUE, 
     main="Classification Tree for FTR")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

install.packages("dplyr")
install.packages("Hmisc")

library(dplyr)
library(Hmisc)

head(EPL)
cor_1 <- round(cor(EPL), 2)
cor_1

cor_2 <- rcorr(as.matrix(EPL))
cor_2

###################################################################
library(caret)
split <- createDataPartition(y=EPL$FTR, p=0.7, list=FALSE)
train <- EPL[split,]
test <- EPL[-split,]

library(tree)

treeMod <- tree(FTR ~ HS+AS+HC+AC, data = train)  # model the tree, including all the variables

plot(treeMod)  # Plot the tree model

text(treeMod, pretty=0) 

out <- predict(treeMod) 


library(ggplot2)
library(plot)
plot(EPL$HC, type = 'p')

install.packages("party")

library(party)

ctree.control=ctree_control(teststat = c("quad", "max"), testtype = "Teststatistic", mincriterion =
                               0.95, minsplit = 20L, minbucket = 7L)

ctree.model <- ctree(FTR~ HTR+HST+HTHG+HTAG+AST+HS+AS, data = train, controls = ctree.control)

plot(ctree.model, uniform=TRUE,
     main= "Classification Tree")

ctreepredictions <- predict(ctree.model, test)

confusionMatrix(ctreepredictions, test$FTR)


#Naive Bayes
library(e1071)
train_control_nb <- trainControl(method="repeatedcv", number=10, repeats=10)
train_nb = naiveBayes(FTR~ HTR+HST+HTHG+HTAG+AST+HS+AS, data= train, trControl = train_control_nb)

train_nb 
test_pred_nb <- predict(train_nb, test)
confusionMatrix(test_pred_nb, test$FTR)


library(randomForest)
library(MAss)

dim(EPL)

train<-sample(1:nrow(EPL),300)
??EPL

EPL.rf<-randomForest(FTR~ HTR+HST+HTHG+HTAG+AST+HS+AS, data = EPL , subset = train)
EPL.rf

plot(EPL.rf)
