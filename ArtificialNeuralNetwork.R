set.seed(1234)
library(datasets)
library(nnet)
library(caret)
data("iris")
attach(iris)


 
indices <- sample(1:nrow(iris),0.75*nrow(iris), replace=FALSE)

Entire.Training.Data <- iris[indices,]
Entire.Training.Data


#normalization
scale.many <- function(dat, column.nos) {
  nms <- names(dat)
  for(col in column.nos) {
    name <- paste(nms[col],".z", sep = "")
    dat[name] <- scale(dat[,col])
  }
  cat(paste("Scaled ", length(column.nos), " variable(s)\n"))
  dat
}
newdata<-scale.many(Entire.Training.Data,c(1:4))
newdata
names(newdata)
newdat<-newdata[-c(1:4)]
newdat






Predictors.Training.Data <- iris[indices,c(1,2,3,4)]
summary(Entire.Training.Data)

#Fitting the nnet model with 2 hidden nodes iin 1 hidden layer 
nnet.model <- nnet(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data = newdata, 
  size = 1, rang = 0.1, decay = 5e-04, maxit = 200, trace = FALSE)
multinom(nnet.model)

### CONSIDER USING NEURALNETTOOLS FOR BETTER VISUALIZATIONS

summary(nnet.model)

################################################################################ 
#IN-SAMPLE STATISTICS
################################################################################
Classification.Training.Probabilities <-  nnet.model$fitted.values
Predicted.Training.Class <-  predict(nnet.model, Predictors.Training.Data, type = "class")
True.Training.Class <- iris[indices,5]
View(data.frame(Classification.Training.Probabilities,Predicted.Training.Class,True.Training.Class))
confusionMatrix(True.Training.Class,as.factor(Predicted.Training.Class))


################################################################################ 
#OUT-OF-SAMPLE STATISTICS
################################################################################
Predictors.Testing.Data <- iris[-indices,c(1,2,3,4)]
Predicted.Testing.Class <-  predict(nnet.model, newdata = Predictors.Testing.Data, type = "class")
True.Testing.Class <- iris[-indices,5]

View(data.frame(Predicted.Testing.Class,True.Testing.Class))
confusionMatrix(True.Testing.Class,as.factor(Predicted.Testing.Class))


##############################################################################
### CONSIDER USING NEURALNETTOOLS FOR BETTER VISUALIZATIONS
##############################################################################
 

library(NeuralNetTools)

 
#Variables Importance
# Y = Setosa
olden(nnet.model,out_var = "setosa",bar_plot = TRUE)
#Y=versicolor
olden(nnet.model,out_var = "versicolor",bar_plot = TRUE)

# Y = virginica
olden(nnet.model,out_var = "virginica",bar_plot = TRUE)
 
plotnet(nnet.model)
summary(nnet.model)

#Computing the AIC for a particular number of hidden nodes
multinom(nnet.model)

x<-c(1:5)
y<-c(1:200)
Result <- data.frame(matrix(nrow = 4, ncol = 2))
colnames(Result) <- c("Size", "AIC")
for(i in x) {
  for (j in y) {
    
  
  nnet.model <- nnet(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data = newdata, 
                     size = i, rang = 0.1, decay = 5e-04, maxit = j, trace = FALSE)
  sum.model <- multinom(nnet.model)
  Result[i,1] <- i
  Result[i,2] <- sum.model$AIC
  print(Result[i,2])
  }
}
Result

sample(12,replace = FALSE)

