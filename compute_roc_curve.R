library(ggplot2)
library(reshape)

data <- read.csv("output.csv")
data$output <- 0 + (data[,1]=="Yes")

points <- 100 # number of data points we want on the ROC curve
thresholds <- seq(0,1,1/points)
classifiers <- c(2,3,4,5) # column indices where classifier predictions are stored

P <- sum(data$output) # total number of positive instances
N <- -sum(data$output-1) # total number of negative instances

TPR <- data.frame(matrix(0,points,length(classifiers)))
FPR <- data.frame(matrix(0,points,length(classifiers)))
for(i in 1:length(thresholds)) {  
  for(j in 1:length(classifiers)) {
    # True Positive = Predicted is 1 AND Actual is 1
    TP <- sum((data[,classifiers[j]] >= thresholds[i]) & data$output)
    
    # False Positive = Predicted is 1 AND Actual is 0
    FP <- sum((data[,classifiers[j]] >= thresholds[i]) & !data$output)
  
    TPR[i,j] <- TP/P
    FPR[i,j] <- FP/N
  }
}

# add baseline representing random predictions
TPR <- cbind(TPR,1-thresholds)
FPR <- cbind(FPR,1-thresholds)

# combine the two data frames into one
result <- data.frame(melt(FPR),melt(TPR))

# plot the ROC curves
ggplot(data=result,aes(x=value,y=value.1,colour=variable)) + 
  geom_line() + 
  opts(title="ROC") + 
  xlab("False Positive Rate (FPR)") + 
  ylab("True Positive Rate (TPR)")
