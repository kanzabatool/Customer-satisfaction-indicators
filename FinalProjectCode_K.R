library(readxl)
coffeeall_Perceptions <- read_excel("DATA_SCIENCE/DataMining/project/coffeeall Perceptions.xlsx")
View(coffeeall_Perceptions)
attach(coffeeall_Perceptions)

coffeeall_Perceptions <- na.omit(coffeeall_Perceptions)

#Part 1  
#a)	Please use principal component analysis, with the varimax rotation procedure to determine the service quality variables as input variables.

#Generate a newdata of only service variables
servicedata <- coffeeall_Perceptions[c(8:40)]

#check available variables
colnames(servicedata)

#check variable class
str(servicedata)

#PCA
library(psych)
servicedata.pca <- prcomp(servicedata, scale. = T)
summary(servicedata.pca)
plot(servicedata.pca)
#dim(servicedata.pca$x)
#outputs the mean of variables
servicedata.pca$center

#outputs the standard deviation of variables
servicedata.pca$scale

#rotatio
servicedata.pca$rotation

biplot(servicedata.pca, scale = 0)

#compute standard deviation of each principal component
std_dev <- servicedata.pca$sdev
std_dev

#compute variance
pr_var <- std_dev^2
pr_var

#proportion of variance explained
prop_varex <- pr_var/sum(pr_var)
prop_varex[1:20]

#scree plot
plot(prop_varex, xlab = "Principal Component",
    ylab = "Proportion of Variance Explained",
    type = "b")

#cumulative scree plot
plot(cumsum(prop_varex), xlab = "Principal Component",
    ylab = "Cumulative Proportion of Variance Explained",
    type = "b")

#PART 2

#coffeeall_Perceptions <- na.omit(coffeeall_Perceptions) #Mean doesnt work if there are missing values as there are in sat4
coffeeall_Perceptions$Mean <- rowMeans(coffeeall_Perceptions[41:44]) #Mean of sat variable rows
mean(coffeeall_Perceptions$Mean) #Mean is 3.84

#Converting into binary , 0 if less than mean and 1 if more than mean
coffeeall_Perceptions$binsat <- 1
coffeeall_Perceptions$binsat[coffeeall_Perceptions$Mean<=mean(coffeeall_Perceptions$Mean)]<-0

#PART C



#add a data with principal components
data <- data.frame(SatisfactionVariable = coffeeall_Perceptions$binsat, servicedata.pca$x)

# Partition Data
set.seed(111)
ind <- sample(2, nrow(data),
              replace = TRUE,
              prob = c(0.8, 0.2))
training <- data[ind==1,]
testing <- data[ind==2,]


#we are interested in first 30 PCAs
#training <- training[,1:31]

#select the first 30 components
#testing <- testing[c(2:34)]
#testing$SatVaraible <- 1

#Training Logistic Regression
Fit_LG <- glm(SatisfactionVariable ~PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+PC11+PC12+PC13+PC14+PC15+PC16+PC17+PC18+PC19+PC20, data = training, family = 'binomial')
summary(Fit_LG)

#Predictions on Logistic Regresssion and accuracy check
LG.prediction <- predict(Fit_LG, training, type = "response")
LG.prediction <- ifelse(LG.prediction > 0.5, 1,0)

tab1 <- table(predicted=LG.prediction, Actual= training$SatisfactionVariable)
tab1

LG.prediction2 <- predict(Fit_LG, testing, type = "response")
LG.prediction2 <- ifelse(LG.prediction2 > 0.5, 1,0)
tab2 <- table(predicted=LG.prediction2, Actual= testing$SatisfactionVariable)
tab2

#Misclassification error
Misc1asserror = 1 - sum(diag(tab2))/sum(tab2) #28.3% misclassification error is very high
Misc1asserror
#accuracy of Logistic Regression
Accuracy = 1 - Misc1asserror
Accuracy #Only 71.6% ... very low




#Training SVM
library(e1071)
Fit_SVM <- svm(SatisfactionVariable ~PC1+PC2+PC5+PC7+PC8+PC9+PC10+PC11+PC12+PC14+PC15+PC18
               +PC19+PC20+PC22+PC24+PC26+PC28+PC29+PC30+PC31+PC33,
               data = training, kernel = "radial", type = "C-classification")
summary(Fit_SVM)

#Prediction on traing set
SVM.prediction <- predict(Fit_SVM, training)

#Confusion matrix on training set
tab1 <- table(predicted=SVM.prediction, Actual= training$SatisfactionVariable)
tab1

#Prediction on testing set
SVM.prediction <- predict(Fit_SVM, testing)

#Confusion matrix on testing set
tab2 <- table(predicted=SVM.prediction, Actual= testing$SatisfactionVariable)
tab2

#Misclassification error
Misc1asserror = 1 - sum(diag(tab2))/sum(tab2) 
Misc1asserror #23.05%

#accuracy of SVM
Accuracy = 1 - Misc1asserror
Accuracy  #76.9%


#Min-Max Normalization for neural network
#scaled.dat <- scale(training)


#Training Neural Network
library(neuralnet)
Fit_NN <- neuralnet(SatisfactionVariable ~PC1+PC2+PC7+PC8+PC9+PC12+PC15+PC18
                    +PC19+PC20+PC22+PC24+PC26+PC28+PC29+PC33+PC16,
                    data = training, hidden = 1,err.fct = "sse",
                    linear.output = T)
summary(Fit_NN)
plot(Fit_NN)

#Prediction on Training set
train <- training[c("PC1","PC2","PC7","PC8","PC9","PC12","PC15","PC18",
                    "PC19","PC20","PC22","PC24","PC26","PC28","PC29","PC33","PC16")]
NN.prediction <- compute(Fit_NN, train)
p1 <-NN.prediction$net.result
pred1 <- ifelse(p1>0.5,1,0)

#Confusion Matrix of Training set
tab1 <- table(predicted=pred1, Actual= training$SatisfactionVariable)

#Prediction on Training set
test <- testing[c("PC1","PC2","PC7","PC8","PC9","PC12","PC15","PC18",
                  "PC19","PC20","PC22","PC24","PC26","PC28","PC29","PC33","PC16")]
NN.prediction <- compute(Fit_NN, test)
p2 <-NN.prediction$net.result
pred2 <- ifelse(p2>0.5,1,0)

#Confusion Matrix of Training set
tab2 <- table(predicted=pred2, Actual= testing$SatisfactionVariable)

#Misclassification error
Misc1asserror = 1 - sum(diag(tab2))/sum(tab2) #24.3%

#Accuracy of NN
Accuracy = 1 - Misc1asserror
Accuracy # 75.6%  









