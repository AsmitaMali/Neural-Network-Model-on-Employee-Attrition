## Let us first set the working directory path
getwd()
##HR_Employee_Attrition_Data.csv

hrData <- read.table("HR_Employee_Attrition_Data - Copy.csv", sep = ",", header = T)

summary(hrData)
str(hrData)

#####Below variables needs to be converted to factors
#Education,EmployeeCount,EnvironmentSatisfaction,JobInvolvement,JobLevel,JobSatisfaction,
#RelationshipSatisfaction,
#StockOptionLevel,WorkLifeBalance

#Conversion of int to factors
hrData$Education <- as.factor(hrData$Education)
hrData$EmployeeCount <- as.factor(hrData$EmployeeCount)
hrData$EnvironmentSatisfaction <- as.factor(hrData$EnvironmentSatisfaction)
hrData$JobInvolvement <- as.factor(hrData$JobInvolvement)
hrData$JobLevel <- as.factor(hrData$JobLevel)
hrData$JobSatisfaction <- as.factor(hrData$JobSatisfaction)
hrData$RelationshipSatisfaction <- as.factor(hrData$RelationshipSatisfaction)
hrData$StockOptionLevel <- as.factor(hrData$StockOptionLevel)
hrData$WorkLifeBalance <- as.factor(hrData$WorkLifeBalance)


##Factors having single value
hrData$EmployeeCount = NULL
hrData$Over18= NULL

### Removing least significant variables using ANOVA by looking at highest p value
hrData$EmployeeNumber = NULL              

##backward elimination
hrData$Department = NULL
hrData$Education = NULL 
hrData$PerformanceRating = NULL 
hrData$PercentSalaryHike = NULL 
hrData$MonthlyRate=NULL
hrData$HourlyRate=NULL
hrData$MaritalStatus=NULL
hrData$StandardHours=NULL

modeldata <- glm(formula = Attrition ~.,data=hrData, family = "binomial")

Anova(modeldata)


library("factoextra")
library("psych")

matrixdata <- model.matrix(~., data = hrData)
modifiedData<- as.data.frame(matrixdata)[-1]

#Scree plot
fviz_eig(pca)


######## PCA and FA ########
modifiedData<- as.data.frame(matrixdata)[-1]
cortest.bartlett(modifiedData[-2])
#Since P value is 0 whch is low we can go for PCA and factor analysis
pca <- princomp(modifiedData[-2])
summary(pca)
#One componene itselfexplains 99% of the variation hence we will go with one factor
pca$loadings

#Factor analysis
fa <- principal(modifiedData[-2],1,rotate = 'varimax')
fa
####Below variables cotributes heavily in the component that we selected for nalysis
#Age(60%),YearsAtCompany(81%),YearsInCurrentRole(67%),YearsSinceLastPromotion(61%),MonthlyIncome(87%),
#NumCompaniesWorked(11%),JobLevel3(28%),JobLevel4(51%),JobLevel5(50%)
fact_val=fa$scores 
fact_val=data.frame(fact_val)
#Adding dependant Y variable from dataset
fact_val<- cbind(modifiedData[2],fact_val)


library(caTools)
#Split training and test data set as 70:30
split = sample.split(fact_val$AttritionYes, SplitRatio = 0.70)

# Create training and testing sets
fact_val.train = subset(fact_val, split == TRUE)
dim(fact_val.train)
fact_val.test = subset(fact_val, split == FALSE)
dim(fact_val.test)
library(neuralnet)
set.seed(1)
nn1 <- neuralnet(formula = AttritionYes ~ . , 
                 data = fact_val.train, 
                 hidden = c(2),
                 #err.fct = "sse",
                 linear.output = FALSE,
                 lifesign = "full",
                 lifesign.step = 10,
                 threshold = 0.01,
                 stepmax = 20000,
                 #startweights = startweightsObj ## Comment this as well if load is commented
)

plot (nn1)
net.results <- compute(nn1, fact_val.test)
hist(net.results$net.result)
hist(fact_val.test$AttritionYes)
discreteValues<-ifelse(net.results$net.result>0.5,1,0)
cleanoutput <- cbind ( fact_val.test$AttritionYes,  
                       as.data.frame(discreteValues) )
colnames(cleanoutput) <- c("Expected Output", "Neural Net Output" )
#View(cleanoutput)
t2<- table(fact_val.test$AttritionYes,discreteValues)
t2
(t2[1]+t2[4])/(nrow(fact_val.test))




#####################################CART################################################
library(rpart)
table(fact_val$AttritionYes)

library(rpart.plot)
Model.CART = rpart(AttritionYes ~ ., data=fact_val.train, method="class")
summary(Model.CART)
#Plot the model
prp(Model.CART)

#Predict the test data
predictionCart <- predict(Model.CART, newdata=fact_val.test, type="class")

#CART Accuracy
#Confusion matrix 
t1 <- table(fact_val.test$Attrition, predictionCart)
t1
#CART model accuracy
(t1[1]+t1[4])/(nrow(fact_val.test))


