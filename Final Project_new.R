# Import data
install.packages("readxl")
library(readxl)
Turnover <- readxl::read_xlsx(path = "Turnover data.xlsx")

# Drop some unnecessary variables
t(t(names(Turnover)))
Turnover_cleaned <- Turnover[ -c(1,2,3,4,6,11,12,14,15,37) ]
t(t(names(Turnover_cleaned)))
# Statistics summary

summary(Turnover_cleaned)

# What proportion of employees in this sample categorized as "Vol" or left the job

mean(Turnover_cleaned$`Term Vol`)

# correlation matrix for Turnover data
round(cor(Turnover_cleaned), 2)
cor(Turnover_cleaned, use = "complete.obs")

#install package containing heatmap
install.packages("gplots")

#correlation matrix with heatmap
library(gplots)
round(cor(Turnover_cleaned), 2)
res <- cor(Turnover_cleaned, use = "complete.obs")
colfunc <- colorRampPalette(c("green", "white", "red"))
heatmap.2(res, Rowv = FALSE, Colv = FALSE, dendrogram = "none",col = colfunc(15),
          cellnote = round(cor(Turnover_cleaned), 2),
          notecol = "black", key = FALSE, trace = 'none', margins = c(10,10))

## Bar plot showing frequency of Stay/Quit employees
table(Turnover$Predictor)
barplot(table(Turnover$Predictor), ylab='Frequency', main="Barplot showing frequency of Stay/Quit employees")

## Box plot showing distribution of 'Quit/Stay' employees based off different predictors
boxplot(Turnover$`Count of Position Moves` ~ Turnover$Predictor, xlab = "Predictor", ylab = "Count of Position Moves", main = "Box plot showing distribution of 'Quit and Stay' employees based off Count of position moves")

boxplot(Turnover$`Count of job changes` ~ Turnover$Predictor, xlab = "Predictor", ylab = "Count of job changes", main = "Box plot showing distribution of 'Quit and Stay' employees based off Count of job changes")

boxplot(Turnover$`Count mgr chg only` ~ Turnover$Predictor, xlab = "Predictor", ylab = "Count of manager changes only", main = "Box plot showing distribution of 'Quit and Stay' employees based off Count of manager changes only")

boxplot(Turnover$`count of area changes` ~ Turnover$Predictor, xlab = "Predictor", ylab = "Count of area changes", main = "Box plot showing distribution of 'Quit and Stay' employees based off Count of area changes")

## k-NN and Discriminant Analysis######################################################################

# Considering an employee with this information:Level=6, First Shift Yes=0, Second Shift=1, Third Shift=0, Other Shift=0, 
# Job Mgmt=0, Job Prof=0, Job Supervisor=1, Job Director=0, Job Executive=0, Sector 1=0, Sector 2=0, Sector 3=0, Sector 4=1, Sector 5=0,
# Count of Position Moves=2, Region 1=0, Region 2=0, Region 3=0, Region 4=0, Region 5=0, Region 6=0, Region 7=1, Region 8=0, Region 9=0, Region 10-0, 
# Count of mgr changes=2, Count of job changes=0, Count of area changes=1, College=0
install.packages("dplyr")
library(dplyr)
new.employee <- data.frame(Level = 6, FirstShiftYes = 0, SecondShift = 1, ThirdShift = 0, OtherShift = 0, 
                           JobMgmt = 0, JobProf = 0, JobSupervisor = 1, JobDirector = 0, JobExecutive = 0, Sector1 = 0, Sector2 = 0, Sector3 = 0, Sector4 = 1, Sector5 = 0,
                           CountofPositionMoves = 2, Region1 = 0, Region2 = 0, Region3 = 0, Region4 = 0, Region5 = 0, Region6 = 0, Region7 = 1, Region8 = 0, Region9 = 0, Region10 = 0, 
                           Countofmgrchangesonly = 2, Countofjobchanges = 0, Countofareachanges = 1, College = 0)

# Normalizing the data 
turnover.norm <- Turnover_cleaned
new.employee.norm <- new.employee
t(t(names(turnover.norm))) 

# Standardize numerical predictors to 0-1 scale

new.employee.norm$Level <- (new.employee.norm$Level - min(turnover.norm$Level)) / (max(turnover.norm$Level) - min(turnover.norm$Level))
new.employee.norm$FirstShiftYes <- (new.employee.norm$FirstShiftYes - min(turnover.norm$`First Shift Yes`)) / (max(turnover.norm$`First Shift Yes`) - min(turnover.norm$`First Shift Yes`))
new.employee.norm$SecondShift <- (new.employee.norm$SecondShift - min(turnover.norm$`Second Shift`)) / (max(turnover.norm$`Second Shift`) - min(turnover.norm$`Second Shift`))
new.employee.norm$ThirdShift <- (new.employee.norm$ThirdShift - min(turnover.norm$`Third Shift`)) / (max(turnover.norm$`Third Shift`) - min(turnover.norm$`Third Shift`))
new.employee.norm$OtherShift <- (new.employee.norm$OtherShift - min(turnover.norm$`Other Shift`)) / (max(turnover.norm$`Other Shift`) - min(turnover.norm$`Other Shift`))
new.employee.norm$JobMgmt <- (new.employee.norm$JobMgmt - min(turnover.norm$`Job Mgmt`)) / (max(turnover.norm$`Job Mgmt`) - min(turnover.norm$`Job Mgmt`))
new.employee.norm$JobProf <- (new.employee.norm$JobProf - min(turnover.norm$`Job Prof`)) / (max(turnover.norm$`Job Prof`) - min(turnover.norm$`Job Prof`))
new.employee.norm$JobSupervisor <- (new.employee.norm$JobSupervisor - min(turnover.norm$`Job Supervisor`)) / (max(turnover.norm$`Job Supervisor`) - min(turnover.norm$`Job Supervisor`))
new.employee.norm$JobDirector <- (new.employee.norm$JobDirector - min(turnover.norm$`Job Director`)) / (max(turnover.norm$`Job Director`) - min(turnover.norm$`Job Director`))
new.employee.norm$JobExecutive <- (new.employee.norm$JobExecutive - min(turnover.norm$`Job Executive`)) / (max(turnover.norm$`Job Executive`) - min(turnover.norm$`Job Executive`))
new.employee.norm$Sector1 <- (new.employee.norm$Sector1 - min(turnover.norm$`Sector 1`)) / (max(turnover.norm$`Sector 1`) - min(turnover.norm$`Sector 1`))
new.employee.norm$Sector2 <- (new.employee.norm$Sector2 - min(turnover.norm$`Sector 2`)) / (max(turnover.norm$`Sector 2`) - min(turnover.norm$`Sector 2`))
new.employee.norm$Sector3 <- (new.employee.norm$Sector3 - min(turnover.norm$`Sector 3`)) / (max(turnover.norm$`Sector 3`) - min(turnover.norm$`Sector 3`))
new.employee.norm$Sector4 <- (new.employee.norm$Sector4 - min(turnover.norm$`Sector 4`)) / (max(turnover.norm$`Sector 4`) - min(turnover.norm$`Sector 4`))
new.employee.norm$Sector5 <- (new.employee.norm$Sector5 - min(turnover.norm$`Sector 5`)) / (max(turnover.norm$`Sector 5`) - min(turnover.norm$`Sector 5`))
new.employee.norm$CountofPositionMoves <- (new.employee.norm$CountofPositionMoves - min(turnover.norm$`Count of Position Moves`)) / (max(turnover.norm$`Count of Position Moves`) - min(turnover.norm$`Count of Position Moves`))
new.employee.norm$Region1 <- (new.employee.norm$Region1 - min(turnover.norm$`Region 1`)) / (max(turnover.norm$`Region 1`) - min(turnover.norm$`Region 1`))
new.employee.norm$Region2 <- (new.employee.norm$Region2 - min(turnover.norm$`Region 2`)) / (max(turnover.norm$`Region 2`) - min(turnover.norm$`Region 2`))
new.employee.norm$Region3 <- (new.employee.norm$Region3 - min(turnover.norm$`Region 3`)) / (max(turnover.norm$`Region 3`) - min(turnover.norm$`Region 3`))
new.employee.norm$Region4 <- (new.employee.norm$Region4 - min(turnover.norm$`Region 4`)) / (max(turnover.norm$`Region 4`) - min(turnover.norm$`Region 4`))
new.employee.norm$Region5 <- (new.employee.norm$Region5 - min(turnover.norm$`Region 5`)) / (max(turnover.norm$`Region 5`) - min(turnover.norm$`Region 5`))
new.employee.norm$Region6 <- (new.employee.norm$Region6 - min(turnover.norm$`Region 6`)) / (max(turnover.norm$`Region 6`) - min(turnover.norm$`Region 6`))
new.employee.norm$Region7 <- (new.employee.norm$Region7 - min(turnover.norm$`Region 7`)) / (max(turnover.norm$`Region 7`) - min(turnover.norm$`Region 7`))
new.employee.norm$Region8 <- (new.employee.norm$Region8 - min(turnover.norm$`Region 8`)) / (max(turnover.norm$`Region 8`) - min(turnover.norm$`Region 8`))
new.employee.norm$Region9 <- (new.employee.norm$Region9 - min(turnover.norm$`Region 9`)) / (max(turnover.norm$`Region 9`) - min(turnover.norm$`Region 9`))
new.employee.norm$Region10 <- (new.employee.norm$Region10 - min(turnover.norm$`Region 10`)) / (max(turnover.norm$`Region 10`) - min(turnover.norm$`Region 10`))
new.employee.norm$Countofmgrchangesonly <- (new.employee.norm$Countofmgrchangesonly - min(turnover.norm$`Count mgr chg only`)) / (max(turnover.norm$`Count mgr chg only`) - min(turnover.norm$`Count mgr chg only`))
new.employee.norm$Countofjobchanges <- (new.employee.norm$Countofjobchanges - min(turnover.norm$`Count of job changes`)) / (max(turnover.norm$`Count of job changes`) - min(turnover.norm$`Count of job changes`))
new.employee.norm$Countofareachanges <- (new.employee.norm$Countofareachanges - min(turnover.norm$`count of area changes`)) / (max(turnover.norm$`count of area changes`) - min(turnover.norm$`count of area changes`))
new.employee.norm$College <- (new.employee.norm$College - min(turnover.norm$College)) / (max(turnover.norm$College) - min(turnover.norm$College))


turnover.norm$Level <- (turnover.norm$Level - min(turnover.norm$Level)) / (max(turnover.norm$Level) - min(turnover.norm$Level))
turnover.norm$`First Shift Yes` <- (turnover.norm$`First Shift Yes` - min(turnover.norm$`First Shift Yes`)) / (max(turnover.norm$`First Shift Yes`) - min(turnover.norm$`First Shift Yes`))
turnover.norm$`Second Shift` <- (turnover.norm$`Second Shift` - min(turnover.norm$`Second Shift`)) / (max(turnover.norm$`Second Shift`) - min(turnover.norm$`Second Shift`))
turnover.norm$`Third Shift` <- (turnover.norm$`Third Shift` - min(turnover.norm$`Third Shift`)) / (max(turnover.norm$`Third Shift`) - min(turnover.norm$`Third Shift`))
turnover.norm$`Other Shift` <- (turnover.norm$`Other Shift` - min(turnover.norm$`Other Shift`)) / (max(turnover.norm$`Other Shift`) - min(turnover.norm$`Other Shift`))
turnover.norm$`Job Mgmt` <- (turnover.norm$`Job Mgmt` - min(turnover.norm$`Job Mgmt`)) / (max(turnover.norm$`Job Mgmt`) - min(turnover.norm$`Job Mgmt`))
turnover.norm$`Job Prof` <- (turnover.norm$`Job Prof` - min(turnover.norm$`Job Prof`)) / (max(turnover.norm$`Job Prof`) - min(turnover.norm$`Job Prof`))
turnover.norm$`Job Supervisor` <- (turnover.norm$`Job Supervisor` - min(turnover.norm$`Job Supervisor`)) / (max(turnover.norm$`Job Supervisor`) - min(turnover.norm$`Job Supervisor`))
turnover.norm$`Job Director` <- (turnover.norm$`Job Director` - min(turnover.norm$`Job Director`)) / (max(turnover.norm$`Job Director`) - min(turnover.norm$`Job Director`))
turnover.norm$`Job Executive` <- (turnover.norm$`Job Executive` - min(turnover.norm$`Job Executive`)) / (max(turnover.norm$`Job Executive`) - min(turnover.norm$`Job Executive`))
turnover.norm$`Sector 1` <- (turnover.norm$`Sector 1` - min(turnover.norm$`Sector 1`)) / (max(turnover.norm$`Sector 1`) - min(turnover.norm$`Sector 1`))
turnover.norm$`Sector 2` <- (turnover.norm$`Sector 2` - min(turnover.norm$`Sector 2`)) / (max(turnover.norm$`Sector 2`) - min(turnover.norm$`Sector 2`))
turnover.norm$`Sector 3` <- (turnover.norm$`Sector 3` - min(turnover.norm$`Sector 3`)) / (max(turnover.norm$`Sector 3`) - min(turnover.norm$`Sector 3`))
turnover.norm$`Sector 4` <- (turnover.norm$`Sector 4` - min(turnover.norm$`Sector 4`)) / (max(turnover.norm$`Sector 4`) - min(turnover.norm$`Sector 4`))
turnover.norm$`Sector 5` <- (turnover.norm$`Sector 5` - min(turnover.norm$`Sector 5`)) / (max(turnover.norm$`Sector 5`) - min(turnover.norm$`Sector 5`))
turnover.norm$`Count of Position Moves` <- (turnover.norm$`Count of Position Moves` - min(turnover.norm$`Count of Position Moves`)) / (max(turnover.norm$`Count of Position Moves`) - min(turnover.norm$`Count of Position Moves`))
turnover.norm$`Region 1` <- (turnover.norm$`Region 1` - min(turnover.norm$`Region 1`)) / (max(turnover.norm$`Region 1`) - min(turnover.norm$`Region 1`))
turnover.norm$`Region 2` <- (turnover.norm$`Region 2` - min(turnover.norm$`Region 2`)) / (max(turnover.norm$`Region 2`) - min(turnover.norm$`Region 2`))
turnover.norm$`Region 3` <- (turnover.norm$`Region 3` - min(turnover.norm$`Region 3`)) / (max(turnover.norm$`Region 3`) - min(turnover.norm$`Region 3`))
turnover.norm$`Region 4` <- (turnover.norm$`Region 4` - min(turnover.norm$`Region 4`)) / (max(turnover.norm$`Region 4`) - min(turnover.norm$`Region 4`))
turnover.norm$`Region 5` <- (turnover.norm$`Region 5` - min(turnover.norm$`Region 5`)) / (max(turnover.norm$`Region 5`) - min(turnover.norm$`Region 5`))
turnover.norm$`Region 6` <- (turnover.norm$`Region 6` - min(turnover.norm$`Region 6`)) / (max(turnover.norm$`Region 6`) - min(turnover.norm$`Region 6`))
turnover.norm$`Region 7` <- (turnover.norm$`Region 7` - min(turnover.norm$`Region 7`)) / (max(turnover.norm$`Region 7`) - min(turnover.norm$`Region 7`))
turnover.norm$`Region 8` <- (turnover.norm$`Region 8` - min(turnover.norm$`Region 8`)) / (max(turnover.norm$`Region 8`) - min(turnover.norm$`Region 8`))
turnover.norm$`Region 9` <- (turnover.norm$`Region 9` - min(turnover.norm$`Region 9`)) / (max(turnover.norm$`Region 9`) - min(turnover.norm$`Region 9`))
turnover.norm$`Region 10` <- (turnover.norm$`Region 10` - min(turnover.norm$`Region 10`)) / (max(turnover.norm$`Region 10`) - min(turnover.norm$`Region 10`))
turnover.norm$`Count mgr chg only` <- (turnover.norm$`Count mgr chg only` - min(turnover.norm$`Count mgr chg only`)) / (max(turnover.norm$`Count mgr chg only`) - min(turnover.norm$`Count mgr chg only`))
turnover.norm$`Count of job changes` <- (turnover.norm$`Count of job changes` - min(turnover.norm$`Count of job changes`)) / (max(turnover.norm$`Count of job changes`) - min(turnover.norm$`Count of job changes`))
turnover.norm$`count of area changes` <- (turnover.norm$`count of area changes` - min(turnover.norm$`count of area changes`)) / (max(turnover.norm$`count of area changes`) - min(turnover.norm$`count of area changes`))
turnover.norm$College <- (turnover.norm$College - min(turnover.norm$College)) / (max(turnover.norm$College) - min(turnover.norm$College))
turnover.norm$`Term Vol` <- (turnover.norm$`Term Vol` - min(turnover.norm$`Term Vol`)) / (max(turnover.norm$`Term Vol`) - min(turnover.norm$`Term Vol`))


summary(turnover.norm)
summary(new.employee.norm)

# Partition dataset 
set.seed(5)
turnover.train.index <- sample(nrow(turnover.norm), nrow(turnover.norm) * 0.6)
turnover.valid.index <- as.numeric(setdiff(rownames(turnover.norm), turnover.train.index))
turnover.train <- turnover.norm[turnover.train.index, ]
turnover.valid <- turnover.norm[turnover.valid.index, ]

# Perform a k-NN classification with all predictors.

install.packages("FNN")
library(FNN)

# Convert Term Vol into a numeric variable
turnover.train$`Term Vol` <- as.numeric(turnover.train$`Term Vol`)

# Exclude outcome variables from your list of predictors
t(t(names(turnover.train)))
t(t(names(new.employee.norm)))

dim(turnover.train)
dim(new.employee.norm)
length(turnover.train$`Term Vol`)
class(turnover.train$`Term Vol`)

nn <- knn(train = turnover.train[, c(1:5, 7:31)], test = new.employee.norm, cl = turnover.train$`Term Vol`, k = 1)
row.names(turnover.train)[attr(nn, "nn.index")]
nn

# Confusion matrix for the validation data where k=1.

nn.1 <- knn(train = turnover.train[, c(1:5,7:31)], test = turnover.valid[, c(1:5,7:31)], cl = turnover.train$`Term Vol`, k = 1)
row.names(turnover.train)[attr(nn.1, "nn.index")]
nn.1

install.packages("caret")
install.packages("e1071")
library(caret)
library(e1071)

confusionMatrix(nn.1, as.factor(turnover.valid$`Term Vol`), positive = "1")
summary(nn.1)

## Determine the best choice of k that maximizes accuracy on the validation data.

# Show the accuracy table you created to compare k

accuracy.df <- data.frame(k = seq(1, 20, 1), accuracy = rep(0, 20))

# Compute knn for different k on validation set

for (i in 1:20) {
  knn.pred <- knn(train = turnover.train[, c(1:5, 7:31)], test = turnover.valid[,c(1:5,7:31)], 
                  cl = turnover.train$`Term Vol`, k = i)
  accuracy.df[i, 2] <- confusionMatrix(knn.pred, as.factor(turnover.valid$`Term Vol`), positive = "1")$overall[1]
}
accuracy.df

# Classifying a new customer using the "best k" = 2

knn.pred.new <- knn(turnover.norm[,c(1:5,7:31)], new.employee.norm, cl = turnover.norm$`Term Vol`, k = 2)
row.names(Turnover_cleaned)[attr(knn.pred.new, "nn.index")]
knn.pred.new

# confusion matrix using best k on validation data
knn.pred.best <- knn(turnover.train[,c(1:5,7:31)], turnover.valid[,c(1:5, 7:31)], 
                     cl = turnover.train$`Term Vol`, k = 2, prob = TRUE)
confusionMatrix(knn.pred.best, as.factor(turnover.valid$`Term Vol`), positive = "1")

## Discriminant Analysis (propensity.high fixed) ##########

# Preprocess data for Discriminant Analysis

# Partition the data

set.seed(5)
train.index.DA <- sample(nrow(Turnover_cleaned), nrow(Turnover_cleaned) * 0.6)
valid.index.DA <- as.numeric(setdiff(rownames(Turnover_cleaned), train.index.DA))
turnover.train.DA <- Turnover_cleaned[train.index.DA, ]
turnover.valid.DA <- Turnover_cleaned[valid.index.DA, ]

t(t(names(turnover.train.DA)))
t(t(names(Turnover_cleaned)))

# Leave out Hire Year, Hire Month, and some dummy variables (Other shift, Job Executive, Sector 5, Region 10) which we will not use for classification

Turnover_cleaned.DA <- Turnover_cleaned[, -c(1,2,7,13,18,29)]
turnover.train.DA <- turnover.train.DA[, -c(1,2,7,13,18,29)]
turnover.valid.DA <- turnover.valid.DA[, -c(1,2,7,13,18,29)]

t(t(names(turnover.train.DA)))
t(t(names(Turnover_cleaned.DA)))

# Code for Discriminant Analysis

install.packages("DiscriMiner")
library(DiscriMiner)

turnover.da <- linDA(Turnover_cleaned.DA[, c(1:4, 6:27)], Turnover_cleaned.DA$`Term Vol`, validation = "learntest", 
                 learn = train.index.DA, 
                 test = valid.index.DA)
turnover.da$functions

# Classification scores, predicted classes, and probabilities
# Compute probabilities manually

propensity.high <- exp(turnover.da$scores[, 2]) / (exp(turnover.da$scores[, 1]) + exp(turnover.da$scores[, 2]))
da.results <- data.frame(Actual = turnover.valid.DA$`Term Vol`, turnover.da$classification, turnover.da$scores, 
                         propensity.high = propensity.high)
options(scipen = 999)
head(da.results, 25)

head(da.results, 20)

# Confusion matrix, specifying the success class as “1” (The employee will quit) and using a cutoff of 0.5

confusionMatrix(turnover.da$classification, 
                as.factor(turnover.valid.DA$`Term Vol`), 
                positive = "1")

# lift chart
install.packages("gains")
library(gains)

gain <- gains(as.numeric(turnover.valid.DA$`Term Vol`), 
              exp(turnover.da$scores[, 2]) / (exp(turnover.da$scores[, 1]) + exp(turnover.da$scores[, 2])), 
              groups = length(turnover.valid.DA))

# plot lift chart
plot(c(0, gain$cume.pct.of.total * sum(as.numeric(turnover.valid.DA$`Term Vol`))) ~ c(0, gain$cume.obs),
     xlab = "# of cases", ylab = "Cumulative", main = "Lift Chart", type = "l")
lines(c(0, sum(as.numeric(turnover.valid.DA$`Term Vol`))) ~ c(0, nrow(turnover.valid.DA)), lty = 2)

# compute deciles and plot decile-wise lift chart
gain <- gains(as.numeric(turnover.valid.DA$`Term Vol`), 
              exp(turnover.da$scores[, 2]) / (exp(turnover.da$scores[, 1]) + exp(turnover.da$scores[, 2])))
heights <- gain$mean.resp / mean(as.numeric(turnover.valid.DA$`Term Vol`))
dec.lift <- barplot(heights, names.arg = gain$depth, ylim = c(0, 8),
                    xlab = "Percentile", ylab = "Mean Response", main = "Decile-wise Lift Chart")



#tyler’s code
# Import data
#install.packages("readxl")
library(readxl)
library(rpart)
library(rpart.plot)
library(dplyr)
library(caret)
library(caret)
library(gains)
library(pROC)
library(forecast)
Turnover <- readxl::read_xlsx(path = "Turnover data.xlsx")

# Drop some unnecessary variables
t(t(names(Turnover)))
Turnover$`Hire Month`<- as.factor(Turnover$`Hire Month`)
Turnover$`Term Date`<- as.factor(Turnover$`Term Date`)
Turnover$`Hire Year`<- as.factor(Turnover$`Hire Year`)
summary(Turnover)

Turnover<- Turnover[ -c(1,2,3,4,6,11,12,14,15,37) ]
set.seed(1)
index <- sample(nrow(Turnover), nrow(Turnover) * 0.6)
Turnover.train <- Turnover[index, ]
Turnover.valid <- Turnover[-index, ]                     
Turnover.largetree <- rpart(`Term Vol` ~ ., data = Turnover.train,method= "class",cp=0.000001,minsplit = 1)
#valid confusion matrix for big ole tree
Turnover.valid.ct.pred1 <- predict(Turnover.largetree, Turnover.valid, type = "class")
confusionMatrix(Turnover.valid.ct.pred1, as.factor(Turnover.valid$`Term Vol`), positive = "1")

#printing big ole tree
prp(Turnover.largetree, type = 1, extra = 1, varlen = -10,
    box.col = ifelse(Turnover.largetree$frame$var == "<leaf>", 'gray', 'white'))

#plotting large tree
plotcp(Turnover.largetree)

#doing the trick to find the right cp (it's 0.00480307 using the method from class)
printcp(Turnover.largetree)

#re-running the tree with new CP
Turnover.largetree <- rpart(`Term Vol` ~ ., data = Turnover.train,method= "class",cp=0.00480307,minsplit = 1)
#printing the tree
Turnover.largetree

prp(Turnover.largetree, type = 1, extra = 1, varlen = -10,
    box.col = ifelse(Turnover.largetree$frame$var == "<leaf>", 'gray', 'white'))
Turnover.valid.ct.pred1 <- predict(Turnover.largetree, Turnover.valid, type = "class")
confusionMatrix(Turnover.valid.ct.pred1, as.factor(Turnover.valid$`Term Vol`), positive = "1")
#confusion matrix for trimmed tree

plotcp(Turnover.largetree)

printcp(Turnover.largetree)

#Logistic Regression
Turnover <- readxl::read_xlsx(path = "Turnover data.xlsx")
#logistic regression 

# Drop some unnecessary variables
t(t(names(Turnover)))
Turnover$`Hire Month`<- as.factor(Turnover$`Hire Month`)
Turnover$`Term Date`<- as.factor(Turnover$`Term Date`)
Turnover$`Hire Year`<- as.factor(Turnover$`Hire Year`)
summary(Turnover)

#dummiesmonth <- as.data.frame(model.matrix(~ 0 + `Hire Month`, data = Turnover))
#t(t(names(dummiesmonth)))

#dummiesyear <- as.data.frame(model.matrix(~ 0 + `Hire Year`, data = Turnover))
#t(t(names(dummiesyear)))

#dummiesdur <- as.data.frame(model.matrix(~ 0 + Duration, data = ebay))
#t(t(names(dummiesdur)))

#dummiesday <- as.data.frame(model.matrix(~ 0 + endDay, data = ebay))
#t(t(names(dummiesday)))
#combine into a data step and drop one

#dummies <- cbind(dummiesmonth[, -1], dummiesyear[, -1])
#head(dummies)
t(t(names(Turnover)))
Turnover<- Turnover[ -c(1,2,3,4,6,11,12,14,15,37) ]
#Turnover <- cbind(Turnover[, -c(1,2)], dummies)
t(t(names(Turnover)))
summary(Turnover)
set.seed(1)
index <- sample(nrow(Turnover), nrow(Turnover) * 0.6)
Turnover.train <- Turnover[index, ]
Turnover.valid <- Turnover[-index, ]   

Turnover.glm0 <- glm(`Term Vol` ~ ., family = "binomial", data = Turnover.train)
summary(Turnover.glm0)

#predicting based off the training data

pred.glm0.train <- predict(Turnover.glm0,newdata = Turnover.train, type = "response")

r <- roc(Turnover.train$`Term Vol`, pred.glm0.train)
plot.roc(r)
auc(r)

#confusion matrix for training data into our model
confusionMatrix(as.factor(ifelse(pred.glm0.train >= 0.5, "1", "0")), as.factor(Turnover.train$`Term Vol`), 
                positive = "1")

#validation data: how does it hold up
#prediction of feeding our validation data into the model
pred.glm0.valid <- predict(Turnover.glm0, newdata = Turnover.valid, type = "response")
data.frame(actual=Turnover.valid$`Term Vol`[1:10],predicted = pred.glm0.valid[1:10])
#sensitivity graph of validation data in original model
r <- roc(Turnover.valid$`Term Vol`, pred.glm0.valid)
plot.roc(r)
gain <- gains(Turnover.valid$`Term Vol`,pred.glm0.valid, groups = length(pred.glm0.valid))
plot(c(0, gain$cume.pct.of.total * sum(Turnover.valid$`Term Vol`)) ~ c(0, gain$cume.obs),
     xlab = "# of cases", ylab = "Cumulative", main = "Lift Chart", type = "l")
lines(c(0, sum(Turnover.valid$`Term Vol`)) ~ c(0, nrow(Turnover.valid)), lty = 2)

# compute deciles and plot decile-wise lift chart
gain <- gains(Turnover.valid$`Term Vol`, pred.glm0.valid)
heights <- gain$mean.resp / mean(Turnover.valid$`Term Vol`)
dec.lift <- barplot(heights, names.arg = gain$depth, ylim = c(0, 4),
                    xlab = "Percentile", ylab = "Mean Response", main = "Decile-wise Lift Chart")
Turnover.glm.null <- glm(`Term Vol` ~ 1, data = Turnover.train, family = "binomial")
#what's our stepwise look like
Turnover.glm.step <- step(Turnover.glm.null, scope = list(Turnover.glm.null, upper = Turnover.glm0),
                          direction = "both")
summary(Turnover.glm.step)
pred.glm0.step <- predict(Turnover.glm.step, newdata = Turnover.valid, type = "response")
r <- roc(Turnover.valid$`Term Vol`, pred.glm0.step)
plot.roc(r)
auc(r)
#confusion matrix for step model
confusionMatrix(as.factor(ifelse(pred.glm0.step >= 0.5, "1", "0")), as.factor(Turnover.valid$`Term Vol`), 
                positive = "1")
#lift measurements
gain <- gains(Turnover.valid$`Term Vol`,pred.glm0.step, groups = length(pred.glm0.step))
#life chart for training data
plot(c(0, gain$cume.pct.of.total * sum(Turnover.valid$`Term Vol`)) ~ c(0, gain$cume.obs),
     xlab = "# of cases", ylab = "Cumulative", main = "Lift Chart", type = "l")
lines(c(0, sum(Turnover.valid$`Term Vol`)) ~ c(0, nrow(Turnover.valid)), lty = 2)
# compute deciles and plot decile-wise lift chart
gain <- gains(Turnover.valid$`Term Vol`, pred.glm0.step)
heights <- gain$mean.resp / mean(Turnover.valid$`Term Vol`)
dec.lift <- barplot(heights, names.arg = gain$depth, ylim = c(0, 4),
                    xlab = "Percentile", ylab = "Mean Response", main = "Decile-wise Lift Chart")



