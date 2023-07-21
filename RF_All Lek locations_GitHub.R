##LPCH Random Forest Lek Analysis for Predicting Habitat Suitability--MGP
##ALL LEKS VS RANDOM POINTS
##Morgan Solomon


require(tidyverse)  
require(raster)    
require(sf)        
require(rgdal)     
require(sp)         
require(ggpubr)
require(ROCR)
require(pROC)
require(caret)
require(randomForest)



workingD<-"C:/Users/morga/OneDrive - Montana State University/Solomon, Morgan/Habitat Model"
setwd(workingD)



#Import HabCov csv (This was changed after adding the EVI and transmission line data)
LPCHdata<-read.csv("C:/Users/morga/OneDrive - Montana State University/Solomon, Morgan/Habitat Model/LPCHdata_1321.csv")

str(LPCHdata) #check to see that POINT_TYPE is a factor and all others= columns are numeric or integer form

#Making values the right type####
LPCHdata$POINT_TYPE<-as.factor(LPCHdata$POINT_TYPE) #must be a factor if you want to run a CLASSSIFICATION random forest model and not a Regression random forest model


leks <- subset(LPCHdata, POINT_TYPE =="LEK")


randoms <- subset(LPCHdata, POINT_C =="RANDOM")
set.seed(123) #set seed so you can rerun same dataset in future
randoms <- randoms[sample(nrow(randoms), 272), ] #randomly select 272 random points to use in this analysis (same number as the number of leks)

RFdata<- rbind(leks, randoms) #combining data to make reduced dataset for RF model. Classification trees suffer from imbalanced data. Imbalanced datasets can result in very low error rates for the majority class (specificity) but very high error rates for minority class (sensitivity) as there is high cross-classification error of the minority class because minority observations are not as frequently during the bootstrap process in RF models. As such, we reduced our observations of random points to equal the number of observations of leks as this provided the lowest error rate for our lek locations without drastically compromising the models ability to correctly classify random points. 


RFdata<- RFdata %>%
  subset(select = - c(Data_set:Stable_lek ))

str(RFdata) 

write.csv(RFdata, file ="C:/Users/morga/OneDrive - Montana State University/Solomon, Morgan/Habitat Model/Random Forest models/RFdata_New.csv")

RFdata<-read.csv("C:/Users/morga/OneDrive - Montana State University/Solomon, Morgan/Habitat Model/Random Forest models/RFdata_New.csv")

RFdata$POINT_TYPE<-as.factor(RFdata$POINT_TYPE) #making sure POINT_TYPE is a factor so that randomForest can be run in the classification instance


str(RFdata) #check that your dataframe only include covariates and POINT_TYPE only

RFdata<-subset(-c(X))

########Using caret package to make random forest model####

#splitting data into training set and a testing set
set.seed(778)
smp_size <- floor(0.80 * nrow(RFdata)) # training data is 80%

train_ind <- sample(seq_len(nrow(RFdata)), size = smp_size)

train <- RFdata[train_ind, ]
test <- RFdata[-train_ind, ] #although we are already doing cross validation in the trainControl function below, I wanted to create a testing set as well

#Determing what mtry should be used in final model####
ctrl <- trainControl(method = 'repeatedcv', number = 5, repeats=10, classProbs = TRUE, summaryFunction = twoClassSummary, search = 'grid') #see above but classProbs = TRUE allows you to have a continuous values when predicting lek occurrence instead of binary and search = 'random' allows us to randomly generate 15 random values to see what mtry provides the best accuracy (this takes a while)

set.seed(779)


tunegrid<- expand.grid(.mtry=(2:24)) #creating a grid that determines what mtry should be set to

mtry_fit <- train(POINT_TYPE ~ .,
                  data = train,
                  method = "rf",
                  verbose = FALSE,
                  metric = "ROC",
                  trControl = ctrl,
                  tuneGrid = tunegrid)
tunegrid
mtry_fit
plot(mtry_fit) #we will use mtry = 2



#Determining how many trees should be in the RF model####
tunegrid <- expand.grid(.mtry = c(sqrt(ncol(RFdata))))
modellist <- list()

#train with different ntree parameters
for (ntree in c(500,600,700,800,900)){
  set.seed(779)
  fit <- train(POINT_TYPE~.,
               data = RFdata,
               method = 'rf',
               metric = 'ROC',
               tuneGrid = tunegrid,
               trControl = ctrl,
               ntree = ntree)
  key <- toString(ntree)
  modellist[[key]] <- fit
}

#Compare results
results <- resamples(modellist)
summary(results) #highest accuracy was at 700 trees

dotplot(results)


#Fit model with all data
str(RFdata)


ctrl <- trainControl(method = 'repeatedcv', number = 5, repeats=100, classProbs = TRUE, summaryFunction = twoClassSummary)

rf.fit.all <- train(POINT_TYPE ~ .,
                    data = RFdata,
                    method = "rf",
                    verbose = FALSE,
                    metric = "ROC",
                    ntree = 700,
                    trControl = ctrl,
                    tuneGrid = data.frame(.mtry = 2))



rf.fit.all
#AUC: 0.89

#Variable Importance Plots
viall<-varImp(rf.fit.all)
b<-plot(viall)
b

#Partial dependency plots
RF<-randomForest(POINT_TYPE~., data = RFdata, ntree = 700, mtry = 2)


line = 1
cex = 2
side = 3
adj=-0.05

par(mfrow = c(2, 5))
partialPlot(RF, RFdata, AveTree, "LEK", main = '', ylab = 'Probability of a lek' )
mtext("A", side=side, line=line, cex=cex, adj=adj)
partialPlot(RF, RFdata, AvePFG, "LEK" , main = '', ylab = 'Probability of a lek')
mtext("B", side=side, line=line, cex=cex, adj=adj)
partialPlot(RF, RFdata, AvePrecip, "LEK", main = '', ylab = 'Probability of a lek' )
mtext("C", side=side, line=line, cex=cex, adj=adj)
partialPlot(RF, RFdata, DensOil, "LEK", main = '', ylab = 'Probability of a lek' )
mtext("D", side=side, line=line, cex=cex, adj=adj)
partialPlot(RF, RFdata, AveCrop, "LEK", main = '', ylab = 'Probability of a lek' )
mtext("E", side=side, line=line, cex=cex, adj=adj)
partialPlot(RF, RFdata, DistHighway, "LEK", main = '', ylab = 'Probability of a lek' )
mtext("F", side=side, line=line, cex=cex, adj=adj)
partialPlot(RF, RFdata, AveAFG, "LEK", main = '', ylab = 'Probability of a lek' )
mtext("G", side=side, line=line, cex=cex, adj=adj)
partialPlot(RF, RFdata, DistTrans, "LEK", main = '', ylab = 'Probability of a lek' )
mtext("H", side=side, line=line, cex=cex, adj=adj)
partialPlot(RF, RFdata, AveTemp, "LEK", main = '', ylab = 'Probability of a lek' )
mtext("I", side=side, line=line, cex=cex, adj=adj)
partialPlot(RF, RFdata, Ruggedness, "LEK", main = '', ylab = 'Probability of a lek' )
mtext("J", side=side, line=line, cex=cex, adj=adj)



str(RFdata)

#Double check model on test data
ctrl <- trainControl(method = 'repeatedcv', number = 5, repeats=100, classProbs = TRUE, summaryFunction = twoClassSummary)

rf.fit <- train(POINT_TYPE ~ .,
                data = train,
                method = "rf",
                verbose = FALSE,
                metric = "ROC",
                trControl = ctrl,
                ntee= 700,
                tuneGrid = tunegrid)

# Build custom AUC function to extract AUC
# from the caret model object
test_roc <- function(model, data) {
  
  roc(data$POINT_TYPE,
      predict(model, data, type = "prob")[, "LEK"])
  
}

rf.fit %>%
  test_roc(data = test) %>%
  auc() #AUC: 0.92

rf.fit



rf.fit.all #ROC: 0.89

rf.fit.all$finalModel$ntree


#Checking a simplified version of AUC
num.sims <- 500

# Create place to store output from bootstrap
AUC <- rep(NA, num.sims)
ctrl <- trainControl(classProbs = TRUE, summaryFunction = twoClassSummary)

for (i in 1:num.sims){
  smp_size <- floor(0.80 * nrow(RFdata)) # training data is 80%
  
  train_ind <- sample(seq_len(nrow(RFdata)), size = smp_size)
  
  train <- RFdata[train_ind, ]
  test <- RFdata[-train_ind, ]
  
  auc_fit <- train(POINT_TYPE ~ .,
                   data = train,
                   method = "rf",
                   verbose = FALSE,
                   metric = "ROC",
                   trControl = ctrl)
  
  # Build custom AUC function to extract AUC
  # from the caret model object
  
  test_roc <- function(model, data) {
    
    roc(data$POINT_TYPE,
        predict(model, data, type = "prob")[, "LEK"])
    
  }
  
  AUC[i]<-auc_fit %>%
    test_roc(data = test) %>%
    auc()
}

mean(AUC) # 0.88
quantile(AUC, c(.025, .975)) #0.82 - 0.94

#Variable importance in final RF model####
VI<-varImp(rf.fit.all)
VI

plot(VI)



#Building stack for covariates included in model and predicitng habitat suitability for MGP####

setwd("C:/Users/morga/OneDrive - Montana State University/Solomon, Morgan/Habitat Model/Final Layers_rasterstack/Habitat Layers/CroppedResampled")


AvePFG<-raster("AvePFG.tif")
AveTree<-raster("AveTree.tif")
AveAFG<- raster("AveAFG.tif")
AveShrub<-raster("AveShrub.tif")
AveLit<-raster("AveLit.tif")
AveBG<-raster("AveBG.tif")


AveCrop<-raster("AveCrop.tif")
AveTemp<-raster("AveTemp.tif")
AvePrecip<-raster("AvePrecip.tif")
AveElev<-raster("AveElev.tif")


DensOil<-raster("DensOil.tif")
DensWind<-raster("DensWind.tif")
DensRoad<-raster("DensRoadway.tif")

DistHighway<-raster("DistHighway.tif")
DistWind<-raster("DistWind.tif")
DistTrans<-raster("DistTrans.tif")
DistRoadway<-raster("DistRoadway.tif")
DistOil<-raster("DistOil.tif")

Ruggedness<-raster("Ruggedness.tif")

VarBG<-raster("VarBG.tif")
VarPFG<-raster("VarPFG.tif")
VarShrub<-raster("VarShrub.tif")
VarAFG<-raster("VarAFG.tif")
VarLit<-raster("VarLit.tif")



stack<-stack(AveTree,AvePFG,AveAFG,AveShrub,AveLit,AveBG,DensOil,DensWind,DensRoad,DistHighway,DistWind,DistTrans,DistRoadway,DistOil,Ruggedness,VarPFG,VarAFG,VarLit,VarShrub,VarBG,AvePrecip,AveCrop,AveTemp) #raster stack to make prediction on

Suitability<-predict(stack, rf.fit.all, type = 'prob', filename = "RF_All_Newdata_seed123.tif" ) #using predict function to predict on your raster stack using the random forest model


#plotting raster
plot(Suitability)
