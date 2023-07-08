##LPCH RSF Lek Analysis for Predicting Habitat Suitability--MGP
##ALL LEKS VS RANDOM POINTS
##Morgan Solomon
## 10.14.21;

library(tidyverse)  #data wrangling and visualization package (dplyr, ggplot,)
library(raster)     #easy to use tool for working with raster data
library(sf)        
library(rgdal)      #great for working with spatial data
library(sp)         #need for feature class data
library(psych)      #applied stats package for psychology but with great basic descriptive routines
library(lme4)       #package with routines for fitting GLMMs
library(AICcmodavg) 
library(mgcv)       #methods for GAMs; multiple smoothing parameter selection (GCV, REML, or UBRE/AIC)
library(gridExtra)  #grid graphics
library(NLMR)       #complements raster package and returns simulations as RasterLayer objects
library(DHARMa)     #Moran's I test, spatial auto correlation
library(ResourceSelection) #functions for use/availablilty data
library(corrgram)
library(ggpubr)


workingD<-"C:/Users/morga/OneDrive - Montana State University/Solomon, Morgan/Habitat Model"
setwd(workingD)



#Import HabCov csv (This was changed after adding the EVI and transmission line data)
LPCHdata<-read.csv("C:/Users/morga/OneDrive - Montana State University/Solomon, Morgan/Habitat Model/LPCHdata_1321.csv")

##################Habitat Modeling - All leks####

#Using gams to visualize trends/pattern in a univariate analysis--- leks vs random points####
gam1 <- gam(PT_Type ~ s(AveTree),family=binomial, data=LPCHdata)
gam2 <- gam(PT_Type ~ s(AvePFG),family=binomial, data=LPCHdata)
gam3 <- gam(PT_Type ~ s(AveAFG),family=binomial, data=LPCHdata)
gam4 <- gam(PT_Type ~ s(AveBG),family=binomial, data=LPCHdata)
gam5 <- gam(PT_Type ~ s(AveLit),family=binomial, data=LPCHdata)
gam6 <- gam(PT_Type ~ s(AveShrub),family=binomial, data=LPCHdata)
gam7 <- gam(PT_Type ~ s(AveCrop),family=binomial, data=LPCHdata)
gam8 <- gam(PT_Type ~ s(AveElev),family=binomial, data=LPCHdata)
gam9 <- gam(PT_Type ~ s(AvePrecip),family=binomial, data=LPCHdata)
gam10<- gam(PT_Type ~ s(AveTemp),family=binomial, data=LPCHdata)
gam11<- gam(PT_Type ~ s(Ruggedness),family=binomial, data=LPCHdata)
gam12<- gam(PT_Type ~ s(DistOil),family=binomial, data=LPCHdata)
gam13<- gam(PT_Type ~ s(DistHighway),family=binomial, data=LPCHdata)
gam14<- gam(PT_Type ~ s(DistWind),family=binomial, data=LPCHdata)
gam15<- gam(PT_Type ~ s(DistRoadway),family=binomial, data=LPCHdata)
gam16<- gam(PT_Type ~ s(DensRoadway),family=binomial, data=LPCHdata)
gam17<- gam(PT_Type ~ s(DensOil),family=binomial, data=LPCHdata)
gam18<- gam(PT_Type ~ s(DensWind),family=binomial, data=LPCHdata)
gam19<- gam(PT_Type ~ s(VarAFG),family=binomial, data=LPCHdata)
gam20<- gam(PT_Type ~ s(VarPFG),family=binomial, data=LPCHdata)
gam21<- gam(PT_Type ~ s(VarLit),family=binomial, data=LPCHdata)
gam22 <- gam(PT_Type ~ s(VarBG),family=binomial, data=LPCHdata)
gam24 <- gam(PT_Type ~ s(VarShrub),family=binomial, data=LPCHdata)
gam25 <- gam(PT_Type ~ s(VarEVI),family=binomial, data=LPCHdata)
gam26 <- gam(PT_Type ~ s(AveEVI),family=binomial, data=LPCHdata)
gam27 <- gam(PT_Type ~ s(DistTrans),family=binomial, data=LPCHdata)

#Plotting relationships of leks vs random points####
plot(gam1, ylab='', xlab="Avg Tree", cex.axis=1.5, cex.lab=1.5, yaxt='n') #linear decreasing
plot(gam2, ylab='', xlab="Avg PFG", cex.axis=1.5, cex.lab=1.5, yaxt='n') #quadratic increasing
plot(gam3, ylab='', xlab="Avg AFG", cex.axis=1.5, cex.lab=1.5, yaxt='n') #linear decreasing
plot(gam4, ylab='', xlab="Avg BG", cex.axis=1.5, cex.lab=1.5, yaxt='n')  #ln(x)
plot(gam5, ylab='', xlab="Avg Litter", cex.axis=1.5, cex.lab=1.5, yaxt='n') # ln(x)
plot(gam6, ylab='', xlab="Avg Shrub", cex.axis=1.5, cex.lab=1.5, yaxt='n') #ln(x)
plot(gam7, ylab='', xlab="Avg Crop", cex.axis=1.5, cex.lab=1.5, yaxt='n') #quadratic decreasing
plot(gam8, ylab='', xlab="Avg Elev", cex.axis=1.5, cex.lab=1.5, yaxt='n') #No effect, remove
plot(gam9, ylab='', xlab="Avg Precip", cex.axis=1.5, cex.lab=1.5, yaxt='n') #Quadratic
plot(gam10, ylab='', xlab="Avg Temp", cex.axis=1.5, cex.lab=1.5, yaxt='n') #Quadratic 
plot(gam11, ylab='', xlab="Ruggedness", cex.axis=1.5, cex.lab=1.5, yaxt='n') #quadratic
plot(gam12, ylab='', xlab="Dist Oil", cex.axis=1.5, cex.lab=1.5, yaxt='n')   #linear
plot(gam13, ylab='', xlab="Dist Highway", cex.axis=1.5, cex.lab=1.5, yaxt='n') #increasing quadratic
plot(gam14, ylab='', xlab="Dist Wind", cex.axis=1.5, cex.lab=1.5, yaxt='n')    #quadratic
plot(gam15, ylab='', xlab="Dist Roadway", cex.axis=1.5, cex.lab=1.5, yaxt='n') #ln(x)
plot(gam16, ylab='', xlab="Dens Roadway", cex.axis=1.5, cex.lab=1.5, yaxt='n') #linear
plot(gam17, ylab='', xlab="Dens Oil", cex.axis=1.5, cex.lab=1.5, yaxt='n') #linear decreasing
plot(gam18, ylab='', xlab="Dens Wind", cex.axis=1.5, cex.lab=1.5, yaxt='n') #linear decreasing
plot(gam19, ylab='', xlab="Var AFG", cex.axis=1.5, cex.lab=1.5, yaxt='n')  #No effect, linear
plot(gam20, ylab='', xlab="Var PFG", cex.axis=1.5, cex.lab=1.5, yaxt='n') #ln(x)
plot(gam21, ylab='', xlab="Var Lit", cex.axis=1.5, cex.lab=1.5, yaxt='n') #linear decreasing
plot(gam22, ylab='', xlab="Var BG", cex.axis=1.5, cex.lab=1.5, yaxt='n') #linear decreasing
plot(gam24, ylab='', xlab="Var Shrub", cex.axis=1.5, cex.lab=1.5, yaxt='n') #Maybe ln(x)
plot(gam25, ylab='', xlab="Var EVI", cex.axis=1.5, cex.lab=1.5, yaxt='n')#quadratic
plot(gam26, ylab='', xlab="Ave EVI", cex.axis=1.5, cex.lab=1.5, yaxt='n')#quadratic
plot(gam27, ylab='', xlab="DistTrans", cex.axis=1.5, cex.lab=1.5, yaxt='n')#quadratic

#Removed AveEVI and VarEVI due to a concern that EVI was correlated with cropland cover (i.e., areas of high cropland cover that had a 'good' EVI for LPCHs)


#Model list and AIC table####

Cand.models <- list( )

Cand.models[[1]]<-glm(PT_Type ~ AveTree + AvePFG + I(AvePFG^2) + AveAFG + log(AveBG+0.001) + log(AveLit+0.001) + AveShrub + I(AveShrub^2) + AveCrop + I(AveCrop^2) + AvePrecip + I(AvePrecip^2) + AveTemp +I(AveTemp^2) + Ruggedness + I(Ruggedness^2) + DistHighway + I(DistHighway^2) + DistOil + DistWind + I(DistWind^2) + log(DistRoadway+0.001) + DensRoadway + I(DensRoadway^2) + DensOil + DensWind + VarPFG + I(VarPFG^2) + VarBG + log(VarShrub+0.001) + log(VarLit+0.001) +  DistTrans + I(DistTrans^2), family=binomial, data = LPCHdata)

Cand.models[[2]] <- glm(PT_Type ~ AveTree + AvePFG + I(AvePFG^2) + AveAFG + log(AveBG+0.001) + log(AveLit+0.001) + AveShrub + I(AveShrub^2) + AveCrop + I(AveCrop^2) + AvePrecip + I(AvePrecip^2) + AveTemp +I(AveTemp^2) + Ruggedness + I(Ruggedness^2) + DistHighway + I(DistHighway^2) + DistOil + DistWind + I(DistWind^2) + log(DistRoadway+0.001) + DensOil + DensWind + VarPFG + I(VarPFG^2) + VarBG + log(VarShrub+0.001) + log(VarLit+0.001) +  DistTrans + I(DistTrans^2), family=binomial, data = LPCHdata) #DensRoadway dropped

Cand.models[[3]] <- glm(PT_Type ~ AveTree + AvePFG + I(AvePFG^2) + AveAFG + log(AveBG+0.001) + AveShrub + I(AveShrub^2) + AveCrop + I(AveCrop^2) + AvePrecip + I(AvePrecip^2) + AveTemp +I(AveTemp^2) + Ruggedness + I(Ruggedness^2) + DistHighway + I(DistHighway^2) + DistOil + DistWind + I(DistWind^2) + log(DistRoadway+0.001) + DensOil + DensWind + VarPFG + I(VarPFG^2) + VarBG + log(VarShrub+0.001) + log(VarLit+0.001) +  DistTrans + I(DistTrans^2), family=binomial, data = LPCHdata) #DensRoadways, AveLit dropped

Cand.models[[4]] <- glm(PT_Type ~ AveTree + AvePFG + I(AvePFG^2) + AveAFG + log(AveBG+0.001) + AveShrub + I(AveShrub^2) + AveCrop + I(AveCrop^2) + AvePrecip + I(AvePrecip^2) + AveTemp +I(AveTemp^2) + Ruggedness + I(Ruggedness^2) + DistHighway + I(DistHighway^2) + DistWind + I(DistWind^2) + log(DistRoadway+0.001) + DensOil + DensWind + VarPFG + I(VarPFG^2) + VarBG + log(VarShrub+0.001) + log(VarLit+0.001) +  DistTrans + I(DistTrans^2), family=binomial, data = LPCHdata) #DensRoadway,AveLit, DistOil dropped

Cand.models[[5]] <- glm(PT_Type ~ AveTree + AvePFG + I(AvePFG^2) + AveAFG + log(AveBG+0.001) + AveShrub + I(AveShrub^2) + AveCrop + I(AveCrop^2) + AvePrecip + I(AvePrecip^2) + AveTemp +I(AveTemp^2) + Ruggedness + I(Ruggedness^2) + DistHighway + I(DistHighway^2) + DistWind + I(DistWind^2) + log(DistRoadway+0.001) + DensOil + DensWind + VarPFG + I(VarPFG^2) + VarBG + log(VarShrub+0.001) +  DistTrans + I(DistTrans^2), family=binomial, data = LPCHdata) #DensRoadway,AveLit, DistOil, VarLitter dropped

Cand.models[[6]] <- glm(PT_Type ~ AveTree + AvePFG + I(AvePFG^2) + AveAFG + log(AveBG+0.001) + AveShrub + I(AveShrub^2) + AveCrop + I(AveCrop^2) + AveTemp +I(AveTemp^2) + Ruggedness + I(Ruggedness^2) + DistHighway + I(DistHighway^2) + DistWind + I(DistWind^2) + log(DistRoadway+0.001) + DensOil + DensWind + VarPFG + I(VarPFG^2) + VarBG + log(VarShrub+0.001) +  DistTrans + I(DistTrans^2), family=binomial, data = LPCHdata) #DensRoadway,AveLit, DistOil, VarLitter dropped, AvePrecip dropped

Cand.models[[7]] <- glm(PT_Type ~ AveTree + AvePFG + I(AvePFG^2) + AveAFG + AveShrub + I(AveShrub^2) + AveCrop + I(AveCrop^2) + AveTemp +I(AveTemp^2) + Ruggedness + I(Ruggedness^2) + DistHighway + I(DistHighway^2) + DistWind + I(DistWind^2) + log(DistRoadway+0.001) + DensOil + DensWind + VarPFG + I(VarPFG^2) + VarBG + log(VarShrub+0.001) +  DistTrans + I(DistTrans^2), family=binomial, data = LPCHdata) #DensRoadway,AveLit, DistOil, VarLitter dropped, AvePrecip,AveBG dropped

Cand.models[[8]] <- glm(PT_Type ~ AveTree + AvePFG + I(AvePFG^2) + AveAFG + AveShrub + I(AveShrub^2) + AveCrop + I(AveCrop^2) + AveTemp +I(AveTemp^2) + Ruggedness + I(Ruggedness^2) + DistHighway + I(DistHighway^2) + DistWind + I(DistWind^2) + DensOil + DensWind + VarPFG + I(VarPFG^2) + VarBG + log(VarShrub+0.001) +  DistTrans + I(DistTrans^2), family=binomial, data = LPCHdata) #DensRoadway,AveLit, DistOil, VarLitter dropped, AvePrecip,AveBG, DistRoadway dropped

Cand.models[[9]] <- glm(PT_Type ~ AveTree + AvePFG + I(AvePFG^2) + AveAFG + AveShrub + I(AveShrub^2) + AveCrop + I(AveCrop^2) + AveTemp +I(AveTemp^2) + Ruggedness + I(Ruggedness^2) + DistHighway + I(DistHighway^2) + DistWind + I(DistWind^2) + DensOil + VarPFG + I(VarPFG^2) + VarBG + log(VarShrub+0.001) +  DistTrans + I(DistTrans^2), family=binomial, data = LPCHdata) #DensRoadway,AveLit, DistOil, VarLitter, AvePrecip,AveBG, DistRoadway, DensWind dropped


###AIC table

Modnames <- paste("mod", 1:length(Cand.models), sep = " ")

aictable<-aictab(cand.set = Cand.models, modnames = Modnames, sort = TRUE, nobs = NULL)

aictable

setwd("C:/Users/morga/OneDrive - Montana State University/Solomon, Morgan/Habitat Model/RSF models")

AIC<-as.data.frame(aictable)


write.csv(AIC, file ="C:/Users/morga/OneDrive - Montana State University/Solomon, Morgan/Habitat Model/RSF models/AICmodelselection_AllLeksVRando.csv")

###SEE MODEL SELECTION.R TO LOOK AT HOW YOU GOT YOUR FINAL MODEL (BACKWARDS SELECTION WITH AIC)

#### Test for spatial autocorrelation#### 


# fit a non-spatial global model; global means that it has an average value for the entire data set
GLOBAL <- glm(PT_Type ~ AveTree + AvePFG + AveAFG + AveBG + AveLit + AveShrub + AveCrop + AveElev + AvePrecip + AveTemp + DistHighway + DistOil + DistWind + DistRoadway + DistTrans + DensRoadway + DensOil + DensWind + VarAFG + VarPFG + VarBG  + VarShrub + VarLit, family=binomial, LPCHdata)   #change these to paramaterize a global logistic regression
# plot residuals

head(LPCHdata)
LPCHdata$resid <- resid(GLOBAL)
LPCHdata$resid_std <- rescale(LPCHdata$resid, 1, 10)
ggplot(LPCHdata, aes(x = x, y = y, size = resid)) +
  geom_point() +
  scale_size_continuous(range = c(1,10))

# The Formal test
sims <- simulateResiduals(GLOBAL)
testSpatialAutocorrelation(sims, plot = FALSE)  # Moran's I from package DHARMa  



### Habitat Modeling  ####
# Leks vs. Random points 
Cand.models <- list( )

#standard model
Cand.models[[1]] <- glm(PT_Type ~ AveTree + AvePFG + AveAFG + AveBG + AveLit + AveShrub + AveBG +AveCrop + AvePrecip + AveTemp + DistHighway + DistOil + DistWind + DistRoadway + DensRoadway + DensOil + DensWind + VarPFG + VarBG + VarShrub + VarLit + VarEVI + AveEVI + DistTrans, family=binomial, LPCHdata)

#model based on GAMs and model selection with AIC and after removing EVI variables which was heavily correlated with Cropland cover 
Cand.models[[2]] <-  glm(PT_Type ~ AveTree + AvePFG + I(AvePFG^2) + AveAFG + AveShrub + I(AveShrub^2) +  + AveCrop + I(AveCrop^2) + AveTemp +I(AveTemp^2) + Ruggedness + I(Ruggedness^2) + DistHighway + I(DistHighway^2) + DistWind + I(DistWind^2) + DensOil + DensWind + VarPFG + I(VarPFG^2) + VarBG + log(VarShrub+0.001) + DistTrans + I(DistTrans^2), family=binomial, data = LPCHdata)



##create a vector of names to trace back models in set
Modnames <- paste("mod", 1:length(Cand.models), sep = " ")

##round to 1 digits after decimal point and give log-likelihood
print(aictab(cand.set = Cand.models, modnames = Modnames, sort = TRUE, nobs = NULL),
      digits = 2, LL = TRUE)



#Test GOF using a Hosmer-Lemeshow GOF test####

library(ResourceSelection)
hoslem.test(LPCHdata$PT_Type, fitted(Cand.models[[2]]))  #model appears to fit the data well because we have not significant differnece between teh model and observed data. Large p-values mean that the model fits the data well.
#X^2 = 6.57 ; p-value = 0.583

##### Validating model using AUROC####

library(ROCR)
pred <- predict(Cand.models[[2]], LPCHdata, type = "response")

test_output <- cbind(LPCHdata,pred)   # should be using a testing data set for first term

#Plotting ROC graph for data, if area more, then good fit
length(fitted(Cand.models[[2]]))==length(LPCHdata$PT_Type)

preds <- prediction(as.numeric(pred), as.numeric(LPCHdata$PT_Type))
perf <- performance(preds,"tpr","fpr") #tpr = true positivie rate, fpr = false positive rate
plot(perf)
abline(a=0, b=1)

auc_ROCR <- performance(preds, measure = "auc")
auc_ROCR <- auc_ROCR@y.values[[1]] 

auc_ROCR #0.89

boxplot(test_output$pred~test_output$PT_Type)

############### Cross validation of leks v randoms model ####
#    Cross validation - 500 folds - calculating AUC at each iteration
#    Put the AUC into a loop that resamples and calulates 95%CI

# Number of runs
num.sims <- 500

# Create place to store output from bootstrap
AUC <- rep(NA, num.sims)
#HL.P <- rep(NA, num.sims)

for (i in 1:num.sims){
  smp_size <- floor(0.80 * nrow(LPCHdata)) # training data is 80%
  
  train_ind <- sample(seq_len(nrow(LPCHdata)), size = smp_size)
  
  train <- LPCHdata[train_ind, ]
  test <- LPCHdata[-train_ind, ]
  
  ## Calculate AUC for test dataset using model calibrated for training set
  # Fit model to test dataset
  model2 <- glm(PT_Type ~ AveTree + AvePFG + I(AvePFG^2) + AveAFG + AveShrub + I(AveShrub^2) + AveCrop + I(AveCrop^2) + AveTemp +I(AveTemp^2) + Ruggedness + I(Ruggedness^2) + DistHighway + I(DistHighway^2) + DistWind + I(DistWind^2) + DensOil + DensWind + VarPFG + I(VarPFG^2) + VarBG + log(VarShrub+0.001) + DistTrans + I(DistTrans^2), family=binomial, data = train)
  
  
  pred <- predict(model2, test, type = "response")
  
  test_output <- cbind(test,pred)   # should be using a testing data set for first term
  
  #Plotting ROC graph for data, if area more, then good fit:
  length(fitted(model2))==length(test$PT_Type)
  
  preds <- prediction(as.numeric(pred), as.numeric(test$PT_Type))
  perf <- performance(preds,"tpr","fpr")
  plot(perf)
  abline(a=0, b=1)
  
  auc_ROCR <- performance(preds, measure = "auc")
  
  AUC[i] <- auc_ROCR@y.values[[1]] 
} 

mean(AUC)  # 0.87
quantile(AUC, c(.025, .975))  #95%CI = 0.84 - 0.90

hist(AUC)

###Model coefficient to csv###

model2<-glm(PT_Type ~ AveTree + AvePFG + I(AvePFG^2) + AveAFG + AveShrub + I(AveShrub^2) + AveCrop + I(AveCrop^2) + AveTemp +I(AveTemp^2) + Ruggedness + I(Ruggedness^2) + DistHighway + I(DistHighway^2) + DistWind + I(DistWind^2) + DensOil + DensWind + VarPFG + I(VarPFG^2) + VarBG + log(VarShrub+0.001) + DistTrans + I(DistTrans^2), family=binomial, data = LPCHdata)

summary(model2)
std.err<-coef(summary(model2))[,2]
stderror<-as.data.frame((std.err))
write.csv(stderror, file ="C:/Users/morga/OneDrive - Montana State University/Solomon, Morgan/Habitat Model/LeksVsRandoms_EVI_standardErrors.csv")

model2coef <- as.data.frame(model2$coefficients)
write.csv(model2coef, file ="C:/Users/morga/OneDrive - Montana State University/Solomon, Morgan/Habitat Model/LeksVsRandoms_noEVI_Coefficients.csv")


#Building stack for covariates included in model####

setwd("C:/Users/morga/OneDrive - Montana State University/Solomon, Morgan/Habitat Model/Final Layers_rasterstack/Habitat Layers/CroppedResampled")


AvePFG<-raster("AvePFG.tif")
AveTree<-raster("AveTree.tif")
AveAFG<- raster("AveAFG.tif")
AveShrub<-raster("AveShrub.tif")

AveCrop<-raster("AveCrop.tif")
AveTemp<-raster("AveTemp.tif")
AveEVI<-raster("AveEVI.tif")

DensOil<-raster("DensOil.tif")
DensWind<-raster("DensWind.tif")

DistHighway<-raster("DistHighway.tif")
DistWind<-raster("DistWind.tif")
DistTrans<-raster("DistTrans.tif")

Ruggedness<-raster("Ruggedness.tif")

VarBG<-raster("VarBG.tif")
VarPFG<-raster("VarPFG.tif")
VarShrub<-raster("VarShrub.tif")
VarEVI<-raster("VarEVI.tif")

head(LPCHdata$DistHighway)
DistHighway

#glm(PT_Type ~ AveTree + AvePFG + I(AvePFG^2) + AveAFG + AveShrub + I(AveShrub^2) + AveCrop + I(AveCrop^2) + AveTemp +I(AveTemp^2) + Ruggedness + I(Ruggedness^2) + DistHighway + I(DistHighway^2) + DistWind + I(DistWind^2) + DensOil + DensWind + VarPFG + I(VarPFG^2) + VarBG + log(VarShrub+0.001) + AveEVI + I(AveEVI^2) + VarEVI + I(VarEVI^2) + DistTrans + I(DistTrans^2), family=binomial, data = LPCHdata)

stack<-stack(AveTree,AvePFG,AveAFG,AveShrub,DensOil,DensWind,DistHighway,DistWind,DistTrans,Ruggedness,VarPFG,VarShrub,VarBG,AveTemp,AveCrop) #building raster stack to use predict function on below

summary(model2)



setwd("C:/Users/morga/OneDrive - Montana State University/Solomon, Morgan/Habitat Model/Suitability rasters/New data rasters")
#Making a habitat suitability raster for the MGP####
Suitability<-predict(stack, model2, filename = "RSF_AllLeks_Newdata.tif", ) #THIS WAS DONE WHEN YOU MADE THE FIRST PREDICTION; USE WHEN FIRST MAKING YOUR SUITABILITY MAPS

Suitability<-raster("RSF_AllLeks_Newdata.tif", RAT = TRUE)

RSFraster<-raster("RSF_AllLeks_Newdata.tif")


