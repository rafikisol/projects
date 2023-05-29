#Title: Predicting soil pH
#Author: Trevan Flynn
#Date: 29.05.2023
#Description: This code predicts pH (H20) content of Otjozondi, Namibia  and data
#from WoSIS found at https//www.data.isric.com. Covariate data comwes from worldclim 2
# (Fick and Hijmans, 2017).

#Load libraries
library(sf) #spatial points to manipulate in r
library(tidyverse) #manipulate and tidy data
library(yardstick) #model evaluation
library(ggpubr) #extra for plotting
library(rgee) #GEE for R

#Initialize GEE in R
ee_Initialize()

#labels
label = "phaq_val_1" #response (how it is in the database)
scale = 30 #resolution of output

#Get Otjozondjupa province in Namibia
worldProvince = ee$FeatureCollection("FAO/GAUL_SIMPLIFIED_500m/2015/level1"); #admin of all provinces 
filterProvince = ee$Filter$eq('ADM1_NAME', 'Otjozondjupa'); #select province
region = worldProvince$filter(filterProvince); #filter to area

#load WoSIS data of pH (point observations = FeatureCollection)
collection = ee$FeatureCollection("projects/tre-projects/assets/WoSIS_pH_H20")$
  select(label)$#select pH 
  filterBounds(region) #clip to region

#check data by plotting
Map$centerObject(region, 6) #zoom to province
Map$addLayer(region) #province
#note, you can zoom and change background map.

#Obtain covariate image (climatic data - 19 bioclim variabes) and sample data from WoSIS collection. 
#Similar to extract in terra or stars package. We then add x and y to add neighborhood into the covariates.

#get worldclim bioclim variables (already in image format)
coStack = ee$Image("WORLDCLIM/V1/BIO")$#get climate data
  clip(region)$#clip to region
  addBands(ee$Image$pixelLonLat())#add x and y as covariates

#resample at resolution desired
coStack = coStack$
  reproject(crs = "EPSG: 3857", scale = scale)$ #sample to 30 m
  resample(mode = 'bilinear') #set resampling mode (can be nearest neighbor or bicubic as well)

#sample covariates at observations
samples = coStack$sampleRegions(
  collection = collection, #collection with response
  properties = list(label), #response
  scale = scale, #resolution
  geometries = T)$ #keep locations
  randomColumn() #add random column for making training/test set

#split data
training = samples$filter(ee$Filter$gt('random',0.2)) # 80% training
validation = samples$filter(ee$Filter$lte('random',0.2)) # 20% testing

#predictor variable names
bands = coStack$bandNames()

#build Regression classifier, must build the model and set the mode to “REGRESSION” before training the model.
#For gradient tree boosting (GTB) you only need to specify numberOfTrees to grow.

#Default values for GTB: 
  #-hrinkage = 0.005 (lower = less over fitting but slower)
  #samplingRate = 0.7 (bag fraction for each tree) 
  #maxNodes = Inf (max nodes in each tree)
  #loss = “LeastAbsoluteDeviation” (default), "LeastSquares" or "Huber"
  #seed = 0 (replicate results)

#build and train
rfReg= ee$Classifier$smileGradientTreeBoost( #model
  numberOfTrees = 500, #500 trees
  shrinkage = 0.1,#higher shrinkage = lower time needed (overfits easier)
  loss = "LeastSquares")$
  setOutputMode("REGRESSION")$ #(default = classification)
  train(features = training,
        classProperty = label,
        inputProperties = bands)

#predict trained RF model over covariate image
pHmap = coStack$classify(rfReg, "predictions")

#plot results (do not need to recenter image) 
Map$addLayer(pHmap, 
             list(min = 7, max = 9, palette = c("blue", "red",
                                                "yellow", "green",
                                                "grey"))) #color

#Evaluate predictions by predicting over the “validation” set (easiest to convert to sf if small area). 
#Can also sample the predictions with the “validation” set using pHmap$sameRegions(…). Both produce the same results.

#set metrics (a bunch in the yardstick library).
met = metric_set(rmse, rsq, ccc) #lins coefficient, R2 and RMSE 

#predict and convert to sf object (can take some time)
eval = validation$classify(rfReg, "predictions")$ #predict
  select(label, "predictions")%>% #select response and predictions
  ee_as_sf(maxFeatures = 1e13) #set maxFeatures if a large amount of data.

#print evaluation results
eval %>%
  met(phaq_val_1, predictions)

## 1 rmse    standard       0.818
## 2 rsq     standard       0.527
## 3 ccc     standard       0.683

#plot reg line and other stats
#R2 and regline plot
ggplot(data = eval, aes(y = phaq_val_1, x = predictions))+
  geom_point()+
  geom_smooth(method = 'lm', se = F)+
  stat_cor(label.y= 9.5, aes(label = after_stat(rr.label)))+
  stat_regline_equation(label.y = 9)+
  labs(x = "Predictions", y = "Measured", title = "pH")+
  theme_bw()


#covariate importance for top 10
varImp = rfReg$explain()$get("importance")$getInfo()%>% #get importance
  as_tibble()%>%#make tibble
  pivot_longer(cols = c(1:ncol(.)))%>% #long df for plotting
  arrange(desc(value))%>% #sort decreasing values
  slice(1:10) #get top 10

#plot
ggplot(data = varImp, aes(x = reorder(name, -value), y = value))+
  geom_bar(stat ="identity", color = 'black', fill = 'blue')+
  labs(x = "Variable", y = "Importance", title = "Covariate importance")+
  theme_bw()

#Can get some statistics (both global and local) straight from GEE, however, this can be very slow. 
#Easier to do after the raster is saved or converted into memory. 
#But you can calculate statistics on validation data if you want (not suggested as sf is faster and easier).

#stats on validation set
pred = validation$classify(rfReg, "predictions")$
  select(c(label, "predictions"))

#Pearson's correlation coefficient
r = pred$reduceColumns(
  reducer = ee$Reducer$pearsonsCorrelation(), #reduce to 1 number
  selectors = list(label, "predictions")
)

#coefficient of determination
r2 = ee$Number(r$get("correlation"))$ #get correlation
  pow(2) #square it

#print
print(r2$getInfo())
# [1] 0.5273032


#RMSE is a bit more complicated and you need to write a function to get residuals for each row and then another function to calculate RMSE.
#get residuals
addResid <- function(feature) {
  res <- ee$Number(feature$get(label))$ #subtract observed from predicted
    subtract(ee$Number(feature$get("predictions")))
  feature$set(list(res = res)) #create new feature in featureCollection
}

#apply function to FeatureCollection for residuals
res = pred$map(addResid)

#calculate RMSE
rmse = ee$Array(res$aggregate_array("res"))$
  pow(2)$ #square it
  reduce("mean", list(0))$ #get mean for the residuals
  sqrt()

#print RMSE
print(rmse$getInfo())
#[1] 0.8183916

##To save or for further analysis, convert to stars object or export to google drive. Converting to stars will load into memory but can take a long time if resolution is high.
#Export
task = ee_image_to_drive(
  image = pHmap,
  description = "pHmap", #name of image
  folder = "rgee_backup", #folder to store in 
  scale = scale,
  region = region
)

#to send to GEE  - task$start() to save to drive or cloud
##END
---