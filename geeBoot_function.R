---
#Title: geeBoot() function
#Author: Trevan Flynn
#Date: 29/05/2023
#Description: Spatial downscaling of categorical soil data with GEE and R through bootstrapping
#Requirements: rgee and tidyerse
#Inputs:  #image = ee$Image with each band a covariate.
          #Model = GEE model, either random forest ("rf"), gradient tree boost ("gtb"), CART ("cart") or SVM ("svm").
          #sType = sample type, either stratified ("s") or random ("r") and defaults to "s".
          #sampNum = number of samples for each bootstrap. For "s", its the number of samples in each strata and for "r" is the total number of samples.
          #boot = number of bootstraps to conduct.
          #... = any parameter of GEE models (see GEE docs).
#Ouputs: A list of 3
        #[[1]] = modal predictions for all bootstraps
        #[[2]] = model of each bootstrap
        #[[3]] = samples from each bootstrap

---
#geeBoot() function:
geeBoot = function(image, model = 'gtb',sType = "s", sampNum = 100, boot = 10,...){
  
  #labels of lists (we want them all)
  samps = list()
  mods = list()
  preds = list()
  
  #define which model 
  if(model == "rf"){
    model = ee$Classifier$smileRandomForest
  }
  
  if(model == "gtb"){
    model = ee$Classifier$smileGradientTreeBoost
  }
  
  if(model == "svm"){
    model = ee$Classifier$libsvm
  }
  
  if(model == "cart"){
    model = ee$Classifier$smileCart
  }
  
  if(model == "nb"){
    model = ee$Classifier$smileNaiveBayes
  }
  
  #loop around
  for(i in 1:boot) {
    
    #stratified samples
    if(sType == "s"){
      samps[[i]] = image$stratifiedSample(
        numPoints = sampNum,
        classBand = label,
        region = region,
        geometries = T,
        scale = scale)
    }
    
    #random sampling
    if(sType == "r"){
      samps[[i]] = image$sample(
        region = region,
        scale = scale,
        numPixels = sampNum,
        geometries = T)
    }
    
    #train models on each bootstap
    mods[[i]] = model(...)$
      train(features = samps[[i]],
            classProperty = label,
            inputProperties = colabs)
    
    #predict each model over covariate
    preds[[i]] = image$classify(mods[[i]], "pred")
    
    #get mode of the imageCollection
    predModal = ee$ImageCollection(preds)$
      mode()
  }
  
  #names list for ease of use
  return(list(predModal, mods, samps)%>%
           setNames(c("predictions", "models", "samples")))
}

#apply function
preds = geeBoot(image, model = 'gtb', sampNum = 50, numberOfTrees = 50)

#REFERENCES

#Brendan P. Malone, Alex B. McBratney, Budiman Minasny, Ichsani Wheeler, (2012). A general method for downscaling earth resource information. Computers & Geosciences, Volume 41, Pages 119-125. https://doi.org/10.1016/j.cageo.2011.08.021.

#P. Roudier, B.P. Malone, C.B. Hedley, B. Minasny, A.B. McBratney, (2017). Comparison of regression methods for spatial downscaling of soil organic carbon stocks maps. Computers and Electronics in Agriculture. Volume 142, Part A, Pages 91-100. https://doi.org/10.1016/j.compag.2017.08.021.