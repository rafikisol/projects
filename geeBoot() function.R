---
#'@title geeBoot() function
#'@author Trevan Flynn
#'@description Spatial downscaling of categorical soil data with GEE and R through bootstrapping.
#'@imports: rgee and tidyerse
#'@param  image An ee$Image with each band a covariate.
#'@param  model  A character of a GEE model, either random forest ("rf"), gradient tree boost ("gtb"), CART ("cart") or SVM ("svm").
#'@param  stype Character of either stratified ("s") or random ("r") sampling which defaults to "s".
#'@param  sampNum An integer specifying the number of samples for each bootstrap. For "s", its the number of samples in each strata and for "r" is the total number of samples.
#'@param  boot Is an integer number of bootstraps to conduct.
#'@param  ... Is any parameter of GEE models (see GEE docs) and if multiple arguments are needed, must be a list.
#'@return A list of the modal predictions as well as model and samples of each bootstrap.
#'@section references:
#'Brendan P. Malone, Alex B. McBratney, Budiman Minasny, Ichsani Wheeler, (2012). A general method for downscaling earth resource information. Computers & Geosciences, Volume 41, Pages 119-125. https://doi.org/10.1016/j.cageo.2011.08.021.
#'P. Roudier, B.P. Malone, C.B. Hedley, B. Minasny, A.B. McBratney, (2017). Comparison of regression methods for spatial downscaling of soil organic carbon stocks maps. Computers and Electronics in Agriculture. Volume 142, Part A, Pages 91-100. https://doi.org/10.1016/j.compag.2017.08.021.
#'@export

#'geeBoot() function:
#'function
geeBoot = function(image, model = 'rf', sType = 'r', sampNum = 100, boot = 10,...){
  
  #'labels of lists (we want them all)
  samps = list()
  
  #'define which model 
  if(model == "nb"){
    model = ee$Classifier$smileNaiveBayes
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
  else{model = ee$Classifier$smileRandomForest
  }
  
  #'loop around
  for(i in 1:boot) {
    #'stratified samples
    if(sType == "s"){
      samps[[i]] = image$stratifiedSample(
        numPoints = sampNum,
        classBand = label,
        region = region,
        geometries = T,
        scale = scale)
    }
    #'random sampling
    else{
      samps[[i]] = image$sample(
        region = region,
        scale = scale,
        numPixels = sampNum,
        geometries = T)
    }
  }
  
  #'function to build classifier and traing
  f1 = function(x) {model(...)$
          train(features = x,
            classProperty = label,
            inputProperties = colabs)}
  mods = lapply(samps, f1)
  
  #'predict each model over covariate
  f2 = function(mods) image$classify(mods, "predictions")
  preds = lapply(mods, f2)
    
  #'get mode of the imageCollection
  predModal = ee$ImageCollection(preds)$
    mode()
  
  #'names list for ease of use
  return(list(predModal, mods, samps)%>%
           setNames(c("predictions", "models", "samples")))
}
#' END
---
