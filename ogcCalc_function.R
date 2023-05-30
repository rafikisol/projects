#'@title Oblique Geo Coords with GEE and R
#'@author: Trevan Flynn
#'
#'@description A function to create Oblique geographic coordinates (OGC) in R from 
#'a Feature/FeatureCollection (or shapefile convered to ee$Object) with Google Earth Engine. It returns,
#'the original x and y coordinates with bands in the image. 
#'
#'@param area A ee$Object (or ee$Geometry, ee$Feauture, ee$FeatureCollection) on the area of interest.
#'@param crs The coordinate system to use (best if projected system).
#'@param scale The resolution of the final image.
#'@param nr The number of OGC to calculate (default = 6).
#'
#'@return An image with each band being OGC calculated at a different angle. 
#function to get an image of coordinates from FeatureCollection
#'
#@section References:
#'Møller, A. B., Beucher, A. M., and Pouladi, N., Greve, M. H. (2020) Oblique geographic coordinates as covariates for digital soil mapping, SOIL 6(2) 269 - 289 https:/10.5194/soil-6-269-2020
#'
#' ogcCalc function
ogcCalc = function(area, crs = "EPSG: 3857", scale = 250, nr = 6){
  
  #'create x and y raster
  img = ee$Image$pixelCoordinates(crs)$ #'x and y from coordinates
    clip(area)$ #'clip to region
    reproject(crs =crs, scale = scale)$
        resample("bilinear") #' Dont need to but I prefer bilinear in general. 
  
  #'select x ane y Images
  x = img$select("x")
  y = img$select("y")
  
  #'angles and position
  pis <- pi*seq(0, 1, 1/nr)[2:(nr)] #'sequence of angles
  pos <- seq(2, nr, 1)
  
  #'need to edit locations for even numbers 
  is.even <-function(x, tol = .Machine$double.eps^0.5){
    abs(x - round(x)) < tol
    }
  
  if(is.even(nr/2)) {
    pis <- pis[ -(nr/2)]
    pos <- pos[ -(nr/2)]
  }
  
  #'Make a list for each image calculation
  calc1 = list()
  calc2 = list()
  ogc = list() #'list of ogc
  
  #'calculation 1
  for(i in 1:length(pis)){
    #'get constant image of angles
    calc1[[pos[i]]] = ee$Image$constant(ee$Number(pis[i]))$
      clip(region)
    
    #'calc 1
    calc1[[pos[i]]] = calc1[[pos[i]]]$subtract((y$divide(x))$atan())$cos()
    
    #'calc 2
    calc2 = (x$pow(2)$add(y$pow(2)))$sqrt()
    
    #'calc 3
    ogc[[pos[i]]] = calc1[[pos[i]]]$multiply(calc2)
  }
  
  #'make sure to account for even numbers (will just be null otherwise)
  ogc[[1]] = x
  ogc[[(nr/2)+1]] = y
  
  #'create labels (cant have "." in GEE names so substitute out for "_" or just make sure there are no decimals)
  labs=  paste0("pi", format(seq(0, 1, 1/nr)[1:nr], digits = 1, nsmall = 2))%>%
    gsub("[.]", "_",.)
  
  #'add xy back in (otherwise names will be wrong)
  ogc = ee$ImageCollection(ogc)$
    toBands()$
    rename(labs)
  
  #'return OGC
  return(ogc)
  ##'END
}
