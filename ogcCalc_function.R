#Title: Oblique Geo Coords with GEE and R
#Author: Trevan Flynn
#Date: 29/05/2023
#
#Description: Function to create Oblique geographic coordinates (OGC) in R from 
#a Feature/FeatureCollection (or shapefile convered to ee$Object) with Google Earth Engine. 
#This function is basically all taken from Anders Bjørn Møller and was developed 
#in Møller et al., (2020). Therefore, their paper should be cited if this code is used. 
#
#This code consists of two function both of which require rgee package installed. The first 
#function calculates x and y from a region (xyImage) and the second calculates the OGC (ogcCalc) image.

---
##Function 1: xyImage() = create an Image with x and y from desired coordinate system and scale.
#Inputs:#area = ee$Feature, ee$Geometry, ee$Object (seems to work with ee$FeatureCollection too).
        #crs = coordinate system to produce image in.
        #scale = resolution to set image to. 
  
#function to get an image of coordinates from FeatureCollection
xyImage = function(area, crs = "EPSG: 4326", scale = 250){
  
  #create x and y raster
  img = ee$Image$pixelCoordinates(crs)$ #x and y from coordinates
    clip(area)$ #clip to region
    reproject(crs =crs, scale = scale)$ #sample to scale desired
    resample('bilinear') #not neccessary (I prefer bilinear over nearest neighbor)
  
  return(img)
}

---
##Function 2: ogcCalc() = Calculates OGC from xyImage() function and combines all images.
#Inputs:#xyImg = Image from xyImage() function
        #nr = number of angles to use to calculate OGC.
  
#Function
ogcCalc = function(xyImg, nr = 6){
  
  #get coordinates and other parameters (mostly in R code)
  x = xyImg$select('x') #extract longitude
  y = xyImg$select('y') #extract latitude
  
  #angles
  pis <- pi*seq(0, 1, 1/nr)[2:(nr)] #sequence of angles
  
  #Make a list for each image calculation
  const = list() #constant images
  c1 = list() #first calculation
  c2 = list() #second calculation
  ogc = list()#list of calculated coordinates
  
  #need to edit locations for even numbers 
  is.wholenumber <-function(x, tol = .Machine$double.eps^0.5)
    abs(x - round(x)) < tol
  
  if(is.wholenumber(nr/2)) {
    const[[(nr/2) + 1]] <- y
    pis <- pis[ -(nr/2)]
  }
  
  #can now calculate the images for each angle (back to GEE mostly)
  for(i in seq_along(pis)){ 
    
    const[[i]] = ee$Image$constant(ee$Number(pis[i]))$
      clip(region)
    
    #Calculation 1
    c1[[i]] = (const[[i]]$subtract((y$divide(x))$atan()))$cos()
    
    #calculation 2
    c2[[i]] = (x$pow(2)$add(y$pow(2)))$sqrt()
    
    #calculation 3
    ogc[[i]] = c1[[i]]$multiply(c2[[i]])
  }
  
  #create labels (cant have "." in GEE names so substitute out for "_" or just make sure there are no decimals)
  labs =  paste0("pi", format(seq(0, 1, 1/nr)[1:nr], digits = 1, nsmall = 2))%>%
    gsub("[.]", "_",.)
  
  #make into an image
  ogc = ee$ImageCollection(ogc)$
    toBands()
  
  #add xy back in (otherwise names will be wrong)
  ogc = xyImg$select(0)$ #longitude
    addBands(ogc)$ #calculated image
    addBands(xyImg$select(1))$ #latitude
    rename(labs)
  
  return(ogc)
}

---
#apply functions
#1
img = xyImage(region, crs = "EPSG: 3857", scale = 30)

#2
ogcImg = ogcCalc(img)

#plot
Map$addLayer(ogcImg)

---
#REFERENCE 

#Møller, A. B., Beucher, A. M., and Pouladi, N., Greve, M. H. (2020) Oblique geographic coordinates as covariates for digital soil mapping, SOIL 6(2) 269 - 289 https:/10.5194/soil-6-269-2020