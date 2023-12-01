library(terra)
library(raster)

raslist <- paste0('/home/tango/Files/Archives/academics/bits/GIS_proj/assets/exp_im20.tif')
landsat <- rast(raslist)
names(landsat) <- c('blue', 'green', 'red', 'NIR', 'SWIR1', 'SWIR2')


esri_lulc <- rast('/home/tango/Files/Archives/academics/bits/GIS_proj/assets/43R_20220101-20230101_clipped',1:7,'.tif')
names(esri_lulc) <- c('lulc2022')

lulc2022 <- esri_lulc[[1]]

lulcclass <- c("Builtup", "Barren", "Vegetation", "Water")
classdf <- data.frame(value = c(7,5,11,2), names = lulcclass)

levels(lulc2022)<- classdf
classcolor <- c("#5475A8", "#B50000", "#D2CDC0", "#38814E")

plot(lulc2022, col=classcolor)

samp2022 <- spatSample(lulc2022, size = 200, method="regular")
table(samp2022[,1])

df <- extract(landsat, ptsamp, ID=FALSE)

sampdata <- data.frame(class = ptsamp$class, df)

# TRAIN CLASSIFIER
cartmodel <- rpart(as.factor(class)~., data = sampdata, method = 'class', minsplit = 5)
print(cartmodel)

plot(cartmodel, uniform=TRUE, main="Classification Tree")
text(cartmodel, cex = 1)