library(imager)

#Cargar imagen original
img = load.image('DSC_0090.JPG')

#Pasar a escala de grises ()
greyImg = img[,,1]+img[,,2]+img[,,3]
greyImg = greyImg / max(greyImg)
rm(img) #in order to save memory

# Specify number of clusters
k = 3

# Run k-means
set.seed(1)
greyImgVector = as.vector(greyImg)
KMC = kmeans(greyImgVector, centers = k, iter.max = 1000)

clusters = KMC$cluster
dim(clusters) = c(nrow(greyImg), ncol(greyImg))

image(clusters, axes = FALSE, col=rainbow(k))
