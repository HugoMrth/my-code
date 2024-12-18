library(rgl)

data(mtcars)
data_cars <- mtcars[,-(8:9), 
                    drop=FALSE]
 head(data_cars)


pca_cars <- prcomp(data_cars, 
                   scale=TRUE)
 
summary(pca_cars)



scores = as.data.frame(pca_cars$x)
 
head(scores[1:4])


#### Plot PCA in 3D
plot3d(scores[,1:3], 
       size=5,
       col = seq(nrow(scores)))
 
text3d(scores[,1:3],
       texts=c(rownames(scores)), 
       cex= 0.7, pos=3)


#### Add Biplot to 3D Plot

plot3d(scores[,1:3])
 
text3d(scores[,1:3],
       texts=rownames(data_cars),
       cex=0.8)
 
text3d(pca_cars$rotation[,1:3], 
       texts=rownames(pca_cars$rotation), 
       col="red", 
       cex=0.8)
 
coords <- NULL
for (i in 1:nrow(pca_cars$rotation)) {
  coords <- rbind(coords, 
                  rbind(c(0,0,0),
                                pca_cars$rotation[i,1:3]))
}
 
lines3d(coords, 
        col="red", 
        lwd=1)
