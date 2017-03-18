# =============
# Task 1

# Load data file, ignoring the missing header row
wine <- read.csv("C:/Users/dh/Code/datamining/data/wine.csv", header = TRUE)

# View the data
View(wine)
summary(wine)

# Check for missing data in all rows
wine[!complete.cases(wine),]

# Remove first column (label)
wineWithoutLabels <- wine[ -c(1) ]

# Run PCA
pcaResults <- prcomp(wineWithoutLabels)

# Show elbow plot of PCA
plot(pcaResults, type = "l")

# Show the weightings of each PC
print(pcaResults)

# View weightings of first two PCs
print(pcaResults$rotation[,1])
print(pcaResults$rotation[,2])

# Define "original data" colour palette
palette(c("red", "green", "blue"))

# View graph with colours as predefined classes
pcaResults$colour <- as.factor(wine[,1])
plot(pcaResults$x[,1], pcaResults$x[,2], col=pcaResults$colour, pch=16, xlim=c(-1000,500), ylim=c(-60,30))

# Copy the PCA values into wine data set
winePCA <- cbind(wine, pcaResults$x[,1:2])

# Run K-Means clustering with the PCA values
clusterResults <- kmeans(winePCA[, 15:16], 3)

# Change colour palette
palette(c("orange", "magenta", "cyan"))

# Plot PCAs with colours coming from the clustering result
pcaResults$cluster <- as.factor(clusterResults$cluster)
plot(pcaResults$x[,1], pcaResults$x[,2], col=pcaResults$cluster, pch=16, xlim=c(-1000,500), ylim=c(-60,30))

# Store calculated cluster in wine data
winePCA$cluster <- clusterResults$cluster

# Display each cluster with the colour coming from its original class
palette(c("red", "blue", "green"))

cluster1 <- winePCA[winePCA$cluster == 1,]
plot(cluster1$PC1, cluster1$PC2, col=cluster1$class, pch=16, xlim=c(-1000,500), ylim=c(-60,30))

cluster2 <- winePCA[winePCA$cluster == 3,]
plot(cluster2$PC1, cluster2$PC2, col=cluster2$class, pch=16, xlim=c(-1000,500), ylim=c(-60,30))

cluster3 <- winePCA[winePCA$cluster == 2,]
plot(cluster3$PC1, cluster3$PC2, col=cluster3$class, pch=16, xlim=c(-1000,500), ylim=c(-60,30))

clusterResults

# Within cluser sum of squares by cluster comes out as 86.5%. Define what this is, compare a few
# runs, cluster cohesion in the slides

# ============
# Task 1.2

# Create new scaled data frame by removing labels, scaling, then re-adding labels
# Otherwise we scale categories and it all breaks
wine12WithoutLabels <- as.data.frame(scale(wineWithoutLabels))
wine12 <- cbind(class=wine$class, wine12WithoutLabels)

# Run PCA
pcaResults12 <- prcomp(wine12WithoutLabels)

# Show elbow plot of PCA
plot(pcaResults12, type = "l")

# Define "original data" colour palette
palette(c("red", "green", "blue"))

# View graph with colours as predefined classes
pcaResults12$colour <- as.factor(wine[,1])
plot(pcaResults12$x[,1], pcaResults12$x[,2], col=pcaResults12$colour, pch=16, xlim=c(-5,5), ylim=c(-5,5))

# Copy the PCA values into wine12 data set
wine12PCA <- as.data.frame(cbind(class=wine12$class, pcaResults12$x[,1:2]))

# Run K-Means clustering with the PCA values
cluster12Results <- kmeans(wine12PCA[, 2:3], 3)

# Change colour palette
palette(c("orange", "magenta", "cyan"))

# Plot PCAs with colours coming from the clustering result
pcaResults12$cluster <- as.factor(cluster12Results$cluster)
plot(pcaResults12$x[,1], pcaResults12$x[,2], col=pcaResults12$cluster, pch=16, xlim=c(-5,5), ylim=c(-5,5))

# Store calculated cluster in wine12 data
wine12PCA <- cbind(wine12PCA, cluster=cluster12Results$cluster)

# Display each cluster with the colour coming from its original class
palette(c("red", "green", "blue"))

summary(wine12PCA)

cluster121 <- wine12PCA[wine12PCA$cluster == 3,]
plot(cluster121$PC1, cluster121$PC2, col=cluster121$class, pch=16, xlim=c(-5,5), ylim=c(-5,5))

cluster122 <- wine12PCA[wine12PCA$cluster == 2,]
plot(cluster122$PC1, cluster122$PC2, col=cluster122$class, pch=16, xlim=c(-5,5), ylim=c(-5,5))

cluster123 <- wine12PCA[wine12PCA$cluster == 1,]
plot(cluster123$PC1, cluster123$PC2, col=cluster123$class, pch=16, xlim=c(-5,5), ylim=c(-5,5))

cluster12Results
# Within cluser sum of squares by cluster comes out as 79.7%. Define what this is, compare a few
# runs, cluster cohesion in the slides
