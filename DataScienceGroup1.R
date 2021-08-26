#CLUSTERING PROBLEM 
#R: R-4.0.5-win   R Studio: Version 1.1.463

#Importing Libraries
library(ggplot2)
library(gridExtra)
library(GGally)
library(lattice)
library(caret)
library(data.table)
library(ggpubr)
library(tidyverse)

#importing the data
shoppers.df <- read.csv("Assign1.csv")
str(shoppers.df)

#Check if there are missing values
which(is.na(shoppers.df[ , 1:18]))

#Convert categorical variables to numerical
shoppers.df.num <- shoppers.df
columns <-c("Month", "VisitorType", "Weekend", "Revenue")
shoppers.df.num[,columns] <- lapply(shoppers.df.num[,columns], 
                                    function(x) as.numeric(as.factor(x)))

#check the dataframe to see if the conversion was successful
str(shoppers.df.num)

# A. start =========================================================================
#Visualize data to see outliers
boxplot(shoppers.df.num)

#second visualisation
plot(shoppers.df.num)

#Visualizing Types of Pages Visited and Their Respective Duration

plot1 <- ggplot(shoppers.df, aes(x=1, y=Administrative)) + geom_violin() + geom_violin(trim=FALSE, fill='#E69F00', color='gray') + coord_flip() + labs(x = " ") + labs(y = "Number of Administrative pages visited") + theme(axis.text.y = element_blank(), axis.ticks = element_blank())
plot2 <- ggplot(shoppers.df, aes(x=1, y=Administrative_Duration)) + geom_violin() + geom_violin(trim=FALSE, fill='#E69F00', color='gray') + coord_flip() + labs(x = " ") + labs(y = "Total time spent in Administrative pages") + theme(axis.text.y = element_blank(), axis.ticks = element_blank())
plot3 <- ggplot(shoppers.df, aes(x=1, y=Informational)) + geom_violin() + geom_violin(trim=FALSE, fill='#56B4E9', color='gray') + coord_flip() + labs(x = " ") + labs(y = "Number of Informational pages visited") + theme(axis.text.y = element_blank(), axis.ticks = element_blank())
plot4 <- ggplot(shoppers.df, aes(x=1, y=Informational_Duration)) + geom_violin() +  geom_violin(trim=FALSE, fill='#56B4E9', color='gray') + coord_flip() + labs(x = " ") + labs(y = "Total time spent in Informational pages") + theme(axis.text.y = element_blank(), axis.ticks = element_blank())
plot5 <- ggplot(shoppers.df, aes(x=1, y=ProductRelated)) + geom_violin() + geom_violin(trim=FALSE, fill='#FF9999', color='gray') + coord_flip() + labs(x = " ") + labs(y = "Number of ProductRelated pages visited") + theme(axis.text.y = element_blank(), axis.ticks = element_blank())
plot6 <- ggplot(shoppers.df, aes(x=1, y=ProductRelated_Duration)) + geom_violin() + geom_violin(trim=FALSE, fill='#FF9999', color='gray') + coord_flip() + labs(x = " ") + labs(y = "Total time spent in ProductRelated pages") + theme(axis.text.y = element_blank(), axis.ticks = element_blank())
grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, nrow = 3, ncol = 2)

#Visualizing The PagesVisted and Duration against Revenue.
plot7 <- ggplot(shoppers.df, aes(x=Revenue, y=Administrative)) + geom_violin() + geom_violin(trim=FALSE, fill='#E69F00', color='gray') + labs(x = "Administrative") + labs(y = " ") + theme(axis.text.y = element_blank(), axis.ticks = element_blank())
plot8 <- ggplot(shoppers.df, aes(x=Revenue, y=Administrative_Duration)) + geom_violin() + geom_violin(trim=FALSE, fill='#E69F00', color='gray') + labs(x = "Administrative_Duration") + labs(y = " ") + theme(axis.text.y = element_blank(), axis.ticks = element_blank())
plot9 <- ggplot(shoppers.df, aes(x=Revenue, y=Informational)) + geom_violin() + geom_violin(trim=FALSE, fill='#56B4E9', color='gray') + labs(x = "Informational") + labs(y = " ") + theme(axis.text.y = element_blank(), axis.ticks = element_blank())
plot10 <- ggplot(shoppers.df, aes(x=Revenue, y=Informational_Duration)) + geom_violin() +  geom_violin(trim=FALSE, fill='#56B4E9', color='gray') + labs(x = "Informational_Duration") + labs(y = " ") + theme(axis.text.y = element_blank(), axis.ticks = element_blank())
plot11 <- ggplot(shoppers.df, aes(x=Revenue, y=ProductRelated)) + geom_violin() + geom_violin(trim=FALSE, fill='#FF9999', color='gray') + labs(x = "ProductRelated") + labs(y = " ") + theme(axis.text.y = element_blank(), axis.ticks = element_blank())
plot12 <- ggplot(shoppers.df, aes(x=Revenue, y=ProductRelated_Duration)) + geom_violin() + geom_violin(trim=FALSE, fill='#FF9999', color='gray') + labs(x = "ProductRelated_Duration") + labs(y = " ") + theme(axis.text.y = element_blank(), axis.ticks = element_blank())
grid.arrange(plot7, plot8, plot9, plot10, plot11, plot12, nrow = 2, ncol = 3)

#Visualizing BounceRates, ExitRates and PageValues
plot13 <- ggdensity(shoppers.df, x = "BounceRates", fill = "thistle2", color = "thistle2", add = "median", rug = TRUE) + labs(y = " ")
plot14 <- ggdensity(shoppers.df, x = "ExitRates", fill = "skyblue1", color = "skyblue1", add = "median", rug = TRUE) + labs(y = " ")
plot15 <- ggdensity(shoppers.df, x = "PageValues", fill = "sienna3", color = "sienna3", add = "median", rug = TRUE) + labs(y = " ")
grid.arrange(plot13, plot14, plot15, nrow = 3)

#Visualing the Special Day variable and its significance to Revenue
plot16 <- ggplot(shoppers.df, aes(x = factor(1), y = SpecialDay)) + geom_boxplot(width = 0.4, fill = "white") + geom_jitter(color = "deepskyblue4", width = 0.1, size = 1, alpha=0.4) + labs(x = "Special Day") + labs(y = "Closeness") + theme(axis.text.x = element_blank(), axis.ticks = element_blank())
plot17 <- ggplot(shoppers.df, aes(x = Revenue, y = SpecialDay)) + geom_boxplot(width = 0.4, fill = "white") + geom_jitter(color = "deepskyblue4", width = 0.2, size = 1, alpha=0.4) + labs(x = "Special Day") + labs(y = " ") + theme(axis.ticks = element_blank())
grid.arrange(plot16, plot17, ncol = 2)

#Visualizing variable Month, OperatingSytems, Browser, Region, TrafficType, Weekend and  VisitorType against Revenue
plot18 <- ggplot(data.frame(shoppers.df), aes(Month, fill=Revenue)) + geom_bar() + labs(x = "Month") + labs(y = " ")
plot19 <- ggplot(data.frame(shoppers.df), aes(OperatingSystems, fill=Revenue)) + geom_bar() + labs(x = "Operating Systems") + labs(y = " ") + scale_x_continuous(breaks = 1:8)
plot20 <- ggplot(data.frame(shoppers.df), aes(Browser, fill=Revenue)) + geom_bar() + labs(x = "Browser") + labs(y = " ") + scale_x_continuous(breaks = 1:13)
plot21 <- ggplot(data.frame(shoppers.df), aes(Region, fill=Revenue)) + geom_bar() + labs(x = "Region") + labs(y = " ") + scale_x_continuous(breaks = 1:9)
plot22 <- ggplot(data.frame(shoppers.df), aes(TrafficType, fill=Revenue)) + geom_bar() + labs(x = "Traffic Type") + labs(y = " ")
plot23 <- ggplot(data.frame(shoppers.df), aes(Weekend, fill=Revenue)) + geom_bar() + labs(x = "Weekend") + labs(y = " ")
plot24 <- ggplot(data.frame(shoppers.df), aes(VisitorType, fill=Revenue)) + geom_bar() + labs(x = "Visitor Type") + labs(y = " ") + scale_x_discrete(labels = c("New_Visitor" = "New", "Other" = "Other", "Returning_Visitor" = "Return"))
grid.arrange(plot18, plot19, plot20, plot21, plot22, plot23, plot24, nrow = 4, ncol = 2)

#Revenue Distribution
plot25 <- ggplot(data.frame(shoppers.df$Revenue), aes(x=shoppers.df$Revenue)) + geom_bar() + labs(x = "Revenue Distribution")
plot25
# A.end ===========================================================================

# B. start =========================================================================
#Normalizing the data
shoppers.df.norm <- sapply(shoppers.df.num, scale)

#Distance matrix between each record
eucl.dist <- dist(shoppers.df.norm[,c(1:18)], method = "euclidean")

#hierachical clustering with ward method
hclust.ward.eucl <- hclust(eucl.dist, method = "ward.D")
plot(hclust.ward.eucl, hang = -1, ann = FALSE)

#hierachical clustering with ward method and maximum distance
max.dist <- dist(shoppers.df.norm[,c(1:18)], method = "maximum")
hclust.ward.max <- hclust(max.dist, method = "ward.D")
plot(hclust.ward.max, hang = -1, ann = FALSE)

#hierachical clustering with ward method and manhattan distance
manh.dist <- dist(shoppers.df.norm[,c(1:18)], method = "manhattan")
hclust.ward.manh <- hclust(manh.dist, method = "ward.D")
plot(hclust.ward.manh, hang = -1, ann = FALSE)

#hierachical clustering with ward method and canberra distance
canb.dist <- dist(shoppers.df.norm[,c(1:18)], method = "canberra")
hclust.ward.canb <- hclust(canb.dist, method = "ward.D")
plot(hclust.ward.canb, hang = -1, ann = FALSE)

#hierachical clustering with ward method and minkowski distance
mink.dist <- dist(shoppers.df.norm[,c(1:18)], method = "minkowski")
hclust.ward.mink <- hclust(mink.dist, method = "ward.D")
plot(hclust.ward.mink, hang = -1, ann = FALSE)

hclust.k = 3

# code to see the clusters with distinct color
suppressPackageStartupMessages(library(dendextend))
avg_dend_obj <- as.dendrogram(hclust.ward.canb)
avg_col_dend <- color_branches(avg_dend_obj, k = hclust.k)
plot(avg_col_dend)
# B. end ============================================================================

# C. start ============================================================================
# Cut tree into k clusters
clusters <- cutree(hclust.ward.eucl, k = hclust.k)

# Number of members in each cluster
table(clusters)

# function to find centroid in each cluster
clust.centroid = function(i, dat, clusters) {
  ind = (clusters == i)
  colMeans(dat[ind,])
}
sapply(unique(clusters), clust.centroid, shoppers.df.num, clusters)
# C. end ============================================================================

# D. start ==========================================================================
#Taking random sample of 95% of the records
set.seed(1)
shoppers.norm.new <- shoppers.df.norm[sample(nrow(shoppers.df.norm),replace=F,
                                             size=0.95*nrow(shoppers.df.norm)),]
d.norm.new <- dist(shoppers.norm.new[,c(1:18)], method = "euclidean")

# Hierarchical clustering using Ward.D method
hclust.ward.new <- hclust(d.norm.new, method = "ward.D")
plot(hclust.ward.new, hang = -1, ann = FALSE)
suppressPackageStartupMessages(library(dendextend))
avg_dend_obj <- as.dendrogram(hclust.ward.new)
avg_col_dend <- color_branches(avg_dend_obj, k = hclust.k)
plot(avg_col_dend)
# D. end ============================================================================

# E. start ==========================================================================
# Elbow method
library(factoextra)
set.seed(123)
fviz_nbclust(shoppers.df.norm, hcut, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)+
  labs(subtitle = "Elbow method")

# Silhouette method
set.seed(124)
fviz_nbclust(shoppers.df.num, hcut, method = "silhouette")+
  labs(subtitle = "Silhouette method")


# Gap statistic
start_time <- Sys.time()
set.seed(125)
fviz_nbclust(shoppers.df.norm, hcut, nstart = 25,  method = "gap_stat", nboot = 50)+
  labs(subtitle = "Gap statistic method")
end_time <- Sys.time()
time_taken <- end_time - start_time
print(time_taken) 
# E. end ============================================================================

# F. start ==========================================================================
kmean.k = 3
# normalized distance:
shoppers.df.norm <- sapply(shoppers.df.num, scale)

# run kmeans algorithm 
set.seed(5)
km <- kmeans(shoppers.df.norm, kmean.k)
# F. end ============================================================================

# G. start ==========================================================================
# centroids
t(km$centers)

#code for plotting profile plot of centroids
library(MASS)
parcoord(km$centers, c('red', 'green', 'blue'), var.label = TRUE)
# G. end ============================================================================

# H. start ==========================================================================
# cluster size
km$size
# H. end ============================================================================
