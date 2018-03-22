set.seed(1)
Demographic = read.csv("Demographic.csv")
States = row.names(Demographic)
States#We see that the row names are 1,2,3 etc.
row.names(Demographic) = Demographic$StateUT #Giving the row names as the stateUT
Demographic = Demographic[,-1]#Removing the column StateUT

States = row.names(Demographic)
States#We see that the row names are 1,2,3 etc.

summary(Demographic)
apply(Demographic,2,mean)
apply(Demographic,2,var)

names(Demographic)
demographic1 = Demographic[,-c(10,11,13)]#Removed the columns "Under5MortalityRate",MMR and MeanAgeAtEffectiveMarriage.Females.
#We have removed these columns as there were about 17 missing values which constitutes to 50% of data

#again we see that there is only 1 missing value in the column Contraceptive. Therefor we impute it with mean of the column
#demographic1[is.na(demographic1$ContraceptiveUse...),demographic1$ContraceptiveUse...] = mean(demographic1$ContraceptiveUse...,na.rm = TRUE)
#demographic1["Nagaland",11]=mean(demographic1$ContraceptiveUse...)
pr.out = prcomp(demographic1,scale = TRUE)


names(pr.out)
pr.out$scale
pr.out$x
pr.out$center
pr.out$sdev
pr.out$rotation
biplot(pr.out,scale=0)

#pr.out$rotation = -pr.out$rotation
#pr.out$x = -pr.out$x
#biplot(pr.out,scale = 0)

pr.var = pr.out$sdev^2
pr.var
pve = pr.var/sum(pr.var)
pve

plot(pve,xlab = "Principal Component", ylab = "Proportion of variance Explained", ylim = c(0,1),type = 'b')
plot(cumsum(pve),xlab = "Principal component", ylab = "Cumulative prportion of variance explained", ylim = c(0,1), type = 'b')

pr.out$rotation[,1:4]


#Running KNN on the above data with Under5MortalityRate",MMR and MeanAgeAtEffectiveMarriage.Females." columns removed
nd = scale(demographic1)
?kmeans



## Determine number of clusters
Cluster_Variability <- matrix(nrow=5, ncol=1)
for (i in 1:5) Cluster_Variability[i] <- kmeans(nd,centers=i, nstart=10)$tot.withinss
plot(1:5, Cluster_Variability, type="b", xlab="Number of clusters", ylab="Within groups sum of squares") ## Elbow curve or Scree plot

#The elbow is formed at 3, so we will create 3 clusters using k means
km.out = kmeans(nd,centers=3,nstart = 20, iter.max = 2000)
km.out
names(km.out)
km.out$cluster
plot(nd,col=(km.out$cluster+1),main = "K Means Clustering reults with k= 4",xlab = "",ylab = "",pch=20,cex=4)
km.out$tot.withinss
km.out$betweenss
demographic2 = demographic1
demographic2$Cluster = km.out$cluster
table(demographic2$Cluster)
demographic2$StateUT = row.names(demographic2)
row.names(demographic2) = 1:nrow(demographic2)
write.csv(demographic2,"demographic2.csv")
