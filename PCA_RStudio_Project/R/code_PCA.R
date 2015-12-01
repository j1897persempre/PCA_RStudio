######################################################################
                     #PRINCIPAL COMPONENT ANALYSIS
######################################################################


###############PRCOMP###############################
data(iris)  #loading the IRIS dataset. contains 150 observations of 5 variables
head(iris)  #Column 1, 2, 3 and 4 are the variables required for PCA

#applying PCA using prcomp
iris.pca <- prcomp(iris[,1:4], center = T, scale.= T) #perfomrs PCA. Data is centered and scaled
plot(iris.pca, type="l", col="blue", lwd=2, main="Scree Plot") #screeplot
names(iris.pca) #displays the lists in the object iris.pca
iris.pca$sdev #standard deviations of principal components
iris.pca$rotation #the matrix of variable loadings. The columns contain eigenvectors

eig <- iris.pca$sdev^2 #eigenvalues of the principal components
variance <- eig*100/sum(eig) #variance explained by each of PCs
cumvar <- cumsum(variance) #cumulative variance

iris.scaled <- scale(iris[,1:4], center = TRUE, scale = TRUE) #centering and scaling the raw data
newData1 <- as.data.frame(t(t(iris.pca$rotation)%*%t(iris.scaled))) #new data after PCA

#creating a dataframe with eigenvalues, variance and cumulative variance of PCA
iris.eig <- data.frame(eig = eig, variance = variance,cumvariance = cumvar)

#some plots
par(mar = c(6,4,4,2)+0.1)
barplot(iris.eig[,2],names.arg=1:nrow(iris.eig), main="Variances", 
        xlab="Principal Components",ylab="Percentage of Variances", col="steelblue",
        space=0.5)
barplot(iris.eig[,3],names.arg=1:nrow(iris.eig), main="Cumulative Variances", 
        xlab="Principal Components",ylab="Percentage of Cumulative Variances", 
        col="steelblue", space = 0.5)
library(corrplot) #library for visualising correlation matrix
corrplot(cor(iris[,1:4]), method="color") #correlation matrix of raw data
corrplot(cor(newData), method="color") #correlation matrix of the data after PCA

####################END OF PRCOMP########################################

########################### PRINCOMP #####################################
iris.princomp <- princomp(iris[,1:4], cor=T)
names(iris.princomp) #displays the list in the object iris.princomp
iris.princomp$sdev #standard deviations of PCs
iris.princomp$loadings
newData2 <- as.data.frame(iris.princomp$scores, 
                          colnames(c("PC1","PC2","PC3","PC4"))) #new data after PCA

####################### END OF PRINCOMP ###################################


################### PCA from FactoMineR library  ##########################
library(FactoMineR)
iris.MineR <- PCA(iris[,1:4], scale.unit = T, graph=F)
names(iris.MineR)
iris.MineR$eig #eigenvalues, variances and cumulative variances
dat <- as.data.frame(iris.MineR$ind)
newData3 <- dat[,1:4] #new data after PCA
################### END OF PCA from FactoMineR Library#####################

summary(newData1) #from prcomp
summary(newData2) #from princomp
summary(newData3) #from PCA

##COMPARING SAS PRINCOMP AND R PRCOMP OR PRINCOMP
library("haven") #for reading SAS data file into R
crime <- read_sas("C:/Acads/Datasets/crime.sas7bdat") #reading SAS data into R
summary(crime)
crime.pca <- prcomp(crime[,2:8], center = T, scale. = T)
crime.princomp <- princomp(crime[,2:8], cor = T)
crime.pca$rotation #the principal components from prcomp
crime.princomp$loadings #the principal components from princomp
#the above PCs looks identical to the output from SAS PROCS PRINCOMP.
#Please refer the SAS documentation of SAS PROCS PRINCOMP v9.2 