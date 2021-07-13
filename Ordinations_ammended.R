#Author: Viwe Mabongo
#Date: 13 July 2021
#Task: Ordinations

library(tidyverse)
library(vegan)
library (ade4)


# Question A --------------------------------------------------------------

#Principal component analysis are used  for reducing the dimensionality
#of such datasets, increasing interpretability but at
#the same time minimizing information loss. The ordination method is not a statistical test,
#that is why it does not explain variation in a dataset.

#We use PC1 and PC2 because those axes (Principal Components) are ordered by the % of variability they explain,
#being PC1 always the axis that explain more variability among the samples included in the test. 
#PC2 is the second axes explaining more variability, and so on.
#So, typically PC1 and PC2 will represent a large % of variability but never 100%.



# Question B --------------------------------------------------------------
ybirds.env <- read.delim ('https://raw.githubusercontent.com/zdealveindy/anadat-r/master/data/ybirds_env.txt', row.names = 1)

Env <- ybirds.env
Env <- dplyr::select(Env, -1, -2)
head(Env)


# Calculate the PCA -------------------------------------------------------


Env_pca <- rda(Env, scale = TRUE)
Env_pca

round(Env_pca$CA$eig[1], 3)

sum(Env_pca$CA$eig)

round(Env_pca$CA$eig[1] / sum(Env_pca$CA$eig) * 100, 1)

summary(Env_pca)


# The graphical representation of the ordinations -------------------------

par(mfrow = c(1, 2))
biplot(Env_pca, scaling = 1, main = "PCA scaling 1", choices = c(1, 2))
biplot(Env_pca, scaling = 2, main = "PCA scaling 2", choices = c(1, 2))



# A -----------------------------------------------------------------------

#In PC1 Ground Cover(GC) have higher positive value(1.21262) and Secondary Tree cover(T2C) have a
#smaller negative value(-1.12045), which means GC and T2C plays a crucial role in the differences 
#among the different sites.PC2 does not have much higher positive values, although Tree Diversity(TD),
#have a fairly high positive values compared to other variables.

#Shrub Cover(SC) does not have much influence on the difference among the sites.
# B -----------------------------------------------------------------------

# The reason there is high positive altitude in these bird communities is because they
#occur in the mountains of which mountains have high altitude by virtue of the height.


# C -----------------------------------------------------------------------
# There is negative correlation between Elevation and Tree Species diversity in PC1 because the,
#higher the elevation the lower there is tree species diversity, meaning sites with higher,
#elevation there will be lower tree species diversity.



# Alpine plant communities ------------------------------------------------


#Importing the file to R.
library(ade4)
env <- data(aravo)
env <- as_tibble(aravo[["env"]])

Env_av <- env
Env_av <- dplyr::select(Env_av, -5)# removing column 5 which in non-numeric.
head(Env_av)

Env_av$Form <- as.numeric(as.character(Env_av$Form))# changing the Form column to numeric value.


# Calculate the PCA -------------------------------------------------------


Env_av_pca <- rda(Env_av,scale = TRUE)
Env_av_pca

round(Env_av_pca$CA$eig[1], 3)

sum(Env_av_pca$CA$eig)

round(Env_av_pca$CA$eig[1] / sum(Env_av_pca$CA$eig) * 100, 1)

summary(Env_av_pca)


# Graphical representation ------------------------------------------------


par(mfrow = c(1, 2))
biplot(Env_av_pca, scaling = 1, main = "PCA scaling 1", choices = c(1, 2))
biplot(Env_av_pca, scaling = 2, main = "PCA scaling 2", choices = c(1, 2))


#This dataset the PC1 and PC2 are not giving a high percentage for us to decide the PCA, then
#PC3 will have to be considered when doing the analysis as it also contains high fraction.

#In PC1 Landform  have higher positive value(1.5156), then Physical Disturbance have a smallest
#negative value(-1,6172),
#these two variables have an influence on the communities of alpine. Therefore this means that
#Form and PhysD contributes to the differences among the different alpine sites.
#This means that the higher the landform value, the less the Physical disturbance.



#In PC2 the slope have higher positive value and Snow-melting have a smaller i.e more negative value, meaning 
#the slope and snow-melting plays a significant role in the differences among the sites, this means that on
#the high steeper slopes there is less snow melting.

#Also as indicated in PC1 snow-melting has high positive value and slope contains small negative values
#meaning that the higher the snow-melting the less steep is the slope.

#Snow-melting and slope though in different directions,have a great impact in 
#the differences in sites of alpine community.

#Also in PC3 with 19% of the inertia the slope shows a higher positive value whereas snow also shows a 
#fairly smaller negative number, which gives to the conclusion that Snow and Slope although move in 
#different direction have great impact on the sites of the alpine communities. The higher steeper
#slope in PC3 shows that there is less snow-melting occurring.

#Aspect have a little influence among the different sites of the alpine community.










