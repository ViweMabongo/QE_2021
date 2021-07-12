#Author: Viwe Mabongo
#Date: 12 July 2021
#Task: Ordinations

library(tidyverse)
library(vegan)
library (ade4)
data(aravo)

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

#GC,ELE,EXP, SLP and CP have large positive loadings on component 1.Meaning these
#variables have strong influence on the component.

#FHD, MBD and SDDB have large negative loadings on component 2.Meaning these components
#variables have strong influence on the component.


# B -----------------------------------------------------------------------

# The reason the is high positive altitude in these bird communities is because, 
# birds by nature fly so mostly they occur in high altitudes and also these birds communities,
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
Env_av <- dplyr::select(Env_av, -5)
head(Env_av)

Env_av$Form <- as.numeric(as.character(Env_av$Form))# changing the Form column to numeric value


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
