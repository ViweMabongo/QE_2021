#Author: Viwe Mabongo
#Date : 06 July 2021
#Task : Correlations and Associations


library(tidyverse)
library(vegan)
library(Hmisc)
library(ggcorrplot)


#1
DoubsEnv <- read_csv("DoubsEnv.csv")
head(env, 1)

# drop the first column
DoubsEnv <- dplyr::select(DoubsEnv, -1)
head(DoubsEnv)



# Compute a correlation matrix
data(DoubsEnv)
corr <- round(cor(DoubsEnv), 1)
head(corr[, 1:6])

corr_M <- cor(as.matrix(DoubsEnv))


# Visualizing the correlation matrix
# --------------------------------

ggcorrplot(corr_M)+ ggtitle("Environmental Pairwise correlation")

#2#Altitude and Distance from the source(dfs) shows a high negative correlation.


#Mean minimum discharge(flo) and Distance from the source(dfs) shows high positive correlation.


#3
#The reason there is negative correlation between dfs and altitude is because as the distance
#from the source increases the altitude above sea level of the river/stream deceases.


#There is high correlation between dfs and flo the higher the is because as the distance goes
# more farthest/increases from the source the discharge(m3/s) speed also increases.


# Question B --------------------------------------------------------------

#1 
#We transpose the species data so that the row and column indices
#of the matrix switches to produce another matrix. When the data is transposed it will
#form the species association matrix.The transpose of species data also plays an important role in
#estimating variances and covariances in regression.


#2 The diagonals of the matrix and transpose matrix remain unchanged
#but all the other elements are rotated around the diagonal.



# Question C --------------------------------------------------------------
#1
#Association matrix diagonal is zero. Symetrical


#Association matrix refers to any relationship between two variables, 
#whereas correlation matrix is often used to refer only to a linear relationship between two variables.
#Correlation diagonal is 1.

#Association matrix refers to any relationship between two variables, 
#whereas the dissimilarity matrix describes pairwise distinction between M objects.

#2 The difference is that spp_assoc1 have big values of association compared,
# spp_assoc2. Spp_assoc1 is calculated from abundance species data, whereas spp_assoc2 is 
#calculated from presence-absence species data.
#Yes the information contained in these association noticeably,
# different from each other.


#3 Association matrix gives us the frequency at which two or more species
#co-occur among a set of spatial locations (study sites,transects, plots, ecological communities).

# Association matrix follows the pattern of the dissimilarity matrix, species that are
# not occurring in the same site or plot have higher values close to 1, 
#whereas those species with smaller association values shows values that are not much
#much further from 0 among the sites.
# The diagonals are also zero in association matrix.










