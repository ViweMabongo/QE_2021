#Author: Viwe Mabongo
#Date: 05 July 2021
#Task: disimilarity_matrices


library(tidyverse)
library(vegan)
library(BiodiversityR)
library(ggplot2)
library(dplyr)
library(readr)
library(betapart)

#Loading the Doubs species Fish Data CSV file that will be used.
DoubsSpe <- read_csv("DoubsSpe.csv")


#Dimension show how many rows and columns we have.
dim(DoubsSpe)


#Removing a the first column that is not relevant in our data
DoubsSpe <- dplyr::select(DoubsSpe, -1)
str(DoubsSpe)

# Species Dissimilarities -------------------------------------------------


# 1 -----------------------------------------------------------------------


#The data set is an abundance data, with each site showing the number of species available,
# it have 30 rows(meaning that there are 30 sites studied) and have 27 columns(meaning),
#there are 27 different species observed in these different sites.


# 2 -----------------------------------------------------------------------

# Bray_Curtis(Bray-Curtis weights the abundances
#of shared species more.)



# 3 -----------------------------------------------------------------------

#Calculating the spcies abundance dissimilarity using Bray-Curtis.
Bray <- round(vegdist(DoubsSpe, binary = FALSE, diag = TRUE), 4)
bray_df <- round(as.matrix(Bray), 4)
bray_df[1:1, ]


# 4 -----------------------------------------------------------------------
#Based on the calculation there are dissimilarities in the abundance of the species among the 
# different sites, site 8 is highly dissimilar when compared to other sites interms of species abundance,
# at site 8 the dissimilarity is standing at 1. The  upper Salmonid Region region has lower 
#dissimilarities among the sites when compared to the lower Cyprinid region.


# 5 -----------------------------------------------------------------------
#The general pattern that is shown by the matrix is that at column 1, there is a clear
# trend in the increase in dissimilaries from upper to lower zones and the trend is visible in the 
# matrix as a whole. Also column 8 shows a great similarities when compared to other sites.


# 6 -----------------------------------------------------------------------

#Plot showing the Doubs species trend
ggplot(data = DoubsSpe, (aes(x =1:30, y = bray_df[1:1,]))) +
  geom_line() + xlab("Ecological Zones, Upper to Lower") + ylab("Disimilarities(Bray-Curtis index)")+
  ggtitle( "Ecological zones along European rivers and streams")



# 7 -----------------------------------------------------------------------

#The graph is illustrating that from site 1 to 5 there are dissimilarities between communities occurring
#there is increase, then at site 7 the dissimilarities were decreasing. At site 8 and 9 the
#dissimilarities were at the highest recording exactly 1, meaning in these sites there were high
#dissimilarities when compared to other sites.


# 8 -----------------------------------------------------------------------

#Convert the abundance data to Presence-absence data
D_St <- round(decostand(DoubsSpe, method = "pa"),4)
D_St[1:10, 1:10] 

sor <- vegdist(DoubsSpe, binary = TRUE) # binary = TRUE sets to presence/absence data
sor_df <- round(as.matrix(sor), 4)
sor_df[1:1, ]


# 9 -----------------------------------------------------------------------

#Create a plot for row number 1 agaisnt all the columns
ggplot(data = DoubsSpe, (aes(x =1:30, y = sor_df[1:1,]))) +
  geom_line() + xlab("Ecological Zones, Upper to Lower") + ylab("Disimilarities(Sorenson index)")+
  ggtitle( "Ecological zones along European rivers and streams")

#In this plot in the upper zones that from the source the dissimilarities are visible among the
#fish communities but as we further South to the lower regions near the mouth the dissimilarities are
#visible higher as they go up to 1. In the middle region of the rivers there are dissimilarities
# in fish communities among the different sites but are little bit lower than the lower rigion.








