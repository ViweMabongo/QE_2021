#Author: Viwe Mabongo
#Date: 02 July 2021
#Beta_Diversity_Measures

library(tidyverse)
library(vegan)
library(BiodiversityR)
library(ggplot2)
library(dplyr)
library(readr)
library(betapart)



# 1 -----------------------------------------------------------------------
#Loading CSV data for analysis
SeaSpp <- read_csv("SeaweedsSpp.csv")


#Checking the number of rows and columns contained by the dataset.
dim(SeaSpp)

#Removing the first column that is not representing species data.
SeaSpp <- dplyr::select(SeaSpp, -1)

#Calculating the species turnover using the absolute beta diversity formula.
abs_beta <- data.frame(beta = ncol(SeaSpp) - specnumber(SeaSpp,
                                                        MARGIN = 1), section_no =c(1:58))



#Decompose Sorensons(Bsor) dissimilarity into "Turnover" (Bsim) component.

Y.core <- betapart.core(SeaSpp)
Y.pair <- beta.pair(Y.core, index.family = "sor")

#Y1 represents the turnover component
Y1 <- as.matrix(Y.pair$beta.sim)

#Calculating Species turnover (Bsim) 
Beta_Sim <- round(Y1[1:58, 1], 4)

#Plot representing species turnover as function of Section number
ggplot(data = abs_beta, (aes(x = section_no,y = Beta_Sim))) +
  geom_line()+ xlab("Coastal section, West to East(km)")+ ylab("Species Turnover(Bsim)")+
  ggtitle("Species turnover across a Coastal section, West to East")

#The trend illustrates that there are lower dissimilarities in the sections that are in the West coast,
# then as we move further east there are higher dissimilarities in the seaweed communities,and 
# the thermal gradient is attributed as the driver for the composition of these Seaweed
# communities. Temperature is really justified to influence the turnover, as the West section is found
#on the cold benguela current and the further East Section is on the warm Agulhas current.



# 2 -----------------------------------------------------------------------

# Nestedness resultant beta biodiversity is defined as process that causes species to be gained or lost,
#and the community with the lowest aplha-diversity is a subset of the richer community.

#Here we have a community of Copepods species along an environmental monitoring transect
#with 12 stations. 
# In the transect there can be 18 different types of Copepods.Then at station 1 we can get a get
# a vertical bongo tow with 8 different species of copepods, then maybe at station 2 we can get
#only 4 types of the species of those in station 1, then this can be a loss of species due to 
#diel vertical migration. Then station 2 will be treated as the subset of 
#station 1 due to it having the lowest alpha-diversity community of copepods.

#Then a situation can occur where station 3 have 5 species of cpepods, then at station 4 we get 9
#species of copepods with  5 species similar to those in station 3 but with an added 4 other different
#species compared to station 3, this is species gain.Starion 3 is the subset of station 4.