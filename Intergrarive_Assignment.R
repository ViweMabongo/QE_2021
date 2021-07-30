#Author: Viwe Mabongo
#Date : 30 July 2021
#Task: Intergrative Assignment


library(tidyverse)
library(vegan)
library(betapart)
library(vegan3d)
library(ggarrange)
library(ggplot2)
library(ggpubr)



#Loading the Doubs species Fish Data CSV file that will be used.
Spe <- read_csv("DoubsSpe.csv")

#Dimension show how many rows and columns we have.
dim(Spe)

#Removing a the first column that is not relevant in our data.
Spe <- dplyr::select(Spe, -1)
Spe <- Spe[rowSums(Spe) > 0, ]
str(Spe)
head(Spe, 8)


#Do Correspondence Analysis(CA)
Spe_ca <- cca(Spe)
Spe_ca

summary(Spe_ca)

round(sum(Spe_ca$CA$eig), 5)

round(Spe_ca$CA$eig[1], 5)

round(sum(Spe_ca$CA$eig[1:2]), 5)

round(sum(Spe_ca$CA$eig[1:2]) / sum(Spe_ca$CA$eig) * 100, 2)

# Ordination diagrams -----------------------------------------------------

par(mfrow = c(1, 2))
plot(Spe_ca, scaling = 1, main = "CA Fish abundances - BP scaling 1")
plot(Spe_ca, scaling = 2, main = "CA Fish abundances - BP scaling 2")


# Advanced plots ----------------------------------------------------------

require('viridis')
palette(viridis(8))
par(mar = c(4, 4, 0.9, 0.5) + .1, mfrow = c(2, 2))
with(Spe, tmp <- ordisurf(Spe_ca ~ Satr, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Satr"))
abline(h = 0, v = 0, lty = 3)
with(Spe, tmp <- ordisurf(Spe_ca ~ Teso, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Teso"))
abline(h = 0, v = 0, lty = 3)
with(Spe, tmp <- ordisurf(Spe_ca ~ Thth, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Thth"))
abline(h = 0, v = 0, lty = 3)
with(Spe, tmp <- ordisurf(Spe_ca ~ Icme, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Icme"))
abline(h = 0, v = 0, lty = 3)# The most negative spe in CA1


#Loading the environmental data
Env <- read_csv("DoubsEnv.csv")
Env <- dplyr::select(Env, -1)# Remove the first column which is not relevant in our data. 

# we removed the 8th row in Spe data, so we also going to remove it also here on environmental data.
Env <- dplyr::slice(Env, -8)

# the last plot produced (CA scaling 2) must be active
# scaling 2 is default
(Spe_ca_Env <- envfit(Spe_ca, Env, scaling = 2))
plot(Spe_ca_Env, col = "grey40")
plot(Spe_ca_Env, p.max = 0.05, col = "red")# Plotting those variables that significant with
#the red colour.



# Preparing data for CCA analysis -----------------------------------------

# Remove the 'dfs' variable from the env data frame
Env1 <- Env[, -1]
# Recode the slope variable (slo) into a factor (qualitative)
# variable to show how these are handled in the ordinations
slo2 <- rep(".very_steep", nrow(Env))
slo2[Env$slo <= quantile(Env$slo)[4]] <- ".steep"
slo2[Env$slo <= quantile(Env$slo)[3]] <- ".moderate"
slo2[Env$slo <= quantile(Env$slo)[2]] <- ".low"
slo2 <- factor(slo2,
               levels = c(".low", ".moderate", ".steep", ".very_steep"))
table(slo2)
# Create an Env data frame with slope as a qualitative variable
# Create an env3 data frame with slope as a qualitative variable
Env2 <- Env1
Env2$slo <- slo2



# Do CCA for Doubs --------------------------------------------------------
#Ho: The environmental conditions have no influence on the species community composition.
#H1: The environmental conditions have influence on species community composition.

(Spe.cca <- cca(Spe ~ ., Env2))
summary(Spe.cca)
# Unadjusted and adjusted R^2 - like statistics
RsquareAdj(Spe.cca)



# Ordination diagrams -----------------------------------------------------


# Biplots -----------------------------------------------------------------
#CCA scaling 1 biplot without species (using lc site scores)
plot(Spe.cca,
     scaling = 1, 
     display = c("lc", "cn"), 
     main = "Biplot CCA Spe ~ Env2 - scaling 1")

#CCA scaling 2 biplot with species but without sites
plot(Spe.cca, 
     scaling = 2, 
     display = c("sp", "cn"), 
     main = "Biplot CCA Spe ~ Env2 - scaling 2")

# Triplots ----------------------------------------------------------------

par(mfrow = c(1, 2))
# Scaling 1: species scoresscaled to the relative eigenvalues, 
# sites are weighted averages of the species
plot(Spe.cca, 
     scaling = 1, 
     display = c("sp", "lc", "cn"), 
     main = "Triplot CCA Spe ~ Env2 - scaling 1")

# Default scaling 2: site scores scaled to the relative 
# eigenvalues, species are weighted averages of the sites
plot(Spe.cca, 
     display = c("sp", "lc", "cn"), 
     main = "Triplot CCA Spe ~ Env2 - scaling 2")




#  Hierarchical cluster analysis ------------------------------------------

dist.mat<-vegdist(Spe,method="bray") #distance matrix based on bray-curtis distance

clust.res1<-hclust(dist.mat,method="average") #agglomerative clustering using avarage linkage

# Dendrogram --------------------------------------------------------------

plot(clust.res1)

# We reject the null hypothesis. We accept the alternate hypothesis.
# The environmental constraints have influence on the species composition of 
# the fish communities.

# END ---------------------------------------------------------------------










