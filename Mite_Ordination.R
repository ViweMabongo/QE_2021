#Author: Viwe Mabongo
#Date: 23 July 2021
#Task: Mite and Dune Ordinations


library(vegan)
library(cluster)
library(tidyverse)



# Loading mite species data -----------------------------------------------

data("mite") #loading mite abundance data

Spe <- mite


# Do nmDS -----------------------------------------------------------------

Spe_nmds <- metaMDS(Spe, distance = "bray")
Spe_nmds

par(mfrow = c(2, 2))
stressplot(Spe_nmds, main = "Shepard plot")
ordiplot(Spe_nmds, type = "t", cex = 1.5, main = paste0("nMDS stress = ", round(Spe_nmds$stress, 2)))
gof = goodness(Spe_nmds)
plot(Spe_nmds, type = "t", main = "Goodness of fit")
points(Spe_nmds, display = "sites", cex = gof * 200)

require('viridis')
palette(viridis(8))
par(mar = c(4, 4, 0.9, 0.5) + .1, mfrow = c(2, 2))
with(Spe, tmp <- ordisurf(Spe_nmds ~ Ceratoz3, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Ceratoz3"))
abline(h = 0, v = 0, lty = 3)
with(Spe, tmp <- ordisurf(Spe_nmds ~SSTR , bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "SSTR"))
abline(h = 0, v = 0, lty = 3)
with(Spe, tmp <- ordisurf(Spe_nmds ~ Trhypch1, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Trhypch1"))
abline(h = 0, v = 0, lty = 3)
with(Spe, tmp <- ordisurf(Spe_nmds ~ Trimalc2, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Trimalc2"))
abline(h = 0, v = 0, lty = 3)



# Adding the environmental variables in the plot --------------------------


data("mite.env") # loading mite environmental data

Env <- mite.env

Dist.mite <- daisy(Env, metric = "gower")# Calculating the distance matrix with Gower distance matrix.

Dist_mat <- as.matrix(Dist.mite)# Calculating the distance matrix with Gower distance matrix.

Env_nMDS <- metaMDS(Dist_mat)# doing the nMDS for environmental data.
Env_nMDS

stressplot(Env_nMDS)# Visualizing the environmental stresspolt.

ordiplot(Env_nMDS, type = "text")# Creating a plot for sites

Env.fit <- envfit(Env_nMDS, Env)# Creating the env fit for these variales.
plot(Env.fit)# Plotting the environmental variables.

#Pane 1 shows that the Ceratoz3 species is found on the right side of the ordination diagram,
#where it mostly abundant.

#Pane 2 shows the SSTR species is highly abundant on the left side of the ordination diagram,
#with green lines indicating that the circles that lie on the same line have thae same abundance
# of this particular species.

#Pane 3 shows that the Trhypch1 species is abundant on the right side of the ordination diagram
#with the big circles showing where this species is highly abundant.

#Pane 4 illustates the abundance of the Trimalc2 species abundance together with the environmental
#variables and pane 3 and pane 4 shows some covariance, where these species are highly abundant.
#The environmental variables that show much influence on the abundance of these mites is 
# Water Content and Substrate Density, this is ecological reasonable as there are mites that
#live in water, meaning these mites in this dataset are those that prefer water as the habitat.


# The Shepered plot shows that there is a good relationship between Ordination Distance and 
#Observed Dissimilarity, with a good r squared that is 0.978.


# Do PCoA for mite abundance data -----------------------------------------

Spe_pcoa <- capscale(Spe ~ 1, distance = "bray")
Spe_pcoa

summary(Spe_pcoa)

round(sum(Spe_pcoa$CA$eig[1:3]) / sum(Spe_pcoa$CA$eig) * 100, 2)


par(mfrow = c(1, 2))
plot(Spe_pcoa, scaling = 1, main = "PCoA mite abundances - biplot scaling 1")
plot(Spe_pcoa, scaling = 2, main = "PCoA mite abundances - biplot scaling 2")

#Scaling 1 biplot shows that the sites are concentrated on the centre of the ordination,
# and towards the right side of the ordination, and most of these species will most occur on
#those concentrated sites.


#Scaling 2 biplot shows that these species are closely associated and will be highly
# abundant on sites 46 and 69 of the ordination diagram.


# Section 2 ---------------------------------------------------------------

data("dune")
data("dune.env")

#This dataset was collected as a part of the project focused on the effect of management on dune meadows.
#It contains 20 plots of 2×2 m2, sampled in 1982 following Braun-Blanquet method estimating plant cover 
#in each plot (using 9-grade ordinal cover scale). Total of 30 species (28 vascular and 2 bryophytes) 
#were recorded.


env <- dune.env
env1 <- dplyr::select(env, -3, -4)
head(env1)

glimpse(env1)


# Calculate the PCA -------------------------------------------------------


env1_pca <- rda(env1,scale = TRUE)
env1_pca

round(env1_pca$CA$eig[1], 3)

sum(env1_pca$CA$eig)

round(env1_pca$CA$eig[1] / sum(env1_pca$CA$eig) * 100, 1)

summary(env1_pca)



# Graphical representation ------------------------------------------------


par(mfrow = c(1, 2))
biplot(env1_pca, scaling = 1, main = "PCA scaling 1", choices = c(1, 2))
biplot(env1_pca, scaling = 2, main = "PCA scaling 2", choices = c(1, 2))

#Scaling plot 1 on the right side of the ordination shows that 14 and 15
#are closely related and are influenced by Moisture content of the soil and Thickness of the A1 horizon.


#Scaling plot 2 on the right side of the ordination shows that thickness of the A1 horizon
# and moisture content of the soil are strongly correlated based on the side of the angle
# betwwen these varialbe vectors.



# Do Correspondence Analysis ----------------------------------------------

spe <- dune

spe_ca <- cca(spe)
spe_ca

summary(spe_ca)

round(sum(spe_ca$CA$eig), 5)

round(spe_ca$CA$eig[1], 5)

round(sum(spe_ca$CA$eig[1:2]), 5)

round(sum(spe_ca$CA$eig[1:2]) / sum(spe_ca$CA$eig) * 100, 2)

# Ordination diagrams -----------------------------------------------------


par(mfrow = c(1, 2))
plot(spe_ca, scaling = 1, main = "CA dune abundances - biplot scaling 1")
plot(spe_ca, scaling = 2, main = "CA dune abundances - biplot scaling 2")

#Scaling 1 biplot shows that sites 17 and 19 are closely assocciated and the 
#species Anthodor is likely to occur in both these two sites. Also sites 15,16 and 20
# on the right side of the ordination are closely associated at the species that occur
#in these sites are the same.


#Scaling 2 biplot on the right site of the ordination shows that
#species Callcusp, Ranuflam, Eleopalu and Comapalu are closely associated in terms of distance,
#which means that all these species are high likely to occur in the same site, 
#which in this case is site 15.


# Advanced plots ----------------------------------------------------------

require('viridis')
palette(viridis(8))
par(mar = c(4, 4, 0.9, 0.5) + .1, mfrow = c(2, 2))
with(spe, tmp <- ordisurf(spe_ca ~ Comapalu, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Comapalu"))
abline(h = 0, v = 0, lty = 3)
with(spe, tmp <- ordisurf(spe_ca ~ Eleopalu, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Eleopalu"))
abline(h = 0, v = 0, lty = 3)
with(spe, tmp <- ordisurf(spe_ca ~ Callcusp, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Callcusp"))
abline(h = 0, v = 0, lty = 3)
with(spe, tmp <- ordisurf(spe_ca ~ Ranuflam , bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Ranuflam"))
abline(h = 0, v = 0, lty = 3)

#Pane 1 shows that the Comapalu species is abundant on the sites that are on the right side of
#the ordination diagram, with big circles on the right side of the ordination diagram, which means
#that is where the species is highly abundant.

#Pane 2 shows that the Eleopalu species also occurs on the right side of the ordination
#diagram, with big circles that fall on the same green line meaning these sites have
#same abundance of this species.

#Pane 3 shows that the Callcusp is also abundant on the right side of the ordination,
#diagram with the same big circles sowing the high abundance of this species.


#Pane 4 shows that the Ranuflam is abundant on the sites that are on the right side of the ordination
# diagram, then in all these four panes there is covarriance on where these species are abundant.

(spe_ca_env1 <- envfit(spe_ca, env1, scaling = 2))
plot(Spe_ca_env1, col = "grey40")
plot(spe_ca_env1, p.max = 0.05, col = "red")

#Based on the sites where these species are mostly abundant Moisture is the 
#environmental variable that influences their distribution, which means where there is high
#Moisture there is high abundance of these 4 selected species.




