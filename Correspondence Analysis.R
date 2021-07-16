#Author: Viwe Mabongo
#Date: 16 July 2021
#Task: Correspondence Analysis


library(tidyverse)
library(vegan)




# Question 1 --------------------------------------------------------------

# In pane 1 of the Satr species, the pattern showed is that on the right side of 
#the Vertical Abline the species is highly dominant on those sites and then,
# the sites with big circles shows that this Satr species mostly occur on those 
#sites.Then the green lines shows that the sites that fall in the same line
# have the same abundance of this Satr species.

#Pane 2, shows that the Scer species occur mostly in the sites that are in the
#left side of the ordination diagram although there is not much abundance but there is 
# great occurrence of this species in one of the sites that is on top of 
#the horizontal left abline.

#Panel 3 shows that the Teso species occurs mostly on the sites that are in the 
#bottom right of the ordination diagram, with those sites that fall in the same
#green line indicating that those sites have the same number of abundance of this 
#Teso species.

# Panel 4 shows that the Cogo species shows the same pattern displayed by the 
#Teso species, this meaning that there is co-variance between the Teso and the
#Cogo species in terms of occurence among the sites. There is little difference
#interms of the patterns displayed in pane 4 that are different from pane 3.
# Then in pane 4 there is also environmental data displayed and the occurence
# of the Cogo species is abundant where the oxygen vector line is strongly pointing,
# this means that the Cogo specie prefers sites where there is high concentration of 
# the dissolved oxygen, same as the Teso species.


# Question 2 --------------------------------------------------------------

ybirds.spe <- read.delim ('https://raw.githubusercontent.com/zdealveindy/anadat-r/master/data/ybirds_spe.txt', row.names = 1)
Spe <- ybirds.spe

Spe_ca <- cca(Spe)
Spe_ca

summary(Spe_ca)

round(sum(Spe_ca$CA$eig), 5)

round(Spe_ca$CA$eig[1], 5)

round(sum(Spe_ca$CA$eig[1:2]), 5)

round(sum(Spe_ca$CA$eig[1:2]) / sum(Spe_ca$CA$eig) * 100, 2)


# Ordination diagrams -----------------------------------------------------
par(mar = c(1, 1, 1, 1))
par(mfrow(1, 2))
plot(Spe_ca, scaling = 1, main = "CA bird abundances - biplot scaling 1")
plot(Spe_ca, scaling = 2, main = "CA bird abundances - biplot scaling 2")


# Scaling 1 biplot analysis -----------------------------------------------

#Sites 46,47,48,49,50, will likely to be loaded with the same species of WRN,
#meaning these sites have similaries interms of the species occuring in them.
# Then site 45 will be heavily loaded with species of VRF, FLT and JBR. Site 50
# also shows that the ALA species which have high eignvalue will occur in it.


# Scaling 2 biplot analysis  ----------------------------------------------

#Many of the species are concentrated in the sites on the right side of the
#ordination diagram. ALA species will likely to be abundant in site 46 but can 
#also occur in sites 47,48,49,50 as these sites are mot that much far from site 46.


# Advanced plots ----------------------------------------------------------

require('viridis')
palette(viridis(8))
par(mar = c(4, 4, 0.9, 0.5) + .1, mfrow = c(2, 2))
with(Spe, tmp <- ordisurf(Spe_ca ~ ALA, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "ALA"))
abline(h = 0, v = 0, lty = 3)
with(Spe, tmp <- ordisurf(Spe_ca ~ VRF, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "VRF"))
abline(h = 0, v = 0, lty = 3)
with(Spe, tmp <- ordisurf(Spe_ca ~ ILT, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "ILT"))
abline(h = 0, v = 0, lty = 3)
with(Spe, tmp <- ordisurf(Spe_ca ~ JAY, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "JAY"))
abline(h = 0, v = 0, lty = 3)

#Loading the environmental data
ybirds.env <- read.delim ('https://raw.githubusercontent.com/zdealveindy/anadat-r/master/data/ybirds_env.txt', row.names = 1)
Env <- ybirds.env
Env <- dplyr::select(Env, -1, -2)
head(Env)


(Spe_ca_Env <- envfit(Spe_ca, Env, scaling = 2))
plot(Spe_ca_Env, col = "grey40")
plot(Spe_ca_Env, p.max = 0.05, col = "red")



#Pane 1shows that the ALA species is abundant on the site that are on the left
#site of the ordination diagram, with big cirles on the top left of the ordination
#diagram, which means those are the sites where the ALA species is highly abundant.
#The ALA species is mostly influenced by the ground cover(GC)
#environmental variable, which shows that the bigger the ground cover the 
#the more the ALA specie will be highly abundant.


#Pane 2 is showing that the VFR species is abundant on the sites that
#are on the left side of the ordination diagram, with the big cirlces 
#closer to the horizontal line, which means that is where the VRF is 
#highly abundant. The VRF species is mostly influenced by the Elevation(ELE)
#variable, meaning the higher the elevation the highly abundant is the VRF species.


#Pane 3 shows that ILT species is abundant on the sites that are on the right side of
#the ordination diagram, with big circles on top of the horizontal line and this
#pane shows covarriance with pane 4.The ILT species is influenced by the Tree Density(TD)
# and Shrub Cover(SC) environmental variables, meaning where there is high tree density,
#there will be high abundance of the ILT also the shrub cover has some influence, so where
#the is high shrub cover there will be high abundance of the ILT.

#Pane 4 shows that JAY species is abundant on the sites that are on the right side of
#the ordination diagram, with big circles on top of the horizontal line and it have
#shows covarriance with pane 3, which means that Jay and ILT species are occurring in the 
#same sites.Then the JAY species also shows the same trend as the ILT species, its abundance
#is influenced by tree density(TD) and Shrub Cover(SC).




# Alpine plant communities ------------------------------------------------

library(ade4)
spe <- data(aravo)
spe <- as_tibble(aravo[["spe"]])

spe_ca <- cca(spe)
spe_ca

summary(spe_ca)

round(sum(spe_ca$CA$eig), 5)

round(spe_ca$CA$eig[1], 5)

round(sum(spe_ca$CA$eig[1:2]), 5)

round(sum(spe_ca$CA$eig[1:2]) / sum(spe_ca$CA$eig) * 100, 2)


# Ordination diagrams -----------------------------------------------------

par(mar = c(1, 1, 1, 1))
par(mfrow(1, 2))
plot(spe_ca, scaling = 1, main = "CA alpine abundances - biplot scaling 1")
plot(spe_ca, scaling = 2, main = "CA alpine abundances - biplot scaling 2")

#Scaling 1 biplot shows sites on the right side of the ordination diagram, 
# are closely asscoiated with on e another and that is where most of these
#alpine species are abundant.


#Scaling 2 biplot shows that these species are mostly abundant on the sites that 
#are on the right side of the ordination diagram.




# Advanced plots ----------------------------------------------------------

require('viridis')
palette(viridis(8))
par(mar = c(4, 4, 0.9, 0.5) + .1, mfrow = c(2, 2))
with(spe, tmp <- ordisurf(spe_ca ~ Anth.alpe, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Anth.alpe"))
abline(h = 0, v = 0, lty = 3)
with(spe, tmp <- ordisurf(spe_ca ~ Care.rupe, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Care.rupe"))
abline(h = 0, v = 0, lty = 3)
with(spe, tmp <- ordisurf(spe_ca ~ Sali.retu, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Sali.retu"))
abline(h = 0, v = 0, lty = 3)
with(spe, tmp <- ordisurf(spe_ca ~ Sali.reti , bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Sali.reti"))
abline(h = 0, v = 0, lty = 3)

#Pane 1 shows that the Anth.alpe species is abundant on the sites that are on the right side of
#the ordination diagram, with big circles on the right side of the ordination diagram, which means
#that is where the species is highly abundant.

#Pane 2 shows that the Care.rupe species also occurs on the right side of the ordination
#diagram, with big circles that fall on the same green line meaning these sites have
#same abundance of this species.

#Pane 3 shows that the sali.retu is also abundant on the right side of the ordination,
#diagram with the same big circles sowing the high abundance of this species.


#Pane 4 shows that the Sali.reti is abundant on the sites that are on the right side of the ordination
# diagram, then in all these four panes there is covarriance on where these species are occurring.



# Loading the environmental data ------------------------------------------

library(ade4)
env <- data(aravo)
env <- as_tibble(aravo[["env"]])

env <- dplyr::select(env, -5)# removing column 5 which in non-numeric.
head(env)

env$Form <- as.numeric(as.character(env$Form))# changing the Form column to numeric value.


(spe_ca_env <- envfit(spe_ca, env, scaling = 2))
plot(Spe_ca_env, col = "grey40")
plot(spe_ca_env, p.max = 0.05, col = "red")


#Based on the sites where these species are mostly abundant Physical Disturbance(PhysD) is the 
#environmental variable that influences their distribution, which means where there is high
#PhysD there is high abundance of these 4 selected species.






















