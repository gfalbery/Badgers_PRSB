
# 3b_Badger Results

{
  
  library(igraph); library(tidyverse); library(INLA); library(cowplot); library(ggregplot);
  library(colorspace); library(patchwork); library(ggimage); library(MCMCglmm)
  
  theme_set(theme_cowplot())
  
  ParasitePalettes <- AlberColours[c(2, 1, 3)] # c("PuRd","PuBu","BuGn","Purples","Oranges")
  ParasiteColours <- AlberColours[c(2, 1, 3)]  %>% c("#DD1c77","#2B8CBE","#2CA25F",
                                                     RColorBrewer::brewer.pal(5,"Purples")[4],
                                                     RColorBrewer::brewer.pal(5,"Oranges")[4])
  
}

# Delta-DICs

IMList$Fleas$dDIC[[1]][["LifetimeDensity"]]

IMList$Fleas$FinalModel %>% INLAPValue("LifetimeDensity")

YoungIMList$Lice$dDIC[[1]][["TrappingDensity"]]

YoungIMList$Lice$FinalModel %>% INLAPValue("TrappingDensity")

IMList$Ticks$dDIC[[1]][["LifetimeDensity"]]

IMList$Ticks$FinalModel %>% INLAPValue("LifetimeDensity")

IMList$Eimeria$dDIC[[1]][["TrappingDensity"]]

IMList$Eimeria$FinalModel %>% INLAPValue("TrappingDensity")

CubIMList$Lice$dDIC[[1]][["TrappingDensity"]]
Marginal <- CubIMList$Lice$FinalModel$marginals.fixed$TrappingDensity %>% 
  inla.rmarginal(marginal = ., 1000000)

PCalc(Marginal)

CubIMList$Lice$FinalModel %>% INLAPValue("TrappingDensity")
CubIMList$Lice$FinalModel %>% GetEstimates("TrappingDensity")

YoungIMList$Lice$FinalModel %>% INLAPValue("TrappingDensity")
YoungIMList$Lice$FinalModel %>% GetEstimates("TrappingDensity")

YoungIMList$Lice$dDIC[[1]][["TrappingDensity"]]
Marginal <- YoungIMList$Lice$FinalModel$marginals.fixed$TrappingDensity %>% 
  inla.rmarginal(marginal = ., 1000000)

PCalc(Marginal)

IMList$Eimeria$dDIC[[1]][["TrappingDensity"]]
Marginal <- IMList$Eimeria$FinalModel$marginals.fixed$TrappingDensity %>% 
  inla.rmarginal(marginal = ., 1000000)

PCalc(Marginal)

IMList$Ticks$dDIC[[1]][["LifetimeDensity"]]
Marginal <- IMList$Ticks$FinalModel$marginals.fixed$LifetimeDensity %>% 
  inla.rmarginal(marginal = ., 1000000)

PCalc(Marginal)

AdultIMList$Ticks$dDIC[[2]][["Degree"]]
Marginal <- AdultIMList$Ticks$FinalModel$marginals.fixed$Degree %>% 
  inla.rmarginal(marginal = ., 1000000) %>% PCalc

PCalc(Marginal)

# Survival ####

SurvivalIMList$Lice$dDIC
SurvivalIMList$Lice$FinalModel %>% INLAPValue("Lice")

CubSurvivalIMList$Lice$dDIC
CubSurvivalIMList$Lice$FinalModel %>% INLAPValue("Lice")


SurvivalIMList$Lice$dDIC
SurvivalIMList$Lice$FinalModel %>% INLAPValue("TrappingDensity")

CubSurvivalIMList$Lice$dDIC
CubSurvivalIMList$Lice$FinalModel %>% INLAPValue("TrappingDensity")


SurvivalIMList$Lice$dDIC

