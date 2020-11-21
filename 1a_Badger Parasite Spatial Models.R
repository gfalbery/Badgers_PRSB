
# Badger Parasite Spatial Models ####

{
  
  library(igraph); library(tidyverse); library(INLA); library(cowplot); library(ggregplot); library(magrittr)
  
  theme_set(theme_cowplot())
  
  Resps <- c("Fleas", "Lice", "Ticks", "Eimeria", "Isospora")
  
}

FamilyList <- c("nbinomial", "nbinomial", rep("binomial", 3))

names(FamilyList) <- Resps

EctoLimits <- c(30, 15, 1, 1, 1, Inf)

Covar <- c("Sex", "Year", "Month", "AgeCat", "BCI")

SpatialBadgerModels <- MeshList <- SpatialTestDFList <- list()

i = 1

inla.setOption(num.threads = 8)

# SpatialBadgerModels <- readRDS(paste0("Output Files/", "SpatialBadgerModels", ".rds"))

# Overall Models ####

{
  
  for(i in i:length(Resps)){
    
    print(Resps[i])
    
    # SpatialBadgerModels[[Resps[i]]] <- list()
    
    Badgers %>% #filter(AgeCat == "A") %>% 
      dplyr::select(Tattoo, X, Y, Resps[i], Covar) %>%
      mutate(fYear = as.factor(Year)) %>%
      mutate_at(c("Year", "BCI"), ~c(scale(.x))) %>%
      na.omit() %>% 
      droplevels ->
      TestDF
    
    TestDF <- TestDF[TestDF[,Resps[i]] <= EctoLimits[i],]
    
    if(Resps[i] == "Isospora") TestDF %>% filter(!Month == 10) -> TestDF
    
    N <- nrow(TestDF); print(N)
    
    if(Resps[i] %in% c("Eimeria", "Isospora")){
      
      SubCovar <- Covar %>% setdiff("Year")
      
    }else{
      
      SubCovar <- Covar  
      
    }
    
    BaseLevels <- GetBaseLevels(SubCovar, TestDF)
    
    TestDF -> SpatialTestDFList[[Resps[i]]]
    
    Points <- TestDF[,c("X", "Y")]
    
    Points %>% as.data.frame() %>% slice(sample(1:n(), 100)) %>% 
      dist %>% c %>% max %>% divide_by(2) ->
      NullRangePrior; NullRangePrior
    
    Xm <- model.matrix(as.formula(paste0("~ -1 +", paste(Covar, collapse = " + "))), data = TestDF)
    X <- as.data.frame(Xm)[,!colnames(Xm)%in%BaseLevels]
    
    f1 <- as.formula(paste0("y ~ -1 + Intercept + ", 
                            paste0(colnames(X), collapse = " + "), 
                            "+ f(Tattoo, model = 'iid') + f(fYear, model= 'iid')"))
    
    f2 <- as.formula(paste0("y ~ -1 + Intercept + ", 
                            paste0(colnames(X), collapse = " + "), 
                            "+ f(Tattoo, model = 'iid') + f(fYear, model= 'iid') + f(w, model = spde)"))
    
    Points = cbind(TestDF$X, TestDF$Y)
    
    MeshList[[Resps[i]]] <- Mesh <- 
      inla.mesh.2d(loc = Points, max.edge = c(NullRangePrior/10, NullRangePrior/5), cutoff = NullRangePrior/20); plot(Mesh); points(Points)
    A3 <- inla.spde.make.A(Mesh, loc = Points) # Making A matrix
    spde = inla.spde2.pcmatern(mesh = Mesh, 
                               prior.range = c(NullRangePrior, 0.5), prior.sigma = c(.5, .5)) # Making SPDE
    
    w.index <- inla.spde.make.index('w', n.spde = spde$n.spde)
    
    SocialStack <- inla.stack(
      data = list(y = TestDF[,Resps[i]]),  
      A = list(1, 1, 1, 1, A3), # Vector of Multiplication factors              
      effects = list(
        Intercept = rep(1, N), # Leave
        X = X, # Leave
        Tattoo = TestDF$Tattoo,
        fYear = TestDF$fYear,
        w = w.index)) # Leave
    
    print("Base!")
    SpatialBadgerModels[[Resps[i]]]$Base <- inla(f1, # f1 + Year and ID random effects
                                                 family = FamilyList[[i]],
                                                 data = inla.stack.data(SocialStack),
                                                 control.compute = list(dic = TRUE),
                                                 control.predictor = list(A = inla.stack.A(SocialStack))
    )
    
    print("SPDE!")
    SpatialBadgerModels[[Resps[i]]]$SPDE <- inla(f2, # f2 + SPDE random effect 
                                                 family = FamilyList[[Resps[i]]],
                                                 data = inla.stack.data(SocialStack),
                                                 control.compute = list(dic = TRUE),
                                                 control.predictor = list(A = inla.stack.A(SocialStack))
    )
    
  }
}

saveRDS(SpatialBadgerModels, paste0("Output Files/", "SpatialBadgerModels", ".rds"))
save(MeshList, SpatialTestDFList,
     file = paste0("Output Files/", "SpatialBadgerData", ".Rdata"))
