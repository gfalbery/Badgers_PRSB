
# 1b_Badger Parasite Density Models ####

{
  
  library(igraph); library(tidyverse); library(INLA); library(cowplot); library(ggregplot)
  
  theme_set(theme_cowplot())
  
}

# Parasite sorting ####

FamilyList <- c("nbinomial", "nbinomial", 
                "binomial", # "zeroinflatedbinomial0",
                rep("binomial", 2))

names(FamilyList) <- Resps

EctoLimits <- c(30, 15, 1, 1, 1)

Covar <- c("AgeCat", "Sex", "Year", "Month", "BCI")

SocCovar <- c("Degree", 
              "GroupSize",
              "LifetimeDensity", "TrappingDensity",
              "AnnualDensity")

ClashList <- 
  list(SocCovar)

MeshList <- NullMeshList <- TestDFList <- 
  IMList <- 
  list()

i <- 1

# Badgers %>% filter(TrappingDensity<0.75) -> Badgers 

for(i in (i:length(Resps))){
  
  print(Resps[i])
  
  Badgers %>% 
    dplyr::select(Tattoo, X, Y, Resps[i], Covar, SocCovar) %>% 
    droplevels %>%
    mutate(AgeCat = factor(AgeCat, levels = c("C", "Y", "A"))) %>%
    mutate(fYear = as.factor(Year), Month = as.factor(Month)) %>%
    mutate_at(c(SocCovar %>% setdiff("Soil"), "Year"), 
              ~c(scale(.x))) %>%
    na.omit() ->
    TestDF
  
  if(Resps[i] == "Isospora") TestDF <- TestDF %>% filter(!Month == 10) %>% droplevels
  
  TestDF <- TestDF[TestDF[,Resps[i]] <= EctoLimits[i],]
  
  TestDF -> TestDFList[[Resps[i]]]
  
  N <- nrow(TestDF); print(N)
  
  if(Resps[i] %in% c("Eimeria", "Isospora")){
    
    IM1 <- INLAModelAdd(Data = TestDF, 
                        Response = Resps[i],
                        Explanatory = Covar %>% setdiff("Year"), 
                        Add = SocCovar, 
                        Random = c("Tattoo", "fYear"), 
                        RandomModel = rep('iid', 2),
                        Clashes = ClashList,
                        Family = FamilyList[i])
    
  }else if(Resps[i] %in% c("Ticks")){
    
    IM1 <- INLAModelAdd(Data = TestDF, 
                        Response = Resps[i],
                        Explanatory = Covar, 
                        Add = SocCovar, 
                        Random = c("Tattoo", "fYear"),# [2], 
                        RandomModel = rep('iid', 2),# [1],
                        Clashes = ClashList,
                        Family = FamilyList[i])
    
  }else{
    
    IM1 <- INLAModelAdd(Data = TestDF, 
                        Response = Resps[i],
                        Explanatory = Covar, 
                        Add = SocCovar,
                        Random = c("Tattoo", "fYear"), 
                        RandomModel = rep('iid', 2),
                        Clashes = ClashList,
                        Family = FamilyList[i])
    
  }
  
  IMList[[Resps[i]]] <- IM1
  
  beepr::beep()
  
}

saveRDS(IMList, file = "Output Files/IMList.rds")

# Only Adults ####

AdultCovar <- c("Age", "Sex", "Year", "Month", "BCI")

AdultIMList <- AdultTestDFList <- list()

i <- 1

for(i in (i:length(Resps))){
  
  print(Resps[i])
  
  Badgers %>% 
    filter(AgeCat == "A") %>%
    dplyr::select(Tattoo, X, Y, Resps[i], AdultCovar, SocCovar) %>% 
    droplevels %>%
    mutate(fYear = as.factor(Year), Month = as.factor(Month)) %>%
    mutate_at(c(SocCovar %>% setdiff("Soil"), "Year", "Age"), ~c(scale(.x))) %>%
    na.omit() ->
    TestDF
  
  if(Resps[i] == "Isospora") TestDF <- TestDF %>% filter(!Month == 10) %>% droplevels
  
  TestDF <- TestDF[TestDF[,Resps[i]] <= EctoLimits[i],]
  
  TestDF -> AdultTestDFList[[Resps[i]]]
  
  N <- nrow(TestDF); print(N)
  
  if(Resps[i] %in% c("Eimeria", "Isospora")){
    
    IM1 <- INLAModelAdd(Data = TestDF, 
                        Response = Resps[i],
                        Explanatory = AdultCovar %>% setdiff(c("Year", "AgeCat")), 
                        Add = SocCovar, 
                        Random = c("Tattoo", "fYear"), 
                        RandomModel = rep('iid', 2),
                        Clashes = ClashList,
                        Family = FamilyList[i])
    
  }else if(Resps[i] %in% c("Ticks")){
    
    IM1 <- INLAModelAdd(Data = TestDF, 
                        Response = Resps[i],
                        Explanatory = AdultCovar, 
                        Add = SocCovar, 
                        Random = c("Tattoo", "fYear"), 
                        RandomModel = rep('iid', 2),
                        Clashes = ClashList,
                        Family = FamilyList[i])
    
  }else{
    
    IM1 <- INLAModelAdd(Data = TestDF, 
                        Response = Resps[i],
                        Explanatory = AdultCovar, 
                        Add = SocCovar, 
                        Random = c("Tattoo", "fYear"), 
                        RandomModel = rep('iid', 2),
                        Clashes = ClashList,
                        Family = FamilyList[i])
    
  }
  
  AdultIMList[[Resps[i]]] <- IM1
  
  beepr::beep()
  
}

saveRDS(AdultTestDFList, file = "Output Files/AdultTestDFList.rds")

# Only Youngs ####

YoungCovar <- c("AgeCat", "Sex", "Year", "Month", "BCI")

YoungIMList <- YoungTestDFList <- list()

i <- 1

for(i in (i:length(Resps))){
  
  print(Resps[i])
  
  Badgers %>% 
    filter(!AgeCat == "A") %>%
    dplyr::select(Tattoo, X, Y, Resps[i], YoungCovar, SocCovar) %>% 
    droplevels %>%
    mutate(fYear = as.factor(Year), Month = as.factor(Month)) %>%
    mutate_at(c(SocCovar %>% setdiff("Soil"), "Year"), ~c(scale(.x))) %>%
    na.omit() ->
    TestDF
  
  if(Resps[i] == "Isospora") TestDF <- TestDF %>% filter(!Month == 10) %>% droplevels
  
  TestDF <- TestDF[TestDF[,Resps[i]] <= EctoLimits[i],]
  
  TestDF -> YoungTestDFList[[Resps[i]]]
  
  N <- nrow(TestDF); print(N)
  
  if(Resps[i] %in% c("Eimeria", "Isospora")){
    
    IM1 <- INLAModelAdd(Data = TestDF, 
                        Response = Resps[i],
                        Explanatory = YoungCovar %>% setdiff(c("Year", "AgeCat")), 
                        Add = SocCovar, 
                        Random = c("Tattoo", "fYear"), 
                        RandomModel = rep('iid', 2),
                        Clashes = ClashList,
                        Family = FamilyList[i])
    
  }else{
    
    IM1 <- INLAModelAdd(Data = TestDF, 
                        Response = Resps[i],
                        Explanatory = YoungCovar %>% setdiff("AgeCat"), 
                        Add = SocCovar, 
                        Random = c("Tattoo", "fYear"), 
                        RandomModel = rep('iid', 2),
                        Clashes = ClashList,
                        Family = FamilyList[i])
    
  }
  
  YoungIMList[[Resps[i]]] <- IM1
  
  beepr::beep()
  
}

saveRDS(YoungIMList, file = "Output Files/YoungIMList.rds")
