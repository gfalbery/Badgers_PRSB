
# Badger Survival Models ####

{
  
  library(igraph); library(tidyverse); library(INLA); library(cowplot); library(ggregplot)
  
  theme_set(theme_cowplot())
  
}

# Adults ####

SurvCovar <- c("AgeCat", "Sex", "Year", "Age", "BCI")[2:5]

EctoLimits <- c(30, 15, 1, 1, 1)

SocCovar <- c("Degree",
              "GroupSize",
              "LifetimeDensity", "TrappingDensity",
              "AnnualDensity", 
              "PopN")

ClashList <- 
  # list(SocCovar[3:4], SocCovar[5:8])
  list(SocCovar[3:5])

SurvivalTestDFList <- SurvivalIMList <- list()

i <- 1

inla.setOption(num.threads = 8)

for(i in (i:length(Resps))){
  
  print(Resps[i])
  
  Badgers %>% dplyr::select(Tattoo, X, Y, Resps[i], SurvCovar, AgeCat, SocCovar, "Survived") %>%
    droplevels %>%
    filter(AgeCat == "A") %>%
    mutate(fYear = as.factor(Year)) %>%
    mutate_at(c(SocCovar, "Year", "Age"), ~c(scale(.x))) %>%
    na.omit() ->
    TestDF
  
  if(Resps[i] == "Fleas") TestDF %>% mutate_at("Fleas", ~log(.x + 1)) -> TestDF
  if(Resps[i] == "Lice") TestDF %>% mutate_at("Lice", ~log(.x + 1)) -> TestDF
  
  # if(Resps[i] == "Isospora") TestDF <- TestDF %>% filter(!Month == 10) %>% droplevels
  
  TestDF <- TestDF[TestDF[,Resps[i]] <= EctoLimits[i],]
  
  TestDF -> SurvivalTestDFList[[Resps[i]]]
  
  N <- nrow(TestDF); print(N)
  
  if(Resps[i] %in% c("Eimeria", "Isospora")){
    
    IM1 <- INLAModelAdd(Data = TestDF,
                        Response = "Survived",
                        Explanatory = SurvCovar %>% setdiff("Year"),
                        Add = SocCovar %>% c(Resps[i]),
                        Random = c("Tattoo", "fYear")[2],
                        RandomModel = rep('iid', 2)[2],
                        Clashes = ClashList,
                        Family = "binomial")
    
    
  }else{
    
    IM1 <- INLAModelAdd(Data = TestDF,
                        Response = "Survived",
                        Explanatory = SurvCovar,
                        Add = SocCovar %>% c(Resps[i]),
                        Random = c("Tattoo", "fYear")[2],
                        RandomModel = rep('iid', 2)[2],
                        Clashes = ClashList,
                        Family = "binomial")
    
  }
  
  SurvivalIMList[[Resps[i]]] <- IM1
  
}

saveRDS(SurvivalIMList, file = "Output Files/SurvivalIMList.rds")
saveRDS(SurvivalTestDFList, file = "Output Files/SurvivalTestDFList.rds")

# SurvivalIMList <- readRDS("Output Files/SurvivalIMList.rds")

# Youngs ####

SurvCovar <- c("AgeCat", "Sex", "Year", "BCI")

EctoLimits <- c(30, 15, 1, 1, 1)

SocCovar <- c("Degree",
              "GroupSize",
              "LifetimeDensity", "TrappingDensity",
              "AnnualDensity", 
              "PopN")

ClashList <- 
  # list(SocCovar[3:4], SocCovar[5:8])
  list(SocCovar[3:5])

YoungSurvivalTestDFList <- YoungSurvivalIMList <- list()

i <- 2

inla.setOption(num.threads = 8)

for(i in (i:length(Resps))){
  
  print(Resps[i])
  
  Badgers %>% dplyr::select(Tattoo, X, Y, Resps[i], SurvCovar, AgeCat, SocCovar, "Survived") %>%
    filter(!AgeCat == "A") %>%
    mutate(fYear = as.factor(Year)) %>%
    mutate_at(c(SocCovar, "Year"), ~c(scale(.x))) %>%
    droplevels %>%
    na.omit() ->
    TestDF
  
  if(Resps[i] == "Fleas") TestDF %>% mutate_at("Fleas", ~log(.x + 1)) -> TestDF
  if(Resps[i] == "Lice") TestDF %>% mutate_at("Lice", ~log(.x + 1)) -> TestDF
  
  # if(Resps[i] == "Isospora") TestDF <- TestDF %>% filter(!Month == 10) %>% droplevels
  
  TestDF <- TestDF[TestDF[,Resps[i]] <= EctoLimits[i],]
  
  TestDF -> YoungSurvivalTestDFList[[Resps[i]]]
  
  N <- nrow(TestDF); print(N)
  
  if(Resps[i] %in% c("Eimeria", "Isospora")){
    
    IM1 <- INLAModelAdd(Data = TestDF,
                        Response = "Survived",
                        Explanatory = SurvCovar %>% setdiff("Year"),
                        Add = c(Resps[i]),
                        Random = c("Tattoo", "fYear")[2],
                        RandomModel = rep('iid', 2)[2],
                        Clashes = ClashList,
                        Family = "binomial")
    
    
  }else{
    
    IM1 <- INLAModelAdd(Data = TestDF,
                        Response = "Survived",
                        Explanatory = SurvCovar,
                        Add = c(Resps[i]),
                        Random = c("Tattoo", "fYear")[2],
                        RandomModel = rep('iid', 2)[2],
                        Clashes = ClashList,
                        Family = "binomial")
    
  }
  
  YoungSurvivalIMList[[Resps[i]]] <- IM1
  
}

saveRDS(YoungSurvivalIMList, file = "Output Files/YoungSurvivalIMList.rds")

# Cubs ####

CubSurvCovar <- c("AgeCat", "Sex", "Year", "BCI")[2:4]

SocCovar <- c("Degree",
              "GroupSize",
              "LifetimeDensity", "TrappingDensity",
              "AnnualDensity", 
              "PopN")

ClashList <- 
  # list(SocCovar[3:4], SocCovar[5:8])
  list(SocCovar[3:5])

CubSurvivalTestDFList <- CubSurvivalIMList <- list()

i <- 1

inla.setOption(num.threads = 8)

for(i in (i:length(Resps))){
  
  print(Resps[i])
  
  Badgers %>% dplyr::select(Tattoo, X, Y, Resps[i], CubSurvCovar, AgeCat, SocCovar, "Survived") %>%
    droplevels %>%
    filter(AgeCat == "C") %>%
    mutate(fYear = as.factor(Year)) %>%
    mutate_at(c(SocCovar, "Year"), ~c(scale(.x))) %>%
    na.omit() ->
    TestDF
  
  if(Resps[i] == "Fleas") TestDF %>% mutate_at("Fleas", ~log(.x + 1)) -> TestDF
  if(Resps[i] == "Lice") TestDF %>% mutate_at("Lice", ~log(.x + 1)) -> TestDF
  
  # if(Resps[i] == "Isospora") TestDF <- TestDF %>% filter(!Month == 10) %>% droplevels
  
  TestDF <- TestDF[TestDF[,Resps[i]] <= EctoLimits[i],]
  
  TestDF -> CubSurvivalTestDFList[[Resps[i]]]
  
  N <- nrow(TestDF); print(N)
  
  if(Resps[i] %in% c("Eimeria", "Isospora")){
    
    IM1 <- INLAModelAdd(Data = TestDF,
                        Response = "Survived",
                        Explanatory = CubSurvCovar %>% setdiff("Year"),
                        Add = SocCovar %>% c(Resps[i]),
                        Random = c("Tattoo", "fYear")[2],
                        RandomModel = rep('iid', 2)[2],
                        Clashes = ClashList,
                        Family = "binomial")
    
    
  }else{
    
    IM1 <- INLAModelAdd(Data = TestDF,
                        Response = "Survived",
                        Explanatory = CubSurvCovar,
                        Add = SocCovar %>% c(Resps[i]),
                        Random = c("Tattoo", "fYear")[2],
                        RandomModel = rep('iid', 2)[2],
                        Clashes = ClashList,
                        Family = "binomial")
    
  }
  
  CubSurvivalIMList[[Resps[i]]] <- IM1
  
}

saveRDS(CubSurvivalIMList, file = "Output Files/CubSurvivalIMList.rds")

   CubSurvivalIMList <- readRDS("Output Files/CubSurvivalIMList.rds")

# Yearlings ####

YearlingSurvCovar <- c("AgeCat", "Sex", "Year", "Age")[2:3]

SocCovar <- c("Degree",
              "GroupSize",
              "LifetimeDensity", "TrappingDensity",
              "AnnualDensity", "AnnualDensityt0",
              "AnnualTrappingDensity", "AnnualTrappingDensityt0",
              "PopN", "PopNt0")

ClashList <- 
  # list(SocCovar[3:4], SocCovar[5:8])
  list(SocCovar[3:8])

YearlingSurvivalTestDFList <- YearlingSurvivalIMList <- list()

i <- 1

inla.setOption(num.threads = 8)

for(i in (i:length(Resps))){
  
  print(Resps[i])
  
  Badgers %>% dplyr::select(Tattoo, X, Y, Resps[i], YearlingSurvCovar, AgeCat, SocCovar, "Survived") %>%
    droplevels %>%
    filter(AgeCat == "Y") %>%
    mutate(fYear = as.factor(Year)) %>%
    mutate_at(c(SocCovar, "Year"), ~c(scale(.x))) %>%
    na.omit() ->
    TestDF
  
  if(Resps[i] == "Fleas") TestDF %>% mutate_at("Fleas", ~log(.x + 1)) -> TestDF
  if(Resps[i] == "Lice") TestDF %>% mutate_at("Lice", ~log(.x + 1)) -> TestDF
  
  # if(Resps[i] == "Isospora") TestDF <- TestDF %>% filter(!Month == 10) %>% droplevels
  
  TestDF <- TestDF[TestDF[,Resps[i]] <= EctoLimits[i],]
  
  TestDF -> YearlingSurvivalTestDFList[[Resps[i]]]
  
  N <- nrow(TestDF); print(N)
  
  if(Resps[i] %in% c("Eimeria", "Isospora")){
    
    IM1 <- INLAModelAdd(Data = TestDF,
                        Response = "Survived",
                        Explanatory = YearlingSurvCovar %>% setdiff("Year"),
                        Add = SocCovar %>% c(Resps[i]),
                        Random = c("Tattoo", "fYear")[2],
                        RandomModel = rep('iid', 2)[2],
                        Clashes = ClashList,
                        Family = "binomial")
    
    
  }else{
    
    IM1 <- INLAModelAdd(Data = TestDF,
                        Response = "Survived",
                        Explanatory = YearlingSurvCovar,
                        Add = SocCovar %>% c(Resps[i]),
                        Random = c("Tattoo", "fYear")[2],
                        RandomModel = rep('iid', 2)[2],
                        Clashes = ClashList,
                        Family = "binomial")
    
  }
  
  YearlingSurvivalIMList[[Resps[i]]] <- IM1
  
}

saveRDS(YearlingSurvivalIMList, file = "Output Files/YearlingSurvivalIMList.rds")

# SurvivalIMList <- readRDS("Output Files/SurvivalIMList.rds")
