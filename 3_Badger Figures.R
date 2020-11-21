
# Badger Figures ####

{
  
  library(igraph); library(tidyverse); library(INLA); library(cowplot); library(ggregplot);
  library(colorspace); library(patchwork); library(ggimage); library(MCMCglmm)
  
  theme_set(theme_cowplot())
  
  ParasitePalettes <- AlberColours[c(2, 1, 3)] # c("PuRd","PuBu","BuGn","Purples","Oranges")
  ParasiteColours <- AlberColours[c(2, 1, 3)]  %>% c("#DD1c77","#2B8CBE","#2CA25F",
                                                     RColorBrewer::brewer.pal(5,"Purples")[4],
                                                     RColorBrewer::brewer.pal(5,"Oranges")[4])
  
}

IMList <- readRDS("Output Files/IMList.rds")
AdultIMList <- readRDS("Output Files/AdultIMList.rds")
YoungIMList <- readRDS("Output Files/YoungIMList.rds")

# Figure 1: Density Maps ####

SPDF <- SpatialPointsDataFrame(data = LifetimeCentroids[,c("XCentroidLifetime", "YCentroidLifetime")],
                               coords = LifetimeCentroids[,c("XCentroidLifetime", "YCentroidLifetime")])

LifetimeKUDL <- kernelUD(SPDF, same4all = TRUE, grid = 500)

LifetimeKUDL@coords %>% as.data.frame() %>%
  mutate(Density = LifetimeKUDL@data %>% unlist) -> DensityTileDF

Lims <- lims(x = c(-0.2, 3)*100, y = c(-0.2, 2.2)*100)

DensityTileDF %>%
  filter(Density>min(Badgers$LifetimeDensity)) %>%
  ggplot(aes(Var2, Var1, fill = Density)) +
  # geom_tile(size = 1, colour = AlberColours[["Blue"]]) + 
  geom_tile() + 
  coord_fixed() +
  labs(x = "Easting", y = "Northing") +
  Lims ->
  
  DensityTilePlot

Badgers %>%
  ggplot(aes(X, Y)) +
  geom_point(aes(colour = LifetimeDensity), size = 7) + #, size = LifetimeDensity)) +
  coord_fixed() +
  labs(x = "Easting", y = "Northing") +
  theme(legend.position = "none") ->
  
  DensityPointPlot

#SpatialBadgerModels <- readRDS("~/BadgeWorks/Output Files/SpatialBadgerModels.rds")
#load("~/BadgeWorks/Output Files/SpatialBadgerData.Rdata")

SpatialParasitePalettes <- c(AlberPalettes[[2]], "Peach", AlberPalettes[[1]], AlberPalettes[[3]])

names(SpatialParasitePalettes) <- Resps[1:4]

SpatialBadgerModels %>% names %>% magrittr::extract(1:4) %>%
  
  map(~ggField(SpatialBadgerModels[[.x]]$SPDE,
               #Res = 100,
               Mesh = MeshList[[.x]]) +
        labs(fill = .x)) -> FieldList

.x <- "Lice"

FieldList[[2]] <- ggField(SpatialBadgerModels[[.x]]$SPDE,
                          FillAlpha = T,
                          #Res = 100,
                          Mesh = MeshList[[.x]]) +
  labs(fill = .x) + guides(alpha = F)

BadgerPoint <- data.frame(
  
  X = 250,
  Y = 190,
  # Image = "InvertedBadgerPic.png"
  Image = "Data/SmallBadgerPic.png"
  
)

Inflate <- 1.5

(((DensityTilePlot + 
     scale_x_continuous(breaks = c(0:3*100), labels = c(0:3), limits = c(-20, 300)) +
     scale_y_continuous(breaks = c(0:5*50), labels = c(0:5/2), limits = c(-20, 220)) +
     geom_image(data = BadgerPoint, inherit.aes = F,
                aes(x = X, y = Y, image = Image),
                size = 0.35) + theme(legend.position = "none") +
     scale_fill_continuous_sequential(palette = AlberPalettes[[1]])|
     
     (DensityPointPlot + 
        scale_x_continuous(breaks = c(0:3*100), labels = c(0:3), limits = c(-20, 300)) +
        scale_y_continuous(breaks = c(0:5*50), labels = c(0:5/2), limits = c(-20, 220)) +
        theme(legend.position = c(0.85, 0.75)) + 
        labs(colour = "Density") + guides(colour = guide_colorbar()) +
        scale_colour_continuous_sequential(palette = AlberPalettes[[1]]))))/
    
    ((FieldList[[1]] +
        scale_x_continuous(breaks = c(0:3*100), labels = c(0:3), limits = c(-20, 300)) +
        scale_y_continuous(breaks = c(0:5*50), labels = c(0:5/2), limits = c(-20, 220)) +
        guides(fill = guide_legend(reverse = F,
                                   direction = "horizontal",
                                   title.position = "left",
                                   title.vjust = 0.25, title.hjust = 1,
                                   label.position = "top",
                                   label.hjust = 0.5,
                                   label.vjust = 1.5,
                                   label.theme = element_text(angle = 0), nrow = 1)) +
        theme(legend.position = "top") + scale_fill_discrete_sequential(SpatialParasitePalettes[[1]])) |
       (FieldList[[2]] +
          scale_x_continuous(breaks = c(0:3*100), labels = c(0:3), limits = c(-20, 300)) +
          scale_y_continuous(breaks = c(0:5*50), labels = c(0:5/2), limits = c(-20, 220)) +
          guides(fill = guide_legend(reverse = F,
                                     direction = "horizontal",
                                     title.position = "left",
                                     title.vjust = 0.25, title.hjust = 1,
                                     label.position = "top",
                                     label.hjust = 0.5,
                                     label.vjust = 1.5,
                                     label.theme = element_text(angle = 0), nrow = 1)) +
          theme(legend.position = "top") + scale_fill_discrete_sequential(SpatialParasitePalettes[[2]])))/
    # theme(legend.position = "top") + scale_fill_discrete_sequential("Oranges")))/
    
    ((FieldList[[3]] +
        scale_x_continuous(breaks = c(0:3*100), labels = c(0:3), limits = c(-20, 300)) +
        scale_y_continuous(breaks = c(0:5*50), labels = c(0:5/2), limits = c(-20, 220)) +
        guides(fill = guide_legend(reverse = F,
                                   direction = "horizontal",
                                   title.position = "left",
                                   title.vjust = 0.25, title.hjust = 1,
                                   label.position = "top",
                                   label.hjust = 0.5,
                                   label.vjust = 1.5,
                                   label.theme = element_text(angle = 0), nrow = 1)) +
        theme(legend.position = "top") + scale_fill_discrete_sequential("PuBu")) |
       (FieldList[[4]] +
          scale_x_continuous(breaks = c(0:3*100), labels = c(0:3), limits = c(-20, 300)) +
          scale_y_continuous(breaks = c(0:5*50), labels = c(0:5/2), limits = c(-20, 220)) +
          guides(fill = guide_legend(reverse = F,
                                     direction = "horizontal",
                                     title.position = "left",
                                     title.vjust = 0.25, title.hjust = 1,
                                     label.position = "top",
                                     label.hjust = 0.5,
                                     label.vjust = 1.5,
                                     label.theme = element_text(angle = 0), nrow = 1)) +
          theme(legend.position = "top") +
          scale_fill_discrete_sequential(SpatialParasitePalettes[[4]])))) +
  plot_layout(guides = "keep") +
  plot_annotation(tag_levels = "A") +
  ggsave(filename = "Figures/Figure1.jpeg",
         units = "mm", height = 220*Inflate, width = 180*Inflate, dpi = 600)

# Figure 2: Density-Parasite Trends ####

Covar <-  c("AgeCat", "Sex", "Year", "Month", "BCI")

if(file.exists("OutputModels.rds")){
  
  OutputModels <- readRDS("OutputModels.rds")
  SlopeDrawDF <- readRDS("SlopeDrawDF.rds")
  InterceptDrawDF <- readRDS("InterceptDrawDF.rds")
  
}else{
  
  OutputModels <- list(IMList$Fleas,
                       YoungIMList$Lice,
                       IMList$Ticks,
                       IMList$Eimeria) %>% 
    map("FinalModel")
  
  DensityVars <- c("LifetimeDensity", "TrappingDensity", 
                   "LifetimeDensity", "TrappingDensity")
  
  1:length(DensityVars) %>% map(~{
    
    print(.x)
    
    OutputModels[[.x]]$marginals.fixed[[DensityVars[.x]]] %>% 
      inla.rmarginal(50, .)
    
  }) -> SlopeDrawDF
  
  CovarList <-
    
    list(
      
      Fleas = c(Covar, "LifetimeDensity"),
      Lice = c(Covar, "TrappingDensity"),
      Ticks = c(Covar, "LifetimeDensity"), # c("AgeCat", "Sex", "Year", "Month", "BCI", "LifetimeDensity"),
      Eimeria = c(Covar, "TrappingDensity")
      
    )
  
  TestDFList <- readRDS("Output Files/TestDFList.rds")
  YoungTestDFList <- readRDS("Output Files/YoungTestDFList.rds")
  
  DFList <- list(TestDFList$Fleas,
                 YoungTestDFList$Lice,
                 TestDFList$Ticks,
                 TestDFList$Eimeria)
  
  1:length(DensityVars) %>%
    
    map(~OutputModels[[.x]] %>%
          INLAFit(Model = .,
                  TestDF = DFList[[.x]],
                  FixedCovar = CovarList[[.x]],
                  Draw = T, NDraw = 50) %>% sapply(mean) %>% return
        
    ) -> InterceptDrawDF
  
  names(SlopeDrawDF) <- names(InterceptDrawDF) <- names(OutputModels) <- c("Fleas", "Lice", "Ticks", "Eimeria")
  
  saveRDS(InterceptDrawDF, file = "InterceptDrawDF.rds")
  saveRDS(SlopeDrawDF, file = "SlopeDrawDF.rds")
  saveRDS(OutputModels, file = "OutputModels.rds")
  
}

YJitter <- 0
PYPosition <- 1.1

{
  
  YoungCovar <- Covar <- c("AgeCat", "Sex", "Year", "Month", "BCI")
  EctoLimits <- c(30, 15, 1, 1, 1)
  
  SocCovar <- c("Degree", 
                "GroupSize",
                "LifetimeDensity", "TrappingDensity",
                "AnnualDensity",
                "PopN")
  
  # Fleas
  
  X <- seq(min(c(scale(Badgers$LifetimeDensity))),
           max(c(scale(Badgers$LifetimeDensity))),
           length.out = 100)
  
  A <- mean(InterceptDrawDF$Fleas) #GetEstimates(OutputModels$Fleas, "(Intercept)", Mode = "Numeric")$mean # Flea count log intercept
  
  B <- A + X*
    GetEstimates(Model = OutputModels$Fleas, Variable = "LifetimeDensity", "Numeric")$mean
  
  FleaSlope <- data.frame(LifetimeDensity = X,
                          Fleas = exp(B))
  
  XLoc2 <- min(FleaSlope$LifetimeDensity) + diff(range(FleaSlope$LifetimeDensity))/2
  
  GetEstimates(Model = OutputModels$Fleas, Variable = "LifetimeDensity") %>%
    paste0("; P < 0.0001") -> Label2
  
  1:length(SlopeDrawDF$Fleas) %>%
    map_dfr(~data.frame(Fleas = exp(SlopeDrawDF$Fleas[[.x]]*X + InterceptDrawDF$Fleas[[.x]]), LifetimeDensity = X, N = 1:100), .id = "Draw") ->
    FleaSlopeDraws
  
  FleaSlopeDraws %>% filter(N%in%c(1, 100)) -> FleaSlopeDraws
  
  Badgers %>% RandomSlice %>% rownames_to_column %>%
    filter(Fleas < EctoLimits[[1]]) %>%
    mutate_at("LifetimeDensity", ~c(scale(.x))) %>%
    ggplot(aes(LifetimeDensity, log(Fleas + 1))) +
    geom_point(alpha = 0.3, aes(colour = rowname)) +
    geom_line(data = FleaSlopeDraws, aes(group = as.factor(Draw)), alpha = 0.1) +
    labs(x = "Lifetime density") +
    theme(legend.position = "none") +
    scale_colour_discrete_sequential(palette = AlberPalettes[[2]]) +
    geom_line(data = FleaSlope) +
    geom_text(data = NULL, aes(XLoc2, log(EctoLimits[[1]] + 1)*1.1), 
              label = Label2, hjust = 0.5,
              colour = AlberColours[[2]]) ->
    DensityFleaPlotFit
  
  # Lice
  
  YoungBadgerDF <-
    Badgers %>%
    filter(!AgeCat == "A") %>%
    dplyr::select(Tattoo, X, Y, Resps[2], YoungCovar, SocCovar) %>%
    droplevels %>%
    mutate(fYear = as.factor(Year), Month = as.factor(Month)) %>%
    mutate_at(c(SocCovar, "Year"), ~c(scale(.x))) %>%
    na.omit()
  
  X <- seq(min(c(scale(YoungBadgerDF$TrappingDensity))),
           max(c(scale(YoungBadgerDF$TrappingDensity))),
           length.out = 100)
  
  A <- mean(InterceptDrawDF$Lice) # GetEstimates(OutputModels$Lice, "(Intercept)", Mode = "Numeric")$mean # Tick prevalence intercept
  
  B <- A + X*
    GetEstimates(Model = OutputModels$Lice, Variable = "TrappingDensity", "Numeric")$mean
  
  LiceSlope <- data.frame(TrappingDensity = X,
                          Lice = exp(B))
  
  XLocLice <- min(LiceSlope$TrappingDensity) + 
    diff(range(LiceSlope$TrappingDensity))/2
  
  INLAPValue(OutputModels[[2]], DensityVars[[2]])
  
  GetEstimates(Model = OutputModels$Lice, Variable = "TrappingDensity") %>%
    paste0("; P = 0.0017") -> LabelLice
  
  1:length(SlopeDrawDF$Lice) %>%
    map_dfr(~data.frame(Lice = exp(SlopeDrawDF$Lice[[.x]]*X + 
                                     InterceptDrawDF$Lice[[.x]]), TrappingDensity = X, N = 1:100), .id = "Draw") ->
    LiceSlopeDraws
  
  LiceSlopeDraws %>% filter(N%in%c(1, 100)) -> LiceSlopeDraws
  
  YoungBadgerDF %>% RandomSlice %>% rownames_to_column %>%
    filter(Lice<EctoLimits[[2]]) %>%
    mutate_at("TrappingDensity", ~c(scale(.x))) %>%
    ggplot(aes(TrappingDensity, log(Lice + 1))) +
    geom_point(alpha = 0.6, aes(colour = rowname)) +
    geom_line(data = LiceSlopeDraws, aes(group = as.factor(Draw)), alpha = 0.1) +
    theme(legend.position = "none") +
    labs(x = "Trapping density") +
    # scale_y_continuous(breaks = c(0:5/5)) +
    scale_colour_discrete_sequential(palette = "Peach", rev = F) +
    geom_line(data = LiceSlope) +
    geom_text(data = NULL, aes(XLocLice, log(EctoLimits[[2]] + 1)*1.1), 
              label = LabelLice, hjust = 0.5,
              colour = last(ParasiteColours)) ->
    DensityLicePlotFit
  
  # Ticks
  
  AdultBadgerDF <-
    Badgers %>%
    #filter(AgeCat == "A") %>%
    dplyr::select(Tattoo, X, Y, Resps[3], Covar, SocCovar) %>%
    droplevels %>%
    mutate(fYear = as.factor(Year), Month = as.factor(Month)) %>%
    mutate_at(c(SocCovar, "Year"), ~c(scale(.x))) %>%
    na.omit()
  
  X <- seq(min(c(scale(AdultBadgerDF$LifetimeDensity))),
           max(c(scale(AdultBadgerDF$LifetimeDensity))),
           length.out = 100)
  
  A <- mean(InterceptDrawDF$Ticks) # GetEstimates(OutputModels$Ticks, "(Intercept)", Mode = "Numeric")$mean # Tick prevalence intercept
  
  B <- A + X*
    GetEstimates(Model = OutputModels$Ticks, Variable = "LifetimeDensity", "Numeric")$mean
  
  1:length(SlopeDrawDF$Ticks) %>%
    map_dfr(~data.frame(Ticks = 
                          logistic(SlopeDrawDF$Ticks[[.x]]*X +
                                     InterceptDrawDF$Ticks[[.x]]), LifetimeDensity = X, N = 1:100), .id = "Draw") ->
    TickSlopeDraws
  
  TickSlope <- data.frame(LifetimeDensity = X,
                          Ticks = logistic(B))
  
  XLoc1 <- min(TickSlope$LifetimeDensity) + diff(range(TickSlope$LifetimeDensity))/2
  
  GetEstimates(Model = OutputModels$Ticks, Variable = "LifetimeDensity") %>%
    paste0("; P = 0.0038") -> Label1
  
  AdultBadgerDF %>% RandomSlice %>% rownames_to_column %>%
    mutate_at("LifetimeDensity", ~c(scale(.x))) %>%
    ggplot(aes(LifetimeDensity, Ticks)) +
    geom_point(alpha = 0.3, aes(colour = rowname, y = scales::rescale(Ticks, c(0, 0.091))), position = position_jitter(h = YJitter)) +
    geom_line(data = TickSlopeDraws, aes(group = as.factor(Draw)), alpha = 0.1) +
    theme(legend.position = "none") +
    labs(x = "Lifetime density") +
    scale_y_continuous(breaks = c(0:5/50)) +
    scale_colour_discrete_sequential(palette = AlberPalettes[[1]]) +
    geom_line(data = TickSlope) +
    geom_text(data = NULL, aes(XLoc1, PYPosition-1, label = Label1), hjust = 0.5,
              colour = AlberColours[[1]]) ->
    
    DensityTickPlotFit
  
  # Eimeria
  
  X <- seq(min(c(scale(Badgers$TrappingDensity))),
           max(c(scale(Badgers$TrappingDensity))),
           length.out = 100)
  
  A <- mean(InterceptDrawDF$Eimeria) # GetEstimates(OutputModels$Eimeria, "(Intercept)", Mode = "Numeric")$mean # Flea count log intercept
  
  B <- A + X*
    GetEstimates(Model = OutputModels$Eimeria, Variable = "TrappingDensity", "Numeric")$mean
  
  EimeriaSlope <- data.frame(TrappingDensity = X,
                             Eimeria = logistic(B))
  
  1:length(SlopeDrawDF$Eimeria) %>%
    map_dfr(~data.frame(Eimeria = logistic(SlopeDrawDF$Eimeria[[.x]]*X +
                                             InterceptDrawDF$Eimeria[[.x]]), TrappingDensity = X, N = 1:100), .id = "Draw") ->
    EimeriaSlopeDraws
  
  XLoc3 <- min(EimeriaSlope$TrappingDensity) + diff(range(EimeriaSlope$TrappingDensity))/2
  
  GetEstimates(Model = OutputModels$Eimeria, Variable = "TrappingDensity") %>%
    paste0("; P = 0.0053") -> Label3
  
  Badgers %>% RandomSlice %>% rownames_to_column %>%
    mutate_at("TrappingDensity", ~c(scale(.x))) %>%
    ggplot(aes(TrappingDensity, Eimeria)) +
    geom_point(alpha = 0.6, aes(colour = rowname), position = position_jitter(h = YJitter)) +
    geom_line(data = EimeriaSlopeDraws, aes(group = as.factor(Draw)), alpha = 0.1) +
    theme(legend.position = "none") +
    labs(x = "Trapping density") +
    scale_y_continuous(breaks = c(0:5/5)) +
    scale_colour_discrete_sequential(palette = AlberPalettes[[3]]) +
    geom_line(data = EimeriaSlope, inherit.aes = F,
              aes(TrappingDensity, Eimeria)) +
    geom_text(data = NULL, aes(XLoc3, PYPosition, label = Label3), hjust = 0.5,
              colour = AlberColours[[3]]) ->
    
    EimeriaDensityPlot
  
  (DensityFleaPlotFit|DensityLicePlotFit)/(DensityTickPlotFit|EimeriaDensityPlot) +
    plot_annotation(tag_levels = "A") +
    # ggsave("Figures/Figure2Tetrad.pdf", units = "mm", height = 200, width = 200, dpi = 600) +
    ggsave("Figures/Figure2Tetrad.jpeg", units = "mm", height = 200, width = 200, dpi = 600)
  
}

# Figure 3: Multivariate + Survival ####

# Multivariate model

FleaMultivariate <- readRDS("Output Files/FleaMultivariate.rds")
AdultTickMultivariate <- readRDS("Output Files/TickMultivariate.rds")
EimeriaMultivariate <- readRDS("Output Files/EimeriaMultivariate.rds")
CubLiceMultivariate <- readRDS("Output Files/YoungLiceMultivariate.rds")

list(FleaMultivariate, CubLiceMultivariate, AdultTickMultivariate, EimeriaMultivariate) %>%
  map_dfr(MultivCovariance, .id = "Model") %>% filter(!Component == "fYear") %>%
  mutate_at(c("Var1", "Var2"), ~.x %>% str_remove("LifetimeDensity")) %>%
  mutate_at("Component", ~.x %>% str_replace_all(c("Tattoo" = "Between-\nindividual",
                                                   "units" = "Within-\nindividual"))) %>%
  mutate(Parasite = paste0(Var1, Var2) %>% str_remove(".1")) ->
  CovarianceEstimates

CovarianceEstimates %>%
  ggplot(aes(Model, Mode, colour = Component)) +
  geom_hline(lty = 2, alpha = 0.3, yintercept = 0) +
  geom_point(position = position_dodge(w = 0.5), aes(shape = Component), size = 3) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper),
                position = position_dodge(w = 0.5),
                width = 0.3) +
  scale_x_discrete(limits = c(4:1) %>% rev,
                   labels = (c("Fleas", "Lice", "Ticks", "Eimeria"))) +
  labs(x = "Model", y = "Density-parasite covariance", colour = NULL, shape = NULL) +
  scale_colour_manual(values = c(ParasiteColours[[6]], "pale green")) +
  scale_shape_manual(values = c(16, 1)) +
  theme(legend.position = c(0.1, 0.2), legend.text = element_text(size = 10)) ->
  
  CovarianceEffects

YoungSurvivalIMList <- readRDS("Output Files/YoungSurvivalIMList.rds")
YoungSurvivalTestDFList <- YoungSurvivalIMList %>% map("Data")

{
  
  YJitter <- 0
  PYPosition <- 1.1
  
  SurvCovar <- c("AgeCat", "Sex", "Year", "BCI")
  
  Badgers %>%
    dplyr::select(Tattoo, X, Y, Resps[2], SurvCovar, "AgeCat", SocCovar, "Survived") %>%
    droplevels %>%
    filter(!AgeCat == "A") %>%
    mutate(fYear = as.factor(Year)) %>%
    mutate_at(c(SocCovar, "Year"), ~c(scale(.x))) %>%
    na.omit() ->
    AdultSurvivalBadgers
  
  X <- seq(0,
           max(log(AdultSurvivalBadgers$Lice + 1)),
           length.out = 100)
  
  INLAFit(Model = YoungSurvivalIMList$Lice$FinalModel,
          TestDF = YoungSurvivalTestDFList$Lice,
          FixedCovar = SurvCovar %>% c("Lice", "TrappingDensity"),
          Draw = T, NDraw = 50) %>% sapply(mean) ->
    
    SurvivalIntercepts
  
  A <- mean(SurvivalIntercepts) # Tick prevalence intercept
  
  B <- A + X*
    (GetEstimates(Model = YoungSurvivalIMList$Lice$FinalModel,
                  Variable = "Lice", Mode = "Numeric")[1] %>% unlist)
  
  SurvivalLiceSlope <- data.frame(Lice = X,
                                  Survived = logistic(B))
  
  XLocSurvival1 <- min(SurvivalLiceSlope$Lice) + diff(range(SurvivalLiceSlope$Lice))/2
  
  INLAPValue(YoungSurvivalIMList$Lice$FinalModel, "Lice")
  
  GetEstimates(Model = YoungSurvivalIMList$Lice$FinalModel,
               Variable = "Lice") %>%
    paste0("; P = 0.038") -> SurvivalLabel1
  
  YoungSurvivalIMList$Lice$FinalModel$marginals.fixed[["Lice"]] %>% inla.rmarginal(50, .) ->
    SurvivalSlopeDraws
  
  1:length(SurvivalSlopeDraws) %>%
    map_dfr(~data.frame(Survived = logistic(SurvivalSlopeDraws[[.x]]*X +
                                              SurvivalIntercepts[[.x]]),
                        Lice = X, N = 1:100), .id = "Draw") ->
    SurvivalSlopeDF
  
  AdultSurvivalBadgers %>%
    mutate_at("Lice", ~log(.x + 1)) %>%
    RandomSlice %>% rownames_to_column %>%
    ggplot(aes(Lice, Survived)) +
    geom_point(alpha = 0.3, aes(colour = rowname), position = position_jitter(h = YJitter)) +
    geom_line(data = SurvivalSlopeDF, aes(group = as.factor(Draw)), alpha = 0.1) +
    theme(legend.position = "none") +
    labs(x = "Log(Lice + 1)") +
    scale_y_continuous(breaks = c(0:5/5)) +
    scale_colour_discrete_sequential(palette = "Mint") +
    geom_line(data = SurvivalLiceSlope) +
    geom_text(data = NULL, aes(XLocSurvival1, PYPosition, label = SurvivalLabel1), hjust = 0.5,
              colour = ParasiteColours[[6]]) ->
    SurvivalLicePlotFit
  
  (SurvivalLicePlotFit|CovarianceEffects) +
    plot_annotation(tag_levels = "A") +
    ggsave("Figures/Figure3Diptych.jpeg", units = "mm", height = 100, width = 250*(2/3), dpi = 600)
  
}

# Supplement #####

ParasiteColours2 <- 
  c(AlberColours[2], 
    RColorBrewer::brewer.pal(5,"Oranges")[3],
    AlberColours[1],
    AlberColours[3],
    "black")

# Figure SI1: Overall model outputs ####

IMList %>% map("FinalModel") %>% 
  Efxplot(ModelNames = names(IMList), Intercept = F,
          PointOutline = T
  ) + 
  scale_colour_manual(values = ParasiteColours2) +
  ggsave("Figures/Figure SI1.jpeg",
         units = "mm", height = 120, width = 150)

# Figure SI2: Adult model outputs ####

AdultIMList %>% map("FinalModel") %>% 
  Efxplot(ModelNames = names(IMList), Intercept = F,
          PointOutline = T
  ) + 
  scale_colour_manual(values = ParasiteColours2) +
  ggsave("Figures/Figure SI2.jpeg",
         units = "mm", height = 120, width = 150)

# Figure SI3: Juvenile model outputs ####

YoungIMList %>% map("FinalModel") %>% 
  Efxplot(ModelNames = names(IMList), Intercept = F,
          PointOutline = T
  ) + 
  scale_x_discrete(limits = rev(c("SexMale", "Year", glue::glue("Month0{6:9}"), glue::glue("Month1{0:1}"), "BCI", "LifetimeDensity", "TrappingDensity"))) +  scale_colour_manual(values = ParasiteColours2) +
  ggsave("Figures/Figure SI3.jpeg",
         units = "mm", height = 120, width = 150)


# Figure SI4: Multivariate model outputs ####

FleaMultivariate <- readRDS("Output Files/FleaMultivariate.rds")
AdultTickMultivariate <- readRDS("Output Files/TickMultivariate.rds")
EimeriaMultivariate <- readRDS("Output Files/EimeriaMultivariate.rds")
CubLiceMultivariate <- readRDS("Output Files/YoungLiceMultivariate.rds")

list(FleaMultivariate, CubLiceMultivariate, AdultTickMultivariate, EimeriaMultivariate) %>% 
  map(c(summary, "solutions", as.data.frame, rownames_to_column)) %>% 
  bind_rows(.id = "Model") %>% 
  mutate_at("Model", ~c("Fleas", "Lice", "Ticks","Eimeria")[as.numeric(.x)] %>% 
              factor(levels = c("Fleas", "Lice", "Ticks","Eimeria"))) %>% 
  filter(!str_detect(rowname, "Density")) %>% 
  mutate_at("rowname", ~.x %>% str_split(":") %>% map_chr(last)) %>% 
  filter(!str_detect(rowname, "trait")) %>% 
  rename(Estimate = post.mean, Var = rowname, Lower = `l-95% CI`, Upper = `u-95% CI`) %>% 
  ggplot(aes(Var, Estimate, colour = Model, group = Model)) + 
  geom_hline(lty = 2, alpha = 0.6, yintercept = 0) +
  geom_errorbar(width = 0.5, position = position_dodge(w = 0.5), aes(ymin = Lower, ymax = Upper)) +
  geom_point(colour = "black", size = 2, position = position_dodge(w = 0.5)) +
  geom_point(size = 1.5, position = position_dodge(w = 0.5)) + 
  labs(x = NULL) +
  coord_flip() + 
  scale_colour_manual(values = ParasiteColours2) +
  ggsave("Figures/Figure SI4.jpeg")


# Tables ####

EmptyRows <- function(Table, Rows = 1, Fill = ""){
  
  NCol <- ncol(Table)
  Vars <- colnames(Table)
  
  DF <- matrix(Fill, ncol = NCol, nrow = Rows) %>% as.data.frame
  
  names(DF) <- Vars
  
  DF %<>% mutate_all(as.character)
  
  return(DF)
  
}

# Table SI1: Overall DIC Tables ####

IMList %>% map(c("dDIC", first, list)) %>% 
  map(DICTableGet) %>% 
  map(~.x %>% mutate_at("Delta", ~round(as.numeric(.x), 3))) %>% 
  map(~mutate_all(.x, as.character)) %>% 
  map(~bind_rows(.x, EmptyRows(.x))) %>% 
  bind_rows(.id = "Parasite") %>% 
  group_by(Parasite) %>% mutate(N = 1:n()) %>% ungroup %>% 
  mutate_at("Parasite", ~ifelse(N == 1, .x, "")) %>% 
  dplyr::select(-N) %>% 
  filter(!str_detect(Variable, "PopN")) ->
  OverallDICChange

OverallDICChange %>% write.csv("Output Files/Table SI1.csv", row.names = F)

# Table SI2: Adult DIC Tables ####

AdultIMList %>% map(c("dDIC", first, list)) %>% 
  map(DICTableGet) %>% 
  map(~.x %>% mutate_at("Delta", ~round(as.numeric(.x), 3))) %>% 
  map(~mutate_all(.x, as.character)) %>% 
  map(~bind_rows(.x, EmptyRows(.x))) %>% 
  bind_rows(.id = "Parasite") %>% 
  group_by(Parasite) %>% mutate(N = 1:n()) %>% ungroup %>% 
  mutate_at("Parasite", ~ifelse(N == 1, .x, "")) %>% 
  dplyr::select(-N) %>% 
  filter(!str_detect(Variable, "PopN")) ->
  AdultDICChange

AdultDICChange %>% write.csv("Output Files/Table SI2.csv", row.names = F)

# Table SI3: Juvenile DIC Tables ####

YoungIMList %>% map(c("dDIC", first, list)) %>% 
  map(DICTableGet) %>% 
  map(~.x %>% mutate_at("Delta", ~round(as.numeric(.x), 3))) %>% 
  map(~mutate_all(.x, as.character)) %>% 
  map(~bind_rows(.x, EmptyRows(.x))) %>% 
  bind_rows(.id = "Parasite") %>% 
  group_by(Parasite) %>% mutate(N = 1:n()) %>% ungroup %>% 
  mutate_at("Parasite", ~ifelse(N == 1, .x, "")) %>% 
  dplyr::select(-N) %>% 
  filter(!str_detect(Variable, "PopN")) ->
  YoungDICChange

YoungDICChange %>% write.csv("Output Files/Table SI3.csv", row.names = F)
