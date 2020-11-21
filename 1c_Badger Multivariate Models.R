
# 1c_Ostracism Models ####

library(MCMCglmm)

TestDFList <- IMList[1:4] %>% map("Data")
TestDFList[["Lice"]] <- YoungIMList$Lice$Data

# Fleas ####

TestDF <- TestDFList$Fleas

MF1 <- as.formula(
  
  paste0("cbind(Fleas, LifetimeDensity)", " ~ trait - 1 + trait:(", 
         paste0(c("Sex", "Year", "AgeCat", "Month", "BCI"), collapse = " + "), ")")
  
)

Iterations <- 1

FleaMultivariate <- MCMCglmm::MCMCglmm(
  
  MF1,
  data = TestDF,
  random =~ us(trait):Tattoo + us(trait):fYear,
  rcov =~ us(trait):units,
  nitt = 21000*Iterations,
  thin = 20*Iterations, 
  burnin = 1000*Iterations,
  family = c("poisson", "gaussian")
  
) 

saveRDS(FleaMultivariate, file = "Output Files/FleaMultivariate.rds")

FleaMultivariate$VCV %>% plot

# Eimeria ####

library(MCMCglmm)

TestDF <- TestDFList$Eimeria

EimeriaPrior <- list(R = list(V = diag(2), nu = 2.002, fix = 2), 
                     G = list(G1 = list(V = diag(2), nu = 2,
                                        alpha.mu = rep(0, 2),
                                        alpha.V = diag(2)*1),
                              G2 = list(V = diag(2), nu = 2,
                                        alpha.mu = rep(0, 2),
                                        alpha.V = diag(2)*1)))

MF2 <- as.formula(
  
  paste0("cbind(TrappingDensity, Eimeria)", " ~ trait - 1 + trait:(", 
         paste0(c("Sex", "Year", "AgeCat", "Month", "BCI"), collapse = " + "), ")")
  
)

Iterations <- 1

EimeriaMultivariate <- MCMCglmm::MCMCglmm(
  
  MF2,
  data = TestDF,
  random =~ us(trait):Tattoo + us(trait):fYear,
  rcov =~ us(trait):units,
  nitt = 21000*Iterations,
  thin = 20*Iterations, 
  burnin = 1000*Iterations,
  family = c("gaussian", "categorical"),
  prior = EimeriaPrior
  
) 

saveRDS(EimeriaMultivariate, file = "Output Files/EimeriaMultivariate.rds")

# Ticks ####

TickPrior <- list(R = list(V = diag(2), nu = 2.002, fix = 2), 
                  G = list(G1 = list(V = diag(2), nu = 2,
                                     alpha.mu = rep(0, 2),
                                     alpha.V = diag(2)*1),
                           G2 = list(V = diag(2), nu = 2,
                                     alpha.mu = rep(0, 2),
                                     alpha.V = diag(2)*1)))

TickTestDF <- TestDFList$Ticks

MF3 <- as.formula(
  
  paste0("cbind(LifetimeDensity, Ticks)", " ~ trait - 1 + trait:(", 
         paste0(c("Sex", "Year", "Month", "AgeCat", "BCI"), collapse = " + "), ")")
  
)

Iterations <- 1

TickMultivariate <- MCMCglmm::MCMCglmm(
  
  MF3,
  data = TickTestDF,
  random =~ us(trait):Tattoo + us(trait):fYear,
  rcov =~ us(trait):units,
  nitt = 21000*Iterations,
  thin = 20*Iterations, 
  burnin = 1000*Iterations,
  family = c("gaussian", "categorical"),
  prior = TickPrior
  
) 

saveRDS(TickMultivariate, file = "Output Files/TickMultivariate.rds")

# Lice ####

LicePrior <- list(R = list(V = diag(2), nu = 2.002), 
                  G = list(G1 = list(V = diag(2), nu = 2,
                                     alpha.mu = rep(0, 2),
                                     alpha.V = diag(2)*10),
                           G2 = list(V = diag(2), nu = 2,
                                     alpha.mu = rep(0, 2),
                                     alpha.V = diag(2)*1)))

YoungTestDFList <- readRDS("Output Files/YoungTestDFList.rds")

LiceTestDF <- YoungTestDFList$Lice

MF3 <- as.formula(
  
  paste0("cbind(TrappingDensity, Lice)", " ~ trait - 1 + trait:(", 
         paste0(c("Sex", "Year", "Month", "AgeCat", "BCI"), collapse = " + "), ")")
  
)

Iterations <- 1

CubLiceMultivariate <- MCMCglmm::MCMCglmm(
  
  MF3,
  data = LiceTestDF,
  random =~ us(trait):Tattoo + us(trait):fYear,
  rcov =~ us(trait):units,
  nitt = 21000*Iterations,
  thin = 20*Iterations, 
  burnin = 1000*Iterations,
  family = c("gaussian", "poisson"),
  prior = LicePrior
  
) 

saveRDS(CubLiceMultivariate, file = "Output Files/YoungLiceMultivariate.rds")
