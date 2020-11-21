
# 0_Saving Badgers ####

Badgers %>% 
  dplyr::select(Tattoo, 
                Eimeria, 
                Isospora, 
                Fleas, 
                Lice,
                Ticks,
                Sex, fYear, Year, Month,
                Age, AgeCat, X, Y, Degree, Strength, GroupSize,
                LifetimeDensity, TrappingDensity, AnnualDensity, 
                Survived, BCI) %>% 
  write.csv("Badgers.csv")
