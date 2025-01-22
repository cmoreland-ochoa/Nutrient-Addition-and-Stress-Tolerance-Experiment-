#### Calculate Species Weights from Buoyant Weight (Davies 1989)

## dry weight of object = weight in water + ((weight in air * Density of water) / Density of object)

## Density of aragonite = 2.93 g/cm-3 (Jokiel et al. 1978)
## Density of CCA = 2.71 g cm-3 (Comeau et al. 2014) "converted to dry weight"
## SA: aluminum foil technique (Marsh 1970)
## avg sea water density = 1.023 g cm-3



#############################
### LOAD LIBRARIES
#############################
library(tidyverse)
library(here)
library(seacarb)
library(readr)



#############################
### READ IN MCAP DATA
#############################
Mbw<-read.csv("C:/Users/austc/Downloads/NASTE_Buoyant Weight - MCAP.csv")

#############################
### CALCULATE DRY WEIGHT
#############################
#### Using function rho from seacarb package
## rho(S = 35, T = 25, P = 0)

Mbwd <- Mbw %>% 
  mutate(sw_dens = rho(S = Salinity, T = Temp, P = 0), # calculate density of seawater
         sw_dens = sw_dens * 0.001) %>%  # convert from kg cm-3 to g cm-3
  drop_na(sw_dens) %>%
  mutate(n = 1:nrow(.))

# isolate LK for skeletal density constant
MnLK <- grep(Mbwd$Frag.ID, pattern = "std")
MnLK <- Mbwd %>%
  filter(n %in% MnLK) %>% 
  mutate(Sp = "std")

# rejoin LK data to include Sp column and add skeletal densities
Mbwd.1 <- Mbwd %>% 
  anti_join(MnLK) %>% 
  mutate(Sp = "Coral") %>% 
  rbind(MnLK) %>%  # deals with possible join issue
  select(-n) %>% 
  mutate(skel_dens = if_else(Sp == "std", 2.71, 2.93)) %>% # add respective skeletal densities
  mutate(dry_weight.g = Weight / (1 - (sw_dens/skel_dens))) %>% 
  select(-Sp)

# dry weight of object = weight in water / (1 - (Density of water / Density of object)) (Jokiel et al. 1978)


#############################
### SAVE FILE
#############################

write.csv(Mbwd.1, "/Users/austc/Downloads/NASTE_BW to Dry Weight_MCAP.csv", row.names=TRUE)



#############################
### READ IN PCOM DATA
#############################
Pbw<-read.csv("C:/Users/austc/Downloads/NASTE_Buoyant Weight - PCOM.csv")

#############################
### CALCULATE DRY WEIGHT
#############################
#### Using function rho from seacarb package
## rho(S = 35, T = 25, P = 0)

Pbwd <- Pbw %>% 
  mutate(sw_dens = rho(S = Salinity, T = Temp, P = 0), # calculate density of seawater
         sw_dens = sw_dens * 0.001) %>%  # convert from kg cm-3 to g cm-3
  drop_na(sw_dens) %>%
  mutate(n = 1:nrow(.))

# isolate LK for skeletal density constant
PnLK <- grep(Pbwd$Frag.ID, pattern = "std")
PnLK <- Pbwd %>%
  filter(n %in% PnLK) %>% 
  mutate(Sp = "std")

# rejoin LK data to include Sp column and add skeletal densities
Pbwd.1 <- Pbwd %>% 
  anti_join(PnLK) %>% 
  mutate(Sp = "Coral") %>% 
  rbind(PnLK) %>%  # deals with possible join issue
  select(-n) %>% 
  mutate(skel_dens = if_else(Sp == "std", 2.71, 2.93)) %>% # add respective skeletal densities
  mutate(dry_weight.g = Weight / (1 - (sw_dens/skel_dens))) %>% 
  select(-Sp)

# dry weight of object = weight in water / (1 - (Density of water / Density of object)) (Jokiel et al. 1978)


#############################
### SAVE FILE
#############################

write.csv(Pbwd.1, "/Users/austc/Downloads/NASTE_BW to Dry Weight_PCOM.csv", row.names=TRUE)

