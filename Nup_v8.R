#### ///// DATABASE CREATION ///// ####
#### a. Database ensemble ####
#### Di Tian ready (unitats?) ####
# Di Tian # units? trees? what is MAT calculate and MAT literature
library(openxlsx)
Tian <- read.xlsx("~/Desktop/MIT/Recerca/NitUP/Data/Site_level_forest_CN_NPP_China_TD_20220129-for-Helena.xlsx")
Tian$leaf_N[45] <- 8
Tian$Root_N[45] <- 3.9
Tian$`Leaf_biomass_per_year(NPP_leaf)` <- Tian$`Leaf_biomass_per_year(NPP_leaf)`*1000*2
Tian$`Stem_biomass_per_year(NPP_stem)` <- Tian$`Stem_biomass_per_year(NPP_stem)`*1000*2
Tian$`Root_biomass_per_year(NPP_root)` <- Tian$`Root_biomass_per_year(NPP_root)`*1000*2

for (i in 1:61){
  if (is.na(Tian$Shoot_N[i])) {
    Tian$Shoot_N[i] <- 3.6
  }
}

Nstock_leaf <- Tian$`Leaf_biomass_per_year(NPP_leaf)` * Tian$leaf_N/1000
Nstock_stem <- Tian$`Stem_biomass_per_year(NPP_stem)` * Tian$Shoot_N/1000
Nstock_root <- Tian$`Root_biomass_per_year(NPP_root)` * Tian$Root_N/1000
D_Tian_DB <- data.frame(Tian[,c(1,3,2,10,13,16,20,21,22)],  Nstock_leaf, Nstock_stem, Nstock_root, Tian[,c(7,8,4)])
D_Tian_DB$db <- "Tian"
D_Tian_DB <- D_Tian_DB[,c(16,1:15)]
colnames(D_Tian_DB)[1:2] <- c("Data_source","DB_id")
colnames(D_Tian_DB)[6] <- c("wood_N")
colnames(D_Tian_DB)[8:10] <- c("Leaf_NPP","Wood_NPP","Root_NPP")
colnames(D_Tian_DB)[14:15] <- c("MAT","MAP")
D_Tian_DB <- subset(D_Tian_DB, !is.na(D_Tian_DB$Nstock_leaf))
# D_Tian_DB <- subset(D_Tian_DB, !is.na(D_Tian_DB$Nstock_stem))
D_Tian_DB <- subset(D_Tian_DB, !is.na(D_Tian_DB$Nstock_root))
D_Tian_DB$Gross_Nuptake <- D_Tian_DB[,11]+D_Tian_DB[,12]+D_Tian_DB[,13]
D_Tian_DB$leaf_N <- D_Tian_DB$leaf_N/10
D_Tian_DB$wood_N <- D_Tian_DB$wood_N/10
D_Tian_DB$Root_N <- D_Tian_DB$Root_N/10

D_Tian_DB <- data.frame(D_Tian_DB[,1:4],"Broadleaved",D_Tian_DB[,5:7],NA,D_Tian_DB[,8:10],NA,D_Tian_DB[,11:13], NA, D_Tian_DB[14:17],NA,NA,NA,NA,NA,NA,NA,NA,NA)
colnames(D_Tian_DB) <- c("DB origen", "id_intern", "Latitude", "Longitude", "Forest_type","Leaf_N","Wood_N","Root_N","Litter_N",
                         "Leaf_NPP","Wood_NPP","Root_NPP","Litter_NPP","Nstock_leaves", "Nstock_wood", 
                         "Nstock_roots", "Nstock_litter", "MAT", "MAP", "Altitude", "Gross_Nuptake","Reabsorved","Net_Nuptake",
                         "Stand_age", "Stand_density", "Soil_CEC", "Soil_ph", "Soil_N","Soil_CN", "Myco type")

#### Cesar DB ready ####
# Cesar (no te coords)
library(readxl)
Ces <- read_excel("~/Desktop/MIT/Recerca/NitUP/Data/n_synthesis-cdiac.xlsx", sheet=2, range="A2:AR169")
Ces <- subset(Ces, Ces$CO2 == "A")
colnames(Ces)
Ces$`Leaf Litterfall (g/m2/yr)` <- Ces$`Leaf Litterfall (g/m2/yr)`*10
Ces$`Foliar Biomass Production (g/m2/yr)` <- Ces$`Foliar Biomass Production (g/m2/yr)`*10
Ces$`Wood Biomass Increment (g/m2/yr)` <- Ces$`Wood Biomass Increment (g/m2/yr)`*10
Ces$`Fine Root Biomass Production (g/m2/yr)` <- Ces$`Fine Root Biomass Production (g/m2/yr)`*10
Reabsorved <- ((Ces[,24]*Ces[,25]/100)-(Ces[,31]*Ces[,32]/100))
colnames(Ces)


# Ces <- data.frame(Ces[,c(1,2,3,4,5,8,23,26,29,33)])
colnames(D_Tian_DB)
colnames(Ces)
Ces <- data.frame("Ces",Ces[,1],Ces[,2],Ces[,3],NA,Ces[,25],Ces[,22],Ces[,28],Ces[,32],Ces[,24],
                  Ces[,21],Ces[,27],Ces[,31],Ces[,26]*10,Ces[,23]*10,Ces[,29]*10,Ces[,33]*10,NA,NA,NA,NA,Reabsorved)

# colnames(Ces) <- c("DB origen","id_intern", "Latitude", "Longitude", "Nstock_leaves", "Nstock_wood", 
#                    "Nstock_roots", "Nstock_litter", "MAT", "MAP", "Altitude", "Gross_Nuptake", "Reabsorved")
Cesar <- aggregate(Ces, by=list(Ces$SITE), FUN=mean)
Gross_Nuptake <- (Cesar[,15]+Cesar[,16]+Cesar[,17])
Forest_type <- c("Coniferous","Broadleaved","Broadleaved","Broadleaved")
Myco <- c("ECM","AM","ECM/AM","ECM/AM")
Cesar <- data.frame("Ces",Cesar[,1],Cesar[,4:5],Forest_type, Cesar[,7:18],NA,NA,NA,Gross_Nuptake,(Cesar[15]-Cesar[,18]),NA,NA,NA,NA,NA,NA,NA,Myco)
colnames(Cesar) <- c("DB origen", "id_intern", "Latitude", "Longitude", "Forest_type","Leaf_N","Wood_N","Root_N","Litter_N",
                         "Leaf_NPP","Wood_NPP","Root_NPP","Litter_NPP","Nstock_leaves", "Nstock_wood", 
                         "Nstock_roots", "Nstock_litter", "MAT", "MAP", "Altitude", "Gross_Nuptake","Reabsorved","Net_Nuptake",
                         "Stand_age", "Stand_density", "Soil_CEC", "Soil_ph", "Soil_N", "Soil_CN", "Myco type")
Cesar$Net_Nuptake <- Cesar$Gross_Nuptake - Cesar$Reabsorved

#### Bauters ready####
# dep_leach <- read.csv("~/Desktop/MIT/Recerca/NitUP/Data/data! (Bauters)/dep_leach_60yrs_Kg_N_ha.csv")
# d1 <- subset(dep_leach, dep_leach$cluster == "dep")
# d1mean <- mean(d1$TDN)
# d2 <- subset(dep_leach, dep_leach$cluster == "leach")
# d2mean <- mean(d2$TDN)

fine_root <- read.csv("~/Desktop/MIT/Recerca/NitUP/Data/data! (Bauters)/Fine_root_60yrs_OG_Mgha.csv")
fine_root <- fine_root[,c(3,4,5,10)]
fine_root$code <- paste(fine_root$plots, fine_root$Date) 
fr <- aggregate(fine_root$Prod_Mgha,by=list(fine_root$plots), FUN=mean)
fr$x <- fr$x/7*12
colnames(fr) <- c("plot", "root_prod_Mgha_yr")

fine_root_N <- read.csv("~/Desktop/MIT/Recerca/NitUP/Data/data! (Bauters)/Fine_litter_root_N_Content_63_OG_Helana.csv")
fine_root_N <- subset(fine_root_N, fine_root_N$Tissue == "Fine roots")
fine_root_N <- aggregate(fine_root_N[,6], by=list(fine_root_N$plots), FUN=mean, na.rm=TRUE)
colnames(fine_root_N) <- c("plot", "fine_root_N")

Nstocks_root <- (fr$root_prod_Mgha_yr*fine_root_N$fine_root_N/100)*1000

# agb <- read.csv("~/Desktop/MIT/Recerca/NitUP/Data/data! (Bauters)/Wood_60yrs_OG_Mgyear.csv")
# agb <- aggregate(agb[,5], by=list(agb$plots), FUN=sum, na.rm=TRUE) #Falta per ha
# colnames(agb) <- c("plot", "agb_prod_Mgha")

litter <- read.csv("~/Desktop/MIT/Recerca/NitUP/Data/data! (Bauters)/Litter_60yrs_OG_Mgha.csv")
litter <- aggregate(litter[,6], by=list(litter$plots), FUN=sum, na.rm=TRUE)
litter$x <- (litter$x/10)*12
colnames(litter) <- c("plot", "litter_prod_Mgha")

litter_N <- read.csv("~/Desktop/MIT/Recerca/NitUP/Data/data! (Bauters)/Fine_litter_root_N_Content_63_OG_Helana.csv")
litter_N <- subset(litter_N, litter_N$Tissue == "Litter")
litter_N <- aggregate(litter_N[,6], by=list(litter_N$plots), FUN=mean, na.rm=TRUE)
colnames(litter_N) <- c("plot", "litter_N")

Nstocks_litter <- (litter$litter_prod_Mgha*litter_N$litter_N/100)*1000

Nleaf <- read.csv("~/Desktop/MIT/Recerca/NitUP/Data/data! (Bauters)/Leaf_N_content_60yrs_OG.csv")
Nleaf <- aggregate(Nleaf[,5], by=list(Nleaf$plots), FUN=mean, na.rm=TRUE)
colnames(Nleaf) <- c("plot", "Leaf_N")

Nstocks_leaf <- (litter$litter_prod_Mgha*Nleaf$Leaf_N/100)*1000
reabs <- ((litter$litter_prod_Mgha*Nleaf$Leaf_N/100)-(litter$litter_prod_Mgha*litter_N$litter_N/100))*1000

Inventory <- read.csv("~/Desktop/MIT/Recerca/NitUP/Data/data! (Bauters)/Inventory_NWood_Helena.csv")
unique(Inventory$stage)
Inventory <- subset(Inventory, Inventory$stage == "60 Y" | Inventory$stage == "OG")
colnames(Inventory)
agb_N <- aggregate(Inventory[,c(34)], by=list(Inventory$plots), FUN=sum, na.rm=TRUE)
wood_N <- aggregate(Inventory[,c(30)], by=list(Inventory$plots), FUN=mean, na.rm=TRUE)

wood_prod <- aggregate(Inventory[,c(28)], by=list(Inventory$plots), FUN=sum, na.rm=TRUE)

# colnames(agb_N) <- c("plot","Nstock")
# agb_N$Nstock_yr <- agb_N$Nstock/22*12
Nstock_wood <- (wood_prod$x * wood_N$x /100)*1000

# bg_Nstock <- fr$root_prod_Mgha_yr*fine_root_N$fine_root_N/100
# abg_Nstock <- agb_N$Nstock_yr_ha

coords <- read.csv("~/Desktop/MIT/Recerca/NitUP/Data/data! (Bauters)/plot location.csv")
soil_N <- read.csv("~/Desktop/MIT/Recerca/NitUP/Data/data! (Bauters)/soil_CNP_60yrs_OG_0_10cm.csv")

Bauters <- data.frame("Bauters",litter$plot, coords$lat, coords$long,"Broadleaved", Nleaf$Leaf_N, wood_N$x, fine_root_N$fine_root_N, litter_N$litter_N, 
                      litter$litter_prod_Mgha*1000, wood_prod$x*1000, fr$root_prod_Mgha_yr*1000, litter$litter_prod_Mgha*1000,Nstocks_leaf, Nstock_wood, Nstocks_root,
                      Nstocks_litter, MAT = 24.95, MAP = 1666, coords$Alt, Nstocks_leaf+Nstock_wood+Nstocks_root,
                      reabs, Nstocks_leaf+Nstock_wood+Nstocks_root-reabs, c(60,60,60,150,150,150),NA,NA,NA,soil_N$N,NA,"AM")

colnames(Bauters) <- c("DB origen", "id_intern", "Latitude", "Longitude", "Forest_type","Leaf_N","Wood_N","Root_N","Litter_N",
                         "Leaf_NPP","Wood_NPP","Root_NPP","Litter_NPP","Nstock_leaves", "Nstock_wood", "Nstock_roots",
                         "Nstock_litter", "MAT", "MAP", "Altitude", "Gross_Nuptake","Reabsorved","Net_Nuptake",
                         "Stand_age", "Stand_density", "Soil_CEC", "Soil_ph", "Soil_N", "Soil_CN", "Myco type")


#### Cat scrapping ####
#still uncompleted but points available
library(openxlsx)
Cat <- read.xlsx("~/Desktop/MIT/Recerca/NitUP/Project/v5/N uptake DB_6juny_units.xlsx")

# Cat <- subset(Cat, Cat$Forest.Type != "Grassland")
# m1 <- lm(Cat$Wood_N_conc ~ Cat$Forest.Type, data=Cat)

colnames(Cat)
Cat <- data.frame("Scrapping", Cat[,2], Cat[,7], Cat[,8], Cat[,5], Cat[,15:18], Cat[,9:12], Cat[,20:23],
                  Cat[,32:34], Cat[,25], Cat[,27],Cat[,28],Cat[,35:36],Cat[,41],Cat[,45],Cat[,43],Cat[,44],Cat[,46])
colnames(Cat) <- c("DB origen", "id_intern", "Latitude", "Longitude", "Forest_type","Leaf_N","Wood_N","Root_N","Litter_N",
                     "Leaf_NPP","Wood_NPP","Root_NPP","Litter_NPP","Nstock_leaves", "Nstock_wood", 
                     "Nstock_roots", "Nstock_litter", "MAT", "MAP", "Altitude", "Gross_Nuptake","Reabsorved","Net_Nuptake",
                     "Stand_age", "Stand_density", "Soil_CEC", "Soil_ph", "Soil_N", "Soil_CN", "Myco type")

Cat <- Cat[!is.na(Cat$Gross_Nuptake) | !is.na(Cat$Net_Nuptake),]

#### Combined db ####
# In kgN/ha/yr
General_DB <- D_Tian_DB
# colnames(General_DB) <- c("DB origen", "id_intern", "Latitude", "Longitude", "Nstock_leaves", "Nstock_wood", 
#                          "Nstock_roots", "MAT", "MAP", "Altitude", "Nuptake")
General_DB <- rbind(General_DB, Cesar)
General_DB <- rbind(General_DB, Bauters)
General_DB <- rbind(General_DB, Cat)

save(General_DB, file = "~/Desktop/MIT/Recerca/NitUP/Project/v5/raw_DB_9jun23.RData")
# load("~/Desktop/MIT/Recerca/NitUP/Project/v5/raw_DB_9jun23.RData")
# hist(General_DB$Net_Nuptake)

write.csv(General_DB, file = "~/Desktop/MIT/Recerca/NitUP/Project/v8/raw_DB_10ago23.csv")
General_DB <- read.csv("~/Desktop/MIT/Recerca/NitUP/Project/v8/raw_DB_10ago23.csv")
General_DB <- General_DB[,2:32]
save(General_DB, file="~/Desktop/MIT/Recerca/NitUP/Project/v8/raw_DB_10ago23.RData")
#tuneo manual per LC i per % AM

#### b. Gap filling ####
library(raster)
load("~/Desktop/MIT/Recerca/NitUP/Project/v8/raw_DB_10ago23.RData")
ups <- stack("~/Desktop/MIT/Recerca/NitUP/Project/v3/ups.gri")
mic_C <- stack("~/Desktop/MIT/Recerca/NitUP/Project/v5/soil_microbes_C.tif")
mic_N <- stack("~/Desktop/MIT/Recerca/NitUP/Project/v5/soil_microbes_N.tif")

NP_point <- SpatialPointsDataFrame(coords = General_DB[,c(4,3)], data = General_DB,
                                   proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
mapdata <- extract(ups, NP_point)
mic_c <- extract(mic_C, NP_point)
mic_n <- extract(mic_N, NP_point)

# load("~/Desktop/MIT/Recerca/NitUP/Project/v5/merged_db.RData")

General_DB <- cbind(General_DB, mapdata, mic_c, mic_n)
# colnames(General_DB) <- colnames(db)
# General_DB <- db
colnames(General_DB)[c(32:39)] <- c("MAT_wc","MAP_wc","Alt_wc","Soil_CN_sg","Soil_ph_sg","woodiness","MAT_MAP_wc","AM_map")

# Include field soil N stocks
library(raster)
# n1 <- raster("~/Desktop/Batch maps/Global maps/SoilGrids/nitrogen.map/nitrogen_0-5cm_mean_1000.tif")
# n2 <- raster("~/Desktop/Batch maps/Global maps/SoilGrids/nitrogen.map/nitrogen_5-15cm_mean_1000.tif")
# n <- mean(n1,n2)
# 
# bd1 <- raster("~/Desktop/Batch maps/Global maps/SoilGrids/bdod.map/bdod_0-5cm_mean_1000.tif")
# bd2 <- raster("~/Desktop/Batch maps/Global maps/SoilGrids/bdod.map/bdod_5-15cm_mean_1000.tif")
# bd <- mean(bd1,bd2)

# N_stock_15cm <- (n*bd/10000)*0.15
# 
# N_stocks <- raster("~/Desktop/MIT/Recerca/NitUP/Project/v2/N_stock.grd")
# plot(N_stocks)
# plot(N_stock_15cm)
# N_stock_15cm <- projectRaster(N_stock_15cm, N_stocks, filename = "~/Desktop/MIT/Recerca/NitUP/Project/v5/N_stock_15cm.grd")

# nvar <- stack(n,bd)
# nvar <- projectRaster(nvar, N_stocks)
# writeRaster(nvar, filename = "~/Desktop/MIT/Recerca/NitUP/Project/v5/nvar.grd")
nvar <- brick("~/Desktop/MIT/Recerca/NitUP/Project/v5/nvar.grd")

NP_point <- SpatialPointsDataFrame(coords = General_DB[,c(4,3)], data = General_DB,
                                   proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
mapdata <- extract(nvar, NP_point)
colnames(mapdata) <- c("N", "BD")
General_DB <- cbind(General_DB, mapdata)

# General_DB$Soil_N <- General_DB$Soil_N*1000 #cg/kg

General_DB$soil_N_combi <- NA

for (i in 1:159){
  if (is.na(General_DB$Soil_N[i])){
    General_DB$soil_N_combi[i] <- mapdata[i,1]
  } else {
    General_DB$soil_N_combi[i] <- General_DB$Soil_N[i]*1000
  }
}

# 
soilBD <- rep(NA, 159)
soilBD[c(24:29)] <- c(1.17,1,1.11,1.17,1,1.11)
# 
soilBD <- soilBD*100 #cg/kg
for (i in 1:159){
  if (is.na(soilBD[i])){
    soilBD[i] <- mapdata[i,2]
  }
}
General_DB$soil_BD_sg <- soilBD

# 
N_stock <- ((General_DB$soil_N_combi * General_DB$soil_BD_sg)/10000)*0.15 #in kgN/m2sol

N_stock[c(50)] <- 0.256 #kg/m2
General_DB$N_stocks_combi <- N_stock

m1 <- lm(Net_Nuptake ~ Gross_Nuptake, data=General_DB)
summary(m1)
hist(resid(m1))
Gross <- as.data.frame(General_DB$Gross_Nuptake)
colnames(Gross) <- "Gross_Nuptake"
General_DB$Predicted_Net_Nuptake <- predict.lm(m1, Gross)

for (i in 1:nrow(General_DB)){
  if (is.na(General_DB$Net_Nuptake[i])){
    General_DB$Net_Nuptake_Combi[i] <- General_DB$Predicted_Net_Nuptake[i]
  } else {
    General_DB$Net_Nuptake_Combi[i] <- General_DB$Net_Nuptake[i]
  }
}

for (i in 1:nrow(General_DB)){
  if (is.na(General_DB$MAT[i])){
    General_DB$MAT_combi[i] <- General_DB$MAT_wc[i]
  } else {
    General_DB$MAT_combi[i] <- General_DB$MAT[i]
  }
}

# General_DB$MAP[123:124] <- c(5000,4500)
for (i in 1:nrow(General_DB)){
  if (is.na(General_DB$MAP[i])){
    General_DB$MAP_combi[i] <- General_DB$MAP_wc[i]
  } else {
    General_DB$MAP_combi[i] <- General_DB$MAP[i]
  }
}

for (i in 1:nrow(General_DB)){
  if (is.na(General_DB$Altitude[i])){
    General_DB$Alt_combi[i] <- General_DB$Alt_wc[i]
  } else {
    General_DB$Alt_combi[i] <- General_DB$Altitude[i]
  }
}

for (i in 1:nrow(General_DB)){
  if (is.na(General_DB$Soil_CN[i])){
    General_DB$Soil_CN_combi[i] <- General_DB$Soil_CN_sg[i]
  } else {
    General_DB$Soil_CN_combi[i] <- General_DB$Soil_CN[i]
  }
}

for (i in 1:nrow(General_DB)){
  if (is.na(General_DB$Soil_ph[i])){
    General_DB$Soil_ph_combi[i] <- General_DB$Soil_ph_sg[i]
  } else {
    General_DB$Soil_ph_combi[i] <- General_DB$Soil_ph[i]
  }
}

for (i in 1:nrow(General_DB)){
  if (General_DB$Forest_type[i] == "Grassland"){
    General_DB$woodiness[i] <- "Non-woody"
  } else {
    General_DB$woodiness[i] <- "Woody"
  }
}

General_DB$Soil_ph_combi <- as.numeric(General_DB$Soil_ph_combi)
General_DB$MAT_MAP_combi <- General_DB$MAP_combi*General_DB$MAT_combi

for (i in 1:nrow(General_DB)){
  if (isTRUE(General_DB$Soil_ph_combi[i] > 10)){
    General_DB$Soil_ph_combi[i] <- General_DB$Soil_ph_combi[i]/10
  } 
}
hist(General_DB$Soil_ph_combi)
NP_point <- SpatialPointsDataFrame(coords = General_DB[,c(4,3)], data = General_DB,
                                   proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

N_stock_15cm <- raster("~/Desktop/MIT/Recerca/NitUP/Project/v5/N_stock_15cm.grd")
General_DB$N_stocks_sg <- extract(N_stock_15cm, NP_point)
save(General_DB, file = "~/Desktop/MIT/Recerca/NitUP/Project/v5/gapFilled_db.RData")

#### c. Sep by latitudes ####
load("~/Desktop/MIT/Recerca/NitUP/Project/v5/gapFilled_db.RData")
NP_point <- SpatialPointsDataFrame(coords = General_DB[,c(4,3)], data = General_DB,
                                   proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
library(rgdal)
wwf <- readOGR("~/Desktop/Batch maps/WWF_eco/wwf_terr_ecos.shp")
Biomes <- extract(wwf, NP_point)
General_DB <- cbind(General_DB, Biomes$BIOME)

General_DB$regions <- NA
for (i in 1:159){
  if (General_DB$`Biomes$BIOME`[i] %in% c(1,2,3,7)){
    General_DB$regions[i] <- "Trop"
  }
}
for (i in 1:159){
  if (General_DB$`Biomes$BIOME`[i] %in% c(4,5,8)){
    General_DB$regions[i] <- "Temp"
  }
}
for (i in 1:159){
  if (General_DB$`Biomes$BIOME`[i] %in% c(6,11)){
    General_DB$regions[i] <- "Bor"
  }
}

for (i in 1:159){
  if(is.na(General_DB$regions[i])){
    if (abs(General_DB$Latitude[i]) < 30) {
      General_DB$regions[i] <- "Trop" }
    else {
      General_DB$regions[i] <- "Temp"
    }
  }
}

save(General_DB, file="~/Desktop/MIT/Recerca/NitUP/Project/v5/gapFilled_db_lats.RData")

#### d. Ratios ####
load("~/Desktop/MIT/Recerca/NitUP/Project/v5/gapFilled_db_lats.RData")

for (i in 1:159) {
  if (is.na(General_DB$Nstock_wood[i])) {
    General_DB$abg_to_roots[i] <- General_DB$Nstock_leaves[i]/General_DB$Nstock_roots[i]
  } else {
    General_DB$abg_to_roots[i] <- (General_DB$Nstock_leaves[i]+General_DB$Nstock_wood[i])/General_DB$Nstock_roots[i]
  }
}

for (i in 1:159) {
  if (is.na(General_DB$Wood_NPP[i])) {
    General_DB$totalNPP[i] <- General_DB$Leaf_NPP[i]+General_DB$Root_NPP[i]
  } else {
    General_DB$totalNPP[i] <- General_DB$Leaf_NPP[i]+General_DB$Wood_NPP[i]+General_DB$Root_NPP[i]
  }
}

General_DB$Leaf_NUE <- General_DB$Leaf_NPP/General_DB$Leaf_N
General_DB$Wood_NUE <- General_DB$Wood_NPP/General_DB$Wood_N
General_DB$Root_NUE <- General_DB$Root_NPP/General_DB$Root_N

for (i in 1:159) {
  if (is.na(General_DB$Wood_NPP[i])) {
    General_DB$NUE[i] <- General_DB$totalNPP[i]/(General_DB$Nstock_leaves[i]+General_DB$Nstock_roots[i])
    
  } else {
    General_DB$NUE[i] <- General_DB$totalNPP[i]/(General_DB$Nstock_leaves[i]+General_DB$Nstock_wood[i]+General_DB$Nstock_roots[i])
  }
}

save(General_DB, file="~/Desktop/MIT/Recerca/NitUP/Project/v5/gapFilled_db_lats_ratios.RData")

#### e. P data ####
library(raster)
mapFiles<- dir("~/Desktop/Batch maps/Global maps/SoilGrids/P map/", full.names=TRUE, pattern=".tif$") #carrega els 12 mapes de cop
# Has de canviar ~/Desktop/Batch maps/Global maps/ pel lloc on tu tinguis els mapes guardats
maps_list<- lapply(mapFiles, raster)
Pi <- stack(maps_list[[1]],maps_list[[2]],maps_list[[3]],maps_list[[4]])
Po <- stack(maps_list[[5]],maps_list[[6]],maps_list[[7]],maps_list[[8]])
Pi <- calc(Pi, sum)
Po <- calc(Po, sum)
P <- stack(Pi,Po)

punts <- SpatialPointsDataFrame(coords = General_DB[,c(4,3)], data = General_DB,
                                proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

# Extraiem la info dels mapes per cada punt i hi posem el nom adequat
mapdata <- extract(P, punts)
colnames(mapdata) <- c("Pi", "Po")
General_DB <- cbind(General_DB, mapdata)

save(General_DB, file="~/Desktop/MIT/Recerca/NitUP/Project/v5/gapFilled_db_lats_ratios.RData")

#### f. Am combi ####

for (i in 1:159) {
  if (is.na(General_DB$AM_percent[i])) {
    General_DB$AM_combi[i] <- General_DB$AM_map[i]*100
  } else {
    General_DB$AM_combi[i] <- General_DB$AM_percent[i]
  }
}
save(General_DB, file="~/Desktop/MIT/Recerca/NitUP/Project/v5/gapFilled_db_lats_ratios.RData")

#### g. Ndep data ####
load("~/Desktop/MIT/Recerca/NitUP/Project/v5/gapFilled_db_lats_ratios.RData")
noy <- raster("~/Desktop/Batch maps/N dep/ndep-noy_1901_2021_ISIMIP.tif")
plot(noy)
nhx <- raster("~/Desktop/Batch maps/N dep/ndep-nhx_1901_2021_ISIMIP.tif")
plot(nhx)
ndep <- stack(noy,nhx)
plot(noy+nhx)
NP_point <- SpatialPointsDataFrame(coords = General_DB[,c(4,3)], data = General_DB,
                                   proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
mapdata <- extract(ndep, NP_point)
General_DB <- cbind(General_DB, mapdata, mapdata[,1]+mapdata[,2])
colnames(General_DB)[68:70] <- c("noy","nhx","ndep")
colnames(General_DB)[44:45] <- c("mic_C","mic_N")

save(General_DB, file="~/Desktop/MIT/Recerca/NitUP/Project/v8/gapFilled_db_lats_ratios_ndep.RData")

#### ///// DATA ANALYSIS //// ####
#### 1. Initial assumptions ####
load("~/Desktop/MIT/Recerca/NitUP/Project/v8/gapFilled_db_lats_ratios_ndep.RData")
hist(General_DB$NUE, nclass=20)
library(ggpmisc)
library(ggpubr)
N_stock <- ggplot(data=General_DB, aes(x=N_stocks_combi, y=log(Net_Nuptake_Combi)))+
  geom_point()+
  # coord_trans(y="log2", ylim=c(4,450)) +
  # geom_smooth(method="lm") +
  stat_poly_line(color="cyan4") +
  stat_poly_eq(use_label(c("eq", "p.value")), label.x = "right", label.y=0.92) + #R2
  theme_classic() +
  # theme(axis.text.x = element_blank(),
  #       axis.title =element_text(size=13))+
  theme(
        axis.title =element_text(size=13))+
  xlab(expression(Soil~N~stock~(kgN/m^{"2"}))) +
  # xlab("") +
  ylab("Plant N uptake \n log(kgN/ha/yr)") +
  ylim(2,7)

Nup_w <- ggplot(data=General_DB, aes(x=woodiness, y=log(Net_Nuptake_Combi))) +
  geom_violin(fill="gray", scale = "width") +
  geom_boxplot(width=0.1, color="cyan4", alpha=0.2) +
  geom_signif(comparisons = list(c("Non-woody", "Woody")),
              y_position = c(6),
              map_signif_level=TRUE)+
  theme_classic() +
  theme(axis.text.x = element_text(size=13),
        axis.text.y = element_blank())+
  # theme(axis.text.x = element_text(size=13))+
  scale_x_discrete(labels=c("Grassland","Woody")) +
  # scale_x_discrete(labels=c("","")) +
  # ylab("Plant N uptake log(kgN/ha/yr)")+
  ylab("")+
  xlab("")+
  ylim(2,7)

N_stock_nue <- ggplot(data=General_DB, aes(x=N_stocks_combi, y=log(NUE)))+
  geom_point()+
  # coord_trans(y="log2", ylim=c(4,450)) +
  # geom_smooth(method="lm") +
  stat_poly_line(color="coral2") +
  stat_poly_eq(use_label(c("eq", "p.value")), label.x = "right", label.y=0.92) + #R2
  theme_classic() +
  theme(axis.title =element_text(size=13))+
  xlab(expression(Soil~N~stock~(kgN/m^{"2"}))) +
  ylab("Plant NUE \n log(kgC/kgN)") +
  ylim(3,7)

Nup_w_nue <- ggplot(data=General_DB, aes(x=woodiness, y=log(NUE))) +
  geom_violin(fill="gray", scale="width") +
  geom_boxplot(width=0.1, color="coral2", alpha=0.2) +
  geom_signif(comparisons = list(c("Non-woody", "Woody")),
              y_position = c(6.8),
              map_signif_level=TRUE)+
  theme_classic() +
  theme(axis.text.x = element_text(size=13),
        axis.text.y = element_blank())+
  scale_x_discrete(labels=c("Grassland","Woody")) +
  # ylab("Plant N uptake log(kgN/ha/yr)")+
  ylab("")+
  xlab("")+
  ylim(3,7)

# library(ggpubr)
# tiff(filename="~/Desktop/MIT/Recerca/NitUP/Project/v6/Figures/Fig1b.tif",
#      width=1600, height = 1400, res= 180)
# ggarrange(N_stock, Nup_w, N_stock_nue, Nup_w_nue, ncol=2, nrow=2, align = "h")
# dev.off()

tiff(filename="~/Desktop/MIT/Recerca/NitUP/Project/v8/Figures/SM/non_sign_Nstock_woody.tif",
     width=1200, height = 600, res= 160)
ggarrange(N_stock, N_stock_nue, ncol=2, align="h", labels = c("a)","b)"))
dev.off()

#### 2.1 Nup:Linear models ####
# load("~/Desktop/MIT/Recerca/NitUP/Project/v5/gapFilled_db_lats_ratios.RData")
load("~/Desktop/MIT/Recerca/NitUP/Project/v8/gapFilled_db_lats_ratios_ndep.RData")
# write.csv(General_DB, file="~/Desktop/MIT/Recerca/NitUP/Project/v8/gapFilled_db_lats_ratios_ndep.csv")

m1 <- lm(Soil_ph_combi ~ mic_N, data= General_DB)
summary(m1)
plot(General_DB$Soil_ph_combi, General_DB$mic_N)

General_DB$AM_categoric <- "Med"

for (i in 1:159) {
  if(isTRUE(General_DB$AM_combi[i] < 40)) {
    General_DB$AM_categoric[i] <- "Low"
  }
}

for (i in 1:159) {
  if(isTRUE(General_DB$AM_combi[i] > 70)) {
    General_DB$AM_categoric[i] <- "High"
  }
}

General_DB$AM_categoric <- as.factor(General_DB$AM_categoric)

# library(dplyr)
# library(forcats)
# options(scipen = 999)
# 
# library(corrplot)
# colnames(General_DB)[c(45,31,57,53,54,55,50,68)]
# cor <- cor(na.exclude(General_DB[,c(45,31,57,53,54,55,50,68)]))
# corrplot(cor)
# 
# write.csv(cor, file = "~/Desktop/MIT/Recerca/NitUP/Project/v8/Figures/SM/variables_correlation.csv")
# m1 <- lm(log(Net_Nuptake_Combi) ~ MAT_combi+AM_percent+MAP_combi+mic_N+Alt_combi+N_stocks_combi+
#            Soil_ph_combi+woodiness,
#          data=General_DB)

General_DB_neta <- subset(General_DB, !is.na(General_DB$AM_combi))
General_DB_neta <- subset(General_DB_neta, !is.na(General_DB_neta$N_stocks_combi))
General_DB_neta <- subset(General_DB_neta, !is.na(General_DB_neta$mic_N))
General_DB_neta <- subset(General_DB_neta, !is.na(General_DB_neta$Soil_ph_combi))

m_aglom <- glm(Net_Nuptake_Combi ~ Alt_combi+AM_combi+MAT_combi+mic_N+noy+woodiness+N_stocks_combi+MAP_combi+Soil_ph_combi, family=Gamma(link = "inverse"), na.action=na.fail, data=General_DB_neta)
summary(m_aglom)

library(MuMIn)
m_select_nup <- MuMIn::dredge(m_aglom)
a <- MuMIn::sw(m_select_nup)
importance_sw <- MuMIn::sw(subset(model.sel(m_select_nup, rank = AIC),
                                  cumsum(weight) <= .95))
# importance_caret <- caret::varImp(m_aglom)

m1_2 <- glm(Net_Nuptake_Combi ~ MAP_combi+MAT_combi+noy, family = Gamma(link = "inverse"), data=General_DB)
summary(m1_2)

# x <- summary(m1_1)
# resum1 <- as.data.frame(x$coefficients)
# resum1$Estimate <- exp(resum1$Estimate)
# for (i in 2:length(resum1)){
#   resum1$Estimate[i] <- (resum1$Estimate[i]-1)*resum1$Estimate[1]
# }
# varimp <- caret::varImp(m1_1)
# resum1$varimp <- c(NA,varimp$Overall)
# resum1$vars <- rownames(resum1)
# resum1$sign <- NA
# 
# options(scipen = 999)
# for (i in 2:nrow(resum1)){
#   if(resum1$`Pr(>|t|)`[i] < 0.05){
#     resum1$sign[i] <- "P.value < 0.05"
#   } else {
#     resum1$sign[i] <- "P.value > 0.05"
#   }
# }

names <- c("Nox deposition", "Temperature", "Precipitation", "Vegetation type", "Arbuscular Myco %", "Altitude",
           "Microbial N", "Soil pH", "Soil N stocks")
resum_Nup <- data.frame(importance_sw, names)

for (i in 1:nrow(resum_Nup)){
  if(resum_Nup$importance_sw[i] < 0.75){
    resum_Nup$imp[i] <- "No important"
  } else {
    resum_Nup$imp[i] <- "Imp"
  }
}

library(ggplot2)
library(ggpmisc)
library(forcats)

Nup_imp <- ggplot(data=resum_Nup, aes(y=importance_sw, x=fct_reorder(names, importance_sw), fill=imp)) +
  geom_bar(stat="identity")+
  scale_fill_manual(values=c("cyan4", "grey50")) +
  theme_classic()+
  theme(axis.text.y = element_text(size=13))+
  coord_flip() +
  geom_segment(aes(x=0,xend=10,y=0.75,yend=0.75),lty=2)+
  ylab("Variable importance")+
  theme(legend.position = "none")+
  labs(fill = "") +
  xlab("")

Nup_noy <- ggplot(data=General_DB, aes(x=noy, y=log(Net_Nuptake_Combi)))+
  geom_point()+
  # geom_boxplot(aes(x=as.factor(AM_percent), y=log(Net_Nuptake_Combi))) +
  # coord_trans(y="log2", ylim=c(4,450)) +
  stat_poly_line(color="cyan4") +
  # geom_abline(slope = -0.0733,
  #             intercept = 89.7, col="blue") +
  stat_poly_eq(use_label(c("eq", "p.value")), label.x = "right", label.y=0.95) + #R2
  theme_classic() +
  # xlab("Nox deposition (gN/m2)") +
  xlab(expression(Nox~deposition~(gN/m^{"2"}))) +
  ylab("Plant N uptake \n log(gN/ha/yr)")+
  ylim(2,6.3)

Nup_temp <- ggplot(data=General_DB, aes(x=MAT_combi, y=log(Net_Nuptake_Combi)))+ #MAT_combi
  geom_point()+
  # geom_boxplot(aes(x=as.factor(AM_percent), y=log(Net_Nuptake_Combi))) +
  # coord_trans(y="log2", ylim=c(4,450)) +
  stat_poly_line(color="cyan4") +
  # geom_abline(slope = -0.0733,
  #             intercept = 89.7, col="blue") +
  stat_poly_eq(use_label(c("eq", "p.value")), label.x = "right", label.y=0.95) + #R2
  theme_classic() +
  xlab("Temperature (°C)") +
  ylab("Plant N uptake \n log(kgN/ha/yr)")+
  ylim(2,6.3)

Nup_prec <- ggplot(data=General_DB, aes(x=MAP_combi, y=log(Net_Nuptake_Combi)))+
  geom_point()+
  # geom_boxplot(aes(x=as.factor(AM_percent), y=log(Net_Nuptake_Combi))) +
  # coord_trans(y="log2", ylim=c(4,450)) +
  stat_poly_line(color="cyan4") +
  # geom_abline(slope = -0.0733,
  #             intercept = 89.7, col="blue") +
  stat_poly_eq(use_label(c("eq", "p.value")), label.x = "right", label.y=0.95) + #R2
  theme_classic() +
  xlab("Precipitation (mm)") +
  ylab("Plant N uptake \n log(kgN/ha/yr)")+
  ylim(2,6.3)

tgrob <- text_grob("Plant nitrogen uptake", size = 18, face="bold")
plot_0 <- as_ggplot(tgrob) + theme(plot.margin = margin(0,0,0,-4.9, "cm"))

library(ggpubr)
tiff(filename="~/Desktop/MIT/Recerca/NitUP/Project/v8/Figures/Fig1_Nup.tif",
     width=1400, height = 1200, res= 140)
ggarrange(plot_0, NULL,
          Nup_imp,Nup_noy,Nup_temp,Nup_prec, ncol=2, nrow = 3, 
                    labels = c("","", "a)","b)","c)","d)"),
          heights = c(1.5,10,10))
dev.off()

#### 2.2 NUE:Linear models ####
General_DB_neta <- subset(General_DB_neta, !is.na(General_DB_neta$NUE))

m_aglom_nue <- glm(NUE ~ MAT_combi+AM_combi+MAP_combi+mic_N+Alt_combi+N_stocks_combi+Soil_ph_combi+woodiness+noy+
                     AM_combi:noy+mic_N:noy,
                   family = Gamma(link="inverse"), na.action = na.fail, data=General_DB_neta)

m_select_nue <- MuMIn::dredge(m_aglom_nue)
importance_NUE <- sw(subset(model.sel(m_select_nue, rank = AIC),
                            cumsum(weight) <= .95))
# importance_NUE_caret <- caret::varImp(m_aglom_nue)

m2 <- lm(log(NUE) ~ AM_combi+mic_N+Soil_ph_combi, data=General_DB)
summary(m2)

names <- c("Arbuscular Myco %", "Soil pH", "Microbial N", "Soil N stocks", "Altitude", "Nox deposition", "Temperature",
           "Precipitation","Vegetation type")
resum_Nue <- data.frame(importance_NUE, names)

for (i in 1:nrow(resum_Nue)){
  if(resum_Nue$importance_NUE[i] < 0.75){
    resum_Nue$imp[i] <- "No important"
  } else {
    resum_Nue$imp[i] <- "Imp"
  }
}

Nue_imp <- ggplot(data=resum_Nue, aes(y=importance_NUE, x=fct_reorder(names, importance_NUE), fill=imp)) +
  geom_bar(stat="identity")+
  scale_fill_manual(values=c("coral2", "grey50")) +
  theme_classic()+
  theme(axis.text.y = element_text(size=13))+
  coord_flip() +
  geom_segment(aes(x=0,xend=10,y=0.75,yend=0.75),lty=2)+
  ylab("Variable importance")+
  theme(legend.position = "none")+
  labs(fill = "") +
  xlab("")

# Nue_am_percent <- ggplot(data=General_DB, aes(x=AM_combi, y=log(NUE)))+
#   geom_point()+
#   # geom_boxplot(aes(x=as.factor(AM_percent), y=log(Net_Nuptake_Combi))) +
#   # coord_trans(y="log2", ylim=c(4,450)) +
#   stat_poly_line(color="coral2") +
#   # geom_abline(slope = -0.0733,
#   #             intercept = 89.7, col="blue") +
#   stat_poly_eq(use_label(c("eq", "p.value")), label.x = "right", label.y=0.95) + #R2
#   theme_classic() +
#   xlab("Arbuscular mycorrhizae %") +
#   ylab("Nitrogen use efficiency (kgC/kgN)")+
#   ylim(3,7.2)

Nue_am_cat <- ggplot(data=General_DB, aes(x=fct_relevel(AM_categoric, "Low", "Med", "High"), y=log(NUE))) +
  geom_violin(fill="gray", scale="width") +
  geom_boxplot(width=0.1, color="coral2", alpha=0.2) +
  geom_signif(comparisons = list(c("Low", "Med"), c("Med", "High"), c("Low", "High")),
              y_position = c(5.8,5.8,6.1),
              map_signif_level=TRUE)+
  theme_classic() +
  # theme(axis.text.x = element_text(size=13))+
  scale_x_discrete(labels=c("Low","Medium","High")) +
  ylab("Nitrogen use efficiency \n (kgC/kgN)")+
  xlab("Arbuscular Mycorrhizal %")+
  # xlab("")+
  ylim(3.5,6.3)

# Nue_temp <- ggplot(data=General_DB, aes(x=MAT_combi, y=log(NUE)))+
#   geom_point()+
#   # geom_boxplot(aes(x=as.factor(AM_percent), y=log(Net_Nuptake_Combi))) +
#   # coord_trans(y="log2", ylim=c(4,450)) +
#   stat_poly_line(color="coral2") +
#   # geom_abline(slope = -0.0733,
#   #             intercept = 89.7, col="blue") +
#   stat_poly_eq(use_label(c("eq", "p.value")), label.x = "right", label.y=0.95) + #R2
#   theme_classic() +
#   xlab("Temperature (°C)") +
#   ylab("Nitrogen use efficiency (kgC/kgN)")+
#   ylim(3,7.2)

# Nue_prec <- ggplot(data=General_DB, aes(x=MAP_combi, y=log(NUE)))+
#   geom_point()+
#   # geom_boxplot(aes(x=as.factor(AM_percent), y=log(Net_Nuptake_Combi))) +
#   # coord_trans(y="log2", ylim=c(4,450)) +
#   stat_poly_line(color="coral2") +
#   # geom_abline(slope = -0.0733,
#   #             intercept = 89.7, col="blue") +
#   stat_poly_eq(use_label(c("eq", "p.value")), label.x = "right", label.y=0.95) + #R2
#   theme_classic() +
#   xlab("Precipitation (mm)") +
#   ylab("Nitrogen use efficiency (kgC/kgN)")+
#   ylim(3,7.2)

# Nue_noy <- ggplot(data=General_DB, aes(x=noy, y=log(NUE)))+
#   geom_point()+
#   # geom_boxplot(aes(x=as.factor(AM_percent), y=log(Net_Nuptake_Combi))) +
#   # coord_trans(y="log2", ylim=c(4,450)) +
#   stat_poly_line(color="coral2") +
#   # geom_abline(slope = -0.0733,
#   #             intercept = 89.7, col="blue") +
#   stat_poly_eq(use_label(c("eq", "p.value")), label.x = "right", label.y=0.95) + #R2
#   theme_classic() +
#   # xlab("Nox deposition (kgN/m2)") +
#   xlab(expression(Nox~deposition~(gN/m^{"2"}))) +
#   ylab("Nitrogen use efficiency (kgC/kgN)")+
#   ylim(3,7.2)

Nue_ph <- ggplot(data=General_DB, aes(x=Soil_ph_combi, y=log(NUE)))+
  geom_point()+
  # geom_boxplot(aes(x=as.factor(AM_percent), y=log(Net_Nuptake_Combi))) +
  # coord_trans(y="log2", ylim=c(4,450)) +
  stat_poly_line(color="coral2") +
  # geom_abline(slope = -0.0733,
  #             intercept = 89.7, col="blue") +
  stat_poly_eq(use_label(c("eq", "p.value")), label.x = "right", label.y=0.95) + #R2
  theme_classic() +
  # xlab("Nox deposition (kgN/m2)") +
  xlab("Soil pH") +
  ylab("Nitrogen use efficiency \n (kgC/kgN)")+
  ylim(3.5,6.3)

Nue_micN <- ggplot(data=General_DB, aes(x=mic_N, y=log(NUE)))+
  geom_point()+
  # geom_boxplot(aes(x=as.factor(AM_percent), y=log(Net_Nuptake_Combi))) +
  # coord_trans(y="log2", ylim=c(4,450)) +
  stat_poly_line(color="coral2") +
  # geom_abline(slope = -0.0733,
  #             intercept = 89.7, col="blue") +
  stat_poly_eq(use_label(c("eq", "p.value")), label.x = "right", label.y=0.95) + #R2
  theme_classic() +
  # xlab("Nox deposition (kgN/m2)") +
  xlab(expression(Microbial~N~stock~(gN/m^{"2"}))) +
  ylab("Nitrogen use efficiency \n (kgC/kgN)")+
  ylim(3.5,6.3)

# Nue_w_nue <- ggplot(data=General_DB, aes(x=woodiness, y=log(NUE))) +
#   geom_violin(fill="gray", scale="width") +
#   geom_boxplot(width=0.1, color="coral2", alpha=0.2) +
#   geom_signif(comparisons = list(c("Non-woody", "Woody")),
#               y_position = c(6.8),
#               map_signif_level=TRUE)+
#   theme_classic() +
#   theme(axis.text.x = element_text(size=13))+
#   scale_x_discrete(labels=c("Grassland","Woody")) +
#   ylab("Nitrogen use efficiency (kgC/kgN)")+
#   # ylab("")+
#   xlab("")+
#   ylim(3,7.2)

library(ggpubr)
tgrob <- text_grob("Nitrogen use efficiency", size = 18, face="bold")
plot_0 <- as_ggplot(tgrob) + theme(plot.margin = margin(0,0,0,-4.5, "cm"))

tiff(filename="~/Desktop/MIT/Recerca/NitUP/Project/v8/Figures/Fig2_Nue.tif",
     width=1400, height = 1200, res= 140)
ggarrange(plot_0, NULL,
          Nue_imp,Nue_am_cat,Nue_ph,Nue_micN, ncol=2, nrow = 3, 
          labels = c("","", "a)","b)","c)","d)"),
          heights = c(1.5,10,10))
# ggarrange(plot_0, NULL,
#           Nue_imp,
#           ggarrange(Nue_am_cat,Nue_ph, labels = c("b)", "c)"), ncol=1, nrow=2),
#           ncol=2, nrow = 2,labels = c("","","a)"),
#           heights = c(1,10), widths = c(6,4))
dev.off()

#### 3. Model creation ####
# load("~/Desktop/MIT/Recerca/NitUP/Project/v5/gapFilled_db_lats_ratios.RData")
load("~/Desktop/MIT/Recerca/NitUP/Project/v8/gapFilled_db_lats_ratios_ndep.RData")

m1 <- lm(Net_Nuptake_Combi ~ NUE, data=General_DB)
summary(m1)
plot(General_DB$Net_Nuptake_Combi, General_DB$NUE)
a <- General_DB$Net_Nuptake_Combi/General_DB$NUE
hist(a)
colnames(General_DB)
library(caret)
library(xgboost)
library(data.table)
func_boost <- function(db,iterations=5, variables, cont_var, y_var_pos,
                       max.depth=4, min_child_weight=0,eta=0.3){
  performance <- as.data.frame(matrix(ncol=3, nrow=iterations))
  colnames(performance) <- c("mse", "rmse", "r2")
  imp_matrix <- data.frame()
  var_behavior <- data.frame()
  models_list <- vector("list", iterations)
  training_data <- vector("list", iterations)
  validation_data <- vector("list", iterations)
  for ( i in 1:iterations) {
    parts = createDataPartition(db[,y_var_pos], p= .9, list=F)
    data=db[parts,]
    validation = db[-parts,]
    
    validation_x = data.matrix(validation[, variables])
    validation_y = validation[,y_var_pos]
    
    partió <- nrow(data)/5
    start <- data[,cont_var]
    selectId <- maxDissim(start, start, obj = minDiss, n = partió)
    
    dissimilar <- data[selectId, ]
    non_dissimilar <- data[-selectId, ]
    
    parts <- sample(1:dim(non_dissimilar)[1], partió)
    
    train = non_dissimilar[-parts, ]
    train <- rbind(dissimilar,train)
    test = data[parts, ]
    
    # parts = createDataPartition(db$Net_Nuptake_Combi, p = .8, list = F, groups=5)
    # train = db[parts, ]
    # test = db[-parts, ]
    
    train_x = data.matrix(train[, variables])
    train_y = train[,y_var_pos]
    test_x = data.matrix(test[, variables])
    test_y = test[,y_var_pos]
    
    xgb_train = xgb.DMatrix(data = train_x, label = train_y)
    xgb_test = xgb.DMatrix(data = test_x, label = test_y)
    
    watchlist = list(train=xgb_train, test=xgb_test)
    
    model = xgb.train(data = xgb_train, max.depth = max.depth, verbose=0,
                      watchlist=watchlist, nrounds = 150,objective="reg:gamma",
                      min_child_weight=min_child_weight, eta=eta)
    min <- min(model$evaluation_log$test_gamma_nloglik)
    min_iter <- model$evaluation_log[which(model$evaluation_log$test_gamma_nloglik==min),]
    number <- min_iter$iter[[1]]
    model_xgboost = xgboost(data = xgb_train, max.depth = max.depth, nrounds = number, verbose = 0, missing=NA,
                            objective="reg:gamma", min_child_weight=min_child_weight, eta=eta)
    pred_y = predict(model_xgboost, xgb_test)
    y_test_mean = mean(test_y)
    tss =  sum((test_y - y_test_mean)^2 )
    rss =  sum((pred_y - test_y)^2)
    
    performance[i,] <- c(mean((test_y - pred_y)^2),caret::RMSE(test_y, pred_y), 1 - (rss/tss))
    importance_matrix = xgb.importance(colnames(xgb_train), model = model_xgboost)
    imp_matrix <- rbind(imp_matrix, importance_matrix)
    models_list[[i]] <- model_xgboost
    training_data[[i]] <- train_x
    validation_data[[i]] <- cbind(validation$Latitude, validation$Longitude, validation_y, validation_x)
  }
  total_performance <- colMeans(performance)
  names(total_performance) <- c("mse", "rmse", "r2")
  importance_matrix <- aggregate(imp_matrix, list(imp_matrix$Feature), FUN=mean)
  importance_matrix <- importance_matrix[,-2]
  colnames(importance_matrix)[1] <- "Feature"
  importance_matrix <- setDT(importance_matrix)
  return(list(total_performance, performance, importance_matrix,
              models_list, training_data, validation_data))
}

validation <- function(model){
  r2mean <- length(model[[6]])
  for (i in 1:length(model[[6]])) {
    model_i <- model[[4]][[i]]
    data <- model[[6]][[i]]
    predi <- predict(model_i,data[,4:ncol(data)])
    m1 <- lm(predi ~ data[,3])
    sum <- summary(m1)
    r2mean[i] <- sum$r.squared
  }
  return(mean(r2mean))
}

all_plot_pdp <- function(func_boost_output, out_var, ncol, nrow, categoric, ymin, ymax, color, ylab) {
  p <- vector("list", length(out_var))
  for (j in 1:length(out_var)){
    accum_db <- data.frame()
    for (i in 1:length(func_boost_output[[4]])) {
      model <- func_boost_output[[4]][[i]]
      training_data <- func_boost_output[[5]][[i]]
      p_data <- partial(model, pred.var = out_var[j], train=training_data)
      accum_db <- rbind(accum_db, p_data)
    }
    if (out_var[j] %in% categoric) {
      # accum_db$woodiness <- ifelse(accum_db$woodiness == 1, 1,2)
      p[[j]] <- local({
        j <- j
        accum_db[,1] <- as.character(accum_db[,1])
        m1 <- ggplot(data=accum_db, aes_string(x=out_var[j], y="yhat", group=out_var[j])) +
          # geom_smooth(span=0.75, se=TRUE, colour="red", method= "loess", formula = "y ~ x") +
          geom_violin(fill="gray") +
          geom_boxplot(width=0.1, color=color, alpha=0.2) +
          # geom_point(color="grey", size=0.8)+
          theme_bw() +
          # ylab("predicted N uptake \n (kgN/ha/yr)")+
          ylab(ylab)+
          xlab(out_var[j])+
          scale_x_discrete(labels=c("grassland","woody")) +
          # ylim(1,8)
          ylim(ymin,ymax)
      })
    } else {
      if (out_var[j] == "AM_percent") {
        p[[j]] <- local({
          j <- j
          means <- aggregate(accum_db[,1], by=list(accum_db[,2]), FUN=mean, na.rm=T)
          colnames(means) <- c("y","x")
          line <- aggregate(accum_db$yhat, list(accum_db$AM_percent), FUN=mean) 
          colnames(line) <- c("AM_percent", "yhat")
          m1 <- ggplot(data=line, aes_string(x=out_var[j], y="yhat")) +
            # geom_smooth(span=0.75, se=T, colour="red", method= "loess", formula = "y ~ x") +
            geom_point(data=means, aes(x=x, y=y), color="grey", size=0.8)+
            geom_line(colour=color, size=1)+
            # geom_smooth(method="lm", col="black")+
            theme_bw() +
            # ylab("predicted N uptake \n (kgN/ha/yr)")+
            ylab(ylab)+
            xlab(out_var[j]) +
            # coord_cartesian(ylim = c(1, 8))
            coord_cartesian(ylim = c(ymin, ymax))
        })
      } else {
        p[[j]] <- local({
          j <- j
          means <- aggregate(accum_db[,1], by=list(accum_db[,2]), FUN=mean, na.rm=T)
          colnames(means) <- c("y","x")
          m1 <- ggplot(data=accum_db, aes_string(x=out_var[j], y="yhat")) +
            geom_point(data=means, aes(x=x, y=y), color="grey", size=0.8)+
            # geom_point(color="grey", size=0.8)+
            geom_smooth(span=0.75, se=T, colour=color, method= "loess", formula = "y ~ x") +
            geom_smooth(method="lm", col="black")+
            theme_bw() +
            # ylab("predicted N uptake \n (kgN/ha/yr)")+
            ylab(ylab)+
            xlab(out_var[j])+
            # coord_cartesian(ylim = c(1,8))
            coord_cartesian(ylim = c(ymin, ymax))
        })
      }
    }
  }
  # plots <- ggpubr::ggarrange(plotlist = pdp_plots, ncol=4, nrow=2)
  return(p)
}


# library(corrplot)
# cor <- cor(na.exclude(General_DB[,c(31,41:46,51:55)]))
# corrplot(cor)
# 
# colnames(General_DB)[c(51:56,41,43:46,37,31,48)]

#### **3.1 Nup ####
colnames(General_DB)[c(37,45,48,51,52,53,55,67,68)]
# General_DB <- subset(General_DB, General_DB$MAT_combi > -5)

m1 <- lm(log(Net_Nuptake_Combi) ~ N_stocks_combi:noy, data=General_DB)
summary(m1)
inter <- list(c(0,1),c(0,2), c(0,4), c(0,5), c(0,8), 
              c(1,2),c(1,3),c(1,4),c(1,7),
              c(2,3),c(2,4),c(2,5),c(2,8),
              c(3,4),c(3,5),c(3,6),c(3,8),
              c(4,5),c(4,6), c(4,7), c(4,8),
              c(5,8))
nup_model <- func_boost(db=General_DB, iterations = 100,
                     variables=c(37,45,31,57,53,54,55,50,68), y_var_pos=52,
                     max.depth=2, min_child_weight=1, eta=0.30,
                     cont_var=c(45,31,57,53,54,55,50,68))
#
nup_model[[1]]
xgb.plot.importance(nup_model[[3]])
validation(nup_model)
hist(General_DB$N_stocks_combi, nclass=20)
hist(General_DB$N_stocks_sg, nclass=20)

# 
# save(nup_model, file="~/Desktop/MIT/Recerca/NitUP/Project/v8/nup_xgboost.RData")
load("~/Desktop/MIT/Recerca/NitUP/Project/v8/nup_xgboost.RData")

im <- nup_model[[3]]
im$Feature
# customNames <- c("MAT * MAP", "Altitude", "Fungal soil biomass", "Bacterial soil biomass", "MAT", "N limitation",
#                  "Soil N stocks", "MAP", "Soil C:N", "Soil pH", "AM percentage", "Woodiness")
customNames <- c("Altitude", "AM %","Precipitation","Temperature",  "Microbial N stock", "Soil N stock","Nox deposition", "Soil pH", "Woodiness")

im$Feature <- customNames

Nup_imp_xgboost <- ggplot(im, aes(x=Gain, y=reorder(Feature, Gain)))+
  geom_bar(stat="identity", fill="deepskyblue3")+
  theme_classic()+
  ylab("")+
  annotate(geom="text", x=0.25, y=1.5, label="Model r2 = 0.54", cex=4)
Nup_imp_xgboost


tiff(filename="~/Desktop/MIT/Recerca/NitUP/Project/v8/Figures/SM/Nup_xgboost_var_imp.tif",
     width=700, height = 500, res= 150)
Nup_imp_xgboost
dev.off()

varnames <- colnames(nup_model[[5]][[1]])
categoric <- varnames[c(1)]
pdp_plots <- all_plot_pdp(func_boost_output=nup_model, out_var=varnames, ncol=4,
                          nrow=3, categoric = categoric, ymin=30, ymax=120,
                          color="deepskyblue3",ylab="Plant N uptake \n log(kgN/ha/yr)"
                          )
ggpubr::ggarrange(plotlist = pdp_plots, ncol=5, nrow=2)

o <- pdp_plots
pdp_plots <- list(o[[5]],o[[6]],o[[9]],o[[2]],o[[7]],o[[3]],o[[4]],o[[8]],o[[1]])
pdp_plots[[1]][["labels"]][["x"]] <- "MAT (ºC)"
pdp_plots[[2]][["labels"]][["x"]] <- "MAP (mm)"
pdp_plots[[3]][["labels"]][["x"]] <- "Nox deposition (gN/m2)"
pdp_plots[[4]][["labels"]][["x"]] <- "Soil microbial N stocks"
pdp_plots[[5]][["labels"]][["x"]] <- "Altitude (m)"
pdp_plots[[6]][["labels"]][["x"]] <- "AM %"
pdp_plots[[7]][["labels"]][["x"]] <- "Soil pH"
pdp_plots[[8]][["labels"]][["x"]] <- "Soil N stocks (kg/m2)"
pdp_plots[[9]][["labels"]][["x"]] <- "Woodiness"

tiff(filename="~/Desktop/MIT/Recerca/NitUP/Project/v8/Figures/SM/Nup_pdp.tif",
     width=1800, height = 1800, res= 190)
ggpubr::ggarrange(plotlist = pdp_plots, ncol=3, nrow=3)
dev.off()

#### **3.2 NUE ####
load("~/Desktop/MIT/Recerca/NitUP/Project/v8/gapFilled_db_lats_ratios_ndep.RData")
# General_DB <- subset(General_DB, General_DB$NUE < 500)
# # colnames(General_DB)[c(37,39,41,42,43,44,45,48,51,52,53,54,55,56,58)]
# nue_mod <- func_boost(db=General_DB, iterations = 20, y_var_pos=66,
#                                  variables=c(37,45,31,57,53,54,55,50,68),
#                                  max.depth=6, min_child_weight=1, eta=0.30,
#                                  cont_var=c(45,31,57,53,54,55,50,68))
# 
# nue_mod[[1]]
# xgb.plot.importance(nue_mod[[3]])
# validation(nue_mod)
# save(nue_mod, file="~/Desktop/MIT/Recerca/NitUP/Project/v8/nue_xgboost.RData")
load("~/Desktop/MIT/Recerca/NitUP/Project/v8/nue_xgboost.RData")

im <- nue_mod[[3]]
im$Feature
# customNames <- c("MAT * MAP", "Altitude", "Fungal soil biomass", "Bacterial soil biomass", "MAT", "N limitation",
#                  "Soil N stocks", "MAP", "Soil C:N", "Soil pH", "AM percentage", "Woodiness")
customNames <- c("Altitude", "AM %","Precipitation","Temperature",  "Microbial N stock", "Soil N stock", "Nox deposition", "Soil pH", "Woodiness")
im$Feature <- customNames

Nue_imp_xgboost <- ggplot(im, aes(x=Gain, y=reorder(Feature, Gain)))+
  geom_bar(stat="identity", fill="coral")+
  theme_classic()+
  ylab("")+
  annotate(geom="text", x=0.15, y=1.5, label="Model r2 = 0.44", cex=4)
Nue_imp_xgboost


tiff(filename="~/Desktop/MIT/Recerca/NitUP/Project/v8/Figures/SM/Nue_xgboost_var_imp.tif",
     width=700, height = 500, res= 150)
Nue_imp_xgboost
dev.off()

varnames <- colnames(nue_mod[[5]][[1]])
categoric <- varnames[c(1)]

pdp_plots <- all_plot_pdp(func_boost_output=nue_mod, out_var=varnames, ncol=4,
                          nrow=3, categoric = categoric, ymin=65, ymax=150,
                          color="coral",ylab="Plant NUE \n log(KgC/KgN)")
ggpubr::ggarrange(plotlist = pdp_plots, ncol=3, nrow=3)

o <- pdp_plots
pdp_plots <- list(o[[2]],o[[7]],o[[6]],o[[4]],o[[3]],o[[8]],o[[5]],o[[9]],o[[1]])
pdp_plots[[1]][["labels"]][["x"]] <- "Soil microbial N stocks"
pdp_plots[[2]][["labels"]][["x"]] <- "Altitude (m)"
pdp_plots[[3]][["labels"]][["x"]] <- "MAP (mm)"
pdp_plots[[4]][["labels"]][["x"]] <- "Soil pH"
pdp_plots[[5]][["labels"]][["x"]] <- "AM %"
pdp_plots[[6]][["labels"]][["x"]] <- "Soil N stocks (kg/m2)"
pdp_plots[[7]][["labels"]][["x"]] <- "MAT (ºC)"
pdp_plots[[8]][["labels"]][["x"]] <- "Nox deposition"
pdp_plots[[9]][["labels"]][["x"]] <- "Woodiness"

tiff(filename="~/Desktop/MIT/Recerca/NitUP/Project/v8/Figures/SM/Nue_pdp.tif",
     width=1800, height = 1800, res= 190)
ggpubr::ggarrange(plotlist = pdp_plots, ncol=3, nrow=3)
dev.off()

#### 4. Maps ####
#Nup
# Nup <- raster("~/Desktop/MIT/Recerca/NitUP/Project/v8/ProducedMaps/Nup_2km.grd")
# LC_reclass <- raster("~/Desktop/MIT/Recerca/NitUP/Project/v3/LC2km_mask_0.0.grd")
# Nup_mean_masked <- mask(Nup, LC_reclass, filename="~/Desktop/MIT/Recerca/NitUP/Project/v8/ProducedMaps/Nup_mean_masked_2km.grd")
Nup_mean_masked <- raster("~/Desktop/MIT/Recerca/NitUP/Project/v7/ProducedMaps/Nup_mean_masked_2km.grd")
sumatori <- cellStats(Nup_mean_masked, "sum")
sum_maskedTG_km <- sumatori*100*4/1000000000 #### 842.215 ### 

#Sd
# nup_sd <- raster("~/Desktop/MIT/Recerca/NitUP/Project/v8/ProducedMaps/Nup_2km_sd.grd")
# Nup_sd_masked <- mask(nup_sd, LC_reclass, filename="~/Desktop/MIT/Recerca/NitUP/Project/v8/ProducedMaps/Nup_sd_masked_2km.grd")
Nup_sd_masked <- raster("~/Desktop/MIT/Recerca/NitUP/Project/v8/ProducedMaps/Nup_sd_masked_2km.grd")
sumatori <- cellStats(Nup_sd_masked, "sum")
sum_maskedTG_km <- sumatori*100*4/1000000000 #### 236.110 ### 

# cv
Nup_cv <- (Nup_sd_masked/Nup_mean_masked)*100
writeRaster(Nup_cv, filename="~/Desktop/MIT/Recerca/NitUP/Project/v8/ProducedMaps/Nup_cv_masked_2km.grd")

#NUE
# Nue <- raster("~/Desktop/MIT/Recerca/NitUP/Project/v8/ProducedMaps/Nue_2km.grd")
# Nue_mean_masked <- mask(Nue, LC_reclass, filename="~/Desktop/MIT/Recerca/NitUP/Project/v8/ProducedMaps/Nue_mean_masked_2km.grd")
Nue_mean_masked <- raster("~/Desktop/MIT/Recerca/NitUP/Project/v8/ProducedMaps/Nue_mean_masked_2km.grd")
sumatori <- cellStats(Nue_mean_masked, "mean") #### 110.262 ###
# library(raster)
# min <- min(na.exclude(getValues(Nup_mean_masked)))
# max <- max(na.exclude(getValues(Nup_mean_masked)))
# Nup_01 <- (Nup_mean_masked-min)/(max-min)
# 
# min <- min(na.exclude(getValues(Nue_mean_masked)))
# max <- max(na.exclude(getValues(Nue_mean_masked)))
# Nue_01<- (Nue_mean_masked-min)/(max-min)
# 
# std_Nlim <- Nup_01/Nue_01
# Nlim_coarse <- aggregate(std_Nlim, fact=10)
# plot(Nlim_coarse)
# Nlim_df <- as.data.frame(Nlim_coarse, xy=T)
# Nlim_df$layer[Nlim_df$layer > 10] <- 10

#sd
# Nue_sd <- raster("~/Desktop/MIT/Recerca/NitUP/Project/v8/ProducedMaps/Nue_2km_sd.grd")
# Nue_sd_masked <- mask(Nue_sd, LC_reclass, filename="~/Desktop/MIT/Recerca/NitUP/Project/v8/ProducedMaps/Nue_sd_masked_2km.grd")
Nue_sd_masked <- raster("~/Desktop/MIT/Recerca/NitUP/Project/v8/ProducedMaps/Nue_sd_masked_2km.grd")
sumatori <- cellStats(Nue_sd_masked, "mean") #### 19.40 ###

#cv
Nue_cv <- (Nue_sd_masked/Nue_mean_masked)*100
writeRaster(Nue_cv, filename="~/Desktop/MIT/Recerca/NitUP/Project/v8/ProducedMaps/Nue_cv_masked_2km.grd")

#### **4.1 Maps plots ####
Nup <- raster("~/Desktop/MIT/Recerca/NitUP/Project/v8/ProducedMaps/Nup_mean_masked_2km.grd")
Nup_coarse <- aggregate(Nup, fact=10)
Nup_df <- as.data.frame(Nup_coarse, xy=T)

library(maptools)
library(ggplot2)
library(viridis)
proj <- CRS('+proj=longlat +ellps=WGS84')
mapaSHP <- readShapeLines('~/Desktop/Batch maps/Global Maps/ne_50m_coastline/ne_50m_coastline.shp',
                          proj=proj)
# hist(Nup_df$layer)
# Nup_df$layer[Nup_df$layer > 120] <- 120

means <- aggregate(Nup_df$layer, by=list(Nup_df$y), FUN=mean, na.rm=T)

bar_nup <- ggplot(means)+
  geom_bar(aes(x=Group.1, y=x), stat="identity", width = 1, col="gray60", fill="gray60")+
  coord_flip() +
  ylim(c(0,150)) +
  geom_hline(yintercept = 0) +
  # geom_hline(yintercept = mean(na.exclude(Nup_df$layer)), col="red") +
  theme(axis.line=element_blank(),
        # axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank())

map_Nup <- ggplot(Nup_df) + 
  geom_raster(aes(x, y, fill=layer)) +
  geom_polygon(data = mapaSHP, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  # scale_fill_gradient2(low = "#0000FFFF", mid = "white", high = "#FF0000FF", midpoint = 0, na.value = NA) +
  scale_fill_viridis(na.value=NA, direction = -1, option="G")+
  theme_minimal()+
  ylim(c(-60,90)) +
  labs(fill = "N uptake \n (KgN/ha/yr)") +
  ggtitle("a) Global nitrogen uptake: 842.215 ± 236.110 Tg N/yr")+
  theme(legend.position = "left") +
  xlab("") +
  ylab("")

# Nue
Nue <- raster("~/Desktop/MIT/Recerca/NitUP/Project/v8/ProducedMaps/Nue_mean_masked_2km.grd")
Nue_coarse <- aggregate(Nue, fact=10)
Nue_df <- as.data.frame(Nue_coarse, xy=T)

library(maptools)
library(ggplot2)
library(viridis)
proj <- CRS('+proj=longlat +ellps=WGS84')
mapaSHP <- readShapeLines('~/Desktop/Batch maps/Global Maps/ne_50m_coastline/ne_50m_coastline.shp',
                          proj=proj)

hist(Nue_df$layer)
# Nue_df$layer[Nue_df$layer > 110] <- 110
# Nue_df$layer[Nue_df$layer < 60] <- 60

means <- aggregate(Nue_df$layer, by=list(Nue_df$y), FUN=mean, na.rm=T)

bar_nue <- ggplot(means)+
  geom_bar(aes(x=Group.1, y=x), stat="identity", width = 1, col="gray60", fill="gray60")+
  coord_flip() +
  ylim(c(0,150)) +
  geom_hline(yintercept = 0) +
  # geom_hline(yintercept = mean(na.exclude(Nue_df$layer)), col="red") +
  theme(axis.line=element_blank(),
        # axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank())

map_Nue <- ggplot(Nue_df) + 
  geom_raster(aes(x, y, fill=layer)) +
  geom_polygon(data = mapaSHP, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  # scale_fill_gradient2(low = "#0000FFFF", mid = "white", high = "#FF0000FF", midpoint = 0, na.value = NA) +
  scale_fill_viridis(na.value=NA, direction = -1, option="A")+
  theme_minimal()+
  ylim(c(-60,90)) +
  labs(fill = "NUE \n (KgC/kgN)") +
  ggtitle("b) Average nitrogen use efficiency: 110.262 ± 19.40 kg C/Kg N")+
  theme(legend.position = "left") +
  xlab("") +
  ylab("")

library(ggpubr)
tiff(filename="~/Desktop/MIT/Recerca/NitUP/Project/v9/Figures/Nup_Nue_maps.tif",
     width=1500, height = 1400, res= 160)
ggarrange(map_Nup, bar_nup, map_Nue, bar_nue, 
          nrow=2, ncol=2, align = "h", widths = c(9, 1))
dev.off()

#### 5. TRENDY comparison ####
# load("~/Desktop/MIT/Recerca/NitUP/Project/v2/stack_correlation_TRENDY.RData")
# names(nup_stack_homogenized) <- c("CABLE-POP", "CLM5.0", "DLEM", "JSBACH", "JULES", "LPJ-GUESS", "LPX-Bern", "ORCHIDEE", "Upscaling")
# 
# nup_stack_homogenized[[3]] <- nup_stack_homogenized[[3]]/1000
# nup_stack_homogenized <- stack(nup_stack_homogenized)
# nup_stack_homogenized <-  nup_stack_homogenized[[1:8]]
# mean_trendy <- calc(nup_stack_homogenized, fun = mean)
# 
# Nup <- raster("~/Desktop/MIT/Recerca/NitUP/Project/v8/ProducedMaps/Nup_mean_masked_2km.grd")
# Nup <- resample(Nup, mean_trendy)
# 
# dif <- mean_trendy-Nup
# plot(dif)
# save(dif, file = "~/Desktop/MIT/Recerca/NitUP/Project/v8/ProducedMaps/dif_map_TRENDY.Rdata")
# 
# dif_pos <- dif
# dif_pos[dif_pos<0] <- NA
# 
# dif_neg <- dif
# dif_neg[dif_neg>0] <- NA
# 
# pond_dif <- (dif/Nup)
# test <- mask(pond_dif, dif_pos)
# dif <- stack(mean_trendy, Nup, dif, dif_pos, dif_neg, pond_dif)
# names(dif) <- c("mean_trendy", "Nup", "dif", "dif_pos", "dif_neg", "dif_pond")
# #
# # General_DB <- read.csv("~/Desktop/MIT/Recerca/NitUP/Project/v3/General_DB_GapFilled_11maig23.csv")
# # NP_point <- SpatialPointsDataFrame(coords = General_DB[,c(5,4)], data = General_DB,
# #                                    proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
# #
# # dif_db <- extract(dif, NP_point)
# # General_DB <- cbind(General_DB, dif_db)
# # colnames(General_DB)[56:59] <- c("dif", "dif_pos", "dif_neg", "dif_pond")
# # save(General_DB, file = "~/Desktop/MIT/Recerca/NitUP/Project/v4/General_DB_dif.Rdata")
# save(dif, file = "~/Desktop/MIT/Recerca/NitUP/Project/v8/ProducedMaps/dif_stack_TRENDY.Rdata")

load("~/Desktop/MIT/Recerca/NitUP/Project/v8/ProducedMaps/dif_stack_TRENDY.Rdata")
# load("~/Desktop/MIT/Recerca/NitUP/Project/v5/ProducedMaps/dif_map_TRENDY.Rdata")
dif_map <- dif$dif
dif_map <- as.data.frame(dif_map,xy=T)

dev_map <- ((dif$mean_trendy-dif$Nup)/(dif$Nup))*100
dev_map <- as.data.frame(dev_map,xy=T)
colnames(dev_map)[3] <- "dev"

library(maptools)
library(ggplot2)
proj <- CRS('+proj=longlat +ellps=WGS84')
mapaSHP <- readShapeLines('~/Desktop/Batch maps/Global Maps/ne_50m_coastline/ne_50m_coastline.shp',
                          proj=proj)

means_dif <- aggregate(dif_map$dif, by=list(dif_map$y), FUN=mean, na.rm=T)
means_dev <- aggregate(dev_map$dev, by=list(dev_map$y), FUN=mean, na.rm=T)

bar_dif <- ggplot(means_dif)+
  geom_bar(aes(x=Group.1, y=x), stat="identity", width = 1, col="gray60", fill="gray60")+
  coord_flip() +
  # ylim(c(-60,90)) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = mean(na.exclude(dif_map$dif)), col="red") +
  theme(axis.line=element_blank(),
        # axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank())


map_dif <- ggplot(dif_map) + 
  geom_raster(aes(x, y, fill=dif)) +
  geom_polygon(data = mapaSHP, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  scale_fill_gradient2(low = "dodgerblue", mid = "white", high = "red", midpoint = 0, na.value = NA) +
  theme_minimal()+
  ylim(c(-60,90)) +
  labs(fill = "TRENDY \n       -\n empirical \n (kg N/ha/yr)") +
  theme(legend.position = "left") +
  xlab("") +
  ylab("")

bar_dev <- ggplot(means_dev)+
  geom_bar(aes(x=Group.1, y=x), stat="identity", width = 1, col="gray60", fill="gray60")+
  coord_flip() +
  # ylim(c(-60,90)) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = mean(na.exclude(dev_map$dev)), col="red") +
  theme(axis.line=element_blank(),
        # axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank())

map_dev <- ggplot(dev_map) + 
  geom_raster(aes(x, y, fill=dev)) +
  geom_polygon(data = mapaSHP, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  scale_fill_gradient2(low = "dodgerblue", mid = "white", high = "red", midpoint = 0, na.value = NA) +
  theme_minimal()+
  ylim(c(-60,90)) +
  labs(fill = "TRENDY \n deviation %") +
  # theme(legend.position = "left", legend.title.align = 1) +
  theme(legend.position = "left") +
  xlab("") +
  ylab("")

library(ggpubr)
# ggarrange(map, bar, ncol=2, widths = c(8, 2), align = "h")

tiff("~/Desktop/MIT/Recerca/NitUP/Project/v9/Figures/TRENDY.tiff", width = 1800, height = 1600, res=180)
ggarrange(map_dif, bar_dif, map_dev, bar_dev,
          ncol=2, nrow=2, widths = c(8, 2), align = "h",
          labels = c("a)","","b)",""))
dev.off()

#### ///// SM FIGURES ///// ####
#### SM ####
#### 1. Sampling maps ####
library(ggmap)
library(maptools)
library(maps)
library(stringi)
library(grDevices)
library(sf)
library(rgdal)
unique(General_DB$woodiness)
load("~/Desktop/MIT/Recerca/NitUP/Project/v8/gapFilled_db_lats_ratios_ndep.RData")
proj <- CRS('+proj=longlat +ellps=WGS84')
mapaSHP <- readOGR('~/Desktop/Batch maps/Global Maps/ne_50m_coastline/ne_50m_coastline.shp')

# General_DB <- subset(General_DB, General_DB$Forest_type != "Mixed forest")
General_DB$Latitude <- round(General_DB$Latitude, 0)
General_DB$Longitude <- round(General_DB$Longitude, 0)

General_DB$Lat_id <- paste(General_DB$Latitude, General_DB$Longitude, sep="_")
a <- plyr::count(General_DB$Lat_id)

for (i in 1:nrow(General_DB)) {
  for (j in 1:nrow(a)) {
    if(isTRUE(General_DB$Lat_id[i]==a$x[j])) {
      General_DB$rep[i] <- a$freq[j]
    }
  }
}

map_sampling <- ggplot(General_DB, aes(x=Longitude, y=Latitude)) + 
  geom_polygon(data = mapaSHP, aes(x = long, y = lat, group = group), size=0.3, colour = "black", fill = NA) +
  # geom_point(aes(col=Forest_type),position = position_jitter(width = 3, height = 3))+
  geom_point(aes(fill=woodiness, size=rep), shape=21, alpha=0.6)+
  theme_minimal() +
  labs(fill="Vegetation type", size="Sample size") +
  ylim(c(-60,90)) 
map_sampling

tiff(filename="~/Desktop/MIT/Recerca/NitUP/Project/v8/Figures/SM/Data_points.tif",
     width=2100, height = 1000, res= 200)
map_sampling
dev.off()

#### 2. Whittaker ####
library(plotbiomes)
library(ggplot2)

plot_1 <- ggplot() +
  # add biome polygons
  geom_polygon(data = Whittaker_biomes, aes(x    = temp_c,
                   y    = precp_cm, fill = biome),
               # adjust polygon borders
               colour = "gray98", size   = 1) +
  geom_point(data=General_DB, aes(x = MAT_wc, y = MAP_wc/10),
             size=3, position = position_jitter(width = 1.5, height = 1.5), shape  = 21,colour = "gray95",fill   = "black",stroke = 1,alpha  = 0.5) +
  theme_minimal() +
  xlab("Mean annual temperature") +
  ylab("Mean annual precipitation")

tiff(filename="~/Desktop/MIT/Recerca/NitUP/Project/v8/Figures/SM/whittaker_biomes.tif",
     width=1400, height = 1000, res= 150)
plot_1
dev.off()

#### 3. TRENDY individual ####
load("~/Desktop/MIT/Recerca/NitUP/Project/v6/ProducedMaps/TRENDY_individual.Rdata")
names(nup_stack_all)[9:10] <- c("Nup", "sd")

Nup_valor_final <- c()
for (i in 1:10){
  sumatori <- cellStats(nup_stack_all[[i]], "sum")
  Nup_valor_final[i] <- sumatori*302500/1000000000 #
}


# # Nup_valor_final[9] <- sumatori*100/1000000000
# 
Names <- c("CABLE-POP", "CLM5.0", "DLEM", "JSBACH", "JULES", "LPJ-GUESS", "LPX-Bern", "ORCHIDEE", "Upscaling")

Results <- data.frame(Names, Nup_valor_final[1:9])
# Results[3,2] <- Results[3,2]/1000
Results[9,2] <- 842.215
Nup_valor_final[9] <- 842.215
Nup_valor_final[10] <- 236.110

library(ggplot2)
res <- ggplot(mapping = aes(x = Results$Names, y = Results$Nup_valor_final)) +
  geom_point(size=4) +
  # geom_hline(yintercept=Nup_valor_final[9]-(Nup_valor_final[10]), linetype="dashed", color = "red") +
  # geom_hline(yintercept=Nup_valor_final[9]+(Nup_valor_final[10]), linetype="dashed", color = "red") +
  geom_rect(aes(xmin=-Inf, xmax=Inf, ymin=Nup_valor_final[9]-(Nup_valor_final[10]),
                ymax=Nup_valor_final[9]+(Nup_valor_final[10])), fill="gray80") +
  coord_flip() + 
  theme(axis.text.x = element_text(size=18))+
  geom_point(size=4) +
  geom_point(aes(x=9,y=Results[9,2]), colour="red", size=4) +
  xlab("") + ylab("TgN/yr")+
  ylim(400,1700)+
  theme_minimal()
res
tiff("~/Desktop/MIT/Recerca/NitUP/Project/v8/Figures/SM/TRENDY_individual.tiff", width = 600, height = 500, res=150)
res
dev.off()

save(Results, file="~/Desktop/MIT/Recerca/NitUP/Project/v2/Result_TRENDY_comparison.RData")

#### 4. NPP ####
load("~/Desktop/MIT/Recerca/NitUP/Project/v8/gapFilled_db_lats_ratios_ndep.RData")

for (i in 1:159) {
  if (is.na(General_DB$Nstock_wood[i])) {
    General_DB$total_plant_Nstocks[i] <- General_DB$Nstock_leaves[i]+General_DB$Nstock_roots[i]
  } else {
    General_DB$total_plant_Nstocks[i] <- General_DB$Nstock_leaves[i]+General_DB$Nstock_wood[i]+General_DB$Nstock_roots[i]
  }
}
colnames(General_DB)
m1 <- lm(log(Net_Nuptake_Combi/totalNPP) ~ woodiness, data=General_DB)
summary(m1)
hist(resid(m1))

m3 <- lm(log(Net_Nuptake_Combi) ~ woodiness, data=General_DB)
summary(m3)
hist(resid(m3))

m4 <- lm(log(total_plant_Nstocks) ~ woodiness, data=General_DB)
summary(m4)
hist(resid(m4))

colnames(General_DB)

NPP_w <- ggplot(data=General_DB, aes(x=woodiness, y=log(totalNPP))) +
  geom_violin(fill="gray", scale="width") +
  geom_boxplot(width=0.1, color="black", alpha=0.2) +
  geom_signif(comparisons = list(c("Non-woody", "Woody")),
              y_position = c(10.5),
              map_signif_level=TRUE)+
  theme_classic() +
  theme(axis.text.x = element_text(size=13),
        axis.text.y = element_text(size=13))+
  scale_x_discrete(labels=c("Grassland","Woody")) +
  ylab("Plant NPP log(kgC/ha/yr)")+
  # ylab("")+
  xlab("")

tiff(filename="~/Desktop/MIT/Recerca/NitUP/Project/v8/Figures/SM/Npp_woodiness.tif",
     width=800, height = 800, res= 150)
NPP_w
dev.off()

#### 5. TRENDY mean ####
load("~/Desktop/MIT/Recerca/NitUP/Project/v8/ProducedMaps/dif_stack_TRENDY.Rdata")
mean_trendy <- as.data.frame(dif$mean_trendy,xy=T)
library(maptools)
library(ggplot2)
proj <- CRS('+proj=longlat +ellps=WGS84')
mapaSHP <- readShapeLines('~/Desktop/Batch maps/Global Maps/ne_50m_coastline/ne_50m_coastline.shp',
                          proj=proj)

# means <- aggregate(dif_map$dif, by=list(dif_map$y), FUN=mean, na.rm=T)
# 
# bar <- ggplot(means)+
#   geom_bar(aes(x=Group.1, y=x), stat="identity", width = 1, col="gray60", fill="gray60")+
#   coord_flip() +
#   ylim(c(-60,90)) +
#   geom_hline(yintercept = 0) +
#   geom_hline(yintercept = mean(na.exclude(dif_map$dif)), col="red") +
#   theme(axis.line=element_blank(),
#         # axis.text.x=element_blank(),
#         axis.text.y=element_blank(),
#         axis.ticks=element_blank(),
#         axis.title.x=element_blank(),
#         axis.title.y=element_blank(),
#         legend.position="none",
#         panel.background=element_blank(),
#         panel.border=element_blank(),
#         panel.grid.major=element_blank(),
#         panel.grid.minor=element_blank(),
#         plot.background=element_blank())


map_Nup <- ggplot(mean_trendy) + 
  geom_raster(aes(x, y, fill=mean_trendy)) +
  geom_polygon(data = mapaSHP, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  # scale_fill_gradient2(low = "#0000FFFF", mid = "white", high = "#FF0000FF", midpoint = 0, na.value = NA) +
  scale_fill_viridis(na.value=NA, direction = -1, option="H")+
  theme_minimal()+
  ylim(c(-60,90)) +
  labs(fill = "N uptake \n (KgN/ha/yr)") +
  theme(legend.position = "left") +
  xlab("") +
  ylab("")
map_Nup
library(ggpubr)
# ggarrange(map, bar, ncol=2, widths = c(8, 2), align = "h")

tiff("~/Desktop/MIT/Recerca/NitUP/Project/v8/Figures/SM/mean_TRENDY.tiff", width = 1800, height = 800, res=180)
map_Nup
dev.off()

##### 6. Ndep ####
library(rgdal)
library(ncdf4)
library(raster)
# noy <- nc_open("~/Desktop/Batch maps/N dep/ndep-nhx_1901soc_monthly_1901_2021.nc")
# lon <- ncvar_get(noy, "lon")
# lat <- ncvar_get(noy, "lat", verbose = F)
# t <- ncvar_get(noy, "time")
# nup.array <- ncvar_get(noy, "nhx")
# fillvalue <- noy[["var"]][["nhx"]][["missval"]]
# nup.array[nup.array == fillvalue] <- NA
# nc_close(Cable)
# 
# noy_1901_2021 <-vector("list", 1452)
# 
# for(j in 1:1452){
#   nup.slice <- nup.array[, ,j] 
#   r <- raster(t(nup.slice), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
#   noy_1901_2021[j] <- r
# }
# 
# nup_stack <- stack(noy_1901_2021)
# total_noy <- calc(nup_stack, sum)
# total_nhx <- total_noy
# plot(total_noy)
# 
# writeRaster(total_nhx, filename = "~/Desktop/Batch maps/N dep/ndep-nhx_1901_2021_ISIMIP.tif")
# 
# ##
# noy <- nc_open("~/Desktop/Batch maps/N dep/ndep-noy_1901soc_monthly_1901_2021.nc")
# lon <- ncvar_get(noy, "lon")
# lat <- ncvar_get(noy, "lat", verbose = F)
# t <- ncvar_get(noy, "time")
# nup.array <- ncvar_get(noy, "noy")
# fillvalue <- noy[["var"]][["noy"]][["missval"]]
# nup.array[nup.array == fillvalue] <- NA
# nc_close(noy)
# 
# noy_1901_2021 <-vector("list", 1452)
# 
# for(j in 1:1452){
#   nup.slice <- nup.array[, ,j] 
#   r <- raster(t(nup.slice), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
#   noy_1901_2021[j] <- r
# }
# 
# nup_stack <- stack(noy_1901_2021)
# plot(nup_stack[[1400]])
# total_noy <- calc(nup_stack, sum)
# plot(total_noy)
# 
# writeRaster(total_noy, filename = "~/Desktop/Batch maps/N dep/ndep-noy_1901_2021_ISIMIP.tif")

noy <- raster("~/Desktop/Batch maps/N dep/ndep-noy_1901_2021_ISIMIP.tif")
noy_map <- as.data.frame(noy,xy=T)
library(maptools)
library(ggplot2)
proj <- CRS('+proj=longlat +ellps=WGS84')
mapaSHP <- readShapeLines('~/Desktop/Batch maps/Global Maps/ne_50m_coastline/ne_50m_coastline.shp',
                          proj=proj)

map_noy <- ggplot(noy_map) + 
  geom_raster(aes(x, y, fill=ndep.noy_1901_2021_ISIMIP)) +
  geom_polygon(data = mapaSHP, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  # scale_fill_gradient2(low = "#0000FFFF", mid = "white", high = "#FF0000FF", midpoint = 0, na.value = NA) +
  scale_fill_viridis(na.value=NA, direction = -1, option="A")+
  theme_minimal()+
  ylim(c(-60,90)) +
  labs(fill = "NUE \n (KgC/kgN)") +
  ggtitle("Accumulated Nox deposition 1901 - 2021")+
  theme(legend.position = "left") +
  xlab("") +
  ylab("")

map_noy
tiff(filename="~/Desktop/MIT/Recerca/NitUP/Project/v8/Figures/SM/Nop_map.tif",
     width=1200, height = 700, res= 160)
map_noy
dev.off()
