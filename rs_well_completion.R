rm(list = ls())

load("C:/Users/mfarr/Documents/R_files/Spotfire.data/wrangler_tbls.RData")

suppressWarnings(library(dplyr,warn.conflicts=FALSE))
suppressWarnings(library(lubridate,warn.conflicts=FALSE))
#library(purrr, warn.conflicts = FALSE)
#library(broom, warn.conflicts = FALSE)
#library(stringr, warn.conflicts = FALSE)


##load into spotfire=====================================================================

suppressWarnings(library(dplyr,warn.conflicts=FALSE))
suppressWarnings(library(lubridate,warn.conflicts=FALSE))

##input parameters
wellhead
wellspacing
prod
geo


#get a list of data.frames
file.names<-ls()[sapply(ls(), function(x) class(get(x))) == 'data.frame']

for(i in 1:length(file.names)){
  
  tmp<- get(file.names[i])
  names(tmp)<-gsub(" ","_", names(tmp))
  names(tmp)<-gsub("[]$*+.?[^{|(\\#%&~/<=>'!,:;`\")}@-]","", names(tmp))
  assign(file.names[i], tmp)
  rm(tmp)
  
}



join <- 
  wellhead %>%
  select(UsApiUwi14, CompletionId, RsOperator, WellName, FirstProdDate, 
         Latitude, LatitudeBH, Longitude, LongitudeBH, RsProdWellType,
         OilGravityApi, PerfIntervalFt, ProppantLbs, 
         TotalFluidPumpedBbl, ProppantIntensityLbsPerFt, TotalFluidPumpedBblPerLateralLengthFt, 
         ProppantLoadingLbsPerGal, GasTestRateMcfPerDay, OilTestRateBblPerDay) %>%
  left_join(., prod %>% 
              select(UsApiUwi14, ProducingMonth, GasProdMcf, LiquidsProdBbl,
                     WaterProdBbl, PdGasMcfPerDay, PdLiquidsBblPerDay, PdProdBoePerDay, 
                     CumGasMcf, CumLiquidsBbl, CumProdBoe), 
            by = "UsApiUwi14") %>% 
  left_join(., wellspacing, by = c("UsApiUwi14" = "API")) %>%
  left_join(., geo, by = c("UsApiUwi14" = "WellID")) %>%
  mutate(Vintage = as.character(year(FirstProdDate)),
         LongitudeBH = ifelse(LongitudeBH == 0, Longitude, LongitudeBH), 
         LatitudeBH = ifelse(LatitudeBH == 0, Latitude, LatitudeBH), 
         PrimaryPhase = ifelse(grepl("GAS",RsProdWellType),'GAS','OIL'), 
         prod = ifelse(PrimaryPhase == "OIL" & LiquidsProdBbl > 30 | 
                         PrimaryPhase == "GAS" & GasProdMcf > 30, 1, NA)) %>%
  arrange(UsApiUwi14, ProducingMonth) %>%
  group_by(UsApiUwi14) %>%
  mutate(MONTHS = cumsum(ifelse(!is.na(prod), prod, 0)), 
         MONTHS = replace(MONTHS, which(is.na(prod)), NA),
         CUM3MO_OilMbo = mean(ifelse(MONTHS == 3, CumLiquidsBbl / 1000, NA), na.rm = TRUE), 
         CUM3MO_GasMMcf = mean(ifelse(MONTHS == 3, CumGasMcf / 1000, NA), na.rm = TRUE), 
         CUM6MO_OilMbo = mean(ifelse(MONTHS == 6, CumLiquidsBbl / 1000, NA), na.rm = TRUE), 
         CUM6MO_GasMMcf = mean(ifelse(MONTHS == 6, CumGasMcf / 1000, NA), na.rm = TRUE), 
         CUM12MO_OilMbo = mean(ifelse(MONTHS == 12, CumLiquidsBbl / 1000, NA), na.rm = TRUE), 
         CUM12MO_GasMMcf = mean(ifelse(MONTHS == 12, CumGasMcf / 1000, NA), na.rm = TRUE))



##create a Rdata file to load in R========================================================
TimeStamp=paste(date(),Sys.timezone())
tdir = 'C:/Users/MFARR/Documents/R_files/Spotfire.data' # place to store diagnostics if it exists (otherwise do nothing)
if(file.exists(tdir) && file.info(tdir)$isdir) suppressWarnings(try(save(list=ls(), file=paste(tdir,'/rs_join.RData',sep=''), RFormat=T )))


##data function to turn summarize prod table into 1 row per well==========================


suppressWarnings(library(dplyr,warn.conflicts=FALSE))

#input table
join



JOIN.WH <- join %>%
  select(UsApiUwi14,Vintage, HS_TC_GROUPS, Latitude, Longitude, LatitudeBH, LongitudeBH,
         PrimaryPhase, OilGravityApi, PerfIntervalFt,
         TotalFluidPumpedBbl, TotalFluidPumpedBblPerLateralLengthFt, ProppantLbs, 
         ProppantIntensityLbsPerFt, ProppantLoadingLbsPerGal, TVDSS, LEFTVDSS, LEFThickness, LEFSoPhiH, 
         LEFAvgPhi, LEFAvgSw, UEFThickness, UEFSoPhiH, UEFAvgPhi, UEFAvgSw, ReservoirPressure, ReservoirTemp, 
         ReservoirPressureGradient, CUM12MO_OilMbo, CUM12MO_GasMMcf, 
         YIELD_6MO, HS_Groups, Spacing_Category,
         eurLatest.gas, eurLatest.oil, eurLatest.gor, EURLiquid_Mbo_NORM, EURGas_MMcf_NORM) %>%
  group_by(UsApiUwi14) %>%
  #summarize_all(mean)
  summarise_all(funs(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.)))
  

  
