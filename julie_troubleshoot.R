rm(list = ls())

#load("C:/Users/mfarr/Documents/R_files/Spotfire.data/midland_tbls.RData")
load("C:/Users/mfarr/Documents/R_files/Spotfire.data/midland_avg.RData")

write.csv(prod_tbl, file = "midland_prod.csv")

###script from julie==================================================================

##parameter input=====================================================================
prod_tbl <- tolkien


#trying to keep Windows 10 from deleting temp files
#cat(file=tempfile())
#library(rlang,  warn.conflicts = FALSE)
#library(tibble, warn.conflicts = FALSE)
library(dplyr,  warn.conflicts = FALSE)

##clean up the data
names(prod_tbl) <- gsub("_", "", colnames(prod_tbl)) #remove underscore from column names 
names(prod_tbl) <- gsub(" ", "", colnames(prod_tbl)) #remove spaces from column names 
names(prod_tbl) <- gsub("-", "", colnames(prod_tbl)) #remove dashes from column names
names(prod_tbl) <- gsub("[()]", "", colnames(prod_tbl))



minWellCount <- 0

##filter and rename the production table, which is the tolkien table. This section of code is creating the averages for the Tolkien table. 

input1 <- prod_tbl %>%
  
  ##need to ensure Time is time/data units for consistency
  mutate(c.MASTERDATE2 = as.POSIXct(as.Date(c.MASTERDATE2, "%m/%d/%Y"), 
                                    origin = "1970-01-01", tz="UTC")) %>%
  filter(!is.na(c.MASTERDATE2)) %>%
  select(API10,	
         c.MASTERDATE2,
         c.DailyAvgGasNormLL,
         c.DailyAvgLiquidNormLL,
         c.DailyAvgWaterNormLL,
         c.DailyAvgBOENormLL, 
         c.DailyAvgGORNormLL, 
         c.DailyAvgWORNormLL,
         c.DailyAvgOilCutNormLL, 
         c.DailyAvgWaterCutNormLL, 
         c.DailyAvgFluidRateNormLL)


####create table called HierarchyAverage to house the data used for DCA
if(nrow(input1) < 1)
{#in no wells are selected, create an Average table with zeros 
  HierarchyAverage <- data.frame(Name = c("None"), 
                                 Days = c(0), 
                                 Gas = c(0), 
                                 Oil = c(0), 
                                 Water = c(0), 
                                 BOEPD = c(0), 
                                 GOR = c(0), 
                                 WOR = c(0),
                                 OilCut = c(0), 
                                 WaterCut = c(0), 
                                 TotalFluid = c(0), 
                                 WellCount = c(0))
  
}else{
  
  ##################dplyr package used for data wrangling
  HierarchyAverage <- input1 %>%
    arrange(API10, 
            c.MASTERDATE2) %>%
    group_by(API10) %>%
    mutate(RowCount = 1, #create a column for wellcount by placeing a 1 in every row
           Days = cumsum(RowCount)) %>%
    group_by(Days) %>%
    summarise(Gas = mean(c.DailyAvgGasNormLL, na.rm = TRUE), #mcf
              Oil = mean(c.DailyAvgLiquidNormLL, na.rm = TRUE), #bbl
              Water = mean(c.DailyAvgWaterNormLL, na.rm = TRUE), #bbl
              BOE = mean(c.DailyAvgBOENormLL, na.rm = TRUE), #bbl
              GOR = mean(c.DailyAvgGORNormLL, na.rm = TRUE), #scf/bbl
              WOR = mean(c.DailyAvgWORNormLL, na.rm = TRUE), #bbl/bbl
              OilCut = mean(c.DailyAvgOilCutNormLL, na.rm = TRUE), #%
              WaterCut = mean(c.DailyAvgWaterCutNormLL, na.rm = TRUE), #%
              TotalFluid = mean(c.DailyAvgFluidRateNormLL, na.rm = TRUE), #bbl
              WellCount = sum(RowCount)) %>%  #sum up RowCount which will give you a wellcount column
    filter(WellCount > minWellCount) %>%
    mutate(WellName = "Average") %>%
    select(WellName, Days, Gas, Oil, Water, BOE, GOR, WOR, TotalFluid, OilCut, WaterCut, WellCount)
  
  #######################
  
}


##filter and rename the production table
input2 <- prod_tbl %>%
  ##need to ensure Time is time/data units for consistency
  mutate(c.MASTERDATE2 = as.POSIXct(as.Date(c.MASTERDATE2, "%m/%d/%Y"), 
                                    origin = "1970-01-01", tz="UTC")) %>%
  filter(!is.na(c.MASTERDATE2)) %>%
  select(API10,
         c.MASTERDATE2,
         c.BDDBDailyGasNormLL,
         c.BDDBDailyOilNormLL,
         c.BDDBDailyWaterNormLL,
         c.BDDBDailyBOENormLL, 
         c.BDDBGORNormLL, 
         c.BDDBWORNormLL, 
         c.BDDBOilCutNormLL,
         c.BDDBWaterCutNormLL, 
         c.BDDBFluidRateNormLL)

####create table called BDDBAverage to house the data used for DCA
if(nrow(input2) < 1)
{#in no wells are selected, create an Average table with zeros 
  BDDBAverage <- data.frame(BDDB_Name = c("None"), 
                            BDDB_Days = c(0), 
                            BDDB_Gas = c(0), 
                            BDDB_Oil = c(0), 
                            BDDB_Water = c(0), 
                            BOEPD = c(0), 
                            BDDB_GOR = c(0), 
                            BDDB_WOR = c(0), 
                            BDDB_OilCut = c(0), 
                            BDDB_WaterCut = c(0), 
                            BDDB_TotalFluid = c(0), 
                            BDDB_WellCount = c(0))
  
}else{
  
  ##################dplyr package used for data wrangling
  BDDBAverage <- input2 %>%
    arrange(API10, 
            c.MASTERDATE2) %>%
    group_by(API10) %>%
    mutate(RowCount = 1, #create a column for wellcount by placeing a 1 in every row
           BDDB_Days = cumsum(RowCount)) %>%
    group_by(BDDB_Days) %>%
    summarise(BDDB_Gas = mean(c.BDDBDailyGasNormLL, na.rm = TRUE), #mcf
              BDDB_Oil = mean(c.BDDBDailyOilNormLL, na.rm = TRUE), #bbl
              BDDB_Water = mean(c.BDDBDailyWaterNormLL, na.rm = TRUE), #bbl
              BDDB_BOE = mean(c.BDDBDailyBOENormLL, na.rm = TRUE), #bbl
              BDDB_GOR = mean(c.BDDBGORNormLL, na.rm = TRUE), #scf/bbl
              BDDB_WOR = mean(c.BDDBWORNormLL, na.rm = TRUE), #bbl/bbl
              BDDB_OilCut = mean(c.BDDBOilCutNormLL, na.rm = TRUE), #%
              BDDB_WaterCut = mean(c.BDDBWaterCutNormLL, na.rm = TRUE), #%
              BDDB_TotalFluid = mean(c.BDDBFluidRateNormLL, na.rm = TRUE), #bbl
              BDDB_WellCount = sum(RowCount)) %>%  #sum up RowCount which will give you a wellcount column
    filter(BDDB_WellCount > minWellCount) %>%
    mutate(BDDB_WellName = "Average") %>%
    select(BDDB_WellName, 
           BDDB_Days, 
           BDDB_Gas, 
           BDDB_Oil, 
           BDDB_Water, 
           BDDB_BOE, 
           BDDB_GOR, 
           BDDB_WOR,
           BDDB_TotalFluid, 
           BDDB_OilCut, 
           BDDB_WaterCut, 
           BDDB_WellCount)
  
  #######################
  
}

