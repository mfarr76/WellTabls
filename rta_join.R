rm(list = ls())

##loadspotfire data======================================================================
#load("C:/Users/MFARR/Documents/R_files/Spotfire.data/cumprod_1.RData")
#load("C:/Users/MFARR/Documents/R_files/Spotfire.data/prod_test.RData")
load("C:/Users/MFARR/Documents/R_files/Spotfire.data/daily_tables.RData")

library(dplyr, warn.conflicts = FALSE)
library(purrr, warn.conflicts = FALSE)
library(broom, warn.conflicts = FALSE)
library(stringr, warn.conflicts = FALSE)



names(geo) <- gsub("-","", names(geo))
names(geo) <- gsub(" ", "_", names(geo))
names(geo) <- gsub("[()]","", names(geo))
names(geo) <- gsub("-","", names(geo))

join <- left_join(geo, rta, by = c("AM_API" = "API"))

sapply(df, function(x) sum(is.na(x)))

df <- join %>%
  select(-dplyr::contains("cum")) %>%
  filter(!is.na(AcK_sqrtT_LateTime), 
         !is.na(Gas_EUR_BCF)) %>% 
  discard(~any(is.na(.x))) %>%
  keep(is.numeric)

#df[,!map_lgl(df, ~any(is.na(.x)))]

#df <- df[!grepl("cum", colnames(df), ignore.case = TRUE)]


#numericVars <- which(sapply(df, is.numeric)) #index vector numeric variables
#numericVarNames <- names(numericVars) #saving names vector for use later on
#df <- df[,numericVarNames] #remove non numeric columns


df %>%
  select(-AcK_sqrtT_LateTime) %>% #exclude outcome, leave only predictors
  map(~lm(df$AcK_mbt_LateTime ~ .x, data = df)) %>%
  map(summary) %>%
  map_dbl("r.squared") %>%
  tidy %>%
  arrange(desc(x)) %>%
  rename(r.squard = x) -> r2s



df %>%
map(~lm(AcK_sqrtT_LateTime ~ .x, data = .x)) %>%
  map(summary) %>%
  map_dbl("r.squared")




