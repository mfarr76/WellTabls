rm(list = ls())

#load("C:/Users/MFARR/Documents/R_files/Spotfire.data/writetoaccess.RData")
load("C:/Users/MFARR/Documents/R_files/Spotfire.data/wrangler_tbls.RData")

#prod <- read.csv("decline.csv")

suppressWarnings(library(dplyr,warn.conflicts=FALSE))
library(ggplot2)

#prod <- prod %>%
#  mutate(PROD_MONTH = as.Date(PROD_MONTH, "%m/%d/%Y")) %>%
#  arrange(API, PROD_MONTH)

names(wellhead) = gsub(" ", "", names(wellhead))
names(prod) = gsub(" ", "", names(prod))
names(geo) = gsub(" ", "", names(geo))

#cdata <- data.frame(api = input1, prodmonths = input2,  months = input3, gas = input4)

#input1 <- prod$UsApiUwi14
#input2 <- prod$ProducingMonth
#input3 <- prod$MONTHS
#input4 <- prod$GasProdMcf



df <- prod %>%
  group_by(UsApiUwi14) %>%
  arrange(UsApiUwi14, ProducingMonth) %>%
  mutate(maxMonth = which.max(GasProdMcf), 
         Time_De = MONTHS - maxMonth,
         maxGas = max(GasProdMcf, na.rm = TRUE),
         Decline_eff = 1 - (maxGas - GasProdMcf) / maxGas) %>%
  filter(Time_De >= 0) %>%
  select(UsApiUwi14, MONTHS, Time_De, maxGas, maxMonth, Decline_eff, GasProdMcf) 

#df <- cdata %>%
#  group_by(api) %>%
#  arrange(api, prodmonths) %>%
#  mutate(maxMonth = which.max(gas), 
#         Time_De = months - maxMonth,
#         maxGas = max(gas, na.rm = TRUE),
#         Decline_eff = 1 - (maxGas - gas) / maxGas)

output <- df$Decline_eff
         
         
df1 <- df %>% group_by(UsApiUwi14) %>% slice(which.max(GAS))
df2 <- df %>% group_by(API) %>% top_n(n=1)
df3 <- df %>% group_by(API) %>% filter(GAS == max(GAS))

ggplot(df, aes(x = Time_De, y = Decline_eff, color = API)) + 
  geom_line()




