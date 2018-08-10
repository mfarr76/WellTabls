rm(list = ls())


load("C:/Users/mfarr/Documents/R_files/Spotfire.data/wrangler_tbls.RData")
load("C:/Users/mfarr/Documents/R_files/Spotfire.data/mov_avg.RData")

wellhead <- read.csv("wellhead.csv", stringsAsFactors = FALSE)


isNamespaceLoaded <- function(name) is.element(name, loadedNamespaces())
library(zoo)
library(xts)
library(dplyr)
library(dygraphs)

##inputs
#wellhead
#method


names(wellhead) = gsub(" ", "", names(wellhead))

wellhead <- wellhead %>% filter(RsPlay == "EAGLE FORD",
                            !is.na(ProppantIntensityLbsPerFt),
                            ProppantIntensityLbsPerFt > 100,
                            FirstProdDate > "2008/01/01", 
                            TotalFluidPumpedBblPerLateralLengthFt > 1) %>%
  mutate(FirstProdDate = as.POSIXct(FirstProdDate, tz = "UTC")) %>%
  group_by(FirstProdDate) %>%
  mutate(lbs_ft_p10 = quantile(ProppantIntensityLbsPerFt, p = 0.90, na.rm = TRUE), 
         lbs_ft_p90 = quantile(ProppantIntensityLbsPerFt, p = 0.10, na.rm = TRUE), 
         lbs_ft_p50 = median(ProppantIntensityLbsPerFt)) %>%
  filter(RsOperator == "SM ENERGY") %>%
  mutate(sm_lbs_ft_p50 = median(ProppantIntensityLbsPerFt)) %>%
  select(FirstProdDate, lbs_ft_p90, lbs_ft_p50, sm_lbs_ft_p50, lbs_ft_p10)

data=xts(x = wellhead[,-1], order.by = wellhead$FirstProdDate)

# Plot
dygraph(data, main = "LBS/FT") %>%
  dySeries(c("lbs_ft_p90", "lbs_ft_p50", "lbs_ft_p10"), color = "red") %>%
  #dySeries("lbs_ft_p10", color = "red") %>%
  #dySeries("lbs_ft_p90", color = "grey") %>%
  dySeries("sm_lbs_ft_p50", color = "blue") %>%
  #dyOptions(stackedGraph = TRUE) %>%
  dyLegend(show = "always")

dygraph(data, main = "LBS/FT") %>%
  dySeries("lbs_ft_p50", color = "red") %>%
  dyLegend(show = "always")


sapply(wellhead, function(x) sum(is.na(x)))

names(wellhead) = gsub(" ", "", names(wellhead))
options(stringsAsFactors = FALSE)
#method="Monthly"
wellhead[,"MovingAvg"]<-NA
#wellhead[,"FirstProdDate"]<-as.Date(wellhead[,"FirstProdDate"],"%m/%d/%Y")




lbsft_out <- xts(wellhead$ProppantIntensityLbsPerFt, wellhead$FirstProdDate)
bblft_out <- xts(wellhead$TotalFluidPumpedBblPerLateralLengthFt, wellhead$FirstProdDate)
efflat_out <- xts(wellhead$PerfIntervalFt, wellhead$FirstProdDate)

lbsftAvg <-as.data.frame(apply.monthly(lbsft_out,mean))
bblftavg <-as.data.frame(apply.monthly(bblft_out,mean))
efflatavg <-as.data.frame(apply.monthly(efflat_out,mean))
datedf=as.POSIXct(rownames(lbsftAvg),tz = "UTC","%Y-%m-%d")
MovingAvgdf <- data.frame(Date = datedf,"lbsft_MovingAvg" = lbsftAvg[,1], 
                          "bblft_MovingAvg" = bblftavg[,1], 
                          "efflat_MovingAvg" = efflatavg[,1])

sm <- wellhead %>%
  filter(RsOperator == "SM ENERGY")


smlbsft_out <- xts(sm$ProppantIntensityLbsPerFt, sm$FirstProdDate)
smbblft_out <- xts(sm$TotalFluidPumpedBblPerLateralLengthFt, sm$FirstProdDate)
smefflat_out <- xts(sm$PerfIntervalFt, sm$FirstProdDate)



smlbsftAvg <-as.data.frame(apply.monthly(smlbsft_out,mean))
smbblftavg <-as.data.frame(apply.monthly(smbblft_out,mean))
smefflatavg <-as.data.frame(apply.monthly(smefflat_out,mean))
smdatedf=as.POSIXct(rownames(smlbsftAvg),tz = "UTC","%Y-%m-%d")
MovingAvgsm <- data.frame(Date = smdatedf,"sm_lbsft_MovingAvg" = smlbsftAvg[,1], 
                          "sm_bblft_MovingAvg" = smbblftavg[,1], 
                          "sm_efflat_MovingAvg" = smefflatavg[,1])






