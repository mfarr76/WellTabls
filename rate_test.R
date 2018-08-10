
library(dplyr, warn.conflicts = FALSE)

##input parameters
wellheader_join

df <- wellheader_join %>%
  filter(c.Spacing_Category != "Unbounded" &
           ProdZoneName == "EAGLE FORD" & !is.na(FirstMonthGas)) %>%
  select(API, OperatorName, WellName, c.BtmHoleLAT, c.BtmHoleLON, PerfIntervalGross, c.Lbs_Ft, 
         c.Bbl_Ft, c.Spacing_Avg, FirstMonthGas, FirstMonthLiquid, First3MonthGas, First3MonthLiquid, 
         SIDE_1_API, SIDE_2_API) %>%
  mutate(IPGas_SIDE_1 = FirstMonthGas[(match(API, SIDE_1_API))],
         IPLiquid_SIDE_1 = FirstMonthLiquid[(match(API, SIDE_1_API))], 
         IPGas_SIDE_2 = FirstMonthGas[(match(API, SIDE_2_API))], 
         IPLiquid_SIDE_2 = FirstMonthLiquid[(match(API, SIDE_2_API))], 
         Gas3Mo_SIDE_1 = First3MonthGas[(match(API, SIDE_1_API))], 
         Liquid3Mo_SIDE_1 = First3MonthLiquid[(match(API, SIDE_1_API))], 
         Gas3Mo_SIDE_2 = First3MonthGas[(match(API, SIDE_2_API))], 
         Liquid3Mo_SIDE_2 = First3MonthLiquid[(match(API, SIDE_2_API))], 
         FirstMonthBOE = FirstMonthGas / 6 + FirstMonthLiquid, 
         BOE_3Month = FirstMonthGas / 6 + First3MonthLiquid) %>%
  rowwise() %>%
  mutate(IPBOE_Offset = ifelse(!is.na(IPGas_SIDE_1) | !is.na(IPGas_SIDE_2),
                               max(IPGas_SIDE_1, IPGas_SIDE_2, na.rm = TRUE), 0) / 6 +
           ifelse(!is.na(IPLiquid_SIDE_1) | !is.na(IPLiquid_SIDE_2),
                  max(IPLiquid_SIDE_1, IPLiquid_SIDE_2, na.rm = TRUE), 0),
         BOE_3Month_Offset = ifelse(!is.na(Gas3Mo_SIDE_1) | !is.na(Gas3Mo_SIDE_2),
                                    max(Gas3Mo_SIDE_1, Gas3Mo_SIDE_2, na.rm = TRUE), 0) / 6 +
           ifelse(!is.na(Liquid3Mo_SIDE_1) | !is.na(Liquid3Mo_SIDE_2),
                  max(Liquid3Mo_SIDE_1, Liquid3Mo_SIDE_2, na.rm = TRUE), 0), 
         IPBOE_Parent_Norm = FirstMonthBOE / PerfIntervalGross * 1000, 
         IPBOE_Offset_Norm = IPBOE_Offset / PerfIntervalGross * 1000, 
         BOE_3Month_Parent_Norm = BOE_3Month / PerfIntervalGross * 1000, 
         BOE_3Month_Offset_Norm = BOE_3Month_Offset / PerfIntervalGross * 1000)