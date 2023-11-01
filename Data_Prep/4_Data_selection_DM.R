
library(tidyverse)

CITESFull_ER <- data.table::fread("Data/3_CITES_Vert_dat_ER.csv") %>% select(-V1)
CITESFull_IR <- data.table::fread("Data/3_CITES_Vert_dat_IR.csv") %>% select(-V1)

## Focus on just the reptile and bird trade (but include wild and captive/live and not live)
## ER
Reptile_CITES_Data <- CITESFull_ER %>% filter(Class == "Reptilia") 

write.csv(Reptile_CITES_Data, "Data/4_DM_Rept_ER.csv", na = "")


## IR
Reptile_CITES_Data_IR <- CITESFull_IR %>% filter(Class == "Reptilia") 

write.csv(Reptile_CITES_Data_IR, "Data/4_DM_Rept_IR.csv", na = "")
