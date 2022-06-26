library(dplyr)
library(tidyverse)

Mecha_df <- read.csv('MechaCar_mpg.csv', check.names = F, stringsAsFactors = F)

lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data = Mecha_df)

summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data = Mecha_df))

Coil_df <- read.csv('Suspension_Coil.csv', check.names = F, stringsAsFactors = F)

total_summary <- Coil_df %>% summarize(Mean=mean(PSI), Median=median(PSI), Variance=var(PSI), SD=sd(PSI))

lot_summary <- Coil_df %>% group_by(Manufacturing_Lot) %>% summarize(Mean=mean(PSI), Median=median(PSI), Variance=var(PSI), SD=sd(PSI))

t.test(Coil_df$PSI, mu=1500)

Lot1_df <- Coil_df %>% subset(Manufacturing_Lot == "Lot1")

Lot2_df <- Coil_df %>% subset(Manufacturing_Lot == "Lot2")

Lot3_df <- Coil_df %>% subset(Manufacturing_Lot == "Lot3")

t.test(Lot1_df$PSI, mu=1500)

t.test(Lot2_df$PSI, mu=1500)

t.test(Lot3_df$PSI, mu=1500)
