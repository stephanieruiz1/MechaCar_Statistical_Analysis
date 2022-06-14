#part 1
library(dplyr)
MechaCar<-read.csv(file ='MechaCar_mpg.csv',check.names = F,stringsAsFactors=F)
Mecha_lm <- lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data = MechaCar)
summary(Mecha_lm)
#part2
Suspension<-read.csv(file = 'Suspension_coil.csv',check.names = F,stringsAsFactors = F)
total_summary <- Suspension %>% summarize(Mean=mean(PSI),Median=median(PSI),Variance=var(PSI),SD=sd(PSI))
lot_summary <-Suspension %>% group_by(Manufacturing_Lot)%>% summarize(Mean=mean(PSI),Median=median(PSI),Variance=var(PSI),SD=sd(PSI))
#part3
# test all lots
t.test(Suspension$PSI,mu=1500)

t.test(subset(Suspension,Manufacturing_Lot=="Lot1")$PSI,mu=1500)

t.test(subset(Suspension,Manufacturing_Lot=="Lot2")$PSI,mu=1500)

t.test(subset(Suspension,Manufacturing_Lot=="Lot3")$PSI,mu=1500)