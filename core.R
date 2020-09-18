rm(list=ls())
suppressMessages({
  library(ggplot2)
library(dplyr)
library(tidyr)
  library(lme4)
  library(lmerTest)
  library(scales)
})
source("functions.R")
NPI.labels <- c(
  "School closure", "Workplace closure", "Public events ban", "Ban on gathering size of >10",
  "Public transport closure", "Stay at home requirement", "Internal movement limits", 
  "International travel limits"
)
#1. Loading dataset----
load(file = "2020-08-25.RData")
load(file = "Google.RData")
#2. Further cleaning----
ns.data <- combined[!(is.na(combined$C1_School.closing)|is.na(combined$C2_Workplace.closing)|
      is.na(combined$C3_Cancel.public.events)|is.na(combined$C4_Restrictions.on.gatherings)|
      is.na(combined$C5_Close.public.transport)|is.na(combined$C6_Stay.at.home.requirements)|
      is.na(combined$C7_Restrictions.on.internal.movement)|
      is.na(combined$C8_International.travel.controls)),] #Remove NAs
ns.data$C1 <- ns.data$C1_School.closing>1
ns.data$C2 <- ns.data$C2_Workplace.closing>1
ns.data$C3 <- ns.data$C3_Cancel.public.events>1
ns.data$C4 <- ns.data$C4_Restrictions.on.gatherings>3
ns.data$C5 <- ns.data$C5_Close.public.transport>1
ns.data$C6 <- ns.data$C6_Stay.at.home.requirements>1
ns.data$C7 <- ns.data$C7_Restrictions.on.internal.movement>1
ns.data$C8 <- ns.data$C8_International.travel.controls>1
ns.data$H2 <- ns.data$H2_Testing.policy>1
ns.data$H3 <- ns.data$H3_Contact.tracing>1
ns.data$C9 <- ns.data$H3_Contact.tracing>1
# Add Google
ns.data <- left_join(ns.data, Google)
# Prepare for sensitivity analyses
ns.data.C4_10 <- ns.data
ns.data.C4_100 <- ns.data
ns.data.C4_100$C4 <- ns.data.C4_100$C4_Restrictions.on.gatherings>2
#3. Generating phases----
suppressWarnings(
  {
    phase.data <- do.call(rbind,
                          by(data = ns.data, ns.data$country,
                             phaseEachCountry))
    phase.data2 <- do.call(rbind,
                           by(data = ns.data, ns.data$country,
                              phaseEachCountry2))
    phase.data.C4_10 <- do.call(rbind,
                            by(data = ns.data.C4_10, ns.data$country,
                               phaseEachCountry2))
    phase.data.C4_100 <- do.call(rbind,
                                by(data = ns.data.C4_100, ns.data$country,
                                   phaseEachCountry2))
    phase.data3 <- do.call(rbind,
                           by(data = ns.data, ns.data$country,
                              phaseEachCountry3))
  }
)
listOfCountries <- unique(phase.data[phase.data$phase==2,]$country)
write.csv(combined[combined$country %in% listOfCountries,] %>% group_by(country) %>%
            dplyr::summarise(start_date = min(date),
                             end_date = max(date)),
          file = "country_table.csv", row.names = FALSE)
phase.data <- phase.data[phase.data$country %in% listOfCountries,]
phase.data$singleNPIFlag <- with(
  phase.data,
    (d.C1!=0) + (d.C2!=0) + (d.C3!=0) + (d.C4!=0) + (d.C5!=0) + (d.C6!=0) +
      (d.C7!=0) + (d.C8!=0)
  )
phase.data$D_order_NPI_Flag <- with(
  phase.data,
  ((d.C1==1) + (d.C2==1) + (d.C3==1) + (d.C4==1) + (d.C5==1) + (d.C6==1) +
         (d.C7==1) + (d.C8==1))>1
)
phase.data$D_lift_NPI_Flag <- with(
  phase.data,
  ((d.C1==-1) + (d.C2==-1) + (d.C3==-1) + (d.C4==-1) + (d.C5==-1) + (d.C6==-1) +
     (d.C7==-1) + (d.C8==-1))>1
)
phase.data2$singleNPIFlag <- with(
  phase.data2,
  (d.C1!=0) + (d.C2!=0) + (d.C3!=0) + (d.C4!=0) + (d.C5!=0) + (d.C6!=0) +
    (d.C7!=0) + (d.C8!=0)
)
phase.data2$D_order_NPI <- with(
  phase.data2,
  ((d.C1==1) + (d.C2==1) + (d.C3==1) + (d.C4==1) + (d.C5==1) + (d.C6==1) +
     (d.C7==1) + (d.C8==1))
)
phase.data2$D_lift_NPI <- with(
  phase.data2,
  ((d.C1==-1) + (d.C2==-1) + (d.C3==-1) + (d.C4==-1) + (d.C5==-1) + (d.C6==-1) +
     (d.C7==-1) + (d.C8==-1))
)
phase.data3$singleNPIFlag <- with(
  phase.data3,
  (d.C1!=0) + (d.C2!=0) + (d.C3!=0) + (d.C4!=0) + (d.C5!=0) + (d.C6!=0) +
    (d.C7!=0) + (d.C8!=0)
)
phase.data3$D_order_NPI <- with(
  phase.data3,
  ((d.C1==1) + (d.C2==1) + (d.C3==1) + (d.C4==1) + (d.C5==1) + (d.C6==1) +
     (d.C7==1) + (d.C8==1))
)
phase.data3$D_lift_NPI <- with(
  phase.data3,
  ((d.C1==-1) + (d.C2==-1) + (d.C3==-1) + (d.C4==-1) + (d.C5==-1) + (d.C6==-1) +
     (d.C7==-1) + (d.C8==-1))
)
phase.data.C4_10$singleNPIFlag <- with(
  phase.data.C4_10,
  (d.C1!=0) + (d.C2!=0) + (d.C3!=0) + (d.C4!=0) + (d.C5!=0) + (d.C6!=0) +
    (d.C7!=0) + (d.C8!=0)
)
phase.data.C4_10$D_order_NPI <- with(
  phase.data.C4_10,
  ((d.C1==1) + (d.C2==1) + (d.C3==1) + (d.C4==1) + (d.C5==1) + (d.C6==1) +
     (d.C7==1) + (d.C8==1))
)
phase.data.C4_10$D_lift_NPI <- with(
  phase.data.C4_10,
  ((d.C1==-1) + (d.C2==-1) + (d.C3==-1) + (d.C4==-1) + (d.C5==-1) + (d.C6==-1) +
     (d.C7==-1) + (d.C8==-1))
)
phase.data.C4_100$singleNPIFlag <- with(
  phase.data.C4_100,
  (d.C1!=0) + (d.C2!=0) + (d.C3!=0) + (d.C4!=0) + (d.C5!=0) + (d.C6!=0) +
    (d.C7!=0) + (d.C8!=0)
)
phase.data.C4_100$D_order_NPI <- with(
  phase.data.C4_100,
  ((d.C1==1) + (d.C2==1) + (d.C3==1) + (d.C4==1) + (d.C5==1) + (d.C6==1) +
     (d.C7==1) + (d.C8==1))
)
phase.data.C4_100$D_lift_NPI <- with(
  phase.data.C4_100,
  ((d.C1==-1) + (d.C2==-1) + (d.C3==-1) + (d.C4==-1) + (d.C5==-1) + (d.C6==-1) +
     (d.C7==-1) + (d.C8==-1))
)
#4. Combine phase and data----
phase.ns.data <- left_join(ns.data[c("country", "date", "median", "lower_90", "upper_90",
                                     paste("C", 1:8, sep = ""))],
                           phase.data[c("country", "date", "phase")
                                       ])
phase.ns.data.long <- gather(phase.ns.data, key = "NPI", value = "IO", 
                            C1,C2, C3,C4,C5,C6,C7,C8)
phase.ns.data.long$NPI <- as.numeric(factor(phase.ns.data.long$NPI))*0.25 +0.25
phase.ns.data.long <- phase.ns.data.long[phase.ns.data.long$country %in% listOfCountries,]
#5. Analysis----
#5.0 Freq, duration and order----
NPI.freq <- do.call(rbind,
                    by(phase.data[phase.data$phase!="1",], 
                       phase.data[phase.data$phase!="1",c("country", "phase")],
                       getFreqEachRow)) %>% group_by(A,B) %>%
  dplyr::summarise(ordered = sum(ordered),
                   lifted = sum(lifted))
NPI.freq.summary <- phase.data[phase.data$phase!="1",] %>% group_by(d.C1, d.C2, d.C3, d.C4,
                                                                    d.C5, d.C6, d.C7, d.C8) %>%
  dplyr::summarise(freq = length(R.ratio))
with(phase.data, c(P50 = median(phase.duration),
            P25 = quantile(phase.duration,0.25),
            P75 = quantile(phase.duration,0.75)))
NPI.duration <- gather(phase.data[c("phase.duration",paste("d.C", 1:8, sep = ""))],
                       key = "NPI", value = "IO", -phase.duration)
NPI.duration <- NPI.duration[NPI.duration$IO!=0,]
NPI.duration <- left_join(NPI.duration,
                          NPI.duration %>% group_by(NPI, IO) %>%
                            dplyr::summarise(n.phase = length(phase.duration)))
NPI.duration %>% group_by(NPI, IO) %>%
  dplyr::summarise(P50 = median(phase.duration),
                   P25 = quantile(phase.duration,0.25),
                   P75 = quantile(phase.duration,0.75))
NPI.order <- do.call(rbind,
                     by(phase.data, phase.data$country, getOrderEachCountry))
NPI.order <-NPI.order %>% group_by(IO, A, B) %>%
  dplyr::summarise(B_early_freq = sum(B_is_early),
                   B_early_prop = round(B_early_freq/sum(counter)*100,0))
NPI.order <- left_join(expand.grid(A = paste("d.C", 1:8, sep = ""),
                                   B = paste("d.C", 1:8, sep = ""),
                                   IO = c(1,-1)),
                       NPI.order)
NPI.order$B_early_prop[is.na(NPI.order$B_early_prop)] <- 0
NPI.order.ranking <- NPI.order %>% group_by(IO, B) %>%
  dplyr::summarise(average.prop=sum(B_early_prop)/7)
NPI.order.ranking$ranking <- 0
NPI.order.ranking$ranking[NPI.order.ranking$IO==1] <- 
  9-rank(NPI.order.ranking[NPI.order.ranking$IO==1,]$average.prop, ties.method = "max")
NPI.order.ranking$ranking[NPI.order.ranking$IO==-1] <- 
  9-rank(NPI.order.ranking[NPI.order.ranking$IO==-1,]$average.prop, ties.method = "max")
# Range of day is 1-28.
Range_Of_Day <- 28
# 5.1 Main analysis----
phase.data2 <- phase.data2[phase.data2$phase!=1,]
phase.data3 <- phase.data3[phase.data3$phase!=1,]
main.data <- do.call(rbind,
                     by(phase.data2[phase.data2$Day_After_NPI<=Range_Of_Day,], 
                        phase.data2[phase.data2$Day_After_NPI<=Range_Of_Day,]$Day_After_NPI,
                        getMainAnalysis))
# 5.2 Single NPI----
sens1.data <- do.call(rbind,
                      by(phase.data2[phase.data2$Day_After_NPI<=Range_Of_Day &
                                       phase.data2$singleNPIFlag==1,], 
                         phase.data2[phase.data2$Day_After_NPI<=Range_Of_Day&
                                       phase.data2$singleNPIFlag==1,]$Day_After_NPI,
                         getSingleNPI))
sens1.1.data <- phase.data2[phase.data2$Day_After_NPI<=Range_Of_Day &
                              phase.data2$singleNPIFlag==1,]
sens1.1.data <- gather(sens1.1.data, key = "NPI", value = "IO", d.C1, d.C2, d.C3, d.C4, d.C5, d.C6, d.C7, d.C8)
sens1.1.data <- sens1.1.data[sens1.1.data$IO!=0,]
sens1.1.data$IO <- factor(sens1.1.data$IO, levels = c(1,-1), labels = c("Introduced", "Lifted"))
sens1.1.data$NPI <- factor(sens1.1.data$NPI, labels = NPI.labels)
sens1.1.data <- left_join(sens1.1.data,
                          sens1.1.data %>% group_by(NPI, IO, Day_After_NPI) %>%
                            dplyr::summarise(N.data = length(R.ratio)))
# 5.3 Excluding a few countries----
countriesToExclude <- c("Brazil", "Canada", "China", "India", "Russia", "United States of America")
sens2.data <- do.call(rbind,
                      by(phase.data2[phase.data2$Day_After_NPI<=Range_Of_Day &
                                       !phase.data2$country %in% countriesToExclude,], 
                         phase.data2[phase.data2$Day_After_NPI<=Range_Of_Day &
                                       !phase.data2$country %in% countriesToExclude,]$Day_After_NPI,
                         getMainAnalysis))
# 5.4 Different ban on gathering size----
sens3.data <- getGatherSizeAnalysis()
# 5.5 with comprehensive tests only----
sens4.data <- do.call(rbind,
                      by(phase.data2[phase.data2$Day_After_NPI<=Range_Of_Day &
                                       phase.data2$H2 & phase.data2$phase_last_H2,], 
                         phase.data2[phase.data2$Day_After_NPI<=Range_Of_Day&
                                       phase.data2$H2 & phase.data2$phase_last_H2,]$Day_After_NPI,
                         getMainAnalysis))
# 5.6 with contact tracing only----
sens5.data <- do.call(rbind,
                      by(phase.data2[phase.data2$Day_After_NPI<=Range_Of_Day &
                                       phase.data2$H3& phase.data2$phase_last_H3,], 
                         phase.data2[phase.data2$Day_After_NPI<=Range_Of_Day&
                                       phase.data2$H3& phase.data2$phase_last_H3,]$Day_After_NPI,
                         getMainAnalysis))
# 5.7 Combined effects----
sens6.data <- do.call(rbind,
                      by(phase.data2[phase.data2$Day_After_NPI<=Range_Of_Day,], 
                                      phase.data2[phase.data2$Day_After_NPI<=Range_Of_Day,]$Day_After_NPI,
                                      getCombinedNPIAnalysis))
# 5.8 (deprecated)----
# 5.9 days taken to reach maximum effect during the first 28 days----
sens8.data <- do.call(rbind,
                      mapply(FUN = function(percentage){
                        as.data.frame(
                          do.call(rbind,by(main.data, main.data[c("NPI", "IO")], getDaysToPeak, percentage = percentage))
                          )}
                        , 1:100, SIMPLIFY = FALSE
                        )
                      )
sens8.data_sub <- do.call(rbind,
                          mapply(FUN = function(percentage){
                            as.data.frame(
                              do.call(rbind,by(sens3.data, sens3.data[c("NPI", "IO")], getDaysToPeak, percentage = percentage))
                            )}
                            , 1:100, SIMPLIFY = FALSE
                          )
)
# 5.10 exclude NPIs that were the first to be introduced----
sens10.data <- do.call(rbind,
                       by(phase.data2[phase.data2$Day_After_NPI<=Range_Of_Day &
                                        phase.data2$phase!=2,], 
                          phase.data2[phase.data2$Day_After_NPI<=Range_Of_Day&
                                        phase.data2$phase!=2,]$Day_After_NPI,
                          getMainAnalysis))
# 5.11 Use the average R in the last 7 days of the previous phase rather than the last day----
sens11.data <- do.call(rbind,
                       by(phase.data2[phase.data2$Day_After_NPI<=Range_Of_Day,], 
                          phase.data2[phase.data2$Day_After_NPI<=Range_Of_Day,]$Day_After_NPI,
                          getLast7Analysis))
# 5.12 Take out 10% of data, 10 times----
set.seed(12346)
randomList <- replicate(20,sample(listOfCountries,10,replace = FALSE))
sens12.data <- do.call(rbind, apply(randomList, 2, get10))
# 5.13 Google mobility----
google_RR.data <- do.call(rbind,
                          by(phase.data3[phase.data3$Day_After_NPI<=Range_Of_Day,], 
                             phase.data3[phase.data3$Day_After_NPI<=Range_Of_Day,]$Day_After_NPI,
                             getGoogle_RR))
google_GP.data <- do.call(rbind,
                          by(phase.data3[phase.data3$Day_After_NPI<=Range_Of_Day,], 
                             phase.data3[phase.data3$Day_After_NPI<=Range_Of_Day,]$Day_After_NPI,
                             getGoogle_GP))
google_PK.data <- do.call(rbind,
                          by(phase.data3[phase.data3$Day_After_NPI<=Range_Of_Day,], 
                             phase.data3[phase.data3$Day_After_NPI<=Range_Of_Day,]$Day_After_NPI,
                             getGoogle_PK))
google_RD.data <- do.call(rbind,
                          by(phase.data3[phase.data3$Day_After_NPI<=Range_Of_Day,], 
                             phase.data3[phase.data3$Day_After_NPI<=Range_Of_Day,]$Day_After_NPI,
                             getGoogle_RD))
google_TS.data <- do.call(rbind,
                          by(phase.data3[phase.data3$Day_After_NPI<=Range_Of_Day,], 
                             phase.data3[phase.data3$Day_After_NPI<=Range_Of_Day,]$Day_After_NPI,
                             getGoogle_TS))
google_WP.data <- do.call(rbind,
                          by(phase.data3[phase.data3$Day_After_NPI<=Range_Of_Day,], 
                             phase.data3[phase.data3$Day_After_NPI<=Range_Of_Day,]$Day_After_NPI,
                             getGoogle_WP))
google_days_RD.data <- do.call(rbind, mapply(FUN = function(percentage){
                               as.data.frame(
                                 do.call(rbind,by(google_RD.data, main.data[c("NPI", "IO")], 
                                                  getDaysToPeak_RD, percentage = percentage))
                               )}, 1:100, SIMPLIFY = FALSE)
)
google_days_WP.data <- do.call(rbind, mapply(FUN = function(percentage){
  as.data.frame(
    do.call(rbind,by(google_WP.data, main.data[c("NPI", "IO")], 
                     getDaysToPeak_WP, percentage = percentage))
  )}, 1:100, SIMPLIFY = FALSE)
)
