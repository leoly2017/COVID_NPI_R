phaseEachCountry <- function(dataset) {
  monitored.Var <- c(
    paste("C", 1:8, sep = "")
  )
  L <- nrow(dataset)
  dataset$date_index <- as.numeric(dataset$date - as.Date("2019-12-01"))
  NPI.matrix <- dataset[2:L,monitored.Var] - dataset[2:L - 1, monitored.Var]
  NPI.matrix$date_index <- dataset$date_index[2:L]
  NPI.matrix$cut <- with(NPI.matrix, 
                         C1|C2|C3|C4|C5|C6|C7|C8)
  dNPI <- rbind(NPI.matrix[1,],
                NPI.matrix[NPI.matrix$cut,])[1:8]
  names(dNPI) <- paste("d.", names(dNPI), sep = "")
  date_index.cut <- c(min(NPI.matrix$date_index-1),NPI.matrix$date_index[NPI.matrix$cut],
                      max(NPI.matrix$date_index+1))
  dataset$phase <- cut(dataset$date_index, date_index.cut, right = FALSE,
                       labels = 1:(length(date_index.cut)-1))
  dNPI$phase <- as.factor(1:(length(date_index.cut)-1))
  res <- dataset %>% group_by(country, phase) %>%
    summarise(phase.duration = as.numeric(max(date) - min(date)+1),
              date = date[1], R = mean(median), 
              R.7 = mean(median[
                (length(median) - min(length(median)-1,6)):length(median)
                ]),
              test = mean(H2,na.rm=TRUE),
#              trace = mean(H3, na.rm =TRUE),
              trace.C = mean(C9, na.rm = TRUE),
              C1=C1[1], C2=C2[1], C3=C3[1], C4=C4[1], C5=C5[1], C6=C6[1],
              C7=C7[1], C8=C8[1])
  res <- left_join(res, dNPI)
  res$R.ratio <- 1
  res$R.ratio.7 <- 1
  res$R.diff <- 0
  res$test.flag <- FALSE
  res$trace.flag <- FALSE
  if(nrow(res)>1){
    res$R.ratio[2:nrow(res)] <- res$R[2:nrow(res)]/res$R[2:nrow(res)-1]
    res$R.ratio.7[2:nrow(res)] <- res$R.7[2:nrow(res)]/res$R.7[2:nrow(res)-1]
    res$R.diff[2:nrow(res)] <- res$R[2:nrow(res)] - res$R[2:nrow(res)-1]
    res$test.flag[2:nrow(res)] <- ((res$test[2:nrow(res)]>0.5)==(res$test[2:nrow(res)-1]>0.5)&
      !is.na(res$test))
    res$trace.flag[2:nrow(res)] <- ((res$trace.C[2:nrow(res)]==1)&(res$trace.C[2:nrow(res)-1]==1)&
                                      !is.na(res$trace.C))
  }
  return(res)
}
getFreqEachRow <- function(EachRow) {
  res1 <- expand.grid(A = paste("d.C", 1:8, sep = ""),
                     B = paste("d.C", 1:8, sep = ""))
  res2 <- expand.grid(A2 = unlist(EachRow[paste("d.C", 1:8, sep = "")]),
                      B2 = unlist(EachRow[paste("d.C", 1:8, sep = "")]))
  res <- cbind(res1,res2)
  res$ordered <- (res$A2==1) & (res$B2==1)
  res$lifted <- (res$A2==-1) & (res$B2==-1)
  return(res)
}
getOrderEachCountry <- function(CountryRow) {
  res1 <- gather(CountryRow[c("date", paste("d.C", 1:8, sep = ""))], 
                   key = "NPI", value = "IO", -date)
  res1 <-   res1 %>% group_by(NPI, IO) %>%
    dplyr::summarise(date = min(date))
  suppressWarnings(
      res1 <- left_join(expand.grid(NPI = paste("d.C", 1:8, sep = ""),
                                IO = c(1,-1)), res1)
  )

  res2 <- expand.grid(A = paste("d.C", 1:8, sep = ""),
                      B = paste("d.C", 1:8, sep = ""),
                      IO = c(1,-1))
  res3 <- rbind(
    expand.grid(dateA = res1$date[res1$IO==1],
                dateB = res1$date[res1$IO==1]),
    expand.grid(dateA = res1$date[res1$IO==-1],
                dateB = res1$date[res1$IO==-1])
  )
  res4 <- cbind(res2, res3)
  res4$B_is_early <- as.numeric(res4$dateA - res4$dateB)>0
  res4$counter <- 1
  return(res4[!is.na(res4$B_is_early), c("A", "B", "IO", "B_is_early", "counter")])
}
addline_format <- function(x,...){
  gsub('\\s','\n',x)
}
phaseEachCountry2 <- function(dataset) {
  monitored.Var <- c(
    paste("C", 1:8, sep = "")
  )
  L <- nrow(dataset)
  dataset$date_index <- as.numeric(dataset$date - as.Date("2019-12-01"))
  NPI.matrix <- dataset[2:L,monitored.Var] - dataset[2:L - 1, monitored.Var]
  NPI.matrix$date_index <- dataset$date_index[2:L]
  NPI.matrix$cut <- with(NPI.matrix, 
                         C1|C2|C3|C4|C5|C6|C7|C8)
  dNPI <- rbind(NPI.matrix[1,],
                NPI.matrix[NPI.matrix$cut,])[1:8]
  names(dNPI) <- paste("d.", names(dNPI), sep = "")
  date_index.cut <- c(min(NPI.matrix$date_index-1),NPI.matrix$date_index[NPI.matrix$cut],
                      max(NPI.matrix$date_index+1))
  dataset$phase <- as.numeric(cut(dataset$date_index, date_index.cut, right = FALSE,
                                  labels = 1:(length(date_index.cut)-1)))
  dNPI$phase <- 1:(length(date_index.cut)-1)
  dataset <- dataset[order(dataset$date_index, decreasing = FALSE),]
  dataset.summary <-   dataset[dataset$phase!=max(dataset$phase),] %>% group_by(phase) %>%
    dplyr::summarise(phase_last_date_index = date_index[length(date_index)],
                     phase_last_R = median[length(median)],
                     phase_last_R_7 = mean(median[(length(median)-min(length(median),7)+1):(length(median))]),
                     phase_last_duration = length(median),
                     phase_last_noNPI_flag = (C1 + C2 + C3+ C4 +C5+ C6+C7 +C8)[1]==0,
                     phase_last_H2 = H2[1],
                     phase_last_H3 = H3[1])
  dataset.summary$phase <- dataset.summary$phase+1
  dataset <- left_join(dataset, dataset.summary)
  dataset$Day_After_NPI <- dataset$date_index - dataset$phase_last_date_index
  dataset$R.ratio <- dataset$median/dataset$phase_last_R
  dataset$R.ratio_7 <- dataset$median/dataset$phase_last_R_7
  dataset <- left_join(dataset, dNPI)
  return(dataset)
}
phaseEachCountry3 <- function(dataset) {
  monitored.Var <- c(
    paste("C", 1:8, sep = "")
  )
  L <- nrow(dataset)
  dataset$date_index <- as.numeric(dataset$date - as.Date("2019-12-01"))
  NPI.matrix <- dataset[2:L,monitored.Var] - dataset[2:L - 1, monitored.Var]
  NPI.matrix$date_index <- dataset$date_index[2:L]
  NPI.matrix$cut <- with(NPI.matrix, 
                         C1|C2|C3|C4|C5|C6|C7|C8)
  dNPI <- rbind(NPI.matrix[1,],
                NPI.matrix[NPI.matrix$cut,])[1:8]
  names(dNPI) <- paste("d.", names(dNPI), sep = "")
  date_index.cut <- c(min(NPI.matrix$date_index-1),NPI.matrix$date_index[NPI.matrix$cut],
                      max(NPI.matrix$date_index+1))
  dataset$phase <- as.numeric(cut(dataset$date_index, date_index.cut, right = FALSE,
                                  labels = 1:(length(date_index.cut)-1)))
  dNPI$phase <- 1:(length(date_index.cut)-1)
  dataset <- dataset[order(dataset$date_index, decreasing = FALSE),]
  dataset.summary <-   dataset[dataset$phase!=max(dataset$phase),] %>% group_by(phase) %>%
    dplyr::summarise(phase_last_date_index = date_index[length(date_index)],
                     phase_last_RR = Retail_Recreation[length(median)],
                     phase_last_GP = Grocery_Pharmacy[length(median)],
                     phase_last_Parks = Parks[length(median)],
                     phase_last_TS = TransitStations[length(median)],
                     phase_last_WP = Workplace[length(median)],
                     phase_last_RD = Residential[length(median)],
                     phase_last_duration = length(median),
                     phase_last_noNPI_flag = (C1 + C2 + C3+ C4 +C5+ C6+C7 +C8)[1]==0,
                     phase_last_H2 = H2[1],
                     phase_last_H3 = H3[1])
  dataset.summary$phase <- dataset.summary$phase+1
  dataset <- left_join(dataset, dataset.summary)
  dataset$Day_After_NPI <- dataset$date_index - dataset$phase_last_date_index
  dataset$RR <- dataset$Retail_Recreation/dataset$phase_last_RR
  dataset$GP <- dataset$Grocery_Pharmacy/dataset$phase_last_GP
  dataset$PK <- dataset$Parks/dataset$phase_last_Parks
  dataset$TS <- dataset$TransitStations/dataset$phase_last_TS
  dataset$WP <- dataset$Workplace/dataset$phase_last_WP
  dataset$RD <- dataset$Residential/dataset$phase_last_RD
  dataset <- left_join(dataset, dNPI)
  return(dataset)
}
getMainAnalysis <- function(dataByDay) {
  fit <-   lm(data = dataByDay, 
              formula = log(R.ratio) ~as.numeric(d.C1==1) + as.numeric(d.C2==1) + 
                as.numeric(d.C3==1) +as.numeric(d.C4==1) + as.numeric(d.C5==1) + 
                as.numeric(d.C6==1) + as.numeric(d.C7==1) + as.numeric(d.C8==1) +
                as.numeric(D_order_NPI>1) +
                as.numeric(d.C1==-1) + as.numeric(d.C2==-1) + as.numeric(d.C3==-1) +
                as.numeric(d.C4==-1) + as.numeric(d.C5==-1) + as.numeric(d.C6==-1) + 
                as.numeric(d.C7==-1) +as.numeric(d.C8==-1)  + 
                as.numeric(D_lift_NPI>1))
  res <- expand.grid(
    NPI = c(NPI.labels, "Interaction"),
    IO = c("Introduced", "Lifted")
  )
  res$alias <- names(summary(fit)$aliased)[-1]
  res$Day_After_NPI <- dataByDay$Day_After_NPI[1]
  summary.df <- as.data.frame(summary(fit)[["coefficients"]])
  summary.df$alias <- row.names(summary.df)
  res <- left_join(res, summary.df)
  res$est0 <- res$Estimate
  res$se0 <- res$`Std. Error`
  res$est <- exp(res$est0)
  res$lci <- exp(res$est0 - 1.96 *res$se0)
  res$uci <- exp(res$est0 + 1.96 *res$se0)
  return(res)
}
getMainAnalysis2 <- function(dataByDay) {
  dataByDay$D_order_NPI <- ifelse(dataByDay$D_order_NPI>1, dataByDay$D_order_NPI-1,0)
  dataByDay$D_lift_NPI <- ifelse(dataByDay$D_lift_NPI>1, dataByDay$D_lift_NPI-1,0)
  fit <-   lm(data = dataByDay, 
              formula = log(R.ratio) ~as.numeric(d.C1==1) + as.numeric(d.C2==1) + 
                as.numeric(d.C3==1) +as.numeric(d.C4==1) + as.numeric(d.C5==1) + 
                as.numeric(d.C6==1) + as.numeric(d.C7==1) + as.numeric(d.C8==1) + D_order_NPI +
                as.numeric(d.C1==-1) + as.numeric(d.C2==-1) + as.numeric(d.C3==-1) +
                as.numeric(d.C4==-1) + as.numeric(d.C5==-1) + as.numeric(d.C6==-1) + 
                as.numeric(d.C7==-1) +as.numeric(d.C8==-1) + D_lift_NPI)
  res <- expand.grid(
    NPI = c(NPI.labels, "Interaction"),
    IO = c("Introduced", "Lifted")
  )
  res$alias <- names(summary(fit)$aliased)[-1]
  res$Day_After_NPI <- dataByDay$Day_After_NPI[1]
  summary.df <- as.data.frame(summary(fit)[["coefficients"]])
  summary.df$alias <- row.names(summary.df)
  res <- left_join(res, summary.df)
  res$est0 <- res$Estimate
  res$se0 <- res$`Std. Error`
  res$est <- exp(res$est0)
  res$lci <- exp(res$est0 - 1.96 *res$se0)
  res$uci <- exp(res$est0 + 1.96 *res$se0)
  return(res)
}
getSingleNPI <- function(dataByDay){
  fit <-   lm(data = dataByDay, 
              formula = log(R.ratio) ~0+ as.numeric(d.C1==1) + as.numeric(d.C2==1) + 
                as.numeric(d.C3==1) +as.numeric(d.C4==1) + as.numeric(d.C5==1) + 
                as.numeric(d.C6==1) + as.numeric(d.C7==1) + as.numeric(d.C8==1) +
                as.numeric(d.C1==-1) + as.numeric(d.C2==-1) + as.numeric(d.C3==-1) +
                as.numeric(d.C4==-1) + as.numeric(d.C5==-1) + as.numeric(d.C6==-1) + 
                as.numeric(d.C7==-1) +as.numeric(d.C8==-1))
  res <- expand.grid(
    NPI = c(NPI.labels),
    IO = c("Introduced", "Lifted")
  )
  res$alias <- names(summary(fit)$aliased)
  res$Day_After_NPI <- dataByDay$Day_After_NPI[1]
  summary.df <- as.data.frame(summary(fit)[["coefficients"]])
  summary.df$alias <- row.names(summary.df)
  res <- left_join(res, summary.df)
  res$est0 <- res$Estimate
  res$se0 <- res$`Std. Error`
  res$est <- exp(res$est0)
  res$lci <- exp(res$est0 - 1.96 *res$se0)
  res$uci <- exp(res$est0 + 1.96 *res$se0)
  return(res)
}
getGatherSizeAnalysis <- function(){
  C4_10 <- do.call(rbind,
                      by(phase.data.C4_10[phase.data.C4_10$Day_After_NPI<=Range_Of_Day,], 
                         phase.data.C4_10[phase.data.C4_10$Day_After_NPI<=Range_Of_Day,]$Day_After_NPI,
                         getMainAnalysis))
  C4_10 <- C4_10[C4_10$NPI==NPI.labels[4],]
  C4_10$NPI <- "Ban on gathering size of >10"
  C4_100 <- do.call(rbind,
                   by(phase.data.C4_100[phase.data.C4_100$Day_After_NPI<=Range_Of_Day,], 
                      phase.data.C4_100[phase.data.C4_100$Day_After_NPI<=Range_Of_Day,]$Day_After_NPI,
                      getMainAnalysis))
  C4_100 <- C4_100[C4_100$NPI==NPI.labels[4],]
  C4_100$NPI <- "Ban on gathering size of >100"
  return(rbind(C4_10, C4_100))
}
getCombinedNPIAnalysis <- function(dataByDay){
  fit <-   lm(data = dataByDay, 
              formula = log(R.ratio) ~as.numeric(d.C1==1) + as.numeric(d.C2==1) + 
                as.numeric(d.C3==1) +as.numeric(d.C4==1) + as.numeric(d.C5==1) + 
                as.numeric(d.C6==1) + as.numeric(d.C7==1) + as.numeric(d.C8==1) +
                as.numeric(D_order_NPI>1) +
                as.numeric(d.C1==-1) + as.numeric(d.C2==-1) + as.numeric(d.C3==-1) +
                as.numeric(d.C4==-1) + as.numeric(d.C5==-1) + as.numeric(d.C6==-1) + 
                as.numeric(d.C7==-1) +as.numeric(d.C8==-1)  + 
                as.numeric(D_lift_NPI>1))
  input.frame <- data.frame(
    NPI = factor(
      c("Public events & gathering (>10) ban", 
        "Workplace closure,\nand public events & gathering (>10) ban",
        "Workplace closure,\npublic events & gathering (>10) ban,\nand internal movement limits",
        "School & workplace closure, public events & gathering (>10) ban,\ninternal movement limits,\nand stay at home requirement"
      ),
      levels = c("Public events & gathering (>10) ban", 
                 "Workplace closure,\nand public events & gathering (>10) ban",
                 "Workplace closure,\npublic events & gathering (>10) ban,\nand internal movement limits",
                 "School & workplace closure, public events & gathering (>10) ban,\ninternal movement limits,\nand stay at home requirement"
      )),
    d.C1 = c(0,0,0,1), d.C2 = c(0,1,1,1), d.C3 = c(1,1,1,1), d.C4 = c(1,1,1,1),
    d.C5 = c(0,0,0,0), d.C6 = c(0,0,0,1), d.C7 = c(0,0,1,1), d.C8 = c(0,0,0,0),
    D_order_NPI = c(2,3,4,6), D_lift_NPI = c(0,0,0,0)
  )
  res <- data.frame(input.frame,
                    Day_After_NPI = dataByDay$Day_After_NPI[1],
                    est0 = predict(fit, input.frame, se = TRUE)$fit,
                    se0 = predict(fit, input.frame, se = TRUE)$se
                    )
  res$est <- exp(res$est0)
  res$lci <- exp(res$est0 - 1.96*res$se0)
  res$uci <- exp(res$est0 + 1.96*res$se0)
  return(res)
}
getDaysToPeak <- function(percentage, eachNPI_IO) {
  res <- data.frame(NPI = eachNPI_IO$NPI[1],
                    IO = eachNPI_IO$IO[1],
                    percentage = percentage)
  if(res$IO=="Introduced"){
    res$max_est <- 1-min(eachNPI_IO$est)
    if(res$max_est >=0) {
      res$days <- min(eachNPI_IO$Day_After_NPI[eachNPI_IO$est <=1 - res$max_est * percentage/100] )
    }else{
      res$days <- NA
    }
  }else{
    res$max_est <- max(eachNPI_IO$est - 1)
    if(res$max_est >=0) {
    res$days <- min(eachNPI_IO$Day_After_NPI[eachNPI_IO$est >=res$max_est * percentage/100 + 1])
    }else{
      res$days <- NA
    }
  }
  return(res)
}
getLast7Analysis <- function(dataByDay) {
  fit <-   lm(data = dataByDay, 
              formula = log(R.ratio_7) ~as.numeric(d.C1==1) + as.numeric(d.C2==1) + 
                as.numeric(d.C3==1) +as.numeric(d.C4==1) + as.numeric(d.C5==1) + 
                as.numeric(d.C6==1) + as.numeric(d.C7==1) + as.numeric(d.C8==1) +
                as.numeric(D_order_NPI>1) +
                as.numeric(d.C1==-1) + as.numeric(d.C2==-1) + as.numeric(d.C3==-1) +
                as.numeric(d.C4==-1) + as.numeric(d.C5==-1) + as.numeric(d.C6==-1) + 
                as.numeric(d.C7==-1) +as.numeric(d.C8==-1)  + 
                as.numeric(D_lift_NPI>1))
  res <- expand.grid(
    NPI = c(NPI.labels, "Interaction"),
    IO = c("Introduced", "Lifted")
  )
  res$alias <- names(summary(fit)$aliased)[-1]
  res$Day_After_NPI <- dataByDay$Day_After_NPI[1]
  summary.df <- as.data.frame(summary(fit)[["coefficients"]])
  summary.df$alias <- row.names(summary.df)
  res <- left_join(res, summary.df)
  res$est0 <- res$Estimate
  res$se0 <- res$`Std. Error`
  res$est <- exp(res$est0)
  res$lci <- exp(res$est0 - 1.96 *res$se0)
  res$uci <- exp(res$est0 + 1.96 *res$se0)
  return(res)
}
getGoogle_RR <- function(dataByDay) {
  fit <-   lm(data = dataByDay, 
              formula = log(RR) ~as.numeric(d.C1==1) + as.numeric(d.C2==1) + 
                as.numeric(d.C3==1) +as.numeric(d.C4==1) + as.numeric(d.C5==1) + 
                as.numeric(d.C6==1) + as.numeric(d.C7==1) + as.numeric(d.C8==1) +
                as.numeric(D_order_NPI>1) +
                as.numeric(d.C1==-1) + as.numeric(d.C2==-1) + as.numeric(d.C3==-1) +
                as.numeric(d.C4==-1) + as.numeric(d.C5==-1) + as.numeric(d.C6==-1) + 
                as.numeric(d.C7==-1) +as.numeric(d.C8==-1)  + 
                as.numeric(D_lift_NPI>1))
  res <- expand.grid(
    NPI = c(NPI.labels, "Interaction"),
    IO = c("Introduced", "Lifted")
  )
  res$alias <- names(summary(fit)$aliased)[-1]
  res$Day_After_NPI <- dataByDay$Day_After_NPI[1]
  summary.df <- as.data.frame(summary(fit)[["coefficients"]])
  summary.df$alias <- row.names(summary.df)
  res <- left_join(res, summary.df)
  res$est0 <- res$Estimate
  res$se0 <- res$`Std. Error`
  res$est <- exp(res$est0)
  res$lci <- exp(res$est0 - 1.96 *res$se0)
  res$uci <- exp(res$est0 + 1.96 *res$se0)
  return(res)
}
getGoogle_GP <- function(dataByDay) {
  fit <-   lm(data = dataByDay, 
              formula = log(GP) ~as.numeric(d.C1==1) + as.numeric(d.C2==1) + 
                as.numeric(d.C3==1) +as.numeric(d.C4==1) + as.numeric(d.C5==1) + 
                as.numeric(d.C6==1) + as.numeric(d.C7==1) + as.numeric(d.C8==1) +
                as.numeric(D_order_NPI>1) +
                as.numeric(d.C1==-1) + as.numeric(d.C2==-1) + as.numeric(d.C3==-1) +
                as.numeric(d.C4==-1) + as.numeric(d.C5==-1) + as.numeric(d.C6==-1) + 
                as.numeric(d.C7==-1) +as.numeric(d.C8==-1)  + 
                as.numeric(D_lift_NPI>1))
  res <- expand.grid(
    NPI = c(NPI.labels, "Interaction"),
    IO = c("Introduced", "Lifted")
  )
  res$alias <- names(summary(fit)$aliased)[-1]
  res$Day_After_NPI <- dataByDay$Day_After_NPI[1]
  summary.df <- as.data.frame(summary(fit)[["coefficients"]])
  summary.df$alias <- row.names(summary.df)
  res <- left_join(res, summary.df)
  res$est0 <- res$Estimate
  res$se0 <- res$`Std. Error`
  res$est <- exp(res$est0)
  res$lci <- exp(res$est0 - 1.96 *res$se0)
  res$uci <- exp(res$est0 + 1.96 *res$se0)
  return(res)
}
getGoogle_PK <- function(dataByDay) {
  fit <-   lm(data = dataByDay, 
              formula = log(PK) ~as.numeric(d.C1==1) + as.numeric(d.C2==1) + 
                as.numeric(d.C3==1) +as.numeric(d.C4==1) + as.numeric(d.C5==1) + 
                as.numeric(d.C6==1) + as.numeric(d.C7==1) + as.numeric(d.C8==1) +
                as.numeric(D_order_NPI>1) +
                as.numeric(d.C1==-1) + as.numeric(d.C2==-1) + as.numeric(d.C3==-1) +
                as.numeric(d.C4==-1) + as.numeric(d.C5==-1) + as.numeric(d.C6==-1) + 
                as.numeric(d.C7==-1) +as.numeric(d.C8==-1)  + 
                as.numeric(D_lift_NPI>1))
  res <- expand.grid(
    NPI = c(NPI.labels, "Interaction"),
    IO = c("Introduced", "Lifted")
  )
  res$alias <- names(summary(fit)$aliased)[-1]
  res$Day_After_NPI <- dataByDay$Day_After_NPI[1]
  summary.df <- as.data.frame(summary(fit)[["coefficients"]])
  summary.df$alias <- row.names(summary.df)
  res <- left_join(res, summary.df)
  res$est0 <- res$Estimate
  res$se0 <- res$`Std. Error`
  res$est <- exp(res$est0)
  res$lci <- exp(res$est0 - 1.96 *res$se0)
  res$uci <- exp(res$est0 + 1.96 *res$se0)
  return(res)
}
getGoogle_WP <- function(dataByDay) {
  fit <-   lm(data = dataByDay, 
              formula = log(WP) ~as.numeric(d.C1==1) + as.numeric(d.C2==1) + 
                as.numeric(d.C3==1) +as.numeric(d.C4==1) + as.numeric(d.C5==1) + 
                as.numeric(d.C6==1) + as.numeric(d.C7==1) + as.numeric(d.C8==1) +
                as.numeric(D_order_NPI>1) +
                as.numeric(d.C1==-1) + as.numeric(d.C2==-1) + as.numeric(d.C3==-1) +
                as.numeric(d.C4==-1) + as.numeric(d.C5==-1) + as.numeric(d.C6==-1) + 
                as.numeric(d.C7==-1) +as.numeric(d.C8==-1)  + 
                as.numeric(D_lift_NPI>1))
  res <- expand.grid(
    NPI = c(NPI.labels, "Interaction"),
    IO = c("Introduced", "Lifted")
  )
  res$alias <- names(summary(fit)$aliased)[-1]
  res$Day_After_NPI <- dataByDay$Day_After_NPI[1]
  summary.df <- as.data.frame(summary(fit)[["coefficients"]])
  summary.df$alias <- row.names(summary.df)
  res <- left_join(res, summary.df)
  res$est0 <- res$Estimate
  res$se0 <- res$`Std. Error`
  res$est <- exp(res$est0)
  res$lci <- exp(res$est0 - 1.96 *res$se0)
  res$uci <- exp(res$est0 + 1.96 *res$se0)
  return(res)
}
getGoogle_RD <- function(dataByDay) {
  fit <-   lm(data = dataByDay, 
              formula = log(RD) ~as.numeric(d.C1==1) + as.numeric(d.C2==1) + 
                as.numeric(d.C3==1) +as.numeric(d.C4==1) + as.numeric(d.C5==1) + 
                as.numeric(d.C6==1) + as.numeric(d.C7==1) + as.numeric(d.C8==1) +
                as.numeric(D_order_NPI>1) +
                as.numeric(d.C1==-1) + as.numeric(d.C2==-1) + as.numeric(d.C3==-1) +
                as.numeric(d.C4==-1) + as.numeric(d.C5==-1) + as.numeric(d.C6==-1) + 
                as.numeric(d.C7==-1) +as.numeric(d.C8==-1)  + 
                as.numeric(D_lift_NPI>1))
  res <- expand.grid(
    NPI = c(NPI.labels, "Interaction"),
    IO = c("Introduced", "Lifted")
  )
  res$alias <- names(summary(fit)$aliased)[-1]
  res$Day_After_NPI <- dataByDay$Day_After_NPI[1]
  summary.df <- as.data.frame(summary(fit)[["coefficients"]])
  summary.df$alias <- row.names(summary.df)
  res <- left_join(res, summary.df)
  res$est0 <- res$Estimate
  res$se0 <- res$`Std. Error`
  res$est <- exp(res$est0)
  res$lci <- exp(res$est0 - 1.96 *res$se0)
  res$uci <- exp(res$est0 + 1.96 *res$se0)
  return(res)
}
getGoogle_TS <- function(dataByDay) {
  fit <-   lm(data = dataByDay, 
              formula = log(TS) ~as.numeric(d.C1==1) + as.numeric(d.C2==1) + 
                as.numeric(d.C3==1) +as.numeric(d.C4==1) + as.numeric(d.C5==1) + 
                as.numeric(d.C6==1) + as.numeric(d.C7==1) + as.numeric(d.C8==1) +
                as.numeric(D_order_NPI>1) +
                as.numeric(d.C1==-1) + as.numeric(d.C2==-1) + as.numeric(d.C3==-1) +
                as.numeric(d.C4==-1) + as.numeric(d.C5==-1) + as.numeric(d.C6==-1) + 
                as.numeric(d.C7==-1) +as.numeric(d.C8==-1)  + 
                as.numeric(D_lift_NPI>1))
  res <- expand.grid(
    NPI = c(NPI.labels, "Interaction"),
    IO = c("Introduced", "Lifted")
  )
  res$alias <- names(summary(fit)$aliased)[-1]
  res$Day_After_NPI <- dataByDay$Day_After_NPI[1]
  summary.df <- as.data.frame(summary(fit)[["coefficients"]])
  summary.df$alias <- row.names(summary.df)
  res <- left_join(res, summary.df)
  res$est0 <- res$Estimate
  res$se0 <- res$`Std. Error`
  res$est <- exp(res$est0)
  res$lci <- exp(res$est0 - 1.96 *res$se0)
  res$uci <- exp(res$est0 + 1.96 *res$se0)
  return(res)
}
getDaysToPeak_RD <- function(percentage, eachNPI_IO) {
  res <- data.frame(NPI = eachNPI_IO$NPI[1],
                    IO = eachNPI_IO$IO[1],
                    percentage = percentage)
  if(res$IO=="Lifted"){
    res$max_est <- 1-min(eachNPI_IO$est)
    if(res$max_est >=0) {
      res$days <- min(eachNPI_IO$Day_After_NPI[eachNPI_IO$est <=1 - res$max_est * percentage/100] )
    }else{
      res$days <- NA
    }
  }else{
    res$max_est <- max(eachNPI_IO$est - 1)
    if(res$max_est >=0) {
      res$days <- min(eachNPI_IO$Day_After_NPI[eachNPI_IO$est >=res$max_est * percentage/100 + 1])
    }else{
      res$days <- NA
    }
  }
  return(res)
}
getDaysToPeak_WP <- function(percentage, eachNPI_IO) {
  res <- data.frame(NPI = eachNPI_IO$NPI[1],
                    IO = eachNPI_IO$IO[1],
                    percentage = percentage)
  if(res$IO=="Introduced"){
    res$max_est <- 1-min(eachNPI_IO$est)
    if(res$max_est >=0) {
      res$days <- min(eachNPI_IO$Day_After_NPI[eachNPI_IO$est <=1 - res$max_est * percentage/100] )
    }else{
      res$days <- NA
    }
  }else{
    res$max_est <- max(eachNPI_IO$est - 1)
    if(res$max_est >=0) {
      res$days <- min(eachNPI_IO$Day_After_NPI[eachNPI_IO$est >=res$max_est * percentage/100 + 1])
    }else{
      res$days <- NA
    }
  }
  return(res)
}
get10 <- function(country_to_exclude) {
  return(
    do.call(rbind,
            by(phase.data2[phase.data2$Day_After_NPI<=Range_Of_Day & !phase.data2$country %in% country_to_exclude,], 
               phase.data2[phase.data2$Day_After_NPI<=Range_Of_Day & !phase.data2$country %in% country_to_exclude,]$Day_After_NPI,
               getMainAnalysis))
  )
}