source("core.R")
library(ggsci)
#1. Each country---- 
genFigureEachCountry <- function(country) {
  ggsave(
    plot.res <- ggplot(data = phase.ns.data.long[phase.ns.data.long$country==country,],
           aes(x = date, y = NPI)) +
      geom_segment(aes(x = min(date), xend = max(date),y =1, yend=1), 
                   linetype = "dashed", color = "red4")+
      geom_tile(aes(fill = IO),alpha = 0.1, color = "grey90")+
      scale_fill_manual(name = "NPIs", values = c("grey50", "red2"), labels = c("Off", "On"))+
      geom_line(data = unique(phase.ns.data.long[phase.ns.data.long$country==country,
                                                 c("country", "date", "median", "lower_90", "upper_90")]),
                aes(x = date, y = median))+
      geom_line(data = unique(phase.ns.data.long[phase.ns.data.long$country==country,
                                                 c("country", "date", "median", "lower_90", "upper_90")]),
                aes(x = date, y = lower_90), color = "blue")+
      geom_line(data = unique(phase.ns.data.long[phase.ns.data.long$country==country,
                                                 c("country", "date", "median", "lower_90", "upper_90")]),
                aes(x = date, y = upper_90), color = "blue")+
      scale_y_continuous(name = "R", limits = c(0,3), breaks = (0:6)*0.5,
                         sec.axis = sec_axis(~ .*1, breaks = (2:9)*0.25, labels = NPI.labels)) +
      scale_x_date(date_breaks = "2 weeks", date_labels = "%e-%b") +
      theme_bw() +
      theme(legend.position = c(0.85, 0.9), legend.direction="horizontal") +
      labs(title = country),
    filename = paste("figures/byCountry/", country, ".pdf", sep = ""),
    width = 10, height = 3.5
  )
  return(country = plot.res)
}
country.plot <- lapply(unique(phase.ns.data.long$country),  genFigureEachCountry)
#2. Freq----
ggsave(
  freq_ordered <- ggplot(data = NPI.freq)+
    geom_tile(aes(x = A, y = B, fill = ordered)) +
    geom_text(aes(x = A, y = B, label = ordered)) +
    scale_fill_gradient2(low = "white", high = "dodgerblue3", limit = c(0, max(NPI.freq$ordered))) +
    scale_x_discrete(label = NPI.labels) +
    scale_y_discrete(label = NPI.labels) +
    theme_bw() +
    theme(legend.position = "none",
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1),
          axis.text = element_text(size = 12)),
  filename = "figures/NPI_freq_1.pdf", width = 6, height = 6
)
ggsave(
  freq_lifted <- ggplot(data = NPI.freq)+
    geom_tile(aes(x = A, y = B, fill = lifted)) +
    geom_text(aes(x = A, y = B, label = lifted)) +
    scale_fill_gradient2(low = "white", high = "red2", limit = c(0, max(NPI.freq$lifted))) +
    scale_x_discrete(label = NPI.labels) +
    scale_y_discrete(label = NPI.labels) +
    theme_bw() +
    theme(legend.position = "none",
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1),
          axis.text = element_text(size = 12)),
  filename = "figures/NPI_freq_-1.pdf", width = 6, height = 6
)
#3. Duration----
ggsave(
  ggplot(data = NPI.duration[NPI.duration$n.phase>2,], aes(x = NPI, y = phase.duration, 
                                  color = factor(IO,levels = c("1", "-1"))))+
    geom_boxplot()+
    geom_text(aes(x = NPI, y = -2, label = n.phase), position = position_dodge(width = 1),
              show.legend = FALSE)+
    scale_color_lancet(name = NULL, labels = c("Introduced", "Lifted"))+
    scale_y_continuous(name = "Duration in days",limits = c(-2, max(NPI.duration$phase.duration))) +
    scale_x_discrete(labels = NPI.labels)+
    theme_bw() +
    theme(axis.text.x = element_text(angle = 25, vjust = 1, hjust=1),
          text = element_text(size = 15)),
  filename = "figures/NPI_duration.pdf", width = 10, height = 5
)
#4. Order----
ggsave(
  freq_ordered <- ggplot(data = NPI.order[NPI.order$IO==1,])+
    geom_tile(aes(x = A, y = B, fill = B_early_prop)) +
    geom_text(aes(x = A, y = B, label = B_early_prop)) +
    scale_fill_gradient2(low = "white", high = "dodgerblue3", 
                         limit = c(0, max(NPI.order[NPI.order$IO==1,]$B_early_prop))) +
    scale_x_discrete(label = NPI.labels) +
    scale_y_discrete(label = NPI.labels) +
    theme_bw() +
    theme(legend.position = "none",
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1),
          axis.text = element_text(size = 12)),
  filename = "figures/NPI_order_1.pdf", width = 6, height = 6
)
ggsave(
  freq_ordered <- ggplot(data = NPI.order[NPI.order$IO==-1,])+
    geom_tile(aes(x = A, y = B, fill = B_early_prop)) +
    geom_text(aes(x = A, y = B, label = B_early_prop)) +
    scale_fill_gradient2(low = "white", high = "red2", 
                         limit = c(0, max(NPI.order[NPI.order$IO==-1,]$B_early_prop))) +
    scale_x_discrete(label = NPI.labels) +
    scale_y_discrete(label = NPI.labels) +
    theme_bw() +
    theme(legend.position = "none",
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1),
          axis.text = element_text(size = 12)),
  filename = "figures/NPI_order_-1.pdf", width = 6, height = 6
)
#5. NPI and R analysis----
#5.1 Main analysis----
ggsave(
  ggplot(data = main.data[main.data$NPI %in% NPI.labels,], aes(x = Day_After_NPI, y = est, group = IO, color = IO)) +
    geom_hline(yintercept = 1, linetype = "dashed")+
    geom_point(position = position_dodge2(width = 0.5)) +
    geom_errorbar(aes(ymin = lci, ymax = uci),position = position_dodge2(), width = 0.5) +
    scale_color_lancet(name = NULL) +
    scale_y_continuous(name = "R ratio", breaks = seq(0,4,0.2)) +
    scale_x_continuous(name = "Day(s) since introducing/lifting NPIs",limits = c(0,29), breaks = seq(1,28,2),
                       expand = c(0,0))+
    facet_wrap(~NPI, ncol = 2)+
    theme_bw()+
    theme(text = element_text(size = 12),legend.position="bottom"),
  filename = "figures/main.pdf", width = 12, height = 8
)
Table_1_data <- main.data[main.data$Day_After_NPI %in% c(7,14,28),]
Table_1_data$R.ratio <- paste(round(Table_1_data$est,2), " (", 
                              round(Table_1_data$lci,2), "-",
                              round(Table_1_data$uci,2), ")",
                              sep = "")
Table_1_data <- spread(Table_1_data[c("NPI", "IO", "Day_After_NPI", "R.ratio")],
                       Day_After_NPI, R.ratio)
write.csv(Table_1_data, file = "res_table.csv",
          row.names = FALSE)
ggsave(
  ggplot(data = main.data[!main.data$NPI %in% NPI.labels,], aes(x = Day_After_NPI, y = est, group = IO, color = IO)) +
    geom_hline(yintercept = 1, linetype = "dashed")+
    geom_point(position = position_dodge2(width = 0.5)) +
    geom_errorbar(aes(ymin = lci, ymax = uci),position = position_dodge2(), width = 0.5) +
    scale_color_lancet(name = NULL) +
    scale_y_continuous(name = "R ratio", breaks = seq(0,4,0.2)) +
    scale_x_continuous(name = "Day(s) since introducing/lifting NPIs",limits = c(0,29), breaks = seq(1,28,2),
                       expand = c(0,0))+
    theme_bw()+
    theme(text = element_text(size = 10),legend.position="bottom"),
  filename = "figures/interaction.pdf", width = 6, height = 2.5
)
#5.2 Only include single NPI----
ggsave(
  ggplot(data = sens1.data, aes(x = Day_After_NPI, y = est, group = IO, color = IO)) +
    geom_hline(yintercept = 1, linetype = "dashed")+
    geom_point(position = position_dodge2(width = 0.5)) +
    geom_errorbar(aes(ymin = lci, ymax = uci),position = position_dodge2(), width = 0.5) +
    scale_color_lancet(name = NULL) +
    scale_y_continuous(name = "R ratio", breaks = seq(0,4,0.2)) +
    scale_x_continuous(name = "Day(s) since introducing/lifting NPIs",limits = c(0,29), breaks = seq(1,28,2),
                       expand = c(0,0))+
    facet_wrap(~NPI, ncol = 2)+
    theme_bw()+
    theme(text = element_text(size = 12),legend.position="bottom"),
  filename = "figures/singleNPI.pdf", width = 12, height = 8
)
#5.3 Excluding a few countries----
ggsave(
  ggplot(data = sens2.data[sens2.data$NPI %in% NPI.labels,], aes(x = Day_After_NPI, y = est, group = IO, color = IO)) +
    geom_hline(yintercept = 1, linetype = "dashed")+
    geom_point(position = position_dodge2(width = 0.5)) +
    geom_errorbar(aes(ymin = lci, ymax = uci),position = position_dodge2(), width = 0.5) +
    scale_color_lancet(name = NULL) +
    scale_y_continuous(name = "R ratio", breaks = seq(0,4,0.2)) +
    scale_x_continuous(name = "Day(s) since introducing/lifting NPIs",limits = c(0,29), breaks = seq(1,28,2),
                       expand = c(0,0))+
    facet_wrap(~NPI, ncol = 2)+
    theme_bw()+
    theme(text = element_text(size = 12),legend.position="bottom"),
  filename = "figures/countriesExcluded.pdf", width = 12, height = 8
)
#5.4 gathering size----
ggsave(
  ggplot(data = sens3.data, aes(x = Day_After_NPI, y = est, group = IO, color = IO)) +
    geom_hline(yintercept = 1, linetype = "dashed")+
    geom_point(position = position_dodge2(width = 0.5)) +
    geom_errorbar(aes(ymin = lci, ymax = uci),position = position_dodge2(), width = 0.5) +
    scale_color_lancet(name = NULL) +
    scale_y_continuous(name = "R ratio", breaks = seq(0,4,0.2)) +
    scale_x_continuous(name = "Day(s) since introducing/lifting NPIs",limits = c(0,29), breaks = seq(1,28,2),
                       expand = c(0,0))+
    facet_wrap(~NPI, ncol = 1)+
    theme_bw()+
    theme(text = element_text(size = 12),legend.position="bottom"),
  filename = "figures/gatheringSize.pdf", width = 6, height = 4.5
)
#5.5 with comprehensive tests only----
ggsave(
  ggplot(data = sens4.data[sens4.data$NPI %in% NPI.labels,], aes(x = Day_After_NPI, y = est, group = IO, color = IO)) +
    geom_hline(yintercept = 1, linetype = "dashed")+
    geom_point(position = position_dodge2(width = 0.5)) +
    geom_errorbar(aes(ymin = lci, ymax = uci),position = position_dodge2(), width = 0.5) +
    scale_color_lancet(name = NULL) +
    scale_y_continuous(name = "R ratio", breaks = seq(0,4,0.4)) +
    scale_x_continuous(name = "Day(s) since introducing/lifting NPIs",limits = c(0,29), breaks = seq(1,28,2),
                       expand = c(0,0))+
    facet_wrap(~NPI, ncol = 2)+
    theme_bw()+
    theme(text = element_text(size = 12),legend.position="bottom"),
  filename = "figures/test.pdf", width = 12, height = 8
)
#5.6 with contact tracing only----
ggsave(
  ggplot(data = sens5.data[sens5.data$NPI %in% NPI.labels,], aes(x = Day_After_NPI, y = est, group = IO, color = IO)) +
    geom_hline(yintercept = 1, linetype = "dashed")+
    geom_point(position = position_dodge2(width = 0.5)) +
    geom_errorbar(aes(ymin = lci, ymax = uci),position = position_dodge2(), width = 0.5) +
    scale_color_lancet(name = NULL) +
    scale_y_continuous(name = "R ratio", breaks = seq(0,4,0.4)) +
    scale_x_continuous(name = "Day(s) since introducing/lifting NPIs",limits = c(0,29), breaks = seq(1,28,2),
                       expand = c(0,0))+
    facet_wrap(~NPI, ncol = 2)+
    theme_bw()+
    theme(text = element_text(size = 12),legend.position="bottom"),
  filename = "figures/contactTracing.pdf", width = 12, height = 8
)
#5.7 combined effects----
ggsave(
  ggplot(data = sens6.data, aes(x = Day_After_NPI, y = est)) +
    geom_hline(yintercept = 1, linetype = "dashed")+
    geom_point(color = pal_lancet()(1)) +
    geom_errorbar(aes(ymin = lci, ymax = uci),width = 0.5,color = pal_lancet()(1)) +
    scale_y_continuous(name = "R ratio", breaks = seq(0,4,0.2)) +
    scale_x_continuous(name = "Day(s) since introducing NPIs",limits = c(0,29), breaks = seq(1,28,2),
                       expand = c(0,0))+
    facet_wrap(~NPI, ncol = 1)+
    theme_bw()+
    theme(text = element_text(size = 12),legend.position="bottom"),
  filename = "figures/combinedEffects.pdf", width = 6, height = 8
)
Table_2_data <- sens6.data[sens6.data$Day_After_NPI %in% c(7,14,28),]
Table_2_data$R.ratio <- paste(round(Table_2_data$est,2), " (", 
                              round(Table_2_data$lci,2), "-",
                              round(Table_2_data$uci,2), ")",
                              sep = "")
Table_2_data <- spread(Table_2_data[c("NPI", "Day_After_NPI", "R.ratio")],
                                       Day_After_NPI, R.ratio)
write.csv(Table_2_data, file = "res_table_2.csv",
          row.names = FALSE)
# 5.8 days taken to reach maximum effect during the first 28 days----
sens8.data$percentage.display <- ifelse(sens8.data$percentage%%10==0, sens8.data$percentage, NA)
ggsave(
  ggplot(data = sens8.data[sens8.data$NPI %in% NPI.labels[c(1:4,6,7)],], 
         aes(x = percentage, y = days, group = IO, color = IO)) +
    geom_line() +
    scale_color_lancet(name =NULL) +
    scale_y_continuous(name = "Days needed", breaks = seq(4,28,4)) +
    scale_x_continuous(name = "Percentage of maximum effect", breaks = seq(0,100,10)) +
    geom_text(aes(x = percentage.display,label = days, y = days+1.5))+
    facet_wrap(~NPI, ncol = 2)+
    theme_bw()+
    theme(text = element_text(size = 12),legend.position="bottom"),
  filename = "figures/daysToEffect.pdf", width = 8, height = 6
)
ggsave(
  ggplot(data = sens8.data_sub[sens8.data_sub$max_est > 0.05,], 
         aes(x = percentage, y = days, group = IO, color = IO)) +
    geom_line() +
    scale_color_lancet(name =NULL) +
    scale_y_continuous(name = "Days needed", breaks = seq(4,28,4)) +
    scale_x_continuous(name = "Percentage of maximum effect", breaks = seq(0,100,10)) +
    facet_wrap(~NPI, ncol = 1)+
    theme_bw()+
    theme(text = element_text(size = 12),legend.position="bottom"),
  filename = "figures/daysToEffect_sub.pdf", width = 4.5, height = 4.5
)
# 5.9 excluding first NPI----
ggsave(
  ggplot(data = sens10.data[sens10.data$NPI %in% NPI.labels,], aes(x = Day_After_NPI, y = est, group = IO, color = IO)) +
    geom_hline(yintercept = 1, linetype = "dashed")+
    geom_point(position = position_dodge2(width = 0.5)) +
    geom_errorbar(aes(ymin = lci, ymax = uci),position = position_dodge2(), width = 0.5) +
    scale_color_lancet(name = NULL) +
    scale_y_continuous(name = "R ratio", breaks = seq(0,4,0.2)) +
    scale_x_continuous(name = "Day(s) since introducing/lifting NPIs",limits = c(0,29), breaks = seq(1,28,2),
                       expand = c(0,0))+
    facet_wrap(~NPI, ncol = 2)+
    theme_bw()+
    theme(text = element_text(size = 12),legend.position="bottom"),
  filename = "figures/excluding1stNPI.pdf", width = 12, height = 8
)
# 5.10 using last 7 days of pervious NPI----
ggsave(
  ggplot(data = sens11.data[sens11.data$NPI %in% NPI.labels,], aes(x = Day_After_NPI, y = est, group = IO, color = IO)) +
    geom_hline(yintercept = 1, linetype = "dashed")+
    geom_point(position = position_dodge2(width = 0.5)) +
    geom_errorbar(aes(ymin = lci, ymax = uci),position = position_dodge2(), width = 0.5) +
    scale_color_lancet(name = NULL) +
    scale_y_continuous(name = "R ratio", breaks = seq(0,4,0.2)) +
    scale_x_continuous(name = "Day(s) since introducing/lifting NPIs",limits = c(0,29), breaks = seq(1,28,2),
                       expand = c(0,0))+
    facet_wrap(~NPI, ncol = 2)+
    theme_bw()+
    theme(text = element_text(size = 12),legend.position="bottom"),
  filename = "figures/last7R.pdf", width = 12, height = 8
)
#5.11 exclude 10----
ggsave(
  ggplot(data = sens12.data[sens12.data$NPI %in% NPI.labels,], aes(x = Day_After_NPI, y = est, group = IO, color = IO)) +
    geom_hline(yintercept = 1, linetype = "dashed")+
    geom_point(position = position_dodge2(width = 0.5), alpha = 0.1) +
    geom_errorbar(aes(ymin = lci, ymax = uci),position = position_dodge2(), width = 0.5, alpha = 0.1) +
    scale_color_lancet(name = NULL) +
    scale_y_continuous(name = "R ratio", breaks = seq(0,4,0.2)) +
    scale_x_continuous(name = "Day(s) since introducing/lifting NPIs",limits = c(0,29), breaks = seq(1,28,2),
                       expand = c(0,0))+
    facet_wrap(~NPI, ncol = 2)+
    theme_bw()+
    theme(text = element_text(size = 12),legend.position="bottom"),
  filename = "figures/exclude10.pdf", width = 12, height = 8
)
# 5.12 Google workplace and stay at home----
# 5.12.1 Workplace----
ggsave(
  ggplot(data = google_WP.data[google_WP.data$NPI %in% NPI.labels[2],], aes(x = Day_After_NPI, y = est, group = IO, color = IO)) +
    geom_hline(yintercept = 1, linetype = "dashed")+
    geom_point(position = position_dodge2(width = 0.5)) +
    geom_errorbar(aes(ymin = lci, ymax = uci),position = position_dodge2(), width = 0.5) +
    scale_color_lancet(name = NULL) +
    scale_y_continuous(name = "Ratio of visits", breaks = seq(0,4,0.2)) +
    scale_x_continuous(name = "Day(s) since introducing/lifting NPIs",limits = c(0,29), breaks = seq(1,28,2),
                       expand = c(0,0))+
    theme_bw()+
    theme(text = element_text(size = 8),legend.position="bottom"),
  filename = "figures/google/WP_WP.pdf", width = 4, height = 2
)
ggsave(
  ggplot(data = google_days_WP.data[google_days_WP.data$NPI %in% NPI.labels[2],], 
         aes(x = percentage, y = days, group = IO, color = IO)) +
    geom_line() +
    scale_color_lancet(name =NULL) +
    scale_y_continuous(name = "Days needed", breaks = seq(4,28,4)) +
    scale_x_continuous(name = "Percentage of maximum effect", breaks = seq(0,100,10)) +
    theme_bw()+
    theme(text = element_text(size = 8),legend.position="bottom"),
  filename = "figures/google/WP_days_WP.pdf", width = 4, height = 2
)
# 5.12.2 Stay at home----
ggsave(
  ggplot(data = google_RD.data[google_RD.data$NPI %in% NPI.labels[6],], aes(x = Day_After_NPI, y = est, group = IO, color = IO)) +
    geom_hline(yintercept = 1, linetype = "dashed")+
    geom_point(position = position_dodge2(width = 0.5)) +
    geom_errorbar(aes(ymin = lci, ymax = uci),position = position_dodge2(), width = 0.5) +
    scale_color_lancet(name = NULL) +
    scale_y_continuous(name = "Ratio of staying time", breaks = seq(0,4,0.05)) +
    scale_x_continuous(name = "Day(s) since introducing/lifting NPIs",limits = c(0,29), breaks = seq(1,28,2),
                       expand = c(0,0))+
    theme_bw()+
    theme(text = element_text(size = 8),legend.position="bottom"),
  filename = "figures/google/RD_RD.pdf", width = 4, height = 2
)
ggsave(
  ggplot(data = google_days_RD.data[google_days_RD.data$NPI %in% NPI.labels[6],], 
         aes(x = percentage, y = days, group = IO, color = IO)) +
    geom_line() +
    scale_color_lancet(name =NULL) +
    scale_y_continuous(name = "Days needed", breaks = seq(4,28,4)) +
    scale_x_continuous(name = "Percentage of maximum effect", breaks = seq(0,100,10)) +
    theme_bw()+
    theme(text = element_text(size = 8),legend.position="bottom"),
  filename = "figures/google/RD_days_RD.pdf", width = 4, height = 2
)
