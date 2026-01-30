  # Required packages to run script. If not on computer download and install.
  library(ggplot2)
  library(lubridate)
  library(reshape2)
  library(data.table)
  library(dplyr)
  library(gridExtra)
  library(zoo)
  library(plotly)
  library(gridExtra)
  library(data.table)
  library(slider)
  library(tidyverse)
  
  
  importCSdata <- function(filename,RetOpt="data"){
    if(RetOpt=="info"){
      # bring in entire header of CSI TOA5 data file for metadata
      stn.info <- scan(file=filename,nlines=4,what=character(),sep="\r")
      return(stn.info)
    } else {
      # second line of header contains variable names
      header <- scan(file=filename,skip=1,nlines=1,what=character(),sep=",")
      # bring in data
      stn.data <- read.table(file=filename,skip=4,header=FALSE, na.strings=c("NAN"),sep=",")
      names(stn.data) <- header
      # add column of R-formatted date/timestamps
      stn.data$TIMESTAMP <- as.POSIXct(strptime(stn.data$TIMESTAMP,format="%m/%d/%Y %H:%M", tz="us/pacific"))
      stn.data$ID<-basename(filename)
      return(stn.data)}}
  
  #set working directory
  #setwd("C:/Program Files/RStudio/Task 5 Microwarming/OLDdata")
  setwd("C:/LBL Data Workfile/Data/Tilden/Data/jackiegraphs")
  
  # import all .dat files within folder
  filenames=list.files(path=getwd(),pattern="[.]csv", recursive=T)
  
  # bring in the files in a list using the Campbell function
  list <- lapply(filenames, importCSdata)
  
  #Combine all files in the list into a data frame. Select fill=T to merge files with different column numbers
  data <-rbindlist(list, fill = TRUE)
  
  #convert to data table
  data <- data.table(data)
  
  # Remove duplicates from data.
  data <- data[!duplicated(data),]
  
  #Order file based on timestamp
  data<-data[order(TIMESTAMP),]
  
  
  #Process GMF temp data
  #Select columns: timestamp and temperature
  temp<-cbind(data[,c(1,5:68)])
  
  #Put all temperature data into only ONE column; change from wide to long table format). Keep information that is pertinent to identify temp like timestamp and PP
  t <- temp %>%
    melt(id.vars=c( "TIMESTAMP"), value.name="temp")
  
  #Extract date from TIMESTAMP (this will make it easier to graph than TIMESTAMP, also so you can use it for daily average)
  t$date<-as.Date(t$TIMESTAMP)
  
  #converts data frame to data table
  t<-as.data.table(t)
  
  #Creates a "depth" column and fills it with NA as a dummy value; Assign depths for temp sensors. After April first use # code. The wire in PP3 was the wrong order and Elaine fixed it on 4/1/24
  t$depth<-NA
  
  t_pre<-filter(t,date<"2024-04-01")
  t_pre$depth<-NA
  
  t_post<-filter(t,date>"2024-04-01")
  t_post$depth<-NA
  
  #CONFIRM THE DEPTHS FOR THE GMF PROBES!!!!!
  t_pre[, depth := ifelse(variable=="Tc_Avg(1)"|variable=="Tc_Avg(6)"|variable=="Tc_Avg(11)"|variable=="Tc_Avg(16)"|variable=="Tc_Avg(22)"|variable=="Tc_Avg(27)", "50 cm", depth)]
  t_pre[, depth := ifelse(variable=="Tc_Avg(2)"|variable=="Tc_Avg(7)"|variable=="Tc_Avg(12)"|variable=="Tc_Avg(17)"|variable=="Tc_Avg(23)"|variable=="Tc_Avg(28)", "30 cm", depth)]
  t_pre[, depth := ifelse(variable=="Tc_Avg(3)"|variable=="Tc_Avg(8)"|variable=="Tc_Avg(13)"|variable=="Tc_Avg(18)"|variable=="Tc_Avg(24)"|variable=="Tc_Avg(29)", "20 cm", depth)]
  t_pre[, depth := ifelse(variable=="Tc_Avg(4)"|variable=="Tc_Avg(9)"|variable=="Tc_Avg(14)"|variable=="Tc_Avg(19)"|variable=="Tc_Avg(25)"|variable=="Tc_Avg(30)", "10 cm", depth)]
  t_pre[, depth := ifelse(variable=="Tc_Avg(5)"|variable=="Tc_Avg(10)"|variable=="Tc_Avg(15)"|variable=="Tc_Avg(20)"|variable=="Tc_Avg(21)"|variable=="Tc_Avg(26)", "5 cm", depth)]
  
  t_post[, depth := ifelse(variable=="Tc_Avg(1)"|variable=="Tc_Avg(6)"|variable=="Tc_Avg(11)"|variable=="Tc_Avg(16)"|variable=="Tc_Avg(21)"|variable=="Tc_Avg(26)"|variable=="Tc_Avg(31)"|variable=="Tc_Avg(36)"|variable=="Tc_Avg(41)"|variable=="Tc_Avg(46)"|variable=="Tc_Avg(51)"|variable=="Tc_Avg(56)", "50 cm", depth)]
  t_post[, depth := ifelse(variable=="Tc_Avg(2)"|variable=="Tc_Avg(7)"|variable=="Tc_Avg(15)"|variable=="Tc_Avg(20)"|variable=="Tc_Avg(22)"|variable=="Tc_Avg(27)"|variable=="Tc_Avg(32)"|variable=="Tc_Avg(37)"|variable=="Tc_Avg(42)"|variable=="Tc_Avg(47)"|variable=="Tc_Avg(52)"|variable=="Tc_Avg(57)", "30 cm", depth)]
  t_post[, depth := ifelse(variable=="Tc_Avg(3)"|variable=="Tc_Avg(8)"|variable=="Tc_Avg(12)"|variable=="Tc_Avg(17)"|variable=="Tc_Avg(23)"|variable=="Tc_Avg(28)"|variable=="Tc_Avg(33)"|variable=="Tc_Avg(38)"|variable=="Tc_Avg(43)"|variable=="Tc_Avg(48)"|variable=="Tc_Avg(53)"|variable=="Tc_Avg(58)", "20 cm", depth)]
  t_post[, depth := ifelse(variable=="Tc_Avg(4)"|variable=="Tc_Avg(9)"|variable=="Tc_Avg(14)"|variable=="Tc_Avg(19)"|variable=="Tc_Avg(24)"|variable=="Tc_Avg(29)"|variable=="Tc_Avg(34)"|variable=="Tc_Avg(39)"|variable=="Tc_Avg(44)"|variable=="Tc_Avg(49)"|variable=="Tc_Avg(54)"|variable=="Tc_Avg(59)", "10 cm", depth)]
  t_post[, depth := ifelse(variable=="Tc_Avg(5)"|variable=="Tc_Avg(10)"|variable=="Tc_Avg(13)"|variable=="Tc_Avg(18)"|variable=="Tc_Avg(25)"|variable=="Tc_Avg(30)"|variable=="Tc_Avg(35)"|variable=="Tc_Avg(40)"|variable=="Tc_Avg(45)"|variable=="Tc_Avg(50)"|variable=="Tc_Avg(55)"|variable=="Tc_Avg(60)", "5 cm", depth)]
  
  #Merge t_pre and t_post
  t_tot<-t_post
  
  #Rename the full table back to t... BEWARE!!!!
  t<-t_tot
  
  #Assign treatment
  t$trt<-NA
  # Assign treatment H
  t[, trt := ifelse(variable %in% c("Tc_Avg(26)", "Tc_Avg(27)", "Tc_Avg(28)", "Tc_Avg(29)", "Tc_Avg(30)",
                                    "Tc_Avg(36)", "Tc_Avg(37)", "Tc_Avg(38)", "Tc_Avg(39)", "Tc_Avg(40)",
                                    "Tc_Avg(46)", "Tc_Avg(47)", "Tc_Avg(48)", "Tc_Avg(49)", "Tc_Avg(50)",
                                    "Tc_Avg(56)", "Tc_Avg(57)", "Tc_Avg(58)", "Tc_Avg(59)", "Tc_Avg(60)"), "H", trt)]
  
  # Assign treatment C (everything else up to 40 + all above 40)
  t[, trt := ifelse(variable %in% c("Tc_Avg(1)", "Tc_Avg(2)", "Tc_Avg(3)", "Tc_Avg(4)", "Tc_Avg(5)",
                                    "Tc_Avg(6)", "Tc_Avg(7)", "Tc_Avg(8)", "Tc_Avg(9)", "Tc_Avg(10)",
                                    "Tc_Avg(16)", "Tc_Avg(17)", "Tc_Avg(18)", "Tc_Avg(19)", "Tc_Avg(20)",
                                    "Tc_Avg(11)", "Tc_Avg(12)", "Tc_Avg(13)", "Tc_Avg(14)", "Tc_Avg(15)",
                                    "Tc_Avg(21)", "Tc_Avg(22)", "Tc_Avg(23)", "Tc_Avg(24)", "Tc_Avg(25)",
                                    "Tc_Avg(31)", "Tc_Avg(32)", "Tc_Avg(33)", "Tc_Avg(34)", "Tc_Avg(35)",
                                    "Tc_Avg(41)", "Tc_Avg(42)", "Tc_Avg(43)", "Tc_Avg(44)", "Tc_Avg(45)",
                                    "Tc_Avg(51)", "Tc_Avg(52)", "Tc_Avg(53)", "Tc_Avg(54)", "Tc_Avg(55)"), "C", trt)]
  
  #Assign paired plot
  t$PP<-NA
  t[, PP := ifelse(variable=="Tc_Avg(21)"|variable=="Tc_Avg(22)"|variable=="Tc_Avg(23)"|variable=="Tc_Avg(24)"|variable=="Tc_Avg(25)"|variable=="Tc_Avg(26)"|variable=="Tc_Avg(27)"|variable=="Tc_Avg(28)"|variable=="Tc_Avg(29)"|variable=="Tc_Avg(30)", "1", PP)]
  
  t[, PP := ifelse(variable=="Tc_Avg(31)"|variable=="Tc_Avg(32)"|variable=="Tc_Avg(33)"|variable=="Tc_Avg(34)"|variable=="Tc_Avg(35)"|variable=="Tc_Avg(36)"|variable=="Tc_Avg(37)"|variable=="Tc_Avg(38)"|variable=="Tc_Avg(39)"|variable=="Tc_Avg(40)", "2", PP)]
  
  t[, PP := ifelse(variable=="Tc_Avg(41)"|variable=="Tc_Avg(42)"|variable=="Tc_Avg(43)"|variable=="Tc_Avg(44)"|variable=="Tc_Avg(45)"|variable=="Tc_Avg(46)"|variable=="Tc_Avg(47)"|variable=="Tc_Avg(48)"|variable=="Tc_Avg(49)"|variable=="Tc_Avg(50)", "3", PP)] 
  t[, PP := ifelse(variable %in% c("Tc_Avg(51)", "Tc_Avg(52)", "Tc_Avg(53)", "Tc_Avg(54)", "Tc_Avg(55)", "Tc_Avg(56)", "Tc_Avg(57)", "Tc_Avg(58)", "Tc_Avg(59)", "Tc_Avg(60)"), "4", PP)]
  t[, PP := fifelse(variable %in% paste0("Tc_Avg(", 1:5, ")"), "5",
                           fifelse(variable %in% paste0("Tc_Avg(", 6:10, ")"), "6",
                                   fifelse(variable %in% paste0("Tc_Avg(", 11:15, ")"), "7",
                                           "8")))]
  # Create a new data frame for NB plots
  nb_plots <- t[variable %in% paste0("Tc_Avg(", 1:20, ")")]
  b_plots <- t[variable %in% paste0("Tc_Avg(", 21:60, ")")]
  #Remove temp <0
  t[, temp := ifelse(temp<(-5), NA, temp)]
  t[, temp := ifelse(temp>55, NA, temp)]
  
  #convert PP and depth to numerical values
  t$PP<- as.numeric(t$PP)
  #t$depth<- as.numeric(t$depth)
  
  #This line is converting the depth column in the data frame t to a factor with ordered levels.
  t$depth <-factor(t$depth,levels = c("5 cm", "10 cm", "20 cm","30 cm","50 cm"),ordered = TRUE)
  
  t$trt <- factor(t$trt,levels = c("H", "C"))
  
  t <- t %>% filter(!is.na(temp) & !is.na(depth) & !is.na(trt) & !is.na(PP))
  t$temp <- as.numeric(t$temp)
  
  # Filter for H and C treatments separately
  t_h <- t %>% filter(trt == "H")
  t_c <- t %>% filter(trt == "C")
  
  # Join H and C datasets based on TIMESTAMP, depth, and PP
  t_diff_gmf <- t_h %>%
    inner_join(t_c, by = c("TIMESTAMP", "depth", "PP")) %>%
    mutate(diff_H_C = temp.x - temp.y) %>%  # Calculate differences for each depth
    select(TIMESTAMP, depth, PP, diff_H_C)  # Select relevant column
  #HUNTER THIS IS THE HISTOGRAM YOU MADE, REMEMBER THIS WAS VERY MUCH A FAST GRAPH. WE ARENT SURE WHAWT DEPTHS ARE WHAT, OR EVEN WHAT PLOTS THE WIRES ARE COMING FROM. ONCE WE RETURN WITH PORT VALUES WE CAN CHANGE THE ABOVE TO DO THIS CORRECTLY
  t_diff_gmf_filtered <- subset(t_diff_gmf, PP == 1)
  t_diff_gmf_filtered$diff_H_C <- t_diff_gmf_filtered$diff_H_C
  
  binned <- t_diff_gmf_filtered %>%
    mutate(bin = cut(diff_H_C, breaks = seq(0, 5, by = 0.5), right = FALSE)) %>%
    group_by(depth, bin) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(depth) %>%
    mutate(prop = n / sum(n)) %>%
    mutate(bin_mid = as.numeric(sub("\\[(\\d+\\.?\\d*),.*", "\\1", bin)) + 0.25)
  
  # Plot
  ggplot(binned, aes(x = bin_mid, y = prop)) +
    geom_col(fill = "grey", color = "black", alpha = 0.7, width = 0.5) +
    facet_wrap(~ depth, scales = "free_y", ncol=6) +
    scale_x_continuous(breaks = seq(0, 5, 0.5), limits = c(0, 5)) +
    scale_y_continuous(limits = c(0, 1)) +
    labs(
      x = "Temperature Difference of Heated and Control",
      y = "Proportion",
      title = "Histogram of GMF Temp Diff by Depth for PP 1"
    ) +
    theme(
      strip.text = element_text(size = 12, face = "italic"),
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 12),
      plot.title = element_text(size = 14, face = "bold")+
    theme(axis.text.x=element_text(angle=-45))
    )

  t %>%
    filter(PP == 1) %>%
    ggplot(aes(x = TIMESTAMP, y = temp, colour = trt)) +
    geom_point(shape = 16, size = 1) +
    ylab(expression("Temperature " (degree * C))) +
    labs(
      title = "GMF Temperature Control and Heated Pair Plot 1",
      color = NULL,  # Remove legend title
      x = NULL
    ) +
    theme_bw() +
    theme(
      legend.text = element_text(size = 12),
      axis.text.x = element_text(size = 12, angle = -90),
      axis.text.y = element_text(size = 14),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12),
      strip.text = element_text(size = 12, color = "black", face = "italic")
    ) +
    scale_color_manual(values = c("red", "blue")) +
    guides(colour = guide_legend(override.aes = list(size = 5))) +
    facet_grid(PP ~ depth) +
    scale_x_datetime(breaks = "1 month", date_labels = "%b-%d")
  
  temp_diff <- t %>%
    filter(PP == 1) %>%
    group_by(TIMESTAMP, depth) %>%
    summarize(
      temp_H = mean(temp[trt == "H"], na.rm = TRUE),
      temp_C = mean(temp[trt == "C"], na.rm = TRUE)
    ) %>%
    mutate(temp_diff = temp_H - temp_C) %>%
    ungroup()
  
  ggplot(temp_diff, aes(x = TIMESTAMP, y = temp_diff)) +
    geom_point(shape = 16, size = 1, color = "purple") +
    ylab(expression("Temperature Difference (H - C) " * degree * C)) +
    labs(
      title = "Temperature Difference Between Heated and Control Treatments Plot 1",
      x = NULL
    ) +
    theme_bw() +
    theme(
      axis.text.x = element_text(size = 12, angle = -90),
      axis.text.y = element_text(size = 14),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12),
      strip.text = element_text(size = 12, color = "black", face = "italic")
    ) + facet_wrap(~ depth, nrow = 1) +
    scale_x_datetime(breaks = "1 month", date_labels = "%b-%d")
  #KEEP
  #Temp raw data
  t %>%
  filter(PP == 1) %>%
  ggplot(aes(x = TIMESTAMP, y = temp, colour = trt)) +
  geom_point(shape = 16, size = 1) +
  ylab(expression("Temperature " (degree * C))) +
  labs(
    title = "GMF Temperature Control and Heated Plots 1-3",
    color = NULL,  # Remove legend title
    x = NULL
  ) +
  theme_bw() +
  theme(
    legend.text = element_text(size = 12),
    axis.text.x = element_text(size = 12, angle = -45),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    strip.text = element_text(size = 12, color = "black", face = "italic")
  ) +
  scale_color_manual(values = c("red", "blue")) +
  guides(colour = guide_legend(override.aes = list(size = 5))) +
  facet_grid(PP ~ depth) +
  scale_x_datetime(breaks = "1 week", date_labels = "%b-%d")
  # ggsave("C:/Program Files/RStudio/Task 5 Microwarming/Plots/GMF Temperature Contol and Heated Plots 1-3.pdf", width=6,height=4,dpi=300)
    ggsave("C:/Program Files/RStudio/Task 5 Microwarming/Plots/Research Paper/GMF Temperature Contol and Heated Plots 1-3.jpg", width=15, height=10)
    
  #performs data summarization and calculates differences between means for different groups


# summary <- t %>%
#   dplyr::group_by(TIMESTAMP,depth, trt, PP) %>%
#   dplyr::summarise(
#     N=length(temp),
#     mean= mean(temp, na.rm=T),
#     sd=sd(temp, na.rm=T),
#     se=sd(temp, na.rm=T)/sqrt(sum(!is.na(temp))))
# 
# t.trt.diff <- summary %>%
#   group_by(TIMESTAMP, depth, PP) %>%
#   mutate(diff = mean - lag(mean, default = NA))

# Step 1: Calculate summary statistics for each group
summary_gmf <- t %>%
  dplyr::group_by(TIMESTAMP, depth, trt, PP) %>%
  dplyr::summarise(
    temp = mean(temp, na.rm = TRUE),  # used to organize the code
  )

# Step 2: Calculate overall mean, sd, and se values per group (TIMESTAMP, depth, trt)
summary_final_gmf <- summary_gmf %>%
  dplyr::group_by(TIMESTAMP, depth, trt) %>%
  dplyr::summarise(
    mean = mean(temp, na.rm = TRUE),  # Calculate mean of 'mean' within each group
    sd = sd(temp, na.rm = TRUE),      # Calculate sd of 'num' within each group
    se = sd(temp, na.rm = TRUE) / sqrt(sum(!is.na(num)))  # Calculate se of 'num' within each group
  )


summary_final_gmf$depth <- factor(summary_final_gmf$depth, labels=c("5", "10", "20", "30", "50"))

# #OLD PLOT
# ggplot(t_diff_gmf, aes(x=TIMESTAMP, y=-diff_H_C, colour=depth)) +
#   geom_point(shape=1,size=0.5) + 
#   ylab(expression(Delta*"Temperature " ( degree*C))) +
#   xlab("")+
#   scale_y_continuous(breaks=c(0,2,4))+
#   #labs(colour="Warming Treatment")+
#   #scale_color_manual(values = c("deepskyblue1","red4"))+ 
#   theme(plot.title=element_text(size=20, face="bold", vjust=1),
#         axis.title=element_text(size=20),
#         axis.text.x=element_text(size=12),
#         axis.text.y = element_text(size=14, hjust = .5),
#         axis.line=element_line(colour="black", size=.2),
#         legend.position="none",
#         panel.spacing.y=unit(0, "lines"),
#         panel.border = element_rect(color = "black", fill = NA, size = .5),
#         #theme(axis.text.x=element_text(angle=-45))+
#         #panel.grid.minor.x = element_blank(),
#         #panel.grid.major.x = element_blank(),
#         #panel.grid.minor.y = element_blank(),
#         panel.background = element_blank(),
#         strip.background=element_rect(fill = 'white', colour = "black"),
#         strip.text = element_text(size = 18, face="bold"))+
#   #panel.grid.major.y=element_line(colour="gray", linetype="solid", size=.2))+
#   geom_hline(yintercept=4,color='red')+
#   geom_hline(yintercept=2,color='orange')+
#   facet_grid(depth~PP)
# #ggsave(""C:/Program Files/RStudio/Task 5 Microwarming/Plots/PP1-2-3_Temp-Diff_Date__10cm.pdf", width=6,height=4,dpi=300)

library(plyr)
library(dplyr)

# data_summary <- function(data, varname, groupnames){
#   require(plyr)
#   summary_func <- function(x, col){
#     c(mean = mean(x[[col]], na.rm=TRUE),
#       sd = sd(x[[col]], na.rm=TRUE))
#   }
#   data_sum<-ddply(data, groupnames, .fun=summary_func,
#                   varname)
#   data_sum <- rename(data_sum, c("mean" = varname))
#   return(data_sum)
# }
# 
# TempAvg<-data_summary(summary,varname="mean",c("TIMESTAMP","depth", "trt"))



#KEEP
#mean soil temp
ggplot(summary_final_gmf, aes(x = TIMESTAMP, y = mean, colour = trt)) +
  geom_point(shape = 1, alpha = 0.5, size = 0.01) +
  geom_line(size = 1, aes(group = trt)) + 
  scale_color_manual(values = c("C" = "blue", "H" = "red")) +
  scale_fill_manual(values = c("C" = "blue", "H" = "red")) +
  ylab("Mean Temperature (degree*C)") +
  xlab("Date") +
  theme_bw() +
  facet_grid(~depth) +
  ggtitle("GMF Mean Temperature Heated and Controlled") +
  scale_x_datetime(breaks = "8 days", date_labels = "%b-%d") +
  theme(axis.text.x = element_text(angle = -45))
# ggsave("C:/Program Files/RStudio/Task 5 Microwarming/Plots/GGMF Heated and Controlled Mean.jpg.pdf", width=15, height=10)
  ggsave("C:/Program Files/RStudio/Task 5 Microwarming/Plots/Research Paper/GMF Mean Temperature Heated and Controlled.jpg", width=15, height=10)


#KEEP
#standard deviation 
ggplot(summary_final_gmf, aes(x = TIMESTAMP, y = mean, colour = trt)) +
  geom_point(size = 0.5) +  geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd, fill = trt), alpha = 0.5, colour = NA) +
  scale_color_manual(values = c("C" = "blue", "H" = "red")) +
  scale_fill_manual(values = c("C" = "blue", "H" = "red")) +
  ylab("Temperature (degree*C)") +
  xlab("Date") +
  theme_bw() +
  theme(
    legend.text = element_text(size=12),
    axis.text.x = element_text(size= 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size=12),  # Increase x-axis label font size
    axis.title.y = element_text(size=12),# Increase y-axis label font size
    strip.text = element_text(size = 12, color = "black", face = "italic")
  ) +
  facet_grid(~depth) +
  ggtitle("GMF Temperature Mean & Standard Deviation Plots 1-3") +
  scale_x_datetime(breaks = "8 days", date_labels = "%b-%d") +
  theme(axis.text.x = element_text(angle = -45))+
  labs(color = NULL, fill = NULL)
#  ggsave("C:/Program Files/RStudio/Task 5 Microwarming/Plots/GMF Temperature Standard Deviation Plots 1-3.pdf", width=15, height=10)
  ggsave("C:/Program Files/RStudio/Task 5 Microwarming/Plots/Research Paper/GMF Temperature Mean & Standard Deviation Plots 1-3.jpg", width=15, height=10)

#KEEP
#standard deviation 50 cm
ggplot(filter(summary_final_gmf, depth == "50"), aes(x = TIMESTAMP, y = mean, colour = trt)) +
  geom_point(shape = 1, alpha = 0.5, size = 0.01) +
  geom_line(size = 1, aes(group = trt)) + 
  geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd, fill = trt), alpha = 0.2) +
  scale_color_manual(values = c("C" = "blue", "H" = "red")) +
  scale_fill_manual(values = c("C" = "blue", "H" = "red")) +
  ylab("Soil Temperature (degree*C)") +
  xlab("Date") +
  theme_bw() +
  ggtitle("GMF Heated and Controlled Standard Deviation 50 cm Depth") +
  scale_x_datetime(breaks = "8 days", date_labels = "%b-%d") +
  theme(axis.text.x = element_text(angle = -45))
#  ggsave("C:/Program Files/RStudio/Task 5 Microwarming/Plots/GMF Heated and Controlled Standard Deviation 50 cm Depth.pdf", width=15, height=10)
#  ggsave("C:/Program Files/RStudio/Task 5 Microwarming/Plots/GMF Heated and Controlled Standard Deviation 50 cm Depth.jpg", width=15, height=10)



# #OLD PLOT
# #Running Average
# ggplot(summary_final_gmf, aes(x=TIMESTAMP, y=mean, colour=trt)) +
#   geom_point(shape=1,size=0.75) + 
#   #geom_pointrange(aes(ymin=mean-sd, ymax=mean+sd)) +
#   #stat_smooth()+
#   geom_smooth()+
#   ylab(expression("Soil temperature " ( degree*C))) +
#   xlab("Date")+
#   theme_bw()+
#   scale_color_manual(values = c("red", "blue"))+
#   #facet_grid(trt~depth)+
#   facet_grid(~depth)+
#   ggtitle("10 cm sensor")+
#   scale_x_datetime(breaks="8 days",date_labels = "%b-%d")+
#   theme(axis.text.x=element_text(angle=-45))
# # ggsave("C:/Program Files/RStudio/Task 5 Microwarming/Plots/10 cm sensor.pdf", width=6,height=4,dpi=300)

t_diff_gmf <- filter(t_diff_gmf, is.na(diff) == FALSE)

#save PP as a factor for plotting purposes
t_diff_gmf$PP <- as.factor(t_diff_gmf$PP)

# 
# ggplot(diff_TempAvg, aes(x=TIMESTAMP, y=-diff, colour=PP)) +
#   geom_point(shape=1,size=0.75) + 
#   theme_bw() +
#   #geom_pointrange(aes(ymin=mean-sd, ymax=mean+sd)) +
#   #stat_smooth()+
#   #geom_smooth()+
#   ylab(expression(Delta*"Soil temperature " ( degree*C))) +
#   xlab("Date")+
#   theme_bw()+
#   #facet_grid(trt~depth)+
#   facet_grid(~depth)+
#   ggtitle("10 cm Sensor") +
#   scale_x_datetime(breaks="8 days",date_labels = "%b-%d")+
#   scale_color_manual(values=palette1) +
#   geom_hline(yintercept=4,color='red')+
#   geom_hline(yintercept=3,color='orange')+
#   geom_hline(yintercept=2,color='green')+
#   theme(axis.text.x=element_text(angle=-45))
# #ggsave("C:/Program Files/RStudio/Task 5 Microwarming/Plots/PP1-2-3_Temp_Date_smoothed_10cm.pdf", width=6,height=4,dpi=300)
# 



#KEEP
# GMF Temp Diff
palette1 <- c("darkblue","deepskyblue2", "darkgray")
ggplot(t_diff_gmf, aes(x=TIMESTAMP, y= diff_H_C, colour=PP)) +
  geom_point(shape=1, size=0.75) + 
  theme_bw() +
  theme(
    legend.text = element_text(size=14),
    axis.text.x = element_text(size= 10),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size=14),  # Increase x-axis label font size
    axis.title.y = element_text(size=14),# Increase y-axis label font size
    strip.text = element_text(size = 12, color = "black", face = "italic")
  ) +
  ylab(expression(Delta*"Soil Temperature " ( degree*C))) +
  xlab("Date") +
  facet_grid(~depth) +
  ggtitle("GMF Heated and Controlled Temperature Difference Plots 1-3") +
  scale_x_datetime(breaks="8 days", date_labels="%b-%d") +
  scale_color_manual(values=palette1) +
  geom_hline(yintercept=4, color='red') +
  geom_hline(yintercept=3, color='orange') +
  geom_hline(yintercept=2, color='green') +
  geom_hline(yintercept=1,color='blue')+
  theme(axis.text.x=element_text(angle=-45), panel.grid.major=element_blank(), panel.grid.minor=element_blank(), legend.position="none")
  # ggsave("C:/Program Files/RStudio/Task 5 Microwarming/Plots/Research Paper/GGMF Heated and Controlled Temperature Difference Plots 1-3.jpg", width=15, height=10)
# ggsave("C:/Program Files/RStudio/Task 5 Microwarming/Plots/GMF Heated and Controlled Temperature Difference Plots 1-3.pdf", width=15, height=10)
# ggsave("C:/Program Files/RStudio/Task 5 Microwarming/Plots/GMF Heated and Controlled Temperature Difference Plots 1-3.jpg", width=15, height=10)
  



