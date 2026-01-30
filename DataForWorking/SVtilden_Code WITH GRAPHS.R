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


#Code from Campbell sci to keep header, remove NANs, convert timestamp to correct format, and add column for file name as ID, 
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
setwd("C:/Users/emmam/Downloads/senior thesis/DataForWorking")

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

# ##SoilVUE
# #Select columns for SoilVUE
# soilvue<-cbind(data[,c(1,69:193)])

##SoilVUE
#Select columns for SoilVUE
SVburnt<-cbind(data[,c(1,69:356)])

#This will make it so there's only ONE column for temp, as opposed to various columns (goes from wide to long table format). Keep information that is pertinent to identify temp like timestamp and PP
sv <- SVburnt %>%
  melt(id.vars=c( "TIMESTAMP"), value.name="num")

sv=data.table(sv)


#Breakup variable name so we can extract plot and sensor info
sv[, c("rest", "plot","depth","avg"):= tstrsplit(variable, "_", fixed=TRUE)]
sv$rest<-NULL

#assign similar column names as above

#Paired plot info

sv[, PP := as.character(NA)]

# Assign paired plot (PP) values based on 'plot' column
sv[, PP := fifelse(plot == "BH1" | plot == "BC1", "1", PP)]
sv[, PP := fifelse(plot == "BH2" | plot == "BC2", "2", PP)]
sv[, PP := fifelse(plot == "BH3" | plot == "BC3", "3", PP)]
sv[, PP := fifelse(plot == "BH4" | plot == "BC4", "4", PP)]

# Map NB plots to their PP values
sv[, PP := fifelse(plot == "NB1", "5", PP)]
sv[, PP := fifelse(plot == "NB2", "6", PP)]
sv[, PP := fifelse(plot == "NB3", "7", PP)]
sv[, PP := fifelse(plot == "NB4", "8", PP)]

# sv$PP<-NA
# sv[, PP := ifelse(plot=="BH1"|plot=="BC1", "1", PP)]
# sv[, PP := ifelse(plot=="BH2"|plot=="BC2", "2", PP)]
# sv[, PP := ifelse(plot=="BH3"|plot=="BC3", "3", PP)]
# sv[, PP := ifelse(plot=="BH4"|plot=="BC4", "4", PP)]
# 
# 
# # Map NB plots to their PP values
# sv[, PP := fifelse(plot == "NB1", "5", PP)]
# sv[, PP := fifelse(plot == "NB2", "6", PP)]
# sv[, PP := fifelse(plot == "NB3", "7", PP)]
# sv[, PP := fifelse(plot == "NB4", "8", PP)]



#assign trt
sv$trt<-NA
sv[, trt := ifelse(plot=="BH1"|plot=="BH2"|plot=="BH3"|plot=="BH4", "H", trt)]
sv[, trt := ifelse(plot=="BC1"|plot=="BC2"|plot=="BC3"|plot=="BC4", "C", trt)]
sv[, trt := ifelse(plot=="NB1"|plot=="NB2"|plot=="NB3"|plot=="NB4", "H", trt)]

#assign measurement type
sv$measurement<-NA
sv[, measurement := ifelse(avg=="Avg(1)", "moisture", measurement)]
sv[, measurement := ifelse(avg=="Avg(2)", "permitivity", measurement)]
sv[, measurement := ifelse(avg=="Avg(3)", "temp", measurement)]
sv[, measurement := ifelse(avg=="Avg(4)", "EC", measurement)]

# #Extract date from TIMESTAMP (this will make it easier to graph than TIMESTAMP, also so you can use it for daily average)
# sv$date<-as.Date(sv$TIMESTAMP)

#subset temp
temp.sv<- subset(sv, measurement=="temp")
moisture.sv <- subset(sv, measurement=="moisture")
#Remove instrument noise
temp.sv[, num := ifelse(num<(-5), NA, num)]
temp.sv[, num := ifelse(num>40, NA, num)]

temp.sv$PP<- as.numeric(temp.sv$PP)
#temp.sv$depth<- as.numeric(temp.sv$depth)

#unique(temp.sv$depth)
temp.sv$depth <- factor(temp.sv$depth, labels=c("5", "10", "20","30", "40","50"))
moisture.sv$depth <- factor(moisture.sv$depth, labels=c("5", "10", "20","30", "40","50"))
temp.sv$trt <- factor(temp.sv$trt,levels = c("H", "C"))
moisture.sv$trt <- factor(moisture.sv$trt, levels = c("H", "C"))
temp.sv <- temp.sv %>% filter(!is.na(num) & !is.na(depth) & !is.na(trt) & !is.na(PP))
moisture.sv <- moisture.sv %>% filter(!is.na(num) & !is.na(depth) & !is.na(trt) & !is.na(PP))
# Filter for H and C treatments separately
t_h_sv <- temp.sv %>% filter(trt == "H")
m_h_sv <- moisture.sv %>% filter(trt == "H") %>% mutate(PP = as.double(PP))
t_c_sv <- temp.sv %>% filter(trt == "C")
m_c_sv <- moisture.sv %>% filter(trt == "C") %>% mutate(PP = as.double(PP))
# Join H and C datasets based on TIMESTAMP, depth, and PP
t_diff_sv <- t_h_sv %>%
  inner_join(t_c_sv, by = c("TIMESTAMP", "depth", "PP")) %>%
  mutate(diff_H_C = num.x - num.y) %>%  # Calculate differences for each depth
  select(TIMESTAMP, depth, PP, diff_H_C)  # Select relevant column
tempdiffbymoisture <- t_diff_sv %>%
  inner_join(m_h_sv, by = c("TIMESTAMP", "depth", "PP")) %>% 
  select(TIMESTAMP, depth, PP, diff_H_C, num)
temp_filtered_Burned <- temp.sv %>% filter(PP %in% 1:4)
moisture_filtered_Burned <- moisture.sv %>% filter(PP %in% 1:4)

temp_filtered_Unburned <- temp.sv %>% filter(PP %in% 5:8)
moisture_filtered_Unburned <- moisture.sv %>% filter(PP %in% 5:8)

#SCATTERPLOT FOR TEMPDIFF BY MOISTURE CONTENT
tempdiffbymoisture %>%
  filter(PP == 1) %>%
  ggplot(aes(x = num, y = diff_H_C)) +
  geom_point(alpha = 0.7, color = "blue") +
  facet_wrap(~ depth) +
  labs(
    x = "Moisture Number (num)",
    y = "Temperature Difference (Heated - Control)",
    title = "Scatter Plot of Temp Diff vs Moisture by Depth (PP = 1)"
  ) +
  theme(
    strip.text = element_text(size = 12, face = "italic"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold")
  )
#HISTOGRAM FOR DISTRIBUTION OF TEMP DIFFERENCE
t_diff_sv_filtered <- t_diff_sv #subset(t_diff_sv, PP == 1)

# Create the histogram
library(dplyr)
library(ggplot2)

# Bin and count
binned <- t_diff_sv_filtered %>%
  mutate(bin = cut(diff_H_C, breaks = seq(-2, 5.5, by = 0.5), right = FALSE)) %>%
  group_by(PP, depth, bin) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(PP, depth) %>%
  mutate(prop = n / sum(n)) %>%
  mutate(bin_mid = as.numeric(sub("\\[(-?\\d+\\.?\\d*),.*", "\\1", bin)) + 0.25)

# Plot
ggplot(binned, aes(x = bin_mid, y = prop)) +
  geom_col(fill = "red", color = "black", alpha = 0.7, width = 0.5) +
  facet_wrap(PP ~ depth, scales = "free_y", ncol=6) +
  scale_x_continuous(breaks = seq(-2, 5.5, 0.5), limits = c(-2, 5.5)) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    x = "Temperature Difference of Heated and Control",
    y = "Proportion",
    title = "Histogram of Temp Diff by Depth for each PP"
  ) +
  theme(
    strip.text = element_text(size = 12, face = "italic"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold")
  )
# KEEP
#Raw Temp data SoilVUE
ggplot(temp_filtered_Burned, aes(x = TIMESTAMP, y = num, color = trt)) +
  geom_point(shape = 16, size = 1) + 
  ylab(expression("Temperature (degree*C)")) +
  xlab("Date") +
  theme_bw() +
  theme(
    legend.text = element_text(size=14),
    axis.text.x = element_text(size= 14, angle = -45, vjust = 0.5),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size=14),  # Increase x-axis label font size
    axis.title.y = element_text(size=14),  # Increase y-axis label font size
    strip.text = element_text(size = 12, color = "black", face = "italic"),
    legend.title = element_blank()  # Remove legend title
  ) +
  guides(color = guide_legend(title = FALSE)) +  # Remove legend title
  facet_grid(PP ~ depth) +
  ggtitle("SoilVUE Controlled and Heated Temperature Burned Plots 1-4") +
  scale_color_manual(values = c("red","blue"))
# ggsave("C:/Program Files/RStudio/Tilden Park Warming/Graphs/SoilVUE Controlled and Heated Temperature Burned Plots 1-4.jpg", width=15,height=10)
# ggsave("C:/Program Files/RStudio/Task 5 Microwarming/Plots/Research Paper/SoilVUE Controlled and Heated Temperature Plots 1-3.jpg", width=15, height=10)

#Only for plot one because we only heated PP 1
ggplot(temp_filtered_Burned, 
       aes(x = TIMESTAMP, y = num, color = trt)) +
  geom_point(shape = 16, size = 1) + 
  ylab(expression("Temperature (degree*C)")) +
  xlab("Date") +
  theme_bw() +
  theme(
    legend.text = element_text(size=14),
    axis.text.x = element_text(size=14, angle = -45, vjust = 0.5),
    axis.text.y = element_text(size=14),
    axis.title.x = element_text(size=14),
    axis.title.y = element_text(size=14),
    strip.text = element_text(size=12, color="black", face="italic"),
    legend.title = element_blank()
  ) +
  guides(color = guide_legend(title = FALSE)) +
  facet_wrap(. ~ depth, ncol = 1) +  # Only facet by depth now
  ggtitle("SoilVUE Controlled and Heated Temperature Burned Plot 1") +
  scale_color_manual(values = c("red", "blue"))

#Raw Temp data SoilVUE
ggplot(temp_filtered_Unburned, aes(x = TIMESTAMP, y = num, color = trt)) +
  geom_point(shape = 16, size = 1) + 
  ylab(expression("Temperature (degree*C)")) +
  xlab("Date") +
  theme_bw() +
  theme(
    legend.text = element_text(size=14),
    axis.text.x = element_text(size= 14, angle = -45, vjust = 0.5),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size=14),  # Increase x-axis label font size
    axis.title.y = element_text(size=14),  # Increase y-axis label font size
    strip.text = element_text(size = 12, color = "black", face = "italic"),
    legend.title = element_blank()  # Remove legend title
  ) +
  guides(color = guide_legend(title = FALSE)) +  # Remove legend title
  facet_grid(PP ~ depth) +
  ggtitle("SoilVUE Controlled Temperature Unburned Plots 1-4") +
  scale_color_manual(values = c("red","blue"))
  ggsave("C:/Program Files/RStudio/Tilden Park Warming/Graphs/SoilVUE Controlled Temperature Unburned Plots 1-4.jpg", width=15,height=10)
# ggsave("C:/Program Files/RStudio/Task 5 Microwarming/Plots/Research Paper/SoilVUE Controlled and Heated Temperature Plots 1-3.jpg", width=15, height=10)

  
  
  
  
  

data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

# temp.sv$PP<- as.factor(temp.sv$PP)

#temp.sv$depth <-factor(temp.sv$depth,levels = c("5 cm", "10 cm", "20 cm","30 cm", "40 cm", "50 cm"),ordered = TRUE)


#Original 
# summary.sv <- temp.sv %>%
#  dplyr::group_by(TIMESTAMP,depth, trt, PP) %>%
#  dplyr::summarise(
#   N=length(num),
#   mean= mean(num, na.rm=T),
#   sd=sd(num, na.rm=T),
#   se=sd(num, na.rm=T)/sqrt(sum(!is.na(num))))


# Convert TIMESTAMP to datetime if it's not already
temp.sv$TIMESTAMP <- as.POSIXct(temp.sv$TIMESTAMP)

# Ensure depth, trt, and PP are factors if they are categorical
temp.sv$depth <- as.factor(temp.sv$depth)
temp.sv$trt <- as.factor(temp.sv$trt)
temp.sv$PP <- as.factor(temp.sv$PP)





# Step 1: Calculate summary statistics for each group
summary_sv <- temp.sv %>%
  dplyr::group_by(TIMESTAMP, depth, trt, PP) %>%
  dplyr::summarise(
    num = mean(num, na.rm = TRUE),  # Calculate mean of 'num' within each group
  )


# Step 2: Calculate overall mean, sd, and se values per group (TIMESTAMP, depth, trt)
summary_final_sv <- summary_sv %>%
  dplyr::group_by(TIMESTAMP, depth, trt) %>%
  dplyr::summarise(
    mean = mean(num, na.rm = TRUE),  # Calculate mean of 'mean' within each group
    sd = sd(num, na.rm = TRUE),      # Calculate sd of 'num' within each group
    se = sd(num, na.rm = TRUE) / sqrt(sum(!is.na(num)))  # Calculate se of 'num' within each group
  )

#TEMP DIFF BETWEEN BURNED AND UNBURNED
# Step 1: Summarize Burned data
newburned <- temp_filtered_Burned %>% filter(trt != "H")
burned_summary <- newburned  %>%
  group_by(TIMESTAMP, depth) %>%
  summarize(mean_num_burned = mean(num, na.rm = TRUE), .groups = "drop")

# Step 2: Summarize Unburned data
unburned_summary <- temp_filtered_Unburned %>%
  group_by(TIMESTAMP, depth) %>%
  summarize(mean_num_unburned = mean(num, na.rm = TRUE), .groups = "drop")

# Step 3: Merge the two summaries by TIMESTAMP and Depth (inner join to get shared ones)
comparison_df <- inner_join(burned_summary, unburned_summary, by = c("TIMESTAMP", "depth"))

# Step 4: Add comparison columns if needed
comparison_df <- comparison_df %>%
  mutate(
    avg_num = (mean_num_burned + mean_num_unburned) / 2,
    diff_num = mean_num_burned - mean_num_unburned
  )

filtered_df <- comparison_df

ggplot(filtered_df, aes(x = TIMESTAMP, y = diff_num)) +
  geom_point(color = "red", size = 1.5) +
  facet_wrap(~ depth, scales = "free_y") +
  labs(
    x = "Timestamp",
    y = "Difference in Temperature C (Burned - Unburned) "
  ) +
  theme_minimal() +
  theme(
    legend.text = element_text(size=20),
    axis.text.x = element_text(size=20, angle = -45, vjust = 0.5),
    axis.text.y = element_text(size=20),
    axis.title.x = element_text(size=20),
    axis.title.y = element_text(size=20),
    strip.text = element_text(size=18, color="black", face="italic"),
    legend.title = element_blank()
  )

#MOISTURE DIFF BETWEEN BURNED AND UNBURNED
# Step 1: Summarize Burned data
newburned <- moisture_filtered_Burned %>% filter(trt != "H")
burned_summary <- newburned %>%
  group_by(TIMESTAMP, depth) %>%
  summarize(mean_num_burned = mean(num, na.rm = TRUE), .groups = "drop")

# Step 2: Summarize Unburned data
unburned_summary <- moisture_filtered_Unburned %>%
  group_by(TIMESTAMP, depth) %>%
  summarize(mean_num_unburned = mean(num, na.rm = TRUE), .groups = "drop")

# Step 3: Merge the two summaries by TIMESTAMP and Depth (inner join to get shared ones)
comparison_df <- inner_join(burned_summary, unburned_summary, by = c("TIMESTAMP", "depth"))

# Step 2: Reshape from wide to long format
long_df <- filtered_df %>%
  pivot_longer(cols = c(mean_num_burned, mean_num_unburned),
               names_to = "condition",
               values_to = "mean_moisture") %>%
  mutate(condition = recode(condition,
                            mean_num_burned = "Burned",
                            mean_num_unburned = "Unburned"))

# Step 3: Plot burned and unburned on same plot
ggplot(long_df, aes(x = TIMESTAMP, y = mean_moisture, color = condition)) +
  geom_line(size = 1.5) +
  facet_wrap(~ depth, scales = "free_y") +
  labs(
    x = "Timestamp",
    y = "Mean Moisture",
    color = "Condition"
  ) +
  theme_minimal() +
  theme(
    legend.text = element_text(size=20),
    legend.title = element_text(size=20),
    axis.text.x = element_text(size=20, angle = -45, vjust = 0.5),
    axis.text.y = element_text(size=20),
    axis.title.x = element_text(size=20),
    axis.title.y = element_text(size=20),
    strip.text = element_text(size=18, color="black", face="italic")
  )
# # Function to calculate summary statistics using data.table
# data_summary <- function(data, varname, groupnames) {
#   data <- as.data.table(data)
#   summary <- data[, .(
#     mean = mean(get(varname), na.rm = TRUE),
#     sd = sd(get(varname), na.rm = TRUE)  # Use get(varname) for sd calculation
#   ), by = groupnames]
#   return(summary)
# }
# 
# 
# # Apply the function to the summary data
# TempAvg_SV <- data_summary(summary_final, varname="mean", groupnames=c("TIMESTAMP", "depth", "trt"))


# t.trt.diff_sv <- summary_final %>%
#   group_by(TIMESTAMP, depth, trt) %>%
#   tidyr::pivot_wider(names_from = trt, values_from = mean) %>%
#   mutate(diff = H - C) %>%
#   tidyr::pivot_longer(cols = c(H, C), names_to = "trt", values_to = "mean") %>%
#   filter(!is.na(mean))

#KEEP
#standard deviation all depths
ggplot(summary_final_sv, aes(x = TIMESTAMP, y = mean, colour = trt)) +
  geom_point(size = 0.5) +
  geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd, fill = trt), alpha = 0.5, colour = NA) +
  scale_color_manual(values = c("C" = "blue", "H" = "red")) +
  scale_fill_manual(values = c("C" = "blue", "H" = "red")) +
  ylab("Mean Temp (degree*C)") +
  xlab("Date") +
  theme_bw() +
  theme(
    legend.text = element_text(size=14),
    axis.text.x = element_text(size= 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size=14),  # Increase x-axis label font size
    axis.title.y = element_text(size=14),# Increase y-axis label font size
    strip.text = element_text(size = 12, color = "black", face = "italic")
  ) +
  facet_grid(~depth) +
  ggtitle("SoilVUE Mean Temperature & Standard Deviation Plots 1-3") +
  scale_x_datetime(breaks = "8 days", date_labels = "%b-%d") +
  theme(axis.text.x = element_text(angle = -45)) +
  labs(color = NULL, fill = NULL)
# ggsave("C:/Program Files/RStudio/Task 5 Microwarming/Plots/SoilVUE Mean Temperature Standard Deviation Plots 1-3.pdf", width=15, height=10)
  # ggsave("C:/Program Files/RStudio/Task 5 Microwarming/Plots/Research Paper/SoilVUE Mean Temperature & Standard Deviation Plots 1-3.jpg", width=15, height=10)



#KEEP 
#standard deviation plots 30 cm depth
summary_final_sv_30cm <- subset(summary_final_sv, depth == '30')

ggplot(summary_final_sv_30cm, aes(x = TIMESTAMP, y = mean, colour = trt)) +
  geom_point(size = 0.5) +
  geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd, fill = trt), alpha = 0.5, colour = NA) +
  scale_color_manual(values = c("C" = "blue", "H" = "red")) +
  scale_fill_manual(values = c("C" = "blue", "H" = "red")) +
  ylab("Temperature (degree*C)") +
  xlab("Date") +
  theme_bw() +
  theme(
    legend.text = element_text(size=14),
    axis.text.x = element_text(size= 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size=14),  # Increase x-axis label font size
    axis.title.y = element_text(size=14),# Increase y-axis label font size
    strip.text = element_text(size = 12, color = "black", face = "italic")
  ) +
  ggtitle("SoilVUE Temperature Standard Deviation Plot for 30 cm Depth") +
  scale_x_datetime(breaks = "8 days", date_labels = "%b-%d") +
  theme(axis.text.x = element_text(angle = -45)) +
  labs(color = NULL, fill = NULL)
# ggsave("C:/Program Files/RStudio/Task 5 Microwarming/Plots/SoilVUE Temperature Standard Deviation Plot for 30cm Depth.pdf", width=10, height=5)
# ggsave("C:/Program Files/RStudio/Task 5 Microwarming/Plots/SoilVUE Temperature Standard Deviation Plot for 30cm Depth.jpg", width=10, height=5)



# the following line of code provides the difference plots for GMF and SC
t_diff_gmf2 <- t_diff_gmf %>%
  mutate(distance = 10)
t_diff_gmf2$PP <- as.numeric(as.character(t_diff_gmf2$PP))
t_diff_gmf2 <- t_diff_gmf2 %>%
  mutate(PP = case_when(
    PP == 1 ~ 4,
    PP == 2 ~ 5,
    PP == 3 ~ 6,
    TRUE ~ PP
  ))
t_diff_gmf2$PP <- as.factor(t_diff_gmf2$PP)
t_diff_gmf2 <- t_diff_gmf2[!is.na(t_diff_gmf2$diff), ]
t_diff_gmf2$depth <- as.character(t_diff_gmf2$depth)


t_diff_sv2 <- t_diff_sv %>%
  mutate(distance = 16.5)
t_diff_sv2$depth <- as.character(t_diff_sv2$depth)


t.trt.diff_sv_gmf <- rbind(t_diff_sv2, t_diff_gmf2)
t.trt.diff_sv_gmf$TIMESTAMP <- as.POSIXct(t.trt.diff_sv_gmf$TIMESTAMP, format = "%Y-%m-%d %H:%M:%S")
t.trt.diff_sv_gmf <- t.trt.diff_sv_gmf %>%
  arrange(TIMESTAMP) %>%
  group_by(TIMESTAMP) %>%
  arrange(match(depth, c("5", "10", "20", "30", "40", "50")))  # Specify the depths in the desired order
t.trt.diff_sv_gmf$depth <- factor(t.trt.diff_sv_gmf$depth, levels = c("5", "10", "20", "30", "40", "50"))


palette1 <- c("turquoise3", "steelblue", "navy", "lightgreen", "greenyellow", "green4")


library(ggnewscale)

# Separate SoilVUE and GMF datasets
soilvue_data <- subset(t.trt.diff_sv_gmf, PP %in% c(1, 2, 3))
gmf_data <- subset(t.trt.diff_sv_gmf, PP %in% c(4, 5, 6))

# SoilVUE and GMF Temp Diff
# Separate SoilVUE and GMF datasets
soilvue_data <- subset(t.trt.diff_sv_gmf, PP %in% c(1, 2, 3))
gmf_data <- subset(t.trt.diff_sv_gmf, PP %in% c(4, 5, 6))



#Keep
# SoilVUE and GMF Temp Diff
ggplot() +
  geom_point(data = soilvue_data, aes(x = TIMESTAMP, y = diff_H_C, colour = as.factor(PP)), shape = 1, size = 0.5) +
  scale_color_manual(name = "SoilVUE", values = palette1[1:3]) +  # Use the first three colors from the palette
  guides(colour = guide_legend(override.aes = list(size = 4, shape = 16))) +  # Increase dot size and make solid in the legend
  new_scale_color() +
  geom_point(data = gmf_data, aes(x = TIMESTAMP, y = diff_H_C, colour = as.factor(PP)), shape = 1, size = 0.5) +
  scale_color_manual(name = "GMF", values = palette1[4:6]) +  # Use the last three colors from the palette
  guides(colour = guide_legend(override.aes = list(size = 4, shape = 16))) +  # Increase dot size and make solid in the legend
  labs(
    x = NULL,
    y = expression(Delta * "Soil Temperature " (degree*C)),
    title = "SoilVUE & GMF Temperature Difference Plots 1-3"
  ) +
  scale_y_continuous(breaks = c(0, 2, 4)) +
  scale_x_datetime(breaks = "8 days", date_labels = "%b-%d") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", vjust = 1),
    axis.title = element_text(size = 16),
    axis.text.x = element_text(size = 12, angle = -45),
    axis.text.y = element_text(size = 14, hjust = 0.5),
    axis.line = element_line(colour = "black", size = 0.2),
    panel.spacing.y = unit(0, "lines"),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    strip.background = element_rect(fill = "white", colour = "black"),
    strip.text = element_text(size = 16, face = "bold")
  ) +
  geom_hline(yintercept = c(1, 2, 3, 4)) +  # Remove colour argument here
  facet_grid(~depth)
# ggsave("C:/Program Files/RStudio/Task 5 Microwarming/Plots/Research Paper/SoilVUE & GMF Temperature Difference Plots 1-3.pdf", width=6,height=3)
  # ggsave("C:/Program Files/RStudio/Task 5 Microwarming/Plots/Research Paper/SoilVUE & GMF Temperature Difference Plots 1-3.jpg", width=15, height=10)



# Old 
# SoilVUE and GMF Temp Diff
# ggplot(t.trt.diff_sv_gmf, aes(x = TIMESTAMP, y = diff_H_C, colour = as.factor(PP))) +
#   geom_point(shape = 1, size = 0.5) + 
#   labs(
#     x = NULL,
#     y = expression(Delta * "Soil Temperature " (degree*C)),
#     colour = "GMF & SoilVUE",
#     title = "SoilVUE & GMF Temperature Difference Plots 1-3"
#   ) +
#   scale_y_continuous(breaks = c(0, 2, 4)) +
#   scale_x_datetime(breaks = "8 days", date_labels = "%b-%d") +
#   theme_minimal() +
#   theme(
#     plot.title = element_text(size = 16, face = "bold", vjust = 1),
#     axis.title = element_text(size = 16),
#     axis.text.x = element_text(size = 12),
#     axis.text.y = element_text(size = 14, hjust = 0.5),
#     axis.line = element_line(colour = "black", size = 0.2),
#     panel.spacing.y = unit(0, "lines"),
#     panel.border = element_rect(color = "black", fill = NA, size = 0.5),
#     strip.background = element_rect(fill = "white", colour = "black"),
#     strip.text = element_text(size = 16, face = "bold")
#   ) +
#   scale_color_manual(values = palette1) +  # Use the defined palette
#   geom_hline(yintercept = c(1, 2, 3, 4)) +  # Remove colour argument here
#   facet_grid(~depth) +
#   theme(axis.text.x = element_text(angle = -45)) + 
#   guides(colour = guide_legend(override.aes = list(size = 4, shape = 16)))  # Increase dot size and make solid in the legend
# ggsave("C:/Program Files/RStudio/Task 5 Microwarming/Plots/SoilVUE & GMF Temperature Difference Plots 1-3.pdf", width=6,height=3)
 # ggsave("C:/Program Files/RStudio/Task 5 Microwarming/Plots/SoilVUE & GMF Temperature Difference Plots 1-3.jpg", width=15, height=10)


#Old Code
# SoilVUE and GMF Temp Diff
# ggplot(t.trt.diff_sv_gmf, aes(x=TIMESTAMP, y=diff_H_C, colour=as.factor(PP))) +
#   geom_point(shape=1, size=0.5) + 
#   ylab(expression(Delta * "Soil Temperature " (degree*C))) +
#   xlab("") +
#   scale_y_continuous(breaks=c(0, 2, 4)) +
#   scale_x_datetime(breaks="8 days", date_labels = "%b-%d") +
#   labs(colour="Warming Treatment") +
#   ggtitle("SoilVUE & GMF Temperature Difference Plots 1-3") +
#   theme(
#     plot.title = element_text(size=16, face="bold", vjust=1),
#     axis.title = element_text(size=16),
#     axis.text.x = element_text(size=12),
#     axis.text.y = element_text(size=14, hjust = 0.5),
#     axis.line = element_line(colour="black", linewidth=0.2),  # Use linewidth instead of size
#     legend.position = "none",
#     panel.spacing.y = unit(0, "lines"),
#     panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor.y = element_blank(),
#     panel.background = element_blank(),
#     strip.background = element_rect(fill = 'white', colour = "black"),
#     strip.text = element_text(size = 16, face="bold")
#   ) +
#   scale_color_manual(values=palette1) +  # Use the defined palette
#   geom_hline(data = subset(t.trt.diff_sv_gmf, depth == "17.5"), aes(yintercept = diff_H_C, colour = as.factor(PP)), show.legend = FALSE) +
#   geom_hline(data = subset(t.trt.diff_sv_gmf, depth == "10"), aes(yintercept = diff_H_C, colour = as.factor(PP)), show.legend = FALSE) +
#   geom_hline(yintercept=4, color='red') +
#   geom_hline(yintercept=3, color='orange') +
#   geom_hline(yintercept=2, color='green') +
#   geom_hline(yintercept=1, color='purple') +
#   facet_grid(~depth) +
#   theme(axis.text.x = element_text(angle=-45))



#Old
# The following lines of code provides the SV diff plots 1-3

t_diff_sv$PP <- as.factor(t_diff_sv$PP)

# #SoilVUE Difference Version 1
# ggplot(t_diff_sv, aes(x=TIMESTAMP, y=diff_H_C, colour=PP)) +
#   geom_point(shape=1,size=0.5) + 
#   ylab(expression(Delta*"Soil temperature " (degree*C))) +
#   xlab("Date")+
#   scale_y_continuous(breaks=c(0,2,4))+
#   scale_x_datetime(breaks="8 days",date_labels = "%b-%d")+
#   labs(colour="Warming Treatment")+
#   ggtitle("SoilVUE Heated and Controlled Temperature Difference Plots 1-3") +
#   theme(plot.title=element_text(size=16, face="bold", vjust=1),
#         axis.title=element_text(size=16),
#         axis.text.x=element_text(size=12),
#         axis.text.y = element_text(size=14, hjust = .5),
#         axis.line=element_line(colour="black", size=.2),
#         legend.position="none",
#         panel.spacing.y=unit(0, "lines"),
#         panel.border = element_rect(color = "black", fill = NA, linewidth = .5),
#         panel.grid.minor.x = element_blank(),
#         panel.grid.major.x = element_blank(),
#         panel.grid.minor.y = element_blank(),
#         panel.background = element_blank(),
#         strip.background=element_rect(fill = 'white', colour = "black"),
#         strip.text = element_text(size = 16, face="bold"))+
#   scale_color_manual(values=palette1) +
#   geom_hline(yintercept=4,color='red')+
#   geom_hline(yintercept=3,color='orange')+
#   geom_hline(yintercept=2,color='green')+
#   geom_hline(yintercept=1,color='blue')+
#   facet_grid(~depth)+
#   theme(axis.text.x=element_text(angle=-45))
# # ggsave("C:/Program Files/RStudio/Task 5 Microwarming/Plots/SoilVUE Heated and Controlled Temperature Difference Plots 1-3.pdf", width=20, height=10)



palette2 <- c("darkblue","deepskyblue2", "darkgray")

# #SoilVUE Temp Difference Version 2  
# ggplot(t_diff_sv, aes(x=TIMESTAMP, y= diff_H_C, colour=PP)) +
#   geom_point(shape=1, size=0.75) + 
#   theme_bw() +
#   theme(
#     legend.text = element_text(size=14),
#     axis.text.x = element_text(size= 10, angle = 45, hjust = 1, vjust = 0.5),
#     axis.text.y = element_text(size = 12),
#     axis.title.x = element_blank(),  # Remove x-axis title
#     #axis.title.x = element_text(size=10),  # Increase x-axis label font size
#     axis.title.y = element_text(size=12),# Increase y-axis label font size
#     strip.text = element_text(size = 12, color = "black", face = "italic"),
#   ) +
#   ylab(expression(Delta*"Soil Temperature " ( degree*C))) +
#   xlab("Date") +
#   facet_grid(~depth) +
#   ggtitle("SoilVUE Heated and Controlled Temperature Difference Plots 1-3") +
#   scale_x_datetime(breaks="20 days", date_labels="%b-%d") +
#   scale_color_manual(values=palette2) +
#   geom_hline(yintercept=4, color='red') +
#   geom_hline(yintercept=3, color='orange') +
#   geom_hline(yintercept=2, color='green') +
#   geom_hline(yintercept=1,color='blue')+
#   theme(axis.text.x=element_text(angle=-45), panel.grid.major=element_blank(), panel.grid.minor=element_blank(), legend.position="none")
# # ggsave("C:/Program Files/RStudio/Task 5 Microwarming/Plots/SoilVUE Heated and Controlled Temperature Difference Plots 1-3.pdf", width=10, height=5)
# # ggsave("C:/Program Files/RStudio/Task 5 Microwarming/Plots/SoilVUE Heated and Controlled Temperature Difference Plots 1-3.jpg", width=15, height=10)


dev.off()

#SoilVUE Moisture
moisture.sv<- subset(sv, measurement=="moisture")


moisture.sv$PP<- as.numeric(moisture.sv$PP)

unique(moisture.sv$depth)
moisture.sv$depth <- factor(moisture.sv$depth, labels=c("5 cm", "10 cm", "20 cm","30 cm", "40 cm","50 cm"))


#Keep
# moisture H and C (raw)
ggplot(moisture.sv, aes(x=TIMESTAMP, y=num, colour=trt)) +
  geom_point(shape=16, size=1) + 
  ylab(expression("Moisture Content")) +
  xlab("Date") +
  scale_color_manual(values = c("C" = "blue", "H" = "red")) +
  ggtitle("SoilVUE Moisture Plots 1-4") +
  theme_bw() +
  theme(
    legend.text = element_text(size=14),
    axis.text.x = element_text(size= 14, angle = -45, vjust = 0.5),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size=14),  # Increase x-axis label font size
    axis.title.y = element_text(size=14),# Increase y-axis label font size
    strip.text = element_text(size = 12, color = "black", face = "italic")
  ) +
  guides(colour = guide_legend(title = NULL)) +
  facet_grid(PP~depth)
  ggsave("C:/Program Files/RStudio/Tilden Park Warming/Graphs/SoilVUE Moisture Plots 1-4.jpg", width=15,height=10)
# ggsave("C:/Program Files/RStudio/Task 5 Microwarming/Plots/Research Paper/SoilVUE Moisture Plots 1-3.pdf.jpg", width=15, height=10)

library(ggplot2)

# Filter the data for PP 3
moisture.sv_PP3 <- subset(moisture.sv, PP == 3)


#Keep 
# Create raw data plot 3
ggplot(moisture.sv_PP3, aes(x=TIMESTAMP, y=num, colour=trt)) +
  geom_point(shape=5, size=0.5) + 
  ylab(expression("Moisture Content")) +
  xlab("Date") +
  scale_color_manual(values = c("C" = "blue", "H" = "red")) +
  #  scale_x_datetime(
  #   date_breaks = "10 days",       # Set break intervals to 20 days
  #    date_labels = "%d %b"       # Format the date labels
  # ) +
  ggtitle("SoilVUE Moisture Plot 3") +
  theme_bw() +
  theme(
    legend.text = element_text(size=14),
    axis.text.x = element_text(size= 12, angle = -45, vjust = 0.5),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size=14),  # Increase x-axis label font size
    axis.title.y = element_text(size=14),# Increase y-axis label font size
    strip.text = element_text(size = 12, color = "black", face = "italic")
  ) +
  guides(colour = guide_legend(title = NULL)) +
  facet_grid(. ~ depth)
# ggsave("C:/Program Files/RStudio/Task 5 Microwarming/Plots/SoilVUE Moisture Plot 3.pdf", width=10, height=5)
# ggsave("C:/Program Files/RStudio/Task 5 Microwarming/Plots/SoilVUE Moisture Plot 3.jpg", width=10, height=5)


moisture_num <- moisture.sv$num

names(temp.sv) <- c("TIMESTAMP", "variable", "temp_num", "plot", "depth", "avg", "PP", "trt", "measurement")

temp_moisture_sv <- cbind(temp.sv, moisture_num = moisture.sv$num)


#Keep 
# Temp vs moisture 
ggplot(temp_moisture_sv, aes(x = temp_num, y = moisture_num, colour = trt)) +
  geom_point(shape = 1, size = 0.75) +
  ylab(expression("Soil Moisture")) +
  xlab("Temperature (degree*C)") +
  labs(title = "SoilVUE Temperature vs Moisture Plots 1-3") +
  theme_bw() +
  facet_grid(PP ~ depth) +
  scale_color_manual(values = c("C" = "blue", "H" = "red"))
# ggsave("C:/Program Files/RStudio/Task 5 Microwarming/Plots/SoilVUE Temperature vs Moisture Plots 1-3.pdf", width=15,height=10,dpi=300)


#????
# temp_moisture_20_30_sv <- temp_moisture_sv %>%
#   filter(depth %in% c("20 cm", "30 cm", "40 cm", "50 cm"))
# 
# ggplot(temp_moisture_20_30_sv, aes(x = temp_num, y = moisture_num, colour = trt)) +
#   geom_point(shape = 1, size = 0.75) +
#   ylab(expression("Soil Moisture")) +
#   xlab("Temperature (degree*C)") +
#   labs(title = "SoilVUE Temperature vs Moisture 20cm-50cm Plots 1-3") +
#   theme_bw() +
#   facet_grid(pp ~ depth) +
#   scale_color_manual(values = c("C" = "blue", "H" = "red"))
# # ggsave("C:/Program Files/RStudio/Task 5 Microwarming/Plots/SoilVUE Temperature vs Moisture 20cm-50cm Plots 1-3.pdf", width=15,height=10)
# 
# temp_moisture_5_10_sv <- temp_moisture_sv %>%
#   filter(depth %in% c("5 cm", "10 cm"))



#????
# ggplot(temp_moisture_5_10_sv, aes(x = temp_num, y = moisture_num, colour = trt)) +
#   geom_point(shape = 1, size = 0.75) +
#   ylab(expression("Soil Moisture")) +
#   xlab("Temperature (degree*C)") +
#   labs(title = "SoilVUE Temperature vs Moisture 5cm-10cm Plots 1-3") +
#   theme_bw() +
#   facet_grid(pp ~ depth) +
#   scale_color_manual(values = c("C" = "blue", "H" = "red"))
# ggsave("C:/Program Files/RStudio/Task 5 Microwarming/Plots/SoilVUE Temperature vs Moisture 5cm-10cm Plots 1-3.pdf", width=15,height=10)

summary.moisture2 <- moisture.sv %>%
  dplyr::group_by(TIMESTAMP,depth, trt) %>%
  dplyr::summarise(
    N=length(num),
    mean= mean(num, na.rm=T),
    sd=sd(num, na.rm=T),
    se=sd(num, na.rm=T)/sqrt(sum(!is.na(num))))


#KEEP
#standard deviation moisture 
ggplot(summary.moisture2, aes(x = TIMESTAMP, y = mean, colour = trt)) +
  geom_point(size = 1) +
  geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd, fill = trt), alpha = 0.5, colour = NA) +
  scale_color_manual(values = c("C" = "blue", "H" = "red")) +
  scale_fill_manual(values = c("C" = "blue", "H" = "red")) +
  ylab("Moisture Content") +
  xlab("Date") +
  theme_bw() +
  theme(
    legend.text = element_text(size=14),
    axis.text.x = element_text(size= 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size=14),  # Increase x-axis label font size
    axis.title.y = element_text(size=14),# Increase y-axis label font size
    strip.text = element_text(size = 12, color = "black", face = "italic")
  ) +
  facet_grid(~depth) +
  ggtitle("SoilVUE Moisture Mean & Standard Deviation Plots 1-3") +
  scale_x_datetime(breaks = "8 days", date_labels = "%b-%d") +
  theme(axis.text.x = element_text(angle = -45)) +
  labs(color = NULL, fill = NULL)
# ggsave("C:/Program Files/RStudio/Task 5 Microwarming/Plots/SoilVUE Moisture Standard Deviation Plots 1-3.pdf", width=15, height=10)
  # ggsave("C:/Program Files/RStudio/Task 5 Microwarming/Plots/Research Paper/SoilVUE Moisture Standard Deviation Plots 1-3.jpg", width=15, height=10)



#SoilVUE Electrical Conductivity
EC.sv<- subset(sv, measurement=="EC")


EC.sv$PP<- as.numeric(EC.sv$PP)

unique(EC.sv$depth)
EC.sv$depth <- factor(EC.sv$depth, labels=c("5 cm", "10 cm", "20 cm","30 cm", "40 cm","50 cm"))


#KEEP
# EC H and C (raw)
ggplot(EC.sv, aes(x=TIMESTAMP, y=num, colour=trt)) +
  geom_point(size=0.5) + 
  ylab(expression("Electrical Conductivity (dS/m)")) +
  xlab("Date") +
  scale_color_manual(values = c("C" = "blue", "H" = "red")) +
  scale_x_datetime(
    date_breaks = "20 days",       # Set break intervals to 20 days
    date_labels = "%d %b"       # Format the date labels
  ) +
  ggtitle("SoilVUE Electrical Conductivity Plots 1-3") +
  theme_bw() +
  theme(
    legend.text = element_text(size=14),
    axis.text.x = element_text(size= 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size=14),  # Increase x-axis label font size
    axis.title.y = element_text(size=14),# Increase y-axis label font size
    strip.text = element_text(size = 12, color = "black", face = "italic")
  ) +
  guides(colour = guide_legend(title = NULL)) +
  facet_grid(PP~depth)
# ggsave("C:/Program Files/RStudio/Task 5 Microwarming/Plots/SoilVUE Moisture Plots 1-3.pdf", width=15, height=10)
# ggsave("C:/Program Files/RStudio/Task 5 Microwarming/Plots/Research Paper/SoilVUE Electrical Conductivity Plots 1-3.jpg", width=15, height=10)



# summary.EC <- EC.sv %>%
#   dplyr::group_by(TIMESTAMP,depth, trt, PP) %>%
#   dplyr::summarise(
#     N=length(num),
#     mean= mean(num, na.rm=T),
#     sd=sd(num, na.rm=T),
#     se=sd(num, na.rm=T)/sqrt(sum(!is.na(num))))

summary.EC2 <- EC.sv %>%
  dplyr::group_by(TIMESTAMP,depth, trt) %>%
  dplyr::summarise(
    N=length(num),
    mean= mean(num, na.rm=T),
    sd=sd(num, na.rm=T),
    se=sd(num, na.rm=T)/sqrt(sum(!is.na(num))))


#KEEP
#Standard deviation all depths
ggplot(summary.EC2, aes(x = TIMESTAMP, y = mean, colour = trt)) +
  geom_point(size = 1) +
  geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd, fill = trt), alpha = 0.5, colour = NA) +
  scale_color_manual(values = c("C" = "blue", "H" = "red")) +
  scale_fill_manual(values = c("C" = "blue", "H" = "red")) +
  ylab("Electrical Conductivity (dS/m)") +
  xlab("Date") +
  theme_bw() +
  theme(
    legend.text = element_text(size=14),
    axis.text.x = element_text(size= 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size=14),  # Increase x-axis label font size
    axis.title.y = element_text(size=14),# Increase y-axis label font size
    strip.text = element_text(size = 12, color = "black", face = "italic")
  ) +
  facet_grid(~depth) +
  ggtitle("SoilVUE Electrical Conductivity Mean & Standard Deviation Plots 1-3") +
  scale_x_datetime(breaks = "8 days", date_labels = "%b-%d") +
  theme(axis.text.x = element_text(angle = -45)) +
  labs(color = NULL, fill = NULL)
# ggsave("C:/Program Files/RStudio/Task 5 Microwarming/Plots/SoilVUE Temperature Standard Deviation Plots 1-3.pdf", width=15, height=10)
  # ggsave("C:/Program Files/RStudio/Task 5 Microwarming/Plots/Research Paper/SoilVUE Electrical Conductivity Standard Deviation Plots 1-3.jpg", width=15, height=10)




#SoilVUE Permitivity
PER.sv<- subset(sv, measurement=="permitivity")


PER.sv$PP<- as.numeric(PER.sv$PP)

unique(PER.sv$depth)
PER.sv$depth <- factor(PER.sv$depth, labels=c("5 cm", "10 cm", "20 cm","30 cm", "40 cm","50 cm"))


#KEEP
# EC H and C (raw)
ggplot(PER.sv, aes(x=TIMESTAMP, y=num, colour=trt)) +
  geom_point(size=0.5) + 
  ylab(expression("Permittivity (ɛ)")) +
  xlab("Date") +
  scale_color_manual(values = c("C" = "blue", "H" = "red")) +
  scale_x_datetime(
    date_breaks = "20 days",       # Set break intervals to 20 days
    date_labels = "%d %b"       # Format the date labels
  ) +
  ggtitle("SoilVUE Permittivity Plots 1-3") +
  theme_bw() +
  theme(
    legend.text = element_text(size=14),
    axis.text.x = element_text(size= 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size=14),  # Increase x-axis label font size
    axis.title.y = element_text(size=14),# Increase y-axis label font size
    strip.text = element_text(size = 12, color = "black", face = "italic")
  ) +
  guides(colour = guide_legend(title = NULL)) +
  facet_grid(PP~depth)
#  ggsave("C:/Program Files/RStudio/Task 5 Microwarming/Plots/SoilVUE Moisture Plots 1-3.pdf", width=15, height=10)
  ggsave("C:/Program Files/RStudio/Task 5 Microwarming/Plots/Research Paper/SoilVUE Permittivity Plots 1-3.jpg", width=15, height=10)
