#
#
#
#
#
#
#
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
#
#
#
#
#
#

#set working directory
setwd("C:/Users/emmam/Downloads/senior thesis/DataForWorking")

#convert to data table
data <- read.csv("20260325_Tilden_Data_foremma.csv", header = TRUE)
data <- data.table(data)

# Remove duplicates from data.
data <- data[!duplicated(data),]

#Order file based on timestamp
data<-data[order(TIMESTAMP),]


##SoilVUE
#Select columns for SoilVUE
SVburnt<-cbind(data[,c(1,69:356)])

#This will make it so there's only ONE column for temp, as opposed to various columns (goes from wide to long table format). Keep information that is pertinent to identify temp like timestamp and PP
sv <- SVburnt %>%
  melt(id.vars=c( "TIMESTAMP"), value.name="num")

# After melting to long format
sv <- data.table::melt(SVburnt, 
                       id.vars = "TIMESTAMP", 
                       value.name = "num")

# Parse the variable column into plot, depth, and avg
# Expected format: "PLOT_DEPTH_Avg(X)" e.g., "BH1_1_Avg(1)"
sv[, c("rest", "plot", "depth", "avg") := tstrsplit(variable, "_", fixed = TRUE)]

# Inspect unique values in avg column
unique(sv$avg) #average is now labelled as Avg.1., Avg.2., etc. which we can use to identify which measurement type it is (temp, moisture, etc.)

# Remove the original variable column if desired
sv[, variable := NULL]

#
#
#
#
#
#
# Now assign PP, trt, measurement using the parsed columns
sv[, PP := as.character(NA)]
sv[, PP := fifelse(plot == "BH1" | plot == "BC1", "1", PP)]
sv[, PP := fifelse(plot == "BH2" | plot == "BC2", "2", PP)]
sv[, PP := fifelse(plot == "BH3" | plot == "BC3", "3", PP)]
sv[, PP := fifelse(plot == "BH4" | plot == "BC4", "4", PP)]
sv[, PP := fifelse(plot == "NB1", "5", PP)]
sv[, PP := fifelse(plot == "NB2", "6", PP)]
sv[, PP := fifelse(plot == "NB3", "7", PP)]
sv[, PP := fifelse(plot == "NB4", "8", PP)]

sv[, trt := as.character(NA)]
sv[, trt := fifelse(plot %in% c("BH1","BH2","BH3","BH4"), "H", trt)]
sv[, trt := fifelse(plot %in% c("BC1","BC2","BC3","BC4"), "C", trt)]
sv[, trt := fifelse(plot %in% c("NB1","NB2","NB3","NB4"), "H", trt)]


sv[, measurement := as.character(NA)]
sv[, measurement := fifelse(avg == "Avg.1.", "moisture", measurement)]
sv[, measurement := fifelse(avg == "Avg.2.", "permitivity", measurement)]
sv[, measurement := fifelse(avg == "Avg.3.", "temp", measurement)]
sv[, measurement := fifelse(avg == "Avg.4.", "EC", measurement)]

# Subset by measurement type
temp.sv <- sv[measurement == "temp"]
moisture.sv <- sv[measurement == "moisture"]
ec.sv <- sv[measurement == "EC"]

# Inspect to confirm parsing worked
head(sv[, .(TIMESTAMP, plot, depth, avg, measurement, num)])
#
#
#
#
#
#Remove instrument noise
temp.sv[, num := ifelse(num<(-5), NA, num)]
temp.sv[, num := ifelse(num>40, NA, num)]
ec.sv[, num := ifelse(num==(-9999), NA, num)] #may 2025 powershutoff all have this value as placeholder, remove this data

temp.sv$PP<- as.numeric(temp.sv$PP)
moisture.sv$PP <- as.numeric(moisture.sv$PP)
ec.sv$PP <- as.numeric(ec.sv$PP)

#setting depths
temp.sv$depth <- factor(temp.sv$depth, labels=c("5", "10", "20","30", "40","50"))
moisture.sv$depth <- factor(moisture.sv$depth, labels=c("5", "10", "20","30", "40","50"))
ec.sv$depth <- factor(ec.sv$depth, labels=c("5", "10", "20","30", "40","50"))

#setting treatments
temp.sv$trt <- factor(temp.sv$trt,levels = c("H", "C"))
moisture.sv$trt <- factor(moisture.sv$trt, levels = c("H", "C"))
ec.sv$trt <- factor(ec.sv$trt, levels = c("H", "C"))

#remove missing data
temp.sv <- temp.sv %>% filter(!is.na(num) & !is.na(depth) & !is.na(trt) & !is.na(PP))
moisture.sv <- moisture.sv %>% filter(!is.na(num) & !is.na(depth) & !is.na(trt) & !is.na(PP))
ec.sv <- ec.sv %>% filter(!is.na(num) & !is.na(depth) & !is.na(trt) & !is.na(PP))


temp_filtered_Burned <- temp.sv %>% filter(PP %in% 1:4)
moisture_filtered_Burned <- moisture.sv %>% filter(PP %in% 1:4)

temp_filtered_Unburned <- temp.sv %>% filter(PP %in% 5:8)
moisture_filtered_Unburned <- moisture.sv %>% filter(PP %in% 5:8)

#fixing timestamp
temp.sv <- temp.sv |>
  mutate(TIMESTAMP = as.POSIXct(TIMESTAMP, format = "%Y-%m-%d %H:%M:%S", tz = "America/Los_Angeles"))
#
#
#
#
temp.sv |>
  filter(plot == "NB1") |>
  ggplot(aes(x = num))+
  geom_histogram(binwidth = 0.5)+
  facet_wrap(~ depth) +
  labs(
    x = "Temperature",
    y = "Frequency",
    title = "Temp of Nonburned Plot 1"
  ) +
  theme(
    strip.text = element_text(size = 12, face = "italic"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold")
  )

temp.sv |>
  filter(plot == "NB2") |>
  ggplot(aes(x = num))+
  geom_histogram(binwidth = 0.5)+
  facet_wrap(~ depth) +
  labs(
    x = "Temperature",
    y = "Frequency",
    title = "Temp of Nonburned Plot 2")+
  theme(
    strip.text = element_text(size = 12, face = "italic"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold")
  )

temp.sv |>
  filter(plot == "NB3") |>
  ggplot(aes(x = num))+
  geom_histogram(binwidth = 0.5)+
  facet_wrap(~ depth) +
  labs(
    x = "Temperature",
    y = "Frequency",
    title = "Temp of Nonburned Plot 3")+
  theme(
    strip.text = element_text(size = 12, face = "italic"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold")
  )

temp.sv |>
  filter(plot == "NB4") |>
  ggplot(aes(x = num))+
  geom_histogram(binwidth = 0.5)+
  facet_wrap(~ depth) +
  labs(
    x = "Temperature",
    y = "Frequency",
    title = "Temp of Nonburned Plot 4")+
  theme(
    strip.text = element_text(size = 12, face = "italic"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold")
  )
#
#
#
#
# Plot all nonburned plots with depths in rows, plots in columns
temp.sv %>%
  filter(plot %in% c("NB1", "NB2", "NB3", "NB4")) %>%
  ggplot(aes(x = num)) +
  geom_histogram(binwidth = 0.5, fill = "darkgreen", color = "darkgreen")+
  facet_grid(depth ~ plot) +
  labs(
    x = "Temperature (°C)",
    y = "Frequency",
    title = "Nonburned Plots Temperature Distribution by Depth"
  ) +
  theme_bw() +
  theme(
    legend.text = element_text(size = 12),
    axis.text.x = element_text(size = 10, angle = -45, hjust = 0),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12),
    strip.text = element_text(size = 11, face = "italic")
  )
#
#
#
#
# Overlapping density plot by depth and treatment

temp.sv |>
  filter(plot %in% c("NB1", "NB2", "NB3", "NB4")) |>
  ggplot(aes(x = num, fill = plot, color = plot)) +
  geom_density(alpha = 0.4) +
  facet_wrap(~ depth) +
  labs(
    x = "Temperature (°C)",
    y = "Density",
    title = "Nonburned Plots Temperature Distribution",
    fill = "Plot",
    color = "Plot"
  ) +
  theme_bw() +
  theme(
    legend.text = element_text(size = 12),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    strip.text = element_text(size = 11, face = "italic")
  )

#
#
#
#
nonburnt_avg <- temp.sv %>%
  filter(plot %in% c("NB1", "NB2", "NB3", "NB4")) %>%
  group_by(TIMESTAMP, depth) %>%
  summarise(mean_num = mean(num, na.rm = TRUE), .groups = "drop") %>%
  mutate(TIMESTAMP = as.POSIXct(TIMESTAMP, format = "%Y-%m-%d %H:%M:%S", tz = "America/Los_Angeles"))

nonburnt_avg |>
  ggplot(aes(x = mean_num)) +
  geom_histogram(binwidth = 0.5, fill = "darkgreen", color = "darkgreen") +
  facet_wrap(~ depth) +
  labs(
    x = "Temperature (°C)",
    y = "Frequency",
    title = "Nonburned Plots Average Temperature Distribution by Depth"
  ) +
  theme_bw() +
  theme(
    legend.text = element_text(size = 12),
    axis.text.x = element_text(size = 10, angle = -45, hjust = 0),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12),
    strip.text = element_text(size = 11, face = "italic")
  )

#
#
#
#
#
temp.sv |>
  filter(plot == "BC1") |>
  ggplot(aes(x = num))+
  geom_histogram(binwidth = 0.5)+
  facet_wrap(~ depth) +
  labs(
    x = "Temperature",
    y = "Frequency",
    title = "Temp of Burned Control Plot 1"
  ) +
  theme(
    strip.text = element_text(size = 12, face = "italic"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold")
  )

temp.sv |>
  filter(plot == "BC2") |>
  ggplot(aes(x = num))+
  geom_histogram(binwidth = 0.5)+
  facet_wrap(~ depth) +
  labs(
    x = "Temperature",
    y = "Frequency",
    title = "Temp of Burned Control Plot 2")+
  theme(
    strip.text = element_text(size = 12, face = "italic"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold")
  )

temp.sv |>
  filter(plot == "BC3") |>
  ggplot(aes(x = num))+
  geom_histogram(binwidth = 0.5)+
  facet_wrap(~ depth) +
  labs(
    x = "Temperature",
    y = "Frequency",
    title = "Temp of Burned Control Plot 3")+
  theme(
    strip.text = element_text(size = 12, face = "italic"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold")
  )

temp.sv |>
  filter(plot == "BC4") |>
  ggplot(aes(x = num))+
  geom_histogram(binwidth = 0.5)+
  facet_wrap(~ depth) +
  labs(
    x = "Temperature",
    y = "Frequency",
    title = "Temp of Burned Control Plot 4")+
  theme(
    strip.text = element_text(size = 12, face = "italic"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold")
  )

temp.sv |>
  filter(plot == "BH2") |>
  ggplot(aes(x = num))+
  geom_histogram(binwidth = 0.5)+
  facet_wrap(~ depth) +
  labs(
    x = "Temperature",
    y = "Frequency",
    title = "Temp of Burned Heat Plot 2"
  ) +
  theme(
    strip.text = element_text(size = 12, face = "italic"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold")
  )

temp.sv |>
  filter(plot == "BH3") |>
  ggplot(aes(x = num))+
  geom_histogram(binwidth = 0.5)+
  facet_wrap(~ depth) +
  labs(
    x = "Temperature",
    y = "Frequency",
    title = "Temp of Burned Heat Plot 3"
  ) +
  theme(
    strip.text = element_text(size = 12, face = "italic"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold")
  )

temp.sv |>
  filter(plot == "BH4") |>
  ggplot(aes(x = num))+
  geom_histogram(binwidth = 0.5)+
  facet_wrap(~ depth) +
  labs(
    x = "Temperature",
    y = "Frequency",
    title = "Temp of Burned Heat Plot 4"
  ) +
  theme(
    strip.text = element_text(size = 12, face = "italic"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold")
  )  
#
#
#
#
temp.sv %>%
  filter(plot %in% c("BC1", "BC2", "BC3", "BC4", "BH2", "BH3", "BH4")) %>%
  ggplot(aes(x = num)) +
  geom_histogram(binwidth = 0.5, fill = "darkblue", color = "darkblue")+
  facet_grid(depth ~ plot) +
  labs(
    x = "Temperature (°C)",
    y = "Frequency",
    title = "Burned Plots Temperature Distribution by Depth"
  ) +
  theme_bw() +
  theme(
    legend.text = element_text(size = 12),
    axis.text.x = element_text(size = 10, angle = -45, hjust = 0),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12),
    strip.text = element_text(size = 11, face = "italic")
  )
#
#
#
#
temp.sv |>
  filter(plot %in% c("BC1", "BC2", "BC3", "BC4", "BH2", "BH3", "BH4")) |>
  ggplot(aes(x = num, fill = plot, color = plot)) +
  geom_density(alpha = 0.4) +
  facet_wrap(~ depth) +
  labs(
    x = "Temperature (°C)",
    y = "Density",
    title = "Burned Plots Temperature Distribution",
    fill = "Plot",
    color = "Plot"
  ) +
  theme_bw() +
  theme(
    legend.text = element_text(size = 12),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    strip.text = element_text(size = 11, face = "italic")
  )

temp.sv |>
  filter(plot %in% c("BC1", "BC2", "BH2")) |>
  ggplot(aes(x = num, fill = plot, color = plot)) +
  geom_density(alpha = 0.4) +
  facet_wrap(~ depth) +
  labs(
    x = "Temperature (°C)",
    y = "Density",
    title = "High Slope Burned Plots Temperature Distribution",
    fill = "Plot",
    color = "Plot"
  ) +
  theme_bw() +
  theme(
    legend.text = element_text(size = 12),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    strip.text = element_text(size = 11, face = "italic")
  )


temp.sv |>
  filter(plot %in% c("BC3", "BH3", "BC4", "BH4")) |>
  ggplot(aes(x = num, fill = plot, color = plot)) +
  geom_density(alpha = 0.4)+
  facet_wrap(~ depth) +
  labs(x= "Temperature", y = "Density", title = "Low Slope Burned Plots Temperature Distribution", fill = "Plot", color = "Plot") +
  theme_bw() +
  theme(legend.text = element_text(size = 12), axis.text = element_text(size = 10), axis.title = element_text(size = 12), strip.text = element_text(size = 11, face = "italic"))
#
#
#
#
burnt_avg <- temp.sv %>%
  filter(plot %in% c("BC1", "BC2", "BC3", "BC4", "BH2", "BH3", "BH4")) %>%
  group_by(TIMESTAMP, depth) %>%
  mutate(TIMESTAMP = as.POSIXct(TIMESTAMP, format = "%Y-%m-%d %H:%M:%S", tz = "America/Los_Angeles")) %>%
  summarise(mean_num = mean(num, na.rm = TRUE), .groups = "drop")

burnt_avg |>
  ggplot(aes(x = mean_num)) +
  geom_histogram(binwidth = 0.5, fill = "darkblue", color = "darkblue") +
  facet_wrap(~ depth) +
  labs(
    x = "Temperature (°C)",
    y = "Frequency",
    title = "Burned Plots Average Temperature Distribution by Depth"
  ) +
  theme_bw() +
  theme(
    legend.text = element_text(size = 12),
    axis.text.x = element_text(size = 10, angle = -45, hjust = 0),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12),
    strip.text = element_text(size = 11, face = "italic")
  )

#
#
#
#
#
temp.sv |>
  filter(plot == "BH1") |>
  ggplot(aes(x = num))+
  geom_histogram(binwidth = 0.5)+
  facet_wrap(~ depth) +
  labs(
    x = "Temperature",
    y = "Frequency",
    title = "Temp of Burned Heat Plot 1"
  ) +
  theme(
    strip.text = element_text(size = 12, face = "italic"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold")
  )  
#
#
#
#
burnt_downslope <- temp.sv |>
  filter(plot %in% c("BC3", "BH3", "BC4", "BH4")) |>
  group_by(TIMESTAMP, depth) |>
  summarise(mean_temp = mean(num, na.rm = TRUE), .groups = "drop") |>
  mutate(trt = "Burned Downslope")

burnt_upslope <- temp.sv |>
  filter(plot %in% c("BC1", "BC2", "BH2")) |>
  group_by(TIMESTAMP, depth) |>
  summarise(mean_temp = mean(num, na.rm = TRUE), .groups = "drop") |>
  mutate(trt = "Burned Upslope")

nonburnt_avg <- nonburnt_avg |>
  rename(mean_temp = mean_num) |>
  mutate(trt = "Unburned")

combined_avg <- bind_rows(burnt_downslope, burnt_upslope, nonburnt_avg)

# Figure 7: Overlapping density plots - Burned Downslope vs Burned Upslope vs Unburned
combined_avg %>%
  ggplot(aes(x = mean_temp, fill = trt, color = trt)) +
  geom_density(alpha = 0.4) +
  facet_wrap(~ depth) +
  scale_fill_manual(values = c("Burned Downslope" = "darkred", 
                               "Burned Upslope" = "orange", 
                               "Unburned" = "darkgreen")) +
  scale_color_manual(values = c("Burned Downslope" = "darkred", 
                                "Burned Upslope" = "orange", 
                                "Unburned" = "darkgreen")) +
  labs(
    x = "Temperature (°C)",
    y = "Density",
    title = "Temperature Distribution by Burn Status and Slope Position",
    fill = "Treatment",
    color = "Treatment"
  ) +
  theme_bw() +
  theme(
    legend.text = element_text(size = 12),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    strip.text = element_text(size = 11, face = "italic")
  )
#
#
#
#
combined_avg %>%
  ggplot(aes(x = mean_temp, y = TIMESTAMP, fill = trt, color = trt)) +
  geom_point(shape = 16, size = 1) +
  facet_wrap(depth ~ .) +
  scale_fill_manual(values = c("Burned Downslope" = "darkred", 
                               "Burned Upslope" = "orange", 
                               "Unburned" = "darkgreen")) +
  scale_color_manual(values = c("Burned Downslope" = "darkred", 
                                "Burned Upslope" = "orange", 
                                "Unburned" = "darkgreen")) +
  labs(
    x = "Temperature (°C)",
    y = "Density",
    title = "Temperature Over Time by Burn Status and Slope Position",
    fill = "Treatment",
    color = "Treatment"
  ) +
  theme_bw() +
  theme(
    legend.text = element_text(size = 12),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    strip.text = element_text(size = 11, face = "italic")
  )
#
#
#
#
# Figure 6: Average Burned - Average Nonburned
temp_diff <- burnt_avg |>
  inner_join(nonburnt_avg, by = c("TIMESTAMP", "depth"), suffix = c("_burned", "_nonburned"))
temp_diff <- temp_diff |>
  mutate(diff_burn_nonburn = mean_num_burned - mean_num_nonburned, TIMESTAMP = as.POSIXct(TIMESTAMP, format = "%Y-%m-%d %H:%M:%S", tz = "America/Los_Angeles")) |>
  arrange(TIMESTAMP, depth)

# quick paired plot (keep y-limits -10..10)
p_diff_pp <- temp_diff %>%
  ggplot(aes(x = TIMESTAMP, y = diff_burn_nonburn, group = depth, color = depth)) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.3) +
  facet_grid(depth ~ .) +
  coord_cartesian(ylim = c(-10, 10)) +
  scale_x_datetime(
    date_breaks = "3 months",
    date_labels = "%b"
    ) +
  labs(x = "Date", y = "Average Burned - Average Nonburned", title = "Average Temp Diff (burned vs unburned)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "none")

p_diff_pp
#
#
#
#
#
#HISTOGRAM FOR DISTRIBUTION OF TEMP DIFFERENCE heated - control
library(dplyr)
library(ggplot2)
# keep

tempdiffbymoisture %>%
  filter(PP == 1) %>%
  ggplot(aes(x = diff_H_C)) +
  geom_histogram(binwidth = 0.5, fill = "lightblue", color = "black", alpha = 0.7) +
  facet_wrap(~ depth) +
  labs(
    x = "Temperature Difference (Heated - Control)",
    y = "Frequency",
    title = "Temp Diff (PP = 1)"
  ) +
  theme(
    strip.text = element_text(size = 12, face = "italic"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold")
  )
#
#
#
#
#
#
# Temperature timeseries BH1
t_h_sv |>
  filter(PP == 1) |>
  ggplot(aes(x = TIMESTAMP, y = num)) +
  geom_point(shape = 16, size = 1, color = "red") +
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
  facet_grid(~ depth) +
  ggtitle("Temperature Timeseries Burned Heated Plot 1")

# temp timeseries BC1

t_c_sv |>
  filter(PP == 1) |>
  ggplot(aes(x = TIMESTAMP, y = num)) +
  geom_point(shape = 16, size = 1, color = "blue") +
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
  facet_grid(~ depth) +
  ggtitle("Temperature Timeseries Burned Control Plot 1")

#
#
#
#
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
#
#
#
#
# Partition timeseries into daytime and nighttime windows
temp.sv$hour <- hour(temp.sv$TIMESTAMP)

# Define windows: daytime 6:00-20:00, nighttime 20:00-6:00
temp_day <- temp.sv %>%
  filter(hour >= 6 & hour < 20, !is.na(num), !is.na(depth)) %>%
  mutate(window = "Daytime (6:00-20:00)")

temp_night <- temp.sv %>%
  filter(hour >= 20 | hour < 6, !is.na(num), !is.na(depth)) %>%
  mutate(window = "Nighttime (20:00-6:00)")

# Combine
temp_windows <- rbind(temp_day, temp_night)
temp_windows <- temp_windows |>
  mutate(burnstatus = ifelse(plot %in% c("NB1", "NB2", "NB3", "NB4"), "Unburned", "Burned"))

# Overlapping density plot by depth and treatment
ggplot(temp_windows, aes(x = num, fill = window, color = window)) +
  geom_density(alpha = 0.4, color= NA) +
  facet_grid(depth ~ burnstatus) +
  labs(
    x = "Temperature (°C)",
    y = "Density",
    title = "Baseline Temperature Distribution: Daytime vs Nighttime",
    fill = "Time Window",
    color = "Time Window"
  ) +
  theme_bw() +
  theme(
    legend.text = element_text(size = 12),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    strip.text = element_text(size = 11, face = "italic")
  )

# Optional: summary stats by window, depth, treatment
window_summary <- temp_windows %>%
  group_by(window, depth, burnstatus) %>%
  summarise(
    mean_temp = mean(num, na.rm = TRUE),
    sd_temp = sd(num, na.rm = TRUE),
    max_temp = max(num, na.rm =TRUE),
    n = n(),
    .groups = "drop"
  )

window_summary |>
  print(n=24)
#
#
#
#
#
#
#
#

paired_diff_pp <- temp.sv %>%
  filter(PP %in% 1:8, !is.na(depth), !is.na(PP), !is.na(num)) %>%
  mutate(
    pair = if_else(as.integer(PP) <= 4L, as.integer(PP), as.integer(PP) - 4L),
    side = if_else(as.integer(PP) <= 4L, "burned", "unburned")
  ) %>%
  group_by(TIMESTAMP, depth, pair, side) %>%
  summarise(mean_num = mean(num, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = side, values_from = mean_num) %>%
  # keep only pairs where both burned and unburned exist
  filter(!is.na(burned) & !is.na(unburned)) %>%
  mutate(
    diff_burn_unburn = burned - unburned,
    pair = factor(pair),
    depth = factor(depth)
  ) %>%
  arrange(pair, depth, TIMESTAMP)

# quick paired plot (keep y-limits -10..10)
p_diff_pp <- paired_diff_pp %>%
  ggplot(aes(x = TIMESTAMP, y = diff_burn_unburn, group = pair, color = pair)) +
  geom_line(size = 0.6) +
  geom_point(size = 1) +
  facet_grid(pair ~ depth) +
  coord_cartesian(ylim = c(-10, 10)) +
  labs(x = "Date", y = "Temperature Difference (Burned - Unburned)", title = "Paired Plot Temp Diff (burned vs unburned)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = -45, hjust = 0))

p_diff_pp

#
#
#
#
#pair 1
paired_diff_pp %>%
  filter(pair == 1) %>%
  ggplot(aes(x = diff_burn_unburn)) +
  geom_histogram(binwidth = 0.5, fill = "red", color = "red", alpha = 0.7) +
  facet_wrap(~ depth) +
  coord_cartesian(xlim = c(-5, 5)) +
  labs(
    x = "Temperature Difference (Burned - Unburned)",
    y = "Frequency",
    title = "Temp Diff (PP = 1)"
  ) +
  theme(
    strip.text = element_text(size = 12, face = "italic"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold")
  )
#
#
#
#pair 2
paired_diff_pp %>%
  filter(pair == 2) %>%
  ggplot(aes(x = diff_burn_unburn)) +
  geom_histogram(binwidth = 0.5, fill = "#87AE73", color = "#87AE73", alpha = 0.7) +
  facet_wrap(~ depth) +
  coord_cartesian(xlim = c(-5, 5)) +
  labs(
    x = "Temperature Difference (Burned - Unburned)",
    y = "Frequency",
    title = "Temp Diff (PP = 2)"
  ) +
  theme(
    strip.text = element_text(size = 12, face = "italic"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold")
  )
#
#
#
#pair 3
paired_diff_pp %>%
  filter(pair == 3) %>%
  ggplot(aes(x = diff_burn_unburn)) +
  geom_histogram(binwidth = 0.5, fill = "#54d4ebff", color = "#54d4ebff", alpha = 0.7) +
  facet_wrap(~ depth) +
  coord_cartesian(xlim = c(-5, 5)) +
  labs(
    x = "Temperature Difference (Burned - Unburned)",
    y = "Frequency",
    title = "Temp Diff (PP = 3)"
  ) +
  theme(
    strip.text = element_text(size = 12, face = "italic"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold")
  )
#
#
#
#pair 4
paired_diff_pp %>%
  filter(pair == 4) %>%
  ggplot(aes(x = diff_burn_unburn)) +
  geom_histogram(binwidth = 0.5, fill = "#ab71ecff", color = "#ab71ecff", alpha = 0.7) +
  facet_wrap(~ depth) +
  coord_cartesian(xlim = c(-5, 5)) +
  labs(
    x = "Temperature Difference (Burned - Unburned)",
    y = "Frequency",
    title = "Temp Diff (PP = 4)"
  ) +
  theme(
    strip.text = element_text(size = 12, face = "italic"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold")
  )
#
#
#
#
# Compute paired differences between H and C treatments within the same PP
# pivot so H and C are side-by-side and compute difference
paired_diff_df <- temp.sv %>%
  filter(!is.na(PP) & !is.na(depth) & !is.na(trt)) %>%
  group_by(TIMESTAMP, depth, PP, trt) %>%
  summarise(mean_num = mean(num, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = trt, values_from = mean_num, names_prefix = "mean_") %>%
  # require both mean_H and mean_C to be present for a paired diff
  filter(!is.na(mean_H) & !is.na(mean_C)) %>%
  mutate(
    diff_num = mean_H - mean_C,
    PP = factor(PP),
    depth = factor(depth)
  )

# quick plot (paired diffs)
p_diff <- paired_diff_df %>%
  arrange(PP, depth, TIMESTAMP) %>%
  ggplot(aes(x = TIMESTAMP, y = diff_num, group = PP, color = PP)) +
  geom_line(size = 0.6) +
  geom_point(size = 1) +
  facet_grid(PP ~ depth) +
  coord_cartesian(ylim = c(-10, 10)) +
  labs(x = "Date", y = "Temperature Difference (H - C)", title = "Paired Plot Temperature Difference") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = -45, hjust = 0))

p_diff
#
#
#
#
#
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
moist_compare <- inner_join(burned_summary, unburned_summary, by = c("TIMESTAMP", "depth"))

# Step 2: Reshape from wide to long format
long_df <- moist_compare %>%
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

#
#
#
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
#
#
#
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
#
#
#
#
#
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
  
  #ggsave("C:/Program Files/RStudio/Tilden Park Warming/Graphs/SoilVUE Moisture Plots 1-4.jpg", width=15,height=10)
# ggsave("C:/Program Files/RStudio/Task 5 Microwarming/Plots/Research Paper/SoilVUE Moisture Plots 1-3.pdf.jpg", width=15, height=10)

library(ggplot2)

# Filter the data for PP 3
moisture.sv_PP3 <- subset(moisture.sv, PP == 3)
#
#
#
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
 # ggsave("C:/Program Files/RStudio/Task 5 Microwarming/Plots/Research Paper/SoilVUE Permittivity Plots 1-3.jpg", width=15, height=10)
#
#
#
#
#
#
