## --- MaPP Kelp Monitoring Program ---
## Reading in temperature and salinity data from lightstations in MaPP region
## Author: Brenton Twist
## Date Created: 2025-02-14  
#-------------------------------------------------


# This script contains code for reading and tidying data from active BC lightstations
# in the MaPP region for the kelp monitoring program 


######################
#   Load packages   ##
######################

library(tidyverse)
library(lubridate)


##################
#   Read data   ##
##################

#Get all the files in the lightstation folders
relativewd <- "./data/lightstations"
filenames <- list.files(path=relativewd,pattern=".csv")
filenames

# Now the relative directory for all files in folder
ligthstation_file_dir<-paste0(relativewd,"/",filenames)

#Reads all the all csv files into a list of data frames
df_list <- lapply(ligthstation_file_dir, read_csv, show_col_types = FALSE) #uses read_csv

# bind all rows together
lightstation_data<-dplyr::bind_rows(df_list)

str(lightstation_data)

# Missing values are represented with 999.9- replace these with NA

lightstation_data <- lightstation_data %>%
  mutate(across(c(Salinity, Temperature), ~ na_if(., 999.9)))


# add year and month to data set
lightstation_data <- lightstation_data %>%
  mutate(Year = year(Date ),
         Month = month(Date )) %>%
  relocate (Year, Month,.after = Date )

# Data should already be quality controlled coming in


###########################
#   Plotting timeseries   ##
###########################
### Make a plot of time series of temperature and salinity values

#Create a group varaible that can be used to show gaps
lightstation_data <- lightstation_data %>%
  arrange(Site, Date) %>%
  group_by(Site) %>%
  mutate(
    date_diff = c(NA, diff(Date)),  # Calculate date difference for each row
    group = cumsum(ifelse(is.na(date_diff) | date_diff > 10, 1, 0))  # Split groups when gap > 10 days
  ) %>%
  ungroup()

# Get data in logical order
lightstation_data <- lightstation_data %>%
  mutate(MaPP_region = factor(MaPP_region, 
                              levels = c("Haida Gwaii", "North Coast", "Central Coast", "North Vancouver Island", "Outside")))


lightstation_data_2019_24<- lightstation_data%>%
  filter(MaPP_region!="Outside")%>%
  filter(Year>=2019)


p1<-ggplot(lightstation_data_2019_24, aes(x = Date, y = Temperature, group = group)) +
  geom_line() +  # Line plot
  facet_wrap(MaPP_region~ Site,  ncol = 1) +  # Facet by Location, 1 column
  geom_hline(yintercept = 18, color = "red", linetype = "solid", size = 0.5) +  # Add red line at 18°C negative effects
  # Add dotted grey lines at 12°C and 16°C & shade- optimal growth see Pontier et al. 2024 discusion
  geom_hline(yintercept = 12, color = "grey", linetype = "dashed", size = 0.5) +
  geom_hline(yintercept = 16, color = "grey", linetype = "dashed", size = 0.5) +
  geom_ribbon(  aes(x = Date, ymin = 12, ymax = 16), 
              fill = "grey80", alpha = 0.3)+
  theme_classic(base_size = 16) +  
  theme(panel.grid.major=element_line())+ # addmajor gridlines back in
  labs(
    title = "Lightstation",
    x = "Date",
    y = "Temperature (°C)"
  ) +
  coord_cartesian(ylim = c(min(lightstation_data_2019_24$Temperature), 20))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

p1

ggsave(p1, width = 22, height = 30, units = "cm", dpi = 300,
       file= "./figures/Lightstation_temperature_timeseries.png")




p2<-ggplot(lightstation_data_2019_24, aes(x = Date, y = Salinity, group = group)) +
  geom_line() +  # Line plot
  facet_wrap(MaPP_region~ Site,  ncol = 1) +  # Facet by Location, 1 column
  theme_classic(base_size = 16) +   
  theme(panel.grid.major=element_line())+ # addmajor gridlines back in
  labs(
    title = "Lightstation- Salinity Time Series",
    x = "Date",
    y = "Salinity (PSU)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

p2

ggsave(p2, width = 22, height = 30, units = "cm", dpi = 300,
       file= "./figures/Lightstation_salinity_timeseries.png")


write.csv(lightstation_data_2019_24,"./output/MaPP_lightstation_temperature_salinity_data_2019-2024.csv")

# Save data with rest of R data
load(file="./data/MaPP_temperature_data_summary.Rdata")
save(lightstation_data_2019_24,ctd_data_nvi,ctd_nvi_summary_data,
     file="./data/MaPP_temperature_data_summary.Rdata")
