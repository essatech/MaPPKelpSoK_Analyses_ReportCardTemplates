## --- MaPP Kelp Monitoring Program ---
## Reading in CTD data from kelp forests in NVI MaPP region
## Author: Brenton Twist
## Edited by: Brenton Twist
## Date Created: 2025-02-13  
#-------------------------------------------------


# This script contains code for reading and tidying data collected from CTD Loggers
# by the MaPP kelp monitoring program in NVI region


######################
#   Load packages   ##
######################

library(tidyverse)
library(readxl)
library(openxlsx2)
library(lubridate)




##################
#   Read data   ##
##################


#Get all the excel file names in the CTD loggers data file
#NOTE- files are housed offline ind not in github directory- move into this file to read
relativewd<-"./data/ctd_loggers"
filenames <- list.files(path=relativewd,pattern=".xls")
filenames
#(filelocation<-paste0(relativewd,"/",filenames))

# Lets read in the all the data combined that is now in one excel sheet
early_ctd_data<-paste0(relativewd,"/","NVI-data_loggers_2019-2024.xlsx")

# Lets create a function that reads  a sheets from the early data

df_list_sheet <- lapply(readxl::excel_sheets(early_ctd_data),  
                        function(x) openxlsx2::read_xlsx(early_ctd_data,
                                                         sheet = x,
                                                         skipEmptyRows=TRUE, 
                                                         skipEmptyCols=TRUE)
)

#Replace all white space in collumns names with .
df_list_sheet <-df_list_sheet  %>% 
  map(~rename_with(.,make.names)) # Replaces gaps in names with dots


# add sheet name to DF
names(df_list_sheet) <-readxl::excel_sheets(early_ctd_data)


# Use lapply to find the index of the dataframe containing "Location"- which has the metadata
df_index <- which(sapply(df_list_sheet, function(df) "Location" %in% colnames(df)))[1]


# If an index is found, store the dataframe with "Location" and remove it from the list
if (!is.na(df_index)) {
  df_metadata <- df_list_sheet[[df_index]]
  df_list_sheet <- df_list_sheet[-df_index]
}

# Check the results
str(df_metadata)  # The dataframe with "Location"
str(df_list_sheet)     # The updated list without the dataframe containing "Location"



###ISSUE- Date time is read in as just a date. This is okay as will be averaging values for the day later on

#Bind all the rows into one - may need to convert to characters first
df_list_sheet<- rapply(df_list_sheet,as.character,how="replace") # Convert all to characters for merge

df_sheet <- dplyr::bind_rows(df_list_sheet,.id = "sheet.name")


##OLD CODE- Owen bay now if main excel file
# #Lets read in Owen bay 2024 data
# 
# owenbay<-openxlsx2::read_xlsx(paste0(relativewd,"/","OwenBay_data_2024_QC.xlsx" ),
#                               skipEmptyRows=TRUE, 
#                               skipEmptyCols=TRUE)
# owenbay<-owenbay%>%
#   mutate(sheet.name="S11937") %>% # Give owen bay logger number to merrge meta data
#   mutate_all(as.character) # makes everything character
# 
# #Replace all collumn names so they are similar to other column headings
# owenbay<-owenbay%>%
#   rename_with(~gsub("[[:space:]&()=?°]", ".", .)) %>%
#   rename_with(~gsub("/", ".", .))
# 
# 
# 
# 
# df_sheet <- dplyr::bind_rows(df_sheet,owenbay)

# add metadata
str(df_metadata) 
str(df_sheet)

df_sheet<- df_sheet %>% rename( "Star.ODDI.logger"= "sheet.name")

ctd_data_nvi_raw<- right_join(df_metadata,df_sheet, by=c("Star.ODDI.logger"))

#automatically detect column type after reading to character
ctd_data_nvi_raw<-readr::type_convert(ctd_data_nvi_raw)

#Get year and day as seperate variables
ctd_data_nvi_raw<- ctd_data_nvi_raw %>%
  mutate(Year = year(Date...Time ),
       Month = month(Date...Time )) %>%
  relocate (Year, Month,.after = Date...Time )

# Create new dataset to modify 
ctd_data_nvi <- ctd_data_nvi_raw


#Lets fix up the names of columns in CTD daily data
str(ctd_data_nvi)

ctd_data_nvi<- ctd_data_nvi%>%
  rename(LoggerID= Star.ODDI.logger,
         Date= Date...Time,
         Temp.logger= Temp..C.,
         Salinity.logger= Salinity.psu.,
         Oxygen.logger= CTD.Oxygen..mL.L. )



###########################
#   QC- Error checking   ##
###########################


str(ctd_data_nvi)

## lets look for errors in water temp and salinity


# Create a boxplot with Location on the x-axis for Temp and Sal
p_temp_out<- ggplot(ctd_data_nvi, aes(x = Location, y = Temp.logger)) + 
  geom_boxplot(outlier.colour = "red", outlier.size = 1) +  # Outliers in red
  labs(title = "Boxplot of Temperature range by Location", 
       x = "Location", y = "Temperature (deg C)") +
  theme_minimal() +
  #scale_y_continuous(breaks = seq(0,  30, by = 2))  +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  

p_temp_out


p_sal_out<- ggplot(ctd_data_nvi, aes(x = Location, y = Salinity.logger)) + 
  geom_boxplot(outlier.colour = "red", outlier.size = 1) +  # Outliers in red
  labs(title = "Boxplot of Temperature range by Location", 
       x = "Location", y = "Salinity (PSU)") +
  theme_minimal() +
  scale_y_continuous(breaks = seq(0,  40, by = 2)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  

p_sal_out
# need to look into lowere salinity values and when to remove Below 1 is probably an outlier as Freshwater is usually above 0.5


# Something wrong with Chatham Ch. Salinity  as it is above 32 remove salinity for whole sites
#Remove only salinity at this site
ctd_data_nvi<- ctd_data_nvi%>%
  mutate(Salinity.logger=case_when(Location== "Chatham Ch."~NA,
                                TRUE~Salinity.logger))
  
  
  
# Remove any temperature values above 18 degrees and other associated data based on on plot as likely outliers

max_temp<-20.0 #change value here to have max temperature value

ctd_data_nvi<- ctd_data_nvi %>%
  filter(Temp.logger<max_temp & Temp.logger>4) # min value of 4 degrees

ctd_data_nvi<- ctd_data_nvi %>%
  filter( Salinity.logger>1) # remove salinity values below 1, as freshwater is really below 1


# Lets average values across day- removes outlier values that are not reflective daily exposures

str(ctd_data_nvi)

ctd_data_nvi <- ctd_data_nvi %>%
  group_by(across(-c(Temp.logger, Salinity.logger, Conduct.mS.cm., Sound.Velocity.m.sec.,Oxygen.logger))) %>%
  summarise(
    Temp.logger= mean(Temp.logger, na.rm = TRUE),
    Salinity.logger = mean(Salinity.logger, na.rm = TRUE),
    Conduct.mS.cm. = mean(Conduct.mS.cm., na.rm = TRUE),
    Sound.Velocity.m.sec. = mean(Sound.Velocity.m.sec., na.rm = TRUE),
    Oxygen.logger = mean(Oxygen.logger, na.rm = TRUE),
    .groups = 'drop'  
  )



### Make a plot of time series of temperature and salinity values

#Create a group variable that can be used to show gaps
ctd_data_nvi <- ctd_data_nvi %>%
  arrange(Location, Date) %>%
  group_by(Location) %>%
  mutate(
    date_diff = c(NA, diff(Date)),  # Calculate date difference for each row
    group = cumsum(ifelse(is.na(date_diff) | date_diff > 10, 1, 0))  # Split groups when gap > 10 days
  ) %>%
  ungroup()

ctd_data_nvi <- ctd_data_nvi %>%
  mutate(Location=as.factor(Location))

# Select the top 5 locations with the most rows
top_locations <- ctd_data_nvi %>%
  group_by(Location) %>%
  count() %>%
  arrange(desc(n)) %>%
  ungroup() %>%  # Ungroup the data
  slice_head(n = 8) %>%
  pull(Location)  # Extract the top 5 Location names

# Filter the original dataset to retain only rows for these top 5 locations
ctd_data_nvi_red <- ctd_data_nvi %>%
  filter(Location %in% top_locations)



p_temp_time<-ggplot(ctd_data_nvi, aes(x = Date, y = Temp.logger, group = group)) +
  geom_line() +  # Line plot
  facet_wrap(~ Location,  ncol = 2) +  # Facet by Location, 1 column
  theme_classic(base_size = 14) +   
  theme(panel.grid.major=element_line())+ # adds major gridlines back in
  labs(
    #title = "Temperature Time Series by Location",
    x = "Date",
    y = "Temperature (°C)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

p_temp_time

ggsave(p_temp_time, width = 30, height = 20, units = "cm", dpi = 300,
       file= "./figures/NVI_CTD_Temperature_timeseries.png")


p_temp_time2<-ggplot(ctd_data_nvi_red, aes(x = Date, y = Temp.logger, group = group)) +
  geom_line() +  # Line plot
  facet_wrap(~ Location,  ncol = 2) +  # Facet by Location, 1 column
  theme_classic(base_size = 16) +   
  theme(panel.grid.major=element_line())+ # adds major gridlines back in
  labs(
    #title = "CTD Temperature Logger NVI",
    x = "Date",
    y = "Temperature (°C)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

p_temp_time2

ggsave(p_temp_time2, width = 22, height = 15, units = "cm", dpi = 300,
       file= "./figures/NVI_CTD_Temperature_timeseries_reduced_top8.png")






p_sal_time<-ggplot(ctd_data_nvi, aes(x = Date, y = Salinity.logger, group = group)) +
  geom_line() +  # Line plot
  facet_wrap(~ Location,  ncol = 2) +  # Facet by Location, 1 column
  theme_classic(base_size = 14) +   
  theme(
    panel.grid.major = element_line(color = "gray", size = 0.5),  # Major gridlines
    panel.grid.minor = element_line(color = "lightgray", size = 0.25)  # Minor gridlines
  ) + 
  labs(
    title = "Salinity Time Series by Location",
    x = "Date",
    y = "Salinity (PSU)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

p_sal_time

ggsave(p_sal_time, width = 30, height = 15, units = "cm", dpi = 300,
       file= "./figures/NVI_CTD_Salinity_timeseries.png")



p_sal_time2<-ggplot(ctd_data_nvi_red, aes(x = Date, y = Salinity.logger, group = group)) +
  geom_line() +  # Line plot
  facet_wrap(~ Location,  ncol = 2) +  # Facet by Location, 1 column
  theme_classic(base_size = 16) +   
  theme(
    panel.grid.major = element_line(color = "gray", size = 0.5),  # Major gridlines
    panel.grid.minor = element_line(color = "lightgray", size = 0.25)  # Minor gridlines
  ) + 
  labs(
   # title = "Salinity Time Series by Location",
    x = "Date",
    y = "Salinity (PSU)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

p_sal_time2

ggsave(p_sal_time2, width = 22, height = 15, units = "cm", dpi = 300,
       file= "./figures/NVI_CTD_Salinity_timeseries_reduced_top8.png")



#Calculate a summary by year for site
ctd_nvi_summary_data <- ctd_data_nvi %>%
  group_by(Location, Year) %>%
  summarise(
    first_month = first(Month),  # First month within the group
    last_month = last(Month),   # Last month within the group
    Temp_mean = mean(Temp..C., na.rm = TRUE),
    Temp_med = median(Temp..C., na.rm = TRUE),
    Temp_max = max(Temp..C., na.rm = TRUE),  # Maximum value
    Temp_95 = quantile(Temp..C., 0.95, na.rm = TRUE),  # 95th percentile
    Temp_5 = quantile(Temp..C., 0.05, na.rm = TRUE),   # 5th percentile
    Temp_min = min(Temp..C., na.rm = TRUE),  # Minimum value
    Sal_mean = mean(Salinity.psu., na.rm = TRUE),
    Sal_med = median(Salinity.psu., na.rm = TRUE),
    Sal_max = max(Salinity.psu., na.rm = TRUE),  # Maximum value
    Sal_95 = quantile(Salinity.psu., 0.95, na.rm = TRUE),  # 95th percentile
    Sal_5 = quantile(Salinity.psu., 0.05, na.rm = TRUE),   # 5th percentile
    Sal_min = min(Salinity.psu., na.rm = TRUE),  # Minimum value
    .groups = 'drop'  
  )




# Export data
write.csv(ctd_nvi_summary_data,"./output/NVI_CTD_Data_logger_summary.csv")

write.csv(ctd_data_nvi, "./output/NVI_CTD_Data_logger_daily_averages.csv")

save(ctd_data_nvi,ctd_nvi_summary_data,file="./data/MaPP_temperature_data_summary.Rdata")





## Lets compare to Hakia data

hakai_temp<- read.csv("./data/ctd_loggers/Hakai_nearshore-temperature-all.csv")
str(hakai_temp)
hakai_temp_sites<- hakai_temp%>%
  select(niceSiteNames, station,survey, depth_m_cd ,latitude_dd,longitude_dd)%>% 
  distinct()

hakai_temp_red<-hakai_temp %>%
  filter(station=="Quadra Fucus North")
  
