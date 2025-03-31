#Script by Ira Sutherland March 3, 2024. 

#overview: This script processes spatial data related to Kelp habitat delineation 
#from 2020 to 2024. It reads, cleans, and combines data from 
#multiple years into a single dataset. 

#It then calculates areas, performs various spatial analyses, visualizes changes,
#and generates plots to assess the distribution of kelp across different sites 
#over time.


#load packages
library(dplyr)
library(tidyverse)

#spatial data libraries 
library(sf)
library(tmap)

#load data 
setwd("C:\\Users\\ISutherland\\ESSA Dropbox\\d_global\\Projects\\EN2900to2999\\EN2955 - MaPP Kelp State of Knowledge\\Data")

st_layers("MaPP NVI\\Kelp\\2020 NVI Kelp\\Habitat Delineation\\KMKS_Habitat_delineation 2020.gdb")

NVI2020KMKS <- st_read(dsn = "MaPP NVI\\Kelp\\2020 NVI Kelp\\Habitat Delineation\\KMKS_Habitat_delineation 2020.gdb", layer = "Kelp_MASTER", quiet = TRUE) %>% st_transform(crs = 3005)
NVI2020MAMA <- st_read(dsn = "MaPP NVI\\Kelp\\2020 NVI Kelp\\Habitat Delineation\\MAMA_Habitat_delineation 2020.gdb", layer = "Kelp_MASTER", quiet = TRUE) %>% st_transform(crs = 3005) 
NVI2020TLOW <- st_read(dsn = "MaPP NVI\\Kelp\\2020 NVI Kelp\\Habitat Delineation\\TLOW_Habitat_delineation 2020.gdb", layer = "Kelp_MASTER", quiet = TRUE) %>% st_transform(crs = 3005)
NVI2020WKUM <- st_read(dsn = "MaPP NVI\\Kelp\\2020 NVI Kelp\\Habitat Delineation\\WKUM_Habitat_delineation 2020.gdb", layer = "Kelp_MASTER", quiet = TRUE) %>% st_transform(crs = 3005)

NVI2020KMKS %>% colnames()

NVI2021KMKS <- st_read(dsn = "MaPP NVI\\Kelp\\2021 NVI Kelp\\Habitat Delineation\\KMKS_Habitat_delineation_2021.gdb", layer = "Kelp_Albers", quiet = TRUE) %>% st_transform(crs = 3005)
NVI2021MAMA <- st_read(dsn = "MaPP NVI\\Kelp\\2021 NVI Kelp\\Habitat Delineation\\Mama_Habitat_delineation 2021.gdb", layer = "Kelp_Albers", quiet = TRUE) %>% st_transform(crs = 3005)
NVI2021TLOW <- st_read(dsn = "MaPP NVI\\Kelp\\2021 NVI Kelp\\Habitat Delineation\\Tlow_Habitat_delineation.gdb", layer = "Kelp_Albers", quiet = TRUE) %>% st_transform(crs = 3005)
NVI2021WKUM <- st_read(dsn = "MaPP NVI\\Kelp\\2021 NVI Kelp\\Habitat Delineation\\WKUM_Habitat_delineation.gdb", layer = "Kelp_Albers", quiet = TRUE) %>% st_transform(crs = 3005)

NVI2021KMKS %>% colnames()

NVI2022KMKS <- st_read(dsn = "MaPP NVI\\Kelp\\2022 NVI Kelp\\Habitat Delineation\\KMKS_Habitat_delineation_2022.gdb", layer = "Kelp_Albers", quiet = TRUE) %>% st_transform(crs = 3005)
NVI2022MAMA <- st_read(dsn = "MaPP NVI\\Kelp\\2022 NVI Kelp\\Habitat Delineation\\MAMA_Habitat_delineation 2022.gdb", layer = "Kelp_Albers", quiet = TRUE) %>% st_transform(crs = 3005)
NVI2022TLOW <- st_read(dsn = "MaPP NVI\\Kelp\\2022 NVI Kelp\\Habitat Delineation\\TLOW_Habitat_delineation 2022.gdb", layer = "Kelp_Albers", quiet = TRUE) %>% st_transform(crs = 3005)
NVI2022WKUM <- st_read(dsn = "MaPP NVI\\Kelp\\2022 NVI Kelp\\Habitat Delineation\\WKUM_Habitat_delineation 2022.gdb", layer = "Kelp_Albers", quiet = TRUE) %>% st_transform(crs = 3005)

NVI2022KMKS %>% colnames()

NVI2023 <- st_read(dsn = "MaPP NVI\\Kelp\\2023 NVI Kelp\\Nanw_KelpDelineation_2023.gdb", layer = "Kelp_AllNations_2023", quiet = TRUE) %>% st_transform(crs = 3005) 
NVI2024 <- st_read(dsn = "MaPP NVI\\Kelp\\2024 NVI Kelp\\Nanw_KelpDelineation_2024.gdb", layer = "Kelp_AllNations_2024", quiet = TRUE) %>% st_transform(crs = 3005)

NVI2023 %>% colnames()
NVI2024 %>% colnames()

#Check crs
NVI2020KMKS %>% st_crs()
NVI2021KMKS %>% st_crs()
NVI2022KMKS %>% st_crs()
NVI2023 %>% st_crs()
NVI2024 %>% st_crs()


#Bind 2020-2022 into single annual layers
NVI2020 <- bind_rows(NVI2020KMKS, NVI2020MAMA, NVI2020TLOW, NVI2020WKUM) %>% mutate(year = 2020)
NVI2021 <- bind_rows(NVI2021KMKS, NVI2021MAMA, NVI2021TLOW, NVI2021WKUM) %>% mutate(year = 2021)
NVI2022 <- bind_rows(NVI2022KMKS, NVI2022MAMA, NVI2022TLOW, NVI2022WKUM) %>% mutate(year = 2022)
NVI2023 <- NVI2023 %>% mutate(year = 2023) %>% rename(SHAPE = Shape)
NVI2024 <- NVI2024 %>% mutate(year = 2024) %>% rename(SHAPE = Shape)

#Bind all years together
NVIKelp <- bind_rows(NVI2020, NVI2021, NVI2022, NVI2023, NVI2024)

#00
#Do some manual data cleaning on the kelp data
#Each of these site years has a problem noted.  
NVIKelp <- NVIKelp %>% 
  
  mutate(siteID = ifelse(is.na(SiteID) & !is.na(Site_ID), Site_ID, SiteID)) %>%
  
  filter(!(siteID == "ML-K06" & year == "2024")) %>% #the only year that focusses only on the northern kelp bed. we either lose this year or both 2021 and 2022. 
  filter(!(siteID == "CR-K02" & year %in% c(2020, 2021))) %>% # these years are for a completely different location (we should assign it a new name?)
  filter(!(siteID == "ML-K12" & year == "2020")) %>% #this year is offset from others. It may be a valid survey. But the resolution is quite poor, so its maybe okay to drop it as opposed to figuring out a way to keep it. 
  filter(!(siteID == "TL-K07")) %>% #this site seems to not have a consistent spatial extent in any years
  filter(!(siteID == "KM-K02" & year == "2022")) #this year is just a tiny polygon, and other years are good
  
table(is.na(NVIKelp$siteID))

#Classify the spatial resolution of each image. This was done manually based on visual inspection of plots below
NVIKelp <- NVIKelp %>%
  mutate(spatResoClass = ifelse(year %in% c("2020", "2021"), "Coarse", #all 2020 and 2021 are coarse
                                
                                #remaining years are fine but with some exceptions visually identified:
                                ifelse(year == "2022" & siteID == "ML-K03", "Medium", 
                                       ifelse(year == "2023" & siteID == "ML-K17", "Coarse", 
                                              ifelse(year == "2022" & siteID == "ML-K14", "Coarse",
                                                     ifelse(year == "2022" & siteID == "KM-K01", "Coarse",
                                                            ifelse(year == "2022" & siteID == "CR-K12", "Medium",
                                                            "Fine")))))))



#make a spatial resolution lookup table for later
NVIKelpLookup <- NVIKelp %>% 
  select(siteID, year, spatResoClass) %>%
  st_drop_geometry() %>%
  distinct()

#Examine polygon distribution. Many small objects.  
#This might reflect spatial scale of kelp and resolution of data capture and processing
#kelp_data_combined <- NVIKelp %>%
 # mutate(area_ha = as.numeric(st_area(SHAPE)) / 10000)   # Convert area to hectares

#hist(kelp_data_combined$area_ha, breaks = 100000, xlim = c(0, .2))


#===================================
#Harmonize the spatial extent across years 
#===================================
# dissolve polygons to simplify geometry
kelpMulti <- NVIKelp %>%
  group_by(siteID, year) %>%
  summarise(.groups = 'drop') 

#Get convex hull (plus a .1m buffer)
hulls <- kelpMulti %>% 
  group_by(siteID, year) %>%
  summarise(SHAPE = st_buffer(st_convex_hull(SHAPE), dist = .1), .groups = 'drop')

#QA: plot some hulls to see it looks as one would expect:
tm_shape(kelpMulti %>% filter(siteID == "ML-K17" & year == "2023")) +
  tm_borders() +
tm_shape(hulls %>% filter(siteID == "ML-K17"& year == "2023")) + 
  tm_borders(col = "red") 

tm_shape(kelpMulti %>% filter(siteID == "TL-K08" & year == "2023")) +
  tm_borders() +
  tm_shape(hulls %>% filter(siteID == "TL-K08"& year == "2023")) + 
  tm_borders(col = "red") 

#Looks okay. 


#------------------
# intersect hulls to identify the minimum extent across sites
#Option 1 returned geos errors. I tried everything! Not working, due to close overlap among some hulls? 
#min_extent <- hulls %>%
  #filter(year %in% c(2020, 2021, 2022, 2023)) %>%
 # group_by(siteID) %>%
  #st_make_valid() %>%
  #summarise(SHAPE = st_union(SHAPE), .groups = 'drop')
  #summarise(SHAPE = st_intersection(st_set_precision(SHAPE, .1)), .groups = 'drop') #%>%
  #mutate(geoType = st_geometry_type(SHAPE)) %>%
  #filter(geoType %in% c("POLYGON"))
  #summarise(SHAPE = st_intersection(st_set_precision(x = SHAPE, precision = 1)), .groups = 'drop')
#QA found some issues with this method. 

#---
#Option 2 (use this): do the intersections year by year (this worked - improve later)

hulls <- hulls %>%
  group_by(siteID) %>%
  mutate(mapNumber = row_number()) #%>%

hulls1only <- hulls %>% group_by(siteID) %>% filter(!any(mapNumber == "2"))
hulls1and2no3 <- hulls %>% group_by(siteID) %>% filter(any(mapNumber == "2") & !any(mapNumber == "3"))
hulls1and2and3no4 <- hulls %>% group_by(siteID) %>% filter(any(mapNumber == "3") & !any(mapNumber == "4"))
hulls1and2and3and4no5 <- hulls %>% group_by(siteID) %>% filter(any(mapNumber == "4") & !any(mapNumber == "5"))
hulls1to5 <- hulls %>% group_by(siteID) %>% filter(any(mapNumber == "5"))

#QA that all are included
checkAllAccounted <- bind_rows(hulls1only, hulls1and2no3, hulls1and2and3no4, hulls1and2and3no4, hulls1and2and3and4no5, hulls1to5)
CheckMis <- setdiff(hulls$siteID, checkAllAccounted$siteID)
hulls %>% filter(siteID %in% c(CheckMis)) #should be empty df. 


hulls1F <- hulls1only #this is final

hulls1and2no3F <- hulls1and2no3[hulls1and2no3$mapNumber == "1",] %>% group_by(siteID) %>% 
  st_intersection(hulls1and2no3[hulls1and2no3$mapNumber == "2",]) 

hulls1and2and3no4_F <- hulls1and2and3no4[hulls1and2and3no4$mapNumber == "1",] %>% group_by(siteID) %>% 
  st_intersection(hulls1and2and3no4[hulls1and2and3no4$mapNumber == "2",]) %>%
  st_intersection(hulls1and2and3no4[hulls1and2and3no4$mapNumber == "3",])

hulls1and2and3and4no5F <- hulls1and2and3and4no5[hulls1and2and3and4no5$mapNumber == "1",] %>% group_by(siteID) %>% 
  st_intersection(hulls1and2and3and4no5[hulls1and2and3and4no5$mapNumber == "2",]) %>%
  st_intersection(hulls1and2and3and4no5[hulls1and2and3and4no5$mapNumber == "3",]) %>%
  st_intersection(hulls1and2and3and4no5[hulls1and2and3and4no5$mapNumber == "4",])

hulls1to5F <- hulls1to5[hulls1to5$mapNumber == "1",] %>% group_by(siteID) %>% 
  st_intersection(hulls1to5[hulls1to5$mapNumber == "2",]) %>%
  st_intersection(hulls1to5[hulls1to5$mapNumber == "3",]) %>%
  st_intersection(hulls1to5[hulls1to5$mapNumber == "4",]) %>%
  st_intersection(hulls1to5[hulls1to5$mapNumber == "5",])

hullsFinal <- 
  bind_rows(hulls1F, hulls1and2no3F, hulls1and2and3no4_F, hulls1and2and3and4no5F, 
            hulls1to5F) %>%
  arrange(siteID, year)

#QA: 
unique(hulls$siteID)
unique(hullsFinal$siteID) #we lost one

setdiff(hulls$siteID, hullsFinal$siteID)
checkID <- setdiff(hulls$siteID, hullsFinal$siteID)
checkmis <- hulls %>% filter(siteID %in% c(checkID) )

#QA: plot data we lost
#Ensure checkmis is blank. If not, it means we might be losing sites along the workflow. 

#QA inspect each site has a single hull 
tm_shape(hullsFinal[1:14,]) + 
  tm_borders(col = "red") +
  #tm_compass(size = 1.5) +
  tm_scalebar(breaks = c(0, 0.01, .02), position = c("bottom", "right")) +
  tm_facets_grid(rows = "siteID")

tm_shape(hullsFinal[15:30,]) + 
  tm_borders(col = "red") +
  #tm_compass(size = 1.5) +
  tm_scalebar(breaks = c(0, 0.01, .02), position = c("bottom", "right")) +
  tm_facets_grid(rows = "siteID")

tm_shape(hullsFinal[31:38,]) + 
  tm_borders(col = "red") +
  #tm_compass(size = 1.5) +
  tm_scalebar(breaks = c(0, 0.01, .02), position = c("bottom", "right")) +
  tm_facets_grid(rows = "siteID")

#=========  

# Perform spatial join (clip) based on siteID
clipped_sites <- NVIKelp %>% 
  group_by(siteID) %>% 
  st_intersection(hullsFinal) %>%
  select(siteID, year, density, species, SHAPE) %>%
  mutate(mapPanel = paste0(siteID, "-", year)) %>%
  arrange(siteID, year)

#The QA below revealed one site not being clipped in one year. 
#Ths one not working. 
tm_shape(clipped_sites %>% filter(siteID == "TL-K08" & year == "2023")) +
  tm_borders() +
  tm_shape(hullsFinal %>% filter(siteID == "TL-K08")) + 
  tm_borders(col = "red") #+
  #tm_shape(hulls %>% filter(siteID == "TL-K08"& year == "2023")) + 
  #tm_borders(col = "red") +
  #tm_shape(hulls %>% filter(siteID == "TL-K08"& year == "2024")) + 
  #tm_borders(col = "red")

#manually fix it here. 
testTLk08 <- NVIKelp %>% 
  filter(siteID == "TL-K08") %>%
  st_intersection(hullsFinal %>% filter( siteID == "TL-K08")) %>%
  select(siteID, year, density, species, SHAPE) %>%
  mutate(mapPanel = paste0(siteID, "-", year))

clipped_sites <- clipped_sites %>%
  filter(!(siteID == "TL-K08" & year == "2023")) %>%
  bind_rows(testTLk08) %>%
  arrange(siteID, year)
  
tm_shape(clipped_sites %>% filter(siteID == "TL-K08" & year == "2023")) +
  tm_borders() #its fixed!

#I noticed this one plotted different hulls for each year, but it turned out to just be that the projection made it look different
tm_shape(hullsFinal %>% filter(siteID == "CR-K12")) + 
  tm_borders(col = "red") +
  tm_shape(clipped_sites %>% filter(siteID == "CR-K12")) +
  tm_borders() +
  tm_facets_grid(columns = "year")



#---
#This one shows as not cropped when plotted. 
  tm_shape(clipped_sites %>% filter(siteID == "ML-K06")) +
  tm_borders() +
  tm_facets_grid(columns = "year") +
  tm_shape(hullsFinal %>% filter(siteID == "ML-K06")) + 
  tm_borders(col = "red")
  
  
#manually fix it here. 
testMLk06 <- NVIKelp %>% 
  filter(siteID == "ML-K06" & year %in% c("2020", "2023")) %>%
  st_intersection(hullsFinal %>% filter(siteID == "ML-K06")) %>%
  select(siteID, year, density, species, SHAPE) %>%
  mutate(mapPanel = paste0(siteID, "-", year))

tm_shape(testMLk06) + tm_borders() + tm_facets_grid(columns = "year")

clipped_sites <- clipped_sites %>%
  filter(!(siteID == "ML-K06" & year %in% c("2020", "2023"))) %>%
  bind_rows(testMLk06) %>%
  arrange(siteID, year)

tm_shape(clipped_sites %>% filter(siteID == "ML-K06")) +
  tm_borders() +
  tm_facets_grid(columns = "year") +
  tm_shape(hullsFinal %>% filter(siteID == "ML-K06")) + 
  tm_borders(col = "red")

#fixed
#===========================
# In this version, I got stuck plotting the empty years. so skip it. 

#This should be the one that works 
#hull_allYears <- min_extent %>%
 # st_drop_geometry() %>%
  #crossing(year = 2020:2024) %>%
  #left_join(min_extent, by = "siteID") %>%
  #st_sf() %>%
  #mutate(mapPanel = paste0(siteID, "_", year)) 

  

#tm_shape(hull_allYears %>% filter(siteID == "KM-K01")) + 
 # tm_borders(col = "red") +  # Border color for hulls
  ##tm_facets(by = "mapPanel", ncol = 5, drop.NA.facets = FALSE) +
  #tm_shape(clipped_sites %>% filter(siteID == "KM-K05")) +
  #tm_borders() +  # Draw borders of polygons for NVIKelpSites
  #tm_polygons(fill = "density") +  # Fill polygons with 'density' (or another attribute)
  #tm_facets_grid(rows = "siteID", columns = "year")+# "mapPanel", ncol = 5)  # Create facets by 'year'
  #tm_scalebar(breaks = c(0, 0.01, .02))


#sf_data_all<- sf_data_all %>% arrange(siteID, year)
siteIDs <- unique(hullsFinal$siteID)

#Visualize non-clipped plots with no hull and no density. 
batch_plot_outline <- function(start, end) {
  tm_shape(NVIKelp %>% filter(siteID %in% siteIDs[start:end])) +
    tm_borders() +  
    #tm_polygons(fill = "#283B1F")+
    #tm_scalebar(breaks = c(0, 0.01, .02), position = c("bottom", "right"), text.size = .5) +
    tm_facets_grid(rows = "siteID", columns = "year")
}

tmap_save(batch_plot_outline(1, 7), "Spatial data repo/Images/siteID_1to7_fullmapNoDensity.png", height = 7)
tmap_save(batch_plot_outline(8, 14), "Spatial data repo/Images/siteID_8to14_fullmapNoDensity.png", height = 7)
tmap_save(batch_plot_outline(15, 21), "Spatial data repo/Images/siteID_15to21_fullmapNoDensity.png", height = 7)
tmap_save(batch_plot_outline(22, 28), "Spatial data repo/Images/siteID_22to28_fullmapNoDensity.png", height = 7)
tmap_save(batch_plot_outline(29, 35), "Spatial data repo/Images/siteID_29to35_fullmapNoDensity.png", height = 7)
tmap_save(batch_plot_outline(36, 42), "Spatial data repo/Images/siteID_36to42_fullmapNoDensity.png", height = 7)

#Visualize non-clipped plots against the hull. 
batch_plot_hull_all <- function(start, end) {
  tm_shape(NVIKelp %>% filter(siteID %in% siteIDs[start:end])) +
    tm_borders() +  
    #tm_polygons(fill = "density")+
    tm_facets_grid(rows = "siteID", columns = "year")+
    tm_shape(hullsFinal %>% filter(siteID %in% siteIDs[start:end])) + 
    tm_borders(col = "red")  
}

tmap_save(batch_plot_hull_all(1, 7), "Spatial data repo/Images/siteID_1to7_fullmapNotCropped.png", height = 7)
tmap_save(batch_plot_hull_all(8, 14), "Spatial data repo/Images/siteID_8to14_fullmapNotCropped.png", height = 7)
tmap_save(batch_plot_hull_all(15, 21), "Spatial data repo/Images/siteID_15to21_fullmapNotCropped.png", height = 7)
tmap_save(batch_plot_hull_all(22, 28), "Spatial data repo/Images/siteID_22to28_fullmapNotCropped.png", height = 7)
tmap_save(batch_plot_hull_all(29, 35), "Spatial data repo/Images/siteID_29to35_fullmapNotCropped.png", height = 7)
tmap_save(batch_plot_hull_all(36, 42), "Spatial data repo/Images/siteID_36to42_fullmapNotCropped.png", height = 7)

#work around to get plot with same dimensions 
workaround <- tm_shape(NVIKelp %>% filter(siteID %in% siteIDs[c(36:39, 1:4)])) +
  tm_borders() +  
  #tm_polygons(fill = "density")+
  tm_facets_grid(rows = "siteID", columns = "year")+
  tm_shape(hullsFinal %>% filter(siteID %in% siteIDs[c(36:39, 1:4)])) + 
  tm_borders(col = "red")  
tmap_save(workaround, "Spatial data repo/Images/siteID_36to42_fullmapNotCropped_workaround.png", height = 7)



#Visualize non-clipped plots with no hull with density
batch_plot_full <- function(start, end) {
  tm_shape(NVIKelp %>% filter(siteID %in% siteIDs[start:end])) +
    tm_borders() +  
    tm_polygons(fill = "density")+
    #tm_scalebar(breaks = c(0, 0.01, .02), position = c("bottom", "right"), text.size = .5) +
    tm_facets_grid(rows = "siteID", columns = "year")
}

tmap_save(batch_plot_full(1, 7), "Spatial data repo/Images/siteID_1to7_fullmap.png", height = 7)
tmap_save(batch_plot_full(8, 14), "Spatial data repo/Images/siteID_8to14_fullmap.png", height = 7)
tmap_save(batch_plot_full(15, 21), "Spatial data repo/Images/siteID_15to21_fullmap.png", height = 7)
tmap_save(batch_plot_full(22, 28), "Spatial data repo/Images/siteID_22to28_fullmap.png", height = 7)
tmap_save(batch_plot_full(29, 35), "Spatial data repo/Images/siteID_29to35_fullmap.png", height = 7)
tmap_save(batch_plot_outline(36, 42), "Spatial data repo/Images/siteID_36to42_fullmap.png", height = 7)

#----
# Plot clipped maps no density
batch_plot_hull_outline <- function(start, end) {
    tm_shape(clipped_sites %>% filter(siteID %in% siteIDs[start:end])) +
    tm_borders() +  
    tm_facets_grid(rows = "siteID", columns = "year")+
    tm_shape(hullsFinal %>% filter(siteID %in% siteIDs[start:end])) + 
    tm_borders(col = "red")  
}

# Save clipped maps 
#QA why ML-K06 is missing its extent and why some are not clipped to the extent on first batch. 
tmap_save(batch_plot_hull_outline(1, 7), "Spatial data repo/Images/siteID_1to7_CroppedNoDensity.png", height = 7)
tmap_save(batch_plot_hull_outline(8, 14), "Spatial data repo/Images/siteID_8to14_CroppedNoDensity.png", height = 7)
tmap_save(batch_plot_hull_outline(15, 21), "Spatial data repo/Images/siteID_15to21_CroppedNoDensity.png", height = 7)
tmap_save(batch_plot_hull_outline(22, 28), "Spatial data repo/Images/siteID_22to28_CroppedNoDensity.png", height = 7)
tmap_save(batch_plot_hull_outline(29, 35), "Spatial data repo/Images/siteID_29to35_CroppedNoDensity.png", height = 7)
tmap_save(batch_plot_hull_outline(36, 42), "Spatial data repo/Images/siteID_36to42_CroppedNoDensity.png", height = 7)


#Plot the maps with only the consistent spatial extent. 
# Plot clipped maps with density
batch_plot_hull <- function(start, end) {
  tm_shape(clipped_sites %>% filter(siteID %in% siteIDs[start:end])) +
    tm_borders() +  
    tm_polygons(fill = "density")+
    tm_facets_grid(rows = "siteID", columns = "year")+
    tm_shape(hullsFinal %>% filter(siteID %in% siteIDs[start:end])) + 
    tm_borders(col = "red")  
}

# Save clipped maps 
#QA why ML-K06 is missing its extent and why some are not clipped to the extent on first batch. 
tmap_save(batch_plot_hull(1, 7), "Spatial data repo/Images/siteID_1to7_Cropped.png", height = 7)
tmap_save(batch_plot_hull(8, 14), "Spatial data repo/Images/siteID_8to14_Cropped.png", height = 7)
tmap_save(batch_plot_hull(15, 21), "Spatial data repo/Images/siteID_15to21_Cropped.png", height = 7)
tmap_save(batch_plot_hull(22, 28), "Spatial data repo/Images/siteID_22to28_Cropped.png", height = 7)
tmap_save(batch_plot_hull(29, 35), "Spatial data repo/Images/siteID_29to35_Cropped.png", height = 7)
tmap_save(batch_plot_hull(36, 42), "Spatial data repo/Images/siteID_36to42_Cropped.png", height = 7)


#======================================
#Finalize table of metrics, and generate plots

# Calculate area and summarize by siteID and year
clipped_sites_area <- clipped_sites %>%
  mutate(area_ha = as.numeric(st_area(SHAPE)) / 10000) %>%  # Convert area to hectares
  st_drop_geometry() %>%
  group_by(year, siteID) %>%
  summarise(area_ha = sum(area_ha), .groups = 'drop')

#join spatial resolution back onto site list. 
NVIKelpLookup <- NVIKelp %>% 
  select(siteID, year, spatResoClass) %>%
  st_drop_geometry() %>%
  distinct()

siteVarsRdy <- clipped_sites_area %>%
  left_join(NVIKelpLookup)

# Plot change in total area per site over time
ggplot() +
  geom_point(siteVarsRdy, mapping = aes(x = year, y = area_ha, color = spatResoClass)) +
  #geom_line(siteVarsRdy, mapping = aes(x = year, y = area_ha)) +
  facet_wrap(~ siteID, scale = "free_y") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 315, vjust = .5, size = 6, hjust = -.1), 
        panel.grid.minor = element_blank()) +
  labs(title = "Change in Cropped Area per SiteID (2020 to 2024)",
       y = "Total Kelp Bed Area (ha)",
       x = "Year")

ggsave(filename = "Spatial data repo/Images/Change in Cropped Area all years.png", width = 21.1, height = 14, units = "cm")


#Calculate area-perimeter ratio
#To do: identify how they developed that metric. 
siteVars <- clipped_sites %>%
  mutate(per_m = st_perimeter(SHAPE), 
         area_m2 = as.numeric(st_area(SHAPE)),
         area_ha = area_m2/10000) %>%
  st_drop_geometry() %>%
  group_by(siteID, year) %>%
  summarize(perimeter_m = as.numeric(sum(per_m)), 
            area_m2 = sum(area_m2), 
            area_ha = sum(area_ha)) %>%
  mutate(areaToPerim_Ratio = perimeter_m / area_ha) %>%
  mutate(year = as.integer(year)) %>%
  left_join(NVIKelpLookup)
  

#write summary to file

write.csv(siteVars, "Spatial data repo/csv summaries/spatial metrics_QAd.csv" )


#=====================================
#Look at change but only within spatial resolution classes
Change_per <- siteVars %>%
  # Step 1: Group by siteID and spatResoClass to ensure we only compare within the same spatial resolution
  group_by(siteID, spatResoClass) %>%
  
  # Step 2: Arrange by year to ensure correct ordering within each group
  arrange(siteID, spatResoClass, year) %>%
  
  # Step 3: Use lag() to get the previous year's values for both area_ha and perimeter_m
  mutate(
    prev_area_ha = lag(area_ha),
    prev_perimeter_m = lag(perimeter_m),
    prev_year = lag(year)
  ) %>%
  
  # Step 4: Calculate the percent change in both area_ha and perimeter_m
  mutate(
    percent_change_area = ifelse(!is.na(prev_area_ha), 
                                 (area_ha - prev_area_ha) / prev_area_ha * 100, 
                                 NA),
    percent_change_perimeter = ifelse(!is.na(prev_perimeter_m), 
                                      (perimeter_m - prev_perimeter_m) / prev_perimeter_m * 100, 
                                      NA)
  ) %>%
  
  # Step 5: Filter out rows where either previous area or perimeter is NA (i.e., no previous year to compare with)
  filter(!is.na(percent_change_area) & !is.na(percent_change_perimeter)) %>%
  
  # Step 6: Output in long format with necessary columns for plotting
  select(siteID, spatResoClass, year, prev_year, percent_change_area, percent_change_perimeter) %>%
  ungroup()

#plot the result
ggplot(Change_per, aes(x = year, y = percent_change_area)) +
  geom_point() +
  geom_line() +
  facet_wrap(~siteID) +
  theme_minimal() +
  geom_hline(yintercept = 0, colour = "black") +
  geom_hline(yintercept = c(-20, 20), colour = "red", linetype = "dashed") +
  ylab("Kelp area change from previous measurement year (%)") +
  ylim(c(-125, 125)) +
  theme(axis.text.x = element_text(angle = 315, vjust = .5, size = 6, hjust = -.1), 
        panel.grid.minor = element_blank()) 

ggsave(filename = "Spatial data repo/Images/Percent Change in Cropped Area.png", width = 21.1, height = 14, units = "cm")


ggplot(Change_per, aes(x = year, y = percent_change_perimeter)) +
  geom_point() +
  geom_line() +
  facet_wrap(~siteID) +
  theme_minimal() +
  geom_hline(yintercept = 0, colour = "black") +
  geom_hline(yintercept = c(-20, 20), colour = "red", linetype = "dashed") +
  ylab("Change in the area to perimeter ratio (%)") +
  ylim(c(-225, 225)) +
  theme(axis.text.x = element_text(angle = 315, vjust = .5, size = 6, hjust = -.1), 
        panel.grid.minor = element_blank())

ggsave(filename = "Spatial data repo/Images/Percent Change in Area to Perim ratio.png", width = 21.1, height = 14, units = "cm")



#===========================
# Plot number of sites per year
#This is old. Do we want to do this on the old or new data, or both? 
kelp_sites_per_year <- kelp_data_combined %>%
  group_by(siteID, year) %>%
  summarize() %>%
  st_drop_geometry()

site_year_count <- as.data.frame(table(kelp_sites_per_year$siteID)) %>%
  rename(`Site ID` = Var1, `Years (#)` = Freq)

ggplot(site_year_count, aes(x = `Years (#)`)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(y = "Frequency (# kelp bed monitoring sites)") +
  theme_minimal()

ggsave(filename = "Spatial data repo/Images/Freq_sites_by_years.png", width = 11.1, height = 10, units = "cm")

