# install.packages("rasterVis")
# install.packages("ncdf4")

library(terra)
library(rasterVis)
library(raster)
library(ggmap)
library(tidyverse)
library(knitr)
library(sf)
# New Packages
library(ncdf4) # to import data from netcdf format

# Create a folder to hold the downloaded data
dir.create("data",showWarnings = F)

lulc_url="https://github.com/adammwilson/DataScienceData/blob/master/inst/extdata/appeears/MCD12Q1.051_aid0001.nc?raw=true"
lst_url="https://github.com/adammwilson/DataScienceData/blob/master/inst/extdata/appeears/MOD11A2.006_aid0001.nc?raw=true"

# download data
download.file(lulc_url,destfile="data/MCD12Q1.051_aid0001.nc", mode="wb")
download.file(lst_url,destfile="data/MOD11A2.006_aid0001.nc", mode="wb")

# load data into R
lulc=rast("data/MCD12Q1.051_aid0001.nc",subds="Land_Cover_Type_1")
lst=rast("data/MOD11A2.006_aid0001.nc",subds="LST_Day_1km")

# Explore LULC data
plot(lulc)

lulc=lulc[[13]]
plot(lulc)

# Assign land cover classes from MODIS website
Land_Cover_Type_1 = c(
  Water = 0, 
  `Evergreen Needleleaf forest` = 1, 
  `Evergreen Broadleaf forest` = 2,
  `Deciduous Needleleaf forest` = 3, 
  `Deciduous Broadleaf forest` = 4,
  `Mixed forest` = 5, 
  `Closed shrublands` = 6,
  `Open shrublands` = 7,
  `Woody savannas` = 8, 
  Savannas = 9,
  Grasslands = 10,
  `Permanent wetlands` = 11, 
  Croplands = 12,
  `Urban & built-up` = 13,
  `Cropland/Natural vegetation mosaic` = 14, 
  `Snow & ice` = 15,
  `Barren/Sparsely vegetated` = 16, 
  Unclassified = 254,
  NoDataFill = 255)

lcd=data.frame(
  ID=Land_Cover_Type_1,
  landcover=names(Land_Cover_Type_1),
  col=c("#000080","#008000","#00FF00", "#99CC00","#99FF99", "#339966", "#993366", "#FFCC99", 
        "#CCFFCC", "#FFCC00", "#FF9900", "#006699", "#FFFF00", "#FF0000", "#999966", "#FFFFFF", 
        "#808080", "#000000", "#000000"),
  stringsAsFactors = F)
# colors from https://lpdaac.usgs.gov/about/news_archive/modisterra_land_cover_types_yearly_l3_global_005deg_cmg_mod12c1
kable(head(lcd))

# convert to raster (easy)
lulc=as.factor(lulc)

# update the RAT with a left join
levels(lulc)=left_join(levels(lulc)[[1]],lcd)[-1,]
activeCat(lulc)=1

# plot it
gplot(lulc)+
  geom_raster(aes(fill=as.factor(value)))+
  scale_fill_manual(values=setNames(lcd$col,lcd$ID),
                    labels=lcd$landcover,
                    breaks=lcd$ID,
                    name="Landcover Type")+
  coord_equal()+
  theme(legend.position = "right")+
  guides(fill=guide_legend(ncol=1,byrow=TRUE))

# LST
plot(lst[[1:12]])

# Convert K to Degrees C
scoff(lst)=cbind(0.02,-273.15)
plot(lst[[1:10]])

# MOD11A2 Quality Control
lstqc=rast("data/MOD11A2.006_aid0001.nc",subds="QC_Day")
plot(lstqc[[1:2]])

values(lstqc[[1:2]])%>%table()

intToBits(65)

intToBits(65)[1:8]

as.integer(intToBits(65)[1:8])

rev(as.integer(intToBits(65)[1:8]))

# Filter the the lst data using the QC data
# set up data frame to hold all combinations
QC_Data <- data.frame(Integer_Value = 0:255,
                      Bit7 = NA, Bit6 = NA, Bit5 = NA, Bit4 = NA,
                      Bit3 = NA, Bit2 = NA, Bit1 = NA, Bit0 = NA,
                      QA_word1 = NA, QA_word2 = NA, QA_word3 = NA,
                      QA_word4 = NA)

for(i in QC_Data$Integer_Value){
  AsInt <- as.integer(intToBits(i)[1:8])
  QC_Data[i+1,2:9]<- AsInt[8:1]
}

QC_Data$QA_word1[QC_Data$Bit1 == 0 & QC_Data$Bit0==0] <- "LST GOOD"
QC_Data$QA_word1[QC_Data$Bit1 == 0 & QC_Data$Bit0==1] <- "LST Produced,Other Quality"
QC_Data$QA_word1[QC_Data$Bit1 == 1 & QC_Data$Bit0==0] <- "No Pixel,clouds"
QC_Data$QA_word1[QC_Data$Bit1 == 1 & QC_Data$Bit0==1] <- "No Pixel, Other QA"

QC_Data$QA_word2[QC_Data$Bit3 == 0 & QC_Data$Bit2==0] <- "Good Data"
QC_Data$QA_word2[QC_Data$Bit3 == 0 & QC_Data$Bit2==1] <- "Other Quality"
QC_Data$QA_word2[QC_Data$Bit3 == 1 & QC_Data$Bit2==0] <- "TBD"
QC_Data$QA_word2[QC_Data$Bit3 == 1 & QC_Data$Bit2==1] <- "TBD"

QC_Data$QA_word3[QC_Data$Bit5 == 0 & QC_Data$Bit4==0] <- "Emiss Error <= .01"
QC_Data$QA_word3[QC_Data$Bit5 == 0 & QC_Data$Bit4==1] <- "Emiss Err >.01 <=.02"
QC_Data$QA_word3[QC_Data$Bit5 == 1 & QC_Data$Bit4==0] <- "Emiss Err >.02 <=.04"
QC_Data$QA_word3[QC_Data$Bit5 == 1 & QC_Data$Bit4==1] <- "Emiss Err > .04"

QC_Data$QA_word4[QC_Data$Bit7 == 0 & QC_Data$Bit6==0] <- "LST Err <= 1"
QC_Data$QA_word4[QC_Data$Bit7 == 0 & QC_Data$Bit6==1] <- "LST Err > 2 LST Err <= 3"
QC_Data$QA_word4[QC_Data$Bit7 == 1 & QC_Data$Bit6==0] <- "LST Err > 1 LST Err <= 2"
QC_Data$QA_word4[QC_Data$Bit7 == 1 & QC_Data$Bit6==1] <- "LST Err > 4"
kable(head(QC_Data))

# Select which QC Levels to keep
keep=QC_Data[QC_Data$Bit1 == 0,]
keepvals=unique(keep$Integer_Value)
keepvals

# See how many observations will be dropped?
qcvals=table(values(lstqc))  # this takes a minute or two


QC_Data%>%
  dplyr::select(everything(),-contains("Bit"))%>%
  mutate(Var1=as.character(Integer_Value),
         keep=Integer_Value%in%keepvals)%>%
  inner_join(data.frame(qcvals))%>%
  kable()

# Filter the LST Data keeping only keepvals
# Make logical flag to use for mask
lstkeep=app(lstqc,function(x) x%in%keepvals)

# Plot the mask
gplot(lstkeep[[4:8]])+
  geom_raster(aes(fill=as.factor(value)))+
  facet_grid(variable~.)+
  scale_fill_manual(values=c("blue","red"),name="Keep")+
  coord_equal()+
  theme(legend.position = "bottom")

# Mask the lst data using the QC data and overwrite the original data.
lst=mask(lst,mask=lstkeep,maskval=0)

# Part 1: Extract timeseries for a point
lw <- data.frame(x = -78.791547, y = 43.007211) %>%
  st_as_sf(coords = c("x", "y"), crs = 4326)

# Project point
raster_crs <- st_crs(lst)  # Get the CRS of the raster
lw_transformed <- st_transform(lw, raster_crs)  # Transform point to the raster CRS

# Extract Lst Values to point
lst_values <- extract(lst, lw_transformed, buffer = 1000, fun = mean, na.rm = TRUE)

# Clean data
lst_values_transposed <- t(lst_values)  # Transpose the data to a long vector
lst_values_clean <- lst_values_transposed[-1]  # Remove the first column (ID)

# Extract data to layer
dates <- time(lst)  # Extract the dates associated with each layer of the raster

# Combine dates and LST values into a data frame
data <- data.frame(Date = as.Date(dates), LST = lst_values_clean)

# Plot the data with ggplot2
ggplot(data, aes(x = Date, y = LST)) +
  geom_point() +
  geom_smooth(method = "loess", span = 0.3, n = 300) +
  theme_minimal() +
  labs(title = "LST Values Over Time",
       x = "Date",
       y = "LST (°C)")

# Part 2: Summarize weekly data to monthly climatologies
# Extract numeric month values by removing non-numeric characters (e.g., "m_" or "X")
month_numbers <- as.numeric(gsub("[^0-9]", "", names(lst_month)))

# Check if the extraction worked
print(month_numbers)

# Set the names to month names (ensure that the numeric values correspond correctly)
names(lst_month) <- month.name[month_numbers]

# Check the result
print(names(lst_month))

# Plot the map for each month with `gplot()` in the RasterVis Package.
gplot(lst_month) + 
  geom_tile(aes(fill = value)) + 
  facet_wrap(~ variable, ncol = 3) +  # Adjust the number of columns as needed
  labs(title = "Monthly Mean Land Surface Temperature") +
  theme_minimal() +
  theme(strip.text = element_text(size = 10))
# Calculate the monthly mean for the entire image
mean_lst_month <- global(lst_month, fun = mean, na.rm = TRUE)

# Print the result
print(mean_lst_month)

# Part 3: Summarize Land Surface Temperature by Land Cover
# Resample LULC to LST grid using nearest neighbor method
lulc2 <- resample(lulc, lst_month, method = "near")

# Extract values and combine into a data frame
lcds1 <- cbind.data.frame(
  values(lst_month),
  ID = values(lulc2[[1]])
) %>%
  na.omit()

lcds1 %>% 
  filter(landcover%in%c("Urban & built-up","Deciduous Broadleaf forest")) %>% 
  ggplot(aes(y=value,x=month))+
  facet_wrap(~landcover)+
  geom_point(alpha=.5,position="jitter")+
  geom_smooth()+
  geom_violin(alpha=.5,col="red",scale = "width",position="dodge")+
  theme(axis.text.x=element_text(angle=90, hjust=1))+
  ylab("Monthly Mean Land Surface Temperature (C)")+
  xlab("Month")+
  ggtitle("Land Surface Temperature in Urban and Forest areas in Buffalo, NY")

