## This script is to run practice examples
## for creating figures in ggplot

## plots include histograms, timeseries, heatmaps, and rasters

# clear your global environment
remove(list=ls())

# import packages
library(ggplot2)
library(reshape2)
library(usmap)

# open the R airquality data as a dataframe
df_full <- airquality

# clean the data by removing NAs
df <- na.omit(df_full)

# reshape the data for plotting ease
df1 <- melt(df, id=c("Month","Day"))

# create a date column
df1$Date = as.Date(paste(df$Month,df$Day,sep="-"),format="%m-%d")

### PLOT THE DATA

### HISTOGRAMS
bin_num = round(sqrt(nrow(df)))

ggplot(df1)+
  geom_histogram(aes(x=value,fill=variable), color = "black", bins = bin_num)+
  facet_wrap(~variable, scales="free")+
  labs(fill="Variable", x="Value",y="Count")+
  theme_minimal()

### TIMESERIES
ggplot(df1)+
  geom_line(aes(x=Date,y=value,color=variable))+
  facet_wrap(~variable, scales="free")+
  labs(color="Variable", y="Value (variable units)")+
  theme_minimal()

### HEAT MAP
# create a correlation matrix
cormat <- round(cor(df[,1:4]),2)
head(cormat)

# reshape the correlation matrix
melted_cormat <- melt(cormat)
head(melted_cormat)

# plot the initial matrix
ggplot(melted_cormat, aes(x=Var1, y=Var2,fill=value)) + 
  geom_tile()+
  labs(x="Variable 1", y="Variable 2",fill="Value")

# find the upper triangle of the correlation matrix
find_upper <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

upper <- find_upper(cormat)
upper

# melt the correlation matrix
# remove the NA values of the lower triangle
melted_cormat <- melt(upper, na.rm = TRUE)

# plot the heatmap using geom_tile()
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile()+
  scale_fill_gradient2(low = "yellow", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab") +
  labs(x="Variable 1", y="Variable 2",fill="Correlation")+
  theme_bw()+ 
  coord_fixed()


### RASTER
# plot a raster atop a map of the US

# generate random raster data for the US

# get the extents of the usa
usa <- map_data("usa")

lats <- seq(from = min(usa$lat), to = max(usa$lat),length.out = 10)
lons <- seq(from = min(usa$long), to = max(usa$long),length.out = 30)

# convert points to a spatial grid
coords <- expand.grid(lats,lons)
coords[,3] <- rnorm(nrow(coords))
colnames(coords) <- c("lat","lon","value")


# plot the map and the raster
ggplot() + 
  geom_polygon(data=usa, aes(x=long, y=lat))+
  geom_raster(data=coords, aes(x=lon, y=lat,
                               fill=value),alpha=0.5)+
  scale_fill_viridis_c()
