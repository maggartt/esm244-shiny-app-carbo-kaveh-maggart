library(stringr)
library(tidyverse)
library(usmap)


# Saving California Counties spatial data as dataframe from usmap package:
map <- us_map("counties",
              include = c("CA")) %>% 
  mutate(county = str_remove_all(county, " County")) 

# Preparing my dataframe with the data to represent,
# and adjusting the variable "county" so it matches "county" in "map" df:
df1 <- df %>% 
  mutate(county = str_to_title(gm_gis_county)) %>% 
  filter(date == "2015-01-15") %>% 
  filter(gm_chemical_name == "Potassium")

# Merging (joining) both dataframes by the shared "county" variable:
mapdata <- left_join(map,df1,"county")

# Representing map:
ggplot(mapdata,aes(x=x,y=y,group=group)) +
  geom_polygon(data=mapdata,aes(x=x,y=y,group=group),color="black",fill="grey88",size = 0.2) +
  geom_polygon(aes(fill=mean_gm_result)) +
  coord_fixed(ratio = 1) 



## Tmap test


tm_shape(shp = combined_sf_shiny) +
  tm_borders(col = 'gray') +
  tm_fill(col = 'mean_gm_result',
          title = "Mean Contaminant Concentration",
          style = 'cont',
          popup.vars = c("Population in Poverty (2019)"="povall_2019","Percent of Population in Poverty (2019)"="pctpovall_2019"),
          popup.format = list()) 