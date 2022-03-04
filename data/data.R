library(here)
library(tidyverse)
library(lubridate)
library(janitor)



# Pollutans

well_info <- read_csv(here("data","raw","gama_allwells_2022-01-07.csv"), col_names = TRUE)
test_data <- read_csv(here("data","raw","ddw2015-2020_2022-01-07.csv"), col_names = TRUE)


well_info_short <- well_info %>% 
  select(gm_well_id,gm_gis_county)

test_data_short <- test_data %>% 
  select(gm_well_id,gm_chemical_vvl,gm_chemical_name,gm_result,gm_samp_collection_date,gm_result_units) %>% 
  filter(gm_chemical_name %in% c("Nitrate as N","Potassium","Bicarbonate Alkalinity")) %>% 
  mutate(gm_result_units = str_to_lower(gm_result_units)) %>% 
  mutate(gm_chemical_name=paste0(gm_chemical_name, " (", gm_result_units,")"),.keep="unused") %>% 
  mutate(gm_samp_collection_date = ymd(gm_samp_collection_date)) %>% 
  mutate(year = year(gm_samp_collection_date)) %>% 
  mutate(month = month(gm_samp_collection_date)) %>% 
  mutate(date = ymd(paste(year, month, "15")))

total <- merge(test_data_short,well_info_short,"gm_well_id")

total_summary <- total %>% 
  group_by(gm_gis_county, gm_chemical_name, date, year) %>% 
  summarise(mean_gm_result=mean(gm_result))
  
write.csv(total_summary,here("data","filtered_data.csv"), row.names = FALSE)


# Water level

well_level_data <- read_csv(here("data","raw","measurements.csv"), col_names = TRUE)

well_level_data_short <- well_level_data %>% 
  select(SITE_CODE,MSMT_DATE,GSE_GWE) %>% 
  mutate(year = year(MSMT_DATE)) %>% 
  mutate(month = month(MSMT_DATE)) %>% 
  mutate(date = ymd(paste(year, month, "15")))


well_level_info <- read_csv(here("data","raw","af157380-fb42-4abf-b72a-6f9f98868077.csv"), col_names = TRUE)

well_level_info_short <- well_level_info %>% 
  select(SITE_CODE,LATITUDE,LONGITUDE,COUNTY_NAME,WELL_USE)

merged_level_data <- merge(well_level_data_short,well_level_info_short,"SITE_CODE") %>% 
  clean_names() %>% 
  na.omit()

## Summarizing data of interest for temporal series
level_summary <- merged_level_data %>% 
  group_by(county_name, date, well_use) %>% 
  summarise(mean_gse_gwe=mean(gse_gwe))

### Creating ALL level for County and Use
level_summary_all_counties <- level_summary %>% 
  group_by(date, well_use) %>% 
  summarise(mean_gse_gwe=mean(mean_gse_gwe)) %>% 
  mutate(county_name = "All", .before = "date")

level_summary_all_uses <- level_summary %>% 
  group_by(county_name,date) %>% 
  summarise(mean_gse_gwe=mean(mean_gse_gwe)) %>% 
  mutate(well_use = "All", .before = "mean_gse_gwe")

level_summary_ALL <- level_summary %>% 
  group_by(date) %>% 
  summarise(mean_gse_gwe=mean(mean_gse_gwe)) %>% 
  mutate(county_name = "All", .before = "date") %>% 
  mutate(well_use = "All", .before = "mean_gse_gwe")

### Combining all ALLs

level_summary <- level_summary_ALL %>% 
  rbind(level_summary_all_counties) %>% 
  rbind(level_summary_all_uses) %>% 
  rbind(level_summary) %>% 
  filter(date > ymd("1925-01-01"))


write.csv(level_summary,here("data","waterlevel_series.csv"), row.names = FALSE)


# plot test
ggplot(data=level_summary_ALL,aes(x=date,y=mean_gse_gwe)) +  # CHANGE VARIABLE NAME FOR THE MONTHLY MEAN GROUNDWATER LEVEL VALUES
  geom_ribbon(aes(x = date, ymin = max(mean_gse_gwe), ymax = mean_gse_gwe),fill="dodgerblue2", alpha=0.4) +
  geom_line(color="dodgerblue2", size=0.5) +
  geom_point(size=0.5, color="dodgerblue2") +
  geom_smooth(se=FALSE) +
  labs(y ="Depth to water table (ft)", x = "Year") +
  scale_y_continuous(trans = "reverse")





### Creating ALL for County and contaminant

df <- read_csv(here("data","filtered_data_2000_2020.csv"), col_names = TRUE) %>% 
  mutate(gm_chemical_name = case_when(
    gm_chemical_name == "Bicarbonate Alkalinity (mg/l)" ~ "Bicarbonate Alkalinity",
    gm_chemical_name == "Potassium (mg/l)"  ~ "Potassium",
    gm_chemical_name == "Nitrate as N (mg/l)" ~ "Nitrate"))


### Creating ALL level for County and Use
df_all_counties <- df %>% 
  group_by(gm_chemical_name,date,year) %>% 
  summarise(mean_gm_result=mean(mean_gm_result)) %>% 
  mutate(gm_gis_county = "All", .before = "gm_chemical_name")

### Combining all ALLs

df <- df_all_counties %>% 
  rbind(df)


