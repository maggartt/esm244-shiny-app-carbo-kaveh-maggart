library(here)
library(tidyverse)
library(lubridate)

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