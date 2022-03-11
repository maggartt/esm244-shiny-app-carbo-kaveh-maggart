library(shiny)
library(tidyverse)
library(shinythemes)
library(here)
library(usmap)
library(plotly)
library(bslib)
library(tmap)
library(janitor)
library(sf)
library(tsibble)
library(lubridate)
library(feasts)
library(patchwork)

# Data input
# ----------------------

## Time series data
df <- read_csv(here("data","filtered_data_2000_2020.csv"), col_names = TRUE) %>% 
  mutate(gm_chemical_name = case_when(
    gm_chemical_name == "Bicarbonate Alkalinity (mg/l)" ~ "Bicarbonate Alkalinity",
    gm_chemical_name == "Potassium (mg/l)"  ~ "Potassium",
    gm_chemical_name == "Nitrate as N (mg/l)" ~ "Nitrate"))

      ### Creating ALL level for County
      df_all_counties <- df %>% 
        group_by(gm_chemical_name,date,year) %>% 
        summarise(mean_gm_result=mean(mean_gm_result)) %>% 
        mutate(gm_gis_county = "All", .before = "gm_chemical_name")
      ### Combining df and all
      df <- df_all_counties %>% 
        rbind(df)


## Map data
map <- us_map("counties",
              include = c("CA")) %>% 
  mutate(county = str_remove_all(county, " County"))

## Dataframe for Contaminant statistics
df1 <- df %>% 
  mutate(county = str_to_title(gm_gis_county), .keep="unused") %>% 
  group_by(county, gm_chemical_name, year) %>% 
  summarise(mean_gm_result=mean(mean_gm_result)) 

## County Map Data
mapdata <- left_join(map,df1,"county") 

## County shapefiles
county_sf <- st_read(here('data','California_County_Boundaries','CA_Counties_TIGER2016.shp')) %>% 
  clean_names() %>% 
  rename(county = name)

## Poverty data for counties
pov_con_data <- read_csv(here('data','pov_con_data.csv'))

## Wrangling data to combine county shapefiles with the poverty data
combined_sf_shiny <- left_join(county_sf, pov_con_data, by = 'county') %>% 
  group_by(county, gm_chemical_name, povall_2019, pctpovall_2019, year) %>% 
  summarize(mean_gm_result = mean(mean_gm_result))

## Reading in data that contains well information (id, location, depth, etc.)
combined_well_shiny_sf <- rbind(
  read_csv(here('data','combined_well_1.csv')),
  read_csv(here('data','combined_well_2.csv')),
  read_csv(here('data','combined_well_3.csv'))) %>% 
  st_as_sf(coords = c('longitude','latitude'), crs = 4326) %>% 
  filter(mean_water_depth >= 0 & mean_water_depth < 1500) %>% 
  rename(well_type = well_use)

# Reading water level data for temporal series

water_level_ts <- read_csv(here("data","waterlevel_series.csv"), col_names = TRUE) 

# ## Reading in well location data
# well_locations <- read_csv(here('data','well_locations.csv')) %>% 
#   clean_names() %>% 
#   rename(county = county_name) %>% 
#   select(site_code,latitude,longitude,well_use,monitoring_program,county)
# 
# ## Reading in water level measurements 
# water_measurements <- read_csv(here('data','measurements.csv')) %>% 
#   clean_names() %>% 
#   select(site_code,gse_gwe,msmt_date) %>% 
#   mutate(year = year(msmt_date)) %>% 
#   mutate(month = month(msmt_date)) %>% 
#   group_by(site_code,year) %>% 
#   drop_na() 
# 
# ## Combining relevant well data
# combined_well <- left_join(well_locations, water_measurements, by = 'site_code') %>% 
#   filter(year > 1950) %>% 
#   rename(well_type = well_use) %>% 
#   group_by(county,year) %>% 
#   summarize(mean_depth = mean(gse_gwe))
# ----------------------

# Create a custom theme
my_theme <- bs_theme(
  bg = "#FFFFFF",
  fg = "#161853",
  primary = "#EC255A",
  base_font = font_google('Avenir')
)

ui <- fluidPage(theme=my_theme,
                navbarPage(
                  "Groundwater California",  # Title
                  
                  tabPanel("Home",fluid = TRUE, icon = icon("home"),
                           titlePanel(h2("California Groundwater Contamination", align = "center")),
                           fluidRow(column(br(),
                                           br(),
                                           tags$img(src="GAMAlogo.png",width="170"),
                                           br(),
                                           br(),
                                           tags$img(src="WBlogo.png",width="170"),align = "center", width = 3),
                                    
                                    column(
                                      p("California faces immense challenges as local groundwater users work to adhere to the mandates of the Sustainable Groundwater Management Act (SGMA) by the 2040s. With pumping reductions and land fallowing looming, many local government agencies and other stakeholders have to grapple with the increasing demands for water combined with climate change induced droughts and declining recharge rates.
In addition to issues of scarcity, increased demand for agricultural goods has led to a variety of pollutants contaminating California’s water supply. Among these, nitrate, potassium, and phosphate have had detrimental effects on groundwater across the state. These effluents can cause eutrophication in downstream systems, leading to algal blooms and decreased oxygen levels. 
To mitigate contamination, California sets standards for Maximum Contaminant Levels (MCLs) for these chemicals. This application aims to evaluate these threats facing California’s water supply. Through interactive maps, graphs, and other visualizations, this interface allows users to better understand the complex issues surrounding California water quality and scarcity.",
                                        style="text-align:justify;align:center;color:black;background-color:white;padding:15px;border-radius:10px"),
                                      tags$img(src="image.png",
                                               width="90%"),
                                      br(),
                                      br(),
                                      p("Data Citations: 
  \n
  California State Water Resources Control Board, Ground Water - Water Quality Results (2022). Open Data Portal.",
                                        style="text-align:center;color:black"),
                                      p("For more information please check the",em("GAMA Groundwater information system"),"page clicking",
                                        a(href="https://data.ca.gov/dataset/ground-water-water-quality-results/resource/be2d189b-dcb7-4c1c-b881-a52b278cf0a7", "HERE",target="_blank"),
                                        style="text-align:center;color:black"),
                                      p("Data Citations:", style = "text-align: center; color: black; font-weight: bold"), 
                                      p("1. California State Water Resources Control Board, Ground Water - Water Quality Results (2022). Open Data Portal.",
                                        style="text-align:center;color:black"),
                                      p("2. California Department of Water Resources, Periodic Groundwater Level Measurements (2022). California Natural Resources Agency.",
                                        style = "text-align: center; color: black"),
                                      width=6, align = "center"),
                                    
                                    
                                    column(br(),
                                           br(),
                                           br(),
                                           tags$img(src="CAlogo.png",width="200"),
                                           br(),
                                           br(),
                                           br(),
                                           br(),
                                           tags$img(src="RSlogo.png",width="200"),
                                           width=3, align = "center")
                           ),
                           
                           hr(),
                           p(em("Developed by"),br("S. Kaveh, T. Maggart & P. Carbó"),style="text-align:center"),
                           fluidRow(column(DT::dataTableOutput("RawData"),
                                           width = 12)),
                           hr()
                  ),
                  
                  tabPanel("Contaminant levels",fluid = TRUE, icon = icon("chart-area"),
                           fluidRow(
                             column(
                               p("This tool explores groundwater contaminants across California counties. 
                               The interface allows the user to select the relevant contaminant and the county of interest. 
                               The resulting figure explores monthly averages of the selected contaminant throughout time given the selected constraints.",
                                 style="text-align:justify;color:black;padding:15px;border-radius:5px; width: 1250px; align: center"),
                               width=4)
                           ),
                           sidebarLayout(
                             sidebarPanel(
                               "What do you want to represent?",
                               br(),
                               hr(),
                               selectInput(inputId = "pick_pollutant",
                                           label = "Select pollutant",
                                           choices = unique(df$gm_chemical_name),
                                           selected = "50 Free"
                               ), # End selectInput
                               
                               selectInput(inputId = "pick_county",
                                           label = "Select County",
                                           choices = unique(df$gm_gis_county),
                                           selected = "50 Free"
                               ), # End selectInput
                               
                               sliderInput(inputId = "pick_range",
                                           label = "Time range",
                                           min = min(df$date),
                                           max = max(df$date),
                                           value = c(min(df$date),max(df$date)),
                                           timeFormat = "%m/%Y",
                                           ticks = F
                               ), # End sliderInput
                               
                               checkboxInput("include_fit","Include trend",FALSE
                               ) # end checkbox
                               
                             ), # End of sidebarPanel
                             
                             mainPanel(
                               "Contaminant Temporal Series",
                               
                               tabsetPanel(
                                 tabPanel("Original time series", plotOutput("gw_contaminant_plot")), 
                                 tabPanel("Seasonal", plotOutput("gw_contaminant_seasonplot")),
                                 tabPanel("Annual", plotOutput("gw_contaminant_annualplot"))
                               ) # End tabsetPanel
                             ) # End of mainPanel
                           ) # End of sidebarLayout
                  ), # End of tabPanel Time Series
                  
                  tabPanel("Contaminant map",fluid = TRUE, icon = icon("map"),
                           fluidRow(
                             column(
                               p("This tool explores groundwater contaminants across California counties. 
                               The interface allows the user to select the relevant contaminant and the county of interest. 
                               The resulting figure explores monthly averages of the selected contaminant throughout time given the selected constraints.",
                                 style="text-align:justify;color:black;padding:15px;border-radius:5px; width: 1250px; align: center"),
                               width=4)
                           ),
                           sidebarLayout(
                             sidebarPanel(
                               "What do you want to represent?",
                               hr(),
                               selectInput(inputId = "pick_pollutant_map",
                                           label = "Select pollutant",
                                           choices = unique(combined_sf_shiny$gm_chemical_name),
                                           selected = "50 Free"
                               ), # End selectInput
                               
                               sliderInput(inputId = "pick_year_map",
                                           label = "Time range",
                                           min = min(combined_sf_shiny$year),
                                           max = max(combined_sf_shiny$year),
                                           value = min(combined_sf_shiny$year),
                               ) # End sliderInput
                               
                             ), # End of sidebarPanel
                             mainPanel(
                               column(
                                 "California Counties Map",
                                 tmapOutput(outputId = "gw_map_con"), width = 8)
                             ) # End of mainPanel
                           ) # End of sidebarLayout
                  ), # End of tabPanel map
                  
                  tabPanel("Groundwater level",fluid = TRUE, icon = icon("chart-area"),
                           fluidRow(
                             column(
                               p("This tool explores groundwater levels evolution across California counties.
              The interface allows the user to select county of interest.
                The resulting figure explores monthly averages ground water levels of the selected county throughout time given the selected constraints.",
                                 style="text-align:justify;color:black;padding:15px;border-radius:5px; width: 1250px; align: center"),
                               width=4)
                           ),
                           sidebarLayout(
                             sidebarPanel(
                               "What do you want to represent?",
                               br(),
                               hr(),
                               selectInput(inputId = "pick_county_evol",
                                           label = "Select county",
                                           choices = unique(water_level_ts$county_name),  # CHANGE VARIABLE NAME FOR COUNTIES
                                           selected = "50 Free"
                               ),  # End selectInput
                               
                               selectInput(inputId = "pick_use_evol",
                                           label = "Select well use",
                                           choices = unique(water_level_ts$well_use),  # CHANGE VARIABLE NAME FOR COUNTIES
                                           selected = "50 Free"
                               ),  # End selectInput
                               
                               sliderInput(inputId = "pick_range_evol",
                                           label = "Time range",
                                           min = min(water_level_ts$date),  # MAKE SURE THE DATE VARIABLE HAS THE SAME NAME. OTHERWISE YOU'LL HAVE TO CHANGE IT
                                           max = max(water_level_ts$date),
                                           value = c(min(water_level_ts$date),max(water_level_ts$date)),
                                           timeFormat = "%m/%Y",
                                           ticks = F
                               ),  # End sliderInput
                               
                               checkboxInput("pick_trend","Include trend",FALSE
                               ), # end checkbox
                               
                             ),  # End of sidebarPanel
                             mainPanel(
                               "Groundwater level evolution",
                               
                               tabsetPanel(
                                 tabPanel("Original time series", plotOutput("gw_level_plot")), 
                                 tabPanel("Seasonal", plotOutput("gw_level_seasonplot")),
                                 tabPanel("Annual", plotOutput("gw_level_annualplot"))
                               ) # End tabsetPanel
                             )  # End of mainPanel
                           )  # End of sidebarLayout
                  ),  # End of tabPanel groundwater level evolution
                  
                  tabPanel("Groundwater map",fluid = TRUE, icon = icon("map"),
                           fluidRow(
                             column(
                               p("This tool explores groundwater levels across California counties. 
                               The interface allows the user to select the relevant well type and county of interest. 
                               The resulting figure explores annual averages of the depth to water table throughout time given the selected constraints.",
                                 style="text-align:justify;color:black;padding:15px;border-radius:5px; width: 1250px; align: center"),
                               width=4)
                           ),
                           sidebarLayout(
                             sidebarPanel(
                               "What do you want to represent?",
                               hr(),
                               selectInput(inputId = "pick_well_map",
                                           label = "Select Well Type",
                                           choices = unique(combined_well_shiny_sf$well_type),
                                           selected = "50 Free"
                               ), # End selectInput
                               
                               selectInput(inputId = "pick_year_gw_map", 
                                           label = ("Select Year"), 
                                           choices = list("Year" = c(min(1950):max(combined_well_shiny_sf$year))),
                                           selected = 1)
                               
                             ), # End of sidebarPanel
                             mainPanel(
                               column(
                                 "California Counties Map",
                                 tmapOutput(outputId = "gw_map_gw"), width = 8)
                             ) # End of mainPanel
                           ) # End of sidebarLayout
                  ), 
                  
                  tabPanel("Contaminant Statistics", fluid = T, icon = icon("table"),
                           fluidRow(
                             p("This table shows general statistics for each pollutant in a selected county at any time range.",
                               style="text-align:justify;color:black;padding:15px;border-radius:5px;align:center;width:1250"),
                           ),
                           sidebarLayout(
                             sidebarPanel(
                               "What do you want to represent?",
                               br(),
                               hr(),
                               selectInput(inputId = "pick_county_table",
                                           label = "Select County",
                                           choices = unique(df$gm_gis_county),
                                           selected = "50 Free"
                               ), # End selectInput
                               
                               selectInput(inputId = "pick_year_table", 
                                           label = ("Select Year"), 
                                           choices = list("Year" = c(min(df$year):max(df$year))),
                                           selected = 1),
                               
                               hr(),
                               fluidRow(column(3, verbatimTextOutput("value"))
                               ), # end selectInput fpr year
                               
                               checkboxGroupInput(inputId = "pick_contaminant_table",
                                                  label = "Contaminant",
                                                  choices = c("Bicarbonate Alkalinity" = "Bicarbonate Alkalinity", 
                                                              "Potassium" = "Potassium", 
                                                              "Nitrate" = "Nitrate"),
                                                  selected = c("Bicarbonate Alkalinity", "Potassium", "Nitrate")
                                                  
                               ), # end checkboxGroup
                               
                               
                             ), # End of sidebarPanel
                             mainPanel(
                               column(
                                 "California Contaminant Statistics",
                                 tableOutput(outputId ="gw_stat"), width = 8
                               ) # End of mainPanel
                             ) # End of sidebarLayout
                           ) # End of tabPanel statistics
                           
                  ) # End of tabPanel
                ) #end of navbarPage
)

## Start of the server
server <- function(input,output) {
  
  ## Contaminants evolution
  ## -------------------------------------------------
  
  ### Contaminants original time series
  
  #### Creating the gw_contaminant_plot_reactive
  gw_contaminant_plot_reactive <- reactive({
    df %>% 
      filter(gm_chemical_name %in% input$pick_pollutant,
             gm_gis_county %in% input$pick_county) %>% 
      filter(date >= input$pick_range[1]) %>%
      filter(date <= input$pick_range[2])
  }) # end gw_contaminant_plot_reactive
  
  #### Creating the gw_contaminant_plot
  output$gw_contaminant_plot <- renderPlot({
    p <- ggplot(data=gw_contaminant_plot_reactive(),aes(x=date,y=mean_gm_result)) +
      geom_ribbon(aes(x = date, ymin = min(mean_gm_result), ymax = mean_gm_result),fill="#FBC7D4", alpha=0.4) +
      geom_line(color="#f6809d", size=1) +
      geom_point(size=1, color="#f6809d") +
      labs(y ="mg/l", x = "Years") +
      theme_minimal()
    
    if (input$include_fit) {
      p <- p + geom_smooth(se=FALSE, color="brown3")
    }
    p
  }) # end output$gw_contaminant_plot
  
  
  ### Contaminants seasonal time series
  
  #### Creating the gw_contaminant_seasonplot_reactive
  gw_contaminant_seasonplot_reactive <- reactive({
    df %>% 
      filter(gm_chemical_name %in% input$pick_pollutant,
             gm_gis_county %in% input$pick_county) %>% 
      filter(date >= input$pick_range[1]) %>%
      filter(date <= input$pick_range[2]) %>%
      ungroup() %>% 
      as_tsibble(key = NULL, index = date) %>% 
      index_by(yr_mo = ~yearmonth(.)) %>% 
      summarize(monthly_mean = mean(mean_gm_result, na.rm = TRUE)) %>% 
      tsibble::fill_gaps()
    
  }) # end gw_contaminant_seasonplot_reactive
  
  ### Creating the gw_contaminant_seasonplot
  output$gw_contaminant_seasonplot <- renderPlot({
    
    p1 <- gg_subseries(data=gw_contaminant_seasonplot_reactive(),y = monthly_mean) +
      theme_minimal() +
      labs(y = "mg/l",
           x="",
           title = paste("Time ranging from",
                         min(gw_contaminant_seasonplot_reactive()$yr_mo),
                         "to",
                         max(gw_contaminant_seasonplot_reactive()$yr_mo))) +
      theme(axis.text.x=element_blank())
    if (input$include_fit) {
      p1 <- p1 + geom_smooth(se=FALSE)
    }
    
    p2 <- gg_season(data=gw_contaminant_seasonplot_reactive(),y = monthly_mean) +
      theme_minimal() +
      labs(x = "Month",
           y = "mg/l",
           Color = "Years")
    p1/p2 
    
  }) # end gw_contaminant_seasonplot
  
  
  ### Contaminants annual time series
  
  #### Creating the gw_contaminant_annualplot_reactive
  gw_contaminant_annualplot_reactive <- reactive({
    
    df %>% 
      filter(gm_chemical_name %in% input$pick_pollutant,
             gm_gis_county %in% input$pick_county) %>% 
      filter(date >= input$pick_range[1]) %>%
      filter(date <= input$pick_range[2]) %>%
      ungroup() %>% 
      as_tsibble(key = NULL, index = date) %>% 
      index_by(yearly = ~year(.)) %>% 
      summarize(annual_mean = mean(mean_gm_result, na.rm = TRUE)) %>% 
      tsibble::fill_gaps()
    
  }) # end gw_contaminant_annualplot_reactive
  
  #### Creating the gw_contaminant_annualplot
  output$gw_contaminant_annualplot <- renderPlot({
    
    p <- ggplot(data = gw_contaminant_annualplot_reactive(),aes(x = yearly, y = annual_mean)) +
      geom_ribbon(aes( ymin = min(annual_mean), ymax = annual_mean),fill="#FBC7D4", alpha=0.4) +
      geom_line(color="#f6809d", size=1) +
      geom_point(size=1, color="#f6809d") +
      labs(y ="mg/l", x = "Year")
    
    if (input$include_fit) {
      p <- p + geom_smooth(se=FALSE,color="brown3")
    }
    p
  }) # end gw_contaminant_annualplot
  
  
  
  ## Contaminant Map 
  ## -------------------------------------------------

  map_reactive_con <- reactive({
    combined_sf_shiny %>% 
      filter(gm_chemical_name %in% input$pick_pollutant_map) %>% 
      filter(year == input$pick_year_map[1])
  }) # end map_reactive
  
  ### Creating the map with tmap
  output$gw_map_con <- renderTmap({
    tm_shape(shp = map_reactive_con()) +
      tm_borders(col = 'gray') +
      tm_fill(col = 'mean_gm_result',
              title = "Mean Contaminant Concentration",
              style = 'cont',
              popup.vars = c("Population in Poverty (2019)"="povall_2019","Percent of Population in Poverty (2019)"="pctpovall_2019"),
              popup.format = list()) 
  }) # end output$gw_map_con
  
  ## Statistic table
  ### the ui and reactives aren't interacting :/
  ca_stat <- reactive({
    df1 %>%
      filter(county == input$pick_county_table,
             year == input$pick_year_table,
             gm_chemical_name %in% input$pick_contaminant_table)
  }) # end ca_stat reactive
  
  ### Creating the table
  output$gw_stat <- renderTable({
    ca_stat() %>% 
      group_by(gm_chemical_name) %>%
      summarise(mean_gm_result = mean_gm_result) %>% 
      rename("Chemical Name" = "gm_chemical_name",
             "Mean Concentration (mg/L)" = "mean_gm_result") 
  }) ### end gw_stat
  
  ## Groundwater Map 
  map_reactive_gw <- reactive({
    combined_well_shiny_sf %>% 
      filter(well_type %in% input$pick_well_map) %>% 
      filter(year == input$pick_year_gw_map[1])
  }) # end map_reactive
  
  ### Creating the map using tmap
  output$gw_map_gw <- renderTmap({
    tm_shape(map_reactive_con()) +
      tm_fill(col = 'cornsilk1') +
      tm_borders(col = 'gray') +
      tm_shape(map_reactive_gw()) +
      tm_dots(size = 0.05,
              alpha = 0.8,
              col = 'mean_water_depth',
              style = 'cont',
              title = "Depth to Water Table (ft)",
              palette = "PuBu",
              popup.vars = c("Depth to Water Table (ft)"="mean_water_depth"),
              popup.format = list())
  }) # end output$gw_map_con
  
  
  ## Water level evolution
  ## -------------------------------------------------
  
  ### Water level original time series
  
  #### Creating the gw_level_plot_reactive
  gw_level_plot_reactive <- reactive({
    water_level_ts %>% 
      filter(county_name %in% input$pick_county_evol,
             well_use %in% input$pick_use_evol) %>% 
      filter(date >= input$pick_range_evol[1]) %>%
      filter(date <= input$pick_range_evol[2])
  }) # end gw_level_plot
  
  #### Creating the gw_level_plot
  output$gw_level_plot <- renderPlot({
    p <- ggplot(data=gw_level_plot_reactive(),aes(x=date,y=mean_gse_gwe)) +
      geom_ribbon(aes(x = date, ymin = max(mean_gse_gwe), ymax = mean_gse_gwe),fill="dodgerblue2", alpha=0.4) +
      geom_line(color="dodgerblue2", size=0.5) +
      geom_point(size=0.5, color="dodgerblue2") +
      labs(y ="Depth to water table (ft)", x = "Year") +
      scale_y_continuous(trans = "reverse")
    # When the "fit" checkbox is checked, add a line
    # of best fit
    if (input$pick_trend) {
      p <- p + geom_smooth(se=FALSE)
    }
    p
  }) # end gw_level_plot
  
  
  ### Water level seasonal series
  
  #### Creating the gw_level_plot_reactive
  gw_level_seasonplot_reactive <- reactive({
    
    water_level_ts %>% 
      filter(county_name %in% input$pick_county_evol,
             well_use %in% input$pick_use_evol) %>% 
      filter(date >= input$pick_range_evol[1]) %>%
      filter(date <= input$pick_range_evol[2]) %>% 
      as_tsibble(key = NULL, index = date) %>% 
      index_by(yr_mo = ~yearmonth(.)) %>% 
      summarize(monthly_mean = mean(mean_gse_gwe, na.rm = TRUE)) %>% 
      tsibble::fill_gaps()
    
  }) # end gw_level_plot_reactive
  
  #### Creating the gw_level_seasonplot
  output$gw_level_seasonplot <- renderPlot({
    
    p1 <- gg_subseries(data=gw_level_seasonplot_reactive(),y = monthly_mean) +
      theme_minimal() +
      labs(y = "Depth to water table (ft)",
           x="",
           title = paste("Time ranging from",
                         min(gw_level_seasonplot_reactive()$yr_mo),
                         "to",
                         max(gw_level_seasonplot_reactive()$yr_mo))) +
      scale_y_continuous(trans = "reverse") +
      theme(axis.text.x=element_blank())
    if (input$pick_trend) {
      p1 <- p1 + geom_smooth(se=FALSE)
    }
    
    p2 <- gg_season(data=gw_level_seasonplot_reactive(),y = monthly_mean) +
      theme_minimal() +
      labs(x = "Month",
           y = "Depth to water table (ft)",
           Color = "Years") +
      scale_y_continuous(trans = "reverse")
    
    p1/p2 
    
  }) # end gw_level_seasonplot
  
  
  ### Water level annual series
  
  #### Creating the gw_level_plot_reactive
  gw_level_annualplot_reactive <- reactive({
    
    water_level_ts %>% 
      filter(county_name %in% input$pick_county_evol,
             well_use %in% input$pick_use_evol) %>% 
      filter(date >= input$pick_range_evol[1]) %>%
      filter(date <= input$pick_range_evol[2]) %>% 
      as_tsibble(key = NULL, index = date) %>% 
      index_by(yearly = ~year(.)) %>%
      summarize(annual_mean = mean(mean_gse_gwe, na.rm = TRUE)) %>% 
      tsibble::fill_gaps()
    
  }) # end gw_level_annualplot_reactive
  
  #### Creating the gw_level_annualplot
  output$gw_level_annualplot <- renderPlot({
    
    p <- ggplot(data = gw_level_annualplot_reactive(), aes(x = yearly, y = annual_mean)) +
      geom_ribbon(aes(x = yearly, ymin = max(annual_mean), ymax = annual_mean),fill="dodgerblue2", alpha=0.4) +
      geom_line(color="dodgerblue2", size=0.5) +
      geom_point(size=0.5, color="dodgerblue2") +
      labs(y ="Depth to water table (ft)", x = "Year") +
      scale_y_continuous(trans = "reverse")
    
    if (input$pick_trend) {
      p <- p + geom_smooth(se=FALSE)
    }
    p
  }) # end of gw_level_annualplot
}

shinyApp(ui=ui, server=server)