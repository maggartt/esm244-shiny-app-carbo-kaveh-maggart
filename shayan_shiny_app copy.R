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
                               selectInput(inputId = "pick_county",
                                           label = "Select County",
                                           choices = unique(stat$gm_gis_county),
                                           selected = "50 Free"
                               ), # End selectInput
                               
                               selectInput(inputId = "pick_year",
                                           label = ("Select Year"),
                                           choices = list("Year" = c(min(stat$year):max(stat$year))),
                                           selected = 1),
                               
                               hr(),
                               fluidRow(column(3, verbatimTextOutput("value"))
                               ), # end selectInput fpr year
                               
                               checkboxGroupInput(inputId = "pick_contaminant",
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
  
  ## Statistic table
  ## -------------------------------------------------
  ca_stat <- reactive({
    stat %>%
      filter(gm_gis_county == input$pick_county) %>%
      filter(year == input$pick_year) %>%
      filter(gm_chemical_name %in% input$pick_contaminant)
  }) # end ca_stat reactive
  
  ### Creating the table
  output$gw_stat <- renderTable({
    ca_stat() %>%
      group_by(gm_chemical_name) %>%
      summarise(mean) %>%
      rename("Chemical Name" = "gm_chemical_name",
             "Mean Concentration (mg/L)" = "mean")
  }) ### end gw_stat
  
}

shinyApp(ui=ui, server=server)
