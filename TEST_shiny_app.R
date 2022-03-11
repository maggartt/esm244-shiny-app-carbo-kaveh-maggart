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

test <- df %>% 
  filter(gm_gis_county == "All",
         gm_chemical_name == "Bicarbonate Alkalinity") %>% 
  as_tsibble(key = NULL, index = date) %>% 
  ungroup() %>% 
  index_by(yr_mo = ~yearmonth(.)) %>% 
  summarize(monthly_mean = mean(mean_gm_result, na.rm = TRUE)) %>% 
  tsibble::fill_gaps()


###Create mean for chemical by county and year
stat <- read_csv(here("data","filtered_data_2000_2020.csv"), col_names = TRUE) %>%
  mutate(gm_chemical_name = case_when(
    gm_chemical_name == "Bicarbonate Alkalinity (mg/l)" ~ "Bicarbonate Alkalinity",
    gm_chemical_name == "Potassium (mg/l)"  ~ "Potassium",
    gm_chemical_name == "Nitrate as N (mg/l)" ~ "Nitrate")) %>%
  group_by(gm_chemical_name, year, gm_gis_county) %>%
  summarise(mean = mean(mean_gm_result)) %>%
  select(gm_chemical_name, gm_gis_county, year, mean)

# Reading water level data for temporal series

water_level_ts <- read_csv(here("data","waterlevel_series.csv"), col_names = TRUE) 


test <- water_level_ts %>% 
  filter(county_name == "All",
         well_use == "All") %>% 
  as_tsibble(key = NULL, index = date) %>% 
  index_by(yr_mo = ~yearmonth(.)) %>% 
  summarize(monthly_mean = mean(mean_gse_gwe, na.rm = TRUE)) %>% 
  tsibble::fill_gaps()

test %>% 
  gg_season(y=monthly_mean) +
  theme_minimal() +
  labs(x = "Month",
       y = "Number",
       title = paste("Years ranging from",
                     min(test$yr_mo)),
       color="Year") +
  scale_y_continuous(trans = "reverse") +
  theme(axis.text.x=element_blank())




# Create a custom theme
my_theme <- bs_theme(
  bg = "#FFFFFF",
  fg = "#161853",
  primary = "#EC255A",
  base_font = font_google('Avenir')
)

ui <- fluidPage(theme=my_theme,
                navbarPage( "Groundwater \n California",
                    # Title
                  
                  tabPanel("Contaminant temporal series",fluid = TRUE, icon = icon("chart-area"),
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
                  
                  
              
                  tabPanel("Groundwater level evolution",fluid = TRUE, icon = icon("chart-area"),
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