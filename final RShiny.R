pacman::p_load("shiny", "tidyverse", "lubridate",
               "leaflet", "spData",
               "rgdal", "sf",
               "rnaturalearth", "WDI", "tigris")
#import data cleaning from Final stsci 4100 file #
master_data <- read.csv("Arsenic_oil_employment_merged.csv")
master_data <- as.data.frame(master_data)
colnames(master_data)[which(names(master_data) == "Avereage_oil")] <- "Average_oil"

#make new columns for incidence
master_data$Bladder_Incidence<-as.integer(master_data$Bladder_Count)/master_data$Population_count*100000
master_data$Colo_Incidence<-as.integer(master_data$Colorectal_Count)/master_data$Population_count*100000
master_data$Kidney_Incidence<- master_data$Kidney_Count/master_data$Population_count*100000

#convert oil and gas to binary
# 1 for if there is oil production, 0 for none 
master_data$Oil_bin <- as.numeric(master_data$Average_oil > 0)

master_data$Average_gas<-apply(master_data[,53:63],1,mean)
master_data$Gas_bin <- as.numeric(master_data$Average_gas > 0)

#remove unnecessary columns 
master_data_drop<-master_data[,-c(41:67)]
#reorganize columns
master_data_drop<-master_data_drop[, c(1:4, 42, 8, 5:7, 43:45, 9:10, 22, 23, 11:21, 24:41, 46:48)]

#import map data
lat_long <- read.csv("us_county_latlng.csv")
library(dplyr)

# merge map data by FIPS
df_merge=left_join(master_data_drop, lat_long, by=c('FIPS'='fips_code'))

# import the space data
unzip('UScounties.zip')
space<-read_sf('UScounties.shp')

space$FIPS=as.integer(space$FIPS)
#merge space data
merge_space <- space %>% inner_join(df_merge, by = c("FIPS" = "FIPS"))%>%
  st_as_sf()

# create the side bar panel
sidebar_content <- sidebarPanel(
      selectInput(
        inputId = "output_var",
        label = h3("Response:"),
        choices = c("Bladder" = "Bladder_Incidence",
                    "Colorectal" = "Colo_Incidence",
                    "Kidney" = "Kidney_Incidence")),
      checkboxGroupInput(
        inputId = "input_var",
        label = h3("Predictor:"),
        choices = c("Unweighted Arsenic" = "unweighted",
                    "Weighted Arsenic" = "weighted",
                    "% Male" = "Percent_Male",
                    "% Black" = "Percent_Black",
                    "% Same County Residence At Least 5 Years" = "Percent_Same_County_Residence_At_Least_5_Years",
                    "% Had Endoscopy" = "Percent_Ever_Had_Endoscopy",
                    "% Smoked" = "Percent_Ever_Smoked",
                    "EQI Air" = "EQI_AIR", 
                    "EQI Water" = "EQI_WATER", 
                    "EQI Land" = "EQI_LAND", 
                    "EQI SD" = "EQI_SD", 
                    "EQI Built" = "EQI_BUILT", 
                    "EQI Overall" = "EQI_OVERALL",
                    "Rural Urban Continuum Code" = "Rural_urban_continuum_code_2013",
                    "Urban Influence Code" = "Urban_influence_code_2013",  
                    "Metro Area" = "Metro_2013",
                    "Median Household Income (2020)" = "Median_Household_Income_2020",
                    "Median Household Income % of State (2020)" = "Med_HH_Income_Percent_of_State_Total_2020",
                    "Avg Unemployment Rate" = "average_unemployment_rate",
                    "Avg oil" = "Average_oil",   
                    "Avg gas" = "Average_gas")),
    )
    
    # Show a plot 
main_content <- mainPanel(
  tabsetPanel(type = "tabs",
              tabPanel("Model Summary", verbatimTextOutput("summary")), # Regression output
              tabPanel("Data", DT::dataTableOutput('tbl')) # Data as datatable
    ))
#create regression panel
first_panel <- tabPanel("Regression",
                        titlePanel("Regression Visualization"),
                        sidebarLayout(
                          sidebar_content, main_content
                        ))

main_content2 <- mainPanel(leafletOutput('mymap'))

#content for the map inputs
sidebar_content2 <- sidebarPanel(
  selectInput("incidence_select", "Cancer Incidence Counts:",
              colnames(df_merge[,c(10:12)]), 
              multiple=F, 
              selected='Kidney_Incidence'),
  sliderInput('race_slider', 
              'Percent Black:', 
              min=0, max=71, 
              step=3,
              value=range(df_merge$Percent_Black))
  
)

second_panel <- tabPanel("Map",
                          titlePanel("US Map"),
                         sidebarLayout(
                           sidebar_content2, main_content2
                         ))

ui <- navbarPage(
  "Arsenic and Cancer Exploration",
  first_panel,
  second_panel
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Regression output
  output$summary <- renderPrint({
    fit <- lm(as.vector(master_data_drop[,input$output_var]) ~ as.matrix(master_data_drop[,input$input_var]))
    summary(fit)
  })
  
  # Data output
  output$tbl = DT::renderDataTable({
    DT::datatable(master_data_drop, options = list(lengthChange = FALSE))
  })
  
  # Map output
  sliderData <- reactive({
    merge_space %>%
      filter(Percent_Black>=input$race_slider[1])%>%
      filter(Percent_Black<=input$race_slider[2])})
  sliderData_nospace<-reactive({df_merge %>%
      filter(Percent_Black>=input$race_slider[1])%>%
      filter(Percent_Black<=input$race_slider[2])
    selectinput_react<-reactive({input$incidence_select})
  })
#add circles and polygons to map
  output$mymap<-renderLeaflet({leaflet(data=sliderData()) %>%
      addTiles()%>%
      addPolygons(color='#444444', 
                  fillColor = colorBin('YlOrRd', 
                                       bins= c(0,10,20,30,100,Inf),df_merge$unweighted)(df_merge$unweighted)
                  ,
                  stroke=FALSE, weight = 1, 
                  smoothFactor = .8, 
                  opacity = 1.0, 
                  fillOpacity = .7,
                  highlightOptions = highlightOptions(color = "white", weight = 5,
                                                      bringToFront = FALSE, stroke=4))%>%
      addLegend( pal=colorBin( palette="YlOrRd",bins= c(0,10,20,30,100,Inf), domain=merge_space$unweighted, na.color="transparent"), 
                 values=~unweighted, 
                 opacity=0.9, 
                 title = "Unweighted Arsenic Level", 
                 position = "bottomleft" ) %>%
      
      addCircles(lng=~lng, lat=~lat, 
                 radius=df_merge[,c(input$incidence_select)]*400,
                 fillOpacity=0.5, 
                 stroke=FALSE, 
                 popup = ~County)
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)