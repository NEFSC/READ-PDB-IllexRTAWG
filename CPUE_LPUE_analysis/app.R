#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(viridis)
library(mapdata)
library(marmap)
library(sf)
library(sodium)


## bring in LPUE data ## for details, see Illex_LPUE_models.R and Illex_LPUE_data_encryption.R


## Load encrypted data
data_encrypted <- readRDS("./www/data/lpueplotdata_encrypted.rds") #readRDS("./lpueplotdata_encrypted.rds")#

## bring in bathymetry data from marmap package
bathy <- readRDS("./www/data/NWAtlBathy.rds") ## turn this off to speed things up and free up memory on the shinyapps.io server

## get coastline from mapdata ## use the low res file to speed things up and free up memory on the shinyapps.io server
reg <- map_data("world") %>% 
  dplyr::filter(region %in% c('Canada', 'USA'),
                between(long, -77,-65), 
                between(lat, 33,45))

## set the color scale for maps -----
colscale <- scale_color_manual(name = "Data Set",
                               breaks = c("VTR", "Observer", "Study Fleet"),
                               values = c("VTR" = "#440154FF", "Observer" = "#21908CFF", "Study Fleet" = "#FDE725FF"))


## set up a placeholder data frame for uncrypted data -----
data_unencrypted <- data.frame(YEAR = factor(),
                               MONTH = factor(),
                               WEEK = factor(),
                               plot_lon = numeric(),
                               plot_lat = numeric(),
                               illexLPUE = numeric(),
                               logLPUE = numeric(),
                               DataSet = factor())

## set the public key -----
 ## this is the "decoder ring" that is used to encrypt the data if the right password is entered
key_public <- "76 8f db 69 b4 f0 f8 a6 1b c4 03 de f6 1a 84 6c b4 f0 0c 00 30 fa 7a ef a2 48 0f e0 f3 5e 9b 33"


## Define UI for application  -------------------------------------------
ui <- fluidPage(

    # Application title
    titlePanel("Northern Shortfin Squid Landings Per Unit Effort"),
    # Set up a sidebar layout 
    sidebarLayout(
      sidebarPanel(width = 3,
                   # selectInput(inputId = "datayear",
                   #             label = "Select a year",
                   #             choices = c("All",2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019),
                   #             selected = 2019,
                   #             multiple = FALSE),
                   # radioButtons(inputId = "showfacets",
                   #              label = "Show data by",
                   #              choices = c("Month"= "MONTH", "Week" = "WEEK"),
                   #              selected = "MONTH"),
                   radioButtons(inputId = "targeted",
                                label = "Do you want to display only targeted trips?",
                                choices = c("Targeted trips (>10,000lbs and >50% of landings)" = "target",
                                            "All trips (at least 100 lbs)" = "alltrips"),
                                selected = "target"),
                   selectInput(inputId = "vessel",
                               label = "Select a vessel type",
                               choices = c("Freezer" = "Freezer"
                                           , "RSW" = "RSW"
                                           , "Ice/Fresh" = "Ice"
                                           , "Unknown" = "UNKNOWN"
                                           , "All"),
                               selected = "All",
                               multiple = FALSE),
                   sliderInput(inputId = "whichweeks",
                               label = "Select week range:",
                               min = 18,
                               max = 43,
                               value = c(23,35))
      ), #close the sidebar panel
      mainPanel(
        # Set up a tab layout 
        tabsetPanel(type = "tabs",
         # The "About" panel holds some introductory information and a password input to control who can see the plotted data
           tabPanel("About",
                   p('Welcome',style = "font-family: 'times'; font-size:14pt"),
                   div('Thanks for your interest in the northern shortfin squid assessment. This app is designed to help explore several fishery dependent datasets for use in landings per unit effort (LPUE) indices. Each of the tabs displays the same data in different ways. On the "By year" tab, you can explore the trends across weeks or months in an individual year. On the "By month" tab, you can compare the landings from a single month across the available years. Finally, on the "Weekly LPUE" tab, you will see boxplots of LPUE for each week across the available years.', style="font-family: 'times'; font-size:11pt"),
                   p('', style = "font-family: 'times'; font-size:14pt"),
                   p('About the data',style = "font-family: 'times'; font-size:14pt"),
                   div("The data presented here are from the Northeast Fishery Science Center Study Fleet, the Northeast Fisheries Observer Program, and from Vessel Trip Reports (VTR) matched to commercial dealer records. Unless otherwise noted, each datum represents a single tow in the case of Study Fleet and Observer records and a single trip or substrip in the VTR records. The unit is pounds landed per hour fished.", style = "font-family: 'times'; font-size:11pt"),
                   p('',style = "font-family: 'times'; font-size:14pt"),
                   p('Getting Started',style = "font-family: 'times'; font-size:14pt"),
                   div("Please enter the password below to see the data. If you do not know the password and think you should have access, please contact Brooke Lowman at brooke.lowman@noaa.gov.", style = "font-family: 'times'; font-size:11pt"),
                   # Password input 
                   passwordInput(inputId = "txt_password",
                                  label = "",
                                  placeholder ="password",
                                  value = NULL),
                   # Submit button
                   actionButton(inputId = "button",
                                label = "Submit"),
                   # Message to let user know if they entered the correct password
                   textOutput("pswd_note"),
                   tags$head(tags$style("#pswd_note{color: red; }"
                   )),
                   p('', style = "font-family: 'times'; font-size:14pt"),
                   # Text and link to more information 
                   p('Want to know more?', style = "font-family: 'times'; font-size:14pt"),
                   div("For more information about the northern shortfin squid stock assessment working group, please visit", style = "font-family: 'times'; font-size:11pt"),
                   a("https://www.fisheries.noaa.gov/event/illex-working-group.", href = "https://www.fisheries.noaa.gov/event/illex-working-group", style = "font-family: 'times'; font-size:11pt")
                   ), #close the "About" tab

          tabPanel("By Year",
                   fluidRow(column(4,
                                   selectInput(inputId = "datayear",
                                        label = "Select a year",
                                        choices = c("All",2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019),
                                        selected = 2019,
                                        multiple = FALSE)),
                            column(4, offset = 1,
                                   radioButtons(inputId = "showfacets",
                                         label = "Show data by",
                                         choices = c("Month"= "MONTH", "Week" = "WEEK"),
                                         selected = "MONTH"))
                            ),
                   fluidRow(plotOutput("byyearplot", width ="100%"))
                   ), #close the "By Year" tab panel

          tabPanel("By Month",
                   fluidRow(selectInput(inputId = "whichmonth",
                                label = "Select a month to compare",
                                choices = c("All", "May", "Jun", "Jul", "Aug", "Sep", "Oct"),
                                selected = "All",
                                multiple = FALSE)
                              ), 
                   fluidRow(plotOutput("bymonthplot", width = "100%"))
                   ), #close the "By Month" tab panel

          tabPanel("Weekly LPUE",
                   verticalLayout(
                   plotOutput("weeklylpue")
                   ) # close the verticalLayout
                   ) #close the "Weekly LPUE" tab panel
                 )#close tab set
) #close the mainpanel 
) #close the sidebar layout
)#close thefluidpage 

# -------------------------------------------------------------------------------------
 # input <- list(datayear = 2008,
 #               showfacets = "MONTH",
 #               whichweeks = c(23,35),
 #               whichmonth =  "Jun",
 #               txt_password = "Illex2021",
 #               targeted = "target"
 #               )
## -------------------------------------------------------------------------------------

## Define server logic required to make a plot
## create reactive data elements for plotting -----
 server <- function(input, output, session) {

## password related junk -----
   ## get the human and shiny readable password
 key_private <- reactive({
   kp <- sha256(charToRaw(input$txt_password))
   paste(pubkey(kp), collapse = " ")
 })  
 
 ## get the machine and sodium readable password
 kpRaw <- reactive({
   sha256(charToRaw(input$txt_password))
 })


## Observe submit button (runs once when submit button is clicked) -----
observeEvent(input$button, {
 ## check if private key provided is correct
  if(key_private() == key_public) {
    output$pswd_note <- renderText("Password accepted. Do not share the plots with unauthorized users.")
    ## unecncrypt the data and make it usable
    data_unencrypted <- as.data.frame(unserialize(simple_decrypt(data_encrypted, kpRaw())))
  } else {
    output$pswd_note <- renderText("Incorrect password. Please try again.")
  }
  
##------
## do the filtering to create the dataframes used for plotting -----

mydat <- reactive({
  mydat <- data_unencrypted %>%
    mutate(MONTH = factor(MONTH, levels = month.abb))
  if(input$vessel != "All") {mydat <- mydat %>% filter(Vess_type == input$vessel)}
  if(input$targeted == "target") {mydat <- mydat %>% filter(istarget == input$targeted)}
  mydat <- mydat %>% droplevels()
  })

databyyear <- reactive({
    databyyear <- mydat() %>%
      filter(between(WEEK, input$whichweeks[1], input$whichweeks[2]))
    if(input$datayear == "All") {databyyear <- databyyear} else {databyyear <- databyyear %>% filter(YEAR == input$datayear)}
    })

databymonth <- reactive({
    databymonth <- mydat()
    if(input$whichmonth == "All") {databymonth <- databymonth} else{ databymonth <- databymonth %>% filter(MONTH == input$whichmonth)}
})

weeklylpuedata <- reactive({
  weeklylpuedata <- mydat() %>% filter(between(WEEK, input$whichweeks[1], input$whichweeks[2]))
})


## landingsplots  -----
output$byyearplot <- renderPlot({
        plotdat <- databyyear()
        #Plot of LPUE
        plot1 <- ggplot() +
          geom_polygon(data = reg, aes(x=long, y = lat, group = group), color = "gray20", fill = "gray10") +
          #geom_contour(data = bathy, aes(x = x, y = y, z = z), breaks=c(-200), colour="gray80", size=0.2) + # turn these off to speed up plotting
          geom_point(data = plotdat, mapping = aes(x = plot_lon, y = plot_lat, size = illexLPUE, color = DataSet), alpha = 0.5) +
          colscale +
          guides(color = guide_legend(override.aes = list(size = 3) ),
                 size = guide_legend(override.aes = list(alpha = 0.5))) +
          coord_sf(xlim = c(-76,-66), ylim = c(34,44)) + 
          theme(panel.background = element_rect(fill = "gray70"),
                panel.grid = element_line(color = "gray60")) +
          ggtitle(label = ifelse(input$datayear == "All", yes = "All Years", no = input$datayear)) +
          facet_wrap(input$showfacets)
        
        # if(showplot == "yes"){plot1}
        plot1
        }
   # , width = "100%"
   , height= set_shiny_plot_height(session, "output_byyearplot_width")
    ) #close the renderPolt function

output$bymonthplot <- renderPlot({
        plotdat <- databymonth()
        #plot of LPUE for a single month
        plot2 <- ggplot(data = plotdat) +
          geom_polygon(data = reg, aes(x=long, y = lat, group = group), color = "gray20", fill = "gray10") +
          #geom_contour(data = bathy, aes(x = x, y = y, z = z), breaks=c(-200), colour="gray80", size=0.2) + # turn these off to speed up plotting
          geom_point(data = plotdat, aes(x = plot_lon, y = plot_lat, size = illexLPUE, color = DataSet), alpha = 0.5) +
          facet_wrap(~YEAR) +
          colscale +
          guides(color = guide_legend(override.aes = list(size = 3) ),
                 size = guide_legend(override.aes = list(alpha = 0.5))) +
          coord_sf(xlim = c(-76,-66), ylim = c(34,44)) +
          theme(panel.background = element_rect(fill = "gray70"),
                panel.grid = element_line(color = "gray60")) +

          ggtitle(label = ifelse(input$whichmonth == "All", yes = "May through October", no = input$whichmonth))

        plot2
  }
  , height= set_shiny_plot_height(session, "output_bymonthplot_width")
  ) #close the monthplot renderPolt function


  output$weeklylpue <- renderPlot({
    plotdat <- weeklylpuedata()
    plot3 <- ggplot(data= plotdat, aes(x = as.factor(WEEK), y = logLPUE)) +
      geom_point(aes(color = DataSet), position = position_dodge2(width = 0.4), alpha = 0.4) +
      geom_boxplot(outlier.shape = NA, fill = NA) +
      colscale +
      ggtitle(label = "log(LPUE)") +
      xlab("Week") +
      ylab("log(lbs landed per hour fished)") +
      facet_wrap(~YEAR) + 
      theme(panel.background = element_rect(fill = "gray70"),
            panel.grid = element_line(color=NA))
    
    plot3
  
}) #close the weeklylpue renderPolt function -------


}) #close the observe event function



# function to calculate plot height
set_shiny_plot_height <- function(session, output_width_name){
  function() {session$clientData[[output_width_name]] }
 }
  # Remove all data when session ends
cancel.onSessionEnded <- session$onSessionEnded(function() {
    rm(list = ls())
  })
 } #close the server function

## Run the application 
shinyApp(ui = ui, server = server)
