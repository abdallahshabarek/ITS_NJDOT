


# https://gist.github.com/aagarw30/ad3e0feb23ade99b85cffe3bf79cb3ae

list.of.packages <- c("readr","shinythemes","hms","dplyr","shinydashboard", "shinyWidgets","plotrix","gplots")
library(shinyWidgets)
library(shinydashboard)
library(plotrix)
library(gplots)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
library(lubridate)
library(reshape)
library(dplyr)
library(hms)
library(sqldf)
library(ggplot2)
library(r2symbols)
library(leaflet)
library(gridExtra)
library(ggthemes)


# new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
# if (length(new.packages)) install.packages(new.packages)
# lapply(list.of.packages, require, character = TRUE)


# rounded_every <- 6 #10 minutes
library(dplyr)
library(shiny)
library(hms)
library(readr)
library(shinythemes)
list_rnow <- list()
counter = 1
# index <- 0
# idx <- 0

ui <- fluidPage(
    titlePanel(tags$div("SCATS Data Manager", tags$br(),
                        h1(strong("Developed by Abdullah Shabarek"), style = "font-size:15px;")))
    
    ,
    dashboardSidebar(
      sidebarMenu(
        menuItem("MX Value Analysis", tabName = "Max_Value_Problems", icon = icon("clock-o")),
        # menuItem("Problematic Sites with Time", tabName = "Problematic_Sites_with_Time", icon = icon("list-alt")),
        menuItem("Detector Alarms", tabName = "Detector_Alarms", icon = icon("cog")),
        menuItem("Degree of Saturation Analysis", tabName = "Degree_Saturation", icon = icon("battery-full")),
        menuItem("Degree of Saturation over Phase Timing Analysis", tabName = "Degree_Saturation_Phase", icon = icon("list-alt")),
        menuItem("Communication, Falling Back, Bad Data & Flashing Yellow Analysis", tabName = "Comm_Analysis", icon = icon("list-alt")),
        menuItem("Offset Analysis", tabName = "Offset_Analysis", icon = icon("list-alt")),
        menuItem("TOD Analysis", tabName = "TOD_analysis", icon = icon("list-alt")),
        menuItem("Incidents Monitoring", tabName = "Incident1", icon = icon("cog")),
        menuItem("Resources and Instructions", tabName = "Resources", icon = icon("flag-checkered"))
      )),
    
    dashboardBody(
      tabItems(
        # Settings tab content
        tabItem(tabName = "Max_Value_Problems",
                fluidPage(
                  titlePanel("MX Value Analysis"),
                  sidebarLayout(
                    sidebarPanel(
                      bootstrapPage(
                        div(style="display:inline-block",textInput(inputId="lowertime", label="Enter the lower time bound", placeholder = "06:00:00", width=120, value = "06:00:00")),
                        div(style="display:inline-block",textInput(inputId="uppertime", label="Enter the upper time bound", placeholder = "21:00:00", width=120, value = "21:00:00")),
                        div(style="display:inline-block",textInput(inputId="max_value_threshold", label="Enter the Max Value Threshold", placeholder = 15, width=120, value = 15))#,
                        # div(style="display:inline-block",textInput(inputId="rounded_every", label="Enter the Max Cycle Length (minutes)", placeholder = 6, width=180, value = 6)),
                        # div(style="display:inline-block",textInput(inputId="coef1", label="Enter the first coefficient", placeholder = 0.9, width=120, value = 0.9)),
                        # div(style="display:inline-block",textInput(inputId="coef2", label="Enter the second coefficient", placeholder = 1.1, width=120, value = 1.1))
                        ),
                      fileInput("file1",
                  "Choose Event files from a directory",
                  multiple = TRUE,
                  accept=c('text/csv', 
                           'text/comma-separated-values,text/plain', 
                           '.csv')),
        # fileInput("file2",
        #           "Choose Phase files from a directory",
        #           multiple = TRUE,
        #           accept=c('text/csv', 
        #                    'text/comma-separated-values,text/plain', 
        #                    '.csv')),
        actionButton('previewData','Preview', icon = icon("refresh")),
        downloadButton('downloadData', 'Download')
      ),
      mainPanel(
        plotOutput("Plot"),
        tableOutput('contents')
      )
      
    )
  )
  ),
  tabItem(tabName = "Problematic_Sites_with_Time",
          fluidPage(
            titlePanel("SCATS Files Analysis2"),
            sidebarLayout(
              sidebarPanel(
                bootstrapPage(
                  div(style="display:inline-block",textInput(inputId="max_count_threshold", label="Enter the threshold Number", placeholder = 25, width=120, value = 25)),
                  div(style="display:inline-block",textInput(inputId="lowertime2", label="Enter the lower time bound", placeholder = "06:00:00", width=120, value = "06:00:00")),
                  div(style="display:inline-block",textInput(inputId="uppertime2", label="Enter the upper time bound", placeholder = "21:00:00", width=120, value = "21:00:00"))
                ),
                fileInput("file2",
                          "Choose Result files from a directory",
                          multiple = TRUE,
                          accept=c('text/csv', 
                                   'text/comma-separated-values,text/plain', 
                                   '.csv')),
                downloadButton('downloadData2', 'Download')
              ),
              mainPanel(
                tableOutput('contents2')
              )
            )
          )
  ),
  tabItem(tabName = "Detector_Alarms",
            fluidPage(
              titlePanel("Detector Alarms"),
              sidebarLayout(
                sidebarPanel(
                  bootstrapPage(
                    # div(style="display:inline-block",textInput(inputId="max_count_threshold", label="Enter the threshold Number", placeholder = 25, width=120, value = 25)),
                    # div(style="display:inline-block",textInput(inputId="lowertime2", label="Enter the lower time bound", placeholder = "06:00:00", width=120, value = "06:00:00")),
                    # div(style="display:inline-block",textInput(inputId="uppertime2", label="Enter the upper time bound", placeholder = "21:00:00", width=120, value = "21:00:00"))
                  ),
                  fileInput("file3",
                            "Choose SCATS Log file from a directory",
                            multiple = FALSE,
                            accept=c('text/csv', 
                                     'text/comma-separated-values,text/plain', 
                                     '.csv')),
                  downloadButton('downloadData3', 'Download')
                ),
                mainPanel(
                  tableOutput('contents3')
                )
              )
            )
  ),
  tabItem(tabName = "Degree_Saturation",
          fluidPage(
            titlePanel("Degree of Saturation Analysis"),
            
            sidebarLayout(
              sidebarPanel(
                bootstrapPage(
                  selectInput("Rt44444", "Route Name:",
                              c("US-1" = "RT1",
                                "NJ-18" = "RT18",
                                "NJ-73" = "RT73",
                                "US-130" = "RT130"))
                  # div(style="display:inline-block",textInput(inputId="max_count_threshold", label="Enter the threshold Number", placeholder = 25, width=120, value = 25)),
                  # div(style="display:inline-block",textInput(inputId="lowertime2", label="Enter the lower time bound", placeholder = "06:00:00", width=120, value = "06:00:00")),
                  # div(style="display:inline-block",textInput(inputId="uppertime2", label="Enter the upper time bound", placeholder = "21:00:00", width=120, value = "21:00:00"))
                ),
                
                fileInput("file4",
                          "Choose SCATS Strategic Monitor files from a directory",
                          multiple = TRUE,
                          accept=c('text/csv', 
                                   'text/comma-separated-values,text/plain', 
                                   '.csv')),
                actionButton('previewData4','Preview', icon = icon("refresh")),
                downloadButton('downloadData4', 'Download')
              ),
              mainPanel(plotOutput("Plot4"),
                tableOutput('contents4')
              )
            )
          )
  ),
  tabItem(tabName = "Degree_Saturation_Phase",
          fluidPage(
            titlePanel("Degree of Saturation over Phase Timing Analysis"),
            
            sidebarLayout(
              sidebarPanel(
                bootstrapPage(
                  
                
                ),selectInput("degree_saturation_analysis_type", "Choose a Type of Analysis:",
                             list(`Type of Analysis` = list("Ratio", "Two Axis"))),
                selectInput("degree_saturation_analysis_type2", "Choose Type of Details:",
                            list(`Type of Analysis` = list("Phases", "Signal Groups")))
                          ,fileInput("file5",
                          "Choose a SCATS Strategic Monitor file from a directory",
                          multiple = FALSE,
                          accept=c('text/csv', 
                                   'text/comma-separated-values,text/plain', 
                                   '.csv')),
                actionButton('previewData5','Preview', icon = icon("refresh")),
                downloadButton('downloadData5', 'Download')
              ),
              mainPanel(
                fluidRow(plotOutput("Plot5_2",height="300px"),
                         plotOutput("Plot5",height="3000px"),
                        tableOutput('contents5'))
              )
            )
          )
  ),
  tabItem(tabName = "Comm_Analysis",
          fluidPage(
            titlePanel("Communication, Falling Back, Bad Data & Flashing Yellow Analysis"),
            
            sidebarLayout(
              sidebarPanel(
                bootstrapPage(
                  # dateInput("date6", "Date:", value = "2021-09-07")
                  
                  
                ),selectInput("comm_type", "Select one of these issues", choices = c("Stopped Talking"="ST", "Falling Back"="FB", "Flashing Yellow"="FY","Bad Data"="BD"))
                ,dateInput("date6", "Date:"),
                fileInput("file6",
                            "Choose a SCATS Log file from a directory",
                            multiple = FALSE,
                            accept=c('text/csv', 
                                     'text/comma-separated-values,text/plain', 
                                     '.csv')),
                actionButton('previewData6','Preview', icon = icon("refresh")),
                downloadButton('downloadData6', 'Download')
              ),
              mainPanel(plotOutput("Plot6"),
                        tableOutput('contents6')
              )
            )
          )
  ),
  tabItem(tabName = "Offset_Analysis",
          fluidPage(
            titlePanel("Offset Analysis"),
            sidebarLayout(
              sidebarPanel(
                bootstrapPage(
                  # div(style="display:inline-block",textInput(inputId="max_count_threshold", label="Enter the threshold Number", placeholder = 25, width=120, value = 25)),
                  # div(style="display:inline-block",textInput(inputId="lowertime2", label="Enter the lower time bound", placeholder = "06:00:00", width=120, value = "06:00:00")),
                  # div(style="display:inline-block",textInput(inputId="uppertime2", label="Enter the upper time bound", placeholder = "21:00:00", width=120, value = "21:00:00"))
                ),
                airDatepickerInput(
                  inputId = "date7_multiple",
                  label = "Select multiple dates for the Average:",
                  placeholder = "You can pick 5 dates",
                  multiple = TRUE, clearButton = TRUE
                ),
                div(style="display:inline-block",textInput(inputId="offset_site1", label="Enter the Reference 1st Site", placeholder = "10018", width=120, value = "10018")),
                
                # dateInput("date7", "Date:", value = "2021-09-07"),
                # tags$b("Selected"),
                # uiOutput("container"),
                
                # fileInput("file7_1",
                #           "Choose SCATS Log file from a directory",
                #           multiple = FALSE,
                #           accept=c('text/csv', 
                #                    'text/comma-separated-values,text/plain', 
                #                    '.csv')),
                actionButton('previewData7_1','Preview Average', icon = icon("refresh")),
                downloadButton('downloadData7', 'Download')
              ),
              mainPanel(
                tableOutput('contents7')
              )
            )
          )
  ),
  tabItem(tabName = "TOD_analysis",
          fluidPage(
            titlePanel("TOD Analysis"),
            
            sidebarLayout(
              sidebarPanel(
                bootstrapPage(
                  selectInput("file_8_time_type", "Analysis Period Type:",
                              c("Midweek" = "midweek",
                                "Mondays" = "monday",
                                "Tuesdays" = "tuesday",
                                "Wednesdays" = "wednesday",
                                "Thursdays" = "thursday",
                                "Fridays" = "friday",
                                "Saturdays" = "saturday",
                                "Sundays" = "sunday")),
                  selectInput("file_8_offpeak_cycles", "Same Off-Peak Cycle Length:",
                              c("Yes" = "yes",
                                "No" = "no")),
                  div(style="display:inline-block",textInput(inputId="file8_zoneid", label="Enter the Zone Number (0 if unknown)", placeholder = 0, width=160, value = 0)),
                  div(style="display:inline-block",textInput(inputId="file8_max_periods", label="Enter the Maximum number of periods", placeholder = 6, width=160, value = 6)),
                  div(style="display:inline-block",textInput(inputId="file8_min_time", label="Enter the minimum number of hours per period", placeholder = 1, width=160, value = 1)),
                  div(style="display:inline-block",textInput(inputId="file8_max_time", label="Enter the maximum number of hours per period", placeholder = 4, width=160, value = 4))

                  # div(style="display:inline-block",textInput(inputId="max_count_threshold", label="Enter the threshold Number", placeholder = 25, width=120, value = 25)),
                  # div(style="display:inline-block",textInput(inputId="lowertime2", label="Enter the lower time bound", placeholder = "06:00:00", width=120, value = "06:00:00")),
                  # div(style="display:inline-block",textInput(inputId="uppertime2", label="Enter the upper time bound", placeholder = "21:00:00", width=120, value = "21:00:00"))
                ),
                
                fileInput("file8",
                          "Choose SCATS SM files",
                          multiple = TRUE,
                          accept=c('text/csv', 
                                   'text/comma-separated-values,text/plain', 
                                   '.csv')),
                actionButton('previewData8_1','Preview', icon = icon("refresh")),
                downloadButton('downloadData8_1', 'Download'),
                downloadButton('downloadData8_2', 'Abstract'),
                p("_________________________________________________________"),
                p("                                         "),
                p("--------Phase Optimization using File Uploads--------"),
                fileInput("file8_2",
                          "Choose SCATS History Viewer files",
                          multiple = TRUE,
                          accept=c('text/csv', 
                                   'text/comma-separated-values,text/plain', 
                                   '.csv')),
                fileInput("file8_3",
                          "Upload Abstract Proposed Cycle from Last Step",
                          multiple = FALSE,
                          accept=c('text/csv', 
                                   'text/comma-separated-values,text/plain', 
                                   '.csv')),
                downloadButton('downloadData8_3', 'Download TOD Phases'),
                p("_________________________________________________________"),
                p("-------Phase Optimization using Historical Data-------"),
                div(style="display:inline-block",textInput(inputId="site_historical_8", label="Enter the Site Number", placeholder = "10018", width=200, value = "10018")),
                airDatepickerInput(
                  inputId = "date8_1_multiple",
                  label = "Select multiple dates for the Average:",
                  placeholder = "You can pick selected dates",
                  multiple = TRUE, clearButton = TRUE
                ),
                fileInput("file8_4",
                          "Upload Abstract Proposed Cycle from Last Step",
                          multiple = FALSE,
                          accept=c('text/csv', 
                                   'text/comma-separated-values,text/plain', 
                                   '.csv')),
                actionButton('previewData8_5','Preview', icon = icon("refresh")),
                downloadButton('downloadData8_4', 'Download TOD Phases')
                
                
              ),
              mainPanel(plotOutput("Plot8_1"),
                        tableOutput('contents_8_3'),
                        tableOutput('contents_8_4')
              )
            )
          )
  ),
  tabItem(tabName = "Incident1",

            titlePanel("Incident Monitoring"),
          actionButton('previewMap','Preview', icon = icon("refresh")),
          mainPanel(
          leafletOutput("map1", width = "100%", height = "100%"),
          absolutePanel(top = 10, right = 10,
                        style="z-index:500;", # legend over my map (map z = 400)
                        tags$h3("Map of NJ"))
            
            
            
  )),
  tabItem(tabName = "Resources",
          fluidPage(
            titlePanel("Resources and Instructions"),
            sidebarLayout(
              sidebarPanel(
                tags$a(href="https://rawcdn.githack.com/abdallahshabarek/ITS_NJDOT/01cbea9a015ed13fe7a1f6bbba5c021338d08a31/Resources/MX%20Value%20Instructions.pdf", "MX Value Instructions"),
                tags$br(),
                tags$a(href="https://rawcdn.githack.com/abdallahshabarek/ITS_NJDOT/3665a38f3f77a005b4fd86207a511569d0b72e8f/Resources/MX%20Value%20Sample%20Data.zip", "MX Value Sample Data"),
                tags$br(),
                tags$a(href="https://rawcdn.githack.com/abdallahshabarek/ITS_NJDOT/2fea1e47ba4cef4f2569888f27c3666f7db6e7d0/Resources/Detector%20Alarms%20Instructions.pdf", "Detector Alarms Instructions"),
                tags$br(),
                tags$a(href="https://rawcdn.githack.com/abdallahshabarek/ITS_NJDOT/aab51a926ccd76d1ac86de6ef3e9cc7f0b4a953a/Resources/Detector%20Alarm%20Sample%20Data.zip", "Detector Alarms Sample Data"),
                tags$br(),
                tags$a(href="https://rawcdn.githack.com/abdallahshabarek/ITS_NJDOT/aedf36424547347637ee5372b675c34abe558acc/Resources/Degree%20of%20Saturation%20Analysis%20Instructions.pdf", "Degree of Saturation Analysis Instructions"),
                tags$br(),
                tags$a(href="https://rawcdn.githack.com/abdallahshabarek/ITS_NJDOT/8fb5c572c956a8f5efb26817fa0e759f8955173a/Resources/Degree%20of%20Saturation%20Analysis%20Sample%20Data.zip", "Degree of Saturation Analysis Sample Data"),
                tags$br(),
                tags$a(href="https://rawcdn.githack.com/abdallahshabarek/ITS_NJDOT/49614a45bddd393e8e505cbaa1c1d63c52274af2/Resources/Degree%20of%20Saturation%20over%20Phase%20Timing%20Analysis%20Instructions.pdf", "Degree of Saturation over Phase Timing Analysis Instructions"),
                tags$br(),
                tags$a(href="https://rawcdn.githack.com/abdallahshabarek/ITS_NJDOT/107a6eefbe4cd7285c93ce2c39f39bfe7d007557/Resources/Degree%20of%20Saturation%20over%20Phase%20Timing%20Analysis%20Sample%20Data.txt","Degree of Saturation over Phase Timing Analysis Sample Data"),
                tags$br(),
                tags$a(href="https://rawcdn.githack.com/abdallahshabarek/ITS_NJDOT/c4842a30a369cd431264716ed20b1c7446992320/Resources/SCATS%20Log%20user%20manual.pdf", "SCATS Log User Manual"),
                tags$br(),
                tags$a(href="https://rawcdn.githack.com/abdallahshabarek/ITS_NJDOT/c4842a30a369cd431264716ed20b1c7446992320/Resources/SCATS%20operating%20instructions.pdf", "SCATS Operating Instructions"),
                tags$br(),
                tags$a(href="https://rawcdn.githack.com/abdallahshabarek/ITS_NJDOT/c4842a30a369cd431264716ed20b1c7446992320/Resources/SCATS%C2%AE%20Troubleshooting.pdf", "SCATS Troubleshooting"),
                tags$br(),
                tags$a(href="https://rawcdn.githack.com/abdallahshabarek/ITS_NJDOT/c4842a30a369cd431264716ed20b1c7446992320/Resources/SCATS%C2%AE.pdf", "SCATS Presentation"),
                tags$br(),
                tags$a(href="https://rawcdn.githack.com/abdallahshabarek/ITS_NJDOT/01cbea9a015ed13fe7a1f6bbba5c021338d08a31/Resources/SCATS%20Traffic%20Reporter%20user%20manual.pdf", "SCATS Traffic Reporter User Manual"),
                tags$br(),
                tags$a(href="https://rawcdn.githack.com/abdallahshabarek/ITS_NJDOT/01cbea9a015ed13fe7a1f6bbba5c021338d08a31/Resources/ATMS%20Rules%201.1.rwz", "ATMS Rules")
              ),
              mainPanel(img(src = "https://www.limsforum.com/wp-content/uploads/uloo0b2c.bmp", height = 200, width = 200))
            )
          )
  )
  
  
  ),hr(),
  h6(print(r2symbols::symbol("copyright")),print(" Abdullah Shabarek"),
     style = "font-size:10px;")
  )
,theme = shinytheme("darkly"))

library(shiny)
library(dplyr)
options(shiny.maxRequestSize = 100*1024^2)
server <-  function(input, output) {
  getData <- reactive({
    inFile <- input$file1
    idx <- 0
    if (is.null(inFile)){
      return(NULL)
    }else {  
      
      # Site_ID <- as.integer(gsub(".*Site_*|.csv*", "", input$file_name))
      files3 = lapply(inFile$datapath, function(y){
        progress <- shiny::Progress$new()
        # Make sure it closes when we exit this reactive, even if there's an error
        on.exit(progress$close())
        
        progress$set(message = "Analyizing the data....", value = 0)
        idx <<- idx + 1
        df = read.csv(y, header = TRUE)
        
        Site_ID <- as.integer(gsub(".*Site_*|.csv*", "", inFile$name[idx]))
        # Site_ID_Phase <- as.integer(gsub(".*Site_*|_Phase.csv*", "", Phase_Link))
        
        
        
        # df$Site_ID <- Site_ID
        
        df2 <- as.data.frame(gsub("\\:.*", "", df$Event))
        df2 = cbind(Site_ID = Site_ID, df2)
        colnames (df2) <- c("Site_ID","Event")
        df2$Info <- gsub(".*:", "", df$Event)
        # colnames (df2) <- c("Site_ID","Event","Info")
        
        uniquevalues <- as.data.frame(unique(df2[,"Event"]))
        colnames (uniquevalues) <- "Unique_Values"
        
        df2$Date <- substr(df$Time, 0, 10)
        df2$Tim <- substr(df$Time, 12, 19)
        df2$Event_Type <- df2$Event
        # colnames(df2)[3] <- "Info"
        # df2$Info <- df2$Info
        
        df_phase_termination <- df2 %>% filter(df2$Event == "Phase termination")
        if( nrow(df_phase_termination) > 1){
        df_phase_termination$Phase <- substr(df_phase_termination$Info, 19,19)
        df4 <- as.data.frame(sub(".*MX=([^.]+)\\, GT.*", "\\1", df_phase_termination$Info))
        colnames (df4) <- "Max_Value"
        df_phase_termination$Max_Value <- df4$Max_Value
        # df_phase_termination$ti <- as.POSIXct(df_phase_termination$Tim,format="%H:%M:%S")
        
        df_phase_termination$ti <- as_hms(df_phase_termination$Tim)
        df_phase_termination <- df_phase_termination %>% filter(df_phase_termination$ti > as_hms(input$lowertime))
        df_phase_termination <- df_phase_termination %>% filter(df_phase_termination$ti < as_hms(input$uppertime))
        df_phase_termination$Max_Value <- as.integer(df_phase_termination$Max_Value)
        for (i in (1:nrow(df_phase_termination))){
          df_phase_termination$Time_Start[i] <- as_hms(as.integer(df_phase_termination$ti[i]/(as.numeric(1)*60))*(as.numeric(1)*60))
        }
        df_phase_termination$Time_Start <- as_hms(df_phase_termination$Time_Start)
        
        
        

        
        
        # df_phase_termination <- df_phase_termination[,2:ncol(df_phase_termination)]
        df_phase_termination <- df_phase_termination
        } else {
          df_phase_termination <- NULL
        } 
        df_phase_termination <- df_phase_termination
        
        
        df_cycle_length <- df2 %>% filter(df2$Event == "Cycle length")
        if( nrow(df_cycle_length) >1) {
        # Getting the Cycle Length Information
        df_cycle_length$cycle_type <- NULL
        df_cycle_length$cycle_type <- gsub("=.*", "", df_cycle_length$Info)
        
        df_cycle_length$cycle_length <- NULL
        df_cycle_length$cycle_length <- gsub(".*=", "", df_cycle_length$Info)
        
        input$lowertime
        input$uppertime
        # Creating a 24-timee periods on a 1 minute based
        number_of_seconds <- as.integer(as_hms(input$uppertime) - as_hms(input$lowertime))
        number_of_minutes <- as.integer(number_of_seconds/60)
        df_time <- NULL
        df_time <- data.frame(matrix(, nrow=1440, ncol=1))
        colnames(df_time) <- "tim"
        
        df_time$tim[1] <- as.integer(as_hms("00:00:00"))
        for (jj in (2:1440-1)){
          df_time$tim[jj+1] <- df_time$tim[jj]+60
        }
        df_time$Time <- NULL
        df_time$Time <- as_hms(df_time$tim)
        
        # Dividing the Data into Nominal and Active Cycle Length
        df_cycle_length$cycle_type <- trimws(df_cycle_length$cycle_type, which = c("both"))
        df_cycle_length_nominal <- df_cycle_length %>% filter(df_cycle_length$cycle_type == "Nominal")
        df_cycle_length_active <- df_cycle_length %>% filter(df_cycle_length$cycle_type == "Active")
        
        # Working on the Nominal Cycle Length
        df_cycle_length_nominal$Time <- NULL
        df_cycle_length_nominal$Time <- as.integer(as.integer(as_hms(df_cycle_length_nominal$Tim))/60)*60
        
        df_cycle_length_nominal2 <- left_join(df_time, df_cycle_length_nominal, by = c("tim" = "Time"))#, "y" = "y2"))
        df_cycle_length_nominal2 <- df_cycle_length_nominal2 %>% tidyr::fill(Site_ID)
        df_cycle_length_nominal2 <- df_cycle_length_nominal2 %>% tidyr::fill(Event)
        df_cycle_length_nominal2 <- df_cycle_length_nominal2 %>% tidyr::fill(Info)
        df_cycle_length_nominal2 <- df_cycle_length_nominal2 %>% tidyr::fill(Date)
        # df_cycle_length_nominal2 <- df_cycle_length_nominal2 %>% tidyr::fill(Tim)
        df_cycle_length_nominal2 <- df_cycle_length_nominal2 %>% tidyr::fill(Event_Type)
        df_cycle_length_nominal2 <- df_cycle_length_nominal2 %>% tidyr::fill(cycle_type)
        df_cycle_length_nominal2 <- df_cycle_length_nominal2 %>% tidyr::fill(cycle_length)
        
        df_cycle_length_nominal2 <- df_cycle_length_nominal2 %>% tidyr::fill(Site_ID, .direction = "up")
        df_cycle_length_nominal2 <- df_cycle_length_nominal2 %>% tidyr::fill(Event, .direction = "up")
        df_cycle_length_nominal2 <- df_cycle_length_nominal2 %>% tidyr::fill(Info, .direction = "up")
        df_cycle_length_nominal2 <- df_cycle_length_nominal2 %>% tidyr::fill(Date, .direction = "up")
        # df_cycle_length_nominal2 <- df_cycle_length_nominal2 %>% tidyr::fill(Tim)
        df_cycle_length_nominal2 <- df_cycle_length_nominal2 %>% tidyr::fill(Event_Type, .direction = "up")
        df_cycle_length_nominal2 <- df_cycle_length_nominal2 %>% tidyr::fill(cycle_type, .direction = "up")
        df_cycle_length_nominal2 <- df_cycle_length_nominal2 %>% tidyr::fill(cycle_length, .direction = "up")
        
        # Working on the Active Cycle Length
        df_cycle_length_active$Time <- NULL
        df_cycle_length_active$Time <- as.integer(as.integer(as_hms(df_cycle_length_active$Tim))/60)*60
        
        df_cycle_length_active2 <- left_join(df_time, df_cycle_length_active, by = c("tim" = "Time"))#, "y" = "y2"))
        df_cycle_length_active2 <- df_cycle_length_active2 %>% tidyr::fill(Site_ID)
        df_cycle_length_active2 <- df_cycle_length_active2 %>% tidyr::fill(Event)
        df_cycle_length_active2 <- df_cycle_length_active2 %>% tidyr::fill(Info)
        df_cycle_length_active2 <- df_cycle_length_active2 %>% tidyr::fill(Date)
        # df_cycle_length_nominal2 <- df_cycle_length_nominal2 %>% tidyr::fill(Tim)
        df_cycle_length_active2 <- df_cycle_length_active2 %>% tidyr::fill(Event_Type)
        df_cycle_length_active2 <- df_cycle_length_active2 %>% tidyr::fill(cycle_type)
        df_cycle_length_active2 <- df_cycle_length_active2 %>% tidyr::fill(cycle_length)
        
        df_cycle_length_active2 <- df_cycle_length_active2 %>% tidyr::fill(Site_ID, .direction = "up")
        df_cycle_length_active2 <- df_cycle_length_active2 %>% tidyr::fill(Event, .direction = "up")
        df_cycle_length_active2 <- df_cycle_length_active2 %>% tidyr::fill(Info, .direction = "up")
        df_cycle_length_active2 <- df_cycle_length_active2 %>% tidyr::fill(Date, .direction = "up")
        # df_cycle_length_nominal2 <- df_cycle_length_nominal2 %>% tidyr::fill(Tim)
        df_cycle_length_active2 <- df_cycle_length_active2 %>% tidyr::fill(Event_Type, .direction = "up")
        df_cycle_length_active2 <- df_cycle_length_active2 %>% tidyr::fill(cycle_type, .direction = "up")
        df_cycle_length_active2 <- df_cycle_length_active2 %>% tidyr::fill(cycle_length, .direction = "up")
        
        # Keeping only 1440 rows (one-minute based)
        df_cycle_length_active2 <- df_cycle_length_active2[!duplicated(df_cycle_length_active2$Time),]
        df_cycle_length_nominal2 <- df_cycle_length_nominal2[!duplicated(df_cycle_length_nominal2$Time),]
        
        # Merging Active and Nominal dataframes
        df_cycle_length_active3 <- df_cycle_length_active2 %>% select(cycle_length)
        colnames(df_cycle_length_active3) <- c("cycle_length_active")
        df_cycle_length_nominal2
        colnames(df_cycle_length_nominal2) <- c("tim", "Time", "Site_ID", "Event", "Info",
                                                "Date","Tim","Event_Type","cycle_type",
                                                "cycle_length_nominal")
        
        df_cycle_length2 <- cbind(df_cycle_length_nominal2, df_cycle_length_active3)
        df_cycle_length2$diff_nominal_active <- NULL
        df_cycle_length2$diff_nominal_active <- as.integer(df_cycle_length2$cycle_length_nominal) - as.integer(df_cycle_length2$cycle_length_active)
        
        # Preparing Cycle_length to be joined
        df_cycle_length_reduced <- df_cycle_length2 %>% select(Time, cycle_length_active,
                                                               cycle_length_nominal,diff_nominal_active)
        colnames(df_cycle_length_reduced) <- c("Time_Start","cycle_length_active",
                                               "cycle_length_nominal","diff_nominal_active")
        for (k in (1:nrow(df_cycle_length_reduced))){
         if(is.na(df_cycle_length_reduced$cycle_length_nominal[k]) == TRUE){
           df_cycle_length_reduced$cycle_length_nominal [k] <- df_cycle_length_reduced$cycle_length_active [k]
         }
        }
        } else {df_cycle_length_reduced <- NULL
        }
        
        
        
        if (is.null(df_phase_termination) | is.null(df_cycle_length_reduced)){
          df_phase_termination2 <- NULL
        } else {
          # Joinin the Cycle length data with 
          df_phase_termination2 <- dplyr::left_join(df_phase_termination, df_cycle_length_reduced, by = c("Time_Start"="Time_Start"))
          df_phase_termination2
        }
        
        df_phase_termination2
        # Joinin the Cycle length data with 
        # df_phase_termination2 <- dplyr::left_join(df_phase_termination, df_cycle_length_reduced, by = c("Time_Start"="Time_Start"))
        # df_phase_termination2
        
        # lastrow = nrow(JSON_csv)
        # shift = function(x, n){
        #   c(x[-(seq(n))], rep(NA, n))
        # }
        # JSON_csv$companyID1 = shift(JSON_csv$companyID1, 1)
        # JSON_csv = JSON_csv[-lastrow, ]
        # JSON_csv 
      }
      
      )
      do.call(rbind, files3)
    }
  })
 
  getData2 <- reactive({
    inFile2 <- input$file2
    index <- 0
    if (is.null(inFile2)){
      return(NULL)
    }else {
      # Site_ID_Phase <- as.integer(gsub(".*Site_*|_Phase.csv*", "", input$file_name2))
      # print(Site_ID_Phase)
      # Site_ID_Phase <- as.integer(gsub(".*Site_*|_Phase.csv*", "", (inFile2$name)))


      files4 = lapply(inFile2$datapath, function(yy){
        index <<- index + 1
        dff = read.csv(yy, header = TRUE)
        # print(inFile2$datapath)
        # print(yy)
        # Site_ID <- as.integer(gsub(".*Site_*|.csv*", "", y))
        # Site_ID_Phase <- as.integer(gsub(".*Site_*|_Phase.csv*", "", yy)) #inFile2$name))
        Site_ID_Phase <- as.integer(gsub(".*Site_*|_Phase.csv*", "", inFile2$name[index]))
        # print(inFile2$name)
        # print(class(inFile2$name))
        dff = cbind(Site_ID = Site_ID_Phase, dff)
        print(inFile2$name[2])
        # dff$Site_ID <- Site_ID_Phase
        # print(nrow(dff))
        # list_rnow[[counter]] <- nrow(dff)
        # counter = counter +1
        # print(list_rnow)



        # for (kk in (1:nrow(df))){
        #     dff$Site_ID[kk]() <- inFile2$name[1]
        #   }


        # Site_ID_Phase <- as.integer(gsub(".*Site_*|_Phase.csv*", "",
        #   input$file_input$file_name2))
        # Site_ID_Phase <- as.integer(gsub(".*Site_*|_Phase.csv*", "", (inFile2$name)))
        # assign(dff$Site_ID, as.integer(gsub(".*Site_*|_Phase.csv*", "", (inFile2$name))))
        # dff <- readr::read_csv(Phase_Link)

        dff <- dff %>% dplyr::filter(as.data.frame(dff$Phase) !="Unknown")
        colnames(dff)[3] <- "Duration"


        Number_of_phases <- 0

        if(nrow(dff %>% dplyr::filter(as.data.frame(dff$Phase) =="<A>"))>1){
          Number_of_phases = 1
        } else {
          Number_of_phases = 0
        }

        if(nrow(dff %>% dplyr::filter(as.data.frame(dff$Phase) =="B"))>1){
          Number_of_phases = Number_of_phases+1
        }

        if(nrow(dff %>% dplyr::filter(as.data.frame(dff$Phase) =="C"))>1){
          Number_of_phases = Number_of_phases+1
        }

        if(nrow(dff %>% dplyr::filter(as.data.frame(dff$Phase) =="D"))>1){
          Number_of_phases = Number_of_phases+1
        }

        if(nrow(dff %>% dplyr::filter(as.data.frame(dff$Phase) =="E"))>1){
          Number_of_phases = Number_of_phases+1
        }

        if(nrow(dff %>% dplyr::filter(as.data.frame(dff$Phase) =="F"))>1){
          Number_of_phases = Number_of_phases+1
        }

        if(nrow(dff %>% dplyr::filter(as.data.frame(dff$Phase) =="G"))>1){
          Number_of_phases = Number_of_phases+1
        }
        if(nrow(dff %>% dplyr::filter(as.data.frame(dff$Phase) =="H"))>1){
          Number_of_phases = Number_of_phases+1
        }
        if(nrow(dff %>% dplyr::filter(as.data.frame(dff$Phase) =="I"))>1){
          Number_of_phases = Number_of_phases+1
        }
        if(nrow(dff %>% dplyr::filter(as.data.frame(dff$Phase) =="J"))>1){
          Number_of_phases = Number_of_phases+1
        }

        dff <- dff %>%
          mutate(Phasenumber = case_when(
            endsWith(Phase, "<A>") ~ as.integer(1),
            endsWith(Phase, "B") ~ as.integer(2),
            endsWith(Phase, "C") ~ as.integer(3),
            endsWith(Phase, "D") ~ as.integer(4),
            endsWith(Phase, "E") ~ as.integer(5),
            endsWith(Phase, "F") ~ as.integer(6),
            endsWith(Phase, "G") ~ as.integer(7),
            endsWith(Phase, "H") ~ as.integer(8),
            endsWith(Phase, "I") ~ as.integer(9),
            endsWith(Phase, "J") ~ as.integer(10),
            endsWith(Phase, "K") ~ as.integer(11)
          ))
        
        if (nrow(dff) > 1) {
        for (i in (1:5)){
          if(dff$Phasenumber[1] != 1){
            dff <- dff[2:nrow(dff),]
          }
        }
        # dff <- dff

        dff$Cycle_number <- NULL
        dff <- dff
        for (i in (1:nrow(dff))){

          if (i == 1){
            dff$Cycle_number[i] <- 1
          } else{

            if(as.integer(dff$Phasenumber[i]) == 1 ){
              dff$Cycle_number[i] <- dff$Cycle_number[as.integer(i-1)]+1

            } else {
              dff$Cycle_number[i] <- dff$Cycle_number[as.integer(i-1)]
            }
          }
        }

        # dff_grouped <- NULL
        # dff_grouped$Cycle_length <- NULL

        # dff <- dff
        # dff$"Duration..s." <- as.integer(dff$"Duration..s.")


        dff_grouped <- dff %>% group_by(Cycle_number,Site_ID) %>% summarise(
          Cycle_length = sum(`Duration`))

        dff <- merge(x = dff, y = dff_grouped, by = c("Cycle_number","Site_ID"), all.x = TRUE)

        dff$Phase_skip <- NULL
        dff$Main_Approach_Gapped <- NULL

        coef1 <- as.numeric(input$coef1)
        coef2 <- as.numeric(input$coef2)
        coef3 <- as.numeric(input$coef1) +1
        coef4 <- as.numeric(input$coef2) +1
        coef5 <- as.numeric(input$coef1) +2
        coef6 <- as.numeric(input$coef2) +2
        coef7 <- as.numeric(input$coef1) +3
        coef8 <- as.numeric(input$coef2) +3
        coef9 <- as.numeric(input$coef1) +4
        coef10 <- as.numeric(input$coef2) +4
        max_phase_number <- as.integer(max(dff$Phasenumber))

        for (i in (1:nrow(dff))){
          if( i < max_phase_number+1) {
            if((
              # (((dff$Cycle_length[i]-dff$Cycle_length[i+1])/dff$Cycle_length[i+1]) > coef3 && ((dff$Cycle_length[i]-dff$Cycle_length[i+1])/dff$Cycle_length[i+1]) < coef4 | ((dff$Cycle_length[i]-dff$Cycle_length[i+1])/dff$Cycle_length[i+1]) > coef5 && ((dff$Cycle_length[i]-dff$Cycle_length[i+1])/dff$Cycle_length[i+1]) < coef6 | ((dff$Cycle_length[i]-dff$Cycle_length[i+1])/dff$Cycle_length[i+1]) > coef7 && ((dff$Cycle_length[i]-dff$Cycle_length[i+1])/dff$Cycle_length[i+1]) < coef8 | ((dff$Cycle_length[i]-dff$Cycle_length[i+1])/dff$Cycle_length[i+1]) > coef9 && ((dff$Cycle_length[i]-dff$Cycle_length[i+1])/dff$Cycle_length[i+1]) < coef10)  |
              (((dff$Cycle_length[i]-dff$Cycle_length[i+max_phase_number])/dff$Cycle_length[i+max_phase_number]) > coef3 && ((dff$Cycle_length[i]-dff$Cycle_length[i+max_phase_number])/dff$Cycle_length[i+max_phase_number]) < coef4 | ((dff$Cycle_length[i]-dff$Cycle_length[i+max_phase_number])/dff$Cycle_length[i+max_phase_number]) > coef5 && ((dff$Cycle_length[i]-dff$Cycle_length[i+max_phase_number])/dff$Cycle_length[i+max_phase_number]) < coef6 | ((dff$Cycle_length[i]-dff$Cycle_length[i+max_phase_number])/dff$Cycle_length[i+max_phase_number]) > coef7 && ((dff$Cycle_length[i]-dff$Cycle_length[i+max_phase_number])/dff$Cycle_length[i+max_phase_number]) < coef8 | ((dff$Cycle_length[i]-dff$Cycle_length[i+max_phase_number])/dff$Cycle_length[i+max_phase_number]) > coef9 && ((dff$Cycle_length[i]-dff$Cycle_length[i+max_phase_number])/dff$Cycle_length[i+max_phase_number]) < coef10))
              # && (is.na(dff$Gapped[i]) == FALSE && dff$Phasenumber[i] != 1)
            )
            {
              dff$Phase_skip[i] <- 1
            } else {
              dff$Phase_skip[i] <- 0
            }
          } else if ( i > nrow(dff)-max_phase_number ){
            if((
              # (((dff$Cycle_length[i]-dff$Cycle_length[i-1])/dff$Cycle_length[i-1]) > coef3 && ((dff$Cycle_length[i]-dff$Cycle_length[i-1])/dff$Cycle_length[i-1]) < coef4 | ((dff$Cycle_length[i]-dff$Cycle_length[i-1])/dff$Cycle_length[i-1]) > coef5 && ((dff$Cycle_length[i]-dff$Cycle_length[i-1])/dff$Cycle_length[i-1]) < coef6 | ((dff$Cycle_length[i]-dff$Cycle_length[i-1])/dff$Cycle_length[i-1]) > coef7 && ((dff$Cycle_length[i]-dff$Cycle_length[i-1])/dff$Cycle_length[i-1]) < coef8 | ((dff$Cycle_length[i]-dff$Cycle_length[i-1])/dff$Cycle_length[i-1]) > coef9 && ((dff$Cycle_length[i]-dff$Cycle_length[i-1])/dff$Cycle_length[i-1]) < coef10) |
              (((dff$Cycle_length[i]-dff$Cycle_length[i-max_phase_number])/dff$Cycle_length[i-max_phase_number]) > coef3 && ((dff$Cycle_length[i]-dff$Cycle_length[i-max_phase_number])/dff$Cycle_length[i-max_phase_number]) < coef4 | ((dff$Cycle_length[i]-dff$Cycle_length[i-max_phase_number])/dff$Cycle_length[i-max_phase_number]) > coef5 && ((dff$Cycle_length[i]-dff$Cycle_length[i-max_phase_number])/dff$Cycle_length[i-max_phase_number]) < coef6 | ((dff$Cycle_length[i]-dff$Cycle_length[i-max_phase_number])/dff$Cycle_length[i-max_phase_number]) > coef7 && ((dff$Cycle_length[i]-dff$Cycle_length[i-max_phase_number])/dff$Cycle_length[i-max_phase_number]) < coef8 | ((dff$Cycle_length[i]-dff$Cycle_length[i-max_phase_number])/dff$Cycle_length[i-max_phase_number]) > coef9 && ((dff$Cycle_length[i]-dff$Cycle_length[i-max_phase_number])/dff$Cycle_length[i-max_phase_number]) < coef10))
              # && (is.na(dff$Gapped[i]) == FALSE && dff$Phasenumber[i] != 1)
            ){
              dff$Phase_skip[i] <- 1
            } else {
              dff$Phase_skip[i] <- 0
            }
          } else {if((
            # (((dff$Cycle_length[i]-dff$Cycle_length[i-1])/dff$Cycle_length[i-1]) > coef1 && ((dff$Cycle_length[i]-dff$Cycle_length[i-1])/dff$Cycle_length[i-1]) < coef2 | ((dff$Cycle_length[i]-dff$Cycle_length[i-1])/dff$Cycle_length[i-1]) > coef3 && ((dff$Cycle_length[i]-dff$Cycle_length[i-1])/dff$Cycle_length[i-1]) < coef4 | ((dff$Cycle_length[i]-dff$Cycle_length[i-1])/dff$Cycle_length[i-1]) > coef5 && ((dff$Cycle_length[i]-dff$Cycle_length[i-1])/dff$Cycle_length[i-1]) < coef6 | ((dff$Cycle_length[i]-dff$Cycle_length[i-1])/dff$Cycle_length[i-1]) > coef7 && ((dff$Cycle_length[i]-dff$Cycle_length[i-1])/dff$Cycle_length[i-1]) < coef8 | ((dff$Cycle_length[i]-dff$Cycle_length[i-1])/dff$Cycle_length[i-1]) > coef9 && ((dff$Cycle_length[i]-dff$Cycle_length[i-1])/dff$Cycle_length[i-1]) < coef10) |
            (((dff$Cycle_length[i]-dff$Cycle_length[i-max_phase_number])/dff$Cycle_length[i-max_phase_number]) > coef1 && ((dff$Cycle_length[i]-dff$Cycle_length[i-max_phase_number])/dff$Cycle_length[i-max_phase_number]) < coef2 | ((dff$Cycle_length[i]-dff$Cycle_length[i-max_phase_number])/dff$Cycle_length[i-max_phase_number]) > coef3 && ((dff$Cycle_length[i]-dff$Cycle_length[i-max_phase_number])/dff$Cycle_length[i-max_phase_number]) < coef4 | ((dff$Cycle_length[i]-dff$Cycle_length[i-max_phase_number])/dff$Cycle_length[i-max_phase_number]) > coef5 && ((dff$Cycle_length[i]-dff$Cycle_length[i-max_phase_number])/dff$Cycle_length[i-max_phase_number]) < coef6 | ((dff$Cycle_length[i]-dff$Cycle_length[i-max_phase_number])/dff$Cycle_length[i-max_phase_number]) > coef7 && ((dff$Cycle_length[i]-dff$Cycle_length[i-max_phase_number])/dff$Cycle_length[i-max_phase_number]) < coef8 | ((dff$Cycle_length[i]-dff$Cycle_length[i-max_phase_number])/dff$Cycle_length[i-max_phase_number]) > coef9 && ((dff$Cycle_length[i]-dff$Cycle_length[i-max_phase_number])/dff$Cycle_length[i-max_phase_number]) < coef10) |
            # (((dff$Cycle_length[i]-dff$Cycle_length[i+1])/dff$Cycle_length[i+1]) > coef1 && ((dff$Cycle_length[i]-dff$Cycle_length[i+1])/dff$Cycle_length[i+1]) < coef2 | ((dff$Cycle_length[i]-dff$Cycle_length[i+1])/dff$Cycle_length[i+1]) > coef3 && ((dff$Cycle_length[i]-dff$Cycle_length[i+1])/dff$Cycle_length[i+1]) < coef4 | ((dff$Cycle_length[i]-dff$Cycle_length[i+1])/dff$Cycle_length[i+1]) > coef5 && ((dff$Cycle_length[i]-dff$Cycle_length[i+1])/dff$Cycle_length[i+1]) < coef6 | ((dff$Cycle_length[i]-dff$Cycle_length[i+1])/dff$Cycle_length[i+1]) > coef7 && ((dff$Cycle_length[i]-dff$Cycle_length[i+1])/dff$Cycle_length[i+1]) < coef8 | ((dff$Cycle_length[i]-dff$Cycle_length[i+1])/dff$Cycle_length[i+1]) > coef9 && ((dff$Cycle_length[i]-dff$Cycle_length[i+1])/dff$Cycle_length[i+1]) < coef10) |
            (((dff$Cycle_length[i]-dff$Cycle_length[i+max_phase_number])/dff$Cycle_length[i+max_phase_number]) > coef1 && ((dff$Cycle_length[i]-dff$Cycle_length[i+max_phase_number])/dff$Cycle_length[i+max_phase_number]) < coef2 | ((dff$Cycle_length[i]-dff$Cycle_length[i+max_phase_number])/dff$Cycle_length[i+max_phase_number]) > coef3 && ((dff$Cycle_length[i]-dff$Cycle_length[i+max_phase_number])/dff$Cycle_length[i+max_phase_number]) < coef4 | ((dff$Cycle_length[i]-dff$Cycle_length[i+max_phase_number])/dff$Cycle_length[i+max_phase_number]) > coef5 && ((dff$Cycle_length[i]-dff$Cycle_length[i+max_phase_number])/dff$Cycle_length[i+max_phase_number]) < coef6 | ((dff$Cycle_length[i]-dff$Cycle_length[i+max_phase_number])/dff$Cycle_length[i+max_phase_number]) > coef7 && ((dff$Cycle_length[i]-dff$Cycle_length[i+max_phase_number])/dff$Cycle_length[i+max_phase_number]) < coef8 | ((dff$Cycle_length[i]-dff$Cycle_length[i+max_phase_number])/dff$Cycle_length[i+max_phase_number]) > coef9 && ((dff$Cycle_length[i]-dff$Cycle_length[i+max_phase_number])/dff$Cycle_length[i+max_phase_number]) < coef10))
            # && (is.na(dff$Gapped[i]) == FALSE && dff$Phasenumber[i] != 1)
          ){
            dff$Phase_skip[i] <- 1
          } else {
            dff$Phase_skip[i] <- 0
          }
          }

        }

        for (i in (1:nrow(dff))){
          if(is.na(dff$Gapped[i]) && dff$Phasenumber[i] == 1){
            dff$Main_Approach_Gapped[i] <- 0
          } else if (dff$Phasenumber[i] != 1){
            dff$Main_Approach_Gapped[i] <- 0
          } else {
            dff$Main_Approach_Gapped[i] <- 1
          }
        }

        # # i <- 37
        # # if((( ((dff$Cycle_length[i]-dff$Cycle_length[i-1])/dff$Cycle_length[i-1]) > 0.9 && ((dff$Cycle_length[i]-dff$Cycle_length[i-1])/dff$Cycle_length[i-1]) < 1.1 | ((dff$Cycle_length[i]-dff$Cycle_length[i-1])/dff$Cycle_length[i-1]) > 1.9 && ((dff$Cycle_length[i]-dff$Cycle_length[i-1])/dff$Cycle_length[i-1]) < 2.1 | ((dff$Cycle_length[i]-dff$Cycle_length[i-1])/dff$Cycle_length[i-1]) > 2.9 && ((dff$Cycle_length[i]-dff$Cycle_length[i-1])/dff$Cycle_length[i-1]) < 3.1 | ((dff$Cycle_length[i]-dff$Cycle_length[i-1])/dff$Cycle_length[i-1]) > 3.9 && ((dff$Cycle_length[i]-dff$Cycle_length[i-1])/dff$Cycle_length[i-1]) < 4.1 | ((dff$Cycle_length[i]-dff$Cycle_length[i-1])/dff$Cycle_length[i-1]) > 4.9 && ((dff$Cycle_length[i]-dff$Cycle_length[i-1])/dff$Cycle_length[i-1]) < 5.1)
        # #     | (((dff$Cycle_length[i]-dff$Cycle_length[i-2])/dff$Cycle_length[i-2]) > 0.9 && ((dff$Cycle_length[i]-dff$Cycle_length[i-2])/dff$Cycle_length[i-2]) < 1.1 | ((dff$Cycle_length[i]-dff$Cycle_length[i-2])/dff$Cycle_length[i-2]) > 1.9 && ((dff$Cycle_length[i]-dff$Cycle_length[i-2])/dff$Cycle_length[i-2]) < 2.1 | ((dff$Cycle_length[i]-dff$Cycle_length[i-2])/dff$Cycle_length[i-2]) > 2.9 && ((dff$Cycle_length[i]-dff$Cycle_length[i-2])/dff$Cycle_length[i-2]) < 3.1 | ((dff$Cycle_length[i]-dff$Cycle_length[i-2])/dff$Cycle_length[i-2]) > 3.9 && ((dff$Cycle_length[i]-dff$Cycle_length[i-2])/dff$Cycle_length[i-2]) < 4.1 | ((dff$Cycle_length[i]-dff$Cycle_length[i-2])/dff$Cycle_length[i-2]) > 4.9 && ((dff$Cycle_length[i]-dff$Cycle_length[i-2])/dff$Cycle_length[i-2]) < 5.1)
        # #     | (((dff$Cycle_length[i]-dff$Cycle_length[i+1])/dff$Cycle_length[i+1]) > 0.9 && ((dff$Cycle_length[i]-dff$Cycle_length[i+1])/dff$Cycle_length[i+1]) < 1.1 | ((dff$Cycle_length[i]-dff$Cycle_length[i+1])/dff$Cycle_length[i+1]) > 1.9 && ((dff$Cycle_length[i]-dff$Cycle_length[i+1])/dff$Cycle_length[i+1]) < 2.1 | ((dff$Cycle_length[i]-dff$Cycle_length[i+1])/dff$Cycle_length[i+1]) > 2.9 && ((dff$Cycle_length[i]-dff$Cycle_length[i+1])/dff$Cycle_length[i+1]) < 3.1 | ((dff$Cycle_length[i]-dff$Cycle_length[i+1])/dff$Cycle_length[i+1]) > 3.9 && ((dff$Cycle_length[i]-dff$Cycle_length[i+1])/dff$Cycle_length[i+1]) < 4.1 | ((dff$Cycle_length[i]-dff$Cycle_length[i+1])/dff$Cycle_length[i+1]) > 4.9 && ((dff$Cycle_length[i]-dff$Cycle_length[i+1])/dff$Cycle_length[i+1]) < 5.1)
        # #     | (((dff$Cycle_length[i]-dff$Cycle_length[i+2])/dff$Cycle_length[i+2]) > 0.9 && ((dff$Cycle_length[i]-dff$Cycle_length[i+2])/dff$Cycle_length[i+2]) < 1.1 | ((dff$Cycle_length[i]-dff$Cycle_length[i+2])/dff$Cycle_length[i+2]) > 1.9 && ((dff$Cycle_length[i]-dff$Cycle_length[i+2])/dff$Cycle_length[i+2]) < 2.1 | ((dff$Cycle_length[i]-dff$Cycle_length[i+2])/dff$Cycle_length[i+2]) > 2.9 && ((dff$Cycle_length[i]-dff$Cycle_length[i+2])/dff$Cycle_length[i+2]) < 3.1 | ((dff$Cycle_length[i]-dff$Cycle_length[i+2])/dff$Cycle_length[i+2]) > 3.9 && ((dff$Cycle_length[i]-dff$Cycle_length[i+2])/dff$Cycle_length[i+2]) < 4.1 | ((dff$Cycle_length[i]-dff$Cycle_length[i+2])/dff$Cycle_length[i+2]) > 4.9 && ((dff$Cycle_length[i]-dff$Cycle_length[i+2])/dff$Cycle_length[i+2]) < 5.1))
        # #    # && (is.na(dff$Gapped[i])== FALSE && dff$Phasenumber[i] != 1)
        # #    ){
        # #   dff$Phase_skip[i] <- 1
        # #   print("HI")
        # # } else {
        # #   dff$Phase_skip[i] <- 0
        # # }
        #
        dff$Date <- substr(dff$Start, 0, 10)
        dff$Time_Start <- substr(dff$Start, 12, 19)
        dff$Time_End <- substr(dff$End, 12, 19)


        dff$Time_Start <- as_hms(dff$Time_Start)
        dff$Time_End <- as_hms(dff$Time_End)

        for (i in (1:nrow(dff))){
          dff$Time_rounded[i] <- as_hms(as.integer(dff$Time_Start[i]/(as.numeric(input$rounded_every)*60))*(as.numeric(input$rounded_every)*60))
        }
        dff$Time_rounded <- as_hms(dff$Time_rounded)
        dff_grouped2 <- dff %>% group_by(Time_rounded, Site_ID) %>% summarise(
          max_cycle_length = max(`Cycle_length`))

        dff_grouped2 <- dff_grouped2
        } else {
          dff_grouped2 <- NULL
        }


        # # lastrow = nrow(JSON_csv)
        # # shift = function(x, n){
        # #   c(x[-(seq(n))], rep(NA, n))
        # # }
        # # JSON_csv$companyID1 = shift(JSON_csv$companyID1, 1)
        # # JSON_csv = JSON_csv[-lastrow, ]
        # # JSON_csv
      }

      )
      do.call(rbind, files4)

    }
  })

  
  
  output$contents <- renderTable( 
    getData() 
  )
  
  observeEvent(input$previewData, {
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    
    progress$set(message = "Creating a Heatmap....", value = 0)
    
      Github_Link <- "https://raw.githubusercontent.com/abdallahshabarek/ITS_NJDOT/main/SCATS_Variation_Routines.csv"
      df_Events <- getData() %>% filter(getData()$Max_Value > as.numeric(input$max_value_threshold))
      
      # df_final <- dplyr::left_join(df_Events, getData2(), by = c("Time_Start"="Time_rounded","Site_ID" = "Site_ID"))
      Sites_vr <- readr::read_csv(Github_Link)
      # df_final <- dplyr::left_join(df_final, Sites_vr, by = c("Site_ID" = "SCATS_Site_ID"))
      df_final <- dplyr::left_join(df_Events, Sites_vr, by = c("Site_ID" = "SCATS_Site_ID"))
      df_final$Cycle_operating <- NULL
      if (nrow(df_final) > 1) {
      for (i in (1:nrow(df_final))) {
        if(df_final$cycle_length_nominal[i] > df_final$VR57_Misol_cycle_length[i]){
          df_final$Cycle_operating[i] <- "MLink"
        } else {
          df_final$Cycle_operating[i] <- "MIsol"
        }
      }
        df_final <- df_final %>% dplyr::filter(Cycle_operating=="MLink" & Phase != "A")
        df_final$hour_time <- as_hms("00:00:00")
        for (j in (1:nrow(df_final))){
          df_final$hour_time[j] <- as_hms(as.integer(as_hms(df_final$Tim[j])/(as.numeric(3600)))*(as.numeric(3600)))
        }
        df_final$hour_time <- as_hms(df_final$hour_time)
        df_grouped <- df_final %>% group_by(Site_ID, hour_time) %>% count(`Site_ID`) %>% rename (mx_value_count = n)
        df_grouped <-df_grouped[order(df_grouped$hour_time),]
        df_pivoted <- df_grouped %>%
          pivot_wider(names_from = hour_time, values_from = mx_value_count, values_fill = 0)
        
        df_pivoted
      } else {
      }
      #For the heat map
      #https://stackoverflow.com/questions/18663159/conditional-coloring-of-cells-in-table
      # https://www.r-graph-gallery.com/215-the-heatmap-function.html

      # colSide <- brewer.pal(9, "Set1")[as.matrix(df_pivoted[1:nrow(df_pivoted),2:ncol(df_pivoted)])]
      # heatmap.2(as.matrix(df_pivoted[1:nrow(df_pivoted),2:ncol(df_pivoted)]), Colv = NA, Rowv = NA, scale="column",cexRow=1,cexCol =1, labRow=paste("site#", unlist(df_pivoted[,1]),sep=""))
      my_palette <- colorRampPalette(c("white", "orange", "red"))(n = 100)
      output$Plot <- renderPlot({
        print(heatmap.2(as.matrix(df_pivoted[1:nrow(df_pivoted),2:ncol(df_pivoted)]),
        col=my_palette,
        Colv = NA, Rowv = NA, scale="none",
        cexRow=0.8,cexCol =0.8, labRow=paste("site#", unlist(df_pivoted[,1]),sep="")))})
      graphics.off()
      # write.csv(df_pivoted, file, row.names=FALSE)
    })
  
  output$downloadData <- downloadHandler(
    filename = function() { 
      paste("data-", Sys.time(), ".csv", sep="")
    },
    content = function(file) { 
      progress <- shiny::Progress$new()
      # Make sure it closes when we exit this reactive, even if there's an error
      on.exit(progress$close())
      
      progress$set(message = "Analyizing the data....", value = 0)
      
      Github_Link <- "https://raw.githubusercontent.com/abdallahshabarek/ITS_NJDOT/main/SCATS_Variation_Routines.csv"
      df_Events <- getData() %>% filter(getData()$Max_Value > as.numeric(input$max_value_threshold))
      
      # df_final <- dplyr::left_join(df_Events, getData2(), by = c("Time_Start"="Time_rounded","Site_ID" = "Site_ID"))
      Sites_vr <- readr::read_csv(Github_Link)
      # df_final <- dplyr::left_join(df_final, Sites_vr, by = c("Site_ID" = "SCATS_Site_ID"))
      df_final <- dplyr::left_join(df_Events, Sites_vr, by = c("Site_ID" = "SCATS_Site_ID"))
      df_final$Cycle_operating <- NULL
      if (nrow(df_final) > 1) {
        for (i in (1:nrow(df_final))) {
          if(df_final$cycle_length_nominal[i] > df_final$VR57_Misol_cycle_length[i]){
            df_final$Cycle_operating[i] <- "MLink"
          } else {
            df_final$Cycle_operating[i] <- "MIsol"
          }
        }
        df_final <- df_final %>% dplyr::filter(Cycle_operating=="MLink" & Phase != "A")
        df_final$hour_time <- as_hms("00:00:00")
        for (j in (1:nrow(df_final))){
          df_final$hour_time[j] <- as_hms(as.integer(as_hms(df_final$Tim[j])/(as.numeric(3600)))*(as.numeric(3600)))
        }
        df_final$hour_time <- as_hms(df_final$hour_time)
        df_grouped <- df_final %>% group_by(Site_ID, hour_time) %>% count(`Site_ID`) %>% rename (mx_value_count = n)
        df_grouped <-df_grouped[order(df_grouped$hour_time),]
        df_pivoted <- df_grouped %>%
          pivot_wider(names_from = hour_time, values_from = mx_value_count, values_fill = 0)
        
        df_pivoted
      } else {
      }
      #For the heat map
      #https://stackoverflow.com/questions/18663159/conditional-coloring-of-cells-in-table
      # https://www.r-graph-gallery.com/215-the-heatmap-function.html
      
      # colSide <- brewer.pal(9, "Set1")[as.matrix(df_pivoted[1:nrow(df_pivoted),2:ncol(df_pivoted)])]
      #### heatmap(as.matrix(df_pivoted[1:nrow(df_pivoted),2:ncol(df_pivoted)]), Colv = NA, Rowv = NA, scale="column",cexRow=0.5,cexCol =0.5, labRow=paste("site#", unlist(df_pivoted[,1]),sep=""))
      #### output$Plot <- renderPlot({print(heatmap(as.matrix(df_pivoted[1:nrow(df_pivoted),2:ncol(df_pivoted)]), Colv = NA, Rowv = NA, scale="column",cexRow=0.5,cexCol =0.5, labRow=paste("site#", unlist(df_pivoted[,1]),sep="")))})
      #### graphics.off()
      write.csv(df_pivoted, file, row.names=FALSE)
    })
  
  #######################################################3
  
  output$downloadData3 <- downloadHandler(
    filename = function() { 
      paste("DetectorAlarms-", Sys.time(), ".csv", sep="")
    },
    content = function(file) { 
      progress <- shiny::Progress$new()
      # Make sure it closes when we exit this reactive, even if there's an error
      on.exit(progress$close())
      
      progress$set(message = "Analyizing the data....", value = 0)
      
      # Detectors_list <- "C:\\Users\\Abdullah.Shabarek\\Desktop\\DA_Analysis\\Detectors_List.csv"
      Detectors_list <- "https://raw.githubusercontent.com/abdallahshabarek/ITS_NJDOT/main/Detectors_List.csv"
      
      df <- readr::read_csv(input$file3$datapath)
      df_detector1 <- df %>% dplyr::filter(df$Event == "DA" & df$`+/-` != "-",!grepl("New: PB\\s*(.*?)\\s*", df$Details)) 
      # df_test <- df %>% dplyr::filter(df$Event =="DA" & !grepl("Cleared: PB\\s*(.*?)\\s*", df$Details))
      
      df_detector1$first_detector <- NULL
      df_detector1$second_detector <- NULL
      df_detector1_add <- data.frame(matrix(, nrow=nrow(df_detector1), ncol=2))
      colnames(df_detector1_add) <- c("first_detector","second_detector")
      df_detector1 <- cbind(df_detector1,df_detector1_add)
      
      for (i in 1:nrow(df_detector1)){
        text1 <- gsub(".*DA*-*", "", df_detector1$Details[i])
        if(grepl("-", substr(text1, 1, 2), fixed = TRUE) == TRUE){
          df_detector1$first_detector[i] <- as.integer(substr(text1, 1, 1))
        } else if (grepl(",", substr(text1, 1, 3), fixed = TRUE) == TRUE) {
          if(is.na(as.integer(substr(text1, 1, 2))) == FALSE){
            df_detector1$first_detector[i] <- as.integer(substr(text1, 1, 2))
          } else if (is.na(as.integer(substr(text1, 1, 2))) == TRUE){
            df_detector1$first_detector[i] <- as.integer(substr(text1, 1, 1))
          }
        } else if (grepl("-", substr(text1, 1, 2), fixed = TRUE) == FALSE) {
          df_detector1$first_detector[i] <- as.integer(substr(text1, 1, 2))
        }
      }
      
      # Important
      # https://stackoverflow.com/questions/39086400/extracting-a-string-between-other-two-strings-in-r
      # stringr::str_match(df_detector1$Details, "DA\\s*(.*?)\\s* ")[2,][2]
      # strsplit(stringr::str_match(df_detector1$Details, "DA\\s*(.*?)\\s* ")[43,][2], split = ",")
      # strsplit(stringr::str_match(df_detector1$Details, "DA\\s*(.*?)\\s* ")[43,][2], split = ",")[[1]]
      # length(strsplit(stringr::str_match(df_detector1$Details, "DA\\s*(.*?)\\s* ")[43,][2], split = ",")[[1]])
      # https://stackoverflow.com/questions/15347282/split-delimited-strings-in-a-column-and-insert-as-new-rows
      # data.frame(V1 = rep(df_detector1$V1, sapply(s, length)), V2 = unlist(s))
      
      s <- strsplit(stringr::str_match(df_detector1$Details, "DA\\s*(.*?)\\s* ")[,2], split = ",")
      df_detector1 <- data.frame(No. = rep(df_detector1$No., sapply(s, length)),
                                 Record = rep(df_detector1$Record, sapply(s, length)) ,
                                 Date = rep(df_detector1$Date, sapply(s, length)) ,
                                 Time = rep(df_detector1$Time, sapply(s, length)),
                                 Source = rep(df_detector1$Source, sapply(s, length)),
                                 Site = rep(df_detector1$Site, sapply(s, length)),
                                 Type = rep(df_detector1$"+/-", sapply(s, length)),
                                 Event = rep(df_detector1$Event, sapply(s, length)),
                                 Mode = rep(df_detector1$Mode, sapply(s, length)),
                                 User = rep(df_detector1$User, sapply(s, length)),
                                 Details = rep(df_detector1$Details, sapply(s, length)), 
                                 Detectors = unlist(s))
      ss <- strsplit(df_detector1$Detectors, split = "-")
      unlist(ss)[[1]]
      df_detector1 <- data.frame(df_detector1,gsub("-.*", "", df_detector1$Detectors),
                                 df_detector1 <- gsub(".*-", "", df_detector1$Detectors))
      colnames(df_detector1[11]) <- "First_detector"
      
      
      df_detector1$second_detector <- NULL
      for (i in 1:nrow(df_detector1)){
        text2 <- gsub(".*-", "", df_detector1$Details[i])
        if(grepl(")", substr(text2, 1, 2), fixed = TRUE) == TRUE){
          df_detector1$second_detector[i] <- as.integer(substr(text2, 1, 1))
        } else if (grepl(")", substr(text2, 1, 2), fixed = TRUE) == FALSE) {
          df_detector1$second_detector[i] <- as.integer(substr(text2, 1, 2))
        }
      }
      
      
      names(df_detector1)[names(df_detector1)=="gsub............df_detector1.Detectors."] <- "First"
      names(df_detector1)[names(df_detector1)=="df_detector1....gsub............df_detector1.Detectors."] <- "Last"
      df_detector1$rep <- NULL
      df_detector1$rep <- as.integer(as.integer(df_detector1$Last) - as.integer(df_detector1$First) +1)
      df_detector1$unique_col <- order(df_detector1$Date, df_detector1$Time, df_detector1$First)
      
      # https://stackoverflow.com/questions/18028297/duplicate-rows-based-on-field-in-r
      v <- do.call("c", (mapply(rep, c(df_detector1$No., df_detector1$Record, df_detector1$Date, df_detector1$Time, df_detector1$Source, df_detector1$Site, df_detector1$Type, df_detector1$Event, df_detector1$Mode, df_detector1$User, df_detector1$Details, df_detector1$Detectors, df_detector1$First, df_detector1$Last, df_detector1$second_detector, df_detector1$rep, df_detector1$unique_col), df_detector1$rep)))
      v <- t(matrix(v, ncol(df_detector1), byrow=T))
      v <- as.data.frame(v)
      v <- setNames(v, names(df_detector1))
      df_detector1 <- v
      
      df_detector1$detector <- 0
      df_detector1$detector[1] <- as.integer(df_detector1$First[1])
      for (i in (2:nrow(df_detector1))){
        if(df_detector1$unique_col[i-1] != df_detector1$unique_col[i]){
          df_detector1$detector[i] <- as.integer(df_detector1$First[i])
        } else if (df_detector1$unique_col[i-1] == df_detector1$unique_col[i]) {
          df_detector1$detector[i] <- as.integer(df_detector1$detector[i-1]) + 1
        }
      }
      
      
      # df <- readr::read_csv(Scats_log)
      df_detector2 <- df %>% dplyr::filter(df$Event == "DA" & df$`+/-` == "-",!grepl("Cleared: PB\\s*(.*?)\\s*", df$Details))
      
      
      df_detector2$first_detector <- NULL
      df_detector2$second_detector <- NULL
      df_detector2_add <- data.frame(matrix(, nrow=nrow(df_detector2), ncol=2))
      colnames(df_detector2_add) <- c("first_detector","second_detector")
      df_detector2 <- cbind(df_detector2,df_detector2_add)
      
      for (i in 1:nrow(df_detector2)){
        text1 <- gsub(".*Cleared: DA*-*", "", df_detector2$Details[i])
        if(grepl("-", substr(text1, 1, 2), fixed = TRUE) == TRUE){
          df_detector2$first_detector[i] <- as.integer(substr(text1, 1, 1))
        } else if (grepl(",", substr(text1, 1, 3), fixed = TRUE) == TRUE) {
          if(is.na(as.integer(substr(text1, 1, 2))) == FALSE){
            df_detector2$first_detector[i] <- as.integer(substr(text1, 1, 2))
          } else if (is.na(as.integer(substr(text1, 1, 2))) == TRUE){
            df_detector2$first_detector[i] <- as.integer(substr(text1, 1, 1))
          }
        } else if (grepl("-", substr(text1, 1, 2), fixed = TRUE) == FALSE) {
          df_detector2$first_detector[i] <- as.integer(substr(text1, 1, 2))
        }
      }
      
      # Important
      # https://stackoverflow.com/questions/39086400/extracting-a-string-between-other-two-strings-in-r
      # stringr::str_match(df_detector2$Details, "DA\\s*(.*?)\\s* ")[2,][2]
      # strsplit(stringr::str_match(df_detector2$Details, "DA\\s*(.*?)\\s* ")[43,][2], split = ",")
      # strsplit(stringr::str_match(df_detector2$Details, "DA\\s*(.*?)\\s* ")[43,][2], split = ",")[[1]]
      # length(strsplit(stringr::str_match(df_detector2$Details, "DA\\s*(.*?)\\s* ")[43,][2], split = ",")[[1]])
      # https://stackoverflow.com/questions/15347282/split-delimited-strings-in-a-column-and-insert-as-new-rows
      # data.frame(V1 = rep(df_detector2$V1, sapply(s, length)), V2 = unlist(s))
      
      # Getting the string between "(Cleared: DA" and ")"
      # To do so delete the last character ")"
      df_detector2$Details = substr(df_detector2$Details,1,nchar(df_detector2$Details)-1)
      # Substring a after a specific text "Cleared: DA"
      s <- strsplit(sub(".*Cleared: DA", "", df_detector2$Details), split = ",")
      df_detector2 <- data.frame(No. = rep(df_detector2$No., sapply(s, length)),
                                 Record = rep(df_detector2$Record, sapply(s, length)) ,
                                 Date = rep(df_detector2$Date, sapply(s, length)) ,
                                 Time = rep(df_detector2$Time, sapply(s, length)),
                                 Source = rep(df_detector2$Source, sapply(s, length)),
                                 Site = rep(df_detector2$Site, sapply(s, length)),
                                 Type = rep(df_detector2$"+/-", sapply(s, length)),
                                 Event = rep(df_detector2$Event, sapply(s, length)),
                                 Mode = rep(df_detector2$Mode, sapply(s, length)),
                                 User = rep(df_detector2$User, sapply(s, length)),
                                 Details = rep(df_detector2$Details, sapply(s, length)), 
                                 Detectors = unlist(s))
      ss <- strsplit(df_detector2$Detectors, split = "-")
      unlist(ss)[[1]]
      df_detector2 <- data.frame(df_detector2,gsub("-.*", "", df_detector2$Detectors),
                                 df_detector2 <- gsub(".*-", "", df_detector2$Detectors))
      colnames(df_detector2[11]) <- "First_detector"
      
      
      df_detector2$second_detector <- NULL
      for (i in 1:nrow(df_detector2)){
        text2 <- gsub(".*-", "", df_detector2$Details[i])
        if(grepl(")", substr(text2, 1, 2), fixed = TRUE) == TRUE){
          df_detector2$second_detector[i] <- as.integer(substr(text2, 1, 1))
        } else if (grepl(")", substr(text2, 1, 2), fixed = TRUE) == FALSE) {
          df_detector2$second_detector[i] <- as.integer(substr(text2, 1, 2))
        }
      }
      
      names(df_detector2)[names(df_detector2)=="gsub............df_detector2.Detectors."] <- "First"
      names(df_detector2)[names(df_detector2)=="df_detector2....gsub............df_detector2.Detectors."] <- "Last"
      df_detector2$rep <- NULL
      df_detector2$rep <- as.integer(as.integer(df_detector2$Last) - as.integer(df_detector2$First) +1)
      df_detector2$unique_col <- order(df_detector2$Date, df_detector2$Time, df_detector2$First)
      
      # https://stackoverflow.com/questions/18028297/duplicate-rows-based-on-field-in-r
      v <- do.call("c", (mapply(rep, c(df_detector2$No., df_detector2$Record, df_detector2$Date, df_detector2$Time, df_detector2$Source, df_detector2$Site, df_detector2$Type, df_detector2$Event, df_detector2$Mode, df_detector2$User, df_detector2$Details, df_detector2$Detectors, df_detector2$First, df_detector2$Last, df_detector2$second_detector, df_detector2$rep, df_detector2$unique_col), df_detector2$rep)))
      v <- t(matrix(v, ncol(df_detector2), byrow=T))
      v <- as.data.frame(v)
      v <- setNames(v, names(df_detector2))
      df_detector2 <- v
      
      df_detector2$detector <- 0
      df_detector2$detector[1] <- as.integer(df_detector2$First[1])
      for (i in (2:nrow(df_detector2))){
        if(df_detector2$unique_col[i-1] != df_detector2$unique_col[i]){
          df_detector2$detector[i] <- as.integer(df_detector2$First[i])
        } else if (df_detector2$unique_col[i-1] == df_detector2$unique_col[i]) {
          df_detector2$detector[i] <- as.integer(df_detector2$detector[i-1]) + 1
        }
      }
      
      # grepl("-", substr(text1, 1, 2), fixed = TRUE) 
      
      df_detector_result <- rbind(df_detector1,df_detector2)
      df_detector_result$Date <- as.Date(df_detector_result$Date, format =  "%d/%m/%Y")
      df_detector_result$Time <- hms::as_hms(as.integer(df_detector_result$Time))
      # df_detector_result$ID <- cumsum(!duplicated(df_detector_result$Site,df_detector_result$detector))
      df_detector_result <- df_detector_result %>% mutate(ID = group_indices(df_detector_result, .dots=c("Site", "detector"))) 
      df_detector_result <- df_detector_result[with(df_detector_result, order(ID, Date, Time)),]
      df_detector_result$end_time <- 0
      df_detector_result$end_date <- 0
      for (kk in 1:nrow(df_detector_result)-1){
        print(kk)
        if((df_detector_result$Type[kk] == "+" | df_detector_result$Type[kk] == "+=")&&(df_detector_result$Type[kk+1] == "-")&&(df_detector_result$ID[kk]==df_detector_result$ID[kk+1])){
          df_detector_result$end_date[kk] = as.character(df_detector_result$Date[kk+1])
          df_detector_result$end_time[kk] = hms::as_hms(as.integer(df_detector_result$Time[kk+1]))
        }
      }
      
      # if((df_detector_result$Type[nrow(df_detector_result)] ="+" | df_detector_result$Type[nrow(df_detector_result)] == "+=")){
      #   
      # }
      
      df_detector_result$end_date <- as.Date(df_detector_result$end_date, format =  "%Y-%m-%d")
      df_detector_result$end_time <- hms::as_hms(as.integer(df_detector_result$end_time))
      
      # Adding the end date for still non-working detectors after filtering out the blips
      max_date <- max(df_detector_result$Date)
      df_detector_result <- df_detector_result[ !duplicated(df_detector_result[, c("Site","Type","end_date","end_time","detector")], fromLast=T),]
      df_detector_result$end_time[is.na(df_detector_result$end_date)] <- hms::as_hms(as.integer(86399))
      df_detector_result$end_date[is.na(df_detector_result$end_date)] <- max_date
      df_detector_result$end_time <- hms::as_hms(as.integer(df_detector_result$end_time))
      df_detector_result <- df_detector_result %>% filter(Type != "-")
      df_detector_result <- df_detector_result[with(df_detector_result, order(ID, Date, Time)),]
      
      df_detector_result$duration <- 0
      df_detector_result$duration <- as.POSIXct(paste0(df_detector_result$end_date," ",hms::as_hms(df_detector_result$end_time)),tz="EST") - as.POSIXct(paste0(df_detector_result$Date," ",hms::as_hms(df_detector_result$Time)),tz="EST")
      
      detectorlists <- readr::read_csv(Detectors_list)
      detectorlists$Site_ID <- as.integer(detectorlists$Site_ID)
      df_detector_result$Site <- as.integer(df_detector_result$Site)
      df_detector_result$duration <- as.double(df_detector_result$duration)
      df_detector_result2 <- dplyr::left_join(df_detector_result, detectorlists, by = c("Site" = "Site_ID"))
      write.csv(df_detector_result2, file, row.names=FALSE)
      # write.csv(df_detector_result,"C:/Users/Abdullah.Shabarek/Desktop/DA_output.csv",row.names = FALSE)
      
      # Aggregating df_detector_result as we want!
      # df_grouped <- df_detector_result %>% group_by(Site) %>% summarise(
      #   total_duration =  sum(duration)
      # )
      # df_grouped$total_duration_hrs <- df_grouped$total_duration/3600
      # 
      # library(dplyr)
      # sum_by_site <- df_grouped %>% group_by(Site) %>% summarise(total_duration = sum(total_duration))
      # sum_by_site <- sum_by_site[order(sum_by_site$total_duration),]
      # 
      # df_grouped <- df_detector_result %>% group_by(Site) %>% sum(as.numeric(df_detector_result$total_duration))
      # df_grouped <-df_grouped[order(df_grouped$hour_time),]
      # df_pivoted <- df_grouped %>%
      #   pivot_wider(names_from = hour_time, values_from = mx_value_count, values_fill = 0)
      # 
      # df_pivoted
      
      
      
     })
  
  #################################################
  # Degree of Saturation
  output$downloadData4 <- downloadHandler(
    
    filename = function() { 
      paste("SM-", Sys.time(), ".csv", sep="")
    },
    content = function(file44) { 
      progress <- shiny::Progress$new()
      # Make sure it closes when we exit this reactive, even if there's an error
      on.exit(progress$close())
      
      progress$set(message = "Analyizing the data....", value = 0)
      Route <- input$Rt44444
      if(Route == "RT1"){RT_code <- "100"}
      if(Route == "RT73"){RT_code <- "73"}
      if(Route == "RT130"){RT_code <- "130"}
      if(Route == "RT18"){RT_code <- "18"}
      # input$file4$datapath
      df_final_heat <- NULL
      site_subsytem_file <- "https://raw.githubusercontent.com/abdallahshabarek/ITS_NJDOT/main/Site_Subsystem.csv"
      phase_naming_file <- "https://raw.githubusercontent.com/abdallahshabarek/ITS_NJDOT/main/SCATS_SAs.csv"
      phase_directions_file <- "https://raw.githubusercontent.com/abdallahshabarek/ITS_NJDOT/main/SCATS_Phasing.csv"
      
      for (kk in (1:length(input$file4$datapath))){
        file <- input$file4$datapath[kk]
        # text <- readtext::readtext(file)
        # text2 <- as.data.frame(gsub("\n", "\t ", text))
        # text2 <- as.data.frame(gsub("\t  ", "\t ", text2))
        # # text2 <- as.data.frame(gsub("\n ", "\\n ", text2))
        # write.table(text2, "C:/Users/Abdullah.Shabarek/Desktop/SM/temp.txt")
        # df <- read.delim2("C:/Users/Abdullah.Shabarek/Desktop/SM/temp.txt",header = FALSE)
        
        df <- read.delim2(file,header = FALSE)
        
        
        df_final <- NULL
        df_tmp1 <- df
        for (i in (1:nrow(df_tmp1))){
          print(paste0(i,"row"))
          if(!is.na(unlist(stringr::str_locate_all(pattern = paste0("\n"), df_tmp1[i,])[1])[1])){
            df_tmp11 <- as.data.frame(df_tmp1[1:i-1,])
            colnames(df_tmp11) <- c("V1")
            df_tmp2 <- as.data.frame(df_tmp1[i+1:nrow(df_tmp1),])
            colnames(df_tmp2) <- c("V1")
            df_tmp3 <- NULL
            df_tmp3$V1 <- NULL
            for (k in ((1:(1+length(unlist(stringr::str_locate_all(pattern = paste0("\n"), df_tmp1[i,])))/2)))){
              print(k)
              if( k == 1) {st <- 1} else {st <- 1+unlist(stringr::str_locate_all(pattern = paste0("\n"), df_tmp1[i,]))[k-1]}
              if( k == (1+length(unlist(stringr::str_locate_all(pattern = paste0("\n"), df_tmp1[i,])))/2)) {end <- nchar(df_tmp1[i,])} else {end <- unlist(stringr::str_locate_all(pattern = paste0("\n"), df_tmp1[i,]))[k]-1}
              tmp3 <- as.data.frame(substr(df_tmp1[i,],st,end))
              colnames(tmp3) <- c("V1")
              df_tmp3 <- as.data.frame(rbind(df_tmp3,tmp3))
              colnames(df_tmp3) <- c("V1")
            }
            df_final <- as.data.frame(rbind(df_final,df_tmp3))
            colnames(df_final) <- c("V1")
          } else {
            # df_tmp11 <- as.data.frame(df_tmp1[1:i-1,])
            # df_tmp2 <- df_tmp1[i:nrow(df_tmp1),]
            df_final <- as.data.frame(rbind(df_final,df_tmp1[i,]))
            colnames(df_final) <- c("V1")
          }
        }
        
        df <- df_final
        
        stringr::str_match(df$V1, "CT\\s*(.*?)\\s* ")[2,][2]
        gsub("^.*?_","_","ATGAS_1121")
        Date <- as.Date(paste0(substr(stringr::str_match(df$V1[1], "_\\s*(.*?)\\s*.sm")[1,][2],1,4),"-",substr(stringr::str_match(df$V1[1], "_\\s*(.*?)\\s*.sm")[1,][2],5,6),"-",substr(stringr::str_match(df$V1[1], "_\\s*(.*?)\\s*.sm")[1,][2],7,8)))
        Year <- substr(stringr::str_match(df$V1[1], "_\\s*(.*?)\\s*.sm")[1,][2],1,4)
        subsystem <- as.integer(substr(df$V1[2],unlist(stringr::str_locate_all(pattern = paste0("SS  "), df$V1[2])[1])[2]+1,unlist(stringr::str_locate_all(pattern = paste0("SS  "), df$V1[2])[1])[2]+2))
        RT_name <- as.character(stringr::str_match(df$V1[1], "filename:\\s*(.*?)\\s*_")[1,][2])
        
        substrRight <- function(x, n){substr(x, nchar(x)-n+1, nchar(x))}
        
        substr(df[950,],18,19)
        substr(df[950,],nchar(df[950,])-3,nchar(df[950,]))
        
        
        if(is.na(as.integer(substrRight(as.character(stringr::str_match(df$V1, "CT \\s*(.*?)\\s* RL")[2,][2]),4)))==TRUE){
          # print("HI")
          as.integer(substrRight(as.character(stringr::str_match(df$V1, "CT \\s*(.*?)\\s* RL")[2,][2]),3))
        }
        
        substr(df$V1[2],1,6)
        
        
        
        
        
        Day_name <- as.character(stringr::str_match(df$V1[1], "Strategic Monitor On \\s*(.*?)\\s* ")[1,][2])
        Date_name <- as.character(stringr::str_match(df$V1[1], paste0(Day_name, " \\s*(.*?)\\s* "))[1,][2])
        Subsystem_number <- as.integer(substr(df$V1[2],nchar(Day_name)+1+nchar(Date_name)+1+6+4+1,nchar(Day_name)+1+nchar(Date_name)+1+6+4+2))
        # Main_site <- as.integer(readr::read_csv(site_subsytem_file)$Site[readr::read_csv(site_subsytem_file)$Subsystem == Subsystem_number])
        # Main_site <- readr::read_csv(site_subsytem_file) %>% dplyr::filter(readr::read_csv(site_subsytem_file)$Subsystem == Subsystem_number & readr::read_csv(site_subsytem_file)$Route_name == RT_name)
        Main_site <- as.integer(as.data.frame(readr::read_csv(site_subsytem_file) %>% dplyr::filter(readr::read_csv(site_subsytem_file)$Subsystem == Subsystem_number & readr::read_csv(site_subsytem_file)$Route_name == RT_name))$Site)
        line_leng <- nchar(df[2,])
        
        df$Degree_Saturation <- ""
        df$Cycle_Length <- ""
        df$Required_Cycle_Length <- ""
        df$time <- ""
        df$Progression <- ""
        df$rotation <- ""
        df$Married <- ""
        df$type <- ""
        df$subsite <- ""
        df$strategic_approach <- ""
        df$notation <- ""
        df$phase <- ""
        df$phase_time <- ""
        df$Avg_DS_Phase <-""
        df$phase_A_GT <- ""
        df$phase_B_GT <- ""
        df$phase_C_GT <- ""
        df$phase_D_GT <- ""
        df$phase_E_GT <- ""
        df$phase_F_GT <- ""
        df$phase_G_GT <- ""
        df$phase_H_GT <- ""
        df$phase_I_GT <- ""
        
        for (i in 1:nrow(df)){
          # print(i/nrow(df)*100)
          if(substr(df$V1[i],1,6)=="Friday" | substr(df$V1[i],1,6)=="Saturd" | substr(df$V1[i],1,6)=="Sunday" | substr(df$V1[i],1,6)=="Monday" | substr(df$V1[i],1,6)=="Tuesda" | substr(df$V1[i],1,6)=="Wednes" | substr(df$V1[i],1,6)=="Thursd"){
            print(paste0(i,":",nchar(df[i,])))
            # df$Degree_Saturation[i] <- as.integer(substr(df[i,],line_leng-2,line_leng))[1]
            df$Degree_Saturation[i] <- as.integer(sub("^.+DS ", "", df$V1[i]))
            df$Cycle_Length[i] <- as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = "CT ", df[i,])[1])[2],unlist(stringr::str_locate_all(pattern = "CT ", df[i,])[1])[2]+4))
            df$Required_Cycle_Length[i] <- as.integer(substr(df$V1[i],1+unlist(stringr::str_locate_all(pattern = "RL", df[i,])[1])[2],unlist(stringr::str_locate_all(pattern = "RL", df[i,])[1])[2]+3))
            df$time[i] <- as_hms(as.POSIXct(substr(df[i,],nchar(Day_name)+1+nchar(Date_name)+2,nchar(Day_name)+1+nchar(Date_name)+1+5),format="%H:%M"))[1]
            df$Progression[i] <- as.double(substr(df[i,],nchar(Day_name)+1+nchar(Date_name)+1+6+4+3+6,nchar(Day_name)+1+nchar(Date_name)+1+6+4+4+7))[1]
            df$rotation[i] <- as.integer(substr(df[i,],line_leng-22,line_leng-20))[1]
            df$Married[i] <- as.character(substr(df[i,],nchar(Day_name)+1+nchar(Date_name)+1+6+4+3,nchar(Day_name)+1+nchar(Date_name)+1+6+4+4)[1])
          } else if (!is.na(as.numeric(substr(df[i,],1,2)[1]))==TRUE) {
            type <- as.character(stringr::str_match(substr(df$V1[i],2,nchar(df$V1[i])-2), " \\s*(.*?)\\s* ")[1,][2])
            df$type[i] <- type
            df$subsite[i] <- as.integer(stringr::str_match(df$V1[i], "\\s*(.*?)\\s* ")[1,][2])
            strategic_approach <- as.integer(stringr::str_match(df$V1[i], paste0(type," \\s*(.*?)\\s* "))[1,][2])
            df$strategic_approach[i] <- strategic_approach
            
            stringr::str_locate_all(pattern = paste0(type," ",strategic_approach," "), df[i,])[1]
            if(type == "SA"){
              end_str <- unlist(stringr::str_locate_all(pattern = paste0(type," ",strategic_approach," "), df[i,])[1])[2]
            } else if (type == "LK"){
              end_str <- unlist(stringr::str_locate_all(pattern = paste0(type,"  ",strategic_approach," "), df[i,])[1])[2]
            }
            df$notation[i] <- substr(df[i,],end_str+1,end_str+2)[1]
            end_str_phase <- unlist(stringr::str_locate_all(pattern = paste0("!"), df[i,])[1])[1]
            # df$phase[i] <- as.character(substr(df[i,],end_str+5,end_str+6)[1])
            df$phase[i] <- as.character(substr(df$V1[i],17,19))
            
            # df$phase_time[i] <- as.integer(substr(df[i,],end_str+8,end_str+10)[1])
            df$phase_time[i] <- as.integer(substr(stringr::str_match(df$V1[i], " \\s*(.*?)\\s*!")[1,][2],nchar(stringr::str_match(df$V1[i], " \\s*(.*?)\\s*!")[1,][2])-3,nchar(stringr::str_match(df$V1[i], " \\s*(.*?)\\s*!")[1,][2])))
            df$Avg_DS_Phase[i] <- as.integer(substr(df[i,],nchar(df[i,])-4,nchar(df[i,]))[1])
          } else if (substr(df$V1[i],1,3)=="A=<"){
            df$phase_A_GT[i] <- as.integer(stringr::str_match(df$V1[i], "A=<\\s*(.*?)\\s*>")[1,][2])
            df$phase_B_GT[i] <- 
              if(is.na(unlist(stringr::str_locate_all(pattern = paste0("B="), df$V1[i])[1])[1])==TRUE){
                ""  
              } else {
                as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("B="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("B="), df$V1[i])[1])[1]+4))
              }
            df$phase_C_GT[i] <- 
              if(is.na(unlist(stringr::str_locate_all(pattern = paste0("C="), df$V1[i])[1])[1])==TRUE){
                ""  
              } else {
                as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("C="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("C="), df$V1[i])[1])[1]+4))
              }
            df$phase_D_GT[i] <- 
              if(is.na(unlist(stringr::str_locate_all(pattern = paste0("D="), df$V1[i])[1])[1])==TRUE){
                ""  
              } else {
                as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("D="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("D="), df$V1[i])[1])[1]+4))
              }
            df$phase_E_GT[i] <- 
              if(is.na(unlist(stringr::str_locate_all(pattern = paste0("E="), df$V1[i])[1])[1])==TRUE){
                ""  
              } else {
                as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("E="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("E="), df$V1[i])[1])[1]+4))
              }
            df$phase_F_GT[i] <- 
              if(is.na(unlist(stringr::str_locate_all(pattern = paste0("F="), df$V1[i])[1])[1])==TRUE){
                ""  
              } else {
                as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("F="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("F="), df$V1[i])[1])[1]+4))
              }
            df$phase_G_GT[i] <- 
              if(is.na(unlist(stringr::str_locate_all(pattern = paste0("G="), df$V1[i])[1])[1])==TRUE){
                ""  
              } else {
                as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("G="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("G="), df$V1[i])[1])[1]+4))
              }
            df$phase_H_GT[i] <- 
              if(is.na(unlist(stringr::str_locate_all(pattern = paste0("H="), df$V1[i])[1])[1])==TRUE){
                ""  
              } else {
                as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("H="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("H="), df$V1[i])[1])[1]+4))
              }
            df$phase_I_GT[i] <- 
              if(is.na(unlist(stringr::str_locate_all(pattern = paste0("I="), df$V1[i])[1])[1])==TRUE){
                ""  
              } else {
                as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("I="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("I="), df$V1[i])[1])[1]+4))
              }
          }
        }
        
        df$Day <- Day_name
        df$Date <- Date
        df$Date_name <- Date_name
        df$SS <- subsystem
        # delete spaces in the selected column
        df$phase <- gsub('\\s+', '',df$phase)
        
        df1 <- readr::read_csv(phase_naming_file)
        # df2 <- readr::read_csv(phase_directions_file)
        # 
        # df3 <- df2 %>% tidyr::pivot_longer(!`Signal ID`, 
        #                                    names_to = "Directions", values_to = "Phases")
        # 
        # colnames(df3) <- c("Signal_ID","Direction","Phase")
        # colnames(df1) <- c("Site", "Phase_name", "Phase_number")
        # df4 <- sqldf::sqldf("select a.Signal_ID, a.Direction, a.Phase, b.Phase_name from df3 as a
        # left join df1 as b
        # on b.Site = a.Signal_ID and (a.Phase = b.Phase_number)")
        # df5 <- unique(df4)
        # df6 <- df5[!is.na(df5$Phase),]
        # df7 <- df6[df6$Signal_ID!="Signal ID",]
        # 
        # df11 <- df7 %>% dplyr::select("Signal_ID", "Phase", "Phase_name")
        # df11 <- unique(df11)
        # 
        # df11$Phase <- as.character(df11$Phase)
        # df22 <- sqldf::sqldf("select a.*,
        # case when (a.Type = 'SA' or a.Type='LK') and (a.phase <> 'A' and  a.phase <> 'B' and a.phase <> 'C' and a.phase <> 'D' and a.phase <> 'E' and a.phase <> 'F' and a.phase <> 'G' and a.phase <> 'H' and a.phase <> 'I') then b.Phase_name
        # when (a.Type = 'SA' or a.Type='LK') and (a.phase = 'A' or  a.phase = 'B' or a.phase = 'C' or a.phase = 'D' or a.phase = 'E' or a.phase = 'F' or a.phase = 'G' or a.phase = 'H' or a.phase = 'I') then a.Phase
        # when (a.Type <> 'SA' and a.Type <> 'LK') then ''
        # end as Phase_name
        # from df as a
        # left join df11 as b
        # on a.subsite = b.Signal_ID and a.phase = b.Phase", drv ="SQLite")
        df1$ss <- NULL
        df1 <- df1 %>% dplyr::filter(Site == Main_site)
        colnames(df1) <- c("site","phase_name","phase_number")
        df22 <- sqldf::sqldf("select a.*,
case when (a.Type = 'SA' or a.Type='LK') and (a.phase <> 'A' and  a.phase <> 'B' and a.phase <> 'C' and a.phase <> 'D' and a.phase <> 'E' and a.phase <> 'F' and a.phase <> 'G' and a.phase <> 'H' and a.phase <> 'I') then b.Phase_name
when (a.Type = 'SA' or a.Type='LK') and (a.phase = 'A' or  a.phase = 'B' or a.phase = 'C' or a.phase = 'D' or a.phase = 'E' or a.phase = 'F' or a.phase = 'G' or a.phase = 'H' or a.phase = 'I') then a.Phase
when (a.Type <> 'SA' and a.Type <> 'LK') then ''
end as Phase_name
from df as a
left join df1 as b
on a.phase = b.phase_number", drv ="SQLite")
        
        
        
        df22 <- df22 %>% naniar::replace_with_na(replace = list(time = c("")))
        df22 <- df22 %>% naniar::replace_with_na(replace = list(rotation = c("")))
        df22 <- df22 %>% naniar::replace_with_na(replace = list(Progression = c("")))
        df22 <- df22 %>% naniar::replace_with_na(replace = list(Cycle_Length = c("")))
        df22 <- df22 %>% naniar::replace_with_na(replace = list(Required_Cycle_Length = c("")))
        df22 <- df22 %>% naniar::replace_with_na(replace = list(Degree_Saturation = c("")))
        df22 <- df22 %>% naniar::replace_with_na(replace = list(Married = c("")))
        df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_A_GT = c("")))
        df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_B_GT = c("")))
        df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_C_GT = c("")))
        df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_D_GT = c("")))
        df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_E_GT = c("")))
        df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_F_GT = c("")))
        df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_G_GT = c("")))
        df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_H_GT = c("")))
        df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_I_GT = c("")))
        
        
        df22 <- df22 %>% tidyr::fill(rotation, .direction = "down")
        df22 <- df22 %>% tidyr::fill(Progression, .direction = "down")
        df22 <- df22 %>% tidyr::fill(Cycle_Length, .direction = "down")
        df22 <- df22 %>% tidyr::fill(time, .direction = "down")
        df22 <- df22 %>% tidyr::fill(Required_Cycle_Length, .direction = "down")
        df22 <- df22 %>% tidyr::fill(Degree_Saturation, .direction = "down")
        df22 <- df22 %>% tidyr::fill(Married, .direction = "down")
        df22 <- df22 %>% tidyr::fill(phase_A_GT, .direction = "up")
        df22 <- df22 %>% tidyr::fill(phase_B_GT, .direction = "up")
        df22 <- df22 %>% tidyr::fill(phase_C_GT, .direction = "up")
        df22 <- df22 %>% tidyr::fill(phase_D_GT, .direction = "up")
        df22 <- df22 %>% tidyr::fill(phase_E_GT, .direction = "up")
        df22 <- df22 %>% tidyr::fill(phase_F_GT, .direction = "up")
        df22 <- df22 %>% tidyr::fill(phase_G_GT, .direction = "up")
        df22 <- df22 %>% tidyr::fill(phase_H_GT, .direction = "up")
        df22 <- df22 %>% tidyr::fill(phase_I_GT, .direction = "up")
        df22 <- df22[!grepl(" Int",substr(df22$V1,1,4)),]
        df22 <- df22[!grepl("Strategic Monitor On",substr(df22$V1,1,20)),]
        df22 <- df22[!grepl("A=<",substr(df22$V1,1,3)),]
        
        
        # To get DS heatmap
        df22 <- df22 %>% dplyr::filter(type == "")
        df22$Site <- Main_site
        df_final_heat <- rbind(df22,df_final_heat)
      }
      
      df_final_heat$time <- hms::as_hms(as.integer(df_final_heat$time))
      df_final_heat$Hour <- as.integer(round(df_final_heat$time/3600,0)*3600)
      # df_heat$Hour <- as.integer(df_heat$Hour)
      # df_heat$Date
      # df_heat$Site
      df_final_heat$Degree_Saturation <- as.double(df_final_heat$Degree_Saturation)
      df_grouped_part1 <- sqldf::sqldf(paste0("select Site, Date, Hour, avg(Degree_Saturation) as [Avg_DS]
                             from df_final_heat
                             group by Site, Hour, Date"), drv="SQLite")
      df_grouped_part1$Datetime <- as.POSIXct(paste0(as.Date(df_grouped_part1$Date)," ",hms::as_hms(df_grouped_part1$Hour)), format="%Y-%m-%d %H:%M:%S", tz="EST")
      df_grouped <- df_grouped_part1
      ########################################################################
      # df <- df %>% dplyr::select(V1,Site,Date,Day,time,Cycle_Length,Required_Cycle_Length,rotation,Progression,Degree_Saturation,Coordination)
      # df$time <- hms::as_hms(df$time)
      # df_final <- rbind(df,df_final)
      # }
      # 
      # df_final$Hour <- as.integer(round(df_final$time/3600,0)*3600)
      # dates_list <- unique(df_final$Date)
      # D <- dates_list[1]
      # df_grouped_part1 <- sqldf::sqldf(paste0("select Site, Date, Hour, avg(Degree_Saturation) as [Avg_DS]
      #                              from df_final
      #                              group by Site, Hour, Date"), drv="SQLite")
      # 
      # df_grouped_part1$Datetime <- as.POSIXct(paste0(as.Date(df_grouped_part1$Date)," ",hms::as_hms(df_grouped_part1$Hour)), format="%Y-%m-%d %H:%M:%S", tz="EST")
      # df_grouped <- df_grouped_part1
      
      # Building a Dataframe from the datetime
      lowerbound <- format(as.POSIXct(paste0(as.Date(min(df_grouped$Date))," ",hms::as_hms("00:00:00")), format="%Y-%m-%d %H:%M:%S", tz="EST"),"%Y-%m-%d %H:%M:%S")
      upperbound <- format(as.POSIXct(paste0(as.Date(max(df_grouped$Date))," ",hms::as_hms("23:00:00")), format="%Y-%m-%d %H:%M:%S", tz="EST"),"%Y-%m-%d %H:%M:%S")
      NoOfHours <- as.numeric(ymd_hms(upperbound) - ymd_hms(lowerbound))*24 
      datetime_list <- ymd_hms(lowerbound) + hours(0:NoOfHours)
      df_grouped2 <- as.data.frame(datetime_list)
      colnames(df_grouped2) <- c("Datetime")
      
      unique_sites <- as.data.frame(unique(df_final_heat$Site))
      colnames(unique_sites) <- c("Site")
      
      df_grouped2 <- tidyr::crossing(df_grouped2,unique_sites)
      df_grouped_part1$Datetime <- as.character(df_grouped_part1$Datetime)
      df_grouped2$Datetime <- as.character(df_grouped2$Datetime)
      df_grouped_part11 <- dplyr::left_join(df_grouped2, df_grouped_part1, by = c("Datetime" = "Datetime", "Site"="Site"))
      df_grouped_part11 <- df_grouped_part11 %>% dplyr::select(Datetime, Site, Avg_DS)
      df_grouped_part11$Avg_DS[is.na(df_grouped_part11$Avg_DS)] <- 0
      dfff <- reshape::cast(df_grouped_part11, Site ~ Datetime, fun.aggregate = mean, value ="Avg_DS")
      dfff <- as.data.frame(dfff)
      dfff <- dfff[,1:25]
      write.csv(dfff, file44, row.names=FALSE)
    })
  observeEvent(input$previewData4, {
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    
    progress$set(message = "Creating a Heatmap....", value = 0)  
    
    
    Route <- input$Rt44444
      if(Route == "RT1"){RT_code <- "100"}
      if(Route == "RT73"){RT_code <- "73"}
      if(Route == "RT130"){RT_code <- "130"}
      if(Route == "RT18"){RT_code <- "18"}
      # input$file4$datapath
      df_final_heat <- NULL
      site_subsytem_file <- "https://raw.githubusercontent.com/abdallahshabarek/ITS_NJDOT/main/Site_Subsystem.csv"
      phase_naming_file <- "https://raw.githubusercontent.com/abdallahshabarek/ITS_NJDOT/main/SCATS_SAs.csv"
      phase_directions_file <- "https://raw.githubusercontent.com/abdallahshabarek/ITS_NJDOT/main/SCATS_Phasing.csv"
      
      for (kk in (1:length(input$file4$datapath))){
          file <- input$file4$datapath[kk]
          # text <- readtext::readtext(file)
          # text2 <- as.data.frame(gsub("\n", "\t ", text))
          # text2 <- as.data.frame(gsub("\t  ", "\t ", text2))
          # # text2 <- as.data.frame(gsub("\n ", "\\n ", text2))
          # write.table(text2, "C:/Users/Abdullah.Shabarek/Desktop/SM/temp.txt")
          # df <- read.delim2("C:/Users/Abdullah.Shabarek/Desktop/SM/temp.txt",header = FALSE)
          
          df <- read.delim2(file,header = FALSE)
          
          
          df_final <- NULL
          df_tmp1 <- df
          for (i in (1:nrow(df_tmp1))){
            print(paste0(i,"row"))
            if(!is.na(unlist(stringr::str_locate_all(pattern = paste0("\n"), df_tmp1[i,])[1])[1])){
              df_tmp11 <- as.data.frame(df_tmp1[1:i-1,])
              colnames(df_tmp11) <- c("V1")
              df_tmp2 <- as.data.frame(df_tmp1[i+1:nrow(df_tmp1),])
              colnames(df_tmp2) <- c("V1")
              df_tmp3 <- NULL
              df_tmp3$V1 <- NULL
              for (k in ((1:(1+length(unlist(stringr::str_locate_all(pattern = paste0("\n"), df_tmp1[i,])))/2)))){
                print(k)
                if( k == 1) {st <- 1} else {st <- 1+unlist(stringr::str_locate_all(pattern = paste0("\n"), df_tmp1[i,]))[k-1]}
                if( k == (1+length(unlist(stringr::str_locate_all(pattern = paste0("\n"), df_tmp1[i,])))/2)) {end <- nchar(df_tmp1[i,])} else {end <- unlist(stringr::str_locate_all(pattern = paste0("\n"), df_tmp1[i,]))[k]-1}
                tmp3 <- as.data.frame(substr(df_tmp1[i,],st,end))
                colnames(tmp3) <- c("V1")
                df_tmp3 <- as.data.frame(rbind(df_tmp3,tmp3))
                colnames(df_tmp3) <- c("V1")
              }
              df_final <- as.data.frame(rbind(df_final,df_tmp3))
              colnames(df_final) <- c("V1")
            } else {
              # df_tmp11 <- as.data.frame(df_tmp1[1:i-1,])
              # df_tmp2 <- df_tmp1[i:nrow(df_tmp1),]
              df_final <- as.data.frame(rbind(df_final,df_tmp1[i,]))
              colnames(df_final) <- c("V1")
            }
          }
          
          df <- df_final
          
          stringr::str_match(df$V1, "CT\\s*(.*?)\\s* ")[2,][2]
          gsub("^.*?_","_","ATGAS_1121")
          Date <- as.Date(paste0(substr(stringr::str_match(df$V1[1], "_\\s*(.*?)\\s*.sm")[1,][2],1,4),"-",substr(stringr::str_match(df$V1[1], "_\\s*(.*?)\\s*.sm")[1,][2],5,6),"-",substr(stringr::str_match(df$V1[1], "_\\s*(.*?)\\s*.sm")[1,][2],7,8)))
          Year <- substr(stringr::str_match(df$V1[1], "_\\s*(.*?)\\s*.sm")[1,][2],1,4)
          subsystem <- as.integer(substr(df$V1[2],unlist(stringr::str_locate_all(pattern = paste0("SS  "), df$V1[2])[1])[2]+1,unlist(stringr::str_locate_all(pattern = paste0("SS  "), df$V1[2])[1])[2]+2))
          RT_name <- as.character(stringr::str_match(df$V1[1], "filename:\\s*(.*?)\\s*_")[1,][2])
          
          substrRight <- function(x, n){substr(x, nchar(x)-n+1, nchar(x))}
          
          substr(df[950,],18,19)
          substr(df[950,],nchar(df[950,])-3,nchar(df[950,]))
          
          
          if(is.na(as.integer(substrRight(as.character(stringr::str_match(df$V1, "CT \\s*(.*?)\\s* RL")[2,][2]),4)))==TRUE){
            # print("HI")
            as.integer(substrRight(as.character(stringr::str_match(df$V1, "CT \\s*(.*?)\\s* RL")[2,][2]),3))
          }
          
          substr(df$V1[2],1,6)
          
          
          
          
          
          Day_name <- as.character(stringr::str_match(df$V1[1], "Strategic Monitor On \\s*(.*?)\\s* ")[1,][2])
          Date_name <- as.character(stringr::str_match(df$V1[1], paste0(Day_name, " \\s*(.*?)\\s* "))[1,][2])
          Subsystem_number <- as.integer(substr(df$V1[2],nchar(Day_name)+1+nchar(Date_name)+1+6+4+1,nchar(Day_name)+1+nchar(Date_name)+1+6+4+2))
          # Main_site <- as.integer(readr::read_csv(site_subsytem_file)$Site[readr::read_csv(site_subsytem_file)$Subsystem == Subsystem_number])
          # Main_site <- readr::read_csv(site_subsytem_file) %>% dplyr::filter(readr::read_csv(site_subsytem_file)$Subsystem == Subsystem_number & readr::read_csv(site_subsytem_file)$Route_name == RT_name)
          Main_site <- as.integer(as.data.frame(readr::read_csv(site_subsytem_file) %>% dplyr::filter(readr::read_csv(site_subsytem_file)$Subsystem == Subsystem_number & readr::read_csv(site_subsytem_file)$Route_name == RT_name))$Site)
          line_leng <- nchar(df[2,])
          
          df$Degree_Saturation <- ""
          df$Cycle_Length <- ""
          df$Required_Cycle_Length <- ""
          df$time <- ""
          df$Progression <- ""
          df$rotation <- ""
          df$Married <- ""
          df$type <- ""
          df$subsite <- ""
          df$strategic_approach <- ""
          df$notation <- ""
          df$phase <- ""
          df$phase_time <- ""
          df$Avg_DS_Phase <-""
          df$phase_A_GT <- ""
          df$phase_B_GT <- ""
          df$phase_C_GT <- ""
          df$phase_D_GT <- ""
          df$phase_E_GT <- ""
          df$phase_F_GT <- ""
          df$phase_G_GT <- ""
          df$phase_H_GT <- ""
          df$phase_I_GT <- ""
          
          for (i in 1:nrow(df)){
            # print(i/nrow(df)*100)
            if(substr(df$V1[i],1,6)=="Friday" | substr(df$V1[i],1,6)=="Saturd" | substr(df$V1[i],1,6)=="Sunday" | substr(df$V1[i],1,6)=="Monday" | substr(df$V1[i],1,6)=="Tuesda" | substr(df$V1[i],1,6)=="Wednes" | substr(df$V1[i],1,6)=="Thursd"){
              print(paste0(i,":",nchar(df[i,])))
              # df$Degree_Saturation[i] <- as.integer(substr(df[i,],line_leng-2,line_leng))[1]
              df$Degree_Saturation[i] <- as.integer(sub("^.+DS ", "", df$V1[i]))
              df$Cycle_Length[i] <- as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = "CT ", df[i,])[1])[2],unlist(stringr::str_locate_all(pattern = "CT ", df[i,])[1])[2]+4))
              df$Required_Cycle_Length[i] <- as.integer(substr(df$V1[i],1+unlist(stringr::str_locate_all(pattern = "RL", df[i,])[1])[2],unlist(stringr::str_locate_all(pattern = "RL", df[i,])[1])[2]+3))
              df$time[i] <- as_hms(as.POSIXct(substr(df[i,],nchar(Day_name)+1+nchar(Date_name)+2,nchar(Day_name)+1+nchar(Date_name)+1+5),format="%H:%M"))[1]
              df$Progression[i] <- as.double(substr(df[i,],nchar(Day_name)+1+nchar(Date_name)+1+6+4+3+6,nchar(Day_name)+1+nchar(Date_name)+1+6+4+4+7))[1]
              df$rotation[i] <- as.integer(substr(df[i,],line_leng-22,line_leng-20))[1]
              df$Married[i] <- as.character(substr(df[i,],nchar(Day_name)+1+nchar(Date_name)+1+6+4+3,nchar(Day_name)+1+nchar(Date_name)+1+6+4+4)[1])
            } else if (!is.na(as.numeric(substr(df[i,],1,2)[1]))==TRUE) {
              type <- as.character(stringr::str_match(substr(df$V1[i],2,nchar(df$V1[i])-2), " \\s*(.*?)\\s* ")[1,][2])
              df$type[i] <- type
              df$subsite[i] <- as.integer(stringr::str_match(df$V1[i], "\\s*(.*?)\\s* ")[1,][2])
              strategic_approach <- as.integer(stringr::str_match(df$V1[i], paste0(type," \\s*(.*?)\\s* "))[1,][2])
              df$strategic_approach[i] <- strategic_approach
              
              stringr::str_locate_all(pattern = paste0(type," ",strategic_approach," "), df[i,])[1]
              if(type == "SA"){
                end_str <- unlist(stringr::str_locate_all(pattern = paste0(type," ",strategic_approach," "), df[i,])[1])[2]
              } else if (type == "LK"){
                end_str <- unlist(stringr::str_locate_all(pattern = paste0(type,"  ",strategic_approach," "), df[i,])[1])[2]
              }
              df$notation[i] <- substr(df[i,],end_str+1,end_str+2)[1]
              end_str_phase <- unlist(stringr::str_locate_all(pattern = paste0("!"), df[i,])[1])[1]
              # df$phase[i] <- as.character(substr(df[i,],end_str+5,end_str+6)[1])
              df$phase[i] <- as.character(substr(df$V1[i],17,19))
              
              # df$phase_time[i] <- as.integer(substr(df[i,],end_str+8,end_str+10)[1])
              df$phase_time[i] <- as.integer(substr(stringr::str_match(df$V1[i], " \\s*(.*?)\\s*!")[1,][2],nchar(stringr::str_match(df$V1[i], " \\s*(.*?)\\s*!")[1,][2])-3,nchar(stringr::str_match(df$V1[i], " \\s*(.*?)\\s*!")[1,][2])))
              df$Avg_DS_Phase[i] <- as.integer(substr(df[i,],nchar(df[i,])-4,nchar(df[i,]))[1])
            } else if (substr(df$V1[i],1,3)=="A=<"){
              df$phase_A_GT[i] <- as.integer(stringr::str_match(df$V1[i], "A=<\\s*(.*?)\\s*>")[1,][2])
              df$phase_B_GT[i] <- 
                if(is.na(unlist(stringr::str_locate_all(pattern = paste0("B="), df$V1[i])[1])[1])==TRUE){
                  ""  
                } else {
                  as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("B="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("B="), df$V1[i])[1])[1]+4))
                }
              df$phase_C_GT[i] <- 
                if(is.na(unlist(stringr::str_locate_all(pattern = paste0("C="), df$V1[i])[1])[1])==TRUE){
                  ""  
                } else {
                  as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("C="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("C="), df$V1[i])[1])[1]+4))
                }
              df$phase_D_GT[i] <- 
                if(is.na(unlist(stringr::str_locate_all(pattern = paste0("D="), df$V1[i])[1])[1])==TRUE){
                  ""  
                } else {
                  as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("D="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("D="), df$V1[i])[1])[1]+4))
                }
              df$phase_E_GT[i] <- 
                if(is.na(unlist(stringr::str_locate_all(pattern = paste0("E="), df$V1[i])[1])[1])==TRUE){
                  ""  
                } else {
                  as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("E="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("E="), df$V1[i])[1])[1]+4))
                }
              df$phase_F_GT[i] <- 
                if(is.na(unlist(stringr::str_locate_all(pattern = paste0("F="), df$V1[i])[1])[1])==TRUE){
                  ""  
                } else {
                  as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("F="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("F="), df$V1[i])[1])[1]+4))
                }
              df$phase_G_GT[i] <- 
                if(is.na(unlist(stringr::str_locate_all(pattern = paste0("G="), df$V1[i])[1])[1])==TRUE){
                  ""  
                } else {
                  as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("G="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("G="), df$V1[i])[1])[1]+4))
                }
              df$phase_H_GT[i] <- 
                if(is.na(unlist(stringr::str_locate_all(pattern = paste0("H="), df$V1[i])[1])[1])==TRUE){
                  ""  
                } else {
                  as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("H="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("H="), df$V1[i])[1])[1]+4))
                }
              df$phase_I_GT[i] <- 
                if(is.na(unlist(stringr::str_locate_all(pattern = paste0("I="), df$V1[i])[1])[1])==TRUE){
                  ""  
                } else {
                  as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("I="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("I="), df$V1[i])[1])[1]+4))
                }
            }
          }
          
          df$Day <- Day_name
          df$Date <- Date
          df$Date_name <- Date_name
          df$SS <- subsystem
          # delete spaces in the selected column
          df$phase <- gsub('\\s+', '',df$phase)
          
          df1 <- readr::read_csv(phase_naming_file)
          # df2 <- readr::read_csv(phase_directions_file)
          # 
          # df3 <- df2 %>% tidyr::pivot_longer(!`Signal ID`, 
          #                                    names_to = "Directions", values_to = "Phases")
          # 
          # colnames(df3) <- c("Signal_ID","Direction","Phase")
          # colnames(df1) <- c("Site", "Phase_name", "Phase_number")
          # df4 <- sqldf::sqldf("select a.Signal_ID, a.Direction, a.Phase, b.Phase_name from df3 as a
          # left join df1 as b
          # on b.Site = a.Signal_ID and (a.Phase = b.Phase_number)")
          # df5 <- unique(df4)
          # df6 <- df5[!is.na(df5$Phase),]
          # df7 <- df6[df6$Signal_ID!="Signal ID",]
          # 
          # df11 <- df7 %>% dplyr::select("Signal_ID", "Phase", "Phase_name")
          # df11 <- unique(df11)
          # 
          # df11$Phase <- as.character(df11$Phase)
          # df22 <- sqldf::sqldf("select a.*,
          # case when (a.Type = 'SA' or a.Type='LK') and (a.phase <> 'A' and  a.phase <> 'B' and a.phase <> 'C' and a.phase <> 'D' and a.phase <> 'E' and a.phase <> 'F' and a.phase <> 'G' and a.phase <> 'H' and a.phase <> 'I') then b.Phase_name
          # when (a.Type = 'SA' or a.Type='LK') and (a.phase = 'A' or  a.phase = 'B' or a.phase = 'C' or a.phase = 'D' or a.phase = 'E' or a.phase = 'F' or a.phase = 'G' or a.phase = 'H' or a.phase = 'I') then a.Phase
          # when (a.Type <> 'SA' and a.Type <> 'LK') then ''
          # end as Phase_name
          # from df as a
          # left join df11 as b
          # on a.subsite = b.Signal_ID and a.phase = b.Phase", drv ="SQLite")
          df1$ss <- NULL
          df1 <- df1 %>% dplyr::filter(Site == Main_site)
          colnames(df1) <- c("site","phase_name","phase_number")
          df22 <- sqldf::sqldf("select a.*,
case when (a.Type = 'SA' or a.Type='LK') and (a.phase <> 'A' and  a.phase <> 'B' and a.phase <> 'C' and a.phase <> 'D' and a.phase <> 'E' and a.phase <> 'F' and a.phase <> 'G' and a.phase <> 'H' and a.phase <> 'I') then b.Phase_name
when (a.Type = 'SA' or a.Type='LK') and (a.phase = 'A' or  a.phase = 'B' or a.phase = 'C' or a.phase = 'D' or a.phase = 'E' or a.phase = 'F' or a.phase = 'G' or a.phase = 'H' or a.phase = 'I') then a.Phase
when (a.Type <> 'SA' and a.Type <> 'LK') then ''
end as Phase_name
from df as a
left join df1 as b
on a.phase = b.phase_number", drv ="SQLite")
          
          
          
          df22 <- df22 %>% naniar::replace_with_na(replace = list(time = c("")))
          df22 <- df22 %>% naniar::replace_with_na(replace = list(rotation = c("")))
          df22 <- df22 %>% naniar::replace_with_na(replace = list(Progression = c("")))
          df22 <- df22 %>% naniar::replace_with_na(replace = list(Cycle_Length = c("")))
          df22 <- df22 %>% naniar::replace_with_na(replace = list(Required_Cycle_Length = c("")))
          df22 <- df22 %>% naniar::replace_with_na(replace = list(Degree_Saturation = c("")))
          df22 <- df22 %>% naniar::replace_with_na(replace = list(Married = c("")))
          df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_A_GT = c("")))
          df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_B_GT = c("")))
          df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_C_GT = c("")))
          df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_D_GT = c("")))
          df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_E_GT = c("")))
          df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_F_GT = c("")))
          df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_G_GT = c("")))
          df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_H_GT = c("")))
          df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_I_GT = c("")))
          
          
          df22 <- df22 %>% tidyr::fill(rotation, .direction = "down")
          df22 <- df22 %>% tidyr::fill(Progression, .direction = "down")
          df22 <- df22 %>% tidyr::fill(Cycle_Length, .direction = "down")
          df22 <- df22 %>% tidyr::fill(time, .direction = "down")
          df22 <- df22 %>% tidyr::fill(Required_Cycle_Length, .direction = "down")
          df22 <- df22 %>% tidyr::fill(Degree_Saturation, .direction = "down")
          df22 <- df22 %>% tidyr::fill(Married, .direction = "down")
          df22 <- df22 %>% tidyr::fill(phase_A_GT, .direction = "up")
          df22 <- df22 %>% tidyr::fill(phase_B_GT, .direction = "up")
          df22 <- df22 %>% tidyr::fill(phase_C_GT, .direction = "up")
          df22 <- df22 %>% tidyr::fill(phase_D_GT, .direction = "up")
          df22 <- df22 %>% tidyr::fill(phase_E_GT, .direction = "up")
          df22 <- df22 %>% tidyr::fill(phase_F_GT, .direction = "up")
          df22 <- df22 %>% tidyr::fill(phase_G_GT, .direction = "up")
          df22 <- df22 %>% tidyr::fill(phase_H_GT, .direction = "up")
          df22 <- df22 %>% tidyr::fill(phase_I_GT, .direction = "up")
          df22 <- df22[!grepl(" Int",substr(df22$V1,1,4)),]
          df22 <- df22[!grepl("Strategic Monitor On",substr(df22$V1,1,20)),]
          df22 <- df22[!grepl("A=<",substr(df22$V1,1,3)),]
          
          
          # To get DS heatmap
          df22 <- df22 %>% dplyr::filter(type == "")
          df22$Site <- Main_site
          df_final_heat <- rbind(df22,df_final_heat)
        }
        
      df_final_heat$time <- hms::as_hms(as.integer(df_final_heat$time))
      df_final_heat$Hour <- as.integer(round(df_final_heat$time/3600,0)*3600)
      # df_heat$Hour <- as.integer(df_heat$Hour)
      # df_heat$Date
      # df_heat$Site
      df_final_heat$Degree_Saturation <- as.double(df_final_heat$Degree_Saturation)
      df_grouped_part1 <- sqldf::sqldf(paste0("select Site, Date, Hour, avg(Degree_Saturation) as [Avg_DS]
                             from df_final_heat
                             group by Site, Hour, Date"), drv="SQLite")
      df_grouped_part1$Datetime <- as.POSIXct(paste0(as.Date(df_grouped_part1$Date)," ",hms::as_hms(df_grouped_part1$Hour)), format="%Y-%m-%d %H:%M:%S", tz="EST")
      df_grouped <- df_grouped_part1
      ########################################################################
      # df <- df %>% dplyr::select(V1,Site,Date,Day,time,Cycle_Length,Required_Cycle_Length,rotation,Progression,Degree_Saturation,Coordination)
      # df$time <- hms::as_hms(df$time)
      # df_final <- rbind(df,df_final)
      # }
      # 
      # df_final$Hour <- as.integer(round(df_final$time/3600,0)*3600)
      # dates_list <- unique(df_final$Date)
      # D <- dates_list[1]
      # df_grouped_part1 <- sqldf::sqldf(paste0("select Site, Date, Hour, avg(Degree_Saturation) as [Avg_DS]
      #                              from df_final
      #                              group by Site, Hour, Date"), drv="SQLite")
      # 
      # df_grouped_part1$Datetime <- as.POSIXct(paste0(as.Date(df_grouped_part1$Date)," ",hms::as_hms(df_grouped_part1$Hour)), format="%Y-%m-%d %H:%M:%S", tz="EST")
      # df_grouped <- df_grouped_part1
      
      # Building a Dataframe from the datetime
      lowerbound <- format(as.POSIXct(paste0(as.Date(min(df_grouped$Date))," ",hms::as_hms("00:00:00")), format="%Y-%m-%d %H:%M:%S", tz="EST"),"%Y-%m-%d %H:%M:%S")
      upperbound <- format(as.POSIXct(paste0(as.Date(max(df_grouped$Date))," ",hms::as_hms("23:00:00")), format="%Y-%m-%d %H:%M:%S", tz="EST"),"%Y-%m-%d %H:%M:%S")
      NoOfHours <- as.numeric(ymd_hms(upperbound) - ymd_hms(lowerbound))*24 
      datetime_list <- ymd_hms(lowerbound) + hours(0:NoOfHours)
      df_grouped2 <- as.data.frame(datetime_list)
      colnames(df_grouped2) <- c("Datetime")
      
      unique_sites <- as.data.frame(unique(df_final_heat$Site))
      colnames(unique_sites) <- c("Site")
      
      df_grouped2 <- tidyr::crossing(df_grouped2,unique_sites)
      df_grouped_part1$Datetime <- as.character(df_grouped_part1$Datetime)
      df_grouped2$Datetime <- as.character(df_grouped2$Datetime)
      df_grouped_part11 <- dplyr::left_join(df_grouped2, df_grouped_part1, by = c("Datetime" = "Datetime", "Site"="Site"))
      df_grouped_part11 <- df_grouped_part11 %>% dplyr::select(Datetime, Site, Avg_DS)
      df_grouped_part11$Avg_DS[is.na(df_grouped_part11$Avg_DS)] <- 0
      dfff <- reshape::cast(df_grouped_part11, Site ~ Datetime, fun.aggregate = mean, value ="Avg_DS")
      dfff <- as.data.frame(dfff)
      dfff <- dfff[,1:25]
      
  my_palette <- colorRampPalette(c("white", "orange", "red"))(n = 100)
  output$Plot4 <- renderPlot({
    print(heatmap.2(as.matrix(dfff[1:nrow(dfff),2:ncol(dfff)]),
                    col=my_palette,
                    Colv = NA, Rowv = NA, scale="none",
                    cexRow=0.8,cexCol =0.8, labRow=paste("site#", unlist(dfff[,1]),sep="")))})
  graphics.off()

  })
  
  output$downloadData5 <- downloadHandler(
    
    filename = function() { 
      paste("SM_Phase_", Sys.time(), ".csv", sep="")
    },
    content = function(file55) { 
      progress <- shiny::Progress$new()
      # Make sure it closes when we exit this reactive, even if there's an error
      on.exit(progress$close())
      
      progress$set(message = "Analyizing the data....", value = 0)
      
      # input$file4$datapath
      df_final_heat <- NULL
      site_subsytem_file <- "https://raw.githubusercontent.com/abdallahshabarek/ITS_NJDOT/main/Site_Subsystem.csv"
      phase_naming_file <- "https://raw.githubusercontent.com/abdallahshabarek/ITS_NJDOT/main/SCATS_SAs.csv"
      phase_directions_file <- "https://raw.githubusercontent.com/abdallahshabarek/ITS_NJDOT/main/SCATS_Phasing.csv"
      
      file <- input$file5$datapath[1]
      df <- read.delim2(file,header = FALSE)
      
      
      df_final <- NULL
      df_tmp1 <- df
      for (i in (1:nrow(df_tmp1))){
        print(paste0(i,"row"))
        if(!is.na(unlist(stringr::str_locate_all(pattern = paste0("\n"), df_tmp1[i,])[1])[1])){
          df_tmp11 <- as.data.frame(df_tmp1[1:i-1,])
          colnames(df_tmp11) <- c("V1")
          df_tmp2 <- as.data.frame(df_tmp1[i+1:nrow(df_tmp1),])
          colnames(df_tmp2) <- c("V1")
          df_tmp3 <- NULL
          df_tmp3$V1 <- NULL
          for (k in ((1:(1+length(unlist(stringr::str_locate_all(pattern = paste0("\n"), df_tmp1[i,])))/2)))){
            print(k)
            if( k == 1) {st <- 1} else {st <- 1+unlist(stringr::str_locate_all(pattern = paste0("\n"), df_tmp1[i,]))[k-1]}
            if( k == (1+length(unlist(stringr::str_locate_all(pattern = paste0("\n"), df_tmp1[i,])))/2)) {end <- nchar(df_tmp1[i,])} else {end <- unlist(stringr::str_locate_all(pattern = paste0("\n"), df_tmp1[i,]))[k]-1}
            tmp3 <- as.data.frame(substr(df_tmp1[i,],st,end))
            colnames(tmp3) <- c("V1")
            df_tmp3 <- as.data.frame(rbind(df_tmp3,tmp3))
            colnames(df_tmp3) <- c("V1")
          }
          df_final <- as.data.frame(rbind(df_final,df_tmp3))
          colnames(df_final) <- c("V1")
        } else {
          # df_tmp11 <- as.data.frame(df_tmp1[1:i-1,])
          # df_tmp2 <- df_tmp1[i:nrow(df_tmp1),]
          df_final <- as.data.frame(rbind(df_final,df_tmp1[i,]))
          colnames(df_final) <- c("V1")
        }
      }
      
      df <- df_final
      
      stringr::str_match(df$V1, "CT\\s*(.*?)\\s* ")[2,][2]
      gsub("^.*?_","_","ATGAS_1121")
      Date <- as.Date(paste0(substr(stringr::str_match(df$V1[1], "_\\s*(.*?)\\s*.sm")[1,][2],1,4),"-",substr(stringr::str_match(df$V1[1], "_\\s*(.*?)\\s*.sm")[1,][2],5,6),"-",substr(stringr::str_match(df$V1[1], "_\\s*(.*?)\\s*.sm")[1,][2],7,8)))
      Year <- substr(stringr::str_match(df$V1[1], "_\\s*(.*?)\\s*.sm")[1,][2],1,4)
      subsystem <- as.integer(substr(df$V1[2],unlist(stringr::str_locate_all(pattern = paste0("SS  "), df$V1[2])[1])[2]+1,unlist(stringr::str_locate_all(pattern = paste0("SS  "), df$V1[2])[1])[2]+2))
      RT_name <- as.character(stringr::str_match(df$V1[1], "filename:\\s*(.*?)\\s*_")[1,][2])
      
      substrRight <- function(x, n){substr(x, nchar(x)-n+1, nchar(x))}
      
      substr(df[950,],18,19)
      substr(df[950,],nchar(df[950,])-3,nchar(df[950,]))
      
      
      if(is.na(as.integer(substrRight(as.character(stringr::str_match(df$V1, "CT \\s*(.*?)\\s* RL")[2,][2]),4)))==TRUE){
        # print("HI")
        as.integer(substrRight(as.character(stringr::str_match(df$V1, "CT \\s*(.*?)\\s* RL")[2,][2]),3))
      }
      
      substr(df$V1[2],1,6)
      
      
      
      
      
      Day_name <- as.character(stringr::str_match(df$V1[1], "Strategic Monitor On \\s*(.*?)\\s* ")[1,][2])
      Date_name <- as.character(stringr::str_match(df$V1[1], paste0(Day_name, " \\s*(.*?)\\s* "))[1,][2])
      Subsystem_number <- as.integer(substr(df$V1[2],nchar(Day_name)+1+nchar(Date_name)+1+6+4+1,nchar(Day_name)+1+nchar(Date_name)+1+6+4+2))
      # Main_site <- as.integer(readr::read_csv(site_subsytem_file)$Site[readr::read_csv(site_subsytem_file)$Subsystem == Subsystem_number])
      # Main_site <- readr::read_csv(site_subsytem_file) %>% dplyr::filter(readr::read_csv(site_subsytem_file)$Subsystem == Subsystem_number & readr::read_csv(site_subsytem_file)$Route_name == RT_name)
      Main_site <- as.integer(as.data.frame(readr::read_csv(site_subsytem_file) %>% dplyr::filter(readr::read_csv(site_subsytem_file)$Subsystem == Subsystem_number & readr::read_csv(site_subsytem_file)$Route_name == RT_name))$Site)
      line_leng <- nchar(df[2,])
      
      df$Degree_Saturation <- ""
      df$Cycle_Length <- ""
      df$Required_Cycle_Length <- ""
      df$time <- ""
      df$Progression <- ""
      df$rotation <- ""
      df$Married <- ""
      df$type <- ""
      df$subsite <- ""
      df$strategic_approach <- ""
      df$notation <- ""
      df$phase <- ""
      df$phase_time <- ""
      df$Avg_DS_Phase <-""
      df$phase_A_GT <- ""
      df$phase_B_GT <- ""
      df$phase_C_GT <- ""
      df$phase_D_GT <- ""
      df$phase_E_GT <- ""
      df$phase_F_GT <- ""
      df$phase_G_GT <- ""
      df$phase_H_GT <- ""
      df$phase_I_GT <- ""
      
      for (i in 1:nrow(df)){
        # print(i/nrow(df)*100)
        if(substr(df$V1[i],1,6)=="Friday" | substr(df$V1[i],1,6)=="Saturd" | substr(df$V1[i],1,6)=="Sunday" | substr(df$V1[i],1,6)=="Monday" | substr(df$V1[i],1,6)=="Tuesda" | substr(df$V1[i],1,6)=="Wednes" | substr(df$V1[i],1,6)=="Thursd"){
          print(paste0(i,":",nchar(df[i,])))
          # df$Degree_Saturation[i] <- as.integer(substr(df[i,],line_leng-2,line_leng))[1]
          df$Degree_Saturation[i] <- as.integer(sub("^.+DS ", "", df$V1[i]))
          df$Cycle_Length[i] <- as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = "CT ", df[i,])[1])[2],unlist(stringr::str_locate_all(pattern = "CT ", df[i,])[1])[2]+4))
          df$Required_Cycle_Length[i] <- as.integer(substr(df$V1[i],1+unlist(stringr::str_locate_all(pattern = "RL", df[i,])[1])[2],unlist(stringr::str_locate_all(pattern = "RL", df[i,])[1])[2]+3))
          df$time[i] <- as_hms(as.POSIXct(substr(df[i,],nchar(Day_name)+1+nchar(Date_name)+2,nchar(Day_name)+1+nchar(Date_name)+1+5),format="%H:%M"))[1]
          df$Progression[i] <- as.double(substr(df[i,],nchar(Day_name)+1+nchar(Date_name)+1+6+4+3+6,nchar(Day_name)+1+nchar(Date_name)+1+6+4+4+7))[1]
          df$rotation[i] <- as.integer(substr(df[i,],line_leng-22,line_leng-20))[1]
          df$Married[i] <- as.character(substr(df[i,],nchar(Day_name)+1+nchar(Date_name)+1+6+4+3,nchar(Day_name)+1+nchar(Date_name)+1+6+4+4)[1])
        } else if (!is.na(as.numeric(substr(df[i,],1,2)[1]))==TRUE) {
          type <- as.character(stringr::str_match(substr(df$V1[i],2,nchar(df$V1[i])-2), " \\s*(.*?)\\s* ")[1,][2])
          df$type[i] <- type
          df$subsite[i] <- as.integer(stringr::str_match(df$V1[i], "\\s*(.*?)\\s* ")[1,][2])
          strategic_approach <- as.integer(stringr::str_match(df$V1[i], paste0(type," \\s*(.*?)\\s* "))[1,][2])
          df$strategic_approach[i] <- strategic_approach
          
          stringr::str_locate_all(pattern = paste0(type," ",strategic_approach," "), df[i,])[1]
          if(type == "SA"){
            end_str <- unlist(stringr::str_locate_all(pattern = paste0(type," ",strategic_approach," "), df[i,])[1])[2]
          } else if (type == "LK"){
            end_str <- unlist(stringr::str_locate_all(pattern = paste0(type,"  ",strategic_approach," "), df[i,])[1])[2]
          }
          df$notation[i] <- substr(df[i,],end_str+1,end_str+2)[1]
          end_str_phase <- unlist(stringr::str_locate_all(pattern = paste0("!"), df[i,])[1])[1]
          # df$phase[i] <- as.character(substr(df[i,],end_str+5,end_str+6)[1])
          df$phase[i] <- as.character(substr(df$V1[i],17,19))
          
          # df$phase_time[i] <- as.integer(substr(df[i,],end_str+8,end_str+10)[1])
          df$phase_time[i] <- as.integer(substr(stringr::str_match(df$V1[i], " \\s*(.*?)\\s*!")[1,][2],nchar(stringr::str_match(df$V1[i], " \\s*(.*?)\\s*!")[1,][2])-3,nchar(stringr::str_match(df$V1[i], " \\s*(.*?)\\s*!")[1,][2])))
          df$Avg_DS_Phase[i] <- as.integer(substr(df[i,],nchar(df[i,])-4,nchar(df[i,]))[1])
        } else if (substr(df$V1[i],1,3)=="A=<"){
          df$phase_A_GT[i] <- as.integer(stringr::str_match(df$V1[i], "A=<\\s*(.*?)\\s*>")[1,][2])
          df$phase_B_GT[i] <- 
            if(is.na(unlist(stringr::str_locate_all(pattern = paste0("B="), df$V1[i])[1])[1])==TRUE){
              ""  
            } else {
              as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("B="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("B="), df$V1[i])[1])[1]+4))
            }
          df$phase_C_GT[i] <- 
            if(is.na(unlist(stringr::str_locate_all(pattern = paste0("C="), df$V1[i])[1])[1])==TRUE){
              ""  
            } else {
              as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("C="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("C="), df$V1[i])[1])[1]+4))
            }
          df$phase_D_GT[i] <- 
            if(is.na(unlist(stringr::str_locate_all(pattern = paste0("D="), df$V1[i])[1])[1])==TRUE){
              ""  
            } else {
              as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("D="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("D="), df$V1[i])[1])[1]+4))
            }
          df$phase_E_GT[i] <- 
            if(is.na(unlist(stringr::str_locate_all(pattern = paste0("E="), df$V1[i])[1])[1])==TRUE){
              ""  
            } else {
              as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("E="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("E="), df$V1[i])[1])[1]+4))
            }
          df$phase_F_GT[i] <- 
            if(is.na(unlist(stringr::str_locate_all(pattern = paste0("F="), df$V1[i])[1])[1])==TRUE){
              ""  
            } else {
              as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("F="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("F="), df$V1[i])[1])[1]+4))
            }
          df$phase_G_GT[i] <- 
            if(is.na(unlist(stringr::str_locate_all(pattern = paste0("G="), df$V1[i])[1])[1])==TRUE){
              ""  
            } else {
              as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("G="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("G="), df$V1[i])[1])[1]+4))
            }
          df$phase_H_GT[i] <- 
            if(is.na(unlist(stringr::str_locate_all(pattern = paste0("H="), df$V1[i])[1])[1])==TRUE){
              ""  
            } else {
              as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("H="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("H="), df$V1[i])[1])[1]+4))
            }
          df$phase_I_GT[i] <- 
            if(is.na(unlist(stringr::str_locate_all(pattern = paste0("I="), df$V1[i])[1])[1])==TRUE){
              ""  
            } else {
              as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("I="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("I="), df$V1[i])[1])[1]+4))
            }
        }
      }
      
      df$Day <- Day_name
      df$Date <- Date
      df$Date_name <- Date_name
      df$SS <- subsystem
      # delete spaces in the selected column
      df$phase <- gsub('\\s+', '',df$phase)
      df <- df %>% dplyr::filter(phase != "AB" & phase != "BC" & phase != "BA" & phase != "CB" & phase != "AC" & phase != "CA" & phase != "AD" & phase != "DA" & phase != "BD" & phase != "DB" & phase != "DC" & phase != "CD")
      
      df1 <- readr::read_csv(phase_naming_file)
      # df2 <- readr::read_csv(phase_directions_file)
      # 
      # df3 <- df2 %>% tidyr::pivot_longer(!`Signal ID`, 
      #                                    names_to = "Directions", values_to = "Phases")
      # 
      # colnames(df3) <- c("Signal_ID","Direction","Phase")
      # colnames(df1) <- c("Site", "Phase_name", "Phase_number")
      # df4 <- sqldf::sqldf("select a.Signal_ID, a.Direction, a.Phase, b.Phase_name from df3 as a
      # left join df1 as b
      # on b.Site = a.Signal_ID and (a.Phase = b.Phase_number)")
      # df5 <- unique(df4)
      # df6 <- df5[!is.na(df5$Phase),]
      # df7 <- df6[df6$Signal_ID!="Signal ID",]
      # 
      # df11 <- df7 %>% dplyr::select("Signal_ID", "Phase", "Phase_name")
      # df11 <- unique(df11)
      # 
      # df11$Phase <- as.character(df11$Phase)
      # df22 <- sqldf::sqldf("select a.*,
      # case when (a.Type = 'SA' or a.Type='LK') and (a.phase <> 'A' and  a.phase <> 'B' and a.phase <> 'C' and a.phase <> 'D' and a.phase <> 'E' and a.phase <> 'F' and a.phase <> 'G' and a.phase <> 'H' and a.phase <> 'I') then b.Phase_name
      # when (a.Type = 'SA' or a.Type='LK') and (a.phase = 'A' or  a.phase = 'B' or a.phase = 'C' or a.phase = 'D' or a.phase = 'E' or a.phase = 'F' or a.phase = 'G' or a.phase = 'H' or a.phase = 'I') then a.Phase
      # when (a.Type <> 'SA' and a.Type <> 'LK') then ''
      # end as Phase_name
      # from df as a
      # left join df11 as b
      # on a.subsite = b.Signal_ID and a.phase = b.Phase", drv ="SQLite")
      df1$ss <- NULL
      df1 <- df1 %>% dplyr::filter(Site == Main_site)
      colnames(df1) <- c("site","phase_name","phase_number")
      df22 <- sqldf::sqldf("select a.*,
case when (a.Type = 'SA' or a.Type='LK') and (a.phase <> 'A' and  a.phase <> 'B' and a.phase <> 'C' and a.phase <> 'D' and a.phase <> 'E' and a.phase <> 'F' and a.phase <> 'G' and a.phase <> 'H' and a.phase <> 'I') then b.Phase_name
when (a.Type = 'SA' or a.Type='LK') and (a.phase = 'A' or  a.phase = 'B' or a.phase = 'C' or a.phase = 'D' or a.phase = 'E' or a.phase = 'F' or a.phase = 'G' or a.phase = 'H' or a.phase = 'I') then a.Phase
when (a.Type <> 'SA' and a.Type <> 'LK') then ''
end as Phase_name
from df as a
left join df1 as b
on a.phase = b.phase_number", drv ="SQLite")
      
      
      
      df22 <- df22 %>% naniar::replace_with_na(replace = list(time = c("")))
      df22 <- df22 %>% naniar::replace_with_na(replace = list(rotation = c("")))
      df22 <- df22 %>% naniar::replace_with_na(replace = list(Progression = c("")))
      df22 <- df22 %>% naniar::replace_with_na(replace = list(Cycle_Length = c("")))
      df22 <- df22 %>% naniar::replace_with_na(replace = list(Required_Cycle_Length = c("")))
      df22 <- df22 %>% naniar::replace_with_na(replace = list(Degree_Saturation = c("")))
      df22 <- df22 %>% naniar::replace_with_na(replace = list(Married = c("")))
      df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_A_GT = c("")))
      df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_B_GT = c("")))
      df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_C_GT = c("")))
      df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_D_GT = c("")))
      df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_E_GT = c("")))
      df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_F_GT = c("")))
      df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_G_GT = c("")))
      df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_H_GT = c("")))
      df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_I_GT = c("")))
      
      
      df22 <- df22 %>% tidyr::fill(rotation, .direction = "down")
      df22 <- df22 %>% tidyr::fill(Progression, .direction = "down")
      df22 <- df22 %>% tidyr::fill(Cycle_Length, .direction = "down")
      df22 <- df22 %>% tidyr::fill(time, .direction = "down")
      df22 <- df22 %>% tidyr::fill(Required_Cycle_Length, .direction = "down")
      df22 <- df22 %>% tidyr::fill(Degree_Saturation, .direction = "down")
      df22 <- df22 %>% tidyr::fill(Married, .direction = "down")
      df22 <- df22 %>% tidyr::fill(phase_A_GT, .direction = "up")
      df22 <- df22 %>% tidyr::fill(phase_B_GT, .direction = "up")
      df22 <- df22 %>% tidyr::fill(phase_C_GT, .direction = "up")
      df22 <- df22 %>% tidyr::fill(phase_D_GT, .direction = "up")
      df22 <- df22 %>% tidyr::fill(phase_E_GT, .direction = "up")
      df22 <- df22 %>% tidyr::fill(phase_F_GT, .direction = "up")
      df22 <- df22 %>% tidyr::fill(phase_G_GT, .direction = "up")
      df22 <- df22 %>% tidyr::fill(phase_H_GT, .direction = "up")
      df22 <- df22 %>% tidyr::fill(phase_I_GT, .direction = "up")
      df22 <- df22[!grepl(" Int",substr(df22$V1,1,4)),]
      df22 <- df22[!grepl("Strategic Monitor On",substr(df22$V1,1,20)),]
      df22 <- df22[!grepl("A=<",substr(df22$V1,1,3)),]
      
      # To get the G_DS Ratio
      df_phase <- df22 %>% dplyr::filter(type == "SA" & subsite == Main_site)
      df_phase$time <- hms::as_hms(as.POSIXct(paste0(sprintf("%02d",as.integer(as.integer(df_phase$time)/3600)),":",sprintf("%02d",as.integer(as.integer((as.integer(df_phase$time))%%3600)/60))),format="%H:%M"))
      df_phase$time <- as.character(df_phase$time)
      df_phase$Avg_DS_Phase <- as.double(df_phase$Avg_DS_Phase)
      df_phase <- sqldf::sqldf("select a.time, a.Phase_name, 
Avg(a.Avg_DS_Phase) as [Avg_DS_by_Phase], phase_A_GT, phase_B_GT, phase_C_GT, phase_D_GT, 
phase_E_GT, phase_F_GT, phase_G_GT, phase_H_GT, phase_I_GT
from df_phase as a 
group by a.time, a.Phase_name", drv="SQLite")
      
      df_phase <- sqldf::sqldf("select a.time, a.Phase_name, a.Avg_DS_by_Phase, 
case when (a.Phase_name= 'A') then a.phase_A_GT
when (a.Phase_name= 'B') then a.phase_B_GT
when (a.Phase_name= 'C') then a.phase_C_GT
when (a.Phase_name= 'D') then a.phase_D_GT
when (a.Phase_name= 'E') then a.phase_E_GT
when (a.Phase_name= 'F') then a.phase_F_GT
when (a.Phase_name= 'G') then a.phase_G_GT
when (a.Phase_name= 'H') then a.phase_H_GT
when (a.Phase_name= 'I') then a.phase_I_GT
End as Phase_time
from df_phase as a", drv="SQLite")
      
      df_phase$DS_Green_Ratio <- as.double(as.double(df_phase$Avg_DS_by_Phase)/as.double(df_phase$Phase_time))
      
      
      phases_unique <- as.data.frame(unique(df_phase$Phase_name))
      colnames(phases_unique) <- "unique_phases"
      
      plot_label <- paste0("Site ",Main_site," on ",Day_name," ",Date_name)
      if(nrow(phases_unique)==2){
        xdata <- hms::as_hms(unique(df_phase$time))
        y1 <- df_phase$DS_Green_Ratio[df_phase$Phase_name==phases_unique$unique_phases[1]]
        y2 <- df_phase$DS_Green_Ratio[df_phase$Phase_name==phases_unique$unique_phases[2]]
        # plot(xdata, y1, type="o", col="blue", pch="o", lty=1, ylim=c(0,2),xlab= "Time", ylab="Avg DS to Green Time" )
        df_plot <- data.frame(xdata, y1, y2)
        g <- ggplot(df_plot, aes(xdata))
        g <- g + ggtitle(plot_label)
        g <- g + geom_line(aes(y=y1, color = paste0("Phase ",phases_unique$unique_phases[1])))
        g <- g + geom_line(aes(y=y2, color = paste0("Phase ",phases_unique$unique_phases[2])))
        g <- g + theme(plot.title = element_text(color="black", size=10, face="bold.italic"),
                       axis.text=element_text(size=8),
                       axis.title=element_text(size=9,face="bold"),
                       legend.text=element_text(size=9),
                       legend.justification=c(1,1),legend.position=c(0.24,1),legend.title=element_blank())
        
        g <- g + scale_color_manual(name = "Phases", values = c("red","darkblue"))
        # g <- g + scale_color_manual(name = "Phase B", values = c("darkblue"))
        g <- g + ylab("Avg DS to Phase Time") + xlab("Time")
        g
      }
      if(nrow(phases_unique)==3){
        xdata <- hms::as_hms(unique(df_phase$time))
        y1 <- df_phase$DS_Green_Ratio[df_phase$Phase_name==phases_unique$unique_phases[1]]
        y2 <- df_phase$DS_Green_Ratio[df_phase$Phase_name==phases_unique$unique_phases[2]]
        y3 <- df_phase$DS_Green_Ratio[df_phase$Phase_name==phases_unique$unique_phases[3]]
        # plot(xdata, y1, type="o", col="blue", pch="o", lty=1, ylim=c(0,2),xlab= "Time", ylab="Avg DS to Green Time" )
        df_plot <- data.frame(xdata, y1, y2, y3)
        g <- ggplot(df_plot, aes(xdata))
        g <- g + ggtitle(plot_label)
        g <- g + geom_line(aes(y=y1, color = paste0("Phase ",phases_unique$unique_phases[1])))
        g <- g + geom_line(aes(y=y2, color = paste0("Phase ",phases_unique$unique_phases[2])))
        g <- g + geom_line(aes(y=y3, color = paste0("Phase ",phases_unique$unique_phases[3])))
        g <- g + theme(plot.title = element_text(color="black", size=10, face="bold.italic"),
                       axis.text=element_text(size=8),
                       axis.title=element_text(size=9,face="bold"),
                       legend.text=element_text(size=9),
                       legend.justification=c(1,1),legend.position=c(0.24,1),legend.title=element_blank())
        
        g <- g + scale_color_manual(name = "Phases", values = c("red","darkblue","darkgreen"))
        # g <- g + scale_color_manual(name = "Phase B", values = c("darkblue"))
        g <- g + ylab("Avg DS to Phase Time") + xlab("Time")
        g
      }
      if(nrow(phases_unique)==4){
        xdata <- hms::as_hms(unique(df_phase$time))
        y1 <- df_phase$DS_Green_Ratio[df_phase$Phase_name==phases_unique$unique_phases[1]]
        y2 <- df_phase$DS_Green_Ratio[df_phase$Phase_name==phases_unique$unique_phases[2]]
        y3 <- df_phase$DS_Green_Ratio[df_phase$Phase_name==phases_unique$unique_phases[3]]
        y4 <- df_phase$DS_Green_Ratio[df_phase$Phase_name==phases_unique$unique_phases[4]]
        # plot(xdata, y1, type="o", col="blue", pch="o", lty=1, ylim=c(0,2),xlab= "Time", ylab="Avg DS to Green Time" )
        df_plot <- data.frame(xdata, y1, y2, y3, y4)
        g <- ggplot(df_plot, aes(xdata))
        g <- g + ggtitle(plot_label)
        g <- g + geom_line(aes(y=y1, color = paste0("Phase ",phases_unique$unique_phases[1])))
        g <- g + geom_line(aes(y=y2, color = paste0("Phase ",phases_unique$unique_phases[2])))
        g <- g + geom_line(aes(y=y3, color = paste0("Phase ",phases_unique$unique_phases[3])))
        g <- g + geom_line(aes(y=y4, color = paste0("Phase ",phases_unique$unique_phases[4])))
        g <- g + theme(plot.title = element_text(color="black", size=10, face="bold.italic"),
                       axis.text=element_text(size=8),
                       axis.title=element_text(size=9,face="bold"),
                       legend.text=element_text(size=9),
                       legend.justification=c(1,1),legend.position=c(0.24,1),legend.title=element_blank())
        
        g <- g + scale_color_manual(name = "Phases", values = c("red","darkblue","darkgreen","orange"))
        # g <- g + scale_color_manual(name = "Phase B", values = c("darkblue"))
        g <- g + ylab("Avg DS to Phase Time") + xlab("Time")
        g
      }
      if(nrow(phases_unique)==5){
        xdata <- hms::as_hms(unique(df_phase$time))
        y1 <- df_phase$DS_Green_Ratio[df_phase$Phase_name==phases_unique$unique_phases[1]]
        y2 <- df_phase$DS_Green_Ratio[df_phase$Phase_name==phases_unique$unique_phases[2]]
        y3 <- df_phase$DS_Green_Ratio[df_phase$Phase_name==phases_unique$unique_phases[3]]
        y4 <- df_phase$DS_Green_Ratio[df_phase$Phase_name==phases_unique$unique_phases[4]]
        y5 <- df_phase$DS_Green_Ratio[df_phase$Phase_name==phases_unique$unique_phases[5]]
        # plot(xdata, y1, type="o", col="blue", pch="o", lty=1, ylim=c(0,2),xlab= "Time", ylab="Avg DS to Green Time" )
        df_plot <- data.frame(xdata, y1, y2, y3, y4, y5)
        g <- ggplot(df_plot, aes(xdata))
        g <- g + ggtitle(plot_label)
        g <- g + geom_line(aes(y=y1, color = paste0("Phase ",phases_unique$unique_phases[1])))
        g <- g + geom_line(aes(y=y2, color = paste0("Phase ",phases_unique$unique_phases[2])))
        g <- g + geom_line(aes(y=y3, color = paste0("Phase ",phases_unique$unique_phases[3])))
        g <- g + geom_line(aes(y=y4, color = paste0("Phase ",phases_unique$unique_phases[4])))
        g <- g + geom_line(aes(y=y5, color = paste0("Phase ",phases_unique$unique_phases[5])))
        g <- g + theme(plot.title = element_text(color="black", size=10, face="bold.italic"),
                       axis.text=element_text(size=8),
                       axis.title=element_text(size=9,face="bold"),
                       legend.text=element_text(size=9),
                       legend.justification=c(1,1),legend.position=c(0.24,1),legend.title=element_blank())
        
        g <- g + scale_color_manual(name = "Phases", values = c("red","darkblue","darkgreen","orange","cyan"))
        # g <- g + scale_color_manual(name = "Phase B", values = c("darkblue"))
        g <- g + ylab("Avg DS to Phase Time") + xlab("Time")
        g
      }
      write.csv(df_phase, file55, row.names=FALSE)
    })
  observeEvent(input$previewData5, {
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    
    progress$set(message = "Creating a Plot....", value = 0)  
    df_final_heat <- NULL
    site_subsytem_file <- "https://raw.githubusercontent.com/abdallahshabarek/ITS_NJDOT/main/Site_Subsystem.csv"
    phase_naming_file <- "https://raw.githubusercontent.com/abdallahshabarek/ITS_NJDOT/main/SCATS_SAs.csv"
    phase_directions_file <- "https://raw.githubusercontent.com/abdallahshabarek/ITS_NJDOT/main/SCATS_Phasing.csv"
    
    file <- input$file5$datapath[1]
    df <- read.delim2(file,header = FALSE)
    
    
    df_final <- NULL
    df_tmp1 <- df
    for (i in (1:nrow(df_tmp1))){
      print(paste0(i,"row"))
      if(!is.na(unlist(stringr::str_locate_all(pattern = paste0("\n"), df_tmp1[i,])[1])[1])){
        df_tmp11 <- as.data.frame(df_tmp1[1:i-1,])
        colnames(df_tmp11) <- c("V1")
        df_tmp2 <- as.data.frame(df_tmp1[i+1:nrow(df_tmp1),])
        colnames(df_tmp2) <- c("V1")
        df_tmp3 <- NULL
        df_tmp3$V1 <- NULL
        for (k in ((1:(1+length(unlist(stringr::str_locate_all(pattern = paste0("\n"), df_tmp1[i,])))/2)))){
          print(k)
          if( k == 1) {st <- 1} else {st <- 1+unlist(stringr::str_locate_all(pattern = paste0("\n"), df_tmp1[i,]))[k-1]}
          if( k == (1+length(unlist(stringr::str_locate_all(pattern = paste0("\n"), df_tmp1[i,])))/2)) {end <- nchar(df_tmp1[i,])} else {end <- unlist(stringr::str_locate_all(pattern = paste0("\n"), df_tmp1[i,]))[k]-1}
          tmp3 <- as.data.frame(substr(df_tmp1[i,],st,end))
          colnames(tmp3) <- c("V1")
          df_tmp3 <- as.data.frame(rbind(df_tmp3,tmp3))
          colnames(df_tmp3) <- c("V1")
        }
        df_final <- as.data.frame(rbind(df_final,df_tmp3))
        colnames(df_final) <- c("V1")
      } else {
        # df_tmp11 <- as.data.frame(df_tmp1[1:i-1,])
        # df_tmp2 <- df_tmp1[i:nrow(df_tmp1),]
        df_final <- as.data.frame(rbind(df_final,df_tmp1[i,]))
        colnames(df_final) <- c("V1")
      }
    }
    
    df <- df_final
    
    stringr::str_match(df$V1, "CT\\s*(.*?)\\s* ")[2,][2]
    gsub("^.*?_","_","ATGAS_1121")
    Date <- as.Date(paste0(substr(stringr::str_match(df$V1[1], "_\\s*(.*?)\\s*.sm")[1,][2],1,4),"-",substr(stringr::str_match(df$V1[1], "_\\s*(.*?)\\s*.sm")[1,][2],5,6),"-",substr(stringr::str_match(df$V1[1], "_\\s*(.*?)\\s*.sm")[1,][2],7,8)))
    Year <- substr(stringr::str_match(df$V1[1], "_\\s*(.*?)\\s*.sm")[1,][2],1,4)
    subsystem <- as.integer(substr(df$V1[2],unlist(stringr::str_locate_all(pattern = paste0("SS  "), df$V1[2])[1])[2]+1,unlist(stringr::str_locate_all(pattern = paste0("SS  "), df$V1[2])[1])[2]+2))
    RT_name <- as.character(stringr::str_match(df$V1[1], "filename:\\s*(.*?)\\s*_")[1,][2])
    
    substrRight <- function(x, n){substr(x, nchar(x)-n+1, nchar(x))}
    
    substr(df[950,],18,19)
    substr(df[950,],nchar(df[950,])-3,nchar(df[950,]))
    
    
    if(is.na(as.integer(substrRight(as.character(stringr::str_match(df$V1, "CT \\s*(.*?)\\s* RL")[2,][2]),4)))==TRUE){
      # print("HI")
      as.integer(substrRight(as.character(stringr::str_match(df$V1, "CT \\s*(.*?)\\s* RL")[2,][2]),3))
    }
    
    substr(df$V1[2],1,6)
    
    
    
    
    
    Day_name <- as.character(stringr::str_match(df$V1[1], "Strategic Monitor On \\s*(.*?)\\s* ")[1,][2])
    Date_name <- as.character(stringr::str_match(df$V1[1], paste0(Day_name, " \\s*(.*?)\\s* "))[1,][2])
    Subsystem_number <- as.integer(substr(df$V1[2],nchar(Day_name)+1+nchar(Date_name)+1+6+4+1,nchar(Day_name)+1+nchar(Date_name)+1+6+4+2))
    # Main_site <- as.integer(readr::read_csv(site_subsytem_file)$Site[readr::read_csv(site_subsytem_file)$Subsystem == Subsystem_number])
    # Main_site <- readr::read_csv(site_subsytem_file) %>% dplyr::filter(readr::read_csv(site_subsytem_file)$Subsystem == Subsystem_number & readr::read_csv(site_subsytem_file)$Route_name == RT_name)
    Main_site <- as.integer(as.data.frame(readr::read_csv(site_subsytem_file) %>% dplyr::filter(readr::read_csv(site_subsytem_file)$Subsystem == Subsystem_number & readr::read_csv(site_subsytem_file)$Route_name == RT_name))$Site)
    line_leng <- nchar(df[2,])
    
    df$Degree_Saturation <- ""
    df$Cycle_Length <- ""
    df$Required_Cycle_Length <- ""
    df$time <- ""
    df$Progression <- ""
    df$rotation <- ""
    df$Married <- ""
    df$type <- ""
    df$subsite <- ""
    df$strategic_approach <- ""
    df$notation <- ""
    df$phase <- ""
    df$phase_time <- ""
    df$Avg_DS_Phase <-""
    df$phase_A_GT <- ""
    df$phase_B_GT <- ""
    df$phase_C_GT <- ""
    df$phase_D_GT <- ""
    df$phase_E_GT <- ""
    df$phase_F_GT <- ""
    df$phase_G_GT <- ""
    df$phase_H_GT <- ""
    df$phase_I_GT <- ""
    
    for (i in 1:nrow(df)){
      # print(i/nrow(df)*100)
      if(substr(df$V1[i],1,6)=="Friday" | substr(df$V1[i],1,6)=="Saturd" | substr(df$V1[i],1,6)=="Sunday" | substr(df$V1[i],1,6)=="Monday" | substr(df$V1[i],1,6)=="Tuesda" | substr(df$V1[i],1,6)=="Wednes" | substr(df$V1[i],1,6)=="Thursd"){
        print(paste0(i,":",nchar(df[i,])))
        # df$Degree_Saturation[i] <- as.integer(substr(df[i,],line_leng-2,line_leng))[1]
        df$Degree_Saturation[i] <- as.integer(sub("^.+DS ", "", df$V1[i]))
        df$Cycle_Length[i] <- as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = "CT ", df[i,])[1])[2],unlist(stringr::str_locate_all(pattern = "CT ", df[i,])[1])[2]+4))
        df$Required_Cycle_Length[i] <- as.integer(substr(df$V1[i],1+unlist(stringr::str_locate_all(pattern = "RL", df[i,])[1])[2],unlist(stringr::str_locate_all(pattern = "RL", df[i,])[1])[2]+3))
        df$time[i] <- as_hms(as.POSIXct(substr(df[i,],nchar(Day_name)+1+nchar(Date_name)+2,nchar(Day_name)+1+nchar(Date_name)+1+5),format="%H:%M"))[1]
        df$Progression[i] <- as.double(substr(df[i,],nchar(Day_name)+1+nchar(Date_name)+1+6+4+3+6,nchar(Day_name)+1+nchar(Date_name)+1+6+4+4+7))[1]
        df$rotation[i] <- as.integer(substr(df[i,],line_leng-22,line_leng-20))[1]
        df$Married[i] <- as.character(substr(df[i,],nchar(Day_name)+1+nchar(Date_name)+1+6+4+3,nchar(Day_name)+1+nchar(Date_name)+1+6+4+4)[1])
      } else if (!is.na(as.numeric(substr(df[i,],1,2)[1]))==TRUE) {
        type <- as.character(stringr::str_match(substr(df$V1[i],2,nchar(df$V1[i])-2), " \\s*(.*?)\\s* ")[1,][2])
        df$type[i] <- type
        df$subsite[i] <- as.integer(stringr::str_match(df$V1[i], "\\s*(.*?)\\s* ")[1,][2])
        strategic_approach <- as.integer(stringr::str_match(df$V1[i], paste0(type," \\s*(.*?)\\s* "))[1,][2])
        df$strategic_approach[i] <- strategic_approach
        
        stringr::str_locate_all(pattern = paste0(type," ",strategic_approach," "), df[i,])[1]
        if(type == "SA"){
          end_str <- unlist(stringr::str_locate_all(pattern = paste0(type," ",strategic_approach," "), df[i,])[1])[2]
        } else if (type == "LK"){
          end_str <- unlist(stringr::str_locate_all(pattern = paste0(type,"  ",strategic_approach," "), df[i,])[1])[2]
        }
        df$notation[i] <- substr(df[i,],end_str+1,end_str+2)[1]
        end_str_phase <- unlist(stringr::str_locate_all(pattern = paste0("!"), df[i,])[1])[1]
        # df$phase[i] <- as.character(substr(df[i,],end_str+5,end_str+6)[1])
        df$phase[i] <- as.character(substr(df$V1[i],17,19))
        
        # df$phase_time[i] <- as.integer(substr(df[i,],end_str+8,end_str+10)[1])
        df$phase_time[i] <- as.integer(substr(stringr::str_match(df$V1[i], " \\s*(.*?)\\s*!")[1,][2],nchar(stringr::str_match(df$V1[i], " \\s*(.*?)\\s*!")[1,][2])-3,nchar(stringr::str_match(df$V1[i], " \\s*(.*?)\\s*!")[1,][2])))
        df$Avg_DS_Phase[i] <- as.integer(substr(df[i,],nchar(df[i,])-4,nchar(df[i,]))[1])
      } else if (substr(df$V1[i],1,3)=="A=<"){
        df$phase_A_GT[i] <- as.integer(stringr::str_match(df$V1[i], "A=<\\s*(.*?)\\s*>")[1,][2])
        df$phase_B_GT[i] <- 
          if(is.na(unlist(stringr::str_locate_all(pattern = paste0("B="), df$V1[i])[1])[1])==TRUE){
            ""  
          } else {
            as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("B="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("B="), df$V1[i])[1])[1]+4))
          }
        df$phase_C_GT[i] <- 
          if(is.na(unlist(stringr::str_locate_all(pattern = paste0("C="), df$V1[i])[1])[1])==TRUE){
            ""  
          } else {
            as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("C="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("C="), df$V1[i])[1])[1]+4))
          }
        df$phase_D_GT[i] <- 
          if(is.na(unlist(stringr::str_locate_all(pattern = paste0("D="), df$V1[i])[1])[1])==TRUE){
            ""  
          } else {
            as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("D="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("D="), df$V1[i])[1])[1]+4))
          }
        df$phase_E_GT[i] <- 
          if(is.na(unlist(stringr::str_locate_all(pattern = paste0("E="), df$V1[i])[1])[1])==TRUE){
            ""  
          } else {
            as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("E="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("E="), df$V1[i])[1])[1]+4))
          }
        df$phase_F_GT[i] <- 
          if(is.na(unlist(stringr::str_locate_all(pattern = paste0("F="), df$V1[i])[1])[1])==TRUE){
            ""  
          } else {
            as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("F="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("F="), df$V1[i])[1])[1]+4))
          }
        df$phase_G_GT[i] <- 
          if(is.na(unlist(stringr::str_locate_all(pattern = paste0("G="), df$V1[i])[1])[1])==TRUE){
            ""  
          } else {
            as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("G="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("G="), df$V1[i])[1])[1]+4))
          }
        df$phase_H_GT[i] <- 
          if(is.na(unlist(stringr::str_locate_all(pattern = paste0("H="), df$V1[i])[1])[1])==TRUE){
            ""  
          } else {
            as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("H="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("H="), df$V1[i])[1])[1]+4))
          }
        df$phase_I_GT[i] <- 
          if(is.na(unlist(stringr::str_locate_all(pattern = paste0("I="), df$V1[i])[1])[1])==TRUE){
            ""  
          } else {
            as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("I="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("I="), df$V1[i])[1])[1]+4))
          }
      }
    }
    
    df$Day <- Day_name
    df$Date <- Date
    df$Date_name <- Date_name
    df$SS <- subsystem
    # delete spaces in the selected column
    df$phase <- gsub('\\s+', '',df$phase)
    df <- df %>% dplyr::filter(phase != "AB" & phase != "BC" & phase != "BA" & phase != "CB" & phase != "AC" & phase != "CA" & phase != "AD" & phase != "DA" & phase != "BD" & phase != "DB" & phase != "DC" & phase != "CD")
    
    
    df1 <- readr::read_csv(phase_naming_file)
    # df2 <- readr::read_csv(phase_directions_file)
    # 
    # df3 <- df2 %>% tidyr::pivot_longer(!`Signal ID`, 
    #                                    names_to = "Directions", values_to = "Phases")
    # 
    # colnames(df3) <- c("Signal_ID","Direction","Phase")
    # colnames(df1) <- c("Site", "Phase_name", "Phase_number")
    # df4 <- sqldf::sqldf("select a.Signal_ID, a.Direction, a.Phase, b.Phase_name from df3 as a
    # left join df1 as b
    # on b.Site = a.Signal_ID and (a.Phase = b.Phase_number)")
    # df5 <- unique(df4)
    # df6 <- df5[!is.na(df5$Phase),]
    # df7 <- df6[df6$Signal_ID!="Signal ID",]
    # 
    # df11 <- df7 %>% dplyr::select("Signal_ID", "Phase", "Phase_name")
    # df11 <- unique(df11)
    # 
    # df11$Phase <- as.character(df11$Phase)
    # df22 <- sqldf::sqldf("select a.*,
    # case when (a.Type = 'SA' or a.Type='LK') and (a.phase <> 'A' and  a.phase <> 'B' and a.phase <> 'C' and a.phase <> 'D' and a.phase <> 'E' and a.phase <> 'F' and a.phase <> 'G' and a.phase <> 'H' and a.phase <> 'I') then b.Phase_name
    # when (a.Type = 'SA' or a.Type='LK') and (a.phase = 'A' or  a.phase = 'B' or a.phase = 'C' or a.phase = 'D' or a.phase = 'E' or a.phase = 'F' or a.phase = 'G' or a.phase = 'H' or a.phase = 'I') then a.Phase
    # when (a.Type <> 'SA' and a.Type <> 'LK') then ''
    # end as Phase_name
    # from df as a
    # left join df11 as b
    # on a.subsite = b.Signal_ID and a.phase = b.Phase", drv ="SQLite")
    df1$ss <- NULL
    df1 <- df1 %>% dplyr::filter(Site == Main_site)
    colnames(df1) <- c("site","phase_name","phase_number")
    df22 <- sqldf::sqldf("select a.*,
case when (a.Type = 'SA' or a.Type='LK') and (a.phase <> 'A' and  a.phase <> 'B' and a.phase <> 'C' and a.phase <> 'D' and a.phase <> 'E' and a.phase <> 'F' and a.phase <> 'G' and a.phase <> 'H' and a.phase <> 'I') then b.Phase_name
when (a.Type = 'SA' or a.Type='LK') and (a.phase = 'A' or  a.phase = 'B' or a.phase = 'C' or a.phase = 'D' or a.phase = 'E' or a.phase = 'F' or a.phase = 'G' or a.phase = 'H' or a.phase = 'I') then a.Phase
when (a.Type <> 'SA' and a.Type <> 'LK') then ''
end as Phase_name
from df as a
left join df1 as b
on a.phase = b.phase_number", drv ="SQLite")
    
    
    
    df22 <- df22 %>% naniar::replace_with_na(replace = list(time = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(rotation = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(Progression = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(Cycle_Length = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(Required_Cycle_Length = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(Degree_Saturation = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(Married = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_A_GT = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_B_GT = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_C_GT = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_D_GT = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_E_GT = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_F_GT = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_G_GT = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_H_GT = c("")))
    df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_I_GT = c("")))
    
    
    df22 <- df22 %>% tidyr::fill(rotation, .direction = "down")
    df22 <- df22 %>% tidyr::fill(Progression, .direction = "down")
    df22 <- df22 %>% tidyr::fill(Cycle_Length, .direction = "down")
    df22 <- df22 %>% tidyr::fill(time, .direction = "down")
    df22 <- df22 %>% tidyr::fill(Required_Cycle_Length, .direction = "down")
    df22 <- df22 %>% tidyr::fill(Degree_Saturation, .direction = "down")
    df22 <- df22 %>% tidyr::fill(Married, .direction = "down")
    df22 <- df22 %>% tidyr::fill(phase_A_GT, .direction = "up")
    df22 <- df22 %>% tidyr::fill(phase_B_GT, .direction = "up")
    df22 <- df22 %>% tidyr::fill(phase_C_GT, .direction = "up")
    df22 <- df22 %>% tidyr::fill(phase_D_GT, .direction = "up")
    df22 <- df22 %>% tidyr::fill(phase_E_GT, .direction = "up")
    df22 <- df22 %>% tidyr::fill(phase_F_GT, .direction = "up")
    df22 <- df22 %>% tidyr::fill(phase_G_GT, .direction = "up")
    df22 <- df22 %>% tidyr::fill(phase_H_GT, .direction = "up")
    df22 <- df22 %>% tidyr::fill(phase_I_GT, .direction = "up")
    df22 <- df22[!grepl(" Int",substr(df22$V1,1,4)),]
    df22 <- df22[!grepl("Strategic Monitor On",substr(df22$V1,1,20)),]
    df22 <- df22[!grepl("A=<",substr(df22$V1,1,3)),]
    
    
    # To get the G_DS Ratio
    df_phase <- df22 %>% dplyr::filter(type == "SA" & subsite == Main_site)
    df_phase$time <- hms::as_hms(as.POSIXct(paste0(sprintf("%02d",as.integer(as.integer(df_phase$time)/3600)),":",sprintf("%02d",as.integer(as.integer((as.integer(df_phase$time))%%3600)/60))),format="%H:%M"))
    df_phase$time <- as.character(df_phase$time)
    df_phase$Avg_DS_Phase <- as.double(df_phase$Avg_DS_Phase)
    
    
    # To get SG
    sites_direction <- readr::read_csv("https://raw.githubusercontent.com/abdallahshabarek/ITS_NJDOT/main/SCATS_Phasing.csv")
    sites_direction <- sites_direction %>%
      tidyr::pivot_longer(!`Signal ID`, names_to = "direction", values_to = "SG")
    colnames(sites_direction) <- c("site","direction","SG")
    sites_direction <- na.omit(sites_direction)
    sites_direction_onesite <- sites_direction %>% dplyr::filter(site == Main_site)
        df_sg <- sqldf::sqldf("select a.time, a.Phase_name, a.phase,
Avg(a.Avg_DS_Phase) as [Avg_DS_by_SG], phase_A_GT, phase_B_GT, phase_C_GT, phase_D_GT, 
phase_E_GT, phase_F_GT, phase_G_GT, phase_H_GT, phase_I_GT
from df_phase as a 
group by a.time, a.Phase_name, a.phase", drv="SQLite")
    df_sg <- sqldf::sqldf("select a.time, a.Phase_name, a.phase, a.Avg_DS_by_SG, 
case when (a.Phase_name= 'A') then cast(a.phase_A_GT as int)
when (a.Phase_name= 'B') then cast(a.phase_B_GT as int)
when (a.Phase_name= 'C') then cast(a.phase_C_GT as int)
when (a.Phase_name= 'D') then cast(a.phase_D_GT as int)
when (a.Phase_name= 'E') then cast(a.phase_E_GT as int)
when (a.Phase_name= 'F') then cast(a.phase_F_GT as int)
when (a.Phase_name= 'G') then cast(a.phase_G_GT as int)
when (a.Phase_name= 'H') then cast(a.phase_H_GT as int)
when (a.Phase_name= 'I') then cast(a.phase_I_GT as int)
End as Phase_time
from df_sg as a", drv="SQLite")
    sg_unique <- as.data.frame(unique(df_sg$phase))
    colnames(sg_unique) <- "unique_sg"
    sites_direction_onesite <- aggregate(data=sites_direction_onesite, direction~site+SG,FUN=paste,collapse = ",")
    # sites_direction_onesite$direction <- as.character(sites_direction_onesite)
    
    # Going back to the Phases
    df_phase <- sqldf::sqldf("select a.time, a.Phase_name, 
Avg(a.Avg_DS_Phase) as [Avg_DS_by_Phase], phase_A_GT, phase_B_GT, phase_C_GT, phase_D_GT, 
phase_E_GT, phase_F_GT, phase_G_GT, phase_H_GT, phase_I_GT
from df_phase as a 
group by a.time, a.Phase_name", drv="SQLite")
    
    
    
    df_phase <- sqldf::sqldf("select a.time, a.Phase_name, a.Avg_DS_by_Phase, 
case when (a.Phase_name= 'A') then cast(a.phase_A_GT as int)
when (a.Phase_name= 'B') then cast(a.phase_B_GT as int)
when (a.Phase_name= 'C') then cast(a.phase_C_GT as int)
when (a.Phase_name= 'D') then cast(a.phase_D_GT as int)
when (a.Phase_name= 'E') then cast(a.phase_E_GT as int)
when (a.Phase_name= 'F') then cast(a.phase_F_GT as int)
when (a.Phase_name= 'G') then cast(a.phase_G_GT as int)
when (a.Phase_name= 'H') then cast(a.phase_H_GT as int)
when (a.Phase_name= 'I') then cast(a.phase_I_GT as int)
End as Phase_time
from df_phase as a", drv="SQLite")
    
    # df_phase$DS_Green_Ratio <- as.double(as.double(df_phase$Avg_DS_by_Phase)/as.double(df_phase$Phase_time))
    
# https://www.r-graph-gallery.com/line-chart-dual-Y-axis-ggplot2.html    
    phases_unique <- as.data.frame(unique(df_phase$Phase_name))
    colnames(phases_unique) <- "unique_phases"
    
    df_sg$phase <- as.integer(df_sg$phase)
     df_sg$DS_Green_Ratio <- as.double(df_sg$Avg_DS_by_SG/df_sg$Phase_time)
    
    
    
    if (input$degree_saturation_analysis_type == "Two Axis"){
      if(input$degree_saturation_analysis_type2 == "Signal Groups"){
        if (1 %in% sg_unique$unique_sg){
          SG_number <- 1
          direction_sg <- sites_direction_onesite$direction[sites_direction_onesite$SG == SG_number]
          plot_label_sg <- paste0("Site ",Main_site," on ",Day_name," ",Date_name," SG ",SG_number," for ",direction_sg)
          name1 <- paste0("SG ",SG_number," for ",direction_sg)
          
          df_sg3 <- df_sg %>% dplyr::filter(phase == SG_number)
          xdata <- hms::as_hms(unique(df_sg3$time))
          y1 <- df_sg3$Avg_DS_by_SG 
          # y2 <- df_phase$Avg_DS_by_Phase[df_phase$Phase_name==phases_unique$unique_phases[2]]
          y11 <- df_sg3$Phase_time
          # y22 <- df_phase$Phase_time[df_phase$Phase_name==phases_unique$unique_phases[2]]
          # plot(xdata, y1, type="o", col="blue", pch="o", lty=1, ylim=c(0,2),xlab= "Time", ylab="Avg DS to Green Time" )
          df_plot <- data.frame(xdata, y1)
          g <- ggplot(df_plot, aes(xdata))
          g <- g + ggtitle(plot_label_sg)
          g <- g + geom_line(aes(y=y1, color = name1))
          # g <- g + geom_line(aes(y=y2, color = paste0("Phase ",phases_unique$unique_phases[2])))
          g <- g + theme(plot.title = element_text(color="black", size=10, face="bold.italic"),
                         axis.text=element_text(size=8),
                         axis.title=element_text(size=9,face="bold"),
                         legend.text=element_text(size=9),
                         legend.justification=c(1,1),legend.position=c(1,1),legend.title=element_blank())
          # Add a second axis and specify its features
          g <- g + geom_line( aes(y=y11),linetype = "dotted", size=1, color="red") +
            scale_y_continuous(sec.axis = sec_axis(~.*1, name="Phase Split Percentage (%)"), labels = seq(0, 140,by=10), breaks = seq(0, 140,by=10)) 
          # g <- g + geom_line( aes(y=y22),linetype = "dotted", size=1, color="darkblue") +
          #   scale_y_continuous(sec.axis = sec_axis(~.*1, name="Phase Split Percentage (%)"))
          
          g <- g + scale_color_manual(name = "Signal Group", values = c("darkblue"))
          # g <- g + scale_color_manual(name = "Phase B", values = c("darkblue"))
          g <- g + ylab("Degree of Saturation (%)") + xlab("Time")
          g <- g + scale_x_continuous(labels = hms::as_hms(seq(0, 3600*23,by=3600*3)), breaks=seq(0, 3600*23,by=3600*3))
          g <- g + theme_calc()
          g1 <- g

        } else { g1 <- ggplot() + theme_void() + geom_text(aes(0,0,label=' ')) + xlab(NULL)} #optional, but safer in case another theme is applied later}
        if (2 %in% sg_unique$unique_sg){
          SG_number <- 2
          direction_sg <- sites_direction_onesite$direction[sites_direction_onesite$SG == SG_number]
          plot_label_sg <- paste0("Site ",Main_site," on ",Day_name," ",Date_name," SG ",SG_number," for ",direction_sg)
          name2 <- paste0("SG ",SG_number," for ",direction_sg)
          
          df_sg3 <- df_sg %>% dplyr::filter(phase == SG_number)
          xdata <- hms::as_hms(unique(df_sg3$time))
          y1 <- df_sg3$Avg_DS_by_SG 
          # y2 <- df_phase$Avg_DS_by_Phase[df_phase$Phase_name==phases_unique$unique_phases[2]]
          y11 <- df_sg3$Phase_time
          # y22 <- df_phase$Phase_time[df_phase$Phase_name==phases_unique$unique_phases[2]]
          # plot(xdata, y1, type="o", col="blue", pch="o", lty=1, ylim=c(0,2),xlab= "Time", ylab="Avg DS to Green Time" )
          df_plot <- data.frame(xdata, y1)
          g <- ggplot(df_plot, aes(xdata))
          g <- g + ggtitle(plot_label_sg)
          g <- g + geom_line(aes(y=y1, color = name2))
          # g <- g + geom_line(aes(y=y2, color = paste0("Phase ",phases_unique$unique_phases[2])))
          g <- g + theme(plot.title = element_text(color="black", size=10, face="bold.italic"),
                         axis.text=element_text(size=8),
                         axis.title=element_text(size=9,face="bold"),
                         legend.text=element_text(size=9),
                         legend.justification=c(1,1),legend.position=c(1,1),legend.title=element_blank())
          # Add a second axis and specify its features
          g <- g + geom_line( aes(y=y11),linetype = "dotted", size=1, color="red") +
            scale_y_continuous(sec.axis = sec_axis(~.*1, name="Phase Split Percentage (%)"), labels = seq(0, 140,by=10), breaks = seq(0, 140,by=10)) 
          # g <- g + geom_line( aes(y=y22),linetype = "dotted", size=1, color="darkblue") +
          #   scale_y_continuous(sec.axis = sec_axis(~.*1, name="Phase Split Percentage (%)"))
          
          g <- g + scale_color_manual(name = "Signal Group", values = c("darkblue"))
          # g <- g + scale_color_manual(name = "Phase B", values = c("darkblue"))
          g <- g + ylab("Degree of Saturation (%)") + xlab("Time")
          g <- g + scale_x_continuous(labels = hms::as_hms(seq(0, 3600*23,by=3600*3)), breaks=seq(0, 3600*23,by=3600*3))
          # g <- g + scale_y_continuous(labels = seq(0, 120,by=10), breaks = seq(0, 120,by=10))
          g <- g + theme_calc()
          g2 <- g
          
        } else { g2 <- ggplot() + theme_void() + geom_text(aes(0,0,label=' ')) + xlab(NULL)}
        if (3 %in% sg_unique$unique_sg){
          SG_number <- 3
          direction_sg <- sites_direction_onesite$direction[sites_direction_onesite$SG == SG_number]
          plot_label_sg <- paste0("Site ",Main_site," on ",Day_name," ",Date_name," SG ",SG_number," for ",direction_sg)
          name3 <- paste0("SG ",SG_number," for ",direction_sg)
          
          df_sg3 <- df_sg %>% dplyr::filter(phase == SG_number)
          xdata <- hms::as_hms(unique(df_sg3$time))
          y1 <- df_sg3$Avg_DS_by_SG 
          # y2 <- df_phase$Avg_DS_by_Phase[df_phase$Phase_name==phases_unique$unique_phases[2]]
          y11 <- df_sg3$Phase_time
          # y22 <- df_phase$Phase_time[df_phase$Phase_name==phases_unique$unique_phases[2]]
          # plot(xdata, y1, type="o", col="blue", pch="o", lty=1, ylim=c(0,2),xlab= "Time", ylab="Avg DS to Green Time" )
          df_plot <- data.frame(xdata, y1)
          g <- ggplot(df_plot, aes(xdata))
          g <- g + ggtitle(plot_label_sg)
          g <- g + geom_line(aes(y=y1, color = name3))
          # g <- g + geom_line(aes(y=y2, color = paste0("Phase ",phases_unique$unique_phases[2])))
          g <- g + theme(plot.title = element_text(color="black", size=10, face="bold.italic"),
                         axis.text=element_text(size=8),
                         axis.title=element_text(size=9,face="bold"),
                         legend.text=element_text(size=9),
                         legend.justification=c(1,1),legend.position=c(1,1),legend.title=element_blank())
          # Add a second axis and specify its features
          g <- g + geom_line( aes(y=y11),linetype = "dotted", size=1, color="red") +
            scale_y_continuous(sec.axis = sec_axis(~.*1, name="Phase Split Percentage (%)"), labels = seq(0, 140,by=10), breaks = seq(0, 140,by=10)) 
          # g <- g + geom_line( aes(y=y22),linetype = "dotted", size=1, color="darkblue") +
          #   scale_y_continuous(sec.axis = sec_axis(~.*1, name="Phase Split Percentage (%)"))
          
          g <- g + scale_color_manual(name = "Signal Group", values = c("darkblue"))
          # g <- g + scale_color_manual(name = "Phase B", values = c("darkblue"))
          g <- g + ylab("Degree of Saturation (%)") + xlab("Time")
          g <- g + scale_x_continuous(labels = hms::as_hms(seq(0, 3600*23,by=3600*3)), breaks=seq(0, 3600*23,by=3600*3))
          g <- g + theme_calc()
          g3 <- g
          
        } else { g3 <- ggplot() + theme_void() + geom_text(aes(0,0,label=' ')) + xlab(NULL)}
        if (4 %in% sg_unique$unique_sg){
          SG_number <- 4
          direction_sg <- sites_direction_onesite$direction[sites_direction_onesite$SG == SG_number]
          plot_label_sg <- paste0("Site ",Main_site," on ",Day_name," ",Date_name," SG ",SG_number," for ",direction_sg)
          name4 <- paste0("SG ",SG_number," for ",direction_sg)
          
          df_sg3 <- df_sg %>% dplyr::filter(phase == SG_number)
          xdata <- hms::as_hms(unique(df_sg3$time))
          y1 <- df_sg3$Avg_DS_by_SG 
          # y2 <- df_phase$Avg_DS_by_Phase[df_phase$Phase_name==phases_unique$unique_phases[2]]
          y11 <- df_sg3$Phase_time
          # y22 <- df_phase$Phase_time[df_phase$Phase_name==phases_unique$unique_phases[2]]
          # plot(xdata, y1, type="o", col="blue", pch="o", lty=1, ylim=c(0,2),xlab= "Time", ylab="Avg DS to Green Time" )
          df_plot <- data.frame(xdata, y1)
          g <- ggplot(df_plot, aes(xdata))
          g <- g + ggtitle(plot_label_sg)
          g <- g + geom_line(aes(y=y1, color = name4))
          # g <- g + geom_line(aes(y=y2, color = paste0("Phase ",phases_unique$unique_phases[2])))
          g <- g + theme(plot.title = element_text(color="black", size=10, face="bold.italic"),
                         axis.text=element_text(size=8),
                         axis.title=element_text(size=9,face="bold"),
                         legend.text=element_text(size=9),
                         legend.justification=c(1,1),legend.position=c(1,1),legend.title=element_blank())
          # Add a second axis and specify its features
          g <- g + geom_line( aes(y=y11),linetype = "dotted", size=1, color="red") +
            scale_y_continuous(sec.axis = sec_axis(~.*1, name="Phase Split Percentage (%)"), labels = seq(0, 140,by=10), breaks = seq(0, 140,by=10)) 
          # g <- g + geom_line( aes(y=y22),linetype = "dotted", size=1, color="darkblue") +
          #   scale_y_continuous(sec.axis = sec_axis(~.*1, name="Phase Split Percentage (%)"))
          
          g <- g + scale_color_manual(name = "Signal Group", values = c("darkblue"))
          # g <- g + scale_color_manual(name = "Phase B", values = c("darkblue"))
          g <- g + ylab("Degree of Saturation (%)") + xlab("Time")
          g <- g + scale_x_continuous(labels = hms::as_hms(seq(0, 3600*23,by=3600*3)), breaks=seq(0, 3600*23,by=3600*3))
          g <- g + theme_calc()
          g4 <- g
          
        } else { g4 <- ggplot() + theme_void() + geom_text(aes(0,0,label=' ')) + xlab(NULL)}
        if (5 %in% sg_unique$unique_sg){
          SG_number <- 5
          direction_sg <- sites_direction_onesite$direction[sites_direction_onesite$SG == SG_number]
          plot_label_sg <- paste0("Site ",Main_site," on ",Day_name," ",Date_name," SG ",SG_number," for ",direction_sg)
          name5 <- paste0("SG ",SG_number," for ",direction_sg)
          
          df_sg3 <- df_sg %>% dplyr::filter(phase == SG_number)
          xdata <- hms::as_hms(unique(df_sg3$time))
          y1 <- df_sg3$Avg_DS_by_SG 
          # y2 <- df_phase$Avg_DS_by_Phase[df_phase$Phase_name==phases_unique$unique_phases[2]]
          y11 <- df_sg3$Phase_time
          # y22 <- df_phase$Phase_time[df_phase$Phase_name==phases_unique$unique_phases[2]]
          # plot(xdata, y1, type="o", col="blue", pch="o", lty=1, ylim=c(0,2),xlab= "Time", ylab="Avg DS to Green Time" )
          df_plot <- data.frame(xdata, y1)
          g <- ggplot(df_plot, aes(xdata))
          g <- g + ggtitle(plot_label_sg)
          g <- g + geom_line(aes(y=y1, color = name5))
          # g <- g + geom_line(aes(y=y2, color = paste0("Phase ",phases_unique$unique_phases[2])))
          g <- g + theme(plot.title = element_text(color="black", size=10, face="bold.italic"),
                         axis.text=element_text(size=8),
                         axis.title=element_text(size=9,face="bold"),
                         legend.text=element_text(size=9),
                         legend.justification=c(1,1),legend.position=c(1,1),legend.title=element_blank())
          # Add a second axis and specify its features
          g <- g + geom_line( aes(y=y11),linetype = "dotted", size=1, color="red") +
            scale_y_continuous(sec.axis = sec_axis(~.*1, name="Phase Split Percentage (%)"), labels = seq(0, 140,by=10), breaks = seq(0, 140,by=10)) 
          # g <- g + geom_line( aes(y=y22),linetype = "dotted", size=1, color="darkblue") +
          #   scale_y_continuous(sec.axis = sec_axis(~.*1, name="Phase Split Percentage (%)"))
          
          g <- g + scale_color_manual(name = "Signal Group", values = c("darkblue"))
          # g <- g + scale_color_manual(name = "Phase B", values = c("darkblue"))
          g <- g + ylab("Degree of Saturation (%)") + xlab("Time")
          g <- g + scale_x_continuous(labels = hms::as_hms(seq(0, 3600*23,by=3600*3)), breaks=seq(0, 3600*23,by=3600*3))
          g <- g + theme_calc()
          g5 <- g
          
        } else { g5 <- ggplot() + theme_void() + geom_text(aes(0,0,label=' ')) + xlab(NULL)}
        if (6 %in% sg_unique$unique_sg){
          SG_number <- 6
          direction_sg <- sites_direction_onesite$direction[sites_direction_onesite$SG == SG_number]
          plot_label_sg <- paste0("Site ",Main_site," on ",Day_name," ",Date_name," SG ",SG_number," for ",direction_sg)
          name6 <- paste0("SG ",SG_number," for ",direction_sg)
          
          df_sg3 <- df_sg %>% dplyr::filter(phase == SG_number)
          xdata <- hms::as_hms(unique(df_sg3$time))
          y1 <- df_sg3$Avg_DS_by_SG 
          # y2 <- df_phase$Avg_DS_by_Phase[df_phase$Phase_name==phases_unique$unique_phases[2]]
          y11 <- df_sg3$Phase_time
          # y22 <- df_phase$Phase_time[df_phase$Phase_name==phases_unique$unique_phases[2]]
          # plot(xdata, y1, type="o", col="blue", pch="o", lty=1, ylim=c(0,2),xlab= "Time", ylab="Avg DS to Green Time" )
          df_plot <- data.frame(xdata, y1)
          g <- ggplot(df_plot, aes(xdata))
          g <- g + ggtitle(plot_label_sg)
          g <- g + geom_line(aes(y=y1, color = name6))
          # g <- g + geom_line(aes(y=y2, color = paste0("Phase ",phases_unique$unique_phases[2])))
          g <- g + theme(plot.title = element_text(color="black", size=10, face="bold.italic"),
                         axis.text=element_text(size=8),
                         axis.title=element_text(size=9,face="bold"),
                         legend.text=element_text(size=9),
                         legend.justification=c(1,1),legend.position=c(1,1),legend.title=element_blank())
          # Add a second axis and specify its features
          g <- g + geom_line( aes(y=y11),linetype = "dotted", size=1, color="red") +
            scale_y_continuous(sec.axis = sec_axis(~.*1, name="Phase Split Percentage (%)"), labels = seq(0, 140,by=10), breaks = seq(0, 140,by=10)) 
          # g <- g + geom_line( aes(y=y22),linetype = "dotted", size=1, color="darkblue") +
          #   scale_y_continuous(sec.axis = sec_axis(~.*1, name="Phase Split Percentage (%)"))
          
          g <- g + scale_color_manual(name = "Signal Group", values = c("darkblue"))
          # g <- g + scale_color_manual(name = "Phase B", values = c("darkblue"))
          g <- g + ylab("Degree of Saturation (%)") + xlab("Time")
          g <- g + scale_x_continuous(labels = hms::as_hms(seq(0, 3600*23,by=3600*3)), breaks=seq(0, 3600*23,by=3600*3))
          g <- g + theme_calc()
          g6 <- g
          
        } else { g6 <- ggplot() + theme_void() + geom_text(aes(0,0,label=' ')) + xlab(NULL)}
        if (7 %in% sg_unique$unique_sg){
          SG_number <- 7
          direction_sg <- sites_direction_onesite$direction[sites_direction_onesite$SG == SG_number]
          plot_label_sg <- paste0("Site ",Main_site," on ",Day_name," ",Date_name," SG ",SG_number," for ",direction_sg)
          name7 <- paste0("SG ",SG_number," for ",direction_sg)
          
          df_sg3 <- df_sg %>% dplyr::filter(phase == SG_number)
          xdata <- hms::as_hms(unique(df_sg3$time))
          y1 <- df_sg3$Avg_DS_by_SG 
          # y2 <- df_phase$Avg_DS_by_Phase[df_phase$Phase_name==phases_unique$unique_phases[2]]
          y11 <- df_sg3$Phase_time
          # y22 <- df_phase$Phase_time[df_phase$Phase_name==phases_unique$unique_phases[2]]
          # plot(xdata, y1, type="o", col="blue", pch="o", lty=1, ylim=c(0,2),xlab= "Time", ylab="Avg DS to Green Time" )
          df_plot <- data.frame(xdata, y1)
          g <- ggplot(df_plot, aes(xdata))
          g <- g + ggtitle(plot_label_sg)
          g <- g + geom_line(aes(y=y1, color = name7))
          # g <- g + geom_line(aes(y=y2, color = paste0("Phase ",phases_unique$unique_phases[2])))
          g <- g + theme(plot.title = element_text(color="black", size=10, face="bold.italic"),
                         axis.text=element_text(size=8),
                         axis.title=element_text(size=9,face="bold"),
                         legend.text=element_text(size=9),
                         legend.justification=c(1,1),legend.position=c(1,1),legend.title=element_blank())
          # Add a second axis and specify its features
          g <- g + geom_line( aes(y=y11),linetype = "dotted", size=1, color="red") +
            scale_y_continuous(sec.axis = sec_axis(~.*1, name="Phase Split Percentage (%)"), labels = seq(0, 140,by=10), breaks = seq(0, 140,by=10)) 
          # g <- g + geom_line( aes(y=y22),linetype = "dotted", size=1, color="darkblue") +
          #   scale_y_continuous(sec.axis = sec_axis(~.*1, name="Phase Split Percentage (%)"))
          
          g <- g + scale_color_manual(name = "Signal Group", values = c("darkblue"))
          # g <- g + scale_color_manual(name = "Phase B", values = c("darkblue"))
          g <- g + ylab("Degree of Saturation (%)") + xlab("Time")
          g <- g + scale_x_continuous(labels = hms::as_hms(seq(0, 3600*23,by=3600*3)), breaks=seq(0, 3600*23,by=3600*3))
          g <- g + theme_calc()
          g7 <- g
          
        } else { g7 <- ggplot() + theme_void() + geom_text(aes(0,0,label=' ')) + xlab(NULL)}
        if (8 %in% sg_unique$unique_sg){
          SG_number <- 8
          direction_sg <- sites_direction_onesite$direction[sites_direction_onesite$SG == SG_number]
          plot_label_sg <- paste0("Site ",Main_site," on ",Day_name," ",Date_name," SG ",SG_number," for ",direction_sg)
          name8 <- paste0("SG ",SG_number," for ",direction_sg)
          
          df_sg3 <- df_sg %>% dplyr::filter(phase == SG_number)
          xdata <- hms::as_hms(unique(df_sg3$time))
          y1 <- df_sg3$Avg_DS_by_SG 
          # y2 <- df_phase$Avg_DS_by_Phase[df_phase$Phase_name==phases_unique$unique_phases[2]]
          y11 <- df_sg3$Phase_time
          # y22 <- df_phase$Phase_time[df_phase$Phase_name==phases_unique$unique_phases[2]]
          # plot(xdata, y1, type="o", col="blue", pch="o", lty=1, ylim=c(0,2),xlab= "Time", ylab="Avg DS to Green Time" )
          df_plot <- data.frame(xdata, y1)
          g <- ggplot(df_plot, aes(xdata))
          g <- g + ggtitle(plot_label_sg)
          g <- g + geom_line(aes(y=y1, color = name8))
          # g <- g + geom_line(aes(y=y2, color = paste0("Phase ",phases_unique$unique_phases[2])))
          g <- g + theme(plot.title = element_text(color="black", size=10, face="bold.italic"),
                         axis.text=element_text(size=8),
                         axis.title=element_text(size=9,face="bold"),
                         legend.text=element_text(size=9),
                         legend.justification=c(1,1),legend.position=c(1,1),legend.title=element_blank())
          # Add a second axis and specify its features
          g <- g + geom_line( aes(y=y11),linetype = "dotted", size=1, color="red") +
            scale_y_continuous(sec.axis = sec_axis(~.*1, name="Phase Split Percentage (%)"), labels = seq(0, 140,by=10), breaks = seq(0, 140,by=10)) 
          # g <- g + geom_line( aes(y=y22),linetype = "dotted", size=1, color="darkblue") +
          #   scale_y_continuous(sec.axis = sec_axis(~.*1, name="Phase Split Percentage (%)"))
          
          g <- g + scale_color_manual(name = "Signal Group", values = c("darkblue"))
          # g <- g + scale_color_manual(name = "Phase B", values = c("darkblue"))
          g <- g + ylab("Degree of Saturation (%)") + xlab("Time")
          g <- g + scale_x_continuous(labels = hms::as_hms(seq(0, 3600*23,by=3600*3)), breaks=seq(0, 3600*23,by=3600*3))
          g <- g + theme_calc()
          g8 <- g
          
        } else { g8 <- ggplot() + theme_void() + geom_text(aes(0,0,label=' ')) + xlab(NULL)}
        if (9 %in% sg_unique$unique_sg){
          SG_number <- 9
          direction_sg <- sites_direction_onesite$direction[sites_direction_onesite$SG == SG_number]
          plot_label_sg <- paste0("Site ",Main_site," on ",Day_name," ",Date_name," SG ",SG_number," for ",direction_sg)
          name9 <- paste0("SG ",SG_number," for ",direction_sg)
          
          df_sg3 <- df_sg %>% dplyr::filter(phase == SG_number)
          xdata <- hms::as_hms(unique(df_sg3$time))
          y1 <- df_sg3$Avg_DS_by_SG 
          # y2 <- df_phase$Avg_DS_by_Phase[df_phase$Phase_name==phases_unique$unique_phases[2]]
          y11 <- df_sg3$Phase_time
          # y22 <- df_phase$Phase_time[df_phase$Phase_name==phases_unique$unique_phases[2]]
          # plot(xdata, y1, type="o", col="blue", pch="o", lty=1, ylim=c(0,2),xlab= "Time", ylab="Avg DS to Green Time" )
          df_plot <- data.frame(xdata, y1)
          g <- ggplot(df_plot, aes(xdata))
          g <- g + ggtitle(plot_label_sg)
          g <- g + geom_line(aes(y=y1, color = name9))
          # g <- g + geom_line(aes(y=y2, color = paste0("Phase ",phases_unique$unique_phases[2])))
          g <- g + theme(plot.title = element_text(color="black", size=10, face="bold.italic"),
                         axis.text=element_text(size=8),
                         axis.title=element_text(size=9,face="bold"),
                         legend.text=element_text(size=9),
                         legend.justification=c(1,1),legend.position=c(1,1),legend.title=element_blank())
          # Add a second axis and specify its features
          g <- g + geom_line( aes(y=y11),linetype = "dotted", size=1, color="red") +
            scale_y_continuous(sec.axis = sec_axis(~.*1, name="Phase Split Percentage (%)"), labels = seq(0, 140,by=10), breaks = seq(0, 140,by=10)) 
          # g <- g + geom_line( aes(y=y22),linetype = "dotted", size=1, color="darkblue") +
          #   scale_y_continuous(sec.axis = sec_axis(~.*1, name="Phase Split Percentage (%)"))
          
          g <- g + scale_color_manual(name = "Signal Group", values = c("darkblue"))
          # g <- g + scale_color_manual(name = "Phase B", values = c("darkblue"))
          g <- g + ylab("Degree of Saturation (%)") + xlab("Time")
          g <- g + scale_x_continuous(labels = hms::as_hms(seq(0, 3600*23,by=3600*3)), breaks=seq(0, 3600*23,by=3600*3))
          g <- g + theme_calc()
          g9 <- g
          
        } else { g9 <- ggplot() + theme_void() + geom_text(aes(0,0,label=' ')) + xlab(NULL)}
        output$Plot5 <- renderPlot({
          p1 <- g1  
          p2 <- g2
          p3 <- g3
          p4 <- g4
          p5 <- g5
          p6 <- g6
          p7 <- g7
          p8 <- g8
          p9 <- g9
          grid.arrange(grobs = list(p1,p2,p3,p4,p5,p6,p7,p8,p9), ncol=1, top = paste0("Site ",Main_site," on ",Day_name," ",Date_name), nrow = 9, byrow = TRUE)})
      } else if(input$degree_saturation_analysis_type2 == "Phases"){
        plot_label <- paste0("Site ",Main_site," on ",Day_name," ",Date_name)
        if(nrow(phases_unique)==2){
          xdata <- hms::as_hms(unique(df_phase$time))
          y1 <- df_phase$Avg_DS_by_Phase[df_phase$Phase_name==phases_unique$unique_phases[1]]
          y2 <- df_phase$Avg_DS_by_Phase[df_phase$Phase_name==phases_unique$unique_phases[2]]
          y11 <- df_phase$Phase_time[df_phase$Phase_name==phases_unique$unique_phases[1]]
          y22 <- df_phase$Phase_time[df_phase$Phase_name==phases_unique$unique_phases[2]]
          # plot(xdata, y1, type="o", col="blue", pch="o", lty=1, ylim=c(0,2),xlab= "Time", ylab="Avg DS to Green Time" )
          df_plot <- data.frame(xdata, y1, y2)
          g <- ggplot(df_plot, aes(xdata))
          g <- g + ggtitle(plot_label)
          g <- g + geom_line(aes(y=y1, color = paste0("Phase ",phases_unique$unique_phases[1])))
          g <- g + geom_line(aes(y=y2, color = paste0("Phase ",phases_unique$unique_phases[2])))
          g <- g + theme(plot.title = element_text(color="black", size=10, face="bold.italic"),
                         axis.text=element_text(size=8),
                         axis.title=element_text(size=9,face="bold"),
                         legend.text=element_text(size=9),
                         legend.justification=c(1,1),legend.position=c(0.24,1),legend.title=element_blank())
          # Add a second axis and specify its features
          g <- g + geom_line( aes(y=y11),linetype = "dotted", size=1, color="red") +
            scale_y_continuous(sec.axis = sec_axis(~.*1, name="Phase Split Percentage (%)"),labels = seq(0, 140,by=10), breaks = seq(0, 140,by=10)) 
          # g <- g + geom_line( aes(y=y22),linetype = "dotted", size=1, color="darkblue") +
          #   scale_y_continuous(sec.axis = sec_axis(~.*1, name="Phase Split Percentage (%)"),labels = seq(0, 140,by=10), breaks = seq(0, 140,by=10))
          
          g <- g + scale_color_manual(name = "Phases", values = c("red","darkblue"))
          # g <- g + scale_color_manual(name = "Phase B", values = c("darkblue"))
          g <- g + ylab("Avg DS to Phase Time") + xlab("Time")
          g <- g + scale_x_continuous(labels = hms::as_hms(seq(0, 3600*23,by=3600*3)), breaks=seq(0, 3600*23,by=3600*3))
          g <- g + theme_calc()
          g
          output$Plot5_2 <- renderPlot({ g })
        }
        if(nrow(phases_unique)==3){
          xdata <- hms::as_hms(unique(df_phase$time))
          y1 <- df_phase$Avg_DS_by_Phase[df_phase$Phase_name==phases_unique$unique_phases[1]]
          y2 <- df_phase$Avg_DS_by_Phase[df_phase$Phase_name==phases_unique$unique_phases[2]]
          y3 <- df_phase$Avg_DS_by_Phase[df_phase$Phase_name==phases_unique$unique_phases[3]]
          y11 <- df_phase$Phase_time[df_phase$Phase_name==phases_unique$unique_phases[1]]
          y22 <- df_phase$Phase_time[df_phase$Phase_name==phases_unique$unique_phases[2]]
          y33 <- df_phase$Phase_time[df_phase$Phase_name==phases_unique$unique_phases[3]]
          # plot(xdata, y1, type="o", col="blue", pch="o", lty=1, ylim=c(0,2),xlab= "Time", ylab="Avg DS to Green Time" )
          df_plot <- data.frame(xdata, y1, y2, y3)
          g <- ggplot(df_plot, aes(xdata))
          g <- g + ggtitle(plot_label)
          g <- g + geom_line(aes(y=y1, color = paste0("Phase ",phases_unique$unique_phases[1])))
          g <- g + geom_line(aes(y=y2, color = paste0("Phase ",phases_unique$unique_phases[2])))
          g <- g + geom_line(aes(y=y3, color = paste0("Phase ",phases_unique$unique_phases[3])))
          g <- g + theme(plot.title = element_text(color="black", size=10, face="bold.italic"),
                         axis.text=element_text(size=8),
                         axis.title=element_text(size=9,face="bold"),
                         legend.text=element_text(size=9),
                         legend.justification=c(1,1),legend.position=c(0.24,1),legend.title=element_blank())
          # Add a second axis and specify its features
          g <- g + geom_line( aes(y=y11),linetype = "dotted", size=1, color="red") +
            scale_y_continuous(sec.axis = sec_axis(~.*1, name="Phase Split Percentage (%)"),labels = seq(0, 140,by=10), breaks = seq(0, 140,by=10)) 
          # g <- g + geom_line( aes(y=y22),linetype = "dotted", size=1, color="darkblue") +
          #   scale_y_continuous(sec.axis = sec_axis(~.*1, name="Phase Split Percentage (%)"),labels = seq(0, 140,by=10), breaks = seq(0, 140,by=10)) 
          # g <- g + geom_line( aes(y=y33),linetype = "dotted", size=1, color="darkgreen") +
          #   scale_y_continuous(sec.axis = sec_axis(~.*1, name="Phase Split Percentage (%)"),labels = seq(0, 140,by=10), breaks = seq(0, 140,by=10)) 
          # g <- g + scale_color_manual(name = "Phases", values = c("red","darkblue","darkgreen"))
          
          g <- g + ylab("Average Degree of Saturation %") + xlab("Time")
          g <- g + scale_x_continuous(labels = hms::as_hms(seq(0, 3600*23,by=3600*3)), breaks=seq(0, 3600*23,by=3600*3))
          g <- g + theme_calc()
          g
          output$Plot5_2 <- renderPlot({ g})
        }
        if(nrow(phases_unique)==4){
          xdata <- hms::as_hms(unique(df_phase$time))
          y1 <- df_phase$Avg_DS_by_Phase[df_phase$Phase_name==phases_unique$unique_phases[1]]
          y2 <- df_phase$Avg_DS_by_Phase[df_phase$Phase_name==phases_unique$unique_phases[2]]
          y3 <- df_phase$Avg_DS_by_Phase[df_phase$Phase_name==phases_unique$unique_phases[3]]
          y4 <- df_phase$Avg_DS_by_Phase[df_phase$Phase_name==phases_unique$unique_phases[4]]
          y11 <- df_phase$Phase_time[df_phase$Phase_name==phases_unique$unique_phases[1]]
          y22 <- df_phase$Phase_time[df_phase$Phase_name==phases_unique$unique_phases[2]]
          y33 <- df_phase$Phase_time[df_phase$Phase_name==phases_unique$unique_phases[3]]
          y44 <- df_phase$Phase_time[df_phase$Phase_name==phases_unique$unique_phases[4]]
          # plot(xdata, y1, type="o", col="blue", pch="o", lty=1, ylim=c(0,2),xlab= "Time", ylab="Avg DS to Green Time" )
          df_plot <- data.frame(xdata, y1, y2, y3, y4)
          g <- ggplot(df_plot, aes(xdata))
          g <- g + ggtitle(plot_label)
          g <- g + geom_line(aes(y=y1, color = paste0("Phase ",phases_unique$unique_phases[1])))
          g <- g + geom_line(aes(y=y2, color = paste0("Phase ",phases_unique$unique_phases[2])))
          g <- g + geom_line(aes(y=y3, color = paste0("Phase ",phases_unique$unique_phases[3])))
          g <- g + geom_line(aes(y=y4, color = paste0("Phase ",phases_unique$unique_phases[4])))
          g <- g + theme(plot.title = element_text(color="black", size=10, face="bold.italic"),
                         axis.text=element_text(size=8),
                         axis.title=element_text(size=9,face="bold"),
                         legend.text=element_text(size=9),
                         legend.justification=c(1,1),legend.position=c(0.24,1),legend.title=element_blank())
          # Add a second axis and specify its features
          g <- g + geom_line( aes(y=y11),linetype = "dotted", size=1, color="red") +
            scale_y_continuous(sec.axis = sec_axis(~.*1, name="Phase Split Percentage (%)"),labels = seq(0, 140,by=10), breaks = seq(0, 140,by=10)) 
          # g <- g + geom_line( aes(y=y22),linetype = "dotted", size=1, color="darkblue") +
          #   scale_y_continuous(sec.axis = sec_axis(~.*1, name="Phase Split Percentage (%)"),labels = seq(0, 140,by=10), breaks = seq(0, 140,by=10)) 
          # g <- g + geom_line( aes(y=y33),linetype = "dotted", size=1, color="darkgreen") +
          #   scale_y_continuous(sec.axis = sec_axis(~.*1, name="Phase Split Percentage (%)"),labels = seq(0, 140,by=10), breaks = seq(0, 140,by=10))
          # g <- g + geom_line( aes(y=y44),linetype = "dotted", size=1, color="orange") +
          #   scale_y_continuous(sec.axis = sec_axis(~.*1, name="Phase Split Percentage (%)"),labels = seq(0, 140,by=10), breaks = seq(0, 140,by=10))
          
          g <- g + scale_color_manual(name = "Phases", values = c("red","darkblue","darkgreen","orange"))
          # g <- g + scale_color_manual(name = "Phase B", values = c("darkblue"))
          g <- g + ylab("Avg DS to Phase Time") + xlab("Time")
          g <- g + scale_x_continuous(labels = hms::as_hms(seq(0, 3600*23,by=3600*3)), breaks=seq(0, 3600*23,by=3600*3))
          g <- g + theme_calc()
          g
          output$Plot5_2 <- renderPlot({ g})
        }
        if(nrow(phases_unique)==5){
          xdata <- hms::as_hms(unique(df_phase$time))
          y1 <- df_phase$Avg_DS_by_Phase[df_phase$Phase_name==phases_unique$unique_phases[1]]
          y2 <- df_phase$Avg_DS_by_Phase[df_phase$Phase_name==phases_unique$unique_phases[2]]
          y3 <- df_phase$Avg_DS_by_Phase[df_phase$Phase_name==phases_unique$unique_phases[3]]
          y4 <- df_phase$Avg_DS_by_Phase[df_phase$Phase_name==phases_unique$unique_phases[4]]
          y5 <- df_phase$Avg_DS_by_Phase[df_phase$Phase_name==phases_unique$unique_phases[5]]
          y11 <- df_phase$Phase_time[df_phase$Phase_name==phases_unique$unique_phases[1]]
          y22 <- df_phase$Phase_time[df_phase$Phase_name==phases_unique$unique_phases[2]]
          y33 <- df_phase$Phase_time[df_phase$Phase_name==phases_unique$unique_phases[3]]
          y44 <- df_phase$Phase_time[df_phase$Phase_name==phases_unique$unique_phases[4]]
          y55 <- df_phase$Phase_time[df_phase$Phase_name==phases_unique$unique_phases[5]]
          # plot(xdata, y1, type="o", col="blue", pch="o", lty=1, ylim=c(0,2),xlab= "Time", ylab="Avg DS to Green Time" )
          df_plot <- data.frame(xdata, y1, y2, y3, y4, y5)
          g <- ggplot(df_plot, aes(xdata))
          g <- g + ggtitle(plot_label)
          g <- g + geom_line(aes(y=y1, color = paste0("Phase ",phases_unique$unique_phases[1])))
          g <- g + geom_line(aes(y=y2, color = paste0("Phase ",phases_unique$unique_phases[2])))
          g <- g + geom_line(aes(y=y3, color = paste0("Phase ",phases_unique$unique_phases[3])))
          g <- g + geom_line(aes(y=y4, color = paste0("Phase ",phases_unique$unique_phases[4])))
          g <- g + geom_line(aes(y=y5, color = paste0("Phase ",phases_unique$unique_phases[5])))
          g <- g + theme(plot.title = element_text(color="black", size=10, face="bold.italic"),
                         axis.text=element_text(size=8),
                         axis.title=element_text(size=9,face="bold"),
                         legend.text=element_text(size=9),
                         legend.justification=c(1,1),legend.position=c(0.24,1),legend.title=element_blank())
          # Add a second axis and specify its features
          g <- g + geom_line( aes(y=y11),linetype = "dotted", size=1, color="red") +
            scale_y_continuous(sec.axis = sec_axis(~.*1, name="Phase Split Percentage (%)"),labels = seq(0, 140,by=10), breaks = seq(0, 140,by=10)) 
          # g <- g + geom_line( aes(y=y22),linetype = "dotted", size=1, color="darkblue") +
          #   scale_y_continuous(sec.axis = sec_axis(~.*1, name="Phase Split Percentage (%)"),labels = seq(0, 140,by=10), breaks = seq(0, 140,by=10)) 
          # g <- g + geom_line( aes(y=y33),linetype = "dotted", size=1, color="darkgreen") +
          #   scale_y_continuous(sec.axis = sec_axis(~.*1, name="Phase Split Percentage (%)"),labels = seq(0, 140,by=10), breaks = seq(0, 140,by=10))
          # g <- g + geom_line( aes(y=y44),linetype = "dotted", size=1, color="orange") +
          #   scale_y_continuous(sec.axis = sec_axis(~.*1, name="Phase Split Percentage (%)"),labels = seq(0, 140,by=10), breaks = seq(0, 140,by=10))
          # g <- g + geom_line( aes(y=y55),linetype = "dotted", size=1, color="cyan") +
          #   scale_y_continuous(sec.axis = sec_axis(~.*1, name="Phase Split Percentage (%)"),labels = seq(0, 140,by=10), breaks = seq(0, 140,by=10))
          
          g <- g + scale_color_manual(name = "Phases", values = c("red","darkblue","darkgreen","orange","cyan"))
          # g <- g + scale_color_manual(name = "Phase B", values = c("darkblue"))
          g <- g + ylab("Avg DS to Phase Time") + xlab("Time")
          g <- g + scale_x_continuous(labels = hms::as_hms(seq(0, 3600*23,by=3600*3)), breaks=seq(0, 3600*23,by=3600*3))
          g <- g + theme_calc()
          g
          output$Plot5_2 <- renderPlot({ g})
        }
      }
      
 
    }
    else if (input$degree_saturation_analysis_type == "Ratio"){
      if(input$degree_saturation_analysis_type2 == "Signal Groups"){
        if (1 %in% sg_unique$unique_sg){
          SG_number <- 1
          direction_sg <- sites_direction_onesite$direction[sites_direction_onesite$SG == SG_number]
          plot_label_sg <- paste0("Site ",Main_site," on ",Day_name," ",Date_name," SG ",SG_number," for ",direction_sg)
          name2 <- paste0("SG ",SG_number," for ",direction_sg)
          
          df_sg3 <- df_sg %>% dplyr::filter(phase == SG_number)
          xdata <- hms::as_hms(unique(df_sg3$time))
          y1 <- df_sg3$DS_Green_Ratio
          # y2 <- df_phase$Avg_DS_by_Phase[df_phase$Phase_name==phases_unique$unique_phases[2]]
          # y11 <- df_sg3$Phase_time
          # y22 <- df_phase$Phase_time[df_phase$Phase_name==phases_unique$unique_phases[2]]
          # plot(xdata, y1, type="o", col="blue", pch="o", lty=1, ylim=c(0,2),xlab= "Time", ylab="Avg DS to Green Time" )
          df_plot <- data.frame(xdata, y1)
          g <- ggplot(df_plot, aes(xdata))
          g <- g + ggtitle(plot_label_sg)
          g <- g + geom_line(aes(y=y1, color = name2))
          # g <- g + geom_line(aes(y=y2, color = paste0("Phase ",phases_unique$unique_phases[2])))
          g <- g + theme(plot.title = element_text(color="black", size=10, face="bold.italic"),
                         axis.text=element_text(size=8),
                         axis.title=element_text(size=9,face="bold"),
                         legend.text=element_text(size=9),
                         legend.justification=c(1,1),legend.position=c(1,1),legend.title=element_blank())
          # Add a second axis and specify its features
          # g <- g + geom_line( aes(y=y11),linetype = "dotted", size=1, color="red") +
          #   scale_y_continuous(sec.axis = sec_axis(~.*1, name="Phase Split Percentage (%)"), labels = seq(0, 140,by=10), breaks = seq(0, 140,by=10)) 
          # g <- g + geom_line( aes(y=y22),linetype = "dotted", size=1, color="darkblue") +
          #   scale_y_continuous(sec.axis = sec_axis(~.*1, name="Phase Split Percentage (%)"))
          
          g <- g + scale_color_manual(name = "Signal Group", values = c("darkblue"))
          # g <- g + scale_color_manual(name = "Phase B", values = c("darkblue"))
          g <- g + ylab("Degree of Saturation (%)") + xlab("Time")
          g <- g + scale_x_continuous(labels = hms::as_hms(seq(0, 3600*23,by=3600*3)), breaks=seq(0, 3600*23,by=3600*3))
          # g <- g + scale_y_continuous(labels = seq(0, 120,by=10), breaks = seq(0, 120,by=10))
          g <- g + theme_calc()
          g1 <- g
          
        } else { g1 <- ggplot() + theme_void() + geom_text(aes(0,0,label=' ')) + xlab(NULL)}
        if (2 %in% sg_unique$unique_sg){
          SG_number <- 2
          direction_sg <- sites_direction_onesite$direction[sites_direction_onesite$SG == SG_number]
          plot_label_sg <- paste0("Site ",Main_site," on ",Day_name," ",Date_name," SG ",SG_number," for ",direction_sg)
          name2 <- paste0("SG ",SG_number," for ",direction_sg)
          
          df_sg3 <- df_sg %>% dplyr::filter(phase == SG_number)
          xdata <- hms::as_hms(unique(df_sg3$time))
          y1 <- df_sg3$DS_Green_Ratio
          # y2 <- df_phase$Avg_DS_by_Phase[df_phase$Phase_name==phases_unique$unique_phases[2]]
          # y11 <- df_sg3$Phase_time
          # y22 <- df_phase$Phase_time[df_phase$Phase_name==phases_unique$unique_phases[2]]
          # plot(xdata, y1, type="o", col="blue", pch="o", lty=1, ylim=c(0,2),xlab= "Time", ylab="Avg DS to Green Time" )
          df_plot <- data.frame(xdata, y1)
          g <- ggplot(df_plot, aes(xdata))
          g <- g + ggtitle(plot_label_sg)
          g <- g + geom_line(aes(y=y1, color = name2))
          # g <- g + geom_line(aes(y=y2, color = paste0("Phase ",phases_unique$unique_phases[2])))
          g <- g + theme(plot.title = element_text(color="black", size=10, face="bold.italic"),
                         axis.text=element_text(size=8),
                         axis.title=element_text(size=9,face="bold"),
                         legend.text=element_text(size=9),
                         legend.justification=c(1,1),legend.position=c(1,1),legend.title=element_blank())
          # Add a second axis and specify its features
          # g <- g + geom_line( aes(y=y11),linetype = "dotted", size=1, color="red") +
          #   scale_y_continuous(sec.axis = sec_axis(~.*1, name="Phase Split Percentage (%)"), labels = seq(0, 140,by=10), breaks = seq(0, 140,by=10)) 
          # g <- g + geom_line( aes(y=y22),linetype = "dotted", size=1, color="darkblue") +
          #   scale_y_continuous(sec.axis = sec_axis(~.*1, name="Phase Split Percentage (%)"))
          
          g <- g + scale_color_manual(name = "Signal Group", values = c("darkblue"))
          # g <- g + scale_color_manual(name = "Phase B", values = c("darkblue"))
          g <- g + ylab("Degree of Saturation (%)") + xlab("Time")
          g <- g + scale_x_continuous(labels = hms::as_hms(seq(0, 3600*23,by=3600*3)), breaks=seq(0, 3600*23,by=3600*3))
          # g <- g + scale_y_continuous(labels = seq(0, 120,by=10), breaks = seq(0, 120,by=10))
          g <- g + theme_calc()
          g2 <- g
          
        } else { g2 <- ggplot() + theme_void() + geom_text(aes(0,0,label=' ')) + xlab(NULL)}
        if (3 %in% sg_unique$unique_sg){
          SG_number <- 3
          direction_sg <- sites_direction_onesite$direction[sites_direction_onesite$SG == SG_number]
          plot_label_sg <- paste0("Site ",Main_site," on ",Day_name," ",Date_name," SG ",SG_number," for ",direction_sg)
          name2 <- paste0("SG ",SG_number," for ",direction_sg)
          
          df_sg3 <- df_sg %>% dplyr::filter(phase == SG_number)
          xdata <- hms::as_hms(unique(df_sg3$time))
          y1 <- df_sg3$DS_Green_Ratio
          # y2 <- df_phase$Avg_DS_by_Phase[df_phase$Phase_name==phases_unique$unique_phases[2]]
          # y11 <- df_sg3$Phase_time
          # y22 <- df_phase$Phase_time[df_phase$Phase_name==phases_unique$unique_phases[2]]
          # plot(xdata, y1, type="o", col="blue", pch="o", lty=1, ylim=c(0,2),xlab= "Time", ylab="Avg DS to Green Time" )
          df_plot <- data.frame(xdata, y1)
          g <- ggplot(df_plot, aes(xdata))
          g <- g + ggtitle(plot_label_sg)
          g <- g + geom_line(aes(y=y1, color = name2))
          # g <- g + geom_line(aes(y=y2, color = paste0("Phase ",phases_unique$unique_phases[2])))
          g <- g + theme(plot.title = element_text(color="black", size=10, face="bold.italic"),
                         axis.text=element_text(size=8),
                         axis.title=element_text(size=9,face="bold"),
                         legend.text=element_text(size=9),
                         legend.justification=c(1,1),legend.position=c(1,1),legend.title=element_blank())
          # Add a second axis and specify its features
          # g <- g + geom_line( aes(y=y11),linetype = "dotted", size=1, color="red") +
          #   scale_y_continuous(sec.axis = sec_axis(~.*1, name="Phase Split Percentage (%)"), labels = seq(0, 140,by=10), breaks = seq(0, 140,by=10)) 
          # g <- g + geom_line( aes(y=y22),linetype = "dotted", size=1, color="darkblue") +
          #   scale_y_continuous(sec.axis = sec_axis(~.*1, name="Phase Split Percentage (%)"))
          
          g <- g + scale_color_manual(name = "Signal Group", values = c("darkblue"))
          # g <- g + scale_color_manual(name = "Phase B", values = c("darkblue"))
          g <- g + ylab("Degree of Saturation (%)") + xlab("Time")
          g <- g + scale_x_continuous(labels = hms::as_hms(seq(0, 3600*23,by=3600*3)), breaks=seq(0, 3600*23,by=3600*3))
          # g <- g + scale_y_continuous(labels = seq(0, 120,by=10), breaks = seq(0, 120,by=10))
          g <- g + theme_calc()
          g3 <- g
          
        } else { g3 <- ggplot() + theme_void() + geom_text(aes(0,0,label=' ')) + xlab(NULL)}
        if (4 %in% sg_unique$unique_sg){
          SG_number <- 4
          direction_sg <- sites_direction_onesite$direction[sites_direction_onesite$SG == SG_number]
          plot_label_sg <- paste0("Site ",Main_site," on ",Day_name," ",Date_name," SG ",SG_number," for ",direction_sg)
          name2 <- paste0("SG ",SG_number," for ",direction_sg)
          
          df_sg3 <- df_sg %>% dplyr::filter(phase == SG_number)
          xdata <- hms::as_hms(unique(df_sg3$time))
          y1 <- df_sg3$DS_Green_Ratio
          # y2 <- df_phase$Avg_DS_by_Phase[df_phase$Phase_name==phases_unique$unique_phases[2]]
          # y11 <- df_sg3$Phase_time
          # y22 <- df_phase$Phase_time[df_phase$Phase_name==phases_unique$unique_phases[2]]
          # plot(xdata, y1, type="o", col="blue", pch="o", lty=1, ylim=c(0,2),xlab= "Time", ylab="Avg DS to Green Time" )
          df_plot <- data.frame(xdata, y1)
          g <- ggplot(df_plot, aes(xdata))
          g <- g + ggtitle(plot_label_sg)
          g <- g + geom_line(aes(y=y1, color = name2))
          # g <- g + geom_line(aes(y=y2, color = paste0("Phase ",phases_unique$unique_phases[2])))
          g <- g + theme(plot.title = element_text(color="black", size=10, face="bold.italic"),
                         axis.text=element_text(size=8),
                         axis.title=element_text(size=9,face="bold"),
                         legend.text=element_text(size=9),
                         legend.justification=c(1,1),legend.position=c(1,1),legend.title=element_blank())
          # Add a second axis and specify its features
          # g <- g + geom_line( aes(y=y11),linetype = "dotted", size=1, color="red") +
          #   scale_y_continuous(sec.axis = sec_axis(~.*1, name="Phase Split Percentage (%)"), labels = seq(0, 140,by=10), breaks = seq(0, 140,by=10)) 
          # g <- g + geom_line( aes(y=y22),linetype = "dotted", size=1, color="darkblue") +
          #   scale_y_continuous(sec.axis = sec_axis(~.*1, name="Phase Split Percentage (%)"))
          
          g <- g + scale_color_manual(name = "Signal Group", values = c("darkblue"))
          # g <- g + scale_color_manual(name = "Phase B", values = c("darkblue"))
          g <- g + ylab("Degree of Saturation (%)") + xlab("Time")
          g <- g + scale_x_continuous(labels = hms::as_hms(seq(0, 3600*23,by=3600*3)), breaks=seq(0, 3600*23,by=3600*3))
          # g <- g + scale_y_continuous(labels = seq(0, 120,by=10), breaks = seq(0, 120,by=10))
          g <- g + theme_calc()
          g4 <- g
          
        } else { g4 <- ggplot() + theme_void() + geom_text(aes(0,0,label=' ')) + xlab(NULL)}
        if (5 %in% sg_unique$unique_sg){
          SG_number <- 5
          direction_sg <- sites_direction_onesite$direction[sites_direction_onesite$SG == SG_number]
          plot_label_sg <- paste0("Site ",Main_site," on ",Day_name," ",Date_name," SG ",SG_number," for ",direction_sg)
          name2 <- paste0("SG ",SG_number," for ",direction_sg)
          
          df_sg3 <- df_sg %>% dplyr::filter(phase == SG_number)
          xdata <- hms::as_hms(unique(df_sg3$time))
          y1 <- df_sg3$DS_Green_Ratio
          # y2 <- df_phase$Avg_DS_by_Phase[df_phase$Phase_name==phases_unique$unique_phases[2]]
          # y11 <- df_sg3$Phase_time
          # y22 <- df_phase$Phase_time[df_phase$Phase_name==phases_unique$unique_phases[2]]
          # plot(xdata, y1, type="o", col="blue", pch="o", lty=1, ylim=c(0,2),xlab= "Time", ylab="Avg DS to Green Time" )
          df_plot <- data.frame(xdata, y1)
          g <- ggplot(df_plot, aes(xdata))
          g <- g + ggtitle(plot_label_sg)
          g <- g + geom_line(aes(y=y1, color = name2))
          # g <- g + geom_line(aes(y=y2, color = paste0("Phase ",phases_unique$unique_phases[2])))
          g <- g + theme(plot.title = element_text(color="black", size=10, face="bold.italic"),
                         axis.text=element_text(size=8),
                         axis.title=element_text(size=9,face="bold"),
                         legend.text=element_text(size=9),
                         legend.justification=c(1,1),legend.position=c(1,1),legend.title=element_blank())
          # Add a second axis and specify its features
          # g <- g + geom_line( aes(y=y11),linetype = "dotted", size=1, color="red") +
          #   scale_y_continuous(sec.axis = sec_axis(~.*1, name="Phase Split Percentage (%)"), labels = seq(0, 140,by=10), breaks = seq(0, 140,by=10)) 
          # g <- g + geom_line( aes(y=y22),linetype = "dotted", size=1, color="darkblue") +
          #   scale_y_continuous(sec.axis = sec_axis(~.*1, name="Phase Split Percentage (%)"))
          
          g <- g + scale_color_manual(name = "Signal Group", values = c("darkblue"))
          # g <- g + scale_color_manual(name = "Phase B", values = c("darkblue"))
          g <- g + ylab("Degree of Saturation (%)") + xlab("Time")
          g <- g + scale_x_continuous(labels = hms::as_hms(seq(0, 3600*23,by=3600*3)), breaks=seq(0, 3600*23,by=3600*3))
          # g <- g + scale_y_continuous(labels = seq(0, 120,by=10), breaks = seq(0, 120,by=10))
          g <- g + theme_calc()
          g5 <- g
          
        } else { g5 <- ggplot() + theme_void() + geom_text(aes(0,0,label=' ')) + xlab(NULL)} 
        if (6 %in% sg_unique$unique_sg){
          SG_number <- 6
          direction_sg <- sites_direction_onesite$direction[sites_direction_onesite$SG == SG_number]
          plot_label_sg <- paste0("Site ",Main_site," on ",Day_name," ",Date_name," SG ",SG_number," for ",direction_sg)
          name2 <- paste0("SG ",SG_number," for ",direction_sg)
          
          df_sg3 <- df_sg %>% dplyr::filter(phase == SG_number)
          xdata <- hms::as_hms(unique(df_sg3$time))
          y1 <- df_sg3$DS_Green_Ratio
          # y2 <- df_phase$Avg_DS_by_Phase[df_phase$Phase_name==phases_unique$unique_phases[2]]
          # y11 <- df_sg3$Phase_time
          # y22 <- df_phase$Phase_time[df_phase$Phase_name==phases_unique$unique_phases[2]]
          # plot(xdata, y1, type="o", col="blue", pch="o", lty=1, ylim=c(0,2),xlab= "Time", ylab="Avg DS to Green Time" )
          df_plot <- data.frame(xdata, y1)
          g <- ggplot(df_plot, aes(xdata))
          g <- g + ggtitle(plot_label_sg)
          g <- g + geom_line(aes(y=y1, color = name2))
          # g <- g + geom_line(aes(y=y2, color = paste0("Phase ",phases_unique$unique_phases[2])))
          g <- g + theme(plot.title = element_text(color="black", size=10, face="bold.italic"),
                         axis.text=element_text(size=8),
                         axis.title=element_text(size=9,face="bold"),
                         legend.text=element_text(size=9),
                         legend.justification=c(1,1),legend.position=c(1,1),legend.title=element_blank())
          # Add a second axis and specify its features
          # g <- g + geom_line( aes(y=y11),linetype = "dotted", size=1, color="red") +
          #   scale_y_continuous(sec.axis = sec_axis(~.*1, name="Phase Split Percentage (%)"), labels = seq(0, 140,by=10), breaks = seq(0, 140,by=10)) 
          # g <- g + geom_line( aes(y=y22),linetype = "dotted", size=1, color="darkblue") +
          #   scale_y_continuous(sec.axis = sec_axis(~.*1, name="Phase Split Percentage (%)"))
          
          g <- g + scale_color_manual(name = "Signal Group", values = c("darkblue"))
          # g <- g + scale_color_manual(name = "Phase B", values = c("darkblue"))
          g <- g + ylab("Degree of Saturation (%)") + xlab("Time")
          g <- g + scale_x_continuous(labels = hms::as_hms(seq(0, 3600*23,by=3600*3)), breaks=seq(0, 3600*23,by=3600*3))
          # g <- g + scale_y_continuous(labels = seq(0, 120,by=10), breaks = seq(0, 120,by=10))
          g <- g + theme_calc()
          g6 <- g
          
        } else { g6 <- ggplot() + theme_void() + geom_text(aes(0,0,label=' ')) + xlab(NULL)}
        if (7 %in% sg_unique$unique_sg){
          SG_number <- 7
          direction_sg <- sites_direction_onesite$direction[sites_direction_onesite$SG == SG_number]
          plot_label_sg <- paste0("Site ",Main_site," on ",Day_name," ",Date_name," SG ",SG_number," for ",direction_sg)
          name2 <- paste0("SG ",SG_number," for ",direction_sg)
          
          df_sg3 <- df_sg %>% dplyr::filter(phase == SG_number)
          xdata <- hms::as_hms(unique(df_sg3$time))
          y1 <- df_sg3$DS_Green_Ratio
          # y2 <- df_phase$Avg_DS_by_Phase[df_phase$Phase_name==phases_unique$unique_phases[2]]
          # y11 <- df_sg3$Phase_time
          # y22 <- df_phase$Phase_time[df_phase$Phase_name==phases_unique$unique_phases[2]]
          # plot(xdata, y1, type="o", col="blue", pch="o", lty=1, ylim=c(0,2),xlab= "Time", ylab="Avg DS to Green Time" )
          df_plot <- data.frame(xdata, y1)
          g <- ggplot(df_plot, aes(xdata))
          g <- g + ggtitle(plot_label_sg)
          g <- g + geom_line(aes(y=y1, color = name2))
          # g <- g + geom_line(aes(y=y2, color = paste0("Phase ",phases_unique$unique_phases[2])))
          g <- g + theme(plot.title = element_text(color="black", size=10, face="bold.italic"),
                         axis.text=element_text(size=8),
                         axis.title=element_text(size=9,face="bold"),
                         legend.text=element_text(size=9),
                         legend.justification=c(1,1),legend.position=c(1,1),legend.title=element_blank())
          # Add a second axis and specify its features
          # g <- g + geom_line( aes(y=y11),linetype = "dotted", size=1, color="red") +
          #   scale_y_continuous(sec.axis = sec_axis(~.*1, name="Phase Split Percentage (%)"), labels = seq(0, 140,by=10), breaks = seq(0, 140,by=10)) 
          # g <- g + geom_line( aes(y=y22),linetype = "dotted", size=1, color="darkblue") +
          #   scale_y_continuous(sec.axis = sec_axis(~.*1, name="Phase Split Percentage (%)"))
          
          g <- g + scale_color_manual(name = "Signal Group", values = c("darkblue"))
          # g <- g + scale_color_manual(name = "Phase B", values = c("darkblue"))
          g <- g + ylab("Degree of Saturation (%)") + xlab("Time")
          g <- g + scale_x_continuous(labels = hms::as_hms(seq(0, 3600*23,by=3600*3)), breaks=seq(0, 3600*23,by=3600*3))
          # g <- g + scale_y_continuous(labels = seq(0, 120,by=10), breaks = seq(0, 120,by=10))
          g <- g + theme_calc()
          g7 <- g
          
        } else { g7 <- ggplot() + theme_void() + geom_text(aes(0,0,label=' ')) + xlab(NULL)}
        if (8 %in% sg_unique$unique_sg){
          SG_number <- 8
          direction_sg <- sites_direction_onesite$direction[sites_direction_onesite$SG == SG_number]
          plot_label_sg <- paste0("Site ",Main_site," on ",Day_name," ",Date_name," SG ",SG_number," for ",direction_sg)
          name2 <- paste0("SG ",SG_number," for ",direction_sg)
          
          df_sg3 <- df_sg %>% dplyr::filter(phase == SG_number)
          xdata <- hms::as_hms(unique(df_sg3$time))
          y1 <- df_sg3$DS_Green_Ratio
          # y2 <- df_phase$Avg_DS_by_Phase[df_phase$Phase_name==phases_unique$unique_phases[2]]
          # y11 <- df_sg3$Phase_time
          # y22 <- df_phase$Phase_time[df_phase$Phase_name==phases_unique$unique_phases[2]]
          # plot(xdata, y1, type="o", col="blue", pch="o", lty=1, ylim=c(0,2),xlab= "Time", ylab="Avg DS to Green Time" )
          df_plot <- data.frame(xdata, y1)
          g <- ggplot(df_plot, aes(xdata))
          g <- g + ggtitle(plot_label_sg)
          g <- g + geom_line(aes(y=y1, color = name2))
          # g <- g + geom_line(aes(y=y2, color = paste0("Phase ",phases_unique$unique_phases[2])))
          g <- g + theme(plot.title = element_text(color="black", size=10, face="bold.italic"),
                         axis.text=element_text(size=8),
                         axis.title=element_text(size=9,face="bold"),
                         legend.text=element_text(size=9),
                         legend.justification=c(1,1),legend.position=c(1,1),legend.title=element_blank())
          # Add a second axis and specify its features
          # g <- g + geom_line( aes(y=y11),linetype = "dotted", size=1, color="red") +
          #   scale_y_continuous(sec.axis = sec_axis(~.*1, name="Phase Split Percentage (%)"), labels = seq(0, 140,by=10), breaks = seq(0, 140,by=10)) 
          # g <- g + geom_line( aes(y=y22),linetype = "dotted", size=1, color="darkblue") +
          #   scale_y_continuous(sec.axis = sec_axis(~.*1, name="Phase Split Percentage (%)"))
          
          g <- g + scale_color_manual(name = "Signal Group", values = c("darkblue"))
          # g <- g + scale_color_manual(name = "Phase B", values = c("darkblue"))
          g <- g + ylab("Degree of Saturation (%)") + xlab("Time")
          g <- g + scale_x_continuous(labels = hms::as_hms(seq(0, 3600*23,by=3600*3)), breaks=seq(0, 3600*23,by=3600*3))
          # g <- g + scale_y_continuous(labels = seq(0, 120,by=10), breaks = seq(0, 120,by=10))
          g <- g + theme_calc()
          g8 <- g
          
        } else { g8 <- ggplot() + theme_void() + geom_text(aes(0,0,label=' ')) + xlab(NULL)}
        if (9 %in% sg_unique$unique_sg){
          SG_number <- 9
          direction_sg <- sites_direction_onesite$direction[sites_direction_onesite$SG == SG_number]
          plot_label_sg <- paste0("Site ",Main_site," on ",Day_name," ",Date_name," SG ",SG_number," for ",direction_sg)
          name2 <- paste0("SG ",SG_number," for ",direction_sg)
          
          df_sg3 <- df_sg %>% dplyr::filter(phase == SG_number)
          xdata <- hms::as_hms(unique(df_sg3$time))
          y1 <- df_sg3$DS_Green_Ratio
          # y2 <- df_phase$Avg_DS_by_Phase[df_phase$Phase_name==phases_unique$unique_phases[2]]
          # y11 <- df_sg3$Phase_time
          # y22 <- df_phase$Phase_time[df_phase$Phase_name==phases_unique$unique_phases[2]]
          # plot(xdata, y1, type="o", col="blue", pch="o", lty=1, ylim=c(0,2),xlab= "Time", ylab="Avg DS to Green Time" )
          df_plot <- data.frame(xdata, y1)
          g <- ggplot(df_plot, aes(xdata))
          g <- g + ggtitle(plot_label_sg)
          g <- g + geom_line(aes(y=y1, color = name2))
          # g <- g + geom_line(aes(y=y2, color = paste0("Phase ",phases_unique$unique_phases[2])))
          g <- g + theme(plot.title = element_text(color="black", size=10, face="bold.italic"),
                         axis.text=element_text(size=8),
                         axis.title=element_text(size=9,face="bold"),
                         legend.text=element_text(size=9),
                         legend.justification=c(1,1),legend.position=c(1,1),legend.title=element_blank())
          # Add a second axis and specify its features
          # g <- g + geom_line( aes(y=y11),linetype = "dotted", size=1, color="red") +
          #   scale_y_continuous(sec.axis = sec_axis(~.*1, name="Phase Split Percentage (%)"), labels = seq(0, 140,by=10), breaks = seq(0, 140,by=10)) 
          # g <- g + geom_line( aes(y=y22),linetype = "dotted", size=1, color="darkblue") +
          #   scale_y_continuous(sec.axis = sec_axis(~.*1, name="Phase Split Percentage (%)"))
          
          g <- g + scale_color_manual(name = "Signal Group", values = c("darkblue"))
          # g <- g + scale_color_manual(name = "Phase B", values = c("darkblue"))
          g <- g + ylab("Degree of Saturation (%)") + xlab("Time")
          g <- g + scale_x_continuous(labels = hms::as_hms(seq(0, 3600*23,by=3600*3)), breaks=seq(0, 3600*23,by=3600*3))
          # g <- g + scale_y_continuous(labels = seq(0, 120,by=10), breaks = seq(0, 120,by=10))
          g <- g + theme_calc()
          g9 <- g
          
        } else { g9 <- ggplot() + theme_void() + geom_text(aes(0,0,label=' ')) + xlab(NULL)}
        output$Plot5 <- renderPlot({
          p1 <- g1  
          p2 <- g2
          p3 <- g3
          p4 <- g4
          p5 <- g5
          p6 <- g6
          p7 <- g7
          p8 <- g8
          p9 <- g9
          grid.arrange(grobs = list(p1,p2,p3,p4,p5,p6,p7,p8,p9), ncol=1, top = paste0("Site ",Main_site," on ",Day_name," ",Date_name), nrow = 9, byrow = TRUE)})
        
      } else if(input$degree_saturation_analysis_type2 == "Phases"){
      df_phase$DS_Green_Ratio <- as.double(as.double(df_phase$Avg_DS_by_Phase)/as.double(df_phase$Phase_time))
      plot_label <- paste0("Site ",Main_site," on ",Day_name," ",Date_name)
      if(nrow(phases_unique)==2){
        xdata <- hms::as_hms(unique(df_phase$time))
        y1 <- df_phase$DS_Green_Ratio[df_phase$Phase_name==phases_unique$unique_phases[1]]
        y2 <- df_phase$DS_Green_Ratio[df_phase$Phase_name==phases_unique$unique_phases[2]]
        # plot(xdata, y1, type="o", col="blue", pch="o", lty=1, ylim=c(0,2),xlab= "Time", ylab="Avg DS to Green Time" )
        df_plot <- data.frame(xdata, y1, y2)
        g <- ggplot(df_plot, aes(xdata))
        g <- g + ggtitle(plot_label)
        g <- g + geom_line(aes(y=y1, color = paste0("Phase ",phases_unique$unique_phases[1])))
        g <- g + geom_line(aes(y=y2, color = paste0("Phase ",phases_unique$unique_phases[2])))
        g <- g + theme(plot.title = element_text(color="black", size=10, face="bold.italic"),
                       axis.text=element_text(size=8),
                       axis.title=element_text(size=9,face="bold"),
                       legend.text=element_text(size=9),
                       legend.justification=c(1,1),legend.position=c(0.24,1),legend.title=element_blank())
        
        g <- g + scale_color_manual(name = "Phases", values = c("red","darkblue"))
        # g <- g + scale_color_manual(name = "Phase B", values = c("darkblue"))
        g <- g + ylab("Avg DS to Phase Time Split Percentage") + xlab("Time")
        g <- g + scale_x_continuous(labels = hms::as_hms(seq(0, 3600*23,by=3600*3)), breaks=seq(0, 3600*23,by=3600*3))
        g <- g + theme_calc()
        g
        output$Plot5_2 <- renderPlot({ g})
      }
      if(nrow(phases_unique)==3){
        xdata <- hms::as_hms(unique(df_phase$time))
        y1 <- df_phase$DS_Green_Ratio[df_phase$Phase_name==phases_unique$unique_phases[1]]
        y2 <- df_phase$DS_Green_Ratio[df_phase$Phase_name==phases_unique$unique_phases[2]]
        y3 <- df_phase$DS_Green_Ratio[df_phase$Phase_name==phases_unique$unique_phases[3]]
        # plot(xdata, y1, type="o", col="blue", pch="o", lty=1, ylim=c(0,2),xlab= "Time", ylab="Avg DS to Green Time" )
        df_plot <- data.frame(xdata, y1, y2, y3)
        g <- ggplot(df_plot, aes(xdata))
        g <- g + ggtitle(plot_label)
        g <- g + geom_line(aes(y=y1, color = paste0("Phase ",phases_unique$unique_phases[1])))
        g <- g + geom_line(aes(y=y2, color = paste0("Phase ",phases_unique$unique_phases[2])))
        g <- g + geom_line(aes(y=y3, color = paste0("Phase ",phases_unique$unique_phases[3])))
        g <- g + theme(plot.title = element_text(color="black", size=10, face="bold.italic"),
                       axis.text=element_text(size=8),
                       axis.title=element_text(size=9,face="bold"),
                       legend.text=element_text(size=9),
                       legend.justification=c(1,1),legend.position=c(0.24,1),legend.title=element_blank())
        
        g <- g + scale_color_manual(name = "Phases", values = c("red","darkblue","darkgreen"))
        # g <- g + scale_color_manual(name = "Phase B", values = c("darkblue"))
        g <- g + ylab("Avg DS to Phase Time Split Percentage") + xlab("Time")
        g <- g + scale_x_continuous(labels = hms::as_hms(seq(0, 3600*23,by=3600*3)), breaks=seq(0, 3600*23,by=3600*3))
        g <- g + theme_calc()
        g
        output$Plot5_2 <- renderPlot({ g})
      }
      if(nrow(phases_unique)==4){
        xdata <- hms::as_hms(unique(df_phase$time))
        y1 <- df_phase$DS_Green_Ratio[df_phase$Phase_name==phases_unique$unique_phases[1]]
        y2 <- df_phase$DS_Green_Ratio[df_phase$Phase_name==phases_unique$unique_phases[2]]
        y3 <- df_phase$DS_Green_Ratio[df_phase$Phase_name==phases_unique$unique_phases[3]]
        y4 <- df_phase$DS_Green_Ratio[df_phase$Phase_name==phases_unique$unique_phases[4]]
        # plot(xdata, y1, type="o", col="blue", pch="o", lty=1, ylim=c(0,2),xlab= "Time", ylab="Avg DS to Green Time" )
        df_plot <- data.frame(xdata, y1, y2, y3, y4)
        g <- ggplot(df_plot, aes(xdata))
        g <- g + ggtitle(plot_label)
        g <- g + geom_line(aes(y=y1, color = paste0("Phase ",phases_unique$unique_phases[1])))
        g <- g + geom_line(aes(y=y2, color = paste0("Phase ",phases_unique$unique_phases[2])))
        g <- g + geom_line(aes(y=y3, color = paste0("Phase ",phases_unique$unique_phases[3])))
        g <- g + geom_line(aes(y=y4, color = paste0("Phase ",phases_unique$unique_phases[4])))
        g <- g + theme(plot.title = element_text(color="black", size=10, face="bold.italic"),
                       axis.text=element_text(size=8),
                       axis.title=element_text(size=9,face="bold"),
                       legend.text=element_text(size=9),
                       legend.justification=c(1,1),legend.position=c(0.24,1),legend.title=element_blank())
        
        g <- g + scale_color_manual(name = "Phases", values = c("red","darkblue","darkgreen","orange"))
        # g <- g + scale_color_manual(name = "Phase B", values = c("darkblue"))
        g <- g + ylab("Avg DS to Phase Time Split Percentage") + xlab("Time")
        g <- g + scale_x_continuous(labels = hms::as_hms(seq(0, 3600*23,by=3600*3)), breaks=seq(0, 3600*23,by=3600*3))
        g <- g + theme_calc()
        g
        output$Plot5_2 <- renderPlot({ g})
      }
      if(nrow(phases_unique)==5){
        xdata <- hms::as_hms(unique(df_phase$time))
        y1 <- df_phase$DS_Green_Ratio[df_phase$Phase_name==phases_unique$unique_phases[1]]
        y2 <- df_phase$DS_Green_Ratio[df_phase$Phase_name==phases_unique$unique_phases[2]]
        y3 <- df_phase$DS_Green_Ratio[df_phase$Phase_name==phases_unique$unique_phases[3]]
        y4 <- df_phase$DS_Green_Ratio[df_phase$Phase_name==phases_unique$unique_phases[4]]
        y5 <- df_phase$DS_Green_Ratio[df_phase$Phase_name==phases_unique$unique_phases[5]]
        # plot(xdata, y1, type="o", col="blue", pch="o", lty=1, ylim=c(0,2),xlab= "Time", ylab="Avg DS to Green Time" )
        df_plot <- data.frame(xdata, y1, y2, y3, y4, y5)
        g <- ggplot(df_plot, aes(xdata))
        g <- g + ggtitle(plot_label)
        g <- g + geom_line(aes(y=y1, color = paste0("Phase ",phases_unique$unique_phases[1])))
        g <- g + geom_line(aes(y=y2, color = paste0("Phase ",phases_unique$unique_phases[2])))
        g <- g + geom_line(aes(y=y3, color = paste0("Phase ",phases_unique$unique_phases[3])))
        g <- g + geom_line(aes(y=y4, color = paste0("Phase ",phases_unique$unique_phases[4])))
        g <- g + geom_line(aes(y=y5, color = paste0("Phase ",phases_unique$unique_phases[5])))
        g <- g + theme(plot.title = element_text(color="black", size=10, face="bold.italic"),
                       axis.text=element_text(size=8),
                       axis.title=element_text(size=9,face="bold"),
                       legend.text=element_text(size=9),
                       legend.justification=c(1,1),legend.position=c(0.24,1),legend.title=element_blank())
        
        g <- g + scale_color_manual(name = "Phases", values = c("red","darkblue","darkgreen","orange","cyan"))
        # g <- g + scale_color_manual(name = "Phase B", values = c("darkblue"))
        g <- g + ylab("Avg DS to Phase Time Split Percentage") + xlab("Time")
        g <- g + scale_x_continuous(labels = hms::as_hms(seq(0, 3600*23,by=3600*3)), breaks=seq(0, 3600*23,by=3600*3))
        g <- g + theme_calc()
        g
        output$Plot5_2 <- renderPlot({ g})
      }
      }
    }

    # output$Plot5 <- renderPlot({ g})
    
    
 
    
  })
  observeEvent(input$previewData6, {
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    
    progress$set(message = "Creating a Heatmap....", value = 0)  
    
    subsystema_file <- "https://raw.githubusercontent.com/abdallahshabarek/ITS_NJDOT/main/Site_Subsystem.csv"
    
    Scats_log <- input$file6$datapath[1]
    Date_choosen <- as.Date(input$date6)
    
    if(Date_choosen ==  as.Date(as.POSIXct(Sys.time(),format="%d/%m/%Y"))){
      print("hi")
      Recent_Datetime <- as.POSIXct(Sys.time(),format="%d/%m/%Y %H:%M:%S")
    } else {
      Recent_Datetime <- Date_choosen
    }
    
    df <- readr::read_csv(Scats_log)
    df <- df %>% dplyr::filter(Event == input$comm_type)
    df <- df %>% dplyr::filter(`+/-` == "+" | `+/-` == "-" | `+/-` =="+=")
    df$Datetime <- as.POSIXct(paste(df$Date, df$Time), format="%d/%m/%Y %H:%M:%S")
    df <- df[with(df, order(Site, Datetime)),]
    
    df$Datetime_end <- as.POSIXct(paste(Recent_Datetime, Sys.time()), format="%d/%m/%Y %H:%M:%S")
    
    for (i in (1:(nrow(df)-1))){
      print(i)
      if ((df$`+/-`[i] == "+" | df$`+/-`[i] == "+=") & df$`+/-`[i+1] == "-" & df$Site[i] == df$Site[i+1]){
        df$Datetime_end[i] <- as.POSIXct(df$Datetime[i+1], format="%d/%m/%Y %H:%M:%S")
      } else if ((df$`+/-`[i] == "+" | df$`+/-`[i] == "+=") & df$Site[i] != df$Site[i+1] ){
        df$Datetime_end[i] <- as.POSIXct(paste(Recent_Datetime, Sys.time()), format="%d/%m/%Y %H:%M:%S")
      }
    }  
    
    df <- df %>% dplyr::filter(df$`+/-` != "-")
    df <- df[!is.na(df$Datetime_end),]
    df$duration <- df$Datetime_end - df$Datetime
    
    time_seq = as.data.frame(seq(from=as.POSIXct(paste0(Date_choosen," 00:00:00"), format="%Y-%m-%d %H:%M:%OS",tz="EST"),    
                                 to=as.POSIXct(paste0(Date_choosen," 23:59:59"), format="%Y-%m-%d %H:%M:%OS", tz="EST"), by=1))
    colnames(time_seq) <- c("Datetime")
    time_seq$time <- as.integer(hms::as_hms(time_seq$Datetime))
    
    colnames(time_seq) <- c("Datetime","time")
    time_seq$Datetime <- as.character(time_seq$Datetime)
    df$Datetime <- as.character(df$Datetime)
    df$Datetime_end <- as.character(df$Datetime_end)
    
    Sites <- unique(readr::read_csv(subsystema_file)$Site)
    df_final_grouped <- NULL
    for (s in Sites){
      print(s)
      time_seq2 <- sqldf::sqldf(paste0("select a.Datetime, a.time, b.Datetime as Datetime_start,
                          c.Datetime_end as Datetime_end
                          from time_seq as a
                          left join (select bb.Datetime, bb.Site from df as bb where bb.Site = ",s,") as b
                          on a.Datetime = b.Datetime
                          left join (select cc.Datetime_end, cc.Site from df as cc where cc.Site = ",s,") as c
                          on a.Datetime = c.Datetime_end
                          order by time asc"), drv = "SQLite")
      time_seq3 <- sqldf::sqldf("select b.Datetime, b.time,
                          case when b.Datetime2 is not null then 1
                          end as flag
                          from(
                          select a.Datetime, a.time, 
                          case when a.Datetime_start is null then a.Datetime_end
                          else a.Datetime_start end as Datetime2
                          from time_seq2 as a) as b", drv ="SQLite")
      
      time_seq3$flag[is.na(time_seq3$flag)] = 0
      time_seq3$flag2 <- 0
      kk = 0
      for (i in (1:nrow(time_seq3))){
        
        if(time_seq3$flag[i] == 1) { 
          kk = kk + 1 
          time_seq3$flag2[i] <- kk
        }
        
      }
      time_seq3$count <- 0
      
      for ( i in (1:nrow(time_seq3))){
        if(i == 1){
          if(!is.integer(time_seq3$flag2[i] %% 2) & time_seq3$flag2[i] != 0){
            time_seq3$count[i] <- 1
          } else {
            time_seq3$count[i] <- 0
          }
        } else if((time_seq3$flag2[i] %% 2) != 0 & time_seq3$flag2[i] != 0){
          time_seq3$count[i] <- 1
        } else if(time_seq3$count[i-1] == 1 & time_seq3$flag2[i] == 0 & (time_seq3$flag2[i-1] %% 2 != 0 | time_seq3$flag2[i-1] == 0)){
          time_seq3$count[i] <- 1
        } else if(time_seq3$count[i-1] == 1 & (time_seq3$flag2[i] %% 2) == 0 & time_seq3$flag2[i] != 0){
          time_seq3$count[i] <- 1
          time_seq3$count[i+1] <- 0
        }
      }
      # 
      # for (i in (1:nrow(time_seq3))){
      #   if (i == 1){
      #     if(time_seq3$flag[i] == 1) {
      #       time_seq3$flag[i] <- 1
      #     } else if (time_seq3$flag[i] != 1) {
      #       time_seq3$flag[i] <- 0
      #     }
      #   } else if (i == nrow(time_seq3)){
      #     if(time_seq3$flag[i-1] == 1) {
      #       time_seq3$flag[i] <- 1
      #     }
      #   } else if (time_seq3$flag[i] == 1 & time_seq3$flag[i+1] != 1 & time_seq3$flag[i-1] != 1) {
      #     time_seq3$flag[i+1] <- 1
      #   } else if (time_seq3$flag[i] == 0 & time_seq3$flag[i+1] != 1 & time_seq3$flag[i-1] == 1) {
      #     time_seq3$flag[i+1] <- 1
      #   } else if (time_seq3$flag[i] == 1 & time_seq3$flag[i+1] != 1 & time_seq3$flag[i-1] == 1) {
      #     time_seq3$flag[i+1] <- 0
      #   }
      # }
      time_seq3$time_hour <- as.integer((time_seq3$time/3600))*3600
      
      time_hour_empty <- as.data.frame(unique(time_seq3$time_hour))
      colnames(time_hour_empty) <- c("time_hour_empty")
      
      time_seq4 <- sqldf::sqldf("select c.time_hour_empty as time_hour, b.frequency as duration_sec from time_hour_empty as c
                  left join (select a.time_hour, count(a.count) as frequency from time_seq3 as a
                          where a.count = 1 group by a.time_hour) as b
                          on b.time_hour = c.time_hour_empty", drv = "SQLite")
      time_seq4$time_hour <- hms::as_hms(time_seq4$time_hour)
      time_seq4$duration_sec[is.na(time_seq4$duration_sec)] = 0
      time_seq4$Site <- s
      df_final_grouped <- rbind(time_seq4,df_final_grouped)
    }
    
    df_final_grouped$time_hour <- as.character(df_final_grouped$time_hour) 
    df_final_grouped$duration_sec <- as.integer(df_final_grouped$duration_sec)
    df_final_grouped2 <- df_final_grouped %>% tidyr::pivot_wider(names_from = time_hour, values_from = duration_sec)
    df_final_grouped2 <- df_final_grouped2[with(df_final_grouped2, order(-Site)),]
    
    my_palette <- colorRampPalette(c("white", "orange", "red"))(n = 100)
    output$Plot6 <- renderPlot({
      # print(paste0("This Plot is generated on ",Sys.Date()," at ",Sys.time()))
      print(heatmap.2(as.matrix(df_final_grouped2[1:nrow(df_final_grouped2),2:ncol(df_final_grouped2)]),
                      col=my_palette,
                      Colv = NA, Rowv = NA, scale="none",
                      cexRow=0.8,cexCol =0.8, labRow=paste("site#", unlist(df_final_grouped2[,1]),sep="")))})
    graphics.off() 
  }) 
  
  output$downloadData6 <- downloadHandler(
    
    filename = function() { 
      paste("Comm_", Sys.time(), ".csv", sep="")
    },
    content = function(file66) { 
      progress <- shiny::Progress$new()
      # Make sure it closes when we exit this reactive, even if there's an error
      on.exit(progress$close())
      
      progress$set(message = "Analyizing the data....", value = 0)
      subsystema_file <- "https://raw.githubusercontent.com/abdallahshabarek/ITS_NJDOT/main/Site_Subsystem.csv"
      
      Scats_log <- input$file6$datapath[1]
      Date_choosen <- as.Date(input$date6)
      
      if(Date_choosen ==  as.Date(as.POSIXct(Sys.time(),format="%d/%m/%Y"))){
        print("hi")
        Recent_Datetime <- as.POSIXct(Sys.time(),format="%d/%m/%Y %H:%M:%S")
      } else {
        Recent_Datetime <- Date_choosen
      }
      
      df <- readr::read_csv(Scats_log)
      df <- df %>% dplyr::filter(Event == input$comm_type)
      df <- df %>% dplyr::filter(`+/-` == "+" | `+/-` == "-" | `+/-` =="+=")
      df$Datetime <- as.POSIXct(paste(df$Date, df$Time), format="%d/%m/%Y %H:%M:%S")
      df <- df[with(df, order(Site, Datetime)),]
      
      df$Datetime_end <- as.POSIXct(paste(Recent_Datetime, Sys.time()), format="%d/%m/%Y %H:%M:%S")
      
      for (i in (1:(nrow(df)-1))){
        print(i)
        if ((df$`+/-`[i] == "+" | df$`+/-`[i] == "+=") & df$`+/-`[i+1] == "-" & df$Site[i] == df$Site[i+1]){
          df$Datetime_end[i] <- as.POSIXct(df$Datetime[i+1], format="%d/%m/%Y %H:%M:%S")
        } else if ((df$`+/-`[i] == "+" | df$`+/-`[i] == "+=") & df$Site[i] != df$Site[i+1] ){
          df$Datetime_end[i] <- as.POSIXct(paste(Recent_Datetime, Sys.time()), format="%d/%m/%Y %H:%M:%S")
        }
      }  
      
      df <- df %>% dplyr::filter(df$`+/-` != "-")
      df <- df[!is.na(df$Datetime_end),]
      df$duration <- df$Datetime_end - df$Datetime
      
      time_seq = as.data.frame(seq(from=as.POSIXct(paste0(Date_choosen," 00:00:00"), format="%Y-%m-%d %H:%M:%OS",tz="EST"),    
                                   to=as.POSIXct(paste0(Date_choosen," 23:59:59"), format="%Y-%m-%d %H:%M:%OS", tz="EST"), by=1))
      colnames(time_seq) <- c("Datetime")
      time_seq$time <- as.integer(hms::as_hms(time_seq$Datetime))
      
      colnames(time_seq) <- c("Datetime","time")
      time_seq$Datetime <- as.character(time_seq$Datetime)
      df$Datetime <- as.character(df$Datetime)
      df$Datetime_end <- as.character(df$Datetime_end)
      
      Sites <- unique(readr::read_csv(subsystema_file)$Site)
      df_final_grouped <- NULL
      for (s in Sites){
        print(s)
        time_seq2 <- sqldf::sqldf(paste0("select a.Datetime, a.time, b.Datetime as Datetime_start,
                          c.Datetime_end as Datetime_end
                          from time_seq as a
                          left join (select bb.Datetime, bb.Site from df as bb where bb.Site = ",s,") as b
                          on a.Datetime = b.Datetime
                          left join (select cc.Datetime_end, cc.Site from df as cc where cc.Site = ",s,") as c
                          on a.Datetime = c.Datetime_end
                          order by time asc"), drv = "SQLite")
        time_seq3 <- sqldf::sqldf("select b.Datetime, b.time,
                          case when b.Datetime2 is not null then 1
                          end as flag
                          from(
                          select a.Datetime, a.time, 
                          case when a.Datetime_start is null then a.Datetime_end
                          else a.Datetime_start end as Datetime2
                          from time_seq2 as a) as b", drv ="SQLite")
        
        time_seq3$flag[is.na(time_seq3$flag)] = 0
        time_seq3$flag2 <- 0
        kk = 0
        for (i in (1:nrow(time_seq3))){
          
          if(time_seq3$flag[i] == 1) { 
            kk = kk + 1 
            time_seq3$flag2[i] <- kk
          }
          
        }
        time_seq3$count <- 0
        
        for ( i in (1:nrow(time_seq3))){
          if(i == 1){
            if(!is.integer(time_seq3$flag2[i] %% 2) & time_seq3$flag2[i] != 0){
              time_seq3$count[i] <- 1
            } else {
              time_seq3$count[i] <- 0
            }
          } else if((time_seq3$flag2[i] %% 2) != 0 & time_seq3$flag2[i] != 0){
            time_seq3$count[i] <- 1
          } else if(time_seq3$count[i-1] == 1 & time_seq3$flag2[i] == 0 & (time_seq3$flag2[i-1] %% 2 != 0 | time_seq3$flag2[i-1] == 0)){
            time_seq3$count[i] <- 1
          } else if(time_seq3$count[i-1] == 1 & (time_seq3$flag2[i] %% 2) == 0 & time_seq3$flag2[i] != 0){
            time_seq3$count[i] <- 1
            time_seq3$count[i+1] <- 0
          }
        }
        # 
        # for (i in (1:nrow(time_seq3))){
        #   if (i == 1){
        #     if(time_seq3$flag[i] == 1) {
        #       time_seq3$flag[i] <- 1
        #     } else if (time_seq3$flag[i] != 1) {
        #       time_seq3$flag[i] <- 0
        #     }
        #   } else if (i == nrow(time_seq3)){
        #     if(time_seq3$flag[i-1] == 1) {
        #       time_seq3$flag[i] <- 1
        #     }
        #   } else if (time_seq3$flag[i] == 1 & time_seq3$flag[i+1] != 1 & time_seq3$flag[i-1] != 1) {
        #     time_seq3$flag[i+1] <- 1
        #   } else if (time_seq3$flag[i] == 0 & time_seq3$flag[i+1] != 1 & time_seq3$flag[i-1] == 1) {
        #     time_seq3$flag[i+1] <- 1
        #   } else if (time_seq3$flag[i] == 1 & time_seq3$flag[i+1] != 1 & time_seq3$flag[i-1] == 1) {
        #     time_seq3$flag[i+1] <- 0
        #   }
        # }
        time_seq3$time_hour <- as.integer((time_seq3$time/3600))*3600
        
        time_hour_empty <- as.data.frame(unique(time_seq3$time_hour))
        colnames(time_hour_empty) <- c("time_hour_empty")
        
        time_seq4 <- sqldf::sqldf("select c.time_hour_empty as time_hour, b.frequency as duration_sec from time_hour_empty as c
                  left join (select a.time_hour, count(a.count) as frequency from time_seq3 as a
                          where a.count = 1 group by a.time_hour) as b
                          on b.time_hour = c.time_hour_empty", drv = "SQLite")
        time_seq4$time_hour <- hms::as_hms(time_seq4$time_hour)
        time_seq4$duration_sec[is.na(time_seq4$duration_sec)] = 0
        time_seq4$Site <- s
        df_final_grouped <- rbind(time_seq4,df_final_grouped)
      }
      
      df_final_grouped$time_hour <- as.character(df_final_grouped$time_hour) 
      df_final_grouped$duration_sec <- as.integer(df_final_grouped$duration_sec)
      df_final_grouped2 <- df_final_grouped %>% tidyr::pivot_wider(names_from = time_hour, values_from = duration_sec)
      df_final_grouped2 <- df_final_grouped2[with(df_final_grouped2, order(-Site)),]
      
      write.csv(df_final_grouped2, file66, row.names=FALSE)
    })
  
  
  observeEvent(input$previewData7_1, {
  
    print("hi")
    print("hi2")
    print(input$date7_multiple)
    offset_avg_df <- as.data.frame(input$date7_multiple)
    colnames(offset_avg_df) <- c("offset_dates")
    }
  )
  
  output$downloadData8_1 <- downloadHandler(
    
    filename = function() { 
      paste("TOD_", Sys.time(), ".csv", sep="")
    },
    content = function(file8) { 
      progress <- shiny::Progress$new()
      # Make sure it closes when we exit this reactive, even if there's an error
      on.exit(progress$close())
      
      progress$set(message = "Analyizing the data....", value = 0)
      subsystema_file <- "https://raw.githubusercontent.com/abdallahshabarek/ITS_NJDOT/main/Site_Subsystem.csv"
      
      if(input$file_8_offpeak_cycles == "no"){
        source("https://raw.githubusercontent.com/abdallahshabarek/ITS_NJDOT/main/Scripts/function5_TOD_2.R")
        source("https://raw.githubusercontent.com/abdallahshabarek/ITS_NJDOT/main/Scripts/Optimization_Script3.R")
      }
      if(input$file_8_offpeak_cycles == "yes"){
        source("https://raw.githubusercontent.com/abdallahshabarek/ITS_NJDOT/main/Scripts/function5_TOD_2.R")
        source("https://raw.githubusercontent.com/abdallahshabarek/ITS_NJDOT/main/Scripts/Optimization_Script4.R")
      }
      
      if(input$file_8_time_type=="midweek"){
      dttt <- function5_midweek(dirname(input$file8$datapath)[1],zoneid = as.integer(input$file8_zoneid))
      }
      if(input$file_8_time_type=="monday"){
        dttt <- function5_mon(dirname(input$file8$datapath)[1],zoneid = as.integer(input$file8_zoneid))
      }
      if(input$file_8_time_type=="tuesday"){
        dttt <- function5_tue(dirname(input$file8$datapath)[1],zoneid = as.integer(input$file8_zoneid))
      }
      if(input$file_8_time_type=="wednesday"){
        dttt <- function5_wed(dirname(input$file8$datapath)[1],zoneid = as.integer(input$file8_zoneid))
      }
      if(input$file_8_time_type=="thursday"){
        dttt <- function5_thu(dirname(input$file8$datapath)[1],zoneid = as.integer(input$file8_zoneid))
      }
      if(input$file_8_time_type=="friday"){
        dttt <- function5_fri(dirname(input$file8$datapath)[1],zoneid = as.integer(input$file8_zoneid))
      }
      if(input$file_8_time_type=="saturday"){
        dttt <- function5_sat(dirname(input$file8$datapath)[1],zoneid = as.integer(input$file8_zoneid))
      }
      if(input$file_8_time_type=="sunday"){
        dttt <- function5_sun(dirname(input$file8$datapath)[1],zoneid = as.integer(input$file8_zoneid))
      }
      
      dttt2 <- TOD_optimization(dataframe_avg = dttt,max_periods = as.integer(input$file8_max_periods), min_dur = as.integer(input$file8_min_time), max_dur = as.integer(input$file8_max_time))
      # dirname(input$file8$datapath)[1]
      write.csv(dttt2, file8, row.names=FALSE)
    }
  )
  
  output$downloadData8_2 <- downloadHandler(
    
    filename = function() { 
      paste("TOD_", Sys.time(), ".csv", sep="")
    },
    content = function(file8) { 
      progress <- shiny::Progress$new()
      # Make sure it closes when we exit this reactive, even if there's an error
      on.exit(progress$close())
      
      progress$set(message = "Analyizing the data....", value = 0)
      subsystema_file <- "https://raw.githubusercontent.com/abdallahshabarek/ITS_NJDOT/main/Site_Subsystem.csv"
      
      if(input$file_8_offpeak_cycles == "no"){
        source("https://raw.githubusercontent.com/abdallahshabarek/ITS_NJDOT/main/Scripts/function5_TOD_2.R")
        source("https://raw.githubusercontent.com/abdallahshabarek/ITS_NJDOT/main/Scripts/Optimization_Script3.R")
      }
      if(input$file_8_offpeak_cycles == "yes"){
        source("https://raw.githubusercontent.com/abdallahshabarek/ITS_NJDOT/main/Scripts/function5_TOD_2.R")
        source("https://raw.githubusercontent.com/abdallahshabarek/ITS_NJDOT/main/Scripts/Optimization_Script4.R")
      }
      
      if(input$file_8_time_type=="midweek"){
        dttt <- function5_midweek(dirname(input$file8$datapath)[1],zoneid = as.integer(input$file8_zoneid))
      }
      if(input$file_8_time_type=="monday"){
        dttt <- function5_mon(dirname(input$file8$datapath)[1],zoneid = as.integer(input$file8_zoneid))
      }
      if(input$file_8_time_type=="tuesday"){
        dttt <- function5_tue(dirname(input$file8$datapath)[1],zoneid = as.integer(input$file8_zoneid))
      }
      if(input$file_8_time_type=="wednesday"){
        dttt <- function5_wed(dirname(input$file8$datapath)[1],zoneid = as.integer(input$file8_zoneid))
      }
      if(input$file_8_time_type=="thursday"){
        dttt <- function5_thu(dirname(input$file8$datapath)[1],zoneid = as.integer(input$file8_zoneid))
      }
      if(input$file_8_time_type=="friday"){
        dttt <- function5_fri(dirname(input$file8$datapath)[1],zoneid = as.integer(input$file8_zoneid))
      }
      if(input$file_8_time_type=="saturday"){
        dttt <- function5_sat(dirname(input$file8$datapath)[1],zoneid = as.integer(input$file8_zoneid))
      }
      if(input$file_8_time_type=="sunday"){
        dttt <- function5_sun(dirname(input$file8$datapath)[1],zoneid = as.integer(input$file8_zoneid))
      }
      
      dttt2 <- TOD_optimization(dataframe_avg = dttt,max_periods = as.integer(input$file8_max_periods), min_dur = as.integer(input$file8_min_time), max_dur = as.integer(input$file8_max_time))
      
      
      time_dttt <- dttt2$time[1]
      proposed_dttt <- dttt2$proposed[1]
      df_file8_abstract <- data.frame(time_dttt,proposed_dttt)
      colnames(df_file8_abstract) <- c("time","proposed")
      for(k_file8_abstract in 2:nrow(dttt2)){
        # print(k_file8_abstract)
        if((dttt2$proposed[k_file8_abstract] != dttt2$proposed[k_file8_abstract-1])==TRUE){
          print(k_file8_abstract)
          temp_file8_abstract <- data.frame(cbind(dttt2$time[k_file8_abstract],dttt2$proposed[k_file8_abstract]))
          colnames(temp_file8_abstract) <- c("time","proposed")
          df_file8_abstract <- rbind(df_file8_abstract,temp_file8_abstract)
          # time_dttt <- rbind(time_dttt,dttt2$time[k_file8_abstract])
          # proposed_dttt <- rbind(proposed_dttt,dttt2$proposed[k_file8_abstract])
        }
      }
      # dttt3 <- sqldf::sqldf("select a.time, a.proposed from dttt2 as a
      #              group by a.proposed
      #                       order by a.time asc", drv = "SQLite")
      # dttt3$time <- hms::as_hms(dttt3$time*60)
      # # dirname(input$file8$datapath)[1]
      df_file8_abstract$time <- hms::as_hms(df_file8_abstract$time*60)
      write.csv(df_file8_abstract, file8, row.names=FALSE)
    }
  )
  
  observeEvent(input$previewData8_1, {
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    
    progress$set(message = "Creating a Plot....", value = 0) 
    if(input$file_8_offpeak_cycles == "no"){
      source("https://raw.githubusercontent.com/abdallahshabarek/ITS_NJDOT/main/Scripts/function5_TOD_2.R")
      source("https://raw.githubusercontent.com/abdallahshabarek/ITS_NJDOT/main/Scripts/Optimization_Script3.R")
    }
    if(input$file_8_offpeak_cycles == "yes"){
      source("https://raw.githubusercontent.com/abdallahshabarek/ITS_NJDOT/main/Scripts/function5_TOD_2.R")
      source("https://raw.githubusercontent.com/abdallahshabarek/ITS_NJDOT/main/Scripts/Optimization_Script4.R")
    }
    
    if(input$file_8_time_type=="midweek"){
      dttt <- function5_midweek(dirname(input$file8$datapath)[1],zoneid = as.integer(input$file8_zoneid))
    }
    if(input$file_8_time_type=="monday"){
      dttt <- function5_mon(dirname(input$file8$datapath)[1],zoneid = as.integer(input$file8_zoneid))
    }
    if(input$file_8_time_type=="tuesday"){
      dttt <- function5_tue(dirname(input$file8$datapath)[1],zoneid = as.integer(input$file8_zoneid))
    }
    if(input$file_8_time_type=="wednesday"){
      dttt <- function5_wed(dirname(input$file8$datapath)[1],zoneid = as.integer(input$file8_zoneid))
    }
    if(input$file_8_time_type=="thursday"){
      dttt <- function5_thu(dirname(input$file8$datapath)[1],zoneid = as.integer(input$file8_zoneid))
    }
    if(input$file_8_time_type=="friday"){
      dttt <- function5_fri(dirname(input$file8$datapath)[1],zoneid = as.integer(input$file8_zoneid))
    }
    if(input$file_8_time_type=="saturday"){
      dttt <- function5_sat(dirname(input$file8$datapath)[1],zoneid = as.integer(input$file8_zoneid))
    }
    if(input$file_8_time_type=="sunday"){
      dttt <- function5_sun(dirname(input$file8$datapath)[1],zoneid = as.integer(input$file8_zoneid))
    }
    
    dttt2 <- TOD_optimization(dataframe_avg = dttt,max_periods = as.integer(input$file8_max_periods), min_dur = as.integer(input$file8_min_time), max_dur = as.integer(input$file8_max_time))
    
    
  plot_label_TOD_cycle <- paste0("TOD Cycle Length Analysis for ",input$file_8_time_type)
  name1 <- paste0("Nominal")
  name2 <- paste0("Proposed")
  name3 <- paste0("Existing TOD")
  
  xdata <- hms::as_hms(unique(dttt2$time)*60)
  y1 <- dttt2$Nominal_Cycle_Length
  y2 <- dttt2$proposed
  y3 <- dttt2$TOD_Cycle_Length
  # y2 <- df_phase$Avg_DS_by_Phase[df_phase$Phase_name==phases_unique$unique_phases[2]]
  # y11 <- df_sg3$Phase_time
  # y22 <- df_phase$Phase_time[df_phase$Phase_name==phases_unique$unique_phases[2]]
  # plot(xdata, y1, type="o", col="blue", pch="o", lty=1, ylim=c(0,2),xlab= "Time", ylab="Avg DS to Green Time" )
  df_plot <- data.frame(xdata, y1, y2,y3)
  g <- ggplot(df_plot, aes(xdata))
  g <- g + ggtitle(plot_label_TOD_cycle)
  g <- g + geom_line(aes(y= y1, color = name1))
  g <- g + geom_line(aes(y= y2, color = name2))
  g <- g + geom_line(aes(y= y3, color = name3))
  # g <- g + geom_line(aes(y=y2, color = paste0("Phase ",phases_unique$unique_phases[2])))
  g <- g + theme(plot.title = element_text(color="black", size=10, face="bold.italic"),
                 axis.text=element_text(size=8),
                 axis.title=element_text(size=9,face="bold"),
                 legend.text=element_text(size=9),
                 legend.justification=c(1,1),legend.position=c(1,1),legend.title=element_blank())
  # Add a second axis and specify its features
  # g <- g + geom_line( aes(y=y11),linetype = "dotted", size=1, color="red") +
  #   scale_y_continuous(sec.axis = sec_axis(~.*1, name="Phase Split Percentage (%)"), labels = seq(0, 140,by=10), breaks = seq(0, 140,by=10)) 
  # g <- g + geom_line( aes(y=y22),linetype = "dotted", size=1, color="darkblue") +
  #   scale_y_continuous(sec.axis = sec_axis(~.*1, name="Phase Split Percentage (%)"))
  
  g <- g + scale_color_manual(name = "Cycle Length", values = c("darkblue","red","darkgreen"))
  # g <- g + scale_color_manual(name = "Phase B", values = c("darkblue"))
  g <- g + ylab("Cycle Length (sec)") + xlab("Time")
  g <- g + scale_x_continuous(labels = hms::as_hms(seq(0, 3600*23,by=3600*3)), breaks=seq(0, 3600*23,by=3600*3))
  g <- g + ylim(0, +240)
  # g <- g + scale_y_continuous(labels = seq(0, 120,by=10), breaks = seq(0, 120,by=10))
  g <- g + theme_calc()
  g_file_8 <- g
  output$Plot8_1 <- renderPlot({g_file_8})
  
  })
  
  output$downloadData8_3 <- downloadHandler(
    
    filename = function() {
      paste("TODPhase_", Sys.time(), ".csv", sep="")
    },
    content = function(file8_2) {
      progress <- shiny::Progress$new()
      # Make sure it closes when we exit this reactive, even if there's an error
      on.exit(progress$close())
      
      progress$set(message = "Analyizing the data....", value = 0)
      #   # content = function(file8_3) { 
          source("https://raw.githubusercontent.com/abdallahshabarek/ITS_NJDOT/main/Scripts/TOD_phase_optimization_from_files_function.R")
          dirname(input$file8_2$datapath)[1]
          dt <- TOD_phase_optimization_from_files(input_phase_folder=paste0(dirname(input$file8_2$datapath)[1],"/"),
                                                  analysis_time_type=input$file_8_time_type)
          df_abstract_phase <- readr::read_csv(input$file8_3$datapath)
          df_abstract_phase$time <- as.integer(df_abstract_phase$time)
          number_phases <- as.integer(nrow(data.frame(unique(dt$phases))))
          
          if(number_phases == 2){
            df_a <- dt[dt$phases=="<A>",]
            df_b <- dt[dt$phases=="B",]
            # df_c <- dt[dt$phases=="C",]
            # df_d <- dt[dt$phases=="D",]
            # df_e <- dt[dt$phases=="E",]
            # df_f <- dt[dt$phases=="F",]
            # df_f <- dt[dt$phases=="G",]
            # df_f <- dt[dt$phases=="H",]
            # df_f <- dt[dt$phases=="I",]
            df_abstract_phase$split_A_Avg <- 0
            df_abstract_phase$split_B_Avg <- 0
            # df_abstract_phase$split_C_Avg <- 0
            # df_abstract_phase$split_D_Avg <- 0
            # df_abstract_phase$split_E_Avg <- 0
            # df_abstract_phase$split_F_Avg <- 0
            # df_abstract_phase$split_G_Avg <- 0
            # df_abstract_phase$split_H_Avg <- 0
            # df_abstract_phase$split_I_Avg <- 0
            
            for (k in 1:nrow(df_abstract_phase)-1){
              df_abstract_phase$split_A_Avg[k] <- mean(df_a$split_percentage[df_a$time >= df_abstract_phase$time[k] & df_a$time < df_abstract_phase$time[k+1]])
              df_abstract_phase$split_B_Avg[k] <- mean(df_b$split_percentage[df_b$time >= df_abstract_phase$time[k] & df_b$time < df_abstract_phase$time[k+1]])
              # df_abstract_phase$split_C_Avg[k] <- mean(df_c$split_percentage[df_c$time >= df_abstract_phase$time[k] & df_c$time < df_abstract_phase$time[k+1]])
              # df_abstract_phase$split_D_Avg[k] <- mean(df_d$split_percentage[df_d$time >= df_abstract_phase$time[k] & df_d$time < df_abstract_phase$time[k+1]])
              # df_abstract_phase$split_E_Avg[k] <- mean(df_e$split_percentage[df_e$time >= df_abstract_phase$time[k] & df_e$time < df_abstract_phase$time[k+1]])
              # df_abstract_phase$split_F_Avg[k] <- mean(df_f$split_percentage[df_f$time >= df_abstract_phase$time[k] & df_f$time < df_abstract_phase$time[k+1]])
              # df_abstract_phase$split_G_Avg[k] <- mean(df_g$split_percentage[df_g$time >= df_abstract_phase$time[k] & df_g$time < df_abstract_phase$time[k+1]])
              # df_abstract_phase$split_H_Avg[k] <- mean(df_h$split_percentage[df_h$time >= df_abstract_phase$time[k] & df_h$time < df_abstract_phase$time[k+1]])
              # df_abstract_phase$split_I_Avg[k] <- mean(df_i$split_percentage[df_i$time >= df_abstract_phase$time[k] & df_i$time < df_abstract_phase$time[k+1]])
            }
            df_abstract_phase$split_A_Avg[nrow(df_abstract_phase)] <- mean(df_a$split_percentage[df_a$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
            df_abstract_phase$split_B_Avg[nrow(df_abstract_phase)] <- mean(df_b$split_percentage[df_b$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
            # df_abstract_phase$split_C_Avg[nrow(df_abstract_phase)] <- mean(df_c$split_percentage[df_c$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
            # df_abstract_phase$split_D_Avg[nrow(df_abstract_phase)] <- mean(df_d$split_percentage[df_d$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
            # df_abstract_phase$split_E_Avg[nrow(df_abstract_phase)] <- mean(df_e$split_percentage[df_e$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
            # df_abstract_phase$split_F_Avg[nrow(df_abstract_phase)] <- mean(df_f$split_percentage[df_d$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
            # df_abstract_phase$split_G_Avg[nrow(df_abstract_phase)] <- mean(df_g$split_percentage[df_e$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
            # df_abstract_phase$split_H_Avg[nrow(df_abstract_phase)] <- mean(df_h$split_percentage[df_d$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
            # df_abstract_phase$split_I_Avg[nrow(df_abstract_phase)] <- mean(df_i$split_percentage[df_e$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
            
            df_abstract_phase$A_sec <- as.integer(round(df_abstract_phase$split_A_Avg*df_abstract_phase$proposed),0)
            df_abstract_phase$B_sec <- as.integer(round(df_abstract_phase$split_B_Avg*df_abstract_phase$proposed),0)
            # df_abstract_phase$C_sec <- as.integer(round(df_abstract_phase$split_C_Avg*df_abstract_phase$proposed),0)
            # df_abstract_phase$D_sec <- as.integer(df_abstract_phase$split_D_Avg*df_abstract_phase$proposed)
            # df_abstract_phase$E_sec <- as.integer(df_abstract_phase$split_E_Avg*df_abstract_phase$proposed)
            # df_abstract_phase$F_sec <- as.integer(df_abstract_phase$split_F_Avg*df_abstract_phase$proposed)
            # df_abstract_phase$G_sec <- as.integer(df_abstract_phase$split_G_Avg*df_abstract_phase$proposed)
            # df_abstract_phase$H_sec <- as.integer(df_abstract_phase$split_H_Avg*df_abstract_phase$proposed)
            # df_abstract_phase$I_sec <- as.integer(df_abstract_phase$split_I_Avg*df_abstract_phase$proposed)
          }
          if(number_phases == 3){
            df_a <- dt[dt$phases=="<A>",]
            df_b <- dt[dt$phases=="B",]
            df_c <- dt[dt$phases=="C",]
            # df_d <- dt[dt$phases=="D",]
            # df_e <- dt[dt$phases=="E",]
            # df_f <- dt[dt$phases=="F",]
            # df_f <- dt[dt$phases=="G",]
            # df_f <- dt[dt$phases=="H",]
            # df_f <- dt[dt$phases=="I",]
            df_abstract_phase$split_A_Avg <- 0
            df_abstract_phase$split_B_Avg <- 0
            df_abstract_phase$split_C_Avg <- 0
            # df_abstract_phase$split_D_Avg <- 0
            # df_abstract_phase$split_E_Avg <- 0
            # df_abstract_phase$split_F_Avg <- 0
            # df_abstract_phase$split_G_Avg <- 0
            # df_abstract_phase$split_H_Avg <- 0
            # df_abstract_phase$split_I_Avg <- 0
            
            for (k in 1:nrow(df_abstract_phase)-1){
              df_abstract_phase$split_A_Avg[k] <- mean(df_a$split_percentage[df_a$time >= df_abstract_phase$time[k] & df_a$time < df_abstract_phase$time[k+1]])
              df_abstract_phase$split_B_Avg[k] <- mean(df_b$split_percentage[df_b$time >= df_abstract_phase$time[k] & df_b$time < df_abstract_phase$time[k+1]])
              df_abstract_phase$split_C_Avg[k] <- mean(df_c$split_percentage[df_c$time >= df_abstract_phase$time[k] & df_c$time < df_abstract_phase$time[k+1]])
              # df_abstract_phase$split_D_Avg[k] <- mean(df_d$split_percentage[df_d$time >= df_abstract_phase$time[k] & df_d$time < df_abstract_phase$time[k+1]])
              # df_abstract_phase$split_E_Avg[k] <- mean(df_e$split_percentage[df_e$time >= df_abstract_phase$time[k] & df_e$time < df_abstract_phase$time[k+1]])
              # df_abstract_phase$split_F_Avg[k] <- mean(df_f$split_percentage[df_f$time >= df_abstract_phase$time[k] & df_f$time < df_abstract_phase$time[k+1]])
              # df_abstract_phase$split_G_Avg[k] <- mean(df_g$split_percentage[df_g$time >= df_abstract_phase$time[k] & df_g$time < df_abstract_phase$time[k+1]])
              # df_abstract_phase$split_H_Avg[k] <- mean(df_h$split_percentage[df_h$time >= df_abstract_phase$time[k] & df_h$time < df_abstract_phase$time[k+1]])
              # df_abstract_phase$split_I_Avg[k] <- mean(df_i$split_percentage[df_i$time >= df_abstract_phase$time[k] & df_i$time < df_abstract_phase$time[k+1]])
            }
            df_abstract_phase$split_A_Avg[nrow(df_abstract_phase)] <- mean(df_a$split_percentage[df_a$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
            df_abstract_phase$split_B_Avg[nrow(df_abstract_phase)] <- mean(df_b$split_percentage[df_b$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
            df_abstract_phase$split_C_Avg[nrow(df_abstract_phase)] <- mean(df_c$split_percentage[df_c$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
            # df_abstract_phase$split_D_Avg[nrow(df_abstract_phase)] <- mean(df_d$split_percentage[df_d$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
            # df_abstract_phase$split_E_Avg[nrow(df_abstract_phase)] <- mean(df_e$split_percentage[df_e$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
            # df_abstract_phase$split_F_Avg[nrow(df_abstract_phase)] <- mean(df_f$split_percentage[df_d$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
            # df_abstract_phase$split_G_Avg[nrow(df_abstract_phase)] <- mean(df_g$split_percentage[df_e$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
            # df_abstract_phase$split_H_Avg[nrow(df_abstract_phase)] <- mean(df_h$split_percentage[df_d$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
            # df_abstract_phase$split_I_Avg[nrow(df_abstract_phase)] <- mean(df_i$split_percentage[df_e$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
            
            df_abstract_phase$A_sec <- as.integer(round(df_abstract_phase$split_A_Avg*df_abstract_phase$proposed),0)
            df_abstract_phase$B_sec <- as.integer(round(df_abstract_phase$split_B_Avg*df_abstract_phase$proposed),0)
            df_abstract_phase$C_sec <- as.integer(round(df_abstract_phase$split_C_Avg*df_abstract_phase$proposed),0)
            # df_abstract_phase$D_sec <- as.integer(df_abstract_phase$split_D_Avg*df_abstract_phase$proposed)
            # df_abstract_phase$E_sec <- as.integer(df_abstract_phase$split_E_Avg*df_abstract_phase$proposed)
            # df_abstract_phase$F_sec <- as.integer(df_abstract_phase$split_F_Avg*df_abstract_phase$proposed)
            # df_abstract_phase$G_sec <- as.integer(df_abstract_phase$split_G_Avg*df_abstract_phase$proposed)
            # df_abstract_phase$H_sec <- as.integer(df_abstract_phase$split_H_Avg*df_abstract_phase$proposed)
            # df_abstract_phase$I_sec <- as.integer(df_abstract_phase$split_I_Avg*df_abstract_phase$proposed)
          }
          if(number_phases == 4){
            df_a <- dt[dt$phases=="<A>",]
            df_b <- dt[dt$phases=="B",]
            df_c <- dt[dt$phases=="C",]
            df_d <- dt[dt$phases=="D",]
            # df_e <- dt[dt$phases=="E",]
            # df_f <- dt[dt$phases=="F",]
            # df_f <- dt[dt$phases=="G",]
            # df_f <- dt[dt$phases=="H",]
            # df_f <- dt[dt$phases=="I",]
            df_abstract_phase$split_A_Avg <- 0
            df_abstract_phase$split_B_Avg <- 0
            df_abstract_phase$split_C_Avg <- 0
            df_abstract_phase$split_D_Avg <- 0
            # df_abstract_phase$split_E_Avg <- 0
            # df_abstract_phase$split_F_Avg <- 0
            # df_abstract_phase$split_G_Avg <- 0
            # df_abstract_phase$split_H_Avg <- 0
            # df_abstract_phase$split_I_Avg <- 0
            
            for (k in 1:nrow(df_abstract_phase)-1){
              df_abstract_phase$split_A_Avg[k] <- mean(df_a$split_percentage[df_a$time >= df_abstract_phase$time[k] & df_a$time < df_abstract_phase$time[k+1]])
              df_abstract_phase$split_B_Avg[k] <- mean(df_b$split_percentage[df_b$time >= df_abstract_phase$time[k] & df_b$time < df_abstract_phase$time[k+1]])
              df_abstract_phase$split_C_Avg[k] <- mean(df_c$split_percentage[df_c$time >= df_abstract_phase$time[k] & df_c$time < df_abstract_phase$time[k+1]])
              df_abstract_phase$split_D_Avg[k] <- mean(df_d$split_percentage[df_d$time >= df_abstract_phase$time[k] & df_d$time < df_abstract_phase$time[k+1]])
              # df_abstract_phase$split_E_Avg[k] <- mean(df_e$split_percentage[df_e$time >= df_abstract_phase$time[k] & df_e$time < df_abstract_phase$time[k+1]])
              # df_abstract_phase$split_F_Avg[k] <- mean(df_f$split_percentage[df_f$time >= df_abstract_phase$time[k] & df_f$time < df_abstract_phase$time[k+1]])
              # df_abstract_phase$split_G_Avg[k] <- mean(df_g$split_percentage[df_g$time >= df_abstract_phase$time[k] & df_g$time < df_abstract_phase$time[k+1]])
              # df_abstract_phase$split_H_Avg[k] <- mean(df_h$split_percentage[df_h$time >= df_abstract_phase$time[k] & df_h$time < df_abstract_phase$time[k+1]])
              # df_abstract_phase$split_I_Avg[k] <- mean(df_i$split_percentage[df_i$time >= df_abstract_phase$time[k] & df_i$time < df_abstract_phase$time[k+1]])
            }
            df_abstract_phase$split_A_Avg[nrow(df_abstract_phase)] <- mean(df_a$split_percentage[df_a$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
            df_abstract_phase$split_B_Avg[nrow(df_abstract_phase)] <- mean(df_b$split_percentage[df_b$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
            df_abstract_phase$split_C_Avg[nrow(df_abstract_phase)] <- mean(df_c$split_percentage[df_c$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
            df_abstract_phase$split_D_Avg[nrow(df_abstract_phase)] <- mean(df_d$split_percentage[df_d$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
            # df_abstract_phase$split_E_Avg[nrow(df_abstract_phase)] <- mean(df_e$split_percentage[df_e$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
            # df_abstract_phase$split_F_Avg[nrow(df_abstract_phase)] <- mean(df_f$split_percentage[df_d$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
            # df_abstract_phase$split_G_Avg[nrow(df_abstract_phase)] <- mean(df_g$split_percentage[df_e$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
            # df_abstract_phase$split_H_Avg[nrow(df_abstract_phase)] <- mean(df_h$split_percentage[df_d$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
            # df_abstract_phase$split_I_Avg[nrow(df_abstract_phase)] <- mean(df_i$split_percentage[df_e$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
            
            df_abstract_phase$A_sec <- as.integer(round(df_abstract_phase$split_A_Avg*df_abstract_phase$proposed),0)
            df_abstract_phase$B_sec <- as.integer(round(df_abstract_phase$split_B_Avg*df_abstract_phase$proposed),0)
            df_abstract_phase$C_sec <- as.integer(round(df_abstract_phase$split_C_Avg*df_abstract_phase$proposed),0)
            df_abstract_phase$D_sec <- as.integer(df_abstract_phase$split_D_Avg*df_abstract_phase$proposed)
            # df_abstract_phase$E_sec <- as.integer(df_abstract_phase$split_E_Avg*df_abstract_phase$proposed)
            # df_abstract_phase$F_sec <- as.integer(df_abstract_phase$split_F_Avg*df_abstract_phase$proposed)
            # df_abstract_phase$G_sec <- as.integer(df_abstract_phase$split_G_Avg*df_abstract_phase$proposed)
            # df_abstract_phase$H_sec <- as.integer(df_abstract_phase$split_H_Avg*df_abstract_phase$proposed)
            # df_abstract_phase$I_sec <- as.integer(df_abstract_phase$split_I_Avg*df_abstract_phase$proposed)
          }
          if(number_phases == 5){
            df_a <- dt[dt$phases=="<A>",]
            df_b <- dt[dt$phases=="B",]
            df_c <- dt[dt$phases=="C",]
            df_d <- dt[dt$phases=="D",]
            df_e <- dt[dt$phases=="E",]
            # df_f <- dt[dt$phases=="F",]
            # df_f <- dt[dt$phases=="G",]
            # df_f <- dt[dt$phases=="H",]
            # df_f <- dt[dt$phases=="I",]
            df_abstract_phase$split_A_Avg <- 0
            df_abstract_phase$split_B_Avg <- 0
            df_abstract_phase$split_C_Avg <- 0
            df_abstract_phase$split_D_Avg <- 0
            df_abstract_phase$split_E_Avg <- 0
            # df_abstract_phase$split_F_Avg <- 0
            # df_abstract_phase$split_G_Avg <- 0
            # df_abstract_phase$split_H_Avg <- 0
            # df_abstract_phase$split_I_Avg <- 0
            
            for (k in 1:nrow(df_abstract_phase)-1){
              df_abstract_phase$split_A_Avg[k] <- mean(df_a$split_percentage[df_a$time >= df_abstract_phase$time[k] & df_a$time < df_abstract_phase$time[k+1]])
              df_abstract_phase$split_B_Avg[k] <- mean(df_b$split_percentage[df_b$time >= df_abstract_phase$time[k] & df_b$time < df_abstract_phase$time[k+1]])
              df_abstract_phase$split_C_Avg[k] <- mean(df_c$split_percentage[df_c$time >= df_abstract_phase$time[k] & df_c$time < df_abstract_phase$time[k+1]])
              df_abstract_phase$split_D_Avg[k] <- mean(df_d$split_percentage[df_d$time >= df_abstract_phase$time[k] & df_d$time < df_abstract_phase$time[k+1]])
              df_abstract_phase$split_E_Avg[k] <- mean(df_e$split_percentage[df_e$time >= df_abstract_phase$time[k] & df_e$time < df_abstract_phase$time[k+1]])
              # df_abstract_phase$split_F_Avg[k] <- mean(df_f$split_percentage[df_f$time >= df_abstract_phase$time[k] & df_f$time < df_abstract_phase$time[k+1]])
              # df_abstract_phase$split_G_Avg[k] <- mean(df_g$split_percentage[df_g$time >= df_abstract_phase$time[k] & df_g$time < df_abstract_phase$time[k+1]])
              # df_abstract_phase$split_H_Avg[k] <- mean(df_h$split_percentage[df_h$time >= df_abstract_phase$time[k] & df_h$time < df_abstract_phase$time[k+1]])
              # df_abstract_phase$split_I_Avg[k] <- mean(df_i$split_percentage[df_i$time >= df_abstract_phase$time[k] & df_i$time < df_abstract_phase$time[k+1]])
            }
            df_abstract_phase$split_A_Avg[nrow(df_abstract_phase)] <- mean(df_a$split_percentage[df_a$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
            df_abstract_phase$split_B_Avg[nrow(df_abstract_phase)] <- mean(df_b$split_percentage[df_b$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
            df_abstract_phase$split_C_Avg[nrow(df_abstract_phase)] <- mean(df_c$split_percentage[df_c$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
            df_abstract_phase$split_D_Avg[nrow(df_abstract_phase)] <- mean(df_d$split_percentage[df_d$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
            df_abstract_phase$split_E_Avg[nrow(df_abstract_phase)] <- mean(df_e$split_percentage[df_e$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
            # df_abstract_phase$split_F_Avg[nrow(df_abstract_phase)] <- mean(df_f$split_percentage[df_d$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
            # df_abstract_phase$split_G_Avg[nrow(df_abstract_phase)] <- mean(df_g$split_percentage[df_e$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
            # df_abstract_phase$split_H_Avg[nrow(df_abstract_phase)] <- mean(df_h$split_percentage[df_d$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
            # df_abstract_phase$split_I_Avg[nrow(df_abstract_phase)] <- mean(df_i$split_percentage[df_e$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
            
            df_abstract_phase$A_sec <- as.integer(round(df_abstract_phase$split_A_Avg*df_abstract_phase$proposed),0)
            df_abstract_phase$B_sec <- as.integer(round(df_abstract_phase$split_B_Avg*df_abstract_phase$proposed),0)
            df_abstract_phase$C_sec <- as.integer(round(df_abstract_phase$split_C_Avg*df_abstract_phase$proposed),0)
            df_abstract_phase$D_sec <- as.integer(df_abstract_phase$split_D_Avg*df_abstract_phase$proposed)
            df_abstract_phase$E_sec <- as.integer(df_abstract_phase$split_E_Avg*df_abstract_phase$proposed)
            # df_abstract_phase$F_sec <- as.integer(df_abstract_phase$split_F_Avg*df_abstract_phase$proposed)
            # df_abstract_phase$G_sec <- as.integer(df_abstract_phase$split_G_Avg*df_abstract_phase$proposed)
            # df_abstract_phase$H_sec <- as.integer(df_abstract_phase$split_H_Avg*df_abstract_phase$proposed)
            # df_abstract_phase$I_sec <- as.integer(df_abstract_phase$split_I_Avg*df_abstract_phase$proposed)
          }
          if(number_phases == 6){
            df_a <- dt[dt$phases=="<A>",]
            df_b <- dt[dt$phases=="B",]
            df_c <- dt[dt$phases=="C",]
            df_d <- dt[dt$phases=="D",]
            df_e <- dt[dt$phases=="E",]
            df_f <- dt[dt$phases=="F",]
            # df_g <- dt[dt$phases=="G",]
            # df_h <- dt[dt$phases=="H",]
            # df_i <- dt[dt$phases=="I",]
            df_abstract_phase$split_A_Avg <- 0
            df_abstract_phase$split_B_Avg <- 0
            df_abstract_phase$split_C_Avg <- 0
            df_abstract_phase$split_D_Avg <- 0
            df_abstract_phase$split_E_Avg <- 0
            df_abstract_phase$split_F_Avg <- 0
            # df_abstract_phase$split_G_Avg <- 0
            # df_abstract_phase$split_H_Avg <- 0
            # df_abstract_phase$split_I_Avg <- 0
            
            for (k in 1:nrow(df_abstract_phase)-1){
              df_abstract_phase$split_A_Avg[k] <- mean(df_a$split_percentage[df_a$time >= df_abstract_phase$time[k] & df_a$time < df_abstract_phase$time[k+1]])
              df_abstract_phase$split_B_Avg[k] <- mean(df_b$split_percentage[df_b$time >= df_abstract_phase$time[k] & df_b$time < df_abstract_phase$time[k+1]])
              df_abstract_phase$split_C_Avg[k] <- mean(df_c$split_percentage[df_c$time >= df_abstract_phase$time[k] & df_c$time < df_abstract_phase$time[k+1]])
              df_abstract_phase$split_D_Avg[k] <- mean(df_d$split_percentage[df_d$time >= df_abstract_phase$time[k] & df_d$time < df_abstract_phase$time[k+1]])
              df_abstract_phase$split_E_Avg[k] <- mean(df_e$split_percentage[df_e$time >= df_abstract_phase$time[k] & df_e$time < df_abstract_phase$time[k+1]])
              df_abstract_phase$split_F_Avg[k] <- mean(df_f$split_percentage[df_f$time >= df_abstract_phase$time[k] & df_f$time < df_abstract_phase$time[k+1]])
              # df_abstract_phase$split_G_Avg[k] <- mean(df_g$split_percentage[df_g$time >= df_abstract_phase$time[k] & df_g$time < df_abstract_phase$time[k+1]])
              # df_abstract_phase$split_H_Avg[k] <- mean(df_h$split_percentage[df_h$time >= df_abstract_phase$time[k] & df_h$time < df_abstract_phase$time[k+1]])
              # df_abstract_phase$split_I_Avg[k] <- mean(df_i$split_percentage[df_i$time >= df_abstract_phase$time[k] & df_i$time < df_abstract_phase$time[k+1]])
            }
            df_abstract_phase$split_A_Avg[nrow(df_abstract_phase)] <- mean(df_a$split_percentage[df_a$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
            df_abstract_phase$split_B_Avg[nrow(df_abstract_phase)] <- mean(df_b$split_percentage[df_b$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
            df_abstract_phase$split_C_Avg[nrow(df_abstract_phase)] <- mean(df_c$split_percentage[df_c$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
            df_abstract_phase$split_D_Avg[nrow(df_abstract_phase)] <- mean(df_d$split_percentage[df_d$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
            df_abstract_phase$split_E_Avg[nrow(df_abstract_phase)] <- mean(df_e$split_percentage[df_e$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
            df_abstract_phase$split_F_Avg[nrow(df_abstract_phase)] <- mean(df_f$split_percentage[df_d$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
            # df_abstract_phase$split_G_Avg[nrow(df_abstract_phase)] <- mean(df_g$split_percentage[df_e$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
            # df_abstract_phase$split_H_Avg[nrow(df_abstract_phase)] <- mean(df_h$split_percentage[df_d$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
            # df_abstract_phase$split_I_Avg[nrow(df_abstract_phase)] <- mean(df_i$split_percentage[df_e$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
            
            df_abstract_phase$A_sec <- as.integer(round(df_abstract_phase$split_A_Avg*df_abstract_phase$proposed),0)
            df_abstract_phase$B_sec <- as.integer(round(df_abstract_phase$split_B_Avg*df_abstract_phase$proposed),0)
            df_abstract_phase$C_sec <- as.integer(round(df_abstract_phase$split_C_Avg*df_abstract_phase$proposed),0)
            df_abstract_phase$D_sec <- as.integer(df_abstract_phase$split_D_Avg*df_abstract_phase$proposed)
            df_abstract_phase$E_sec <- as.integer(df_abstract_phase$split_E_Avg*df_abstract_phase$proposed)
            df_abstract_phase$F_sec <- as.integer(df_abstract_phase$split_F_Avg*df_abstract_phase$proposed)
            # df_abstract_phase$G_sec <- as.integer(df_abstract_phase$split_G_Avg*df_abstract_phase$proposed)
            # df_abstract_phase$H_sec <- as.integer(df_abstract_phase$split_H_Avg*df_abstract_phase$proposed)
            # df_abstract_phase$I_sec <- as.integer(df_abstract_phase$split_I_Avg*df_abstract_phase$proposed)
          }
          if(number_phases == 7){
            df_a <- dt[dt$phases=="<A>",]
            df_b <- dt[dt$phases=="B",]
            df_c <- dt[dt$phases=="C",]
            df_d <- dt[dt$phases=="D",]
            df_e <- dt[dt$phases=="E",]
            df_f <- dt[dt$phases=="F",]
            df_g <- dt[dt$phases=="G",]
            # df_h <- dt[dt$phases=="H",]
            # df_i <- dt[dt$phases=="I",]
            df_abstract_phase$split_A_Avg <- 0
            df_abstract_phase$split_B_Avg <- 0
            df_abstract_phase$split_C_Avg <- 0
            df_abstract_phase$split_D_Avg <- 0
            df_abstract_phase$split_E_Avg <- 0
            df_abstract_phase$split_F_Avg <- 0
            df_abstract_phase$split_G_Avg <- 0
            # df_abstract_phase$split_H_Avg <- 0
            # df_abstract_phase$split_I_Avg <- 0
            
            for (k in 1:nrow(df_abstract_phase)-1){
              df_abstract_phase$split_A_Avg[k] <- mean(df_a$split_percentage[df_a$time >= df_abstract_phase$time[k] & df_a$time < df_abstract_phase$time[k+1]])
              df_abstract_phase$split_B_Avg[k] <- mean(df_b$split_percentage[df_b$time >= df_abstract_phase$time[k] & df_b$time < df_abstract_phase$time[k+1]])
              df_abstract_phase$split_C_Avg[k] <- mean(df_c$split_percentage[df_c$time >= df_abstract_phase$time[k] & df_c$time < df_abstract_phase$time[k+1]])
              df_abstract_phase$split_D_Avg[k] <- mean(df_d$split_percentage[df_d$time >= df_abstract_phase$time[k] & df_d$time < df_abstract_phase$time[k+1]])
              df_abstract_phase$split_E_Avg[k] <- mean(df_e$split_percentage[df_e$time >= df_abstract_phase$time[k] & df_e$time < df_abstract_phase$time[k+1]])
              df_abstract_phase$split_F_Avg[k] <- mean(df_f$split_percentage[df_f$time >= df_abstract_phase$time[k] & df_f$time < df_abstract_phase$time[k+1]])
              df_abstract_phase$split_G_Avg[k] <- mean(df_g$split_percentage[df_g$time >= df_abstract_phase$time[k] & df_g$time < df_abstract_phase$time[k+1]])
              # df_abstract_phase$split_H_Avg[k] <- mean(df_h$split_percentage[df_h$time >= df_abstract_phase$time[k] & df_h$time < df_abstract_phase$time[k+1]])
              # df_abstract_phase$split_I_Avg[k] <- mean(df_i$split_percentage[df_i$time >= df_abstract_phase$time[k] & df_i$time < df_abstract_phase$time[k+1]])
            }
            df_abstract_phase$split_A_Avg[nrow(df_abstract_phase)] <- mean(df_a$split_percentage[df_a$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
            df_abstract_phase$split_B_Avg[nrow(df_abstract_phase)] <- mean(df_b$split_percentage[df_b$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
            df_abstract_phase$split_C_Avg[nrow(df_abstract_phase)] <- mean(df_c$split_percentage[df_c$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
            df_abstract_phase$split_D_Avg[nrow(df_abstract_phase)] <- mean(df_d$split_percentage[df_d$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
            df_abstract_phase$split_E_Avg[nrow(df_abstract_phase)] <- mean(df_e$split_percentage[df_e$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
            df_abstract_phase$split_F_Avg[nrow(df_abstract_phase)] <- mean(df_f$split_percentage[df_d$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
            df_abstract_phase$split_G_Avg[nrow(df_abstract_phase)] <- mean(df_g$split_percentage[df_e$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
            # df_abstract_phase$split_H_Avg[nrow(df_abstract_phase)] <- mean(df_h$split_percentage[df_d$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
            # df_abstract_phase$split_I_Avg[nrow(df_abstract_phase)] <- mean(df_i$split_percentage[df_e$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
            
            df_abstract_phase$A_sec <- as.integer(round(df_abstract_phase$split_A_Avg*df_abstract_phase$proposed),0)
            df_abstract_phase$B_sec <- as.integer(round(df_abstract_phase$split_B_Avg*df_abstract_phase$proposed),0)
            df_abstract_phase$C_sec <- as.integer(round(df_abstract_phase$split_C_Avg*df_abstract_phase$proposed),0)
            df_abstract_phase$D_sec <- as.integer(df_abstract_phase$split_D_Avg*df_abstract_phase$proposed)
            df_abstract_phase$E_sec <- as.integer(df_abstract_phase$split_E_Avg*df_abstract_phase$proposed)
            df_abstract_phase$F_sec <- as.integer(df_abstract_phase$split_F_Avg*df_abstract_phase$proposed)
            df_abstract_phase$G_sec <- as.integer(df_abstract_phase$split_G_Avg*df_abstract_phase$proposed)
            # df_abstract_phase$H_sec <- as.integer(df_abstract_phase$split_H_Avg*df_abstract_phase$proposed)
            # df_abstract_phase$I_sec <- as.integer(df_abstract_phase$split_I_Avg*df_abstract_phase$proposed)
          }
          if(number_phases == 8){
            df_a <- dt[dt$phases=="<A>",]
            df_b <- dt[dt$phases=="B",]
            df_c <- dt[dt$phases=="C",]
            df_d <- dt[dt$phases=="D",]
            df_e <- dt[dt$phases=="E",]
            df_f <- dt[dt$phases=="F",]
            df_g <- dt[dt$phases=="G",]
            df_h <- dt[dt$phases=="H",]
            # df_i <- dt[dt$phases=="I",]
            df_abstract_phase$split_A_Avg <- 0
            df_abstract_phase$split_B_Avg <- 0
            df_abstract_phase$split_C_Avg <- 0
            df_abstract_phase$split_D_Avg <- 0
            df_abstract_phase$split_E_Avg <- 0
            df_abstract_phase$split_F_Avg <- 0
            df_abstract_phase$split_G_Avg <- 0
            df_abstract_phase$split_H_Avg <- 0
            # df_abstract_phase$split_I_Avg <- 0
            
            for (k in 1:nrow(df_abstract_phase)-1){
              df_abstract_phase$split_A_Avg[k] <- mean(df_a$split_percentage[df_a$time >= df_abstract_phase$time[k] & df_a$time < df_abstract_phase$time[k+1]])
              df_abstract_phase$split_B_Avg[k] <- mean(df_b$split_percentage[df_b$time >= df_abstract_phase$time[k] & df_b$time < df_abstract_phase$time[k+1]])
              df_abstract_phase$split_C_Avg[k] <- mean(df_c$split_percentage[df_c$time >= df_abstract_phase$time[k] & df_c$time < df_abstract_phase$time[k+1]])
              df_abstract_phase$split_D_Avg[k] <- mean(df_d$split_percentage[df_d$time >= df_abstract_phase$time[k] & df_d$time < df_abstract_phase$time[k+1]])
              df_abstract_phase$split_E_Avg[k] <- mean(df_e$split_percentage[df_e$time >= df_abstract_phase$time[k] & df_e$time < df_abstract_phase$time[k+1]])
              df_abstract_phase$split_F_Avg[k] <- mean(df_f$split_percentage[df_f$time >= df_abstract_phase$time[k] & df_f$time < df_abstract_phase$time[k+1]])
              df_abstract_phase$split_G_Avg[k] <- mean(df_g$split_percentage[df_g$time >= df_abstract_phase$time[k] & df_g$time < df_abstract_phase$time[k+1]])
              df_abstract_phase$split_H_Avg[k] <- mean(df_h$split_percentage[df_h$time >= df_abstract_phase$time[k] & df_h$time < df_abstract_phase$time[k+1]])
              # df_abstract_phase$split_I_Avg[k] <- mean(df_i$split_percentage[df_i$time >= df_abstract_phase$time[k] & df_i$time < df_abstract_phase$time[k+1]])
            }
            df_abstract_phase$split_A_Avg[nrow(df_abstract_phase)] <- mean(df_a$split_percentage[df_a$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
            df_abstract_phase$split_B_Avg[nrow(df_abstract_phase)] <- mean(df_b$split_percentage[df_b$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
            df_abstract_phase$split_C_Avg[nrow(df_abstract_phase)] <- mean(df_c$split_percentage[df_c$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
            df_abstract_phase$split_D_Avg[nrow(df_abstract_phase)] <- mean(df_d$split_percentage[df_d$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
            df_abstract_phase$split_E_Avg[nrow(df_abstract_phase)] <- mean(df_e$split_percentage[df_e$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
            df_abstract_phase$split_F_Avg[nrow(df_abstract_phase)] <- mean(df_f$split_percentage[df_d$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
            df_abstract_phase$split_G_Avg[nrow(df_abstract_phase)] <- mean(df_g$split_percentage[df_e$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
            df_abstract_phase$split_H_Avg[nrow(df_abstract_phase)] <- mean(df_h$split_percentage[df_d$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
            # df_abstract_phase$split_I_Avg[nrow(df_abstract_phase)] <- mean(df_i$split_percentage[df_e$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
            
            df_abstract_phase$A_sec <- as.integer(round(df_abstract_phase$split_A_Avg*df_abstract_phase$proposed),0)
            df_abstract_phase$B_sec <- as.integer(round(df_abstract_phase$split_B_Avg*df_abstract_phase$proposed),0)
            df_abstract_phase$C_sec <- as.integer(round(df_abstract_phase$split_C_Avg*df_abstract_phase$proposed),0)
            df_abstract_phase$D_sec <- as.integer(df_abstract_phase$split_D_Avg*df_abstract_phase$proposed)
            df_abstract_phase$E_sec <- as.integer(df_abstract_phase$split_E_Avg*df_abstract_phase$proposed)
            df_abstract_phase$F_sec <- as.integer(df_abstract_phase$split_F_Avg*df_abstract_phase$proposed)
            df_abstract_phase$G_sec <- as.integer(df_abstract_phase$split_G_Avg*df_abstract_phase$proposed)
            df_abstract_phase$H_sec <- as.integer(df_abstract_phase$split_H_Avg*df_abstract_phase$proposed)
            # df_abstract_phase$I_sec <- as.integer(df_abstract_phase$split_I_Avg*df_abstract_phase$proposed)
          }
          if(number_phases == 9){
            df_a <- dt[dt$phases=="<A>",]
            df_b <- dt[dt$phases=="B",]
            df_c <- dt[dt$phases=="C",]
            df_d <- dt[dt$phases=="D",]
            df_e <- dt[dt$phases=="E",]
            df_f <- dt[dt$phases=="F",]
            df_g <- dt[dt$phases=="G",]
            df_h <- dt[dt$phases=="H",]
            df_i <- dt[dt$phases=="I",]
            df_abstract_phase$split_A_Avg <- 0
            df_abstract_phase$split_B_Avg <- 0
            df_abstract_phase$split_C_Avg <- 0
            df_abstract_phase$split_D_Avg <- 0
            df_abstract_phase$split_E_Avg <- 0
            df_abstract_phase$split_F_Avg <- 0
            df_abstract_phase$split_G_Avg <- 0
            df_abstract_phase$split_H_Avg <- 0
            df_abstract_phase$split_I_Avg <- 0
            
            for (k in 1:nrow(df_abstract_phase)-1){
              df_abstract_phase$split_A_Avg[k] <- mean(df_a$split_percentage[df_a$time >= df_abstract_phase$time[k] & df_a$time < df_abstract_phase$time[k+1]])
              df_abstract_phase$split_B_Avg[k] <- mean(df_b$split_percentage[df_b$time >= df_abstract_phase$time[k] & df_b$time < df_abstract_phase$time[k+1]])
              df_abstract_phase$split_C_Avg[k] <- mean(df_c$split_percentage[df_c$time >= df_abstract_phase$time[k] & df_c$time < df_abstract_phase$time[k+1]])
              df_abstract_phase$split_D_Avg[k] <- mean(df_d$split_percentage[df_d$time >= df_abstract_phase$time[k] & df_d$time < df_abstract_phase$time[k+1]])
              df_abstract_phase$split_E_Avg[k] <- mean(df_e$split_percentage[df_e$time >= df_abstract_phase$time[k] & df_e$time < df_abstract_phase$time[k+1]])
              df_abstract_phase$split_F_Avg[k] <- mean(df_f$split_percentage[df_f$time >= df_abstract_phase$time[k] & df_f$time < df_abstract_phase$time[k+1]])
              df_abstract_phase$split_G_Avg[k] <- mean(df_g$split_percentage[df_g$time >= df_abstract_phase$time[k] & df_g$time < df_abstract_phase$time[k+1]])
              df_abstract_phase$split_H_Avg[k] <- mean(df_h$split_percentage[df_h$time >= df_abstract_phase$time[k] & df_h$time < df_abstract_phase$time[k+1]])
              df_abstract_phase$split_I_Avg[k] <- mean(df_i$split_percentage[df_i$time >= df_abstract_phase$time[k] & df_i$time < df_abstract_phase$time[k+1]])
            }
            df_abstract_phase$split_A_Avg[nrow(df_abstract_phase)] <- mean(df_a$split_percentage[df_a$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
            df_abstract_phase$split_B_Avg[nrow(df_abstract_phase)] <- mean(df_b$split_percentage[df_b$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
            df_abstract_phase$split_C_Avg[nrow(df_abstract_phase)] <- mean(df_c$split_percentage[df_c$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
            df_abstract_phase$split_D_Avg[nrow(df_abstract_phase)] <- mean(df_d$split_percentage[df_d$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
            df_abstract_phase$split_E_Avg[nrow(df_abstract_phase)] <- mean(df_e$split_percentage[df_e$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
            df_abstract_phase$split_F_Avg[nrow(df_abstract_phase)] <- mean(df_f$split_percentage[df_d$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
            df_abstract_phase$split_G_Avg[nrow(df_abstract_phase)] <- mean(df_g$split_percentage[df_e$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
            df_abstract_phase$split_H_Avg[nrow(df_abstract_phase)] <- mean(df_h$split_percentage[df_d$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
            df_abstract_phase$split_I_Avg[nrow(df_abstract_phase)] <- mean(df_i$split_percentage[df_e$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
            
            df_abstract_phase$A_sec <- as.integer(round(df_abstract_phase$split_A_Avg*df_abstract_phase$proposed),0)
            df_abstract_phase$B_sec <- as.integer(round(df_abstract_phase$split_B_Avg*df_abstract_phase$proposed),0)
            df_abstract_phase$C_sec <- as.integer(round(df_abstract_phase$split_C_Avg*df_abstract_phase$proposed),0)
            df_abstract_phase$D_sec <- as.integer(df_abstract_phase$split_D_Avg*df_abstract_phase$proposed)
            df_abstract_phase$E_sec <- as.integer(df_abstract_phase$split_E_Avg*df_abstract_phase$proposed)
            df_abstract_phase$F_sec <- as.integer(df_abstract_phase$split_F_Avg*df_abstract_phase$proposed)
            df_abstract_phase$G_sec <- as.integer(df_abstract_phase$split_G_Avg*df_abstract_phase$proposed)
            df_abstract_phase$H_sec <- as.integer(df_abstract_phase$split_H_Avg*df_abstract_phase$proposed)
            df_abstract_phase$I_sec <- as.integer(df_abstract_phase$split_I_Avg*df_abstract_phase$proposed)
          }
          
          df_abstract_phase$time <- hms::as_hms(df_abstract_phase$time)
          write.csv(df_abstract_phase, file8_2, row.names=FALSE)
      # 
    }
  )
  
  output$downloadData8_4 <- downloadHandler(
    
    filename = function() {
      paste("TODPhase_",input$site_historical_8, Sys.time(), ".csv", sep="")
    },
    content = function(file8_4) {
      progress <- shiny::Progress$new()
      # Make sure it closes when we exit this reactive, even if there's an error
      on.exit(progress$close())
      
      progress$set(message = "Analyizing the data....", value = 0)
      #   # content = function(file8_3) { 
      print(input$date8_1_multiple)
      
      source("https://raw.githubusercontent.com/abdallahshabarek/ITS_NJDOT/main/Scripts/TOD_phase_optimization_from_historical_dates_function.R")
      # dirname(input$file8_2$datapath)[1]
      dates_list2_file8 <- as.data.frame(input$date8_1_multiple)
      colnames(dates_list2_file8) <- c("dates")
      dates_list3_file8 <- as.list(dates_list2_file8$dates)
      dt <- TOD_phase_optimization_from_historical_dates(site=as.integer(input$site_historical_8),
                                                         dates_list = dates_list3_file8,
                                              analysis_time_type=input$file_8_time_type)
      df_abstract_phase <- readr::read_csv(input$file8_4$datapath)
      df_abstract_phase$time <- as.integer(df_abstract_phase$time)
      number_phases <- as.integer(nrow(data.frame(unique(dt$phases))))
      
      if(number_phases == 2){
        df_a <- dt[dt$phases=="<A>",]
        df_b <- dt[dt$phases=="B",]
        # df_c <- dt[dt$phases=="C",]
        # df_d <- dt[dt$phases=="D",]
        # df_e <- dt[dt$phases=="E",]
        # df_f <- dt[dt$phases=="F",]
        # df_f <- dt[dt$phases=="G",]
        # df_f <- dt[dt$phases=="H",]
        # df_f <- dt[dt$phases=="I",]
        df_abstract_phase$split_A_Avg <- 0
        df_abstract_phase$split_B_Avg <- 0
        # df_abstract_phase$split_C_Avg <- 0
        # df_abstract_phase$split_D_Avg <- 0
        # df_abstract_phase$split_E_Avg <- 0
        # df_abstract_phase$split_F_Avg <- 0
        # df_abstract_phase$split_G_Avg <- 0
        # df_abstract_phase$split_H_Avg <- 0
        # df_abstract_phase$split_I_Avg <- 0
        
        for (k in 1:nrow(df_abstract_phase)-1){
          df_abstract_phase$split_A_Avg[k] <- mean(df_a$split_percentage[df_a$time >= df_abstract_phase$time[k] & df_a$time < df_abstract_phase$time[k+1]])
          df_abstract_phase$split_B_Avg[k] <- mean(df_b$split_percentage[df_b$time >= df_abstract_phase$time[k] & df_b$time < df_abstract_phase$time[k+1]])
          # df_abstract_phase$split_C_Avg[k] <- mean(df_c$split_percentage[df_c$time >= df_abstract_phase$time[k] & df_c$time < df_abstract_phase$time[k+1]])
          # df_abstract_phase$split_D_Avg[k] <- mean(df_d$split_percentage[df_d$time >= df_abstract_phase$time[k] & df_d$time < df_abstract_phase$time[k+1]])
          # df_abstract_phase$split_E_Avg[k] <- mean(df_e$split_percentage[df_e$time >= df_abstract_phase$time[k] & df_e$time < df_abstract_phase$time[k+1]])
          # df_abstract_phase$split_F_Avg[k] <- mean(df_f$split_percentage[df_f$time >= df_abstract_phase$time[k] & df_f$time < df_abstract_phase$time[k+1]])
          # df_abstract_phase$split_G_Avg[k] <- mean(df_g$split_percentage[df_g$time >= df_abstract_phase$time[k] & df_g$time < df_abstract_phase$time[k+1]])
          # df_abstract_phase$split_H_Avg[k] <- mean(df_h$split_percentage[df_h$time >= df_abstract_phase$time[k] & df_h$time < df_abstract_phase$time[k+1]])
          # df_abstract_phase$split_I_Avg[k] <- mean(df_i$split_percentage[df_i$time >= df_abstract_phase$time[k] & df_i$time < df_abstract_phase$time[k+1]])
        }
        df_abstract_phase$split_A_Avg[nrow(df_abstract_phase)] <- mean(df_a$split_percentage[df_a$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
        df_abstract_phase$split_B_Avg[nrow(df_abstract_phase)] <- mean(df_b$split_percentage[df_b$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
        # df_abstract_phase$split_C_Avg[nrow(df_abstract_phase)] <- mean(df_c$split_percentage[df_c$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
        # df_abstract_phase$split_D_Avg[nrow(df_abstract_phase)] <- mean(df_d$split_percentage[df_d$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
        # df_abstract_phase$split_E_Avg[nrow(df_abstract_phase)] <- mean(df_e$split_percentage[df_e$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
        # df_abstract_phase$split_F_Avg[nrow(df_abstract_phase)] <- mean(df_f$split_percentage[df_d$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
        # df_abstract_phase$split_G_Avg[nrow(df_abstract_phase)] <- mean(df_g$split_percentage[df_e$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
        # df_abstract_phase$split_H_Avg[nrow(df_abstract_phase)] <- mean(df_h$split_percentage[df_d$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
        # df_abstract_phase$split_I_Avg[nrow(df_abstract_phase)] <- mean(df_i$split_percentage[df_e$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
        
        df_abstract_phase$A_sec <- as.integer(round(df_abstract_phase$split_A_Avg*df_abstract_phase$proposed),0)
        df_abstract_phase$B_sec <- as.integer(round(df_abstract_phase$split_B_Avg*df_abstract_phase$proposed),0)
        # df_abstract_phase$C_sec <- as.integer(round(df_abstract_phase$split_C_Avg*df_abstract_phase$proposed),0)
        # df_abstract_phase$D_sec <- as.integer(df_abstract_phase$split_D_Avg*df_abstract_phase$proposed)
        # df_abstract_phase$E_sec <- as.integer(df_abstract_phase$split_E_Avg*df_abstract_phase$proposed)
        # df_abstract_phase$F_sec <- as.integer(df_abstract_phase$split_F_Avg*df_abstract_phase$proposed)
        # df_abstract_phase$G_sec <- as.integer(df_abstract_phase$split_G_Avg*df_abstract_phase$proposed)
        # df_abstract_phase$H_sec <- as.integer(df_abstract_phase$split_H_Avg*df_abstract_phase$proposed)
        # df_abstract_phase$I_sec <- as.integer(df_abstract_phase$split_I_Avg*df_abstract_phase$proposed)
      }
      if(number_phases == 3){
        df_a <- dt[dt$phases=="<A>",]
        df_b <- dt[dt$phases=="B",]
        df_c <- dt[dt$phases=="C",]
        # df_d <- dt[dt$phases=="D",]
        # df_e <- dt[dt$phases=="E",]
        # df_f <- dt[dt$phases=="F",]
        # df_f <- dt[dt$phases=="G",]
        # df_f <- dt[dt$phases=="H",]
        # df_f <- dt[dt$phases=="I",]
        df_abstract_phase$split_A_Avg <- 0
        df_abstract_phase$split_B_Avg <- 0
        df_abstract_phase$split_C_Avg <- 0
        # df_abstract_phase$split_D_Avg <- 0
        # df_abstract_phase$split_E_Avg <- 0
        # df_abstract_phase$split_F_Avg <- 0
        # df_abstract_phase$split_G_Avg <- 0
        # df_abstract_phase$split_H_Avg <- 0
        # df_abstract_phase$split_I_Avg <- 0
        
        for (k in 1:nrow(df_abstract_phase)-1){
          df_abstract_phase$split_A_Avg[k] <- mean(df_a$split_percentage[df_a$time >= df_abstract_phase$time[k] & df_a$time < df_abstract_phase$time[k+1]])
          df_abstract_phase$split_B_Avg[k] <- mean(df_b$split_percentage[df_b$time >= df_abstract_phase$time[k] & df_b$time < df_abstract_phase$time[k+1]])
          df_abstract_phase$split_C_Avg[k] <- mean(df_c$split_percentage[df_c$time >= df_abstract_phase$time[k] & df_c$time < df_abstract_phase$time[k+1]])
          # df_abstract_phase$split_D_Avg[k] <- mean(df_d$split_percentage[df_d$time >= df_abstract_phase$time[k] & df_d$time < df_abstract_phase$time[k+1]])
          # df_abstract_phase$split_E_Avg[k] <- mean(df_e$split_percentage[df_e$time >= df_abstract_phase$time[k] & df_e$time < df_abstract_phase$time[k+1]])
          # df_abstract_phase$split_F_Avg[k] <- mean(df_f$split_percentage[df_f$time >= df_abstract_phase$time[k] & df_f$time < df_abstract_phase$time[k+1]])
          # df_abstract_phase$split_G_Avg[k] <- mean(df_g$split_percentage[df_g$time >= df_abstract_phase$time[k] & df_g$time < df_abstract_phase$time[k+1]])
          # df_abstract_phase$split_H_Avg[k] <- mean(df_h$split_percentage[df_h$time >= df_abstract_phase$time[k] & df_h$time < df_abstract_phase$time[k+1]])
          # df_abstract_phase$split_I_Avg[k] <- mean(df_i$split_percentage[df_i$time >= df_abstract_phase$time[k] & df_i$time < df_abstract_phase$time[k+1]])
        }
        df_abstract_phase$split_A_Avg[nrow(df_abstract_phase)] <- mean(df_a$split_percentage[df_a$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
        df_abstract_phase$split_B_Avg[nrow(df_abstract_phase)] <- mean(df_b$split_percentage[df_b$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
        df_abstract_phase$split_C_Avg[nrow(df_abstract_phase)] <- mean(df_c$split_percentage[df_c$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
        # df_abstract_phase$split_D_Avg[nrow(df_abstract_phase)] <- mean(df_d$split_percentage[df_d$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
        # df_abstract_phase$split_E_Avg[nrow(df_abstract_phase)] <- mean(df_e$split_percentage[df_e$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
        # df_abstract_phase$split_F_Avg[nrow(df_abstract_phase)] <- mean(df_f$split_percentage[df_d$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
        # df_abstract_phase$split_G_Avg[nrow(df_abstract_phase)] <- mean(df_g$split_percentage[df_e$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
        # df_abstract_phase$split_H_Avg[nrow(df_abstract_phase)] <- mean(df_h$split_percentage[df_d$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
        # df_abstract_phase$split_I_Avg[nrow(df_abstract_phase)] <- mean(df_i$split_percentage[df_e$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
        
        df_abstract_phase$A_sec <- as.integer(round(df_abstract_phase$split_A_Avg*df_abstract_phase$proposed),0)
        df_abstract_phase$B_sec <- as.integer(round(df_abstract_phase$split_B_Avg*df_abstract_phase$proposed),0)
        df_abstract_phase$C_sec <- as.integer(round(df_abstract_phase$split_C_Avg*df_abstract_phase$proposed),0)
        # df_abstract_phase$D_sec <- as.integer(df_abstract_phase$split_D_Avg*df_abstract_phase$proposed)
        # df_abstract_phase$E_sec <- as.integer(df_abstract_phase$split_E_Avg*df_abstract_phase$proposed)
        # df_abstract_phase$F_sec <- as.integer(df_abstract_phase$split_F_Avg*df_abstract_phase$proposed)
        # df_abstract_phase$G_sec <- as.integer(df_abstract_phase$split_G_Avg*df_abstract_phase$proposed)
        # df_abstract_phase$H_sec <- as.integer(df_abstract_phase$split_H_Avg*df_abstract_phase$proposed)
        # df_abstract_phase$I_sec <- as.integer(df_abstract_phase$split_I_Avg*df_abstract_phase$proposed)
      }
      if(number_phases == 4){
        df_a <- dt[dt$phases=="<A>",]
        df_b <- dt[dt$phases=="B",]
        df_c <- dt[dt$phases=="C",]
        df_d <- dt[dt$phases=="D",]
        # df_e <- dt[dt$phases=="E",]
        # df_f <- dt[dt$phases=="F",]
        # df_f <- dt[dt$phases=="G",]
        # df_f <- dt[dt$phases=="H",]
        # df_f <- dt[dt$phases=="I",]
        df_abstract_phase$split_A_Avg <- 0
        df_abstract_phase$split_B_Avg <- 0
        df_abstract_phase$split_C_Avg <- 0
        df_abstract_phase$split_D_Avg <- 0
        # df_abstract_phase$split_E_Avg <- 0
        # df_abstract_phase$split_F_Avg <- 0
        # df_abstract_phase$split_G_Avg <- 0
        # df_abstract_phase$split_H_Avg <- 0
        # df_abstract_phase$split_I_Avg <- 0
        
        for (k in 1:nrow(df_abstract_phase)-1){
          df_abstract_phase$split_A_Avg[k] <- mean(df_a$split_percentage[df_a$time >= df_abstract_phase$time[k] & df_a$time < df_abstract_phase$time[k+1]])
          df_abstract_phase$split_B_Avg[k] <- mean(df_b$split_percentage[df_b$time >= df_abstract_phase$time[k] & df_b$time < df_abstract_phase$time[k+1]])
          df_abstract_phase$split_C_Avg[k] <- mean(df_c$split_percentage[df_c$time >= df_abstract_phase$time[k] & df_c$time < df_abstract_phase$time[k+1]])
          df_abstract_phase$split_D_Avg[k] <- mean(df_d$split_percentage[df_d$time >= df_abstract_phase$time[k] & df_d$time < df_abstract_phase$time[k+1]])
          # df_abstract_phase$split_E_Avg[k] <- mean(df_e$split_percentage[df_e$time >= df_abstract_phase$time[k] & df_e$time < df_abstract_phase$time[k+1]])
          # df_abstract_phase$split_F_Avg[k] <- mean(df_f$split_percentage[df_f$time >= df_abstract_phase$time[k] & df_f$time < df_abstract_phase$time[k+1]])
          # df_abstract_phase$split_G_Avg[k] <- mean(df_g$split_percentage[df_g$time >= df_abstract_phase$time[k] & df_g$time < df_abstract_phase$time[k+1]])
          # df_abstract_phase$split_H_Avg[k] <- mean(df_h$split_percentage[df_h$time >= df_abstract_phase$time[k] & df_h$time < df_abstract_phase$time[k+1]])
          # df_abstract_phase$split_I_Avg[k] <- mean(df_i$split_percentage[df_i$time >= df_abstract_phase$time[k] & df_i$time < df_abstract_phase$time[k+1]])
        }
        df_abstract_phase$split_A_Avg[nrow(df_abstract_phase)] <- mean(df_a$split_percentage[df_a$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
        df_abstract_phase$split_B_Avg[nrow(df_abstract_phase)] <- mean(df_b$split_percentage[df_b$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
        df_abstract_phase$split_C_Avg[nrow(df_abstract_phase)] <- mean(df_c$split_percentage[df_c$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
        df_abstract_phase$split_D_Avg[nrow(df_abstract_phase)] <- mean(df_d$split_percentage[df_d$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
        # df_abstract_phase$split_E_Avg[nrow(df_abstract_phase)] <- mean(df_e$split_percentage[df_e$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
        # df_abstract_phase$split_F_Avg[nrow(df_abstract_phase)] <- mean(df_f$split_percentage[df_d$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
        # df_abstract_phase$split_G_Avg[nrow(df_abstract_phase)] <- mean(df_g$split_percentage[df_e$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
        # df_abstract_phase$split_H_Avg[nrow(df_abstract_phase)] <- mean(df_h$split_percentage[df_d$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
        # df_abstract_phase$split_I_Avg[nrow(df_abstract_phase)] <- mean(df_i$split_percentage[df_e$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
        
        df_abstract_phase$A_sec <- as.integer(round(df_abstract_phase$split_A_Avg*df_abstract_phase$proposed),0)
        df_abstract_phase$B_sec <- as.integer(round(df_abstract_phase$split_B_Avg*df_abstract_phase$proposed),0)
        df_abstract_phase$C_sec <- as.integer(round(df_abstract_phase$split_C_Avg*df_abstract_phase$proposed),0)
        df_abstract_phase$D_sec <- as.integer(df_abstract_phase$split_D_Avg*df_abstract_phase$proposed)
        # df_abstract_phase$E_sec <- as.integer(df_abstract_phase$split_E_Avg*df_abstract_phase$proposed)
        # df_abstract_phase$F_sec <- as.integer(df_abstract_phase$split_F_Avg*df_abstract_phase$proposed)
        # df_abstract_phase$G_sec <- as.integer(df_abstract_phase$split_G_Avg*df_abstract_phase$proposed)
        # df_abstract_phase$H_sec <- as.integer(df_abstract_phase$split_H_Avg*df_abstract_phase$proposed)
        # df_abstract_phase$I_sec <- as.integer(df_abstract_phase$split_I_Avg*df_abstract_phase$proposed)
      }
      if(number_phases == 5){
        df_a <- dt[dt$phases=="<A>",]
        df_b <- dt[dt$phases=="B",]
        df_c <- dt[dt$phases=="C",]
        df_d <- dt[dt$phases=="D",]
        df_e <- dt[dt$phases=="E",]
        # df_f <- dt[dt$phases=="F",]
        # df_f <- dt[dt$phases=="G",]
        # df_f <- dt[dt$phases=="H",]
        # df_f <- dt[dt$phases=="I",]
        df_abstract_phase$split_A_Avg <- 0
        df_abstract_phase$split_B_Avg <- 0
        df_abstract_phase$split_C_Avg <- 0
        df_abstract_phase$split_D_Avg <- 0
        df_abstract_phase$split_E_Avg <- 0
        # df_abstract_phase$split_F_Avg <- 0
        # df_abstract_phase$split_G_Avg <- 0
        # df_abstract_phase$split_H_Avg <- 0
        # df_abstract_phase$split_I_Avg <- 0
        
        for (k in 1:nrow(df_abstract_phase)-1){
          df_abstract_phase$split_A_Avg[k] <- mean(df_a$split_percentage[df_a$time >= df_abstract_phase$time[k] & df_a$time < df_abstract_phase$time[k+1]])
          df_abstract_phase$split_B_Avg[k] <- mean(df_b$split_percentage[df_b$time >= df_abstract_phase$time[k] & df_b$time < df_abstract_phase$time[k+1]])
          df_abstract_phase$split_C_Avg[k] <- mean(df_c$split_percentage[df_c$time >= df_abstract_phase$time[k] & df_c$time < df_abstract_phase$time[k+1]])
          df_abstract_phase$split_D_Avg[k] <- mean(df_d$split_percentage[df_d$time >= df_abstract_phase$time[k] & df_d$time < df_abstract_phase$time[k+1]])
          df_abstract_phase$split_E_Avg[k] <- mean(df_e$split_percentage[df_e$time >= df_abstract_phase$time[k] & df_e$time < df_abstract_phase$time[k+1]])
          # df_abstract_phase$split_F_Avg[k] <- mean(df_f$split_percentage[df_f$time >= df_abstract_phase$time[k] & df_f$time < df_abstract_phase$time[k+1]])
          # df_abstract_phase$split_G_Avg[k] <- mean(df_g$split_percentage[df_g$time >= df_abstract_phase$time[k] & df_g$time < df_abstract_phase$time[k+1]])
          # df_abstract_phase$split_H_Avg[k] <- mean(df_h$split_percentage[df_h$time >= df_abstract_phase$time[k] & df_h$time < df_abstract_phase$time[k+1]])
          # df_abstract_phase$split_I_Avg[k] <- mean(df_i$split_percentage[df_i$time >= df_abstract_phase$time[k] & df_i$time < df_abstract_phase$time[k+1]])
        }
        df_abstract_phase$split_A_Avg[nrow(df_abstract_phase)] <- mean(df_a$split_percentage[df_a$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
        df_abstract_phase$split_B_Avg[nrow(df_abstract_phase)] <- mean(df_b$split_percentage[df_b$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
        df_abstract_phase$split_C_Avg[nrow(df_abstract_phase)] <- mean(df_c$split_percentage[df_c$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
        df_abstract_phase$split_D_Avg[nrow(df_abstract_phase)] <- mean(df_d$split_percentage[df_d$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
        df_abstract_phase$split_E_Avg[nrow(df_abstract_phase)] <- mean(df_e$split_percentage[df_e$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
        # df_abstract_phase$split_F_Avg[nrow(df_abstract_phase)] <- mean(df_f$split_percentage[df_d$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
        # df_abstract_phase$split_G_Avg[nrow(df_abstract_phase)] <- mean(df_g$split_percentage[df_e$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
        # df_abstract_phase$split_H_Avg[nrow(df_abstract_phase)] <- mean(df_h$split_percentage[df_d$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
        # df_abstract_phase$split_I_Avg[nrow(df_abstract_phase)] <- mean(df_i$split_percentage[df_e$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
        
        df_abstract_phase$A_sec <- as.integer(round(df_abstract_phase$split_A_Avg*df_abstract_phase$proposed),0)
        df_abstract_phase$B_sec <- as.integer(round(df_abstract_phase$split_B_Avg*df_abstract_phase$proposed),0)
        df_abstract_phase$C_sec <- as.integer(round(df_abstract_phase$split_C_Avg*df_abstract_phase$proposed),0)
        df_abstract_phase$D_sec <- as.integer(df_abstract_phase$split_D_Avg*df_abstract_phase$proposed)
        df_abstract_phase$E_sec <- as.integer(df_abstract_phase$split_E_Avg*df_abstract_phase$proposed)
        # df_abstract_phase$F_sec <- as.integer(df_abstract_phase$split_F_Avg*df_abstract_phase$proposed)
        # df_abstract_phase$G_sec <- as.integer(df_abstract_phase$split_G_Avg*df_abstract_phase$proposed)
        # df_abstract_phase$H_sec <- as.integer(df_abstract_phase$split_H_Avg*df_abstract_phase$proposed)
        # df_abstract_phase$I_sec <- as.integer(df_abstract_phase$split_I_Avg*df_abstract_phase$proposed)
      }
      if(number_phases == 6){
        df_a <- dt[dt$phases=="<A>",]
        df_b <- dt[dt$phases=="B",]
        df_c <- dt[dt$phases=="C",]
        df_d <- dt[dt$phases=="D",]
        df_e <- dt[dt$phases=="E",]
        df_f <- dt[dt$phases=="F",]
        # df_g <- dt[dt$phases=="G",]
        # df_h <- dt[dt$phases=="H",]
        # df_i <- dt[dt$phases=="I",]
        df_abstract_phase$split_A_Avg <- 0
        df_abstract_phase$split_B_Avg <- 0
        df_abstract_phase$split_C_Avg <- 0
        df_abstract_phase$split_D_Avg <- 0
        df_abstract_phase$split_E_Avg <- 0
        df_abstract_phase$split_F_Avg <- 0
        # df_abstract_phase$split_G_Avg <- 0
        # df_abstract_phase$split_H_Avg <- 0
        # df_abstract_phase$split_I_Avg <- 0
        
        for (k in 1:nrow(df_abstract_phase)-1){
          df_abstract_phase$split_A_Avg[k] <- mean(df_a$split_percentage[df_a$time >= df_abstract_phase$time[k] & df_a$time < df_abstract_phase$time[k+1]])
          df_abstract_phase$split_B_Avg[k] <- mean(df_b$split_percentage[df_b$time >= df_abstract_phase$time[k] & df_b$time < df_abstract_phase$time[k+1]])
          df_abstract_phase$split_C_Avg[k] <- mean(df_c$split_percentage[df_c$time >= df_abstract_phase$time[k] & df_c$time < df_abstract_phase$time[k+1]])
          df_abstract_phase$split_D_Avg[k] <- mean(df_d$split_percentage[df_d$time >= df_abstract_phase$time[k] & df_d$time < df_abstract_phase$time[k+1]])
          df_abstract_phase$split_E_Avg[k] <- mean(df_e$split_percentage[df_e$time >= df_abstract_phase$time[k] & df_e$time < df_abstract_phase$time[k+1]])
          df_abstract_phase$split_F_Avg[k] <- mean(df_f$split_percentage[df_f$time >= df_abstract_phase$time[k] & df_f$time < df_abstract_phase$time[k+1]])
          # df_abstract_phase$split_G_Avg[k] <- mean(df_g$split_percentage[df_g$time >= df_abstract_phase$time[k] & df_g$time < df_abstract_phase$time[k+1]])
          # df_abstract_phase$split_H_Avg[k] <- mean(df_h$split_percentage[df_h$time >= df_abstract_phase$time[k] & df_h$time < df_abstract_phase$time[k+1]])
          # df_abstract_phase$split_I_Avg[k] <- mean(df_i$split_percentage[df_i$time >= df_abstract_phase$time[k] & df_i$time < df_abstract_phase$time[k+1]])
        }
        df_abstract_phase$split_A_Avg[nrow(df_abstract_phase)] <- mean(df_a$split_percentage[df_a$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
        df_abstract_phase$split_B_Avg[nrow(df_abstract_phase)] <- mean(df_b$split_percentage[df_b$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
        df_abstract_phase$split_C_Avg[nrow(df_abstract_phase)] <- mean(df_c$split_percentage[df_c$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
        df_abstract_phase$split_D_Avg[nrow(df_abstract_phase)] <- mean(df_d$split_percentage[df_d$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
        df_abstract_phase$split_E_Avg[nrow(df_abstract_phase)] <- mean(df_e$split_percentage[df_e$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
        df_abstract_phase$split_F_Avg[nrow(df_abstract_phase)] <- mean(df_f$split_percentage[df_d$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
        # df_abstract_phase$split_G_Avg[nrow(df_abstract_phase)] <- mean(df_g$split_percentage[df_e$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
        # df_abstract_phase$split_H_Avg[nrow(df_abstract_phase)] <- mean(df_h$split_percentage[df_d$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
        # df_abstract_phase$split_I_Avg[nrow(df_abstract_phase)] <- mean(df_i$split_percentage[df_e$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
        
        df_abstract_phase$A_sec <- as.integer(round(df_abstract_phase$split_A_Avg*df_abstract_phase$proposed),0)
        df_abstract_phase$B_sec <- as.integer(round(df_abstract_phase$split_B_Avg*df_abstract_phase$proposed),0)
        df_abstract_phase$C_sec <- as.integer(round(df_abstract_phase$split_C_Avg*df_abstract_phase$proposed),0)
        df_abstract_phase$D_sec <- as.integer(df_abstract_phase$split_D_Avg*df_abstract_phase$proposed)
        df_abstract_phase$E_sec <- as.integer(df_abstract_phase$split_E_Avg*df_abstract_phase$proposed)
        df_abstract_phase$F_sec <- as.integer(df_abstract_phase$split_F_Avg*df_abstract_phase$proposed)
        # df_abstract_phase$G_sec <- as.integer(df_abstract_phase$split_G_Avg*df_abstract_phase$proposed)
        # df_abstract_phase$H_sec <- as.integer(df_abstract_phase$split_H_Avg*df_abstract_phase$proposed)
        # df_abstract_phase$I_sec <- as.integer(df_abstract_phase$split_I_Avg*df_abstract_phase$proposed)
      }
      if(number_phases == 7){
        df_a <- dt[dt$phases=="<A>",]
        df_b <- dt[dt$phases=="B",]
        df_c <- dt[dt$phases=="C",]
        df_d <- dt[dt$phases=="D",]
        df_e <- dt[dt$phases=="E",]
        df_f <- dt[dt$phases=="F",]
        df_g <- dt[dt$phases=="G",]
        # df_h <- dt[dt$phases=="H",]
        # df_i <- dt[dt$phases=="I",]
        df_abstract_phase$split_A_Avg <- 0
        df_abstract_phase$split_B_Avg <- 0
        df_abstract_phase$split_C_Avg <- 0
        df_abstract_phase$split_D_Avg <- 0
        df_abstract_phase$split_E_Avg <- 0
        df_abstract_phase$split_F_Avg <- 0
        df_abstract_phase$split_G_Avg <- 0
        # df_abstract_phase$split_H_Avg <- 0
        # df_abstract_phase$split_I_Avg <- 0
        
        for (k in 1:nrow(df_abstract_phase)-1){
          df_abstract_phase$split_A_Avg[k] <- mean(df_a$split_percentage[df_a$time >= df_abstract_phase$time[k] & df_a$time < df_abstract_phase$time[k+1]])
          df_abstract_phase$split_B_Avg[k] <- mean(df_b$split_percentage[df_b$time >= df_abstract_phase$time[k] & df_b$time < df_abstract_phase$time[k+1]])
          df_abstract_phase$split_C_Avg[k] <- mean(df_c$split_percentage[df_c$time >= df_abstract_phase$time[k] & df_c$time < df_abstract_phase$time[k+1]])
          df_abstract_phase$split_D_Avg[k] <- mean(df_d$split_percentage[df_d$time >= df_abstract_phase$time[k] & df_d$time < df_abstract_phase$time[k+1]])
          df_abstract_phase$split_E_Avg[k] <- mean(df_e$split_percentage[df_e$time >= df_abstract_phase$time[k] & df_e$time < df_abstract_phase$time[k+1]])
          df_abstract_phase$split_F_Avg[k] <- mean(df_f$split_percentage[df_f$time >= df_abstract_phase$time[k] & df_f$time < df_abstract_phase$time[k+1]])
          df_abstract_phase$split_G_Avg[k] <- mean(df_g$split_percentage[df_g$time >= df_abstract_phase$time[k] & df_g$time < df_abstract_phase$time[k+1]])
          # df_abstract_phase$split_H_Avg[k] <- mean(df_h$split_percentage[df_h$time >= df_abstract_phase$time[k] & df_h$time < df_abstract_phase$time[k+1]])
          # df_abstract_phase$split_I_Avg[k] <- mean(df_i$split_percentage[df_i$time >= df_abstract_phase$time[k] & df_i$time < df_abstract_phase$time[k+1]])
        }
        df_abstract_phase$split_A_Avg[nrow(df_abstract_phase)] <- mean(df_a$split_percentage[df_a$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
        df_abstract_phase$split_B_Avg[nrow(df_abstract_phase)] <- mean(df_b$split_percentage[df_b$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
        df_abstract_phase$split_C_Avg[nrow(df_abstract_phase)] <- mean(df_c$split_percentage[df_c$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
        df_abstract_phase$split_D_Avg[nrow(df_abstract_phase)] <- mean(df_d$split_percentage[df_d$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
        df_abstract_phase$split_E_Avg[nrow(df_abstract_phase)] <- mean(df_e$split_percentage[df_e$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
        df_abstract_phase$split_F_Avg[nrow(df_abstract_phase)] <- mean(df_f$split_percentage[df_d$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
        df_abstract_phase$split_G_Avg[nrow(df_abstract_phase)] <- mean(df_g$split_percentage[df_e$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
        # df_abstract_phase$split_H_Avg[nrow(df_abstract_phase)] <- mean(df_h$split_percentage[df_d$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
        # df_abstract_phase$split_I_Avg[nrow(df_abstract_phase)] <- mean(df_i$split_percentage[df_e$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
        
        df_abstract_phase$A_sec <- as.integer(round(df_abstract_phase$split_A_Avg*df_abstract_phase$proposed),0)
        df_abstract_phase$B_sec <- as.integer(round(df_abstract_phase$split_B_Avg*df_abstract_phase$proposed),0)
        df_abstract_phase$C_sec <- as.integer(round(df_abstract_phase$split_C_Avg*df_abstract_phase$proposed),0)
        df_abstract_phase$D_sec <- as.integer(df_abstract_phase$split_D_Avg*df_abstract_phase$proposed)
        df_abstract_phase$E_sec <- as.integer(df_abstract_phase$split_E_Avg*df_abstract_phase$proposed)
        df_abstract_phase$F_sec <- as.integer(df_abstract_phase$split_F_Avg*df_abstract_phase$proposed)
        df_abstract_phase$G_sec <- as.integer(df_abstract_phase$split_G_Avg*df_abstract_phase$proposed)
        # df_abstract_phase$H_sec <- as.integer(df_abstract_phase$split_H_Avg*df_abstract_phase$proposed)
        # df_abstract_phase$I_sec <- as.integer(df_abstract_phase$split_I_Avg*df_abstract_phase$proposed)
      }
      if(number_phases == 8){
        df_a <- dt[dt$phases=="<A>",]
        df_b <- dt[dt$phases=="B",]
        df_c <- dt[dt$phases=="C",]
        df_d <- dt[dt$phases=="D",]
        df_e <- dt[dt$phases=="E",]
        df_f <- dt[dt$phases=="F",]
        df_g <- dt[dt$phases=="G",]
        df_h <- dt[dt$phases=="H",]
        # df_i <- dt[dt$phases=="I",]
        df_abstract_phase$split_A_Avg <- 0
        df_abstract_phase$split_B_Avg <- 0
        df_abstract_phase$split_C_Avg <- 0
        df_abstract_phase$split_D_Avg <- 0
        df_abstract_phase$split_E_Avg <- 0
        df_abstract_phase$split_F_Avg <- 0
        df_abstract_phase$split_G_Avg <- 0
        df_abstract_phase$split_H_Avg <- 0
        # df_abstract_phase$split_I_Avg <- 0
        
        for (k in 1:nrow(df_abstract_phase)-1){
          df_abstract_phase$split_A_Avg[k] <- mean(df_a$split_percentage[df_a$time >= df_abstract_phase$time[k] & df_a$time < df_abstract_phase$time[k+1]])
          df_abstract_phase$split_B_Avg[k] <- mean(df_b$split_percentage[df_b$time >= df_abstract_phase$time[k] & df_b$time < df_abstract_phase$time[k+1]])
          df_abstract_phase$split_C_Avg[k] <- mean(df_c$split_percentage[df_c$time >= df_abstract_phase$time[k] & df_c$time < df_abstract_phase$time[k+1]])
          df_abstract_phase$split_D_Avg[k] <- mean(df_d$split_percentage[df_d$time >= df_abstract_phase$time[k] & df_d$time < df_abstract_phase$time[k+1]])
          df_abstract_phase$split_E_Avg[k] <- mean(df_e$split_percentage[df_e$time >= df_abstract_phase$time[k] & df_e$time < df_abstract_phase$time[k+1]])
          df_abstract_phase$split_F_Avg[k] <- mean(df_f$split_percentage[df_f$time >= df_abstract_phase$time[k] & df_f$time < df_abstract_phase$time[k+1]])
          df_abstract_phase$split_G_Avg[k] <- mean(df_g$split_percentage[df_g$time >= df_abstract_phase$time[k] & df_g$time < df_abstract_phase$time[k+1]])
          df_abstract_phase$split_H_Avg[k] <- mean(df_h$split_percentage[df_h$time >= df_abstract_phase$time[k] & df_h$time < df_abstract_phase$time[k+1]])
          # df_abstract_phase$split_I_Avg[k] <- mean(df_i$split_percentage[df_i$time >= df_abstract_phase$time[k] & df_i$time < df_abstract_phase$time[k+1]])
        }
        df_abstract_phase$split_A_Avg[nrow(df_abstract_phase)] <- mean(df_a$split_percentage[df_a$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
        df_abstract_phase$split_B_Avg[nrow(df_abstract_phase)] <- mean(df_b$split_percentage[df_b$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
        df_abstract_phase$split_C_Avg[nrow(df_abstract_phase)] <- mean(df_c$split_percentage[df_c$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
        df_abstract_phase$split_D_Avg[nrow(df_abstract_phase)] <- mean(df_d$split_percentage[df_d$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
        df_abstract_phase$split_E_Avg[nrow(df_abstract_phase)] <- mean(df_e$split_percentage[df_e$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
        df_abstract_phase$split_F_Avg[nrow(df_abstract_phase)] <- mean(df_f$split_percentage[df_d$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
        df_abstract_phase$split_G_Avg[nrow(df_abstract_phase)] <- mean(df_g$split_percentage[df_e$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
        df_abstract_phase$split_H_Avg[nrow(df_abstract_phase)] <- mean(df_h$split_percentage[df_d$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
        # df_abstract_phase$split_I_Avg[nrow(df_abstract_phase)] <- mean(df_i$split_percentage[df_e$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
        
        df_abstract_phase$A_sec <- as.integer(round(df_abstract_phase$split_A_Avg*df_abstract_phase$proposed),0)
        df_abstract_phase$B_sec <- as.integer(round(df_abstract_phase$split_B_Avg*df_abstract_phase$proposed),0)
        df_abstract_phase$C_sec <- as.integer(round(df_abstract_phase$split_C_Avg*df_abstract_phase$proposed),0)
        df_abstract_phase$D_sec <- as.integer(df_abstract_phase$split_D_Avg*df_abstract_phase$proposed)
        df_abstract_phase$E_sec <- as.integer(df_abstract_phase$split_E_Avg*df_abstract_phase$proposed)
        df_abstract_phase$F_sec <- as.integer(df_abstract_phase$split_F_Avg*df_abstract_phase$proposed)
        df_abstract_phase$G_sec <- as.integer(df_abstract_phase$split_G_Avg*df_abstract_phase$proposed)
        df_abstract_phase$H_sec <- as.integer(df_abstract_phase$split_H_Avg*df_abstract_phase$proposed)
        # df_abstract_phase$I_sec <- as.integer(df_abstract_phase$split_I_Avg*df_abstract_phase$proposed)
      }
      if(number_phases == 9){
        df_a <- dt[dt$phases=="<A>",]
        df_b <- dt[dt$phases=="B",]
        df_c <- dt[dt$phases=="C",]
        df_d <- dt[dt$phases=="D",]
        df_e <- dt[dt$phases=="E",]
        df_f <- dt[dt$phases=="F",]
        df_g <- dt[dt$phases=="G",]
        df_h <- dt[dt$phases=="H",]
        df_i <- dt[dt$phases=="I",]
        df_abstract_phase$split_A_Avg <- 0
        df_abstract_phase$split_B_Avg <- 0
        df_abstract_phase$split_C_Avg <- 0
        df_abstract_phase$split_D_Avg <- 0
        df_abstract_phase$split_E_Avg <- 0
        df_abstract_phase$split_F_Avg <- 0
        df_abstract_phase$split_G_Avg <- 0
        df_abstract_phase$split_H_Avg <- 0
        df_abstract_phase$split_I_Avg <- 0
        
        for (k in 1:nrow(df_abstract_phase)-1){
          df_abstract_phase$split_A_Avg[k] <- mean(df_a$split_percentage[df_a$time >= df_abstract_phase$time[k] & df_a$time < df_abstract_phase$time[k+1]])
          df_abstract_phase$split_B_Avg[k] <- mean(df_b$split_percentage[df_b$time >= df_abstract_phase$time[k] & df_b$time < df_abstract_phase$time[k+1]])
          df_abstract_phase$split_C_Avg[k] <- mean(df_c$split_percentage[df_c$time >= df_abstract_phase$time[k] & df_c$time < df_abstract_phase$time[k+1]])
          df_abstract_phase$split_D_Avg[k] <- mean(df_d$split_percentage[df_d$time >= df_abstract_phase$time[k] & df_d$time < df_abstract_phase$time[k+1]])
          df_abstract_phase$split_E_Avg[k] <- mean(df_e$split_percentage[df_e$time >= df_abstract_phase$time[k] & df_e$time < df_abstract_phase$time[k+1]])
          df_abstract_phase$split_F_Avg[k] <- mean(df_f$split_percentage[df_f$time >= df_abstract_phase$time[k] & df_f$time < df_abstract_phase$time[k+1]])
          df_abstract_phase$split_G_Avg[k] <- mean(df_g$split_percentage[df_g$time >= df_abstract_phase$time[k] & df_g$time < df_abstract_phase$time[k+1]])
          df_abstract_phase$split_H_Avg[k] <- mean(df_h$split_percentage[df_h$time >= df_abstract_phase$time[k] & df_h$time < df_abstract_phase$time[k+1]])
          df_abstract_phase$split_I_Avg[k] <- mean(df_i$split_percentage[df_i$time >= df_abstract_phase$time[k] & df_i$time < df_abstract_phase$time[k+1]])
        }
        df_abstract_phase$split_A_Avg[nrow(df_abstract_phase)] <- mean(df_a$split_percentage[df_a$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
        df_abstract_phase$split_B_Avg[nrow(df_abstract_phase)] <- mean(df_b$split_percentage[df_b$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
        df_abstract_phase$split_C_Avg[nrow(df_abstract_phase)] <- mean(df_c$split_percentage[df_c$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
        df_abstract_phase$split_D_Avg[nrow(df_abstract_phase)] <- mean(df_d$split_percentage[df_d$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
        df_abstract_phase$split_E_Avg[nrow(df_abstract_phase)] <- mean(df_e$split_percentage[df_e$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
        df_abstract_phase$split_F_Avg[nrow(df_abstract_phase)] <- mean(df_f$split_percentage[df_d$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
        df_abstract_phase$split_G_Avg[nrow(df_abstract_phase)] <- mean(df_g$split_percentage[df_e$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
        df_abstract_phase$split_H_Avg[nrow(df_abstract_phase)] <- mean(df_h$split_percentage[df_d$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
        df_abstract_phase$split_I_Avg[nrow(df_abstract_phase)] <- mean(df_i$split_percentage[df_e$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
        
        df_abstract_phase$A_sec <- as.integer(round(df_abstract_phase$split_A_Avg*df_abstract_phase$proposed),0)
        df_abstract_phase$B_sec <- as.integer(round(df_abstract_phase$split_B_Avg*df_abstract_phase$proposed),0)
        df_abstract_phase$C_sec <- as.integer(round(df_abstract_phase$split_C_Avg*df_abstract_phase$proposed),0)
        df_abstract_phase$D_sec <- as.integer(df_abstract_phase$split_D_Avg*df_abstract_phase$proposed)
        df_abstract_phase$E_sec <- as.integer(df_abstract_phase$split_E_Avg*df_abstract_phase$proposed)
        df_abstract_phase$F_sec <- as.integer(df_abstract_phase$split_F_Avg*df_abstract_phase$proposed)
        df_abstract_phase$G_sec <- as.integer(df_abstract_phase$split_G_Avg*df_abstract_phase$proposed)
        df_abstract_phase$H_sec <- as.integer(df_abstract_phase$split_H_Avg*df_abstract_phase$proposed)
        df_abstract_phase$I_sec <- as.integer(df_abstract_phase$split_I_Avg*df_abstract_phase$proposed)
      }
      
      df_abstract_phase$time <- hms::as_hms(df_abstract_phase$time)
      output$contents_8_4 <- renderTable( 
        df_abstract_phase 
      )
      write.csv(df_abstract_phase, file8_4, row.names=FALSE)
    }
  )
  observeEvent(input$previewData8_5, {
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    
    progress$set(message = "Creating a Plot....", value = 0) 
    source("https://raw.githubusercontent.com/abdallahshabarek/ITS_NJDOT/main/Scripts/TOD_phase_optimization_from_historical_dates_function.R")
    # dirname(input$file8_2$datapath)[1]
    dates_list2_file8 <- as.data.frame(input$date8_1_multiple)
    colnames(dates_list2_file8) <- c("dates")
    dates_list3_file8 <- as.list(dates_list2_file8$dates)
    dt <- TOD_phase_optimization_from_historical_dates(site=as.integer(input$site_historical_8),
                                                       dates_list = dates_list3_file8,
                                                       analysis_time_type=input$file_8_time_type)
    df_abstract_phase <- readr::read_csv(input$file8_4$datapath)
    df_abstract_phase$time <- as.integer(df_abstract_phase$time)
    number_phases <- as.integer(nrow(data.frame(unique(dt$phases))))
    
    if(number_phases == 2){
      df_a <- dt[dt$phases=="<A>",]
      df_b <- dt[dt$phases=="B",]
      # df_c <- dt[dt$phases=="C",]
      # df_d <- dt[dt$phases=="D",]
      # df_e <- dt[dt$phases=="E",]
      # df_f <- dt[dt$phases=="F",]
      # df_f <- dt[dt$phases=="G",]
      # df_f <- dt[dt$phases=="H",]
      # df_f <- dt[dt$phases=="I",]
      df_abstract_phase$split_A_Avg <- 0
      df_abstract_phase$split_B_Avg <- 0
      # df_abstract_phase$split_C_Avg <- 0
      # df_abstract_phase$split_D_Avg <- 0
      # df_abstract_phase$split_E_Avg <- 0
      # df_abstract_phase$split_F_Avg <- 0
      # df_abstract_phase$split_G_Avg <- 0
      # df_abstract_phase$split_H_Avg <- 0
      # df_abstract_phase$split_I_Avg <- 0
      
      for (k in 1:nrow(df_abstract_phase)-1){
        df_abstract_phase$split_A_Avg[k] <- mean(df_a$split_percentage[df_a$time >= df_abstract_phase$time[k] & df_a$time < df_abstract_phase$time[k+1]])
        df_abstract_phase$split_B_Avg[k] <- mean(df_b$split_percentage[df_b$time >= df_abstract_phase$time[k] & df_b$time < df_abstract_phase$time[k+1]])
        # df_abstract_phase$split_C_Avg[k] <- mean(df_c$split_percentage[df_c$time >= df_abstract_phase$time[k] & df_c$time < df_abstract_phase$time[k+1]])
        # df_abstract_phase$split_D_Avg[k] <- mean(df_d$split_percentage[df_d$time >= df_abstract_phase$time[k] & df_d$time < df_abstract_phase$time[k+1]])
        # df_abstract_phase$split_E_Avg[k] <- mean(df_e$split_percentage[df_e$time >= df_abstract_phase$time[k] & df_e$time < df_abstract_phase$time[k+1]])
        # df_abstract_phase$split_F_Avg[k] <- mean(df_f$split_percentage[df_f$time >= df_abstract_phase$time[k] & df_f$time < df_abstract_phase$time[k+1]])
        # df_abstract_phase$split_G_Avg[k] <- mean(df_g$split_percentage[df_g$time >= df_abstract_phase$time[k] & df_g$time < df_abstract_phase$time[k+1]])
        # df_abstract_phase$split_H_Avg[k] <- mean(df_h$split_percentage[df_h$time >= df_abstract_phase$time[k] & df_h$time < df_abstract_phase$time[k+1]])
        # df_abstract_phase$split_I_Avg[k] <- mean(df_i$split_percentage[df_i$time >= df_abstract_phase$time[k] & df_i$time < df_abstract_phase$time[k+1]])
      }
      df_abstract_phase$split_A_Avg[nrow(df_abstract_phase)] <- mean(df_a$split_percentage[df_a$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
      df_abstract_phase$split_B_Avg[nrow(df_abstract_phase)] <- mean(df_b$split_percentage[df_b$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
      # df_abstract_phase$split_C_Avg[nrow(df_abstract_phase)] <- mean(df_c$split_percentage[df_c$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
      # df_abstract_phase$split_D_Avg[nrow(df_abstract_phase)] <- mean(df_d$split_percentage[df_d$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
      # df_abstract_phase$split_E_Avg[nrow(df_abstract_phase)] <- mean(df_e$split_percentage[df_e$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
      # df_abstract_phase$split_F_Avg[nrow(df_abstract_phase)] <- mean(df_f$split_percentage[df_d$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
      # df_abstract_phase$split_G_Avg[nrow(df_abstract_phase)] <- mean(df_g$split_percentage[df_e$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
      # df_abstract_phase$split_H_Avg[nrow(df_abstract_phase)] <- mean(df_h$split_percentage[df_d$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
      # df_abstract_phase$split_I_Avg[nrow(df_abstract_phase)] <- mean(df_i$split_percentage[df_e$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
      
      df_abstract_phase$A_sec <- as.integer(round(df_abstract_phase$split_A_Avg*df_abstract_phase$proposed),0)
      df_abstract_phase$B_sec <- as.integer(round(df_abstract_phase$split_B_Avg*df_abstract_phase$proposed),0)
      # df_abstract_phase$C_sec <- as.integer(round(df_abstract_phase$split_C_Avg*df_abstract_phase$proposed),0)
      # df_abstract_phase$D_sec <- as.integer(df_abstract_phase$split_D_Avg*df_abstract_phase$proposed)
      # df_abstract_phase$E_sec <- as.integer(df_abstract_phase$split_E_Avg*df_abstract_phase$proposed)
      # df_abstract_phase$F_sec <- as.integer(df_abstract_phase$split_F_Avg*df_abstract_phase$proposed)
      # df_abstract_phase$G_sec <- as.integer(df_abstract_phase$split_G_Avg*df_abstract_phase$proposed)
      # df_abstract_phase$H_sec <- as.integer(df_abstract_phase$split_H_Avg*df_abstract_phase$proposed)
      # df_abstract_phase$I_sec <- as.integer(df_abstract_phase$split_I_Avg*df_abstract_phase$proposed)
    }
    if(number_phases == 3){
      df_a <- dt[dt$phases=="<A>",]
      df_b <- dt[dt$phases=="B",]
      df_c <- dt[dt$phases=="C",]
      # df_d <- dt[dt$phases=="D",]
      # df_e <- dt[dt$phases=="E",]
      # df_f <- dt[dt$phases=="F",]
      # df_f <- dt[dt$phases=="G",]
      # df_f <- dt[dt$phases=="H",]
      # df_f <- dt[dt$phases=="I",]
      df_abstract_phase$split_A_Avg <- 0
      df_abstract_phase$split_B_Avg <- 0
      df_abstract_phase$split_C_Avg <- 0
      # df_abstract_phase$split_D_Avg <- 0
      # df_abstract_phase$split_E_Avg <- 0
      # df_abstract_phase$split_F_Avg <- 0
      # df_abstract_phase$split_G_Avg <- 0
      # df_abstract_phase$split_H_Avg <- 0
      # df_abstract_phase$split_I_Avg <- 0
      
      for (k in 1:nrow(df_abstract_phase)-1){
        df_abstract_phase$split_A_Avg[k] <- mean(df_a$split_percentage[df_a$time >= df_abstract_phase$time[k] & df_a$time < df_abstract_phase$time[k+1]])
        df_abstract_phase$split_B_Avg[k] <- mean(df_b$split_percentage[df_b$time >= df_abstract_phase$time[k] & df_b$time < df_abstract_phase$time[k+1]])
        df_abstract_phase$split_C_Avg[k] <- mean(df_c$split_percentage[df_c$time >= df_abstract_phase$time[k] & df_c$time < df_abstract_phase$time[k+1]])
        # df_abstract_phase$split_D_Avg[k] <- mean(df_d$split_percentage[df_d$time >= df_abstract_phase$time[k] & df_d$time < df_abstract_phase$time[k+1]])
        # df_abstract_phase$split_E_Avg[k] <- mean(df_e$split_percentage[df_e$time >= df_abstract_phase$time[k] & df_e$time < df_abstract_phase$time[k+1]])
        # df_abstract_phase$split_F_Avg[k] <- mean(df_f$split_percentage[df_f$time >= df_abstract_phase$time[k] & df_f$time < df_abstract_phase$time[k+1]])
        # df_abstract_phase$split_G_Avg[k] <- mean(df_g$split_percentage[df_g$time >= df_abstract_phase$time[k] & df_g$time < df_abstract_phase$time[k+1]])
        # df_abstract_phase$split_H_Avg[k] <- mean(df_h$split_percentage[df_h$time >= df_abstract_phase$time[k] & df_h$time < df_abstract_phase$time[k+1]])
        # df_abstract_phase$split_I_Avg[k] <- mean(df_i$split_percentage[df_i$time >= df_abstract_phase$time[k] & df_i$time < df_abstract_phase$time[k+1]])
      }
      df_abstract_phase$split_A_Avg[nrow(df_abstract_phase)] <- mean(df_a$split_percentage[df_a$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
      df_abstract_phase$split_B_Avg[nrow(df_abstract_phase)] <- mean(df_b$split_percentage[df_b$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
      df_abstract_phase$split_C_Avg[nrow(df_abstract_phase)] <- mean(df_c$split_percentage[df_c$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
      # df_abstract_phase$split_D_Avg[nrow(df_abstract_phase)] <- mean(df_d$split_percentage[df_d$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
      # df_abstract_phase$split_E_Avg[nrow(df_abstract_phase)] <- mean(df_e$split_percentage[df_e$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
      # df_abstract_phase$split_F_Avg[nrow(df_abstract_phase)] <- mean(df_f$split_percentage[df_d$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
      # df_abstract_phase$split_G_Avg[nrow(df_abstract_phase)] <- mean(df_g$split_percentage[df_e$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
      # df_abstract_phase$split_H_Avg[nrow(df_abstract_phase)] <- mean(df_h$split_percentage[df_d$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
      # df_abstract_phase$split_I_Avg[nrow(df_abstract_phase)] <- mean(df_i$split_percentage[df_e$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
      
      df_abstract_phase$A_sec <- as.integer(round(df_abstract_phase$split_A_Avg*df_abstract_phase$proposed),0)
      df_abstract_phase$B_sec <- as.integer(round(df_abstract_phase$split_B_Avg*df_abstract_phase$proposed),0)
      df_abstract_phase$C_sec <- as.integer(round(df_abstract_phase$split_C_Avg*df_abstract_phase$proposed),0)
      # df_abstract_phase$D_sec <- as.integer(df_abstract_phase$split_D_Avg*df_abstract_phase$proposed)
      # df_abstract_phase$E_sec <- as.integer(df_abstract_phase$split_E_Avg*df_abstract_phase$proposed)
      # df_abstract_phase$F_sec <- as.integer(df_abstract_phase$split_F_Avg*df_abstract_phase$proposed)
      # df_abstract_phase$G_sec <- as.integer(df_abstract_phase$split_G_Avg*df_abstract_phase$proposed)
      # df_abstract_phase$H_sec <- as.integer(df_abstract_phase$split_H_Avg*df_abstract_phase$proposed)
      # df_abstract_phase$I_sec <- as.integer(df_abstract_phase$split_I_Avg*df_abstract_phase$proposed)
    }
    if(number_phases == 4){
      df_a <- dt[dt$phases=="<A>",]
      df_b <- dt[dt$phases=="B",]
      df_c <- dt[dt$phases=="C",]
      df_d <- dt[dt$phases=="D",]
      # df_e <- dt[dt$phases=="E",]
      # df_f <- dt[dt$phases=="F",]
      # df_f <- dt[dt$phases=="G",]
      # df_f <- dt[dt$phases=="H",]
      # df_f <- dt[dt$phases=="I",]
      df_abstract_phase$split_A_Avg <- 0
      df_abstract_phase$split_B_Avg <- 0
      df_abstract_phase$split_C_Avg <- 0
      df_abstract_phase$split_D_Avg <- 0
      # df_abstract_phase$split_E_Avg <- 0
      # df_abstract_phase$split_F_Avg <- 0
      # df_abstract_phase$split_G_Avg <- 0
      # df_abstract_phase$split_H_Avg <- 0
      # df_abstract_phase$split_I_Avg <- 0
      
      for (k in 1:nrow(df_abstract_phase)-1){
        df_abstract_phase$split_A_Avg[k] <- mean(df_a$split_percentage[df_a$time >= df_abstract_phase$time[k] & df_a$time < df_abstract_phase$time[k+1]])
        df_abstract_phase$split_B_Avg[k] <- mean(df_b$split_percentage[df_b$time >= df_abstract_phase$time[k] & df_b$time < df_abstract_phase$time[k+1]])
        df_abstract_phase$split_C_Avg[k] <- mean(df_c$split_percentage[df_c$time >= df_abstract_phase$time[k] & df_c$time < df_abstract_phase$time[k+1]])
        df_abstract_phase$split_D_Avg[k] <- mean(df_d$split_percentage[df_d$time >= df_abstract_phase$time[k] & df_d$time < df_abstract_phase$time[k+1]])
        # df_abstract_phase$split_E_Avg[k] <- mean(df_e$split_percentage[df_e$time >= df_abstract_phase$time[k] & df_e$time < df_abstract_phase$time[k+1]])
        # df_abstract_phase$split_F_Avg[k] <- mean(df_f$split_percentage[df_f$time >= df_abstract_phase$time[k] & df_f$time < df_abstract_phase$time[k+1]])
        # df_abstract_phase$split_G_Avg[k] <- mean(df_g$split_percentage[df_g$time >= df_abstract_phase$time[k] & df_g$time < df_abstract_phase$time[k+1]])
        # df_abstract_phase$split_H_Avg[k] <- mean(df_h$split_percentage[df_h$time >= df_abstract_phase$time[k] & df_h$time < df_abstract_phase$time[k+1]])
        # df_abstract_phase$split_I_Avg[k] <- mean(df_i$split_percentage[df_i$time >= df_abstract_phase$time[k] & df_i$time < df_abstract_phase$time[k+1]])
      }
      df_abstract_phase$split_A_Avg[nrow(df_abstract_phase)] <- mean(df_a$split_percentage[df_a$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
      df_abstract_phase$split_B_Avg[nrow(df_abstract_phase)] <- mean(df_b$split_percentage[df_b$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
      df_abstract_phase$split_C_Avg[nrow(df_abstract_phase)] <- mean(df_c$split_percentage[df_c$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
      df_abstract_phase$split_D_Avg[nrow(df_abstract_phase)] <- mean(df_d$split_percentage[df_d$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
      # df_abstract_phase$split_E_Avg[nrow(df_abstract_phase)] <- mean(df_e$split_percentage[df_e$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
      # df_abstract_phase$split_F_Avg[nrow(df_abstract_phase)] <- mean(df_f$split_percentage[df_d$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
      # df_abstract_phase$split_G_Avg[nrow(df_abstract_phase)] <- mean(df_g$split_percentage[df_e$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
      # df_abstract_phase$split_H_Avg[nrow(df_abstract_phase)] <- mean(df_h$split_percentage[df_d$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
      # df_abstract_phase$split_I_Avg[nrow(df_abstract_phase)] <- mean(df_i$split_percentage[df_e$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
      
      df_abstract_phase$A_sec <- as.integer(round(df_abstract_phase$split_A_Avg*df_abstract_phase$proposed),0)
      df_abstract_phase$B_sec <- as.integer(round(df_abstract_phase$split_B_Avg*df_abstract_phase$proposed),0)
      df_abstract_phase$C_sec <- as.integer(round(df_abstract_phase$split_C_Avg*df_abstract_phase$proposed),0)
      df_abstract_phase$D_sec <- as.integer(df_abstract_phase$split_D_Avg*df_abstract_phase$proposed)
      # df_abstract_phase$E_sec <- as.integer(df_abstract_phase$split_E_Avg*df_abstract_phase$proposed)
      # df_abstract_phase$F_sec <- as.integer(df_abstract_phase$split_F_Avg*df_abstract_phase$proposed)
      # df_abstract_phase$G_sec <- as.integer(df_abstract_phase$split_G_Avg*df_abstract_phase$proposed)
      # df_abstract_phase$H_sec <- as.integer(df_abstract_phase$split_H_Avg*df_abstract_phase$proposed)
      # df_abstract_phase$I_sec <- as.integer(df_abstract_phase$split_I_Avg*df_abstract_phase$proposed)
    }
    if(number_phases == 5){
      df_a <- dt[dt$phases=="<A>",]
      df_b <- dt[dt$phases=="B",]
      df_c <- dt[dt$phases=="C",]
      df_d <- dt[dt$phases=="D",]
      df_e <- dt[dt$phases=="E",]
      # df_f <- dt[dt$phases=="F",]
      # df_f <- dt[dt$phases=="G",]
      # df_f <- dt[dt$phases=="H",]
      # df_f <- dt[dt$phases=="I",]
      df_abstract_phase$split_A_Avg <- 0
      df_abstract_phase$split_B_Avg <- 0
      df_abstract_phase$split_C_Avg <- 0
      df_abstract_phase$split_D_Avg <- 0
      df_abstract_phase$split_E_Avg <- 0
      # df_abstract_phase$split_F_Avg <- 0
      # df_abstract_phase$split_G_Avg <- 0
      # df_abstract_phase$split_H_Avg <- 0
      # df_abstract_phase$split_I_Avg <- 0
      
      for (k in 1:nrow(df_abstract_phase)-1){
        df_abstract_phase$split_A_Avg[k] <- mean(df_a$split_percentage[df_a$time >= df_abstract_phase$time[k] & df_a$time < df_abstract_phase$time[k+1]])
        df_abstract_phase$split_B_Avg[k] <- mean(df_b$split_percentage[df_b$time >= df_abstract_phase$time[k] & df_b$time < df_abstract_phase$time[k+1]])
        df_abstract_phase$split_C_Avg[k] <- mean(df_c$split_percentage[df_c$time >= df_abstract_phase$time[k] & df_c$time < df_abstract_phase$time[k+1]])
        df_abstract_phase$split_D_Avg[k] <- mean(df_d$split_percentage[df_d$time >= df_abstract_phase$time[k] & df_d$time < df_abstract_phase$time[k+1]])
        df_abstract_phase$split_E_Avg[k] <- mean(df_e$split_percentage[df_e$time >= df_abstract_phase$time[k] & df_e$time < df_abstract_phase$time[k+1]])
        # df_abstract_phase$split_F_Avg[k] <- mean(df_f$split_percentage[df_f$time >= df_abstract_phase$time[k] & df_f$time < df_abstract_phase$time[k+1]])
        # df_abstract_phase$split_G_Avg[k] <- mean(df_g$split_percentage[df_g$time >= df_abstract_phase$time[k] & df_g$time < df_abstract_phase$time[k+1]])
        # df_abstract_phase$split_H_Avg[k] <- mean(df_h$split_percentage[df_h$time >= df_abstract_phase$time[k] & df_h$time < df_abstract_phase$time[k+1]])
        # df_abstract_phase$split_I_Avg[k] <- mean(df_i$split_percentage[df_i$time >= df_abstract_phase$time[k] & df_i$time < df_abstract_phase$time[k+1]])
      }
      df_abstract_phase$split_A_Avg[nrow(df_abstract_phase)] <- mean(df_a$split_percentage[df_a$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
      df_abstract_phase$split_B_Avg[nrow(df_abstract_phase)] <- mean(df_b$split_percentage[df_b$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
      df_abstract_phase$split_C_Avg[nrow(df_abstract_phase)] <- mean(df_c$split_percentage[df_c$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
      df_abstract_phase$split_D_Avg[nrow(df_abstract_phase)] <- mean(df_d$split_percentage[df_d$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
      df_abstract_phase$split_E_Avg[nrow(df_abstract_phase)] <- mean(df_e$split_percentage[df_e$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
      # df_abstract_phase$split_F_Avg[nrow(df_abstract_phase)] <- mean(df_f$split_percentage[df_d$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
      # df_abstract_phase$split_G_Avg[nrow(df_abstract_phase)] <- mean(df_g$split_percentage[df_e$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
      # df_abstract_phase$split_H_Avg[nrow(df_abstract_phase)] <- mean(df_h$split_percentage[df_d$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
      # df_abstract_phase$split_I_Avg[nrow(df_abstract_phase)] <- mean(df_i$split_percentage[df_e$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
      
      df_abstract_phase$A_sec <- as.integer(round(df_abstract_phase$split_A_Avg*df_abstract_phase$proposed),0)
      df_abstract_phase$B_sec <- as.integer(round(df_abstract_phase$split_B_Avg*df_abstract_phase$proposed),0)
      df_abstract_phase$C_sec <- as.integer(round(df_abstract_phase$split_C_Avg*df_abstract_phase$proposed),0)
      df_abstract_phase$D_sec <- as.integer(df_abstract_phase$split_D_Avg*df_abstract_phase$proposed)
      df_abstract_phase$E_sec <- as.integer(df_abstract_phase$split_E_Avg*df_abstract_phase$proposed)
      # df_abstract_phase$F_sec <- as.integer(df_abstract_phase$split_F_Avg*df_abstract_phase$proposed)
      # df_abstract_phase$G_sec <- as.integer(df_abstract_phase$split_G_Avg*df_abstract_phase$proposed)
      # df_abstract_phase$H_sec <- as.integer(df_abstract_phase$split_H_Avg*df_abstract_phase$proposed)
      # df_abstract_phase$I_sec <- as.integer(df_abstract_phase$split_I_Avg*df_abstract_phase$proposed)
    }
    if(number_phases == 6){
      df_a <- dt[dt$phases=="<A>",]
      df_b <- dt[dt$phases=="B",]
      df_c <- dt[dt$phases=="C",]
      df_d <- dt[dt$phases=="D",]
      df_e <- dt[dt$phases=="E",]
      df_f <- dt[dt$phases=="F",]
      # df_g <- dt[dt$phases=="G",]
      # df_h <- dt[dt$phases=="H",]
      # df_i <- dt[dt$phases=="I",]
      df_abstract_phase$split_A_Avg <- 0
      df_abstract_phase$split_B_Avg <- 0
      df_abstract_phase$split_C_Avg <- 0
      df_abstract_phase$split_D_Avg <- 0
      df_abstract_phase$split_E_Avg <- 0
      df_abstract_phase$split_F_Avg <- 0
      # df_abstract_phase$split_G_Avg <- 0
      # df_abstract_phase$split_H_Avg <- 0
      # df_abstract_phase$split_I_Avg <- 0
      
      for (k in 1:nrow(df_abstract_phase)-1){
        df_abstract_phase$split_A_Avg[k] <- mean(df_a$split_percentage[df_a$time >= df_abstract_phase$time[k] & df_a$time < df_abstract_phase$time[k+1]])
        df_abstract_phase$split_B_Avg[k] <- mean(df_b$split_percentage[df_b$time >= df_abstract_phase$time[k] & df_b$time < df_abstract_phase$time[k+1]])
        df_abstract_phase$split_C_Avg[k] <- mean(df_c$split_percentage[df_c$time >= df_abstract_phase$time[k] & df_c$time < df_abstract_phase$time[k+1]])
        df_abstract_phase$split_D_Avg[k] <- mean(df_d$split_percentage[df_d$time >= df_abstract_phase$time[k] & df_d$time < df_abstract_phase$time[k+1]])
        df_abstract_phase$split_E_Avg[k] <- mean(df_e$split_percentage[df_e$time >= df_abstract_phase$time[k] & df_e$time < df_abstract_phase$time[k+1]])
        df_abstract_phase$split_F_Avg[k] <- mean(df_f$split_percentage[df_f$time >= df_abstract_phase$time[k] & df_f$time < df_abstract_phase$time[k+1]])
        # df_abstract_phase$split_G_Avg[k] <- mean(df_g$split_percentage[df_g$time >= df_abstract_phase$time[k] & df_g$time < df_abstract_phase$time[k+1]])
        # df_abstract_phase$split_H_Avg[k] <- mean(df_h$split_percentage[df_h$time >= df_abstract_phase$time[k] & df_h$time < df_abstract_phase$time[k+1]])
        # df_abstract_phase$split_I_Avg[k] <- mean(df_i$split_percentage[df_i$time >= df_abstract_phase$time[k] & df_i$time < df_abstract_phase$time[k+1]])
      }
      df_abstract_phase$split_A_Avg[nrow(df_abstract_phase)] <- mean(df_a$split_percentage[df_a$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
      df_abstract_phase$split_B_Avg[nrow(df_abstract_phase)] <- mean(df_b$split_percentage[df_b$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
      df_abstract_phase$split_C_Avg[nrow(df_abstract_phase)] <- mean(df_c$split_percentage[df_c$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
      df_abstract_phase$split_D_Avg[nrow(df_abstract_phase)] <- mean(df_d$split_percentage[df_d$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
      df_abstract_phase$split_E_Avg[nrow(df_abstract_phase)] <- mean(df_e$split_percentage[df_e$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
      df_abstract_phase$split_F_Avg[nrow(df_abstract_phase)] <- mean(df_f$split_percentage[df_d$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
      # df_abstract_phase$split_G_Avg[nrow(df_abstract_phase)] <- mean(df_g$split_percentage[df_e$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
      # df_abstract_phase$split_H_Avg[nrow(df_abstract_phase)] <- mean(df_h$split_percentage[df_d$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
      # df_abstract_phase$split_I_Avg[nrow(df_abstract_phase)] <- mean(df_i$split_percentage[df_e$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
      
      df_abstract_phase$A_sec <- as.integer(round(df_abstract_phase$split_A_Avg*df_abstract_phase$proposed),0)
      df_abstract_phase$B_sec <- as.integer(round(df_abstract_phase$split_B_Avg*df_abstract_phase$proposed),0)
      df_abstract_phase$C_sec <- as.integer(round(df_abstract_phase$split_C_Avg*df_abstract_phase$proposed),0)
      df_abstract_phase$D_sec <- as.integer(df_abstract_phase$split_D_Avg*df_abstract_phase$proposed)
      df_abstract_phase$E_sec <- as.integer(df_abstract_phase$split_E_Avg*df_abstract_phase$proposed)
      df_abstract_phase$F_sec <- as.integer(df_abstract_phase$split_F_Avg*df_abstract_phase$proposed)
      # df_abstract_phase$G_sec <- as.integer(df_abstract_phase$split_G_Avg*df_abstract_phase$proposed)
      # df_abstract_phase$H_sec <- as.integer(df_abstract_phase$split_H_Avg*df_abstract_phase$proposed)
      # df_abstract_phase$I_sec <- as.integer(df_abstract_phase$split_I_Avg*df_abstract_phase$proposed)
    }
    if(number_phases == 7){
      df_a <- dt[dt$phases=="<A>",]
      df_b <- dt[dt$phases=="B",]
      df_c <- dt[dt$phases=="C",]
      df_d <- dt[dt$phases=="D",]
      df_e <- dt[dt$phases=="E",]
      df_f <- dt[dt$phases=="F",]
      df_g <- dt[dt$phases=="G",]
      # df_h <- dt[dt$phases=="H",]
      # df_i <- dt[dt$phases=="I",]
      df_abstract_phase$split_A_Avg <- 0
      df_abstract_phase$split_B_Avg <- 0
      df_abstract_phase$split_C_Avg <- 0
      df_abstract_phase$split_D_Avg <- 0
      df_abstract_phase$split_E_Avg <- 0
      df_abstract_phase$split_F_Avg <- 0
      df_abstract_phase$split_G_Avg <- 0
      # df_abstract_phase$split_H_Avg <- 0
      # df_abstract_phase$split_I_Avg <- 0
      
      for (k in 1:nrow(df_abstract_phase)-1){
        df_abstract_phase$split_A_Avg[k] <- mean(df_a$split_percentage[df_a$time >= df_abstract_phase$time[k] & df_a$time < df_abstract_phase$time[k+1]])
        df_abstract_phase$split_B_Avg[k] <- mean(df_b$split_percentage[df_b$time >= df_abstract_phase$time[k] & df_b$time < df_abstract_phase$time[k+1]])
        df_abstract_phase$split_C_Avg[k] <- mean(df_c$split_percentage[df_c$time >= df_abstract_phase$time[k] & df_c$time < df_abstract_phase$time[k+1]])
        df_abstract_phase$split_D_Avg[k] <- mean(df_d$split_percentage[df_d$time >= df_abstract_phase$time[k] & df_d$time < df_abstract_phase$time[k+1]])
        df_abstract_phase$split_E_Avg[k] <- mean(df_e$split_percentage[df_e$time >= df_abstract_phase$time[k] & df_e$time < df_abstract_phase$time[k+1]])
        df_abstract_phase$split_F_Avg[k] <- mean(df_f$split_percentage[df_f$time >= df_abstract_phase$time[k] & df_f$time < df_abstract_phase$time[k+1]])
        df_abstract_phase$split_G_Avg[k] <- mean(df_g$split_percentage[df_g$time >= df_abstract_phase$time[k] & df_g$time < df_abstract_phase$time[k+1]])
        # df_abstract_phase$split_H_Avg[k] <- mean(df_h$split_percentage[df_h$time >= df_abstract_phase$time[k] & df_h$time < df_abstract_phase$time[k+1]])
        # df_abstract_phase$split_I_Avg[k] <- mean(df_i$split_percentage[df_i$time >= df_abstract_phase$time[k] & df_i$time < df_abstract_phase$time[k+1]])
      }
      df_abstract_phase$split_A_Avg[nrow(df_abstract_phase)] <- mean(df_a$split_percentage[df_a$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
      df_abstract_phase$split_B_Avg[nrow(df_abstract_phase)] <- mean(df_b$split_percentage[df_b$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
      df_abstract_phase$split_C_Avg[nrow(df_abstract_phase)] <- mean(df_c$split_percentage[df_c$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
      df_abstract_phase$split_D_Avg[nrow(df_abstract_phase)] <- mean(df_d$split_percentage[df_d$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
      df_abstract_phase$split_E_Avg[nrow(df_abstract_phase)] <- mean(df_e$split_percentage[df_e$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
      df_abstract_phase$split_F_Avg[nrow(df_abstract_phase)] <- mean(df_f$split_percentage[df_d$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
      df_abstract_phase$split_G_Avg[nrow(df_abstract_phase)] <- mean(df_g$split_percentage[df_e$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
      # df_abstract_phase$split_H_Avg[nrow(df_abstract_phase)] <- mean(df_h$split_percentage[df_d$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
      # df_abstract_phase$split_I_Avg[nrow(df_abstract_phase)] <- mean(df_i$split_percentage[df_e$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
      
      df_abstract_phase$A_sec <- as.integer(round(df_abstract_phase$split_A_Avg*df_abstract_phase$proposed),0)
      df_abstract_phase$B_sec <- as.integer(round(df_abstract_phase$split_B_Avg*df_abstract_phase$proposed),0)
      df_abstract_phase$C_sec <- as.integer(round(df_abstract_phase$split_C_Avg*df_abstract_phase$proposed),0)
      df_abstract_phase$D_sec <- as.integer(df_abstract_phase$split_D_Avg*df_abstract_phase$proposed)
      df_abstract_phase$E_sec <- as.integer(df_abstract_phase$split_E_Avg*df_abstract_phase$proposed)
      df_abstract_phase$F_sec <- as.integer(df_abstract_phase$split_F_Avg*df_abstract_phase$proposed)
      df_abstract_phase$G_sec <- as.integer(df_abstract_phase$split_G_Avg*df_abstract_phase$proposed)
      # df_abstract_phase$H_sec <- as.integer(df_abstract_phase$split_H_Avg*df_abstract_phase$proposed)
      # df_abstract_phase$I_sec <- as.integer(df_abstract_phase$split_I_Avg*df_abstract_phase$proposed)
    }
    if(number_phases == 8){
      df_a <- dt[dt$phases=="<A>",]
      df_b <- dt[dt$phases=="B",]
      df_c <- dt[dt$phases=="C",]
      df_d <- dt[dt$phases=="D",]
      df_e <- dt[dt$phases=="E",]
      df_f <- dt[dt$phases=="F",]
      df_g <- dt[dt$phases=="G",]
      df_h <- dt[dt$phases=="H",]
      # df_i <- dt[dt$phases=="I",]
      df_abstract_phase$split_A_Avg <- 0
      df_abstract_phase$split_B_Avg <- 0
      df_abstract_phase$split_C_Avg <- 0
      df_abstract_phase$split_D_Avg <- 0
      df_abstract_phase$split_E_Avg <- 0
      df_abstract_phase$split_F_Avg <- 0
      df_abstract_phase$split_G_Avg <- 0
      df_abstract_phase$split_H_Avg <- 0
      # df_abstract_phase$split_I_Avg <- 0
      
      for (k in 1:nrow(df_abstract_phase)-1){
        df_abstract_phase$split_A_Avg[k] <- mean(df_a$split_percentage[df_a$time >= df_abstract_phase$time[k] & df_a$time < df_abstract_phase$time[k+1]])
        df_abstract_phase$split_B_Avg[k] <- mean(df_b$split_percentage[df_b$time >= df_abstract_phase$time[k] & df_b$time < df_abstract_phase$time[k+1]])
        df_abstract_phase$split_C_Avg[k] <- mean(df_c$split_percentage[df_c$time >= df_abstract_phase$time[k] & df_c$time < df_abstract_phase$time[k+1]])
        df_abstract_phase$split_D_Avg[k] <- mean(df_d$split_percentage[df_d$time >= df_abstract_phase$time[k] & df_d$time < df_abstract_phase$time[k+1]])
        df_abstract_phase$split_E_Avg[k] <- mean(df_e$split_percentage[df_e$time >= df_abstract_phase$time[k] & df_e$time < df_abstract_phase$time[k+1]])
        df_abstract_phase$split_F_Avg[k] <- mean(df_f$split_percentage[df_f$time >= df_abstract_phase$time[k] & df_f$time < df_abstract_phase$time[k+1]])
        df_abstract_phase$split_G_Avg[k] <- mean(df_g$split_percentage[df_g$time >= df_abstract_phase$time[k] & df_g$time < df_abstract_phase$time[k+1]])
        df_abstract_phase$split_H_Avg[k] <- mean(df_h$split_percentage[df_h$time >= df_abstract_phase$time[k] & df_h$time < df_abstract_phase$time[k+1]])
        # df_abstract_phase$split_I_Avg[k] <- mean(df_i$split_percentage[df_i$time >= df_abstract_phase$time[k] & df_i$time < df_abstract_phase$time[k+1]])
      }
      df_abstract_phase$split_A_Avg[nrow(df_abstract_phase)] <- mean(df_a$split_percentage[df_a$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
      df_abstract_phase$split_B_Avg[nrow(df_abstract_phase)] <- mean(df_b$split_percentage[df_b$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
      df_abstract_phase$split_C_Avg[nrow(df_abstract_phase)] <- mean(df_c$split_percentage[df_c$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
      df_abstract_phase$split_D_Avg[nrow(df_abstract_phase)] <- mean(df_d$split_percentage[df_d$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
      df_abstract_phase$split_E_Avg[nrow(df_abstract_phase)] <- mean(df_e$split_percentage[df_e$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
      df_abstract_phase$split_F_Avg[nrow(df_abstract_phase)] <- mean(df_f$split_percentage[df_d$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
      df_abstract_phase$split_G_Avg[nrow(df_abstract_phase)] <- mean(df_g$split_percentage[df_e$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
      df_abstract_phase$split_H_Avg[nrow(df_abstract_phase)] <- mean(df_h$split_percentage[df_d$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
      # df_abstract_phase$split_I_Avg[nrow(df_abstract_phase)] <- mean(df_i$split_percentage[df_e$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
      
      df_abstract_phase$A_sec <- as.integer(round(df_abstract_phase$split_A_Avg*df_abstract_phase$proposed),0)
      df_abstract_phase$B_sec <- as.integer(round(df_abstract_phase$split_B_Avg*df_abstract_phase$proposed),0)
      df_abstract_phase$C_sec <- as.integer(round(df_abstract_phase$split_C_Avg*df_abstract_phase$proposed),0)
      df_abstract_phase$D_sec <- as.integer(df_abstract_phase$split_D_Avg*df_abstract_phase$proposed)
      df_abstract_phase$E_sec <- as.integer(df_abstract_phase$split_E_Avg*df_abstract_phase$proposed)
      df_abstract_phase$F_sec <- as.integer(df_abstract_phase$split_F_Avg*df_abstract_phase$proposed)
      df_abstract_phase$G_sec <- as.integer(df_abstract_phase$split_G_Avg*df_abstract_phase$proposed)
      df_abstract_phase$H_sec <- as.integer(df_abstract_phase$split_H_Avg*df_abstract_phase$proposed)
      # df_abstract_phase$I_sec <- as.integer(df_abstract_phase$split_I_Avg*df_abstract_phase$proposed)
    }
    if(number_phases == 9){
      df_a <- dt[dt$phases=="<A>",]
      df_b <- dt[dt$phases=="B",]
      df_c <- dt[dt$phases=="C",]
      df_d <- dt[dt$phases=="D",]
      df_e <- dt[dt$phases=="E",]
      df_f <- dt[dt$phases=="F",]
      df_g <- dt[dt$phases=="G",]
      df_h <- dt[dt$phases=="H",]
      df_i <- dt[dt$phases=="I",]
      df_abstract_phase$split_A_Avg <- 0
      df_abstract_phase$split_B_Avg <- 0
      df_abstract_phase$split_C_Avg <- 0
      df_abstract_phase$split_D_Avg <- 0
      df_abstract_phase$split_E_Avg <- 0
      df_abstract_phase$split_F_Avg <- 0
      df_abstract_phase$split_G_Avg <- 0
      df_abstract_phase$split_H_Avg <- 0
      df_abstract_phase$split_I_Avg <- 0
      
      for (k in 1:nrow(df_abstract_phase)-1){
        df_abstract_phase$split_A_Avg[k] <- mean(df_a$split_percentage[df_a$time >= df_abstract_phase$time[k] & df_a$time < df_abstract_phase$time[k+1]])
        df_abstract_phase$split_B_Avg[k] <- mean(df_b$split_percentage[df_b$time >= df_abstract_phase$time[k] & df_b$time < df_abstract_phase$time[k+1]])
        df_abstract_phase$split_C_Avg[k] <- mean(df_c$split_percentage[df_c$time >= df_abstract_phase$time[k] & df_c$time < df_abstract_phase$time[k+1]])
        df_abstract_phase$split_D_Avg[k] <- mean(df_d$split_percentage[df_d$time >= df_abstract_phase$time[k] & df_d$time < df_abstract_phase$time[k+1]])
        df_abstract_phase$split_E_Avg[k] <- mean(df_e$split_percentage[df_e$time >= df_abstract_phase$time[k] & df_e$time < df_abstract_phase$time[k+1]])
        df_abstract_phase$split_F_Avg[k] <- mean(df_f$split_percentage[df_f$time >= df_abstract_phase$time[k] & df_f$time < df_abstract_phase$time[k+1]])
        df_abstract_phase$split_G_Avg[k] <- mean(df_g$split_percentage[df_g$time >= df_abstract_phase$time[k] & df_g$time < df_abstract_phase$time[k+1]])
        df_abstract_phase$split_H_Avg[k] <- mean(df_h$split_percentage[df_h$time >= df_abstract_phase$time[k] & df_h$time < df_abstract_phase$time[k+1]])
        df_abstract_phase$split_I_Avg[k] <- mean(df_i$split_percentage[df_i$time >= df_abstract_phase$time[k] & df_i$time < df_abstract_phase$time[k+1]])
      }
      df_abstract_phase$split_A_Avg[nrow(df_abstract_phase)] <- mean(df_a$split_percentage[df_a$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
      df_abstract_phase$split_B_Avg[nrow(df_abstract_phase)] <- mean(df_b$split_percentage[df_b$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
      df_abstract_phase$split_C_Avg[nrow(df_abstract_phase)] <- mean(df_c$split_percentage[df_c$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
      df_abstract_phase$split_D_Avg[nrow(df_abstract_phase)] <- mean(df_d$split_percentage[df_d$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
      df_abstract_phase$split_E_Avg[nrow(df_abstract_phase)] <- mean(df_e$split_percentage[df_e$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
      df_abstract_phase$split_F_Avg[nrow(df_abstract_phase)] <- mean(df_f$split_percentage[df_d$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
      df_abstract_phase$split_G_Avg[nrow(df_abstract_phase)] <- mean(df_g$split_percentage[df_e$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
      df_abstract_phase$split_H_Avg[nrow(df_abstract_phase)] <- mean(df_h$split_percentage[df_d$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
      df_abstract_phase$split_I_Avg[nrow(df_abstract_phase)] <- mean(df_i$split_percentage[df_e$time >= df_abstract_phase$time[nrow(df_abstract_phase)]])
      
      df_abstract_phase$A_sec <- as.integer(round(df_abstract_phase$split_A_Avg*df_abstract_phase$proposed),0)
      df_abstract_phase$B_sec <- as.integer(round(df_abstract_phase$split_B_Avg*df_abstract_phase$proposed),0)
      df_abstract_phase$C_sec <- as.integer(round(df_abstract_phase$split_C_Avg*df_abstract_phase$proposed),0)
      df_abstract_phase$D_sec <- as.integer(df_abstract_phase$split_D_Avg*df_abstract_phase$proposed)
      df_abstract_phase$E_sec <- as.integer(df_abstract_phase$split_E_Avg*df_abstract_phase$proposed)
      df_abstract_phase$F_sec <- as.integer(df_abstract_phase$split_F_Avg*df_abstract_phase$proposed)
      df_abstract_phase$G_sec <- as.integer(df_abstract_phase$split_G_Avg*df_abstract_phase$proposed)
      df_abstract_phase$H_sec <- as.integer(df_abstract_phase$split_H_Avg*df_abstract_phase$proposed)
      df_abstract_phase$I_sec <- as.integer(df_abstract_phase$split_I_Avg*df_abstract_phase$proposed)
    }
    
    df_abstract_phase$time <- hms::as_hms(df_abstract_phase$time)
    df_abstract_phase$time <- as.character(df_abstract_phase$time)
    output$contents_8_4 <- renderTable( 
      df_abstract_phase,
      caption = paste0("Final Results for Site ",input$site_historical_8),
      caption.placement = getOption("xtable.caption.placement", "top")
    )
  }
  )
  observeEvent(input$previewMap, {
  df_map <- as.data.frame(sf::read_sf(paste0("https://github.com/abdallahshabarek/ITS_NJDOT/releases/download/Incidents/incident_",as.integer(as.integer(as.POSIXct(Sys.time()))/300)*300,".gpkg")))
  reactive_data_chrono <- reactive({
    df_map
  })
  

  
  
  
  # static backround map
  output$map1 <- renderLeaflet({
    
    leaflet(df_map) %>%
      addProviderTiles(providers$CartoDB.DarkMatterNoLabels) %>%
      # addTiles() %>%
      # fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat))
      fitBounds(~-75.167, ~39.695, ~-74.202, ~40.774)
    #-75.167,39.695
    #-74.202,40.774
  })  
  
  # reactive circles map
  # observe({
    leafletProxy("map1", data = reactive_data_chrono()) %>%
      clearShapes() %>%
      addMarkers(lng=~long,
                 lat=~lat,
                 layerId = ~Details,
                 popup = paste0(reactive_data_chrono()$datetime," ",reactive_data_chrono()$Type," ",reactive_data_chrono()$Details)
                 ) # Assigning df id to layerid
  # })
  })
  
}

shinyApp(ui = ui, server = server)