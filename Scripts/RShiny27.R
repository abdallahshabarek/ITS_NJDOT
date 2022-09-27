


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
library(shinyTime)
library(grid)
library(leafpop)
library(scales)
library(leaflet.multiopacity)
library(leafem)


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
        # menuItem("Detector Alarms", tabName = "Detector_Alarms", icon = icon("cog")),
        menuItem("Degree of Saturation Analysis", tabName = "Degree_Saturation", icon = icon("battery-full")),
        menuItem("Degree of Saturation over Phase Timing Analysis", tabName = "Degree_Saturation_Phase", icon = icon("list-alt")),
        # menuItem("Communication, Falling Back, Bad Data & Flashing Yellow Analysis", tabName = "Comm_Analysis", icon = icon("list-alt")),
        # menuItem("Offset Analysis", tabName = "Offset_Analysis", icon = icon("list-alt")),
        # menuItem("TOD Analysis", tabName = "TOD_analysis", icon = icon("list-alt")),
        # menuItem("Incidents Monitoring", tabName = "Incident1", icon = icon("cog")),
        menuItem("Travel Time - Variation Routine Analysis", tabName = "travel_time_VR", icon = icon("list-alt")),
        menuItem("MX Value & Actual Split Percentage", tabName = "MX_Value_Split_Percentage", icon = icon("list-alt")),
        menuItem("Pedestrain Movements Analysis", tabName = "ped_movement_analysis", icon = icon("list-alt")),
        menuItem("Bottleneck Ranking Analysis", tabName = "bottleneck_ranking_analysis", icon = icon("list-alt")),
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
                             list(`Type of Analysis` = list("Two Axis","Ratio"))),
                selectInput("degree_saturation_analysis_type2", "Choose Type of Details:",
                            list(`Type of Analysis` = list("Phases"
                                                           ,"Signal Groups"
                                                           )))
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
                  selectInput("file_8_avg_or_percenile", "Average or Percentile:",
                              c("Percentile" = "percentile",
                                "Average" = "average")),
                  div(style="display:inline-block",textInput(inputId="file8_zoneid", label="Zone Number (0 if unknown)", placeholder = 0, width=160, value = 0)),
                  div(style="display:inline-block",textInput(inputId="file8_max_periods", label="Maximum number of periods", placeholder = 7, width=160, value = 7)),
                  div(style="display:inline-block",textInput(inputId="file8_min_time", label="Minimum number of hours per period", placeholder = 2, width=160, value = 2)),
                  div(style="display:inline-block",textInput(inputId="file8_max_time", label="Maximum number of hours per period", placeholder = 4, width=160, value = 4)),
                  div(style="display:inline-block",textInput(inputId="file8_percentile", label="Preferred percentile", placeholder = 0.8, width=160, value = 0.7)),
                  div(style="display:inline-block",textInput(inputId="file8_rounded_cl", label="Rounded cycle length", placeholder = 5, width=160, value = 5)),
                  selectInput("file_8_smooth_or_not", "Do we Smooth the Cycle Length:",
                              c("No" = "no",
                                "Yes" = "yes")),
                  div(style="display:inline-block",textInput(inputId="file8_smoothed_value_cl", label="Smoothing between consecutive cycle lengths", placeholder = 10, width=320, value = 10))

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
                selectInput("file_8_5_min_type", "Do you need to consider minimum phasing time:",
                            c("No" = "No",
                              "Yes" = "Yes")),
                # div(style="display:inline-block",textInput(inputId="file8_5_min_A_%", label="Min Phase A %", placeholder = 10, width=160, value = 10)),
                # div(style="display:inline-block",textInput(inputId="file8_5_min_A_sec", label="Min Phase A (sec)", placeholder = 10, width=160, value = 10)),
                # div(style="display:inline-block",textInput(inputId="file8_5_min_B_per", label="Min Phase B %", placeholder = 10, width=160, value = 10)),
                div(style="display:inline-block",textInput(inputId="file8_5_min_B_sec", label="Min Phase B (sec)", placeholder = 10, width=160, value = 10)),
                # div(style="display:inline-block",textInput(inputId="file8_5_min_C_per", label="Min Phase C %", placeholder = 10, width=160, value = 10)),
                div(style="display:inline-block",textInput(inputId="file8_5_min_C_sec", label="Min Phase C (sec)", placeholder = 10, width=160, value = 10)),
                # div(style="display:inline-block",textInput(inputId="file8_5_min_D_per", label="Min Phase D %", placeholder = 10, width=160, value = 10)),
                div(style="display:inline-block",textInput(inputId="file8_5_min_D_sec", label="Min Phase D (sec)", placeholder = 10, width=160, value = 10)),
                # div(style="display:inline-block",textInput(inputId="file8_5_min_E_per", label="Min Phase E %", placeholder = 10, width=160, value = 10)),
                div(style="display:inline-block",textInput(inputId="file8_5_min_E_sec", label="Min Phase E (sec)", placeholder = 10, width=160, value = 10)),
                # div(style="display:inline-block",textInput(inputId="file8_5_min_F_per", label="Min Phase F %", placeholder = 10, width=160, value = 10)),
                div(style="display:inline-block",textInput(inputId="file8_5_min_F_sec", label="Min Phase F (sec)", placeholder = 10, width=160, value = 10)),
                # div(style="display:inline-block",textInput(inputId="file8_5_min_G_per", label="Min Phase G %", placeholder = 10, width=160, value = 10)),
                div(style="display:inline-block",textInput(inputId="file8_5_min_G_sec", label="Min Phase G (sec)", placeholder = 10, width=160, value = 10)),
                # div(style="display:inline-block",textInput(inputId="file8_5_min_H_per", label="Min Phase H %", placeholder = 10, width=160, value = 10)),
                div(style="display:inline-block",textInput(inputId="file8_5_min_H_sec", label="Min Phase H (sec)", placeholder = 10, width=160, value = 10)),
                # div(style="display:inline-block",textInput(inputId="file8_5_min_I_per", label="Min Phase I %", placeholder = 10, width=160, value = 10)),
                div(style="display:inline-block",textInput(inputId="file8_5_min_I_sec", label="Min Phase I (sec)", placeholder = 10, width=160, value = 10)),
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
  tabItem(tabName = "travel_time_VR",
          fluidPage(
            titlePanel("Travel Time / Variation Routine Analysis"),
            
            sidebarLayout(
              sidebarPanel(
                bootstrapPage(airDatepickerInput(
                  inputId = "date9_1_multiple",
                  label = "Select the first date:",
                  placeholder = "You can pick a date",
                  multiple = TRUE, clearButton = TRUE
                  ),
                  airDatepickerInput(
                    inputId = "date9_2_multiple",
                    label = "Select the second date:",
                    placeholder = "You can pick a date",
                    multiple = TRUE, clearButton = TRUE
                  ),
                   div(style="display:inline-block",timeInput("file9_2_lower_time", "Lower Time", strptime("14:00:00","%T")))
                  ,div(style="display:inline-block",timeInput("file9_2_upper_time", "Upper Time", strptime("18:00:00","%T")))
                  ,textAreaInput("file9_2_TMClist","Type TMCs at every line (Enter to go to next line)")
                  ,selectInput("file_9_2_direction", "Select the TMC direction:",
                              c("Eastbound" = "EB",
                                "Westbound" = "WB",
                                "Northbound" = "NB",
                                "Southbound" = "SB"))
                  ,div(style="display:inline-block",textInput(inputId="file9_2_site_ID", label="Site ID", placeholder = 10012, width=160, value = 10012))
                  ,div(style="display:inline-block",textInput(inputId="file9_2_var1", label="Variation 1", placeholder = 128, width=160, value = 128))
                  ,div(style="display:inline-block",textInput(inputId="file9_2_var2", label="Variation 2", placeholder = 129, width=160, value = 129))
                  ,fileInput("file9_2_ritis_travel_time_file",
                            "Upload RITIS Travel Time CSV file",
                            multiple = FALSE,
                            accept=c('text/csv', 
                                     'text/comma-separated-values,text/plain', 
                                     '.csv'))
                  ,fileInput("file9_2_ritis_tmc_identification",
                             "Upload RITIS TMC Identification CSV file",
                             multiple = FALSE,
                             accept=c('text/csv', 
                                      'text/comma-separated-values,text/plain', 
                                      '.csv'))
                  ,fileInput("file9_2_SCATS_log",
                             "Upload SCATS log as CSV file",
                             multiple = FALSE,
                             accept=c('text/csv', 
                                      'text/comma-separated-values,text/plain', 
                                      '.csv'))
                  ,selectInput("Rt_file_9", "Route Name:",
                               c("US-1" = "RT1",
                                 "NJ-18" = "RT18",
                                 "NJ-73" = "RT73",
                                 "US-130" = "RT130"))
                  ,fileInput("file9_2_sm_files",
                             "Upload Strategic Monitoring files file",
                             multiple = TRUE,
                             accept=c('text/csv', 
                                      'text/comma-separated-values,text/plain', 
                                      '.csv'))
                  ,actionButton('previewData9','Preview TT', icon = icon("refresh"))
                  ,actionButton('previewData9_2','Preview DS', icon = icon("refresh"))
                  ,downloadButton('downloadData9', 'Download')
                )
              ),
              mainPanel(plotOutput("Plot9_1")
                       ,plotOutput("Plot9_2"))
            )
          )
          ),
  tabItem(tabName = "MX_Value_Split_Percentage",
          fluidPage(
            titlePanel("MX Value / Actual Split Percentage Analysis"),
            
            sidebarLayout(
              sidebarPanel(
                bootstrapPage(
                 div(style="display:inline-block",textInput(inputId="file11_2_site_ID", label="Site ID", placeholder = 10012, width=100, value = 10012))
                ,p("                                                       ")
                ,div(style="display:inline-block",timeInput("file11_2_lower_time", "Lower Time", strptime("14:00:00","%T")))
                ,div(style="display:inline-block",timeInput("file11_2_upper_time", "Upper Time", strptime("18:00:00","%T")))
                ,selectInput("file_11_2_phase_selected", "Select the Phase:",
                             c("Phase A" = "A",
                               "Phase B" = "B",
                               "Phase C" = "C",
                               "Phase D" = "D",
                               "Phase E" = "E",
                               "Phase F" = "F",
                               "Phase G" = "G",
                               "Phase H" = "H",
                               "Phase I" = "I"
                               ))
                ,fileInput("file11_2_event_file",
                           "Upload CSV Event File from History Viewer",
                           multiple = FALSE,
                           accept=c('text/csv', 
                                    'text/comma-separated-values,text/plain', 
                                    '.csv'))
                ,fileInput("file11_2_phase_file",
                           "Upload CSV Phase File from History Viewer",
                           multiple = FALSE,
                           accept=c('text/csv', 
                                    'text/comma-separated-values,text/plain', 
                                    '.csv'))
                ,actionButton('previewData11','Preview MX', icon = icon("refresh"))
                ,actionButton('previewData11_2','Preview Phase%', icon = icon("refresh"))
                ,downloadButton('downloadData11_1', 'Download MX')
                ,downloadButton('downloadData11_2', 'Download Phase%')
                )
              ),
              mainPanel(plotOutput("Plot11_1"),
                        plotOutput("Plot11_2"))
            )
          )
  ),
    tabItem(tabName = "bottleneck_ranking_analysis",
            fluidPage(
              titlePanel("Bottleneck Ranking Analysis"),
              sidebarLayout(
                sidebarPanel(
                  bootstrapPage(
                    # div(style="display:inline-block",textInput(inputId="lowertime", label="Enter the lower time bound", placeholder = "06:00:00", width=120, value = "06:00:00")),
                    # div(style="display:inline-block",textInput(inputId="uppertime", label="Enter the upper time bound", placeholder = "21:00:00", width=120, value = "21:00:00")),
                    # div(style="display:inline-block",textInput(inputId="max_value_threshold", label="Enter the Max Value Threshold", placeholder = 15, width=120, value = 15))#,
                    # div(style="display:inline-block",textInput(inputId="rounded_every", label="Enter the Max Cycle Length (minutes)", placeholder = 6, width=180, value = 6)),
                    # div(style="display:inline-block",textInput(inputId="coef1", label="Enter the first coefficient", placeholder = 0.9, width=120, value = 0.9)),
                    # div(style="display:inline-block",textInput(inputId="coef2", label="Enter the second coefficient", placeholder = 1.1, width=120, value = 1.1))
                  # ,
                  fileInput("bottlneck_file1",
                            "Choose the first file (i.e., the before file) from a directory",
                            multiple = FALSE,
                            accept=c('text/csv',
                                     'text/comma-separated-values,text/plain',
                                     '.csv')),
                  fileInput("bottlneck_file2",
                            "Choose the second file (i.e., the after file) from a directory",
                            multiple = FALSE,
                            accept=c('text/csv',
                                     'text/comma-separated-values,text/plain',
                                     '.csv')),
                  selectInput("bottleneck_ranking_threshold_1", "Analysis Results based on: ",
                              c("Total Congested Duration (Days)" = "total_duration",
                                "Average Congested Duration (Hours)" = "avg_duration",
                                "Agency Reported Events" = "agency_reported_events",
                                "Volume Estimates" = "volume_estimate",
                                "Base Impact" = "base_impact",
                                "Speed Differential" = "speed_differential",
                                "Congestion" = "congestion",
                                "Total Delay" = "total_delay"
                                )
                              ),
                  div(style="display:inline-block",numericInput(inputId="bottleneck_avg_daily_threshold", label="Minimum Avg Daily Congestion Duration (hours)", width=330, value = 1, min=0)),
                  div(style="display:inline-block",numericInput(inputId="bottleneck_total_threshold", label="Minimum Total Congestion Duration (Days)", width=330, value = 0, min=0)),
                  p(""),
                  div(style="display:inline-block",numericInput(inputId="bottleneck_agency_reported_events_threshold", label="Minimum Agency Reported Events", width=240, value = 0)),
                  div(style="display:inline-block",numericInput(inputId="bottleneck_volume_estimate_threshold", label="Minimum Volume Estimates", width=240, value = 0, min=0)),
                  div(style="display:inline-block",numericInput(inputId="bottleneck_base_impact_threshold", label="Minimum Base Impact", width=240, value = 0, min=0)),
                  div(style="display:inline-block",numericInput(inputId="bottleneck_speed_differential_threshold", label="Minimum Speed Differential", width=240, value = 0, min=0)),
                  div(style="display:inline-block",numericInput(inputId="bottleneck_congestion_threshold", label="Minimum Congestion", width=240, value = 0, min=0)),
                  div(style="display:inline-block",numericInput(inputId="bottleneck_total_delay_threshold", label="Minimum Total Delay", width=240, value = 0, min=0)),
                  p(""),
                  div(style="display:inline-block",numericInput(inputId="circle_size_bottleneck_ranking", label="Circle Size (Only For Vizualization)", value = 10, width=300, min=0.01, max=100)),
                  p(""),
                  actionButton('previewData_bottleneck_ranking','Preview', icon = icon("refresh")),
                  downloadButton('downloadData_bottleneck_ranking', 'Download')
                )
                ),
                mainPanel(
                  # tag.map.title <- tags$style(HTML("
                  #     .leaflet-control.map-title { 
                  #       transform: translate(-50%,20%);
                  #       position: fixed !important;
                  #       left: 50%;
                  #       text-align: center;
                  #       padding-left: 10px; 
                  #       padding-right: 10px; 
                  #       background: rgba(255,255,255,0.75);
                  #       font-weight: bold;
                  #       font-size: 28px;
                  #     }
                  #   ")),
                  # title <- tags$div(
                  #   tag.map.title, HTML("Map title")
                  # )  ,
                  
                  leafletOutput("bottleneck_ranking_map",height = "110vh")
                )
                
              )
            )
    ),
  tabItem(tabName = "ped_movement_analysis",
          fluidPage(
            titlePanel("Pedestrian Movements Analysis"),
            
            sidebarLayout(
              sidebarPanel(
                bootstrapPage(
                  div(style="display:inline-block",textInput(inputId="file12_2_site_ID", label="Site ID", placeholder = 10012, width=100, value = 10012))
                  ,p("                                                       ")
                  ,div(style="display:inline-block",timeInput("file12_2_lower_time", "Lower Time", strptime("14:00:00","%T")))
                  ,div(style="display:inline-block",timeInput("file12_2_upper_time", "Upper Time", strptime("18:00:00","%T")))
                  ,div(style="display:inline-block",textInput(inputId="file_12_2_ped_phase_selected", label="Pedestrian Movement Number", placeholder = 4, width=220, value = 4))
                  ,fileInput("file12_2_event_file",
                             "Upload CSV Event File from History Viewer",
                             multiple = FALSE,
                             accept=c('text/csv', 
                                      'text/comma-separated-values,text/plain', 
                                      '.csv'))
                  ,actionButton('previewData12','Preview', icon = icon("refresh"))
                  ,downloadButton('downloadData12_1', 'Download')
                )
              ),
              mainPanel(plotOutput("Plot12_1"),
                        plotOutput("Plot12_2"))
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
                tags$a(href="https://rawcdn.githack.com/shabarekjpcl/ITS_NJDOT/e3a8e73121e0096cafa4b8b1a39bbe0787a0d01b/Resources/Detector%20Alarms%20Instructions.pdf", "MX Value Instructions"),
                tags$br(),
                tags$a(href="https://rawcdn.githack.com/shabarekjpcl/ITS_NJDOT/e3a8e73121e0096cafa4b8b1a39bbe0787a0d01b/Resources/Degree%20of%20Saturation%20Analysis%20Sample%20Data.zip", "MX Value Sample Data"),
                tags$br(),
                tags$a(href="https://rawcdn.githack.com/shabarekjpcl/ITS_NJDOT/e3a8e73121e0096cafa4b8b1a39bbe0787a0d01b/Resources/Detector%20Alarms%20Instructions.pdf", "Detector Alarms Instructions"),
                tags$br(),
                tags$a(href="https://rawcdn.githack.com/shabarekjpcl/ITS_NJDOT/e3a8e73121e0096cafa4b8b1a39bbe0787a0d01b/Resources/Detector%20Alarm%20Sample%20Data.zip", "Detector Alarms Sample Data"),
                tags$br(),
                tags$a(href="https://rawcdn.githack.com/shabarekjpcl/ITS_NJDOT/e3a8e73121e0096cafa4b8b1a39bbe0787a0d01b/Resources/Degree%20of%20Saturation%20Analysis%20Instructions.pdf", "Degree of Saturation Analysis Instructions"),
                tags$br(),
                tags$a(href="https://rawcdn.githack.com/shabarekjpcl/ITS_NJDOT/e3a8e73121e0096cafa4b8b1a39bbe0787a0d01b/Resources/Degree%20of%20Saturation%20Analysis%20Sample%20Data.zip", "Degree of Saturation Analysis Sample Data"),
                tags$br(),
                tags$a(href="https://rawcdn.githack.com/shabarekjpcl/ITS_NJDOT/e3a8e73121e0096cafa4b8b1a39bbe0787a0d01b/Resources/Degree%20of%20Saturation%20over%20Phase%20Timing%20Analysis%20Instructions.pdf", "Degree of Saturation over Phase Timing Analysis Instructions"),
                tags$br(),
                tags$a(href="https://rawcdn.githack.com/shabarekjpcl/ITS_NJDOT/e3a8e73121e0096cafa4b8b1a39bbe0787a0d01b/Resources/Degree%20of%20Saturation%20over%20Phase%20Timing%20Analysis%20Sample%20Data.txt","Degree of Saturation over Phase Timing Analysis Sample Data"),
                tags$br(),
                tags$a(href="https://rawcdn.githack.com/shabarekjpcl/ITS_NJDOT/d39a417fcc4e1ada81e761a12fa7c26b005538d7/Resources/Instructions%20for%20Travel%20Time.pdf","Travel Time / Variation Routine Analysis"),
                tags$br(),
                tags$a(href="https://rawcdn.githack.com/shabarekjpcl/ITS_NJDOT/0d040e9ccee9b744ec006ceb234558b75330eb62/Resources/Instructions%20for%20MX%20Value%20and%20Actual%20Phase%20Percentage.pdf","MX Value & Actual Split % Analysis"),
                tags$br(),
                tags$a(href="https://rawcdn.githack.com/shabarekjpcl/ITS_NJDOT/e85b87584b2beeb40a256e0dbff2df740cf0b85d/Resources/TOD%20Analysis%20Instructions.pdf","TOD Analysis Instructions"),
                tags$br()
                # ,
                # tags$a(href="https://rawcdn.githack.com/abdallahshabarek/ITS_NJDOT/c4842a30a369cd431264716ed20b1c7446992320/Resources/SCATS%20Log%20user%20manual.pdf", "SCATS Log User Manual"),
                # tags$br(),
                # tags$a(href="https://rawcdn.githack.com/abdallahshabarek/ITS_NJDOT/c4842a30a369cd431264716ed20b1c7446992320/Resources/SCATS%20operating%20instructions.pdf", "SCATS Operating Instructions"),
                # tags$br(),
                # tags$a(href="https://rawcdn.githack.com/abdallahshabarek/ITS_NJDOT/c4842a30a369cd431264716ed20b1c7446992320/Resources/SCATS%C2%AE%20Troubleshooting.pdf", "SCATS Troubleshooting"),
                # tags$br(),
                # tags$a(href="https://rawcdn.githack.com/abdallahshabarek/ITS_NJDOT/c4842a30a369cd431264716ed20b1c7446992320/Resources/SCATS%C2%AE.pdf", "SCATS Presentation"),
                # tags$br(),
                # tags$a(href="https://rawcdn.githack.com/abdallahshabarek/ITS_NJDOT/01cbea9a015ed13fe7a1f6bbba5c021338d08a31/Resources/SCATS%20Traffic%20Reporter%20user%20manual.pdf", "SCATS Traffic Reporter User Manual"),
                # tags$br(),
                # tags$a(href="https://rawcdn.githack.com/abdallahshabarek/ITS_NJDOT/01cbea9a015ed13fe7a1f6bbba5c021338d08a31/Resources/ATMS%20Rules%201.1.rwz", "ATMS Rules")
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
        g <- g + ylab("Avg DS") + xlab("Time")
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
        g <- g + ylab("Avg DS") + xlab("Time")
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
        g <- g + ylab("Avg DS") + xlab("Time")
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
        g <- g + ylab("Avg DS") + xlab("Time")
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
            scale_y_continuous(sec.axis = sec_axis(~.*1, name="Phase Split Percentage (%) (Dots .....)"),labels = seq(0, 140,by=10), breaks = seq(0, 140,by=10)) 
          g <- g + geom_line( aes(y=y22),linetype = "dotted", size=1, color="darkblue") +
            scale_y_continuous(sec.axis = sec_axis(~.*1, name="Phase Split Percentage (%) (Dots .....)"),labels = seq(0, 140,by=10), breaks = seq(0, 140,by=10))
          
          g <- g + scale_color_manual(name = "Phases", values = c("red","darkblue"))
          # g <- g + scale_color_manual(name = "Phase B", values = c("darkblue"))
          g <- g + ylab("Average Degree of Saturation % (Solid ____)") + xlab("Time")
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
            scale_y_continuous(sec.axis = sec_axis(~.*1, name="Phase Split Percentage (%) (Dots .....)"),labels = seq(0, 140,by=10), breaks = seq(0, 140,by=10)) 
          g <- g + geom_line( aes(y=y22),linetype = "dotted", size=1, color="darkblue") +
            scale_y_continuous(sec.axis = sec_axis(~.*1, name="Phase Split Percentage (%) (Dots .....)"),labels = seq(0, 140,by=10), breaks = seq(0, 140,by=10))
          g <- g + geom_line( aes(y=y33),linetype = "dotted", size=1, color="darkgreen") +
            scale_y_continuous(sec.axis = sec_axis(~.*1, name="Phase Split Percentage (%) (Dots .....)"),labels = seq(0, 140,by=10), breaks = seq(0, 140,by=10))
          g <- g + scale_color_manual(name = "Phases", values = c("red","darkblue","darkgreen"))
          
          g <- g + ylab("Average Degree of Saturation % (Solid ____)") + xlab("Time")
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
            scale_y_continuous(sec.axis = sec_axis(~.*1, name="Phase Split Percentage (%) (Dots .....)"),labels = seq(0, 140,by=10), breaks = seq(0, 140,by=10)) 
          g <- g + geom_line( aes(y=y22),linetype = "dotted", size=1, color="darkblue") +
            scale_y_continuous(sec.axis = sec_axis(~.*1, name="Phase Split Percentage (%) (Dots .....)"),labels = seq(0, 140,by=10), breaks = seq(0, 140,by=10))
          g <- g + geom_line( aes(y=y33),linetype = "dotted", size=1, color="darkgreen") +
            scale_y_continuous(sec.axis = sec_axis(~.*1, name="Phase Split Percentage (%) (Dots .....)"),labels = seq(0, 140,by=10), breaks = seq(0, 140,by=10))
          g <- g + geom_line( aes(y=y44),linetype = "dotted", size=1, color="orange") +
            scale_y_continuous(sec.axis = sec_axis(~.*1, name="Phase Split Percentage (%) (Dots .....)"),labels = seq(0, 140,by=10), breaks = seq(0, 140,by=10))
          
          g <- g + scale_color_manual(name = "Phases", values = c("red","darkblue","darkgreen","orange"))
          # g <- g + scale_color_manual(name = "Phase B", values = c("darkblue"))
          g <- g + ylab("Average Degree of Saturation % (Solid ____)") + xlab("Time")
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
            scale_y_continuous(sec.axis = sec_axis(~.*1, name="Phase Split Percentage (%) (Dots .....)"),labels = seq(0, 140,by=10), breaks = seq(0, 140,by=10)) 
          g <- g + geom_line( aes(y=y22),linetype = "dotted", size=1, color="darkblue") +
            scale_y_continuous(sec.axis = sec_axis(~.*1, name="Phase Split Percentage (%) (Dots .....)"),labels = seq(0, 140,by=10), breaks = seq(0, 140,by=10))
          g <- g + geom_line( aes(y=y33),linetype = "dotted", size=1, color="darkgreen") +
            scale_y_continuous(sec.axis = sec_axis(~.*1, name="Phase Split Percentage (%) (Dots .....)"),labels = seq(0, 140,by=10), breaks = seq(0, 140,by=10))
          g <- g + geom_line( aes(y=y44),linetype = "dotted", size=1, color="orange") +
            scale_y_continuous(sec.axis = sec_axis(~.*1, name="Phase Split Percentage (%) (Dots .....)"),labels = seq(0, 140,by=10), breaks = seq(0, 140,by=10))
          g <- g + geom_line( aes(y=y55),linetype = "dotted", size=1, color="cyan") +
            scale_y_continuous(sec.axis = sec_axis(~.*1, name="Phase Split Percentage (%)"),labels = seq(0, 140,by=10), breaks = seq(0, 140,by=10))
          
          g <- g + scale_color_manual(name = "Phases", values = c("red","darkblue","darkgreen","orange","cyan"))
          # g <- g + scale_color_manual(name = "Phase B", values = c("darkblue"))
          g <- g + ylab("Average Degree of Saturation % (Solid ____)") + xlab("Time")
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
          g <- g + ylab("Avg Degree of Saturation to Phase Percentage") + xlab("Time")
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
          g <- g + ylab("Avg Degree of Saturation to Phase Percentage") + xlab("Time")
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
          g <- g + ylab("Avg Degree of Saturation to Phase Percentage") + xlab("Time")
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
          g <- g + ylab("Avg Degree of Saturation to Phase Percentage") + xlab("Time")
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
          g <- g + ylab("Avg Degree of Saturation to Phase Percentage") + xlab("Time")
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
          g <- g + ylab("Avg Degree of Saturation to Phase Percentage") + xlab("Time")
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
          g <- g + ylab("Avg Degree of Saturation to Phase Percentage") + xlab("Time")
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
          g <- g + ylab("Avg Degree of Saturation to Phase Percentage") + xlab("Time")
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
          g <- g + ylab("Avg Degree of Saturation to Phase Percentage") + xlab("Time")
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
          g <- g + ylab("Avg DS") + xlab("Time")
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
          g <- g + ylab("Avg DS") + xlab("Time")
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
          g <- g + ylab("Avg Degree of Saturation to Phase Percentage") + xlab("Time")
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
          g <- g + ylab("Avg Degree of Saturation to Phase Percentage") + xlab("Time")
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
    offset_avg_df <- as.data.frame(input$dae7_multiple)
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
      
      if(input$file_8_avg_or_percenile == "average"){
        if(input$file_8_offpeak_cycles == "no"){
          source("https://raw.githubusercontent.com/abdallahshabarek/ITS_NJDOT/main/Scripts/function5_TOD_2.R")
          source("https://raw.githubusercontent.com/abdallahshabarek/ITS_NJDOT/main/Scripts/Optimization_Script_avg_diff_cycle.R")
        }
        if(input$file_8_offpeak_cycles == "yes"){
          source("https://raw.githubusercontent.com/abdallahshabarek/ITS_NJDOT/main/Scripts/function5_TOD_2.R")
          source("https://raw.githubusercontent.com/abdallahshabarek/ITS_NJDOT/main/Scripts/Optimization_Script_avg.R")
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
        
        dttt2 <- TOD_optimization(dataframe_avg = dttt,max_periods = as.integer(input$file8_max_periods), min_dur = as.integer(input$file8_min_time), max_dur = as.integer(input$file8_max_time), cycle_length_rounded = as.integer(input$file8_rounded_cl))
        
        # dirname(input$file8$datapath)[1]
      }
      if(input$file_8_avg_or_percenile == "percentile"){
        if(input$file_8_offpeak_cycles == "no"){
          source("https://raw.githubusercontent.com/abdallahshabarek/ITS_NJDOT/main/Scripts/function5_TOD_2.R")
          source("https://raw.githubusercontent.com/abdallahshabarek/ITS_NJDOT/main/Scripts/Optimization_Script_percentile_diff_cycle.R")
        }
        if(input$file_8_offpeak_cycles == "yes"){
          source("https://raw.githubusercontent.com/abdallahshabarek/ITS_NJDOT/main/Scripts/function5_TOD_2.R")
          source("https://raw.githubusercontent.com/abdallahshabarek/ITS_NJDOT/main/Scripts/Optimization_Script_percentile.R")
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
        
        dttt2 <- TOD_optimization(dataframe_avg = dttt,max_periods = as.integer(input$file8_max_periods), min_dur = as.integer(input$file8_min_time), max_dur = as.integer(input$file8_max_time),percentile = as.double(input$file8_percentile) ,cycle_length_rounded = as.integer(input$file8_rounded_cl))
        # dirname(input$file8$datapath)[1]
      }
      
      # Smoothing Starts
      if(input$file_8_smooth_or_not == "no"){
        dttt2 <- dttt2
      }
      if(input$file_8_smooth_or_not == "yes"){
        time_dttt_2 <- dttt2$time[1]
        proposed_dttt_2 <- dttt2$proposed[1]
        df_file8_abstract_2 <- data.frame(time_dttt_2,proposed_dttt_2)
        colnames(df_file8_abstract_2) <- c("time","proposed")
        for(k_file8_abstract_2 in 2:nrow(dttt2)){
          if((dttt2$proposed[k_file8_abstract_2] != dttt2$proposed[k_file8_abstract_2-1])==TRUE){
            print(k_file8_abstract_2)
            temp_file8_abstract_2 <- data.frame(cbind(dttt2$time[k_file8_abstract_2],dttt2$proposed[k_file8_abstract_2]))
            colnames(temp_file8_abstract_2) <- c("time","proposed")
            df_file8_abstract_2 <- rbind(df_file8_abstract_2,temp_file8_abstract_2)
          }
        }
        # df_file8_abstract$time <- hms::as_hms(df_file8_abstract$time*60)
        for(uu in (2:nrow(df_file8_abstract_2))){
          if(abs(df_file8_abstract_2$proposed[uu-1]-df_file8_abstract_2$proposed[uu]) <= as.integer(input$file8_smoothed_value_cl)){
            df_file8_abstract_2$proposed[uu-1] <- max(df_file8_abstract_2$proposed[uu], df_file8_abstract_2$proposed[uu-1])
          }
        }
        for(uu in (1:(nrow(df_file8_abstract_2)-1))){
          if(abs(df_file8_abstract_2$proposed[uu]-df_file8_abstract_2$proposed[uu+1]) <= as.integer(input$file8_smoothed_value_cl)){
            df_file8_abstract_2$proposed[uu+1] <- max(df_file8_abstract_2$proposed[uu+1], df_file8_abstract_2$proposed[uu])
          }
        }
        dttt3 <- dttt2
        dttt3$proposed <- NULL
        dttt3 <- dplyr::left_join(dttt3, df_file8_abstract_2, by=c("time" = "time"))
        dttt3 <- dttt3 %>% tidyr::fill(proposed, .direction = "downup")
        dttt2 <- dttt3
      }
      # Finished the smoothing
      
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
      
      if(input$file_8_avg_or_percenile == "average"){
        if(input$file_8_offpeak_cycles == "no"){
          source("https://raw.githubusercontent.com/abdallahshabarek/ITS_NJDOT/main/Scripts/function5_TOD_2.R")
          source("https://raw.githubusercontent.com/abdallahshabarek/ITS_NJDOT/main/Scripts/Optimization_Script_avg_diff_cycle.R")
        }
        if(input$file_8_offpeak_cycles == "yes"){
          source("https://raw.githubusercontent.com/abdallahshabarek/ITS_NJDOT/main/Scripts/function5_TOD_2.R")
          source("https://raw.githubusercontent.com/abdallahshabarek/ITS_NJDOT/main/Scripts/Optimization_Script_avg.R")
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
        
        dttt2 <- TOD_optimization(dataframe_avg = dttt,max_periods = as.integer(input$file8_max_periods), min_dur = as.integer(input$file8_min_time), max_dur = as.integer(input$file8_max_time), cycle_length_rounded = as.integer(input$file8_rounded_cl))
        # dirname(input$file8$datapath)[1]
      }
      if(input$file_8_avg_or_percenile == "percentile"){
        if(input$file_8_offpeak_cycles == "no"){
          source("https://raw.githubusercontent.com/abdallahshabarek/ITS_NJDOT/main/Scripts/function5_TOD_2.R")
          source("https://raw.githubusercontent.com/abdallahshabarek/ITS_NJDOT/main/Scripts/Optimization_Script_percentile_diff_cycle.R")
        }
        if(input$file_8_offpeak_cycles == "yes"){
          source("https://raw.githubusercontent.com/abdallahshabarek/ITS_NJDOT/main/Scripts/function5_TOD_2.R")
          source("https://raw.githubusercontent.com/abdallahshabarek/ITS_NJDOT/main/Scripts/Optimization_Script_percentile.R")
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
        
        dttt2 <- TOD_optimization(dataframe_avg = dttt,max_periods = as.integer(input$file8_max_periods), min_dur = as.integer(input$file8_min_time), max_dur = as.integer(input$file8_max_time),percentile = as.double(input$file8_percentile) ,cycle_length_rounded = as.integer(input$file8_rounded_cl))
        # dirname(input$file8$datapath)[1]
      }      
      # Smoothing Starts
      if(input$file_8_smooth_or_not == "no"){
        dttt2 <- dttt2
      }
      if(input$file_8_smooth_or_not == "yes"){
        time_dttt_2 <- dttt2$time[1]
        proposed_dttt_2 <- dttt2$proposed[1]
        df_file8_abstract_2 <- data.frame(time_dttt_2,proposed_dttt_2)
        colnames(df_file8_abstract_2) <- c("time","proposed")
        for(k_file8_abstract_2 in 2:nrow(dttt2)){
          if((dttt2$proposed[k_file8_abstract_2] != dttt2$proposed[k_file8_abstract_2-1])==TRUE){
            print(k_file8_abstract_2)
            temp_file8_abstract_2 <- data.frame(cbind(dttt2$time[k_file8_abstract_2],dttt2$proposed[k_file8_abstract_2]))
            colnames(temp_file8_abstract_2) <- c("time","proposed")
            df_file8_abstract_2 <- rbind(df_file8_abstract_2,temp_file8_abstract_2)
          }
        }
        # df_file8_abstract$time <- hms::as_hms(df_file8_abstract$time*60)
        for(uu in (2:nrow(df_file8_abstract_2))){
          if(abs(df_file8_abstract_2$proposed[uu-1]-df_file8_abstract_2$proposed[uu]) <= as.integer(input$file8_smoothed_value_cl)){
            df_file8_abstract_2$proposed[uu-1] <- max(df_file8_abstract_2$proposed[uu], df_file8_abstract_2$proposed[uu-1])
          }
        }
        for(uu in (1:(nrow(df_file8_abstract_2)-1))){
          if(abs(df_file8_abstract_2$proposed[uu]-df_file8_abstract_2$proposed[uu+1]) <= as.integer(input$file8_smoothed_value_cl)){
            df_file8_abstract_2$proposed[uu+1] <- max(df_file8_abstract_2$proposed[uu+1], df_file8_abstract_2$proposed[uu])
          }
        }
        dttt3 <- dttt2
        dttt3$proposed <- NULL
        dttt3 <- dplyr::left_join(dttt3, df_file8_abstract_2, by=c("time" = "time"))
        dttt3 <- dttt3 %>% tidyr::fill(proposed, .direction = "downup")
        dttt2 <- dttt3
      }
      # Finished the smoothing
      
      
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
    subsystema_file <- "https://raw.githubusercontent.com/abdallahshabarek/ITS_NJDOT/main/Site_Subsystem.csv"
    
    if(input$file_8_avg_or_percenile == "average"){
      if(input$file_8_offpeak_cycles == "no"){
        source("https://raw.githubusercontent.com/abdallahshabarek/ITS_NJDOT/main/Scripts/function5_TOD_2.R")
        source("https://raw.githubusercontent.com/abdallahshabarek/ITS_NJDOT/main/Scripts/Optimization_Script_avg_diff_cycle.R")
      }
      if(input$file_8_offpeak_cycles == "yes"){
        source("https://raw.githubusercontent.com/abdallahshabarek/ITS_NJDOT/main/Scripts/function5_TOD_2.R")
        source("https://raw.githubusercontent.com/abdallahshabarek/ITS_NJDOT/main/Scripts/Optimization_Script_avg.R")
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
      
      dttt2 <- TOD_optimization(dataframe_avg = dttt,max_periods = as.integer(input$file8_max_periods), min_dur = as.integer(input$file8_min_time), max_dur = as.integer(input$file8_max_time), cycle_length_rounded = as.integer(input$file8_rounded_cl))
      # dirname(input$file8$datapath)[1]
    }
    if(input$file_8_avg_or_percenile == "percentile"){
      if(input$file_8_offpeak_cycles == "no"){
        source("https://raw.githubusercontent.com/abdallahshabarek/ITS_NJDOT/main/Scripts/function5_TOD_2.R")
        source("https://raw.githubusercontent.com/abdallahshabarek/ITS_NJDOT/main/Scripts/Optimization_Script_percentile_diff_cycle.R")
      }
      if(input$file_8_offpeak_cycles == "yes"){
        source("https://raw.githubusercontent.com/abdallahshabarek/ITS_NJDOT/main/Scripts/function5_TOD_2.R")
        source("https://raw.githubusercontent.com/abdallahshabarek/ITS_NJDOT/main/Scripts/Optimization_Script_percentile.R")
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
      
      dttt2 <- TOD_optimization(dataframe_avg = dttt,max_periods = as.integer(input$file8_max_periods), min_dur = as.integer(input$file8_min_time), max_dur = as.integer(input$file8_max_time),percentile = as.double(input$file8_percentile) ,cycle_length_rounded = as.integer(input$file8_rounded_cl))
      # dirname(input$file8$datapath)[1]
    }  
    
    # Smoothing Starts
    if(input$file_8_smooth_or_not == "no"){
      dttt2 <- dttt2
    }
    if(input$file_8_smooth_or_not == "yes"){
      time_dttt_2 <- dttt2$time[1]
      proposed_dttt_2 <- dttt2$proposed[1]
      df_file8_abstract_2 <- data.frame(time_dttt_2,proposed_dttt_2)
      colnames(df_file8_abstract_2) <- c("time","proposed")
      for(k_file8_abstract_2 in 2:nrow(dttt2)){
        if((dttt2$proposed[k_file8_abstract_2] != dttt2$proposed[k_file8_abstract_2-1])==TRUE){
          print(k_file8_abstract_2)
          temp_file8_abstract_2 <- data.frame(cbind(dttt2$time[k_file8_abstract_2],dttt2$proposed[k_file8_abstract_2]))
          colnames(temp_file8_abstract_2) <- c("time","proposed")
          df_file8_abstract_2 <- rbind(df_file8_abstract_2,temp_file8_abstract_2)
        }
      }
      # df_file8_abstract$time <- hms::as_hms(df_file8_abstract$time*60)
      for(uu in (2:nrow(df_file8_abstract_2))){
        if(abs(df_file8_abstract_2$proposed[uu-1]-df_file8_abstract_2$proposed[uu]) <= as.integer(input$file8_smoothed_value_cl)){
          df_file8_abstract_2$proposed[uu-1] <- max(df_file8_abstract_2$proposed[uu], df_file8_abstract_2$proposed[uu-1])
        }
      }
      for(uu in (1:(nrow(df_file8_abstract_2)-1))){
        if(abs(df_file8_abstract_2$proposed[uu]-df_file8_abstract_2$proposed[uu+1]) <= as.integer(input$file8_smoothed_value_cl)){
          df_file8_abstract_2$proposed[uu+1] <- max(df_file8_abstract_2$proposed[uu+1], df_file8_abstract_2$proposed[uu])
        }
      }
      dttt3 <- dttt2
      dttt3$proposed <- NULL
      dttt3 <- dplyr::left_join(dttt3, df_file8_abstract_2, by=c("time" = "time"))
      dttt3 <- dttt3 %>% tidyr::fill(proposed, .direction = "downup")
      dttt2 <- dttt3
    }
    # Finished the smoothing
    
    
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
      
      if (input$file_8_5_min_type == "No"){
        df_abstract_phase$time <- hms::as_hms(df_abstract_phase$time)
      }  
      if (input$file_8_5_min_type == "Yes"){
        if(number_phases == 2){
          df_abstract_phase$B_sec[df_abstract_phase$B_sec < as.integer(input$file8_5_min_B_sec)] <- as.integer(input$file8_5_min_B_sec)
          # df_abstract_phase$C_sec[df_abstract_phase$C_sec < as.integer(input$file8_5_min_C_sec)] <- as.integer(input$file8_5_min_C_sec)
          # df_abstract_phase$D_sec[df_abstract_phase$D_sec < as.integer(input$file8_5_min_D_sec)] <- as.integer(input$file8_5_min_D_sec)
          # df_abstract_phase$E_sec[df_abstract_phase$E_sec < as.integer(input$file8_5_min_E_sec)] <- as.integer(input$file8_5_min_E_sec)
          # df_abstract_phase$F_sec[df_abstract_phase$F_sec < as.integer(input$file8_5_min_F_sec)] <- as.integer(input$file8_5_min_F_sec)
          # df_abstract_phase$G_sec[df_abstract_phase$G_sec < as.integer(input$file8_5_min_G_sec)] <- as.integer(input$file8_5_min_G_sec)
          # df_abstract_phase$H_sec[df_abstract_phase$H_sec < as.integer(input$file8_5_min_H_sec)] <- as.integer(input$file8_5_min_H_sec)
          # df_abstract_phase$I_sec[df_abstract_phase$I_sec < as.integer(input$file8_5_min_I_sec)] <- as.integer(input$file8_5_min_I_sec)
          df_abstract_phase$A_sec <- as.integer(round(df_abstract_phase$proposed-(
            +df_abstract_phase$B_sec
            # +df_abstract_phase$C_sec
            # +df_abstract_phase$D_sec
            # +df_abstract_phase$E_sec
            # +df_abstract_phase$F_sec
            # +df_abstract_phase$G_sec
            # +df_abstract_phase$H_sec
            # +df_abstract_phase$I_sec
          ),0))
          df_abstract_phase$split_A_Avg <- round(df_abstract_phase$A_sec/df_abstract_phase$proposed,2)
          df_abstract_phase$split_B_Avg <- round(df_abstract_phase$B_sec/df_abstract_phase$proposed,2)
          # df_abstract_phase$split_C_Avg <- round(df_abstract_phase$C_sec/df_abstract_phase$proposed,2)
          # df_abstract_phase$split_D_Avg <- round(df_abstract_phase$D_sec/df_abstract_phase$proposed,2)
          # df_abstract_phase$split_E_Avg <- round(df_abstract_phase$E_sec/df_abstract_phase$proposed,2)
          # df_abstract_phase$split_F_Avg <- round(df_abstract_phase$F_sec/df_abstract_phase$proposed,2)
          # df_abstract_phase$split_G_Avg <- round(df_abstract_phase$G_sec/df_abstract_phase$proposed,2)
          # df_abstract_phase$split_H_Avg <- round(df_abstract_phase$H_sec/df_abstract_phase$proposed,2)
          # df_abstract_phase$split_I_Avg <- round(df_abstract_phase$I_sec/df_abstract_phase$proposed,2)
          
        }
        if(number_phases == 3){
          df_abstract_phase$B_sec[df_abstract_phase$B_sec < as.integer(input$file8_5_min_B_sec)] <- as.integer(input$file8_5_min_B_sec)
          df_abstract_phase$C_sec[df_abstract_phase$C_sec < as.integer(input$file8_5_min_C_sec)] <- as.integer(input$file8_5_min_C_sec)
          # df_abstract_phase$D_sec[df_abstract_phase$D_sec < as.integer(input$file8_5_min_D_sec)] <- as.integer(input$file8_5_min_D_sec)
          # df_abstract_phase$E_sec[df_abstract_phase$E_sec < as.integer(input$file8_5_min_E_sec)] <- as.integer(input$file8_5_min_E_sec)
          # df_abstract_phase$F_sec[df_abstract_phase$F_sec < as.integer(input$file8_5_min_F_sec)] <- as.integer(input$file8_5_min_F_sec)
          # df_abstract_phase$G_sec[df_abstract_phase$G_sec < as.integer(input$file8_5_min_G_sec)] <- as.integer(input$file8_5_min_G_sec)
          # df_abstract_phase$H_sec[df_abstract_phase$H_sec < as.integer(input$file8_5_min_H_sec)] <- as.integer(input$file8_5_min_H_sec)
          # df_abstract_phase$I_sec[df_abstract_phase$I_sec < as.integer(input$file8_5_min_I_sec)] <- as.integer(input$file8_5_min_I_sec)
          df_abstract_phase$A_sec <- as.integer(round(df_abstract_phase$proposed-(
            +df_abstract_phase$B_sec
            +df_abstract_phase$C_sec
            # +df_abstract_phase$D_sec
            # +df_abstract_phase$E_sec
            # +df_abstract_phase$F_sec
            # +df_abstract_phase$G_sec
            # +df_abstract_phase$H_sec
            # +df_abstract_phase$I_sec
          ),0))
          df_abstract_phase$split_A_Avg <- round(df_abstract_phase$A_sec/df_abstract_phase$proposed,2)
          df_abstract_phase$split_B_Avg <- round(df_abstract_phase$B_sec/df_abstract_phase$proposed,2)
          df_abstract_phase$split_C_Avg <- round(df_abstract_phase$C_sec/df_abstract_phase$proposed,2)
          # df_abstract_phase$split_D_Avg <- round(df_abstract_phase$D_sec/df_abstract_phase$proposed,2)
          # df_abstract_phase$split_E_Avg <- round(df_abstract_phase$E_sec/df_abstract_phase$proposed,2)
          # df_abstract_phase$split_F_Avg <- round(df_abstract_phase$F_sec/df_abstract_phase$proposed,2)
          # df_abstract_phase$split_G_Avg <- round(df_abstract_phase$G_sec/df_abstract_phase$proposed,2)
          # df_abstract_phase$split_H_Avg <- round(df_abstract_phase$H_sec/df_abstract_phase$proposed,2)
          # df_abstract_phase$split_I_Avg <- round(df_abstract_phase$I_sec/df_abstract_phase$proposed,2)
        }
        if(number_phases == 4){
          df_abstract_phase$B_sec[df_abstract_phase$B_sec < as.integer(input$file8_5_min_B_sec)] <- as.integer(input$file8_5_min_B_sec)
          df_abstract_phase$C_sec[df_abstract_phase$C_sec < as.integer(input$file8_5_min_C_sec)] <- as.integer(input$file8_5_min_C_sec)
          df_abstract_phase$D_sec[df_abstract_phase$D_sec < as.integer(input$file8_5_min_D_sec)] <- as.integer(input$file8_5_min_D_sec)
          # df_abstract_phase$E_sec[df_abstract_phase$E_sec < as.integer(input$file8_5_min_E_sec)] <- as.integer(input$file8_5_min_E_sec)
          # df_abstract_phase$F_sec[df_abstract_phase$F_sec < as.integer(input$file8_5_min_F_sec)] <- as.integer(input$file8_5_min_F_sec)
          # df_abstract_phase$G_sec[df_abstract_phase$G_sec < as.integer(input$file8_5_min_G_sec)] <- as.integer(input$file8_5_min_G_sec)
          # df_abstract_phase$H_sec[df_abstract_phase$H_sec < as.integer(input$file8_5_min_H_sec)] <- as.integer(input$file8_5_min_H_sec)
          # df_abstract_phase$I_sec[df_abstract_phase$I_sec < as.integer(input$file8_5_min_I_sec)] <- as.integer(input$file8_5_min_I_sec)
          df_abstract_phase$A_sec <- as.integer(round(df_abstract_phase$proposed-(
            +df_abstract_phase$B_sec
            +df_abstract_phase$C_sec
            +df_abstract_phase$D_sec
            # +df_abstract_phase$E_sec
            # +df_abstract_phase$F_sec
            # +df_abstract_phase$G_sec
            # +df_abstract_phase$H_sec
            # +df_abstract_phase$I_sec
          ),0))
          df_abstract_phase$split_A_Avg <- round(df_abstract_phase$A_sec/df_abstract_phase$proposed,2)
          df_abstract_phase$split_B_Avg <- round(df_abstract_phase$B_sec/df_abstract_phase$proposed,2)
          df_abstract_phase$split_C_Avg <- round(df_abstract_phase$C_sec/df_abstract_phase$proposed,2)
          df_abstract_phase$split_D_Avg <- round(df_abstract_phase$D_sec/df_abstract_phase$proposed,2)
          # df_abstract_phase$split_E_Avg <- round(df_abstract_phase$E_sec/df_abstract_phase$proposed,2)
          # df_abstract_phase$split_F_Avg <- round(df_abstract_phase$F_sec/df_abstract_phase$proposed,2)
          # df_abstract_phase$split_G_Avg <- round(df_abstract_phase$G_sec/df_abstract_phase$proposed,2)
          # df_abstract_phase$split_H_Avg <- round(df_abstract_phase$H_sec/df_abstract_phase$proposed,2)
          # df_abstract_phase$split_I_Avg <- round(df_abstract_phase$I_sec/df_abstract_phase$proposed,2)
        }
        if(number_phases == 5){
          df_abstract_phase$B_sec[df_abstract_phase$B_sec < as.integer(input$file8_5_min_B_sec)] <- as.integer(input$file8_5_min_B_sec)
          df_abstract_phase$C_sec[df_abstract_phase$C_sec < as.integer(input$file8_5_min_C_sec)] <- as.integer(input$file8_5_min_C_sec)
          df_abstract_phase$D_sec[df_abstract_phase$D_sec < as.integer(input$file8_5_min_D_sec)] <- as.integer(input$file8_5_min_D_sec)
          df_abstract_phase$E_sec[df_abstract_phase$E_sec < as.integer(input$file8_5_min_E_sec)] <- as.integer(input$file8_5_min_E_sec)
          # df_abstract_phase$F_sec[df_abstract_phase$F_sec < as.integer(input$file8_5_min_F_sec)] <- as.integer(input$file8_5_min_F_sec)
          # df_abstract_phase$G_sec[df_abstract_phase$G_sec < as.integer(input$file8_5_min_G_sec)] <- as.integer(input$file8_5_min_G_sec)
          # df_abstract_phase$H_sec[df_abstract_phase$H_sec < as.integer(input$file8_5_min_H_sec)] <- as.integer(input$file8_5_min_H_sec)
          # df_abstract_phase$I_sec[df_abstract_phase$I_sec < as.integer(input$file8_5_min_I_sec)] <- as.integer(input$file8_5_min_I_sec)
          df_abstract_phase$A_sec <- as.integer(round(df_abstract_phase$proposed-(
            +df_abstract_phase$B_sec
            +df_abstract_phase$C_sec
            +df_abstract_phase$D_sec
            +df_abstract_phase$E_sec
            # +df_abstract_phase$F_sec
            # +df_abstract_phase$G_sec
            # +df_abstract_phase$H_sec
            # +df_abstract_phase$I_sec
          ),0))
          df_abstract_phase$split_A_Avg <- round(df_abstract_phase$A_sec/df_abstract_phase$proposed,2)
          df_abstract_phase$split_B_Avg <- round(df_abstract_phase$B_sec/df_abstract_phase$proposed,2)
          df_abstract_phase$split_C_Avg <- round(df_abstract_phase$C_sec/df_abstract_phase$proposed,2)
          df_abstract_phase$split_D_Avg <- round(df_abstract_phase$D_sec/df_abstract_phase$proposed,2)
          df_abstract_phase$split_E_Avg <- round(df_abstract_phase$E_sec/df_abstract_phase$proposed,2)
          # df_abstract_phase$split_F_Avg <- round(df_abstract_phase$F_sec/df_abstract_phase$proposed,2)
          # df_abstract_phase$split_G_Avg <- round(df_abstract_phase$G_sec/df_abstract_phase$proposed,2)
          # df_abstract_phase$split_H_Avg <- round(df_abstract_phase$H_sec/df_abstract_phase$proposed,2)
          # df_abstract_phase$split_I_Avg <- round(df_abstract_phase$I_sec/df_abstract_phase$proposed,2)
        }
        if(number_phases == 6){
          df_abstract_phase$B_sec[df_abstract_phase$B_sec < as.integer(input$file8_5_min_B_sec)] <- as.integer(input$file8_5_min_B_sec)
          df_abstract_phase$C_sec[df_abstract_phase$C_sec < as.integer(input$file8_5_min_C_sec)] <- as.integer(input$file8_5_min_C_sec)
          df_abstract_phase$D_sec[df_abstract_phase$D_sec < as.integer(input$file8_5_min_D_sec)] <- as.integer(input$file8_5_min_D_sec)
          df_abstract_phase$E_sec[df_abstract_phase$E_sec < as.integer(input$file8_5_min_E_sec)] <- as.integer(input$file8_5_min_E_sec)
          df_abstract_phase$F_sec[df_abstract_phase$F_sec < as.integer(input$file8_5_min_F_sec)] <- as.integer(input$file8_5_min_F_sec)
          # df_abstract_phase$G_sec[df_abstract_phase$G_sec < as.integer(input$file8_5_min_G_sec)] <- as.integer(input$file8_5_min_G_sec)
          # df_abstract_phase$H_sec[df_abstract_phase$H_sec < as.integer(input$file8_5_min_H_sec)] <- as.integer(input$file8_5_min_H_sec)
          # df_abstract_phase$I_sec[df_abstract_phase$I_sec < as.integer(input$file8_5_min_I_sec)] <- as.integer(input$file8_5_min_I_sec)
          df_abstract_phase$A_sec <- as.integer(round(df_abstract_phase$proposed-(
            +df_abstract_phase$B_sec
            +df_abstract_phase$C_sec
            +df_abstract_phase$D_sec
            +df_abstract_phase$E_sec
            +df_abstract_phase$F_sec
            # +df_abstract_phase$G_sec
            # +df_abstract_phase$H_sec
            # +df_abstract_phase$I_sec
          ),0))
          df_abstract_phase$split_A_Avg <- round(df_abstract_phase$A_sec/df_abstract_phase$proposed,2)
          df_abstract_phase$split_B_Avg <- round(df_abstract_phase$B_sec/df_abstract_phase$proposed,2)
          df_abstract_phase$split_C_Avg <- round(df_abstract_phase$C_sec/df_abstract_phase$proposed,2)
          df_abstract_phase$split_D_Avg <- round(df_abstract_phase$D_sec/df_abstract_phase$proposed,2)
          df_abstract_phase$split_E_Avg <- round(df_abstract_phase$E_sec/df_abstract_phase$proposed,2)
          df_abstract_phase$split_F_Avg <- round(df_abstract_phase$F_sec/df_abstract_phase$proposed,2)
          # df_abstract_phase$split_G_Avg <- round(df_abstract_phase$G_sec/df_abstract_phase$proposed,2)
          # df_abstract_phase$split_H_Avg <- round(df_abstract_phase$H_sec/df_abstract_phase$proposed,2)
          # df_abstract_phase$split_I_Avg <- round(df_abstract_phase$I_sec/df_abstract_phase$proposed,2)
        }
        if(number_phases == 7){
          df_abstract_phase$B_sec[df_abstract_phase$B_sec < as.integer(input$file8_5_min_B_sec)] <- as.integer(input$file8_5_min_B_sec)
          df_abstract_phase$C_sec[df_abstract_phase$C_sec < as.integer(input$file8_5_min_C_sec)] <- as.integer(input$file8_5_min_C_sec)
          df_abstract_phase$D_sec[df_abstract_phase$D_sec < as.integer(input$file8_5_min_D_sec)] <- as.integer(input$file8_5_min_D_sec)
          df_abstract_phase$E_sec[df_abstract_phase$E_sec < as.integer(input$file8_5_min_E_sec)] <- as.integer(input$file8_5_min_E_sec)
          df_abstract_phase$F_sec[df_abstract_phase$F_sec < as.integer(input$file8_5_min_F_sec)] <- as.integer(input$file8_5_min_F_sec)
          df_abstract_phase$G_sec[df_abstract_phase$G_sec < as.integer(input$file8_5_min_G_sec)] <- as.integer(input$file8_5_min_G_sec)
          # df_abstract_phase$H_sec[df_abstract_phase$H_sec < as.integer(input$file8_5_min_H_sec)] <- as.integer(input$file8_5_min_H_sec)
          # df_abstract_phase$I_sec[df_abstract_phase$I_sec < as.integer(input$file8_5_min_I_sec)] <- as.integer(input$file8_5_min_I_sec)
          df_abstract_phase$A_sec <- as.integer(round(df_abstract_phase$proposed-(
            +df_abstract_phase$B_sec
            +df_abstract_phase$C_sec
            +df_abstract_phase$D_sec
            +df_abstract_phase$E_sec
            +df_abstract_phase$F_sec
            +df_abstract_phase$G_sec
            # +df_abstract_phase$H_sec
            # +df_abstract_phase$I_sec
          ),0))
          df_abstract_phase$split_A_Avg <- round(df_abstract_phase$A_sec/df_abstract_phase$proposed,2)
          df_abstract_phase$split_B_Avg <- round(df_abstract_phase$B_sec/df_abstract_phase$proposed,2)
          df_abstract_phase$split_C_Avg <- round(df_abstract_phase$C_sec/df_abstract_phase$proposed,2)
          df_abstract_phase$split_D_Avg <- round(df_abstract_phase$D_sec/df_abstract_phase$proposed,2)
          df_abstract_phase$split_E_Avg <- round(df_abstract_phase$E_sec/df_abstract_phase$proposed,2)
          df_abstract_phase$split_F_Avg <- round(df_abstract_phase$F_sec/df_abstract_phase$proposed,2)
          df_abstract_phase$split_G_Avg <- round(df_abstract_phase$G_sec/df_abstract_phase$proposed,2)
          # df_abstract_phase$split_H_Avg <- round(df_abstract_phase$H_sec/df_abstract_phase$proposed,2)
          # df_abstract_phase$split_I_Avg <- round(df_abstract_phase$I_sec/df_abstract_phase$proposed,2)
        }
        if(number_phases == 8){
          df_abstract_phase$B_sec[df_abstract_phase$B_sec < as.integer(input$file8_5_min_B_sec)] <- as.integer(input$file8_5_min_B_sec)
          df_abstract_phase$C_sec[df_abstract_phase$C_sec < as.integer(input$file8_5_min_C_sec)] <- as.integer(input$file8_5_min_C_sec)
          df_abstract_phase$D_sec[df_abstract_phase$D_sec < as.integer(input$file8_5_min_D_sec)] <- as.integer(input$file8_5_min_D_sec)
          df_abstract_phase$E_sec[df_abstract_phase$E_sec < as.integer(input$file8_5_min_E_sec)] <- as.integer(input$file8_5_min_E_sec)
          df_abstract_phase$F_sec[df_abstract_phase$F_sec < as.integer(input$file8_5_min_F_sec)] <- as.integer(input$file8_5_min_F_sec)
          df_abstract_phase$G_sec[df_abstract_phase$G_sec < as.integer(input$file8_5_min_G_sec)] <- as.integer(input$file8_5_min_G_sec)
          df_abstract_phase$H_sec[df_abstract_phase$H_sec < as.integer(input$file8_5_min_H_sec)] <- as.integer(input$file8_5_min_H_sec)
          # df_abstract_phase$I_sec[df_abstract_phase$I_sec < as.integer(input$file8_5_min_I_sec)] <- as.integer(input$file8_5_min_I_sec)
          df_abstract_phase$A_sec <- as.integer(round(df_abstract_phase$proposed-(
            +df_abstract_phase$B_sec
            +df_abstract_phase$C_sec
            +df_abstract_phase$D_sec
            +df_abstract_phase$E_sec
            +df_abstract_phase$F_sec
            +df_abstract_phase$G_sec
            +df_abstract_phase$H_sec
            # +df_abstract_phase$I_sec
          ),0))
          df_abstract_phase$split_A_Avg <- round(df_abstract_phase$A_sec/df_abstract_phase$proposed,2)
          df_abstract_phase$split_B_Avg <- round(df_abstract_phase$B_sec/df_abstract_phase$proposed,2)
          df_abstract_phase$split_C_Avg <- round(df_abstract_phase$C_sec/df_abstract_phase$proposed,2)
          df_abstract_phase$split_D_Avg <- round(df_abstract_phase$D_sec/df_abstract_phase$proposed,2)
          df_abstract_phase$split_E_Avg <- round(df_abstract_phase$E_sec/df_abstract_phase$proposed,2)
          df_abstract_phase$split_F_Avg <- round(df_abstract_phase$F_sec/df_abstract_phase$proposed,2)
          df_abstract_phase$split_G_Avg <- round(df_abstract_phase$G_sec/df_abstract_phase$proposed,2)
          df_abstract_phase$split_H_Avg <- round(df_abstract_phase$H_sec/df_abstract_phase$proposed,2)
          # df_abstract_phase$split_I_Avg <- round(df_abstract_phase$I_sec/df_abstract_phase$proposed,2)
        }
        if(number_phases == 9){
          df_abstract_phase$B_sec[df_abstract_phase$B_sec < as.integer(input$file8_5_min_B_sec)] <- as.integer(input$file8_5_min_B_sec)
          df_abstract_phase$C_sec[df_abstract_phase$C_sec < as.integer(input$file8_5_min_C_sec)] <- as.integer(input$file8_5_min_C_sec)
          df_abstract_phase$D_sec[df_abstract_phase$D_sec < as.integer(input$file8_5_min_D_sec)] <- as.integer(input$file8_5_min_D_sec)
          df_abstract_phase$E_sec[df_abstract_phase$E_sec < as.integer(input$file8_5_min_E_sec)] <- as.integer(input$file8_5_min_E_sec)
          df_abstract_phase$F_sec[df_abstract_phase$F_sec < as.integer(input$file8_5_min_F_sec)] <- as.integer(input$file8_5_min_F_sec)
          df_abstract_phase$G_sec[df_abstract_phase$G_sec < as.integer(input$file8_5_min_G_sec)] <- as.integer(input$file8_5_min_G_sec)
          df_abstract_phase$H_sec[df_abstract_phase$H_sec < as.integer(input$file8_5_min_H_sec)] <- as.integer(input$file8_5_min_H_sec)
          df_abstract_phase$I_sec[df_abstract_phase$I_sec < as.integer(input$file8_5_min_I_sec)] <- as.integer(input$file8_5_min_I_sec)
          df_abstract_phase$A_sec <- as.integer(round(df_abstract_phase$proposed-(
            +df_abstract_phase$B_sec
            +df_abstract_phase$C_sec
            +df_abstract_phase$D_sec
            +df_abstract_phase$E_sec
            +df_abstract_phase$F_sec
            +df_abstract_phase$G_sec
            +df_abstract_phase$H_sec
            +df_abstract_phase$I_sec
          ),0))
          df_abstract_phase$split_A_Avg <- round(df_abstract_phase$A_sec/df_abstract_phase$proposed,2)
          df_abstract_phase$split_B_Avg <- round(df_abstract_phase$B_sec/df_abstract_phase$proposed,2)
          df_abstract_phase$split_C_Avg <- round(df_abstract_phase$C_sec/df_abstract_phase$proposed,2)
          df_abstract_phase$split_D_Avg <- round(df_abstract_phase$D_sec/df_abstract_phase$proposed,2)
          df_abstract_phase$split_E_Avg <- round(df_abstract_phase$E_sec/df_abstract_phase$proposed,2)
          df_abstract_phase$split_F_Avg <- round(df_abstract_phase$F_sec/df_abstract_phase$proposed,2)
          df_abstract_phase$split_G_Avg <- round(df_abstract_phase$G_sec/df_abstract_phase$proposed,2)
          df_abstract_phase$split_H_Avg <- round(df_abstract_phase$H_sec/df_abstract_phase$proposed,2)
          df_abstract_phase$split_I_Avg <- round(df_abstract_phase$I_sec/df_abstract_phase$proposed,2)
        }
        df_abstract_phase$time <- hms::as_hms(df_abstract_phase$time)
      }
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
      if (input$file_8_5_min_type == "No"){
        df_abstract_phase$time <- hms::as_hms(df_abstract_phase$time)
      }  
      if (input$file_8_5_min_type == "Yes"){
        if(number_phases == 2){
          df_abstract_phase$B_sec[df_abstract_phase$B_sec < as.integer(input$file8_5_min_B_sec)] <- as.integer(input$file8_5_min_B_sec)
          # df_abstract_phase$C_sec[df_abstract_phase$C_sec < as.integer(input$file8_5_min_C_sec)] <- as.integer(input$file8_5_min_C_sec)
          # df_abstract_phase$D_sec[df_abstract_phase$D_sec < as.integer(input$file8_5_min_D_sec)] <- as.integer(input$file8_5_min_D_sec)
          # df_abstract_phase$E_sec[df_abstract_phase$E_sec < as.integer(input$file8_5_min_E_sec)] <- as.integer(input$file8_5_min_E_sec)
          # df_abstract_phase$F_sec[df_abstract_phase$F_sec < as.integer(input$file8_5_min_F_sec)] <- as.integer(input$file8_5_min_F_sec)
          # df_abstract_phase$G_sec[df_abstract_phase$G_sec < as.integer(input$file8_5_min_G_sec)] <- as.integer(input$file8_5_min_G_sec)
          # df_abstract_phase$H_sec[df_abstract_phase$H_sec < as.integer(input$file8_5_min_H_sec)] <- as.integer(input$file8_5_min_H_sec)
          # df_abstract_phase$I_sec[df_abstract_phase$I_sec < as.integer(input$file8_5_min_I_sec)] <- as.integer(input$file8_5_min_I_sec)
          df_abstract_phase$A_sec <- as.integer(round(df_abstract_phase$proposed-(
            +df_abstract_phase$B_sec
            # +df_abstract_phase$C_sec
            # +df_abstract_phase$D_sec
            # +df_abstract_phase$E_sec
            # +df_abstract_phase$F_sec
            # +df_abstract_phase$G_sec
            # +df_abstract_phase$H_sec
            # +df_abstract_phase$I_sec
          ),0))
          df_abstract_phase$split_A_Avg <- round(df_abstract_phase$A_sec/df_abstract_phase$proposed,2)
          df_abstract_phase$split_B_Avg <- round(df_abstract_phase$B_sec/df_abstract_phase$proposed,2)
          # df_abstract_phase$split_C_Avg <- round(df_abstract_phase$C_sec/df_abstract_phase$proposed,2)
          # df_abstract_phase$split_D_Avg <- round(df_abstract_phase$D_sec/df_abstract_phase$proposed,2)
          # df_abstract_phase$split_E_Avg <- round(df_abstract_phase$E_sec/df_abstract_phase$proposed,2)
          # df_abstract_phase$split_F_Avg <- round(df_abstract_phase$F_sec/df_abstract_phase$proposed,2)
          # df_abstract_phase$split_G_Avg <- round(df_abstract_phase$G_sec/df_abstract_phase$proposed,2)
          # df_abstract_phase$split_H_Avg <- round(df_abstract_phase$H_sec/df_abstract_phase$proposed,2)
          # df_abstract_phase$split_I_Avg <- round(df_abstract_phase$I_sec/df_abstract_phase$proposed,2)
          
        }
        if(number_phases == 3){
          df_abstract_phase$B_sec[df_abstract_phase$B_sec < as.integer(input$file8_5_min_B_sec)] <- as.integer(input$file8_5_min_B_sec)
          df_abstract_phase$C_sec[df_abstract_phase$C_sec < as.integer(input$file8_5_min_C_sec)] <- as.integer(input$file8_5_min_C_sec)
          # df_abstract_phase$D_sec[df_abstract_phase$D_sec < as.integer(input$file8_5_min_D_sec)] <- as.integer(input$file8_5_min_D_sec)
          # df_abstract_phase$E_sec[df_abstract_phase$E_sec < as.integer(input$file8_5_min_E_sec)] <- as.integer(input$file8_5_min_E_sec)
          # df_abstract_phase$F_sec[df_abstract_phase$F_sec < as.integer(input$file8_5_min_F_sec)] <- as.integer(input$file8_5_min_F_sec)
          # df_abstract_phase$G_sec[df_abstract_phase$G_sec < as.integer(input$file8_5_min_G_sec)] <- as.integer(input$file8_5_min_G_sec)
          # df_abstract_phase$H_sec[df_abstract_phase$H_sec < as.integer(input$file8_5_min_H_sec)] <- as.integer(input$file8_5_min_H_sec)
          # df_abstract_phase$I_sec[df_abstract_phase$I_sec < as.integer(input$file8_5_min_I_sec)] <- as.integer(input$file8_5_min_I_sec)
          df_abstract_phase$A_sec <- as.integer(round(df_abstract_phase$proposed-(
            +df_abstract_phase$B_sec
            +df_abstract_phase$C_sec
            # +df_abstract_phase$D_sec
            # +df_abstract_phase$E_sec
            # +df_abstract_phase$F_sec
            # +df_abstract_phase$G_sec
            # +df_abstract_phase$H_sec
            # +df_abstract_phase$I_sec
          ),0))
          df_abstract_phase$split_A_Avg <- round(df_abstract_phase$A_sec/df_abstract_phase$proposed,2)
          df_abstract_phase$split_B_Avg <- round(df_abstract_phase$B_sec/df_abstract_phase$proposed,2)
          df_abstract_phase$split_C_Avg <- round(df_abstract_phase$C_sec/df_abstract_phase$proposed,2)
          # df_abstract_phase$split_D_Avg <- round(df_abstract_phase$D_sec/df_abstract_phase$proposed,2)
          # df_abstract_phase$split_E_Avg <- round(df_abstract_phase$E_sec/df_abstract_phase$proposed,2)
          # df_abstract_phase$split_F_Avg <- round(df_abstract_phase$F_sec/df_abstract_phase$proposed,2)
          # df_abstract_phase$split_G_Avg <- round(df_abstract_phase$G_sec/df_abstract_phase$proposed,2)
          # df_abstract_phase$split_H_Avg <- round(df_abstract_phase$H_sec/df_abstract_phase$proposed,2)
          # df_abstract_phase$split_I_Avg <- round(df_abstract_phase$I_sec/df_abstract_phase$proposed,2)
        }
        if(number_phases == 4){
          df_abstract_phase$B_sec[df_abstract_phase$B_sec < as.integer(input$file8_5_min_B_sec)] <- as.integer(input$file8_5_min_B_sec)
          df_abstract_phase$C_sec[df_abstract_phase$C_sec < as.integer(input$file8_5_min_C_sec)] <- as.integer(input$file8_5_min_C_sec)
          df_abstract_phase$D_sec[df_abstract_phase$D_sec < as.integer(input$file8_5_min_D_sec)] <- as.integer(input$file8_5_min_D_sec)
          # df_abstract_phase$E_sec[df_abstract_phase$E_sec < as.integer(input$file8_5_min_E_sec)] <- as.integer(input$file8_5_min_E_sec)
          # df_abstract_phase$F_sec[df_abstract_phase$F_sec < as.integer(input$file8_5_min_F_sec)] <- as.integer(input$file8_5_min_F_sec)
          # df_abstract_phase$G_sec[df_abstract_phase$G_sec < as.integer(input$file8_5_min_G_sec)] <- as.integer(input$file8_5_min_G_sec)
          # df_abstract_phase$H_sec[df_abstract_phase$H_sec < as.integer(input$file8_5_min_H_sec)] <- as.integer(input$file8_5_min_H_sec)
          # df_abstract_phase$I_sec[df_abstract_phase$I_sec < as.integer(input$file8_5_min_I_sec)] <- as.integer(input$file8_5_min_I_sec)
          df_abstract_phase$A_sec <- as.integer(round(df_abstract_phase$proposed-(
            +df_abstract_phase$B_sec
            +df_abstract_phase$C_sec
            +df_abstract_phase$D_sec
            # +df_abstract_phase$E_sec
            # +df_abstract_phase$F_sec
            # +df_abstract_phase$G_sec
            # +df_abstract_phase$H_sec
            # +df_abstract_phase$I_sec
          ),0))
          df_abstract_phase$split_A_Avg <- round(df_abstract_phase$A_sec/df_abstract_phase$proposed,2)
          df_abstract_phase$split_B_Avg <- round(df_abstract_phase$B_sec/df_abstract_phase$proposed,2)
          df_abstract_phase$split_C_Avg <- round(df_abstract_phase$C_sec/df_abstract_phase$proposed,2)
          df_abstract_phase$split_D_Avg <- round(df_abstract_phase$D_sec/df_abstract_phase$proposed,2)
          # df_abstract_phase$split_E_Avg <- round(df_abstract_phase$E_sec/df_abstract_phase$proposed,2)
          # df_abstract_phase$split_F_Avg <- round(df_abstract_phase$F_sec/df_abstract_phase$proposed,2)
          # df_abstract_phase$split_G_Avg <- round(df_abstract_phase$G_sec/df_abstract_phase$proposed,2)
          # df_abstract_phase$split_H_Avg <- round(df_abstract_phase$H_sec/df_abstract_phase$proposed,2)
          # df_abstract_phase$split_I_Avg <- round(df_abstract_phase$I_sec/df_abstract_phase$proposed,2)
        }
        if(number_phases == 5){
          df_abstract_phase$B_sec[df_abstract_phase$B_sec < as.integer(input$file8_5_min_B_sec)] <- as.integer(input$file8_5_min_B_sec)
          df_abstract_phase$C_sec[df_abstract_phase$C_sec < as.integer(input$file8_5_min_C_sec)] <- as.integer(input$file8_5_min_C_sec)
          df_abstract_phase$D_sec[df_abstract_phase$D_sec < as.integer(input$file8_5_min_D_sec)] <- as.integer(input$file8_5_min_D_sec)
          df_abstract_phase$E_sec[df_abstract_phase$E_sec < as.integer(input$file8_5_min_E_sec)] <- as.integer(input$file8_5_min_E_sec)
          # df_abstract_phase$F_sec[df_abstract_phase$F_sec < as.integer(input$file8_5_min_F_sec)] <- as.integer(input$file8_5_min_F_sec)
          # df_abstract_phase$G_sec[df_abstract_phase$G_sec < as.integer(input$file8_5_min_G_sec)] <- as.integer(input$file8_5_min_G_sec)
          # df_abstract_phase$H_sec[df_abstract_phase$H_sec < as.integer(input$file8_5_min_H_sec)] <- as.integer(input$file8_5_min_H_sec)
          # df_abstract_phase$I_sec[df_abstract_phase$I_sec < as.integer(input$file8_5_min_I_sec)] <- as.integer(input$file8_5_min_I_sec)
          df_abstract_phase$A_sec <- as.integer(round(df_abstract_phase$proposed-(
            +df_abstract_phase$B_sec
            +df_abstract_phase$C_sec
            +df_abstract_phase$D_sec
            +df_abstract_phase$E_sec
            # +df_abstract_phase$F_sec
            # +df_abstract_phase$G_sec
            # +df_abstract_phase$H_sec
            # +df_abstract_phase$I_sec
          ),0))
          df_abstract_phase$split_A_Avg <- round(df_abstract_phase$A_sec/df_abstract_phase$proposed,2)
          df_abstract_phase$split_B_Avg <- round(df_abstract_phase$B_sec/df_abstract_phase$proposed,2)
          df_abstract_phase$split_C_Avg <- round(df_abstract_phase$C_sec/df_abstract_phase$proposed,2)
          df_abstract_phase$split_D_Avg <- round(df_abstract_phase$D_sec/df_abstract_phase$proposed,2)
          df_abstract_phase$split_E_Avg <- round(df_abstract_phase$E_sec/df_abstract_phase$proposed,2)
          # df_abstract_phase$split_F_Avg <- round(df_abstract_phase$F_sec/df_abstract_phase$proposed,2)
          # df_abstract_phase$split_G_Avg <- round(df_abstract_phase$G_sec/df_abstract_phase$proposed,2)
          # df_abstract_phase$split_H_Avg <- round(df_abstract_phase$H_sec/df_abstract_phase$proposed,2)
          # df_abstract_phase$split_I_Avg <- round(df_abstract_phase$I_sec/df_abstract_phase$proposed,2)
        }
        if(number_phases == 6){
          df_abstract_phase$B_sec[df_abstract_phase$B_sec < as.integer(input$file8_5_min_B_sec)] <- as.integer(input$file8_5_min_B_sec)
          df_abstract_phase$C_sec[df_abstract_phase$C_sec < as.integer(input$file8_5_min_C_sec)] <- as.integer(input$file8_5_min_C_sec)
          df_abstract_phase$D_sec[df_abstract_phase$D_sec < as.integer(input$file8_5_min_D_sec)] <- as.integer(input$file8_5_min_D_sec)
          df_abstract_phase$E_sec[df_abstract_phase$E_sec < as.integer(input$file8_5_min_E_sec)] <- as.integer(input$file8_5_min_E_sec)
          df_abstract_phase$F_sec[df_abstract_phase$F_sec < as.integer(input$file8_5_min_F_sec)] <- as.integer(input$file8_5_min_F_sec)
          # df_abstract_phase$G_sec[df_abstract_phase$G_sec < as.integer(input$file8_5_min_G_sec)] <- as.integer(input$file8_5_min_G_sec)
          # df_abstract_phase$H_sec[df_abstract_phase$H_sec < as.integer(input$file8_5_min_H_sec)] <- as.integer(input$file8_5_min_H_sec)
          # df_abstract_phase$I_sec[df_abstract_phase$I_sec < as.integer(input$file8_5_min_I_sec)] <- as.integer(input$file8_5_min_I_sec)
          df_abstract_phase$A_sec <- as.integer(round(df_abstract_phase$proposed-(
            +df_abstract_phase$B_sec
            +df_abstract_phase$C_sec
            +df_abstract_phase$D_sec
            +df_abstract_phase$E_sec
            +df_abstract_phase$F_sec
            # +df_abstract_phase$G_sec
            # +df_abstract_phase$H_sec
            # +df_abstract_phase$I_sec
          ),0))
          df_abstract_phase$split_A_Avg <- round(df_abstract_phase$A_sec/df_abstract_phase$proposed,2)
          df_abstract_phase$split_B_Avg <- round(df_abstract_phase$B_sec/df_abstract_phase$proposed,2)
          df_abstract_phase$split_C_Avg <- round(df_abstract_phase$C_sec/df_abstract_phase$proposed,2)
          df_abstract_phase$split_D_Avg <- round(df_abstract_phase$D_sec/df_abstract_phase$proposed,2)
          df_abstract_phase$split_E_Avg <- round(df_abstract_phase$E_sec/df_abstract_phase$proposed,2)
          df_abstract_phase$split_F_Avg <- round(df_abstract_phase$F_sec/df_abstract_phase$proposed,2)
          # df_abstract_phase$split_G_Avg <- round(df_abstract_phase$G_sec/df_abstract_phase$proposed,2)
          # df_abstract_phase$split_H_Avg <- round(df_abstract_phase$H_sec/df_abstract_phase$proposed,2)
          # df_abstract_phase$split_I_Avg <- round(df_abstract_phase$I_sec/df_abstract_phase$proposed,2)
        }
        if(number_phases == 7){
          df_abstract_phase$B_sec[df_abstract_phase$B_sec < as.integer(input$file8_5_min_B_sec)] <- as.integer(input$file8_5_min_B_sec)
          df_abstract_phase$C_sec[df_abstract_phase$C_sec < as.integer(input$file8_5_min_C_sec)] <- as.integer(input$file8_5_min_C_sec)
          df_abstract_phase$D_sec[df_abstract_phase$D_sec < as.integer(input$file8_5_min_D_sec)] <- as.integer(input$file8_5_min_D_sec)
          df_abstract_phase$E_sec[df_abstract_phase$E_sec < as.integer(input$file8_5_min_E_sec)] <- as.integer(input$file8_5_min_E_sec)
          df_abstract_phase$F_sec[df_abstract_phase$F_sec < as.integer(input$file8_5_min_F_sec)] <- as.integer(input$file8_5_min_F_sec)
          df_abstract_phase$G_sec[df_abstract_phase$G_sec < as.integer(input$file8_5_min_G_sec)] <- as.integer(input$file8_5_min_G_sec)
          # df_abstract_phase$H_sec[df_abstract_phase$H_sec < as.integer(input$file8_5_min_H_sec)] <- as.integer(input$file8_5_min_H_sec)
          # df_abstract_phase$I_sec[df_abstract_phase$I_sec < as.integer(input$file8_5_min_I_sec)] <- as.integer(input$file8_5_min_I_sec)
          df_abstract_phase$A_sec <- as.integer(round(df_abstract_phase$proposed-(
            +df_abstract_phase$B_sec
            +df_abstract_phase$C_sec
            +df_abstract_phase$D_sec
            +df_abstract_phase$E_sec
            +df_abstract_phase$F_sec
            +df_abstract_phase$G_sec
            # +df_abstract_phase$H_sec
            # +df_abstract_phase$I_sec
          ),0))
          df_abstract_phase$split_A_Avg <- round(df_abstract_phase$A_sec/df_abstract_phase$proposed,2)
          df_abstract_phase$split_B_Avg <- round(df_abstract_phase$B_sec/df_abstract_phase$proposed,2)
          df_abstract_phase$split_C_Avg <- round(df_abstract_phase$C_sec/df_abstract_phase$proposed,2)
          df_abstract_phase$split_D_Avg <- round(df_abstract_phase$D_sec/df_abstract_phase$proposed,2)
          df_abstract_phase$split_E_Avg <- round(df_abstract_phase$E_sec/df_abstract_phase$proposed,2)
          df_abstract_phase$split_F_Avg <- round(df_abstract_phase$F_sec/df_abstract_phase$proposed,2)
          df_abstract_phase$split_G_Avg <- round(df_abstract_phase$G_sec/df_abstract_phase$proposed,2)
          # df_abstract_phase$split_H_Avg <- round(df_abstract_phase$H_sec/df_abstract_phase$proposed,2)
          # df_abstract_phase$split_I_Avg <- round(df_abstract_phase$I_sec/df_abstract_phase$proposed,2)
        }
        if(number_phases == 8){
          df_abstract_phase$B_sec[df_abstract_phase$B_sec < as.integer(input$file8_5_min_B_sec)] <- as.integer(input$file8_5_min_B_sec)
          df_abstract_phase$C_sec[df_abstract_phase$C_sec < as.integer(input$file8_5_min_C_sec)] <- as.integer(input$file8_5_min_C_sec)
          df_abstract_phase$D_sec[df_abstract_phase$D_sec < as.integer(input$file8_5_min_D_sec)] <- as.integer(input$file8_5_min_D_sec)
          df_abstract_phase$E_sec[df_abstract_phase$E_sec < as.integer(input$file8_5_min_E_sec)] <- as.integer(input$file8_5_min_E_sec)
          df_abstract_phase$F_sec[df_abstract_phase$F_sec < as.integer(input$file8_5_min_F_sec)] <- as.integer(input$file8_5_min_F_sec)
          df_abstract_phase$G_sec[df_abstract_phase$G_sec < as.integer(input$file8_5_min_G_sec)] <- as.integer(input$file8_5_min_G_sec)
          df_abstract_phase$H_sec[df_abstract_phase$H_sec < as.integer(input$file8_5_min_H_sec)] <- as.integer(input$file8_5_min_H_sec)
          # df_abstract_phase$I_sec[df_abstract_phase$I_sec < as.integer(input$file8_5_min_I_sec)] <- as.integer(input$file8_5_min_I_sec)
          df_abstract_phase$A_sec <- as.integer(round(df_abstract_phase$proposed-(
            +df_abstract_phase$B_sec
            +df_abstract_phase$C_sec
            +df_abstract_phase$D_sec
            +df_abstract_phase$E_sec
            +df_abstract_phase$F_sec
            +df_abstract_phase$G_sec
            +df_abstract_phase$H_sec
            # +df_abstract_phase$I_sec
          ),0))
          df_abstract_phase$split_A_Avg <- round(df_abstract_phase$A_sec/df_abstract_phase$proposed,2)
          df_abstract_phase$split_B_Avg <- round(df_abstract_phase$B_sec/df_abstract_phase$proposed,2)
          df_abstract_phase$split_C_Avg <- round(df_abstract_phase$C_sec/df_abstract_phase$proposed,2)
          df_abstract_phase$split_D_Avg <- round(df_abstract_phase$D_sec/df_abstract_phase$proposed,2)
          df_abstract_phase$split_E_Avg <- round(df_abstract_phase$E_sec/df_abstract_phase$proposed,2)
          df_abstract_phase$split_F_Avg <- round(df_abstract_phase$F_sec/df_abstract_phase$proposed,2)
          df_abstract_phase$split_G_Avg <- round(df_abstract_phase$G_sec/df_abstract_phase$proposed,2)
          df_abstract_phase$split_H_Avg <- round(df_abstract_phase$H_sec/df_abstract_phase$proposed,2)
          # df_abstract_phase$split_I_Avg <- round(df_abstract_phase$I_sec/df_abstract_phase$proposed,2)
        }
        if(number_phases == 9){
          df_abstract_phase$B_sec[df_abstract_phase$B_sec < as.integer(input$file8_5_min_B_sec)] <- as.integer(input$file8_5_min_B_sec)
          df_abstract_phase$C_sec[df_abstract_phase$C_sec < as.integer(input$file8_5_min_C_sec)] <- as.integer(input$file8_5_min_C_sec)
          df_abstract_phase$D_sec[df_abstract_phase$D_sec < as.integer(input$file8_5_min_D_sec)] <- as.integer(input$file8_5_min_D_sec)
          df_abstract_phase$E_sec[df_abstract_phase$E_sec < as.integer(input$file8_5_min_E_sec)] <- as.integer(input$file8_5_min_E_sec)
          df_abstract_phase$F_sec[df_abstract_phase$F_sec < as.integer(input$file8_5_min_F_sec)] <- as.integer(input$file8_5_min_F_sec)
          df_abstract_phase$G_sec[df_abstract_phase$G_sec < as.integer(input$file8_5_min_G_sec)] <- as.integer(input$file8_5_min_G_sec)
          df_abstract_phase$H_sec[df_abstract_phase$H_sec < as.integer(input$file8_5_min_H_sec)] <- as.integer(input$file8_5_min_H_sec)
          df_abstract_phase$I_sec[df_abstract_phase$I_sec < as.integer(input$file8_5_min_I_sec)] <- as.integer(input$file8_5_min_I_sec)
          df_abstract_phase$A_sec <- as.integer(round(df_abstract_phase$proposed-(
            +df_abstract_phase$B_sec
            +df_abstract_phase$C_sec
            +df_abstract_phase$D_sec
            +df_abstract_phase$E_sec
            +df_abstract_phase$F_sec
            +df_abstract_phase$G_sec
            +df_abstract_phase$H_sec
            +df_abstract_phase$I_sec
          ),0))
          df_abstract_phase$split_A_Avg <- round(df_abstract_phase$A_sec/df_abstract_phase$proposed,2)
          df_abstract_phase$split_B_Avg <- round(df_abstract_phase$B_sec/df_abstract_phase$proposed,2)
          df_abstract_phase$split_C_Avg <- round(df_abstract_phase$C_sec/df_abstract_phase$proposed,2)
          df_abstract_phase$split_D_Avg <- round(df_abstract_phase$D_sec/df_abstract_phase$proposed,2)
          df_abstract_phase$split_E_Avg <- round(df_abstract_phase$E_sec/df_abstract_phase$proposed,2)
          df_abstract_phase$split_F_Avg <- round(df_abstract_phase$F_sec/df_abstract_phase$proposed,2)
          df_abstract_phase$split_G_Avg <- round(df_abstract_phase$G_sec/df_abstract_phase$proposed,2)
          df_abstract_phase$split_H_Avg <- round(df_abstract_phase$H_sec/df_abstract_phase$proposed,2)
          df_abstract_phase$split_I_Avg <- round(df_abstract_phase$I_sec/df_abstract_phase$proposed,2)
        }
        df_abstract_phase$time <- hms::as_hms(df_abstract_phase$time)
      }
      # input$file8_5_min_B_per
      # df_abstract_phase$time <- hms::as_hms(df_abstract_phase$time)
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
    if (input$file_8_5_min_type == "No"){
      df_abstract_phase$time <- hms::as_hms(df_abstract_phase$time)
    }  
    if (input$file_8_5_min_type == "Yes"){
      if(number_phases == 2){
        df_abstract_phase$B_sec[df_abstract_phase$B_sec < as.integer(input$file8_5_min_B_sec)] <- as.integer(input$file8_5_min_B_sec)
        # df_abstract_phase$C_sec[df_abstract_phase$C_sec < as.integer(input$file8_5_min_C_sec)] <- as.integer(input$file8_5_min_C_sec)
        # df_abstract_phase$D_sec[df_abstract_phase$D_sec < as.integer(input$file8_5_min_D_sec)] <- as.integer(input$file8_5_min_D_sec)
        # df_abstract_phase$E_sec[df_abstract_phase$E_sec < as.integer(input$file8_5_min_E_sec)] <- as.integer(input$file8_5_min_E_sec)
        # df_abstract_phase$F_sec[df_abstract_phase$F_sec < as.integer(input$file8_5_min_F_sec)] <- as.integer(input$file8_5_min_F_sec)
        # df_abstract_phase$G_sec[df_abstract_phase$G_sec < as.integer(input$file8_5_min_G_sec)] <- as.integer(input$file8_5_min_G_sec)
        # df_abstract_phase$H_sec[df_abstract_phase$H_sec < as.integer(input$file8_5_min_H_sec)] <- as.integer(input$file8_5_min_H_sec)
        # df_abstract_phase$I_sec[df_abstract_phase$I_sec < as.integer(input$file8_5_min_I_sec)] <- as.integer(input$file8_5_min_I_sec)
        df_abstract_phase$A_sec <- as.integer(round(df_abstract_phase$proposed-(
          +df_abstract_phase$B_sec
          # +df_abstract_phase$C_sec
          # +df_abstract_phase$D_sec
          # +df_abstract_phase$E_sec
          # +df_abstract_phase$F_sec
          # +df_abstract_phase$G_sec
          # +df_abstract_phase$H_sec
          # +df_abstract_phase$I_sec
        ),0))
        df_abstract_phase$split_A_Avg <- round(df_abstract_phase$A_sec/df_abstract_phase$proposed,2)
        df_abstract_phase$split_B_Avg <- round(df_abstract_phase$B_sec/df_abstract_phase$proposed,2)
        # df_abstract_phase$split_C_Avg <- round(df_abstract_phase$C_sec/df_abstract_phase$proposed,2)
        # df_abstract_phase$split_D_Avg <- round(df_abstract_phase$D_sec/df_abstract_phase$proposed,2)
        # df_abstract_phase$split_E_Avg <- round(df_abstract_phase$E_sec/df_abstract_phase$proposed,2)
        # df_abstract_phase$split_F_Avg <- round(df_abstract_phase$F_sec/df_abstract_phase$proposed,2)
        # df_abstract_phase$split_G_Avg <- round(df_abstract_phase$G_sec/df_abstract_phase$proposed,2)
        # df_abstract_phase$split_H_Avg <- round(df_abstract_phase$H_sec/df_abstract_phase$proposed,2)
        # df_abstract_phase$split_I_Avg <- round(df_abstract_phase$I_sec/df_abstract_phase$proposed,2)
        
      }
      if(number_phases == 3){
        df_abstract_phase$B_sec[df_abstract_phase$B_sec < as.integer(input$file8_5_min_B_sec)] <- as.integer(input$file8_5_min_B_sec)
        df_abstract_phase$C_sec[df_abstract_phase$C_sec < as.integer(input$file8_5_min_C_sec)] <- as.integer(input$file8_5_min_C_sec)
        # df_abstract_phase$D_sec[df_abstract_phase$D_sec < as.integer(input$file8_5_min_D_sec)] <- as.integer(input$file8_5_min_D_sec)
        # df_abstract_phase$E_sec[df_abstract_phase$E_sec < as.integer(input$file8_5_min_E_sec)] <- as.integer(input$file8_5_min_E_sec)
        # df_abstract_phase$F_sec[df_abstract_phase$F_sec < as.integer(input$file8_5_min_F_sec)] <- as.integer(input$file8_5_min_F_sec)
        # df_abstract_phase$G_sec[df_abstract_phase$G_sec < as.integer(input$file8_5_min_G_sec)] <- as.integer(input$file8_5_min_G_sec)
        # df_abstract_phase$H_sec[df_abstract_phase$H_sec < as.integer(input$file8_5_min_H_sec)] <- as.integer(input$file8_5_min_H_sec)
        # df_abstract_phase$I_sec[df_abstract_phase$I_sec < as.integer(input$file8_5_min_I_sec)] <- as.integer(input$file8_5_min_I_sec)
        df_abstract_phase$A_sec <- as.integer(round(df_abstract_phase$proposed-(
          +df_abstract_phase$B_sec
          +df_abstract_phase$C_sec
          # +df_abstract_phase$D_sec
          # +df_abstract_phase$E_sec
          # +df_abstract_phase$F_sec
          # +df_abstract_phase$G_sec
          # +df_abstract_phase$H_sec
          # +df_abstract_phase$I_sec
        ),0))
        df_abstract_phase$split_A_Avg <- round(df_abstract_phase$A_sec/df_abstract_phase$proposed,2)
        df_abstract_phase$split_B_Avg <- round(df_abstract_phase$B_sec/df_abstract_phase$proposed,2)
        df_abstract_phase$split_C_Avg <- round(df_abstract_phase$C_sec/df_abstract_phase$proposed,2)
        # df_abstract_phase$split_D_Avg <- round(df_abstract_phase$D_sec/df_abstract_phase$proposed,2)
        # df_abstract_phase$split_E_Avg <- round(df_abstract_phase$E_sec/df_abstract_phase$proposed,2)
        # df_abstract_phase$split_F_Avg <- round(df_abstract_phase$F_sec/df_abstract_phase$proposed,2)
        # df_abstract_phase$split_G_Avg <- round(df_abstract_phase$G_sec/df_abstract_phase$proposed,2)
        # df_abstract_phase$split_H_Avg <- round(df_abstract_phase$H_sec/df_abstract_phase$proposed,2)
        # df_abstract_phase$split_I_Avg <- round(df_abstract_phase$I_sec/df_abstract_phase$proposed,2)
      }
      if(number_phases == 4){
        df_abstract_phase$B_sec[df_abstract_phase$B_sec < as.integer(input$file8_5_min_B_sec)] <- as.integer(input$file8_5_min_B_sec)
        df_abstract_phase$C_sec[df_abstract_phase$C_sec < as.integer(input$file8_5_min_C_sec)] <- as.integer(input$file8_5_min_C_sec)
        df_abstract_phase$D_sec[df_abstract_phase$D_sec < as.integer(input$file8_5_min_D_sec)] <- as.integer(input$file8_5_min_D_sec)
        # df_abstract_phase$E_sec[df_abstract_phase$E_sec < as.integer(input$file8_5_min_E_sec)] <- as.integer(input$file8_5_min_E_sec)
        # df_abstract_phase$F_sec[df_abstract_phase$F_sec < as.integer(input$file8_5_min_F_sec)] <- as.integer(input$file8_5_min_F_sec)
        # df_abstract_phase$G_sec[df_abstract_phase$G_sec < as.integer(input$file8_5_min_G_sec)] <- as.integer(input$file8_5_min_G_sec)
        # df_abstract_phase$H_sec[df_abstract_phase$H_sec < as.integer(input$file8_5_min_H_sec)] <- as.integer(input$file8_5_min_H_sec)
        # df_abstract_phase$I_sec[df_abstract_phase$I_sec < as.integer(input$file8_5_min_I_sec)] <- as.integer(input$file8_5_min_I_sec)
        df_abstract_phase$A_sec <- as.integer(round(df_abstract_phase$proposed-(
          +df_abstract_phase$B_sec
          +df_abstract_phase$C_sec
          +df_abstract_phase$D_sec
          # +df_abstract_phase$E_sec
          # +df_abstract_phase$F_sec
          # +df_abstract_phase$G_sec
          # +df_abstract_phase$H_sec
          # +df_abstract_phase$I_sec
        ),0))
        df_abstract_phase$split_A_Avg <- round(df_abstract_phase$A_sec/df_abstract_phase$proposed,2)
        df_abstract_phase$split_B_Avg <- round(df_abstract_phase$B_sec/df_abstract_phase$proposed,2)
        df_abstract_phase$split_C_Avg <- round(df_abstract_phase$C_sec/df_abstract_phase$proposed,2)
        df_abstract_phase$split_D_Avg <- round(df_abstract_phase$D_sec/df_abstract_phase$proposed,2)
        # df_abstract_phase$split_E_Avg <- round(df_abstract_phase$E_sec/df_abstract_phase$proposed,2)
        # df_abstract_phase$split_F_Avg <- round(df_abstract_phase$F_sec/df_abstract_phase$proposed,2)
        # df_abstract_phase$split_G_Avg <- round(df_abstract_phase$G_sec/df_abstract_phase$proposed,2)
        # df_abstract_phase$split_H_Avg <- round(df_abstract_phase$H_sec/df_abstract_phase$proposed,2)
        # df_abstract_phase$split_I_Avg <- round(df_abstract_phase$I_sec/df_abstract_phase$proposed,2)
      }
      if(number_phases == 5){
        df_abstract_phase$B_sec[df_abstract_phase$B_sec < as.integer(input$file8_5_min_B_sec)] <- as.integer(input$file8_5_min_B_sec)
        df_abstract_phase$C_sec[df_abstract_phase$C_sec < as.integer(input$file8_5_min_C_sec)] <- as.integer(input$file8_5_min_C_sec)
        df_abstract_phase$D_sec[df_abstract_phase$D_sec < as.integer(input$file8_5_min_D_sec)] <- as.integer(input$file8_5_min_D_sec)
        df_abstract_phase$E_sec[df_abstract_phase$E_sec < as.integer(input$file8_5_min_E_sec)] <- as.integer(input$file8_5_min_E_sec)
        # df_abstract_phase$F_sec[df_abstract_phase$F_sec < as.integer(input$file8_5_min_F_sec)] <- as.integer(input$file8_5_min_F_sec)
        # df_abstract_phase$G_sec[df_abstract_phase$G_sec < as.integer(input$file8_5_min_G_sec)] <- as.integer(input$file8_5_min_G_sec)
        # df_abstract_phase$H_sec[df_abstract_phase$H_sec < as.integer(input$file8_5_min_H_sec)] <- as.integer(input$file8_5_min_H_sec)
        # df_abstract_phase$I_sec[df_abstract_phase$I_sec < as.integer(input$file8_5_min_I_sec)] <- as.integer(input$file8_5_min_I_sec)
        df_abstract_phase$A_sec <- as.integer(round(df_abstract_phase$proposed-(
          +df_abstract_phase$B_sec
          +df_abstract_phase$C_sec
          +df_abstract_phase$D_sec
          +df_abstract_phase$E_sec
          # +df_abstract_phase$F_sec
          # +df_abstract_phase$G_sec
          # +df_abstract_phase$H_sec
          # +df_abstract_phase$I_sec
        ),0))
        df_abstract_phase$split_A_Avg <- round(df_abstract_phase$A_sec/df_abstract_phase$proposed,2)
        df_abstract_phase$split_B_Avg <- round(df_abstract_phase$B_sec/df_abstract_phase$proposed,2)
        df_abstract_phase$split_C_Avg <- round(df_abstract_phase$C_sec/df_abstract_phase$proposed,2)
        df_abstract_phase$split_D_Avg <- round(df_abstract_phase$D_sec/df_abstract_phase$proposed,2)
        df_abstract_phase$split_E_Avg <- round(df_abstract_phase$E_sec/df_abstract_phase$proposed,2)
        # df_abstract_phase$split_F_Avg <- round(df_abstract_phase$F_sec/df_abstract_phase$proposed,2)
        # df_abstract_phase$split_G_Avg <- round(df_abstract_phase$G_sec/df_abstract_phase$proposed,2)
        # df_abstract_phase$split_H_Avg <- round(df_abstract_phase$H_sec/df_abstract_phase$proposed,2)
        # df_abstract_phase$split_I_Avg <- round(df_abstract_phase$I_sec/df_abstract_phase$proposed,2)
      }
      if(number_phases == 6){
        df_abstract_phase$B_sec[df_abstract_phase$B_sec < as.integer(input$file8_5_min_B_sec)] <- as.integer(input$file8_5_min_B_sec)
        df_abstract_phase$C_sec[df_abstract_phase$C_sec < as.integer(input$file8_5_min_C_sec)] <- as.integer(input$file8_5_min_C_sec)
        df_abstract_phase$D_sec[df_abstract_phase$D_sec < as.integer(input$file8_5_min_D_sec)] <- as.integer(input$file8_5_min_D_sec)
        df_abstract_phase$E_sec[df_abstract_phase$E_sec < as.integer(input$file8_5_min_E_sec)] <- as.integer(input$file8_5_min_E_sec)
        df_abstract_phase$F_sec[df_abstract_phase$F_sec < as.integer(input$file8_5_min_F_sec)] <- as.integer(input$file8_5_min_F_sec)
        # df_abstract_phase$G_sec[df_abstract_phase$G_sec < as.integer(input$file8_5_min_G_sec)] <- as.integer(input$file8_5_min_G_sec)
        # df_abstract_phase$H_sec[df_abstract_phase$H_sec < as.integer(input$file8_5_min_H_sec)] <- as.integer(input$file8_5_min_H_sec)
        # df_abstract_phase$I_sec[df_abstract_phase$I_sec < as.integer(input$file8_5_min_I_sec)] <- as.integer(input$file8_5_min_I_sec)
        df_abstract_phase$A_sec <- as.integer(round(df_abstract_phase$proposed-(
          +df_abstract_phase$B_sec
          +df_abstract_phase$C_sec
          +df_abstract_phase$D_sec
          +df_abstract_phase$E_sec
          +df_abstract_phase$F_sec
          # +df_abstract_phase$G_sec
          # +df_abstract_phase$H_sec
          # +df_abstract_phase$I_sec
        ),0))
        df_abstract_phase$split_A_Avg <- round(df_abstract_phase$A_sec/df_abstract_phase$proposed,2)
        df_abstract_phase$split_B_Avg <- round(df_abstract_phase$B_sec/df_abstract_phase$proposed,2)
        df_abstract_phase$split_C_Avg <- round(df_abstract_phase$C_sec/df_abstract_phase$proposed,2)
        df_abstract_phase$split_D_Avg <- round(df_abstract_phase$D_sec/df_abstract_phase$proposed,2)
        df_abstract_phase$split_E_Avg <- round(df_abstract_phase$E_sec/df_abstract_phase$proposed,2)
        df_abstract_phase$split_F_Avg <- round(df_abstract_phase$F_sec/df_abstract_phase$proposed,2)
        # df_abstract_phase$split_G_Avg <- round(df_abstract_phase$G_sec/df_abstract_phase$proposed,2)
        # df_abstract_phase$split_H_Avg <- round(df_abstract_phase$H_sec/df_abstract_phase$proposed,2)
        # df_abstract_phase$split_I_Avg <- round(df_abstract_phase$I_sec/df_abstract_phase$proposed,2)
      }
      if(number_phases == 7){
        df_abstract_phase$B_sec[df_abstract_phase$B_sec < as.integer(input$file8_5_min_B_sec)] <- as.integer(input$file8_5_min_B_sec)
        df_abstract_phase$C_sec[df_abstract_phase$C_sec < as.integer(input$file8_5_min_C_sec)] <- as.integer(input$file8_5_min_C_sec)
        df_abstract_phase$D_sec[df_abstract_phase$D_sec < as.integer(input$file8_5_min_D_sec)] <- as.integer(input$file8_5_min_D_sec)
        df_abstract_phase$E_sec[df_abstract_phase$E_sec < as.integer(input$file8_5_min_E_sec)] <- as.integer(input$file8_5_min_E_sec)
        df_abstract_phase$F_sec[df_abstract_phase$F_sec < as.integer(input$file8_5_min_F_sec)] <- as.integer(input$file8_5_min_F_sec)
        df_abstract_phase$G_sec[df_abstract_phase$G_sec < as.integer(input$file8_5_min_G_sec)] <- as.integer(input$file8_5_min_G_sec)
        # df_abstract_phase$H_sec[df_abstract_phase$H_sec < as.integer(input$file8_5_min_H_sec)] <- as.integer(input$file8_5_min_H_sec)
        # df_abstract_phase$I_sec[df_abstract_phase$I_sec < as.integer(input$file8_5_min_I_sec)] <- as.integer(input$file8_5_min_I_sec)
        df_abstract_phase$A_sec <- as.integer(round(df_abstract_phase$proposed-(
          +df_abstract_phase$B_sec
          +df_abstract_phase$C_sec
          +df_abstract_phase$D_sec
          +df_abstract_phase$E_sec
          +df_abstract_phase$F_sec
          +df_abstract_phase$G_sec
          # +df_abstract_phase$H_sec
          # +df_abstract_phase$I_sec
        ),0))
        df_abstract_phase$split_A_Avg <- round(df_abstract_phase$A_sec/df_abstract_phase$proposed,2)
        df_abstract_phase$split_B_Avg <- round(df_abstract_phase$B_sec/df_abstract_phase$proposed,2)
        df_abstract_phase$split_C_Avg <- round(df_abstract_phase$C_sec/df_abstract_phase$proposed,2)
        df_abstract_phase$split_D_Avg <- round(df_abstract_phase$D_sec/df_abstract_phase$proposed,2)
        df_abstract_phase$split_E_Avg <- round(df_abstract_phase$E_sec/df_abstract_phase$proposed,2)
        df_abstract_phase$split_F_Avg <- round(df_abstract_phase$F_sec/df_abstract_phase$proposed,2)
        df_abstract_phase$split_G_Avg <- round(df_abstract_phase$G_sec/df_abstract_phase$proposed,2)
        # df_abstract_phase$split_H_Avg <- round(df_abstract_phase$H_sec/df_abstract_phase$proposed,2)
        # df_abstract_phase$split_I_Avg <- round(df_abstract_phase$I_sec/df_abstract_phase$proposed,2)
      }
      if(number_phases == 8){
        df_abstract_phase$B_sec[df_abstract_phase$B_sec < as.integer(input$file8_5_min_B_sec)] <- as.integer(input$file8_5_min_B_sec)
        df_abstract_phase$C_sec[df_abstract_phase$C_sec < as.integer(input$file8_5_min_C_sec)] <- as.integer(input$file8_5_min_C_sec)
        df_abstract_phase$D_sec[df_abstract_phase$D_sec < as.integer(input$file8_5_min_D_sec)] <- as.integer(input$file8_5_min_D_sec)
        df_abstract_phase$E_sec[df_abstract_phase$E_sec < as.integer(input$file8_5_min_E_sec)] <- as.integer(input$file8_5_min_E_sec)
        df_abstract_phase$F_sec[df_abstract_phase$F_sec < as.integer(input$file8_5_min_F_sec)] <- as.integer(input$file8_5_min_F_sec)
        df_abstract_phase$G_sec[df_abstract_phase$G_sec < as.integer(input$file8_5_min_G_sec)] <- as.integer(input$file8_5_min_G_sec)
        df_abstract_phase$H_sec[df_abstract_phase$H_sec < as.integer(input$file8_5_min_H_sec)] <- as.integer(input$file8_5_min_H_sec)
        # df_abstract_phase$I_sec[df_abstract_phase$I_sec < as.integer(input$file8_5_min_I_sec)] <- as.integer(input$file8_5_min_I_sec)
        df_abstract_phase$A_sec <- as.integer(round(df_abstract_phase$proposed-(
          +df_abstract_phase$B_sec
          +df_abstract_phase$C_sec
          +df_abstract_phase$D_sec
          +df_abstract_phase$E_sec
          +df_abstract_phase$F_sec
          +df_abstract_phase$G_sec
          +df_abstract_phase$H_sec
          # +df_abstract_phase$I_sec
        ),0))
        df_abstract_phase$split_A_Avg <- round(df_abstract_phase$A_sec/df_abstract_phase$proposed,2)
        df_abstract_phase$split_B_Avg <- round(df_abstract_phase$B_sec/df_abstract_phase$proposed,2)
        df_abstract_phase$split_C_Avg <- round(df_abstract_phase$C_sec/df_abstract_phase$proposed,2)
        df_abstract_phase$split_D_Avg <- round(df_abstract_phase$D_sec/df_abstract_phase$proposed,2)
        df_abstract_phase$split_E_Avg <- round(df_abstract_phase$E_sec/df_abstract_phase$proposed,2)
        df_abstract_phase$split_F_Avg <- round(df_abstract_phase$F_sec/df_abstract_phase$proposed,2)
        df_abstract_phase$split_G_Avg <- round(df_abstract_phase$G_sec/df_abstract_phase$proposed,2)
        df_abstract_phase$split_H_Avg <- round(df_abstract_phase$H_sec/df_abstract_phase$proposed,2)
        # df_abstract_phase$split_I_Avg <- round(df_abstract_phase$I_sec/df_abstract_phase$proposed,2)
      }
      if(number_phases == 9){
        df_abstract_phase$B_sec[df_abstract_phase$B_sec < as.integer(input$file8_5_min_B_sec)] <- as.integer(input$file8_5_min_B_sec)
        df_abstract_phase$C_sec[df_abstract_phase$C_sec < as.integer(input$file8_5_min_C_sec)] <- as.integer(input$file8_5_min_C_sec)
        df_abstract_phase$D_sec[df_abstract_phase$D_sec < as.integer(input$file8_5_min_D_sec)] <- as.integer(input$file8_5_min_D_sec)
        df_abstract_phase$E_sec[df_abstract_phase$E_sec < as.integer(input$file8_5_min_E_sec)] <- as.integer(input$file8_5_min_E_sec)
        df_abstract_phase$F_sec[df_abstract_phase$F_sec < as.integer(input$file8_5_min_F_sec)] <- as.integer(input$file8_5_min_F_sec)
        df_abstract_phase$G_sec[df_abstract_phase$G_sec < as.integer(input$file8_5_min_G_sec)] <- as.integer(input$file8_5_min_G_sec)
        df_abstract_phase$H_sec[df_abstract_phase$H_sec < as.integer(input$file8_5_min_H_sec)] <- as.integer(input$file8_5_min_H_sec)
        df_abstract_phase$I_sec[df_abstract_phase$I_sec < as.integer(input$file8_5_min_I_sec)] <- as.integer(input$file8_5_min_I_sec)
        df_abstract_phase$A_sec <- as.integer(round(df_abstract_phase$proposed-(
          +df_abstract_phase$B_sec
          +df_abstract_phase$C_sec
          +df_abstract_phase$D_sec
          +df_abstract_phase$E_sec
          +df_abstract_phase$F_sec
          +df_abstract_phase$G_sec
          +df_abstract_phase$H_sec
          +df_abstract_phase$I_sec
        ),0))
        df_abstract_phase$split_A_Avg <- round(df_abstract_phase$A_sec/df_abstract_phase$proposed,2)
        df_abstract_phase$split_B_Avg <- round(df_abstract_phase$B_sec/df_abstract_phase$proposed,2)
        df_abstract_phase$split_C_Avg <- round(df_abstract_phase$C_sec/df_abstract_phase$proposed,2)
        df_abstract_phase$split_D_Avg <- round(df_abstract_phase$D_sec/df_abstract_phase$proposed,2)
        df_abstract_phase$split_E_Avg <- round(df_abstract_phase$E_sec/df_abstract_phase$proposed,2)
        df_abstract_phase$split_F_Avg <- round(df_abstract_phase$F_sec/df_abstract_phase$proposed,2)
        df_abstract_phase$split_G_Avg <- round(df_abstract_phase$G_sec/df_abstract_phase$proposed,2)
        df_abstract_phase$split_H_Avg <- round(df_abstract_phase$H_sec/df_abstract_phase$proposed,2)
        df_abstract_phase$split_I_Avg <- round(df_abstract_phase$I_sec/df_abstract_phase$proposed,2)
      }
      df_abstract_phase$time <- hms::as_hms(df_abstract_phase$time)
    }
    # df_abstract_phase$time <- hms::as_hms(df_abstract_phase$time)
    df_abstract_phase$time <- as.character(df_abstract_phase$time)
    output$contents_8_4 <- renderTable( 
      df_abstract_phase,
      caption = paste0("Final Results for Site ",input$site_historical_8),
      caption.placement = getOption("xtable.caption.placement", "top")
    )
  }
  )
  observeEvent(input$previewData9, {
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = "Creating a Plot....", value = 0)  
    Dates1 <- as.list(input$date9_1_multiple) 
    Dates2 <- as.list(input$date9_2_multiple) 
    lower_time <- paste0(hms::as_hms(input$file9_2_lower_time)) 
    upper_time <- paste0(hms::as_hms(input$file9_2_upper_time)) 
    input_file <- paste0(input$file9_2_ritis_travel_time_file$datapath)
    TMC_file <- paste0(input$file9_2_ritis_tmc_identification$datapath)
    TMC_lists <- strsplit(input$file9_2_TMClist, "\n")
    TMC_lists <- as.list(unlist(TMC_lists))
    direction <- paste0(input$file_9_2_direction)
    site <- as.integer(input$file9_2_site_ID)
    file_variation_log <- paste0(input$file9_2_SCATS_log$datapath)
    var1 <- as.integer(input$file9_2_var1)
    var2 <- as.integer(input$file9_2_var2)
    df <- readr::read_csv(input_file)
    df <- df %>% dplyr::filter(tmc_code %in% TMC_lists)
    df$Date <- as.Date(substr(df$measurement_tstamp,1,10))
    df$time <- as.integer(hms::as_hms(substr(df$measurement_tstamp,11,19)))
    variation_routin_display <- function(site, file_location, var_on, var_off, date, lower_time, upper_time){
      df <- readr::read_csv(file_location)
      df <- df %>% dplyr::filter(Site == site)
      df <- df %>% dplyr::filter(User == "VR")
      df$Date <- format(as.Date(df$Date,'%d/%m/%Y'), '%m/%d/%Y')
      df <- df %>% dplyr::filter(Date == format(as.Date(date), '%m/%d/%Y'))
      df = df[!duplicated(df$Time,fromLast=T),]
      df1 <- df %>% dplyr::filter(grepl(paste0(" ",as.character(var_on),": I="),Details))
      df1$var <- 1
      df2 <- df %>% dplyr::filter(grepl(paste0(" ",as.character(var_off),": I="),Details))
      df2$var <- 0
      df <- rbind(df1,df2)
      df <- df[order(df$Time),]
      
      df$duration <- NULL
      if(nrow(df) != 0){
        if((nrow(df) %% 2 == 0) == TRUE && (df$var[nrow(df)] == 0)){
          for (i in (1:(nrow(df)-1))){
            print(i)
            if((df$var[i]==1)==TRUE && (df$var[i+1]==0) ==TRUE){
              df$duration[i] <- df$Time[i+1]-df$Time[i]
            } else if ((df$var[i]==1) == TRUE && (df$var[i+1]==1) == TRUE ){
              df$duration[i] <- 0
            } else if ((df$var[i]==0) == TRUE && (df$var[i+1]==1) == TRUE){
              df$duration[i] <- 0
            }
            df$duration[nrow(df)] <- 0
          }
        }
        if((nrow(df) %% 2 == 0) == FALSE && df$var[nrow(df)] == 0){
          for (i in (1:(nrow(df)-1))){
            print(i)
            if((df$var[i]==1) && (df$var[i+1]==0)){
              df$duration[i] <- df$Time[i+1]-df$Time[i]
            } else if ((df$var[i]==1) == TRUE && (df$var[i+1]==1)){
              df$duration[i] <- 0
            } else if ((df$var[i]==0) && (df$var[i+1]==1)){
              df$duration[i] <- 0
            }
            df$duration[nrow(df)] <- 0
          }
        }
        if((nrow(df) %% 2 == 0) == FALSE && df$var[nrow(df)] == 1){
          for (i in (1:(nrow(df)-1))){
            print(i)
            if((df$var[i]==1) && (df$var[i+1]==0)){
              df$duration[i] <- df$Time[i+1]-df$Time[i]
            } else if ((df$var[i]==1) == TRUE && (df$var[i+1]==1)){
              df$duration[i] <- 0
            } else if ((df$var[i]==0) && (df$var[i+1]==1)){
              df$duration[i] <- 0
            }
            df$duration[nrow(df)] <- hms::as_hms(11:59:59) - df$Time[nrow(df)]  
          }
        }
        if((nrow(df) %% 2 == 0) == TRUE && df$var[nrow(df)] == 1){
          for (i in (1:(nrow(df)-1))){
            print(i)
            if((df$var[i]==1) && (df$var[i+1]==0)){
              df$duration[i] <- df$Time[i+1]-df$Time[i]
            } else if ((df$var[i]==1) == TRUE && (df$var[i+1]==1)){
              df$duration[i] <- 0
            } else if ((df$var[i]==0) && (df$var[i+1]==1)){
              df$duration[i] <- 0
            }
            df$duration[nrow(df)] <- hms::as_hms(11:59:59) - df$Time[nrow(df)]  
          }
        }
        
        df <- df %>% dplyr::filter(duration != 0)
        df$Time2 <- hms::as_hms(as.integer(df$Time) + df$duration)
      }
      df_time <- as.data.frame(seq(1,60*24, by =1))
      colnames(df_time) <- c("time")
      df_time <- df_time*60
      df_time$flag <- 0
      if(nrow(df) != 0){
        for(k in (1:nrow(df_time))){
          for ( l in (1:nrow(df))){
            if(df_time$flag[k] == 1){
              df_time$flag[k] <- 1
            } else if(df_time$time[k] >= as.integer(df$Time[l]) && df_time$time[k] <= as.integer(df$Time2[l])){
              df_time$flag[k] <- 1
            }
          }
        }
      }
      lower_time_int <- as.integer(hms::as_hms(lower_time))
      upper_time_int <- as.integer(hms::as_hms(upper_time))
      df_time <- df_time[df_time$time >= as.integer(hms::as_hms(lower_time)),]
      df_time <- df_time[df_time$time <= as.integer(hms::as_hms(upper_time)),]
      return(df_time)
    }
    df_variation_date1 <- variation_routin_display(site = site , file_location = file_variation_log, var_on = var1, var_off = var2, date = Dates1[[1]], lower_time = lower_time , upper_time = upper_time)
    df_variation_date2 <- variation_routin_display(site = site , file_location = file_variation_log, var_on = var1, var_off = var2, date = Dates2[[1]], lower_time = lower_time , upper_time = upper_time)
    TMC_df <- readr::read_csv(TMC_file)
    TMC_df <- TMC_df %>% dplyr::filter(tmc %in% TMC_lists)
    TMC_length <- round(sum(TMC_df$miles),2)
    df1 <- df %>% dplyr::filter(Date == Dates1)
    df1 <- df1 %>% dplyr::filter(time >= as.integer(hms::as_hms(lower_time)))
    df1 <- df1 %>% dplyr::filter(time <= as.integer(hms::as_hms(upper_time)))
    results1 <- df1 %>% group_by(Date, time) %>%
      summarise(Travel_time_mts  = sum(travel_time_seconds/60))
    df2 <- df %>% dplyr::filter(Date == Dates2)
    df2 <- df2 %>% dplyr::filter(time >= as.integer(hms::as_hms(lower_time)))
    df2 <- df2 %>% dplyr::filter(time <= as.integer(hms::as_hms(upper_time)))
    results2 <- df2 %>% group_by(Date, time) %>%
      summarise(Travel_time_mts  = sum(travel_time_seconds/60))
    results1_time <- as.data.frame(seq(1,60*24, by =1))
    colnames(results1_time) <- c("time")
    results1_time$time <- results1_time$time*60
    results1_time <- results1_time %>% dplyr::filter(time >= as.integer(hms::as_hms(lower_time)))
    results1_time <- results1_time %>% dplyr::filter(time <= as.integer(hms::as_hms(upper_time)))
    results1_time <- sqldf::sqldf("select a.time, b.Date, b.Travel_time_mts from results1_time as a
                              left join results1 as b
                              on a.time = b.time", drv = "SQLite")
    results1_time <- tidyr::fill(results1_time, c("Date", "Travel_time_mts"), .direction = "downup")
    results2_time <- as.data.frame(seq(1,60*24, by =1))
    colnames(results2_time) <- c("time")
    results2_time$time <- results2_time$time*60
    results2_time <- results2_time %>% dplyr::filter(time >= as.integer(hms::as_hms(lower_time)))
    results2_time <- results2_time %>% dplyr::filter(time <= as.integer(hms::as_hms(upper_time)))
    results2_time <- sqldf::sqldf("select a.time, b.Date, b.Travel_time_mts from results2_time as a
                              left join results2 as b
                              on a.time = b.time", drv = "SQLite")
    results2_time <- tidyr::fill(results2_time, c("Date", "Travel_time_mts"), .direction = "downup")
    date1_var_utilization <- as.integer(round(sum(df_variation_date1$flag)/nrow(df_variation_date1)*100,0))
    date2_var_utilization <- as.integer(round(sum(df_variation_date2$flag)/nrow(df_variation_date2)*100,0))
    lower_time_int <- as.integer(hms::as_hms(lower_time))
    upper_time_int <- as.integer(hms::as_hms(upper_time))
    name11 <- paste0("Group1  ", round(mean(results1_time$Travel_time_mts),2)," mts")
    name22 <- paste0("Group2  ", round(mean(results2_time$Travel_time_mts),2)," mts")
    name33 <- paste0("VR Utilizing Group1  ", date1_var_utilization," %")
    name44 <- paste0("VR Utilizing Group2  ", date2_var_utilization," %")
    name1 <- paste0("Avg Group1")
    name2 <- paste0("Avg Group2")
    xdata <- hms::as_hms(unique(results1_time$time))
    y1 <- results1_time$Travel_time_mts
    y2 <- results2_time$Travel_time_mts
    y3 <- df_variation_date1$flag
    y4 <- df_variation_date2$flag
    y11 <- rep(mean(results1_time$Travel_time_mts),each = nrow(results1_time))
    y22 <- rep(mean(results2_time$Travel_time_mts),each = nrow(results2_time))
    df_plot <- data.frame(xdata, y1, y2, y3, y4)
    g_file_9_1 <- ggplot(df_plot, aes(xdata))
    g_file_9_1 <- g_file_9_1 + geom_line(aes(y=y1, color = name1))
    g_file_9_1 <- g_file_9_1 + geom_line(aes(y=y2, color = name2))
    g_file_9_1 <- g_file_9_1 + geom_line(aes(y=y3*max(y1,y2)/8, color = name1), size = 1, linetype = "dotted")
    g_file_9_1 <- g_file_9_1 + geom_line(aes(y=y4*max(y1,y2)/4, color = name2), size = 1, linetype = "dotted")
    g_file_9_1 <- g_file_9_1 + geom_line(aes(y=y11, color = name11))
    g_file_9_1 <- g_file_9_1 + geom_line(aes(y=y22, color = name22))
    g_file_9_1 <- g_file_9_1 + geom_line(aes(y=y11, color = name33))
    g_file_9_1 <- g_file_9_1 + geom_line(aes(y=y22, color = name44))
    g_file_9_1 <- g_file_9_1 + theme(plot.title = element_text(color="black", size=10, face="bold.italic"),
                   axis.text=element_text(size=8),
                   axis.title=element_text(size=9,face="bold"),
                   legend.text=element_text(size=9),
                   legend.justification=c(1,1),legend.position=c(1,1),legend.title=element_blank())
    g_file_9_1 <- g_file_9_1 + scale_color_manual(name = "Dates", values = c("red","darkblue","red","darkblue","red","darkblue"))
    g_file_9_1 <- g_file_9_1 + ylab(paste0("Travel Time (mts)")) + xlab("Time")
    g_file_9_1 <- g_file_9_1 + scale_x_continuous(labels = hms::as_hms(seq(lower_time_int, upper_time_int,by=3600)), breaks=seq(lower_time_int, upper_time_int,by=3600))
    g_file_9_1 <- g_file_9_1 + ylim(0, max(results2_time$Travel_time_mts,results1_time$Travel_time_mts))
    g_file_9_1 <- g_file_9_1 + theme_calc()
    # https://stackoverflow.com/questions/49735290/ggplot2-color-individual-words-in-title-to-match-colors-of-groups
    xmin <- lower_time_int-(upper_time_int-lower_time_int)/3
    xmax <- lower_time_int + as.integer(((upper_time_int - lower_time_int)*1))-(upper_time_int-lower_time_int)/3
    ymin <- min(y1,y2) + (max(y1,y2) - min(y1,y2) )*1.02
    ymax <- min(y1,y2) + (max(y1,y2) - min(y1,y2) )*1.1
    g_file_9_1 <- g_file_9_1  +
      annotation_custom(textGrob(paste0("Travel Time on ", site  ," ",direction," for ",TMC_length," miles"," for "), gp = gpar(col = 'black'))
                        ,xmin = xmin+(xmax-xmin)*0.10, xmax = xmax, ymin = ymin, ymax = ymax
      ) +
      annotation_custom(textGrob(paste0("Group1 ",as.character(Dates1[[1]])), gp = gpar(col = 'red', fontface = 'bold'))
                        ,xmin = xmin+(xmax-xmin)*0.72, xmax = xmax, ymin = ymin, ymax = ymax
      ) +
      annotation_custom(textGrob(" and ", gp = gpar(col = 'black'))
                        ,xmin = xmin+(xmax-xmin)*0.97, xmax = xmax, ymin = ymin, ymax = ymax
      ) +
      annotation_custom(textGrob(paste0("Group2 ",as.character(Dates2[[1]])), gp = gpar(col = 'darkblue', fontface = 'bold'))
                        ,xmin = xmin+(xmax-xmin)*1.22, xmax = xmax, ymin = ymin, ymax = ymax
      )
    output$Plot9_1 <- renderPlot({g_file_9_1})
  })
  observeEvent(input$previewData9_2, {
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = "Creating a Plot....", value = 0)  
    Dates1 <- as.list(input$date9_1_multiple) 
    Dates2 <- as.list(input$date9_2_multiple) 
    lower_time <- paste0(hms::as_hms(input$file9_2_lower_time)) 
    upper_time <- paste0(hms::as_hms(input$file9_2_upper_time)) 
    direction <- paste0(input$file_9_2_direction)
    site <- as.integer(input$file9_2_site_ID)
    file_variation_log <- paste0(input$file9_2_SCATS_log$datapath)
    var1 <- as.integer(input$file9_2_var1)
    var2 <- as.integer(input$file9_2_var2)
    
    site_subsytem_file <- "https://raw.githubusercontent.com/shabarekjpcl/ITS_NJDOT/main/Site_Subsystem.csv"
    phase_naming_file <- "https://raw.githubusercontent.com/shabarekjpcl/ITS_NJDOT/main/SCATS_SAs.csv"
    phase_directions_file <- "https://raw.githubusercontent.com/shabarekjpcl/ITS_NJDOT/main/SCATS_Phasing.csv"
    df_final_ds_9 <- NULL
    df_final_ds_9_2 <- NULL
    for (kk in (1:length(input$file9_2_sm_files$datapath))){
      file <- input$file9_2_sm_files$datapath[kk]
      # text <- readtext::readtext(file)
      # text2 <- as.data.frame(gsub("\n", "\t ", text))
      # text2 <- as.data.frame(gsub("\t  ", "\t ", text2))
      # # text2 <- as.data.frame(gsub("\n ", "\\n ", text2))
      # write.table(text2, "C:/Users/Abdullah.Shabarek/Desktop/SM/temp.txt")
      # df <- read.delim2("C:/Users/Abdullah.Shabarek/Desktop/SM/temp.txt",header = FALSE)
      
      df <- read.delim2(file,header = FALSE)
      Day_name <- as.character(stringr::str_match(df$V1[1], "Strategic Monitor On \\s*(.*?)\\s* ")[1,][2])
      Date_name <- as.character(stringr::str_match(df$V1[1], paste0(Day_name, " \\s*(.*?)\\s* "))[1,][2])
      Date_name2 <- Date_name
      Date_name2 <- gsub("January", "1", Date_name2)
      Date_name2 <- gsub("February", "2", Date_name2)
      Date_name2 <- gsub("March", "3", Date_name2)
      Date_name2 <- gsub("April", "4", Date_name2)
      Date_name2 <- gsub("May", "5", Date_name2)
      Date_name2 <- gsub("June", "6", Date_name2)
      Date_name2 <- gsub("July", "7", Date_name2)
      Date_name2 <- gsub("August", "8", Date_name2)
      Date_name2 <- gsub("September", "9", Date_name2)
      Date_name2 <- gsub("October", "10", Date_name2)
      Date_name2 <- gsub("November", "11", Date_name2)
      Date_name2 <- gsub("December", "12", Date_name2)
      Date_name2 <- as.Date(Date_name2,format = "%d-%m-%Y")
      Subsystem_number <- as.integer(substr(df$V1[2],nchar(Day_name)+1+nchar(Date_name)+1+6+4+1,nchar(Day_name)+1+nchar(Date_name)+1+6+4+2))
      
      if(input$Rt_file_9 == "RT1"){Subsystem_number <- as.integer(paste0("100",Subsystem_number))}
      if(input$Rt_file_9 == "RT18"){Subsystem_number <- as.integer(paste0("18",Subsystem_number))}
      if(input$Rt_file_9 == "RT73"){Subsystem_number <- as.integer(paste0("73",Subsystem_number))}
      if(input$Rt_file_9 == "RT130"){Subsystem_number <- as.integer(paste0("130",Subsystem_number))}
      
      if(Date_name2 %in% Dates1  & Subsystem_number==site){
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
      df22$time <- as.integer(df22$time)
      time_df <- data.frame(seq(60,60*60*24,by=60))
      colnames(time_df) <- c("time")
      df22 <- time_df %>% dplyr::left_join(df22, by = c("time" = "time"))
      df22 <- df22 %>% tidyr::fill(Degree_Saturation,.direction = c("downup"))
      df22 <- df22 %>% tidyr::fill(Cycle_Length,.direction = c("downup"))
      df22 <- df22 %>% tidyr::fill(Required_Cycle_Length,.direction = c("downup"))
      df22 <- df22 %>% tidyr::fill(Progression,.direction = c("downup"))
      df22 <- df22 %>% tidyr::fill(rotation,.direction = c("downup"))
      df22 <- df22 %>% tidyr::fill(phase_A_GT,.direction = c("downup"))
      df22 <- df22 %>% tidyr::fill(phase_B_GT,.direction = c("downup"))
      df22 <- df22 %>% tidyr::fill(phase_C_GT,.direction = c("downup"))
      df22 <- df22 %>% tidyr::fill(phase_D_GT,.direction = c("downup"))
      df22 <- df22 %>% tidyr::fill(phase_E_GT,.direction = c("downup"))
      df22 <- df22 %>% tidyr::fill(phase_F_GT,.direction = c("downup"))
      df22 <- df22 %>% tidyr::fill(phase_G_GT,.direction = c("downup"))
      df22 <- df22 %>% tidyr::fill(phase_H_GT,.direction = c("downup"))
      df22 <- df22 %>% tidyr::fill(phase_I_GT,.direction = c("downup"))
      df22 <- df22 %>% dplyr::select("time","Degree_Saturation","Cycle_Length"
                                     ,"Required_Cycle_Length","rotation",
                                     "phase_A_GT","phase_B_GT","phase_C_GT"
                                     ,"phase_D_GT","phase_E_GT","phase_F_GT",
                                     "phase_G_GT","phase_H_GT","phase_I_GT")
      df22$phase_A_GT[is.na(df22$phase_A_GT)] <- 0
      df22$phase_B_GT[is.na(df22$phase_B_GT)] <- 0
      df22$phase_C_GT[is.na(df22$phase_C_GT)] <- 0
      df22$phase_D_GT[is.na(df22$phase_D_GT)] <- 0
      df22$phase_E_GT[is.na(df22$phase_E_GT)] <- 0
      df22$phase_F_GT[is.na(df22$phase_F_GT)] <- 0
      df22$phase_G_GT[is.na(df22$phase_G_GT)] <- 0
      df22$phase_H_GT[is.na(df22$phase_H_GT)] <- 0
      df22$phase_I_GT[is.na(df22$phase_I_GT)] <- 0
      df22$Degree_Saturation <- as.numeric(df22$Degree_Saturation)
      df22$Cycle_Length <- as.numeric(df22$Cycle_Length)
      df22$Required_Cycle_Length <- as.numeric(df22$Required_Cycle_Length)
      df22$rotation <- as.numeric(df22$rotation)
      df22$phase_A_GT <- as.numeric(df22$phase_A_GT)
      df22$phase_B_GT <- as.numeric(df22$phase_B_GT)
      df22$phase_C_GT <- as.numeric(df22$phase_C_GT)
      df22$phase_D_GT <- as.numeric(df22$phase_D_GT)
      df22$phase_E_GT <- as.numeric(df22$phase_E_GT)
      df22$phase_F_GT <- as.numeric(df22$phase_F_GT)
      df22$phase_G_GT <- as.numeric(df22$phase_G_GT)
      df22$phase_H_GT <- as.numeric(df22$phase_H_GT)
      df22$phase_I_GT <- as.numeric(df22$phase_I_GT)
      
      df_final_ds_9 <- rbind(df22,df_final_ds_9)
      
      df_final_ds_9 <- df_final_ds_9 %>% dplyr::group_by(time) %>% summarise(
        Degree_Saturation =  mean(Degree_Saturation),
        Cycle_Length =  mean(Cycle_Length),
        Required_Cycle_Length =  mean(Required_Cycle_Length),
        rotation =  mean(rotation),
        phase_A_GT =  mean(phase_A_GT),
        phase_B_GT =  mean(phase_B_GT),
        phase_C_GT =  mean(phase_C_GT),
        phase_D_GT =  mean(phase_D_GT),
        phase_E_GT =  mean(phase_E_GT),
        phase_F_GT =  mean(phase_F_GT),
        phase_G_GT =  mean(phase_G_GT),
        phase_H_GT =  mean(phase_H_GT),
        phase_I_GT =  mean(phase_I_GT)
      )
      if(sum(df_final_ds_9$phase_A_GT) == 0){df_final_ds_9$phase_A_GT <- NULL}
      if(sum(df_final_ds_9$phase_B_GT) == 0){df_final_ds_9$phase_B_GT <- NULL}
      if(sum(df_final_ds_9$phase_C_GT) == 0){df_final_ds_9$phase_C_GT <- NULL}
      if(sum(df_final_ds_9$phase_D_GT) == 0){df_final_ds_9$phase_D_GT <- NULL}
      if(sum(df_final_ds_9$phase_E_GT) == 0){df_final_ds_9$phase_E_GT <- NULL}
      if(sum(df_final_ds_9$phase_F_GT) == 0){df_final_ds_9$phase_F_GT <- NULL}
      if(sum(df_final_ds_9$phase_G_GT) == 0){df_final_ds_9$phase_G_GT <- NULL}
      if(sum(df_final_ds_9$phase_H_GT) == 0){df_final_ds_9$phase_H_GT <- NULL}
      if(sum(df_final_ds_9$phase_I_GT) == 0){df_final_ds_9$phase_I_GT <- NULL}
      number_of_phases <- ncol(df_final_ds_9) - 5 
      }
    }
    for (kk in (1:length(input$file9_2_sm_files$datapath))){
      file <- input$file9_2_sm_files$datapath[kk]
      # text <- readtext::readtext(file)
      # text2 <- as.data.frame(gsub("\n", "\t ", text))
      # text2 <- as.data.frame(gsub("\t  ", "\t ", text2))
      # # text2 <- as.data.frame(gsub("\n ", "\\n ", text2))
      # write.table(text2, "C:/Users/Abdullah.Shabarek/Desktop/SM/temp.txt")
      # df <- read.delim2("C:/Users/Abdullah.Shabarek/Desktop/SM/temp.txt",header = FALSE)
      
      df <- read.delim2(file,header = FALSE)
      Day_name <- as.character(stringr::str_match(df$V1[1], "Strategic Monitor On \\s*(.*?)\\s* ")[1,][2])
      Date_name <- as.character(stringr::str_match(df$V1[1], paste0(Day_name, " \\s*(.*?)\\s* "))[1,][2])
      Date_name2 <- Date_name
      Date_name2 <- gsub("January", "1", Date_name2)
      Date_name2 <- gsub("February", "2", Date_name2)
      Date_name2 <- gsub("March", "3", Date_name2)
      Date_name2 <- gsub("April", "4", Date_name2)
      Date_name2 <- gsub("May", "5", Date_name2)
      Date_name2 <- gsub("June", "6", Date_name2)
      Date_name2 <- gsub("July", "7", Date_name2)
      Date_name2 <- gsub("August", "8", Date_name2)
      Date_name2 <- gsub("September", "9", Date_name2)
      Date_name2 <- gsub("October", "10", Date_name2)
      Date_name2 <- gsub("November", "11", Date_name2)
      Date_name2 <- gsub("December", "12", Date_name2)
      Date_name2 <- as.Date(Date_name2,format = "%d-%m-%Y")
      Subsystem_number <- as.integer(substr(df$V1[2],nchar(Day_name)+1+nchar(Date_name)+1+6+4+1,nchar(Day_name)+1+nchar(Date_name)+1+6+4+2))
      
      if(input$Rt_file_9 == "RT1"){Subsystem_number <- as.integer(paste0("100",Subsystem_number))}
      if(input$Rt_file_9 == "RT18"){Subsystem_number <- as.integer(paste0("18",Subsystem_number))}
      if(input$Rt_file_9 == "RT73"){Subsystem_number <- as.integer(paste0("73",Subsystem_number))}
      if(input$Rt_file_9 == "RT130"){Subsystem_number <- as.integer(paste0("130",Subsystem_number))}
      
      if(Date_name2 %in% Dates2  & Subsystem_number==site){
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
        df22$time <- as.integer(df22$time)
        time_df <- data.frame(seq(60,60*60*24,by=60))
        colnames(time_df) <- c("time")
        df22 <- time_df %>% dplyr::left_join(df22, by = c("time" = "time"))
        df22 <- df22 %>% tidyr::fill(Degree_Saturation,.direction = c("downup"))
        df22 <- df22 %>% tidyr::fill(Cycle_Length,.direction = c("downup"))
        df22 <- df22 %>% tidyr::fill(Required_Cycle_Length,.direction = c("downup"))
        df22 <- df22 %>% tidyr::fill(Progression,.direction = c("downup"))
        df22 <- df22 %>% tidyr::fill(rotation,.direction = c("downup"))
        df22 <- df22 %>% tidyr::fill(phase_A_GT,.direction = c("downup"))
        df22 <- df22 %>% tidyr::fill(phase_B_GT,.direction = c("downup"))
        df22 <- df22 %>% tidyr::fill(phase_C_GT,.direction = c("downup"))
        df22 <- df22 %>% tidyr::fill(phase_D_GT,.direction = c("downup"))
        df22 <- df22 %>% tidyr::fill(phase_E_GT,.direction = c("downup"))
        df22 <- df22 %>% tidyr::fill(phase_F_GT,.direction = c("downup"))
        df22 <- df22 %>% tidyr::fill(phase_G_GT,.direction = c("downup"))
        df22 <- df22 %>% tidyr::fill(phase_H_GT,.direction = c("downup"))
        df22 <- df22 %>% tidyr::fill(phase_I_GT,.direction = c("downup"))
        df22 <- df22 %>% dplyr::select("time","Degree_Saturation","Cycle_Length"
                                       ,"Required_Cycle_Length","rotation",
                                       "phase_A_GT","phase_B_GT","phase_C_GT"
                                       ,"phase_D_GT","phase_E_GT","phase_F_GT",
                                       "phase_G_GT","phase_H_GT","phase_I_GT")
        df22$phase_A_GT[is.na(df22$phase_A_GT)] <- 0
        df22$phase_B_GT[is.na(df22$phase_B_GT)] <- 0
        df22$phase_C_GT[is.na(df22$phase_C_GT)] <- 0
        df22$phase_D_GT[is.na(df22$phase_D_GT)] <- 0
        df22$phase_E_GT[is.na(df22$phase_E_GT)] <- 0
        df22$phase_F_GT[is.na(df22$phase_F_GT)] <- 0
        df22$phase_G_GT[is.na(df22$phase_G_GT)] <- 0
        df22$phase_H_GT[is.na(df22$phase_H_GT)] <- 0
        df22$phase_I_GT[is.na(df22$phase_I_GT)] <- 0
        df22$Degree_Saturation <- as.numeric(df22$Degree_Saturation)
        df22$Cycle_Length <- as.numeric(df22$Cycle_Length)
        df22$Required_Cycle_Length <- as.numeric(df22$Required_Cycle_Length)
        df22$rotation <- as.numeric(df22$rotation)
        df22$phase_A_GT <- as.numeric(df22$phase_A_GT)
        df22$phase_B_GT <- as.numeric(df22$phase_B_GT)
        df22$phase_C_GT <- as.numeric(df22$phase_C_GT)
        df22$phase_D_GT <- as.numeric(df22$phase_D_GT)
        df22$phase_E_GT <- as.numeric(df22$phase_E_GT)
        df22$phase_F_GT <- as.numeric(df22$phase_F_GT)
        df22$phase_G_GT <- as.numeric(df22$phase_G_GT)
        df22$phase_H_GT <- as.numeric(df22$phase_H_GT)
        df22$phase_I_GT <- as.numeric(df22$phase_I_GT)
        
        df_final_ds_9_2 <- rbind(df22,df_final_ds_9_2)
      
      df_final_ds_9_2 <- df_final_ds_9_2 %>% dplyr::group_by(time) %>% summarise(
        Degree_Saturation =  mean(Degree_Saturation),
        Cycle_Length =  mean(Cycle_Length),
        Required_Cycle_Length =  mean(Required_Cycle_Length),
        rotation =  mean(rotation),
        phase_A_GT =  mean(phase_A_GT),
        phase_B_GT =  mean(phase_B_GT),
        phase_C_GT =  mean(phase_C_GT),
        phase_D_GT =  mean(phase_D_GT),
        phase_E_GT =  mean(phase_E_GT),
        phase_F_GT =  mean(phase_F_GT),
        phase_G_GT =  mean(phase_G_GT),
        phase_H_GT =  mean(phase_H_GT),
        phase_I_GT =  mean(phase_I_GT)
      )
      if(sum(df_final_ds_9_2$phase_A_GT) == 0){df_final_ds_9_2$phase_A_GT <- NULL}
      if(sum(df_final_ds_9_2$phase_B_GT) == 0){df_final_ds_9_2$phase_B_GT <- NULL}
      if(sum(df_final_ds_9_2$phase_C_GT) == 0){df_final_ds_9_2$phase_C_GT <- NULL}
      if(sum(df_final_ds_9_2$phase_D_GT) == 0){df_final_ds_9_2$phase_D_GT <- NULL}
      if(sum(df_final_ds_9_2$phase_E_GT) == 0){df_final_ds_9_2$phase_E_GT <- NULL}
      if(sum(df_final_ds_9_2$phase_F_GT) == 0){df_final_ds_9_2$phase_F_GT <- NULL}
      if(sum(df_final_ds_9_2$phase_G_GT) == 0){df_final_ds_9_2$phase_G_GT <- NULL}
      if(sum(df_final_ds_9_2$phase_H_GT) == 0){df_final_ds_9_2$phase_H_GT <- NULL}
      if(sum(df_final_ds_9_2$phase_I_GT) == 0){df_final_ds_9_2$phase_I_GT <- NULL}
      number_of_phases_2 <- ncol(df_final_ds_9_2) - 5
      }
    }
    print("hi")
    output$Plot9_2 <- renderPlot({g_file_9_2})
  })
  output$downloadData9 <- downloadHandler(
    
    filename = function() { 
      paste("Traveltime_VR_", Sys.time(), ".csv", sep="")
    },
    content = function(file99) { 
      progress <- shiny::Progress$new()
      # Make sure it closes when we exit this reactive, even if there's an error
      on.exit(progress$close())
      
      progress$set(message = "Analyizing the data....", value = 0)
      
      Dates1 <- as.list(input$date9_1_multiple) 
      Dates2 <- as.list(input$date9_2_multiple) 
      lower_time <- paste0(hms::as_hms(input$file9_2_lower_time)) 
      upper_time <- paste0(hms::as_hms(input$file9_2_upper_time)) 
      input_file <- paste0(input$file9_2_ritis_travel_time_file$datapath)
      TMC_file <- paste0(input$file9_2_ritis_tmc_identification$datapath)
      TMC_lists <- strsplit(input$file9_2_TMClist, "\n")
      TMC_lists <- as.list(unlist(TMC_lists))
      direction <- paste0(input$file_9_2_direction)
      site <- as.integer(input$file9_2_site_ID)
      file_variation_log <- paste0(input$file9_2_SCATS_log$datapath)
      var1 <- as.integer(input$file9_2_var1)
      var2 <- as.integer(input$file9_2_var2)
      df <- readr::read_csv(input_file)
      df <- df %>% dplyr::filter(tmc_code %in% TMC_lists)
      df$Date <- as.Date(substr(df$measurement_tstamp,1,10))
      df$time <- as.integer(hms::as_hms(substr(df$measurement_tstamp,11,19)))
      variation_routin_display <- function(site, file_location, var_on, var_off, date, lower_time, upper_time){
        df <- readr::read_csv(file_location)
        df <- df %>% dplyr::filter(Site == site)
        df <- df %>% dplyr::filter(User == "VR")
        df$Date <- format(as.Date(df$Date,'%d/%m/%Y'), '%m/%d/%Y')
        df <- df %>% dplyr::filter(Date == format(as.Date(date), '%m/%d/%Y'))
        df = df[!duplicated(df$Time,fromLast=T),]
        df1 <- df %>% dplyr::filter(grepl(paste0(" ",as.character(var_on),": I="),Details))
        df1$var <- 1
        df2 <- df %>% dplyr::filter(grepl(paste0(" ",as.character(var_off),": I="),Details))
        df2$var <- 0
        df <- rbind(df1,df2)
        df <- df[order(df$Time),]
        
        df$duration <- NULL
        if(nrow(df) != 0){
          if((nrow(df) %% 2 == 0) == TRUE && (df$var[nrow(df)] == 0)){
            for (i in (1:(nrow(df)-1))){
              print(i)
              if((df$var[i]==1)==TRUE && (df$var[i+1]==0) ==TRUE){
                df$duration[i] <- df$Time[i+1]-df$Time[i]
              } else if ((df$var[i]==1) == TRUE && (df$var[i+1]==1) == TRUE ){
                df$duration[i] <- 0
              } else if ((df$var[i]==0) == TRUE && (df$var[i+1]==1) == TRUE){
                df$duration[i] <- 0
              }
              df$duration[nrow(df)] <- 0
            }
          }
          if((nrow(df) %% 2 == 0) == FALSE && df$var[nrow(df)] == 0){
            for (i in (1:(nrow(df)-1))){
              print(i)
              if((df$var[i]==1) && (df$var[i+1]==0)){
                df$duration[i] <- df$Time[i+1]-df$Time[i]
              } else if ((df$var[i]==1) == TRUE && (df$var[i+1]==1)){
                df$duration[i] <- 0
              } else if ((df$var[i]==0) && (df$var[i+1]==1)){
                df$duration[i] <- 0
              }
              df$duration[nrow(df)] <- 0
            }
          }
          if((nrow(df) %% 2 == 0) == FALSE && df$var[nrow(df)] == 1){
            for (i in (1:(nrow(df)-1))){
              print(i)
              if((df$var[i]==1) && (df$var[i+1]==0)){
                df$duration[i] <- df$Time[i+1]-df$Time[i]
              } else if ((df$var[i]==1) == TRUE && (df$var[i+1]==1)){
                df$duration[i] <- 0
              } else if ((df$var[i]==0) && (df$var[i+1]==1)){
                df$duration[i] <- 0
              }
              df$duration[nrow(df)] <- hms::as_hms(11:59:59) - df$Time[nrow(df)]  
            }
          }
          if((nrow(df) %% 2 == 0) == TRUE && df$var[nrow(df)] == 1){
            for (i in (1:(nrow(df)-1))){
              print(i)
              if((df$var[i]==1) && (df$var[i+1]==0)){
                df$duration[i] <- df$Time[i+1]-df$Time[i]
              } else if ((df$var[i]==1) == TRUE && (df$var[i+1]==1)){
                df$duration[i] <- 0
              } else if ((df$var[i]==0) && (df$var[i+1]==1)){
                df$duration[i] <- 0
              }
              df$duration[nrow(df)] <- hms::as_hms(11:59:59) - df$Time[nrow(df)]  
            }
          }
          
          df <- df %>% dplyr::filter(duration != 0)
          df$Time2 <- hms::as_hms(as.integer(df$Time) + df$duration)
        }
        df_time <- as.data.frame(seq(1,60*24, by =1))
        colnames(df_time) <- c("time")
        df_time <- df_time*60
        df_time$flag <- 0
        if(nrow(df) != 0){
          for(k in (1:nrow(df_time))){
            for ( l in (1:nrow(df))){
              if(df_time$flag[k] == 1){
                df_time$flag[k] <- 1
              } else if(df_time$time[k] >= as.integer(df$Time[l]) && df_time$time[k] <= as.integer(df$Time2[l])){
                df_time$flag[k] <- 1
              }
            }
          }
        }
        lower_time_int <- as.integer(hms::as_hms(lower_time))
        upper_time_int <- as.integer(hms::as_hms(upper_time))
        df_time <- df_time[df_time$time >= as.integer(hms::as_hms(lower_time)),]
        df_time <- df_time[df_time$time <= as.integer(hms::as_hms(upper_time)),]
        return(df_time)
      }
      df_variation_date1 <- variation_routin_display(site = site , file_location = file_variation_log, var_on = var1, var_off = var2, date = Dates1[[1]], lower_time = lower_time , upper_time = upper_time)
      df_variation_date2 <- variation_routin_display(site = site , file_location = file_variation_log, var_on = var1, var_off = var2, date = Dates2[[1]], lower_time = lower_time , upper_time = upper_time)
      TMC_df <- readr::read_csv(TMC_file)
      TMC_df <- TMC_df %>% dplyr::filter(tmc %in% TMC_lists)
      TMC_length <- round(sum(TMC_df$miles),2)
      df1 <- df %>% dplyr::filter(Date == Dates1)
      df1 <- df1 %>% dplyr::filter(time >= as.integer(hms::as_hms(lower_time)))
      df1 <- df1 %>% dplyr::filter(time <= as.integer(hms::as_hms(upper_time)))
      results1 <- df1 %>% group_by(Date, time) %>%
        summarise(Travel_time_mts  = sum(travel_time_seconds/60))
      df2 <- df %>% dplyr::filter(Date == Dates2)
      df2 <- df2 %>% dplyr::filter(time >= as.integer(hms::as_hms(lower_time)))
      df2 <- df2 %>% dplyr::filter(time <= as.integer(hms::as_hms(upper_time)))
      results2 <- df2 %>% group_by(Date, time) %>%
        summarise(Travel_time_mts  = sum(travel_time_seconds/60))
      results1_time <- as.data.frame(seq(1,60*24, by =1))
      colnames(results1_time) <- c("time")
      results1_time$time <- results1_time$time*60
      results1_time <- results1_time %>% dplyr::filter(time >= as.integer(hms::as_hms(lower_time)))
      results1_time <- results1_time %>% dplyr::filter(time <= as.integer(hms::as_hms(upper_time)))
      results1_time <- sqldf::sqldf("select a.time, b.Date, b.Travel_time_mts from results1_time as a
                              left join results1 as b
                              on a.time = b.time", drv = "SQLite")
      results1_time <- tidyr::fill(results1_time, c("Date", "Travel_time_mts"), .direction = "downup")
      results2_time <- as.data.frame(seq(1,60*24, by =1))
      colnames(results2_time) <- c("time")
      results2_time$time <- results2_time$time*60
      results2_time <- results2_time %>% dplyr::filter(time >= as.integer(hms::as_hms(lower_time)))
      results2_time <- results2_time %>% dplyr::filter(time <= as.integer(hms::as_hms(upper_time)))
      results2_time <- sqldf::sqldf("select a.time, b.Date, b.Travel_time_mts from results2_time as a
                              left join results2 as b
                              on a.time = b.time", drv = "SQLite")
      results2_time <- tidyr::fill(results2_time, c("Date", "Travel_time_mts"), .direction = "downup")
      date1_var_utilization <- as.integer(round(sum(df_variation_date1$flag)/nrow(df_variation_date1)*100,0))
      date2_var_utilization <- as.integer(round(sum(df_variation_date2$flag)/nrow(df_variation_date2)*100,0))
      lower_time_int <- as.integer(hms::as_hms(lower_time))
      upper_time_int <- as.integer(hms::as_hms(upper_time))
      name11 <- paste0("Group1  ", round(mean(results1_time$Travel_time_mts),2)," mts")
      name22 <- paste0("Group2  ", round(mean(results2_time$Travel_time_mts),2)," mts")
      name33 <- paste0("VR Utilizing Group1  ", date1_var_utilization," %")
      name44 <- paste0("VR Utilizing Group2  ", date2_var_utilization," %")
      name1 <- paste0("Avg Group1")
      name2 <- paste0("Avg Group2")
      xdata <- hms::as_hms(unique(results1_time$time))
      y1 <- results1_time$Travel_time_mts
      y2 <- results2_time$Travel_time_mts
      y3 <- df_variation_date1$flag
      y4 <- df_variation_date2$flag
      y11 <- rep(mean(results1_time$Travel_time_mts),each = nrow(results1_time))
      y22 <- rep(mean(results2_time$Travel_time_mts),each = nrow(results2_time))
      df_plot9_2 <- data.frame(xdata, y1, y2, y3, y4,y11,y22)
      colnames(df_plot9_2) <- c("time",paste0("Travel Time ",Dates1[[1]]," (mts)"),paste0("Travel Time ",Dates2[[1]]," (mts)"),paste0("VR Activation ",Dates1[[1]]),paste0("VR Activation ",Dates2[[1]]),paste0("Average Travel Time ",Dates1[[1]]," (mts)"),paste0("Average Travel Time ",Dates2[[1]]," (mts)"))
      write.csv(df_plot9_2, file99, row.names=FALSE)
    })
  
  observeEvent(input$previewData11, {
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = "Creating a Plot....", value = 0)  
    lower_time <- paste0(hms::as_hms(input$file11_2_lower_time)) 
    upper_time <- paste0(hms::as_hms(input$file11_2_upper_time)) 
    input_file_event <- paste0(input$file11_2_event_file$datapath)
    site <- as.integer(input$file11_2_site_ID)
    
    MX_Value_extraction <- function(input_file_event){
      df <- readr::read_csv(input_file_event)
      df$date <- as.Date(substr(df$Time,1,10))
      df$start_time <- as.integer(hms::as_hms(substr(df$Time,12,19)))
      df$day <- weekdays(df$date)
      df <- df[substr(df$Event,1,36)== "Phase termination: Terminated phase=",]
      df$start_time2 <- as.integer(df$start_time/60)*60
      pattern <- "MX=\\s*(.*?)\\s*,"
      df$MXValue <- as.integer(gsub(".*MX=(.*)\\, GT.*", "\\1", df$Event))
      df_A <- df[substr(df$Event,1,37)== "Phase termination: Terminated phase=A",]
      df_B <- df[substr(df$Event,1,37)== "Phase termination: Terminated phase=B",]
      df_C <- df[substr(df$Event,1,37)== "Phase termination: Terminated phase=C",]
      df_D <- df[substr(df$Event,1,37)== "Phase termination: Terminated phase=D",]
      df_E <- df[substr(df$Event,1,37)== "Phase termination: Terminated phase=E",]
      df_F <- df[substr(df$Event,1,37)== "Phase termination: Terminated phase=F",]
      df_G <- df[substr(df$Event,1,37)== "Phase termination: Terminated phase=G",]
      df_H <- df[substr(df$Event,1,37)== "Phase termination: Terminated phase=H",]
      df_I <- df[substr(df$Event,1,37)== "Phase termination: Terminated phase=I",]
      phases = 0
      if(nrow(df_A) > 0){
        phases = phases + 1
      }
      if(nrow(df_B) > 0){
        phases = phases + 1
      }
      if(nrow(df_C) > 0){
        phases = phases + 1
      }
      if(nrow(df_D) > 0){
        phases = phases + 1
      }
      if(nrow(df_E) > 0){
        phases = phases + 1
      }
      if(nrow(df_F) > 0){
        phases = phases + 1
      }
      if(nrow(df_G) > 0){
        phases = phases + 1
      }
      if(nrow(df_H) > 0){
        phases = phases + 1
      }
      if(nrow(df_I) > 0){
        phases = phases + 1
      }
      time_df <- data.frame(seq(60,60*60*24,by=60))
      colnames(time_df) <- c("time")
      df_A <- dplyr::left_join(time_df, df_A, by = c("time" = "start_time2")) %>% group_by(time) %>% dplyr::summarize(MXValue = mean(MXValue, na.rm=TRUE))
      df_B <- dplyr::left_join(time_df, df_B, by = c("time" = "start_time2")) %>% group_by(time) %>% dplyr::summarize(MXValue = mean(MXValue, na.rm=TRUE))
      df_C <- dplyr::left_join(time_df, df_C, by = c("time" = "start_time2")) %>% group_by(time) %>% dplyr::summarize(MXValue = mean(MXValue, na.rm=TRUE))
      df_D <- dplyr::left_join(time_df, df_D, by = c("time" = "start_time2")) %>% group_by(time) %>% dplyr::summarize(MXValue = mean(MXValue, na.rm=TRUE))
      df_E <- dplyr::left_join(time_df, df_E, by = c("time" = "start_time2")) %>% group_by(time) %>% dplyr::summarize(MXValue = mean(MXValue, na.rm=TRUE))
      df_F <- dplyr::left_join(time_df, df_F, by = c("time" = "start_time2")) %>% group_by(time) %>% dplyr::summarize(MXValue = mean(MXValue, na.rm=TRUE))
      df_G <- dplyr::left_join(time_df, df_G, by = c("time" = "start_time2")) %>% group_by(time) %>% dplyr::summarize(MXValue = mean(MXValue, na.rm=TRUE))
      df_H <- dplyr::left_join(time_df, df_H, by = c("time" = "start_time2")) %>% group_by(time) %>% dplyr::summarize(MXValue = mean(MXValue, na.rm=TRUE))
      df_I <- dplyr::left_join(time_df, df_I, by = c("time" = "start_time2")) %>% group_by(time) %>% dplyr::summarize(MXValue = mean(MXValue, na.rm=TRUE))
      df_A <- df_A %>% tidyr::fill(time)
      df_A <- df_A %>% tidyr::fill(time, .direction ="downup")
      df_A$MXValue[is.na(df_A$MXValue)] <- 0
      df_B <- df_B %>% tidyr::fill(time)
      df_B <- df_B %>% tidyr::fill(time, .direction ="downup")
      df_B$MXValue[is.na(df_B$MXValue)] <- 0
      df_C <- df_C %>% tidyr::fill(time)
      df_C <- df_C %>% tidyr::fill(time, .direction ="downup")
      df_C$MXValue[is.na(df_C$MXValue)] <- 0
      df_D <- df_D %>% tidyr::fill(time)
      df_D <- df_D %>% tidyr::fill(time, .direction ="downup")
      df_D$MXValue[is.na(df_D$MXValue)] <- 0
      df_E <- df_E %>% tidyr::fill(time)
      df_E <- df_E %>% tidyr::fill(time, .direction ="downup")
      df_E$MXValue[is.na(df_E$MXValue)] <- 0
      df_F <- df_F %>% tidyr::fill(time)
      df_F <- df_F %>% tidyr::fill(time, .direction ="downup")
      df_F$MXValue[is.na(df_F$MXValue)] <- 0
      df_G <- df_G %>% tidyr::fill(time)
      df_G <- df_G %>% tidyr::fill(time, .direction ="downup")
      df_G$MXValue[is.na(df_G$MXValue)] <- 0
      df_H <- df_H %>% tidyr::fill(time)
      df_H <- df_H %>% tidyr::fill(time, .direction ="downup")
      df_H$MXValue[is.na(df_H$MXValue)] <- 0
      df_I <- df_I %>% tidyr::fill(time)
      df_I <- df_I %>% tidyr::fill(time, .direction ="downup")
      df_I$MXValue[is.na(df_I$MXValue)] <- 0
      df_final_results <- NULL
      df_final_results$time <- as.integer(df_A$time)
      df_final_results <- as.data.frame(df_final_results)
      df_final_results$MXValue_A <- df_A$MXValue
      if(is.na(df_B$time) != TRUE){
        df_final_results$MXValue_B <- df_B$MXValue
      }
      if(is.na(df_C$time) != TRUE){
        df_final_results$MXValue_C <- df_C$MXValue
      }
      if(is.na(df_D$time) != TRUE){
        df_final_results$MXValue_D <- df_D$MXValue
      }
      if(is.na(df_E$time) != TRUE){
        df_final_results$MXValue_E <- df_E$MXValue
      }
      if(is.na(df_F$time) != TRUE){
        df_final_results$MXValue_F <- df_F$MXValue
      }
      if(is.na(df_G$time) != TRUE){
        df_final_results$MXValue_G <- df_G$MXValue
      }
      if(is.na(df_H$time) != TRUE){
        df_final_results$MXValue_H <- df_H$MXValue
      }
      if(is.na(df_I$time) != TRUE){
        df_final_results$MXValue_I <- df_I$MXValue
      }
      return(df_final_results)
    }
    
    Date_file11 <- as.Date(substr(readr::read_csv(input_file_event)$Time[1],1,10))
    
    df_11_MX_Value_extraction <- MX_Value_extraction(input_file_event = input_file_event)
    
    number_phases_11 <- ncol(df_11_MX_Value_extraction)-1
    
    df_11_MX_Value_extraction <- df_11_MX_Value_extraction %>% dplyr::filter(time >= as.integer(hms::as_hms(lower_time)))
    df_11_MX_Value_extraction <- df_11_MX_Value_extraction %>% dplyr::filter(time <= as.integer(hms::as_hms(upper_time)))
    
    lower_time_int <- as.integer(hms::as_hms(lower_time))
    upper_time_int <- as.integer(hms::as_hms(upper_time))
    
    xdata <- hms::as_hms(unique(df_11_MX_Value_extraction$time))
    if(input$file_11_2_phase_selected == "A"){
      y1 <- df_11_MX_Value_extraction$MXValue_A
      y11 <- rep(mean(df_11_MX_Value_extraction$MXValue_A),each = nrow(df_11_MX_Value_extraction))
      name11 <- paste0("Avg MX Value ", round(mean(df_11_MX_Value_extraction$MXValue_A),2)," sec")
      max_y_value_11 <- max(df_11_MX_Value_extraction$MXValue_A)
      min_y_value_11 <- min(df_11_MX_Value_extraction$MXValue_A)
    }
    if(input$file_11_2_phase_selected == "B"){
      y1 <- df_11_MX_Value_extraction$MXValue_B
      y11 <- rep(mean(df_11_MX_Value_extraction$MXValue_B),each = nrow(df_11_MX_Value_extraction))
      name11 <- paste0("Avg MX Value ", round(mean(df_11_MX_Value_extraction$MXValue_B),2)," sec")
      max_y_value_11 <- max(df_11_MX_Value_extraction$MXValue_B)
      min_y_value_11 <- min(df_11_MX_Value_extraction$MXValue_B)
    }
    if(input$file_11_2_phase_selected == "C"){
      y1 <- df_11_MX_Value_extraction$MXValue_C
      y11 <- rep(mean(df_11_MX_Value_extraction$MXValue_C),each = nrow(df_11_MX_Value_extraction))
      name11 <- paste0("Avg MX Value ", round(mean(df_11_MX_Value_extraction$MXValue_C),2)," sec")
      max_y_value_11 <- max(df_11_MX_Value_extraction$MXValue_C)
      min_y_value_11 <- min(df_11_MX_Value_extraction$MXValue_C)
    }
    if(input$file_11_2_phase_selected == "D"){
      y1 <- df_11_MX_Value_extraction$MXValue_D
      y11 <- rep(mean(df_11_MX_Value_extraction$MXValue_D),each = nrow(df_11_MX_Value_extraction))
      name11 <- paste0("Avg MX Value ", round(mean(df_11_MX_Value_extraction$MXValue_D),2)," sec")
      max_y_value_11 <- max(df_11_MX_Value_extraction$MXValue_D)
      min_y_value_11 <- min(df_11_MX_Value_extraction$MXValue_D)
    }
    if(input$file_11_2_phase_selected == "E"){
      y1 <- df_11_MX_Value_extraction$MXValue_E
      y11 <- rep(mean(df_11_MX_Value_extraction$MXValue_E),each = nrow(df_11_MX_Value_extraction))
      name11 <- paste0("Avg MX Value ", round(mean(df_11_MX_Value_extraction$MXValue_E),2)," sec")
      max_y_value_11 <- max(df_11_MX_Value_extraction$MXValue_E)
      min_y_value_11 <- min(df_11_MX_Value_extraction$MXValue_E)
    }
    if(input$file_11_2_phase_selected == "F"){
      y1 <- df_11_MX_Value_extraction$MXValue_F
      y11 <- rep(mean(df_11_MX_Value_extraction$MXValue_F),each = nrow(df_11_MX_Value_extraction))
      name11 <- paste0("Avg MX Value ", round(mean(df_11_MX_Value_extraction$MXValue_F),2)," sec")
      max_y_value_11 <- max(df_11_MX_Value_extraction$MXValue_F)
      min_y_value_11 <- min(df_11_MX_Value_extraction$MXValue_F)
    }
    if(input$file_11_2_phase_selected == "G"){
      y1 <- df_11_MX_Value_extraction$MXValue_G
      y11 <- rep(mean(df_11_MX_Value_extraction$MXValue_G),each = nrow(df_11_MX_Value_extraction))
      name11 <- paste0("Avg MX Value ", round(mean(df_11_MX_Value_extraction$MXValue_G),2)," sec")
      max_y_value_11 <- max(df_11_MX_Value_extraction$MXValue_G)
      min_y_value_11 <- min(df_11_MX_Value_extraction$MXValue_G)
    }
    if(input$file_11_2_phase_selected == "H"){
      y1 <- df_11_MX_Value_extraction$MXValue_H
      y11 <- rep(mean(df_11_MX_Value_extraction$MXValue_H),each = nrow(df_11_MX_Value_extraction))
      name11 <- paste0("Avg MX Value ", round(mean(df_11_MX_Value_extraction$MXValue_H),2)," sec")
      max_y_value_11 <- max(df_11_MX_Value_extraction$MXValue_H)
      min_y_value_11 <- min(df_11_MX_Value_extraction$MXValue_H)
    }
    if(input$file_11_2_phase_selected == "I"){
      y1 <- df_11_MX_Value_extraction$MXValue_I
      y11 <- rep(mean(df_11_MX_Value_extraction$MXValue_I),each = nrow(df_11_MX_Value_extraction))
      name11 <- paste0("Avg MX Value Phase I ", round(mean(df_11_MX_Value_extraction$MXValue_I),2)," sec")
      max_y_value_11 <- max(df_11_MX_Value_extraction$MXValue_I)
      min_y_value_11 <- min(df_11_MX_Value_extraction$MXValue_I)
    }
    df_plot <- data.frame(xdata, y1, y11)
    g_file_11_1 <- ggplot(df_plot, aes(xdata))
    g_file_11_1 <- g_file_11_1 + geom_line(aes(y=y1, color = "MX value (Sec)"))
    g_file_11_1 <- g_file_11_1 + geom_line(aes(y=y11, color = name11))
    g_file_11_1 <- g_file_11_1 + theme(plot.title = element_text(color="black", size=10, face="bold.italic"),
                                     axis.text=element_text(size=8),
                                     axis.title=element_text(size=9,face="bold"),
                                     legend.text=element_text(size=9),
                                     legend.justification=c(1,1),legend.position=c(1,1),legend.title=element_blank())
    g_file_11_1 <- g_file_11_1 + scale_color_manual(name = " ", values = c("red","darkblue"))
    g_file_11_1 <- g_file_11_1 + ylab(paste0("MX Value (sec)")) + xlab("Time")
    g_file_11_1 <- g_file_11_1 + scale_x_continuous(labels = hms::as_hms(seq(lower_time_int, upper_time_int,by=3600)), breaks=seq(lower_time_int, upper_time_int,by=3600))
    g_file_11_1 <- g_file_11_1 + ylim(min_y_value_11, max_y_value_11)
    g_file_11_1 <- g_file_11_1 + ggtitle(paste0("MX Value for ",site," Phase ",input$file_11_2_phase_selected," on ",Date_file11))
    g_file_11_1 <- g_file_11_1 + theme_calc()
    output$Plot11_1 <- renderPlot({g_file_11_1})
  })
  output$downloadData11_1 <- downloadHandler(
    
    filename = function() { 
      paste("MX_Value_Phase", Sys.time(), ".csv", sep="")
    },
    content = function(file11_1) { 
      progress <- shiny::Progress$new()
      # Make sure it closes when we exit this reactive, even if there's an error
      on.exit(progress$close())
      progress$set(message = "Analyizing the data....", value = 0)
      lower_time <- paste0(hms::as_hms(input$file11_2_lower_time)) 
      upper_time <- paste0(hms::as_hms(input$file11_2_upper_time)) 
      input_file_event <- paste0(input$file11_2_event_file$datapath)
      site <- as.integer(input$file11_2_site_ID)
      
      MX_Value_extraction <- function(input_file_event){
        df <- readr::read_csv(input_file_event)
        df$date <- as.Date(substr(df$Time,1,10))
        df$start_time <- as.integer(hms::as_hms(substr(df$Time,12,19)))
        df$day <- weekdays(df$date)
        df <- df[substr(df$Event,1,36)== "Phase termination: Terminated phase=",]
        df$start_time2 <- as.integer(df$start_time/60)*60
        pattern <- "MX=\\s*(.*?)\\s*,"
        df$MXValue <- as.integer(gsub(".*MX=(.*)\\, GT.*", "\\1", df$Event))
        df_A <- df[substr(df$Event,1,37)== "Phase termination: Terminated phase=A",]
        df_B <- df[substr(df$Event,1,37)== "Phase termination: Terminated phase=B",]
        df_C <- df[substr(df$Event,1,37)== "Phase termination: Terminated phase=C",]
        df_D <- df[substr(df$Event,1,37)== "Phase termination: Terminated phase=D",]
        df_E <- df[substr(df$Event,1,37)== "Phase termination: Terminated phase=E",]
        df_F <- df[substr(df$Event,1,37)== "Phase termination: Terminated phase=F",]
        df_G <- df[substr(df$Event,1,37)== "Phase termination: Terminated phase=G",]
        df_H <- df[substr(df$Event,1,37)== "Phase termination: Terminated phase=H",]
        df_I <- df[substr(df$Event,1,37)== "Phase termination: Terminated phase=I",]
        phases = 0
        if(nrow(df_A) > 0){
          phases = phases + 1
        }
        if(nrow(df_B) > 0){
          phases = phases + 1
        }
        if(nrow(df_C) > 0){
          phases = phases + 1
        }
        if(nrow(df_D) > 0){
          phases = phases + 1
        }
        if(nrow(df_E) > 0){
          phases = phases + 1
        }
        if(nrow(df_F) > 0){
          phases = phases + 1
        }
        if(nrow(df_G) > 0){
          phases = phases + 1
        }
        if(nrow(df_H) > 0){
          phases = phases + 1
        }
        if(nrow(df_I) > 0){
          phases = phases + 1
        }
        time_df <- data.frame(seq(60,60*60*24,by=60))
        colnames(time_df) <- c("time")
        df_A <- dplyr::left_join(time_df, df_A, by = c("time" = "start_time2")) %>% group_by(time) %>% dplyr::summarize(MXValue = mean(MXValue, na.rm=TRUE))
        df_B <- dplyr::left_join(time_df, df_B, by = c("time" = "start_time2")) %>% group_by(time) %>% dplyr::summarize(MXValue = mean(MXValue, na.rm=TRUE))
        df_C <- dplyr::left_join(time_df, df_C, by = c("time" = "start_time2")) %>% group_by(time) %>% dplyr::summarize(MXValue = mean(MXValue, na.rm=TRUE))
        df_D <- dplyr::left_join(time_df, df_D, by = c("time" = "start_time2")) %>% group_by(time) %>% dplyr::summarize(MXValue = mean(MXValue, na.rm=TRUE))
        df_E <- dplyr::left_join(time_df, df_E, by = c("time" = "start_time2")) %>% group_by(time) %>% dplyr::summarize(MXValue = mean(MXValue, na.rm=TRUE))
        df_F <- dplyr::left_join(time_df, df_F, by = c("time" = "start_time2")) %>% group_by(time) %>% dplyr::summarize(MXValue = mean(MXValue, na.rm=TRUE))
        df_G <- dplyr::left_join(time_df, df_G, by = c("time" = "start_time2")) %>% group_by(time) %>% dplyr::summarize(MXValue = mean(MXValue, na.rm=TRUE))
        df_H <- dplyr::left_join(time_df, df_H, by = c("time" = "start_time2")) %>% group_by(time) %>% dplyr::summarize(MXValue = mean(MXValue, na.rm=TRUE))
        df_I <- dplyr::left_join(time_df, df_I, by = c("time" = "start_time2")) %>% group_by(time) %>% dplyr::summarize(MXValue = mean(MXValue, na.rm=TRUE))
        df_A <- df_A %>% tidyr::fill(time)
        df_A <- df_A %>% tidyr::fill(time, .direction ="downup")
        df_A$MXValue[is.na(df_A$MXValue)] <- 0
        df_B <- df_B %>% tidyr::fill(time)
        df_B <- df_B %>% tidyr::fill(time, .direction ="downup")
        df_B$MXValue[is.na(df_B$MXValue)] <- 0
        df_C <- df_C %>% tidyr::fill(time)
        df_C <- df_C %>% tidyr::fill(time, .direction ="downup")
        df_C$MXValue[is.na(df_C$MXValue)] <- 0
        df_D <- df_D %>% tidyr::fill(time)
        df_D <- df_D %>% tidyr::fill(time, .direction ="downup")
        df_D$MXValue[is.na(df_D$MXValue)] <- 0
        df_E <- df_E %>% tidyr::fill(time)
        df_E <- df_E %>% tidyr::fill(time, .direction ="downup")
        df_E$MXValue[is.na(df_E$MXValue)] <- 0
        df_F <- df_F %>% tidyr::fill(time)
        df_F <- df_F %>% tidyr::fill(time, .direction ="downup")
        df_F$MXValue[is.na(df_F$MXValue)] <- 0
        df_G <- df_G %>% tidyr::fill(time)
        df_G <- df_G %>% tidyr::fill(time, .direction ="downup")
        df_G$MXValue[is.na(df_G$MXValue)] <- 0
        df_H <- df_H %>% tidyr::fill(time)
        df_H <- df_H %>% tidyr::fill(time, .direction ="downup")
        df_H$MXValue[is.na(df_H$MXValue)] <- 0
        df_I <- df_I %>% tidyr::fill(time)
        df_I <- df_I %>% tidyr::fill(time, .direction ="downup")
        df_I$MXValue[is.na(df_I$MXValue)] <- 0
        df_final_results <- NULL
        df_final_results$time <- as.integer(df_A$time)
        df_final_results <- as.data.frame(df_final_results)
        df_final_results$MXValue_A <- df_A$MXValue
        if(is.na(df_B$time) != TRUE){
          df_final_results$MXValue_B <- df_B$MXValue
        }
        if(is.na(df_C$time) != TRUE){
          df_final_results$MXValue_C <- df_C$MXValue
        }
        if(is.na(df_D$time) != TRUE){
          df_final_results$MXValue_D <- df_D$MXValue
        }
        if(is.na(df_E$time) != TRUE){
          df_final_results$MXValue_E <- df_E$MXValue
        }
        if(is.na(df_F$time) != TRUE){
          df_final_results$MXValue_F <- df_F$MXValue
        }
        if(is.na(df_G$time) != TRUE){
          df_final_results$MXValue_G <- df_G$MXValue
        }
        if(is.na(df_H$time) != TRUE){
          df_final_results$MXValue_H <- df_H$MXValue
        }
        if(is.na(df_I$time) != TRUE){
          df_final_results$MXValue_I <- df_I$MXValue
        }
        return(df_final_results)
      }
      
      Date_file11 <- as.Date(substr(readr::read_csv(input_file_event)$Time[1],1,10))
      
      df_11_MX_Value_extraction <- MX_Value_extraction(input_file_event = input_file_event)
      
      number_phases_11 <- ncol(df_11_MX_Value_extraction)-1
      
      df_11_MX_Value_extraction <- df_11_MX_Value_extraction %>% dplyr::filter(time >= as.integer(hms::as_hms(lower_time)))
      df_11_MX_Value_extraction <- df_11_MX_Value_extraction %>% dplyr::filter(time <= as.integer(hms::as_hms(upper_time)))
      df_11_MX_Value_extraction$time <- hms::as_hms(df_11_MX_Value_extraction$time)
      write.csv(df_11_MX_Value_extraction, file11_1, row.names=FALSE)
  })
  observeEvent(input$previewData11_2, {
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = "Creating a Plot....", value = 0)  
    lower_time <- paste0(hms::as_hms(input$file11_2_lower_time)) 
    upper_time <- paste0(hms::as_hms(input$file11_2_upper_time)) 
    input_file_phase <- paste0(input$file11_2_phase_file$datapath)
    site <- as.integer(input$file11_2_site_ID)
    
    Actual_Phase_extraction <- function(input_file_phase){
      df <- readr::read_csv(input_file_phase)
      df$date <- as.Date(substr(df$Start,1,10))
      df$start_time <- as.integer(hms::as_hms(substr(df$Start,12,19)))
      df$day <- weekdays(df$date)
      df <- df[df$Phase!= "Unknown",]
      
      phases <- unique(df$Phase)
      if(df$Phase[1]!="<A>"){
        df <- df[-c(1),]
        if(df$Phase[1]!="<A>"){
          df <- df[-c(1),]
          if(df$Phase[1]!="<A>"){
            df <- df[-c(1),]
            if(df$Phase[1]!="<A>"){
              df <- df[-c(1),]
              if(df$Phase[1]!="<A>"){
                df <- df[-c(1),]
                if(df$Phase[1]!="<A>"){
                  df <- df[-c(1),]
                  if(df$Phase[1]!="<A>"){
                    df <- df[-c(1),]
                    if(df$Phase[1]!="<A>"){
                      df <- df[-c(1),]
                      if(df$Phase[1]!="<A>"){
                        df <- df[-c(1),]
                        if(df$Phase[1]!="<A>"){
                          df <- df[-c(1),]
                          if(df$Phase[1]!="<A>"){
                            df <- df[-c(1),]
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
      df$rank <- rank(df$Phase,ties.method= "first")
      df$rank[df$Phase!="<A>"] <- NA
      df$start_time2 <- df$start_time
      df$start_time2[df$Phase!="<A>"] <- NA
      df <- df %>% tidyr::fill(rank,.direction = c("down"))
      df <- df %>% tidyr::fill(start_time2,.direction = c("down"))
      df <- data.table::as.data.table(df)
      df <- df[, actual_cycle_length := sum(`Duration (s)`), by = "rank"][]
      rank_df <- df %>% dplyr::select("rank","day","date","start_time2","actual_cycle_length")
      rank_df <- data.frame(unique(rank_df))
      
      phases_df <- data.frame(phases)
      occur_df <- data.frame(1:max(df$rank))
      colnames(occur_df) <- c("rank")
      df1 <- merge(phases_df, occur_df,by=NULL)
      df2 <- sqldf::sqldf("select a.phases, a.rank, b.`Duration (s)`, b.Start, b.End, b.Gapped, c.date, b.start_time, c.day, c.start_time2, c.actual_cycle_length
                    from df1 as a
                    left join df as b
                    on a.rank = b.rank
                    and a.phases = b.Phase
                    left join rank_df as c
                    on a.rank = c.rank", drv = "SQLite")
      df2$`Duration (s)`[is.na(df2$`Duration (s)`)] <- 0
      df2$split_percentage <- df2$`Duration (s)`/df2$actual_cycle_length
      df2$start_time3 <- as.integer(df2$start_time2/60)*60
      
      
      df3 <- sqldf::sqldf("select a.start_time3, a.phases, a.day, a.date, avg(a.split_percentage) as split_percentage, avg(a.actual_cycle_length) as actual_cycle_length
                    from df2 as a
                    group by a.start_time3, a.phases, a.day, a.date", drv = "SQLite")
      
      time_df <- data.frame(seq(60,60*60*24,by=60))
      colnames(time_df) <- c("time")
      time_df2 <- merge(phases_df, time_df,by=NULL)
      
      df4 <- sqldf::sqldf("select b.time, a.start_time3, b.phases, a.day, a.date, a.split_percentage, a.actual_cycle_length
                    from time_df2 as b
                    left join df3 as a
                    on b.time = a.start_time3
                    and b.phases = a.phases", drv = "SQLite")
      df4 <- df4 %>% tidyr::fill(start_time3,.direction = c("downup"))
      # df4 <- df4 %>% tidyr::fill(Site,.direction = c("downup"))
      df4 <- df4 %>% tidyr::fill(day,.direction = c("downup"))
      df4 <- df4 %>% tidyr::fill(date,.direction = c("downup"))
      df4 <- df4 %>% tidyr::fill(actual_cycle_length,.direction = c("downup"))
      
      df5 <-  df4[with(df4, order(phases, time)),]
      df5 <- df5 %>% tidyr::fill(split_percentage,.direction ="downup")
      df5 <-  df5[with(df5, order(time)),]
      rownames(df5) <- 1:nrow(df5)
      df_final_results <- NULL
      if(length(phases)==2){
        df_final_results_A <- df5 %>% dplyr::filter(phases == "<A>")
        df_final_results_B <- df5 %>% dplyr::filter(phases == "B")
        df_final_results$time <- df_final_results_A$time
        df_final_results$split_percentage_A <- df_final_results_A$split_percentage*100
        df_final_results$split_percentage_B <- df_final_results_B$split_percentage*100
        df_final_results <- as.data.frame(df_final_results)
      }
      if(length(phases)==3){
        df_final_results_A <- df5 %>% dplyr::filter(phases == "<A>")
        df_final_results_B <- df5 %>% dplyr::filter(phases == "B")
        df_final_results_C <- df5 %>% dplyr::filter(phases == "C")
        df_final_results$time <- df_final_results_A$time
        df_final_results$split_percentage_A <- df_final_results_A$split_percentage*100
        df_final_results$split_percentage_B <- df_final_results_B$split_percentage*100
        df_final_results$split_percentage_C <- df_final_results_C$split_percentage*100
        df_final_results <- as.data.frame(df_final_results)
      }
      if(length(phases)==4){
        df_final_results_A <- df5 %>% dplyr::filter(phases == "<A>")
        df_final_results_B <- df5 %>% dplyr::filter(phases == "B")
        df_final_results_C <- df5 %>% dplyr::filter(phases == "C")
        df_final_results_D <- df5 %>% dplyr::filter(phases == "D")
        df_final_results$time <- df_final_results_A$time
        df_final_results$split_percentage_A <- df_final_results_A$split_percentage*100
        df_final_results$split_percentage_B <- df_final_results_B$split_percentage*100
        df_final_results$split_percentage_C <- df_final_results_C$split_percentage*100
        df_final_results$split_percentage_D <- df_final_results_D$split_percentage*100
        df_final_results <- as.data.frame(df_final_results)
      }
      if(length(phases)==5){
        df_final_results_A <- df5 %>% dplyr::filter(phases == "<A>")
        df_final_results_B <- df5 %>% dplyr::filter(phases == "B")
        df_final_results_C <- df5 %>% dplyr::filter(phases == "C")
        df_final_results_D <- df5 %>% dplyr::filter(phases == "D")
        df_final_results_E <- df5 %>% dplyr::filter(phases == "E")
        df_final_results$time <- df_final_results_A$time
        df_final_results$split_percentage_A <- df_final_results_A$split_percentage*100
        df_final_results$split_percentage_B <- df_final_results_B$split_percentage*100
        df_final_results$split_percentage_C <- df_final_results_C$split_percentage*100
        df_final_results$split_percentage_D <- df_final_results_D$split_percentage*100
        df_final_results$split_percentage_E <- df_final_results_E$split_percentage*100
        df_final_results <- as.data.frame(df_final_results)
      }
      if(length(phases)==6){
        df_final_results_A <- df5 %>% dplyr::filter(phases == "<A>")
        df_final_results_B <- df5 %>% dplyr::filter(phases == "B")
        df_final_results_C <- df5 %>% dplyr::filter(phases == "C")
        df_final_results_D <- df5 %>% dplyr::filter(phases == "D")
        df_final_results_E <- df5 %>% dplyr::filter(phases == "E")
        df_final_results_F <- df5 %>% dplyr::filter(phases == "F")
        df_final_results$time <- df_final_results_A$time
        df_final_results$split_percentage_A <- df_final_results_A$split_percentage*100
        df_final_results$split_percentage_B <- df_final_results_B$split_percentage*100
        df_final_results$split_percentage_C <- df_final_results_C$split_percentage*100
        df_final_results$split_percentage_D <- df_final_results_D$split_percentage*100
        df_final_results$split_percentage_E <- df_final_results_E$split_percentage*100
        df_final_results$split_percentage_F <- df_final_results_F$split_percentage*100
        df_final_results <- as.data.frame(df_final_results)
      }
      if(length(phases)==7){
        df_final_results_A <- df5 %>% dplyr::filter(phases == "<A>")
        df_final_results_B <- df5 %>% dplyr::filter(phases == "B")
        df_final_results_C <- df5 %>% dplyr::filter(phases == "C")
        df_final_results_D <- df5 %>% dplyr::filter(phases == "D")
        df_final_results_E <- df5 %>% dplyr::filter(phases == "E")
        df_final_results_F <- df5 %>% dplyr::filter(phases == "F")
        df_final_results_G <- df5 %>% dplyr::filter(phases == "G")
        df_final_results$time <- df_final_results_A$time
        df_final_results$split_percentage_A <- df_final_results_A$split_percentage*100
        df_final_results$split_percentage_B <- df_final_results_B$split_percentage*100
        df_final_results$split_percentage_C <- df_final_results_C$split_percentage*100
        df_final_results$split_percentage_D <- df_final_results_D$split_percentage*100
        df_final_results$split_percentage_E <- df_final_results_E$split_percentage*100
        df_final_results$split_percentage_F <- df_final_results_F$split_percentage*100
        df_final_results$split_percentage_F <- df_final_results_G$split_percentage*100
        df_final_results <- as.data.frame(df_final_results)
      }
      if(length(phases)==8){
        df_final_results_A <- df5 %>% dplyr::filter(phases == "<A>")
        df_final_results_B <- df5 %>% dplyr::filter(phases == "B")
        df_final_results_C <- df5 %>% dplyr::filter(phases == "C")
        df_final_results_D <- df5 %>% dplyr::filter(phases == "D")
        df_final_results_E <- df5 %>% dplyr::filter(phases == "E")
        df_final_results_F <- df5 %>% dplyr::filter(phases == "F")
        df_final_results_G <- df5 %>% dplyr::filter(phases == "G")
        df_final_results_H <- df5 %>% dplyr::filter(phases == "H")
        df_final_results$time <- df_final_results_A$time
        df_final_results$split_percentage_A <- df_final_results_A$split_percentage*100
        df_final_results$split_percentage_B <- df_final_results_B$split_percentage*100
        df_final_results$split_percentage_C <- df_final_results_C$split_percentage*100
        df_final_results$split_percentage_D <- df_final_results_D$split_percentage*100
        df_final_results$split_percentage_E <- df_final_results_E$split_percentage*100
        df_final_results$split_percentage_F <- df_final_results_F$split_percentage*100
        df_final_results$split_percentage_F <- df_final_results_G$split_percentage*100
        df_final_results$split_percentage_H <- df_final_results_H$split_percentage*100
        df_final_results <- as.data.frame(df_final_results)
      }
      return(df_final_results)
    }
    Date_file11_2 <- as.Date(substr(readr::read_csv(input_file_phase)$Start[1],1,10))
    
    df_11_Actual_Phase_extraction <- Actual_Phase_extraction(input_file_phase = input_file_phase)
    
    number_phases_11 <- ncol(df_11_Actual_Phase_extraction)-1
    
    df_11_Actual_Phase_extraction <- df_11_Actual_Phase_extraction %>% dplyr::filter(time >= as.integer(hms::as_hms(lower_time)))
    df_11_Actual_Phase_extraction <- df_11_Actual_Phase_extraction %>% dplyr::filter(time <= as.integer(hms::as_hms(upper_time)))
    
    lower_time_int <- as.integer(hms::as_hms(lower_time))
    upper_time_int <- as.integer(hms::as_hms(upper_time))
    
    xdata_2 <- hms::as_hms(unique(df_11_Actual_Phase_extraction$time))
    if(number_phases_11 == 2){
      y1_2 <- df_11_Actual_Phase_extraction$split_percentage_A
      y2_2 <- df_11_Actual_Phase_extraction$split_percentage_B
      y11_2 <- rep(mean(df_11_Actual_Phase_extraction$split_percentage_A),each = nrow(df_11_Actual_Phase_extraction))
      y22_2 <- rep(mean(df_11_Actual_Phase_extraction$split_percentage_B),each = nrow(df_11_Actual_Phase_extraction))
      name11_2 <- paste0("Avg Phase A ", round(mean(df_11_Actual_Phase_extraction$split_percentage_A),2)," %")
      name22_2 <- paste0("Avg Phase B ", round(mean(df_11_Actual_Phase_extraction$split_percentage_B),2)," %")
      df_plot_22_2 <- data.frame(xdata_2, y1_2, y2_2, y11_2, y22_2)
      g_file_11_2 <- ggplot(df_plot_22_2, aes(xdata_2))
      g_file_11_2 <- g_file_11_2 + geom_line(aes(y=y1_2, color = "Phase A %"))
      g_file_11_2 <- g_file_11_2 + geom_line(aes(y=y2_2, color = "Phase B %"))
      g_file_11_2 <- g_file_11_2 + geom_line(aes(y=y11_2, color = name11_2),linetype = "dotted", size=1)
      g_file_11_2 <- g_file_11_2 + geom_line(aes(y=y22_2, color = name22_2),linetype = "dotted", size=1)
      g_file_11_2 <- g_file_11_2 + theme(plot.title = element_text(color="black", size=10, face="bold.italic"),
                                         axis.text=element_text(size=8),
                                         axis.title=element_text(size=9,face="bold"),
                                         legend.text=element_text(size=9),
                                         legend.justification=c(1,1),legend.position=c(1,1),legend.title=element_blank())
      g_file_11_2 <- g_file_11_2 + scale_color_manual(name = " ", values = c("red","darkblue","red","darkblue"))
      g_file_11_2 <- g_file_11_2 + ylab(paste0("Actual Split Percentage %")) + xlab("Time")
      g_file_11_2 <- g_file_11_2 + scale_x_continuous(labels = hms::as_hms(seq(lower_time_int, upper_time_int,by=3600)), breaks=seq(lower_time_int, upper_time_int,by=3600))
      g_file_11_2 <- g_file_11_2 + ylim(0, 100)
      g_file_11_2 <- g_file_11_2 + ggtitle(paste0("Actual Split Percentage for ",site," on ",Date_file11_2))
      g_file_11_2 <- g_file_11_2 + theme_calc()
    }
    if(number_phases_11 == 3){
      y1_2 <- df_11_Actual_Phase_extraction$split_percentage_A
      y2_2 <- df_11_Actual_Phase_extraction$split_percentage_B
      y3_2 <- df_11_Actual_Phase_extraction$split_percentage_C
      y11_2 <- rep(mean(df_11_Actual_Phase_extraction$split_percentage_A),each = nrow(df_11_Actual_Phase_extraction))
      y22_2 <- rep(mean(df_11_Actual_Phase_extraction$split_percentage_B),each = nrow(df_11_Actual_Phase_extraction))
      y33_2 <- rep(mean(df_11_Actual_Phase_extraction$split_percentage_C),each = nrow(df_11_Actual_Phase_extraction))
      name11_2 <- paste0("Avg Phase A ", round(mean(df_11_Actual_Phase_extraction$split_percentage_A),2)," %")
      name22_2 <- paste0("Avg Phase B ", round(mean(df_11_Actual_Phase_extraction$split_percentage_B),2)," %")
      name33_2 <- paste0("Avg Phase C ", round(mean(df_11_Actual_Phase_extraction$split_percentage_C),2)," %")
      df_plot_22_2 <- data.frame(xdata_2, y1_2, y2_2, y3_2, y11_2, y22_2, y33_2)
      g_file_11_2 <- ggplot(df_plot_22_2, aes(xdata_2))
      g_file_11_2 <- g_file_11_2 + geom_line(aes(y=y1_2, color = "Phase A %"))
      g_file_11_2 <- g_file_11_2 + geom_line(aes(y=y2_2, color = "Phase B %"))
      g_file_11_2 <- g_file_11_2 + geom_line(aes(y=y3_2, color = "Phase C %"))
      g_file_11_2 <- g_file_11_2 + geom_line(aes(y=y11_2, color = name11_2),linetype = "dotted", size=1)
      g_file_11_2 <- g_file_11_2 + geom_line(aes(y=y22_2, color = name22_2),linetype = "dotted", size=1)
      g_file_11_2 <- g_file_11_2 + geom_line(aes(y=y33_2, color = name33_2),linetype = "dotted", size=1)
      g_file_11_2 <- g_file_11_2 + theme(plot.title = element_text(color="black", size=10, face="bold.italic"),
                                         axis.text=element_text(size=8),
                                         axis.title=element_text(size=9,face="bold"),
                                         legend.text=element_text(size=9),
                                         legend.justification=c(1,1),legend.position=c(1,1),legend.title=element_blank())
      g_file_11_2 <- g_file_11_2 + scale_color_manual(name = " ", values = c("red","darkblue","orange","red","darkblue","orange"))
      g_file_11_2 <- g_file_11_2 + ylab(paste0("Actual Split Percentage %")) + xlab("Time")
      g_file_11_2 <- g_file_11_2 + scale_x_continuous(labels = hms::as_hms(seq(lower_time_int, upper_time_int,by=3600)), breaks=seq(lower_time_int, upper_time_int,by=3600))
      g_file_11_2 <- g_file_11_2 + ylim(0, 100)
      g_file_11_2 <- g_file_11_2 + ggtitle(paste0("Actual Split Percentage for ",site," on ",Date_file11_2))
      g_file_11_2 <- g_file_11_2 + theme_calc()
    }
    if(number_phases_11 == 4){
      y1_2 <- df_11_Actual_Phase_extraction$split_percentage_A
      y2_2 <- df_11_Actual_Phase_extraction$split_percentage_B
      y3_2 <- df_11_Actual_Phase_extraction$split_percentage_C
      y4_2 <- df_11_Actual_Phase_extraction$split_percentage_D
      y11_2 <- rep(mean(df_11_Actual_Phase_extraction$split_percentage_A),each = nrow(df_11_Actual_Phase_extraction))
      y22_2 <- rep(mean(df_11_Actual_Phase_extraction$split_percentage_B),each = nrow(df_11_Actual_Phase_extraction))
      y33_2 <- rep(mean(df_11_Actual_Phase_extraction$split_percentage_C),each = nrow(df_11_Actual_Phase_extraction))
      y44_2 <- rep(mean(df_11_Actual_Phase_extraction$split_percentage_D),each = nrow(df_11_Actual_Phase_extraction))
      name11_2 <- paste0("Avg Phase A ", round(mean(df_11_Actual_Phase_extraction$split_percentage_A),2)," %")
      name22_2 <- paste0("Avg Phase B ", round(mean(df_11_Actual_Phase_extraction$split_percentage_B),2)," %")
      name33_2 <- paste0("Avg Phase C ", round(mean(df_11_Actual_Phase_extraction$split_percentage_C),2)," %")
      name44_2 <- paste0("Avg Phase D ", round(mean(df_11_Actual_Phase_extraction$split_percentage_D),2)," %")
      df_plot_22_2 <- data.frame(xdata_2, y1_2, y2_2, y3_2, y4_2, y11_2, y22_2, y33_2, y44_2)
      g_file_11_2 <- ggplot(df_plot_22_2, aes(xdata_2))
      g_file_11_2 <- g_file_11_2 + geom_line(aes(y=y1_2, color = "Phase A %"))
      g_file_11_2 <- g_file_11_2 + geom_line(aes(y=y2_2, color = "Phase B %"))
      g_file_11_2 <- g_file_11_2 + geom_line(aes(y=y3_2, color = "Phase C %"))
      g_file_11_2 <- g_file_11_2 + geom_line(aes(y=y4_2, color = "Phase D %"))
      g_file_11_2 <- g_file_11_2 + geom_line(aes(y=y11_2, color = name11_2),linetype = "dotted", size=1)
      g_file_11_2 <- g_file_11_2 + geom_line(aes(y=y22_2, color = name22_2),linetype = "dotted", size=1)
      g_file_11_2 <- g_file_11_2 + geom_line(aes(y=y33_2, color = name33_2),linetype = "dotted", size=1)
      g_file_11_2 <- g_file_11_2 + geom_line(aes(y=y44_2, color = name44_2),linetype = "dotted", size=1)
      g_file_11_2 <- g_file_11_2 + theme(plot.title = element_text(color="black", size=10, face="bold.italic"),
                                         axis.text=element_text(size=8),
                                         axis.title=element_text(size=9,face="bold"),
                                         legend.text=element_text(size=9),
                                         legend.justification=c(1,1),legend.position=c(1,1),legend.title=element_blank())
      g_file_11_2 <- g_file_11_2 + scale_color_manual(name = " ", values = c("red","darkblue","orange","black","red","darkblue","orange","black"))
      g_file_11_2 <- g_file_11_2 + ylab(paste0("Actual Split Percentage %")) + xlab("Time")
      g_file_11_2 <- g_file_11_2 + scale_x_continuous(labels = hms::as_hms(seq(lower_time_int, upper_time_int,by=3600)), breaks=seq(lower_time_int, upper_time_int,by=3600))
      g_file_11_2 <- g_file_11_2 + ylim(0, 100)
      g_file_11_2 <- g_file_11_2 + ggtitle(paste0("Actual Split Percentage for ",site," on ",Date_file11_2))
      g_file_11_2 <- g_file_11_2 + theme_calc()
    }
    if(number_phases_11 == 5){
      y1_2 <- df_11_Actual_Phase_extraction$split_percentage_A
      y2_2 <- df_11_Actual_Phase_extraction$split_percentage_B
      y3_2 <- df_11_Actual_Phase_extraction$split_percentage_C
      y4_2 <- df_11_Actual_Phase_extraction$split_percentage_D
      y5_2 <- df_11_Actual_Phase_extraction$split_percentage_E
      y11_2 <- rep(mean(df_11_Actual_Phase_extraction$split_percentage_A),each = nrow(df_11_Actual_Phase_extraction))
      y22_2 <- rep(mean(df_11_Actual_Phase_extraction$split_percentage_B),each = nrow(df_11_Actual_Phase_extraction))
      y33_2 <- rep(mean(df_11_Actual_Phase_extraction$split_percentage_C),each = nrow(df_11_Actual_Phase_extraction))
      y44_2 <- rep(mean(df_11_Actual_Phase_extraction$split_percentage_D),each = nrow(df_11_Actual_Phase_extraction))
      y55_2 <- rep(mean(df_11_Actual_Phase_extraction$split_percentage_E),each = nrow(df_11_Actual_Phase_extraction))
      name11_2 <- paste0("Avg Phase A ", round(mean(df_11_Actual_Phase_extraction$split_percentage_A),2)," %")
      name22_2 <- paste0("Avg Phase B ", round(mean(df_11_Actual_Phase_extraction$split_percentage_B),2)," %")
      name33_2 <- paste0("Avg Phase C ", round(mean(df_11_Actual_Phase_extraction$split_percentage_C),2)," %")
      name44_2 <- paste0("Avg Phase D ", round(mean(df_11_Actual_Phase_extraction$split_percentage_D),2)," %")
      name55_2 <- paste0("Avg Phase E ", round(mean(df_11_Actual_Phase_extraction$split_percentage_E),2)," %")
      df_plot_22_2 <- data.frame(xdata_2, y1_2, y2_2, y3_2, y4_2, y5_2, y11_2, y22_2, y33_2, y44_2, y55_2)
      g_file_11_2 <- ggplot(df_plot_22_2, aes(xdata_2))
      g_file_11_2 <- g_file_11_2 + geom_line(aes(y=y1_2, color = "Phase A %"))
      g_file_11_2 <- g_file_11_2 + geom_line(aes(y=y2_2, color = "Phase B %"))
      g_file_11_2 <- g_file_11_2 + geom_line(aes(y=y3_2, color = "Phase C %"))
      g_file_11_2 <- g_file_11_2 + geom_line(aes(y=y4_2, color = "Phase D %"))
      g_file_11_2 <- g_file_11_2 + geom_line(aes(y=y5_2, color = "Phase E %"))
      g_file_11_2 <- g_file_11_2 + geom_line(aes(y=y11_2, color = name11_2),linetype = "dotted", size=1)
      g_file_11_2 <- g_file_11_2 + geom_line(aes(y=y22_2, color = name22_2),linetype = "dotted", size=1)
      g_file_11_2 <- g_file_11_2 + geom_line(aes(y=y33_2, color = name33_2),linetype = "dotted", size=1)
      g_file_11_2 <- g_file_11_2 + geom_line(aes(y=y44_2, color = name44_2),linetype = "dotted", size=1)
      g_file_11_2 <- g_file_11_2 + geom_line(aes(y=y55_2, color = name55_2),linetype = "dotted", size=1)
      g_file_11_2 <- g_file_11_2 + theme(plot.title = element_text(color="black", size=10, face="bold.italic"),
                                         axis.text=element_text(size=8),
                                         axis.title=element_text(size=9,face="bold"),
                                         legend.text=element_text(size=9),
                                         legend.justification=c(1,1),legend.position=c(1,1),legend.title=element_blank())
      g_file_11_2 <- g_file_11_2 + scale_color_manual(name = " ", values = c("red","darkblue","orange","black","darkgreen","red","darkblue","orange","black","darkgreen"))
      g_file_11_2 <- g_file_11_2 + ylab(paste0("Actual Split Percentage %")) + xlab("Time")
      g_file_11_2 <- g_file_11_2 + scale_x_continuous(labels = hms::as_hms(seq(lower_time_int, upper_time_int,by=3600)), breaks=seq(lower_time_int, upper_time_int,by=3600))
      g_file_11_2 <- g_file_11_2 + ylim(0, 100)
      g_file_11_2 <- g_file_11_2 + ggtitle(paste0("Actual Split Percentage for ",site," on ",Date_file11_2))
      g_file_11_2 <- g_file_11_2 + theme_calc()
    }
    if(number_phases_11 == 6){
      y1_2 <- df_11_Actual_Phase_extraction$split_percentage_A
      y2_2 <- df_11_Actual_Phase_extraction$split_percentage_B
      y3_2 <- df_11_Actual_Phase_extraction$split_percentage_C
      y4_2 <- df_11_Actual_Phase_extraction$split_percentage_D
      y5_2 <- df_11_Actual_Phase_extraction$split_percentage_E
      y6_2 <- df_11_Actual_Phase_extraction$split_percentage_F
      y11_2 <- rep(mean(df_11_Actual_Phase_extraction$split_percentage_A),each = nrow(df_11_Actual_Phase_extraction))
      y22_2 <- rep(mean(df_11_Actual_Phase_extraction$split_percentage_B),each = nrow(df_11_Actual_Phase_extraction))
      y33_2 <- rep(mean(df_11_Actual_Phase_extraction$split_percentage_C),each = nrow(df_11_Actual_Phase_extraction))
      y44_2 <- rep(mean(df_11_Actual_Phase_extraction$split_percentage_D),each = nrow(df_11_Actual_Phase_extraction))
      y55_2 <- rep(mean(df_11_Actual_Phase_extraction$split_percentage_E),each = nrow(df_11_Actual_Phase_extraction))
      y66_2 <- rep(mean(df_11_Actual_Phase_extraction$split_percentage_F),each = nrow(df_11_Actual_Phase_extraction))
      name11_2 <- paste0("Avg Phase A ", round(mean(df_11_Actual_Phase_extraction$split_percentage_A),2)," %")
      name22_2 <- paste0("Avg Phase B ", round(mean(df_11_Actual_Phase_extraction$split_percentage_B),2)," %")
      name33_2 <- paste0("Avg Phase C ", round(mean(df_11_Actual_Phase_extraction$split_percentage_C),2)," %")
      name44_2 <- paste0("Avg Phase D ", round(mean(df_11_Actual_Phase_extraction$split_percentage_D),2)," %")
      name55_2 <- paste0("Avg Phase E ", round(mean(df_11_Actual_Phase_extraction$split_percentage_E),2)," %")
      name66_2 <- paste0("Avg Phase F ", round(mean(df_11_Actual_Phase_extraction$split_percentage_F),2)," %")
      df_plot_22_2 <- data.frame(xdata_2, y1_2, y2_2, y3_2, y4_2, y5_2, y6_2, y11_2, y22_2, y33_2, y44_2, y55_2, y66_2)
      g_file_11_2 <- ggplot(df_plot_22_2, aes(xdata_2))
      g_file_11_2 <- g_file_11_2 + geom_line(aes(y=y1_2, color = "Phase A %"))
      g_file_11_2 <- g_file_11_2 + geom_line(aes(y=y2_2, color = "Phase B %"))
      g_file_11_2 <- g_file_11_2 + geom_line(aes(y=y3_2, color = "Phase C %"))
      g_file_11_2 <- g_file_11_2 + geom_line(aes(y=y4_2, color = "Phase D %"))
      g_file_11_2 <- g_file_11_2 + geom_line(aes(y=y5_2, color = "Phase E %"))
      g_file_11_2 <- g_file_11_2 + geom_line(aes(y=y6_2, color = "Phase F %"))
      g_file_11_2 <- g_file_11_2 + geom_line(aes(y=y11_2, color = name11_2),linetype = "dotted", size=1)
      g_file_11_2 <- g_file_11_2 + geom_line(aes(y=y22_2, color = name22_2),linetype = "dotted", size=1)
      g_file_11_2 <- g_file_11_2 + geom_line(aes(y=y33_2, color = name33_2),linetype = "dotted", size=1)
      g_file_11_2 <- g_file_11_2 + geom_line(aes(y=y44_2, color = name44_2),linetype = "dotted", size=1)
      g_file_11_2 <- g_file_11_2 + geom_line(aes(y=y55_2, color = name55_2),linetype = "dotted", size=1)
      g_file_11_2 <- g_file_11_2 + geom_line(aes(y=y66_2, color = name66_2),linetype = "dotted", size=1)
      g_file_11_2 <- g_file_11_2 + theme(plot.title = element_text(color="black", size=10, face="bold.italic"),
                                         axis.text=element_text(size=8),
                                         axis.title=element_text(size=9,face="bold"),
                                         legend.text=element_text(size=9),
                                         legend.justification=c(1,1),legend.position=c(1,1),legend.title=element_blank())
      g_file_11_2 <- g_file_11_2 + scale_color_manual(name = " ", values = c("red","darkblue","orange","black","darkgreen","grey","red","darkblue","orange","black","darkgreen","grey"))
      g_file_11_2 <- g_file_11_2 + ylab(paste0("Actual Split Percentage %")) + xlab("Time")
      g_file_11_2 <- g_file_11_2 + scale_x_continuous(labels = hms::as_hms(seq(lower_time_int, upper_time_int,by=3600)), breaks=seq(lower_time_int, upper_time_int,by=3600))
      g_file_11_2 <- g_file_11_2 + ylim(0, 100)
      g_file_11_2 <- g_file_11_2 + ggtitle(paste0("Actual Split Percentage for ",site," on ",Date_file11_2))
      g_file_11_2 <- g_file_11_2 + theme_calc()
    }
    if(number_phases_11 == 7){
      y1_2 <- df_11_Actual_Phase_extraction$split_percentage_A
      y2_2 <- df_11_Actual_Phase_extraction$split_percentage_B
      y3_2 <- df_11_Actual_Phase_extraction$split_percentage_C
      y4_2 <- df_11_Actual_Phase_extraction$split_percentage_D
      y5_2 <- df_11_Actual_Phase_extraction$split_percentage_E
      y6_2 <- df_11_Actual_Phase_extraction$split_percentage_F
      y7_2 <- df_11_Actual_Phase_extraction$split_percentage_G
      y11_2 <- rep(mean(df_11_Actual_Phase_extraction$split_percentage_A),each = nrow(df_11_Actual_Phase_extraction))
      y22_2 <- rep(mean(df_11_Actual_Phase_extraction$split_percentage_B),each = nrow(df_11_Actual_Phase_extraction))
      y33_2 <- rep(mean(df_11_Actual_Phase_extraction$split_percentage_C),each = nrow(df_11_Actual_Phase_extraction))
      y44_2 <- rep(mean(df_11_Actual_Phase_extraction$split_percentage_D),each = nrow(df_11_Actual_Phase_extraction))
      y55_2 <- rep(mean(df_11_Actual_Phase_extraction$split_percentage_E),each = nrow(df_11_Actual_Phase_extraction))
      y66_2 <- rep(mean(df_11_Actual_Phase_extraction$split_percentage_F),each = nrow(df_11_Actual_Phase_extraction))
      y77_2 <- rep(mean(df_11_Actual_Phase_extraction$split_percentage_G),each = nrow(df_11_Actual_Phase_extraction))
      name11_2 <- paste0("Avg Phase A ", round(mean(df_11_Actual_Phase_extraction$split_percentage_A),2)," %")
      name22_2 <- paste0("Avg Phase B ", round(mean(df_11_Actual_Phase_extraction$split_percentage_B),2)," %")
      name33_2 <- paste0("Avg Phase C ", round(mean(df_11_Actual_Phase_extraction$split_percentage_C),2)," %")
      name44_2 <- paste0("Avg Phase D ", round(mean(df_11_Actual_Phase_extraction$split_percentage_D),2)," %")
      name55_2 <- paste0("Avg Phase E ", round(mean(df_11_Actual_Phase_extraction$split_percentage_E),2)," %")
      name66_2 <- paste0("Avg Phase F ", round(mean(df_11_Actual_Phase_extraction$split_percentage_F),2)," %")
      name77_2 <- paste0("Avg Phase G ", round(mean(df_11_Actual_Phase_extraction$split_percentage_G),2)," %")
      df_plot_22_2 <- data.frame(xdata_2, y1_2, y2_2, y3_2, y4_2, y5_2, y6_2, y7_2, y11_2, y22_2, y33_2, y44_2, y55_2, y66_2, y77_2)
      g_file_11_2 <- ggplot(df_plot_22_2, aes(xdata_2))
      g_file_11_2 <- g_file_11_2 + geom_line(aes(y=y1_2, color = "Phase A %"))
      g_file_11_2 <- g_file_11_2 + geom_line(aes(y=y2_2, color = "Phase B %"))
      g_file_11_2 <- g_file_11_2 + geom_line(aes(y=y3_2, color = "Phase C %"))
      g_file_11_2 <- g_file_11_2 + geom_line(aes(y=y4_2, color = "Phase D %"))
      g_file_11_2 <- g_file_11_2 + geom_line(aes(y=y5_2, color = "Phase E %"))
      g_file_11_2 <- g_file_11_2 + geom_line(aes(y=y6_2, color = "Phase F %"))
      g_file_11_2 <- g_file_11_2 + geom_line(aes(y=y7_2, color = "Phase G %"))
      g_file_11_2 <- g_file_11_2 + geom_line(aes(y=y11_2, color = name11_2),linetype = "dotted", size=1)
      g_file_11_2 <- g_file_11_2 + geom_line(aes(y=y22_2, color = name22_2),linetype = "dotted", size=1)
      g_file_11_2 <- g_file_11_2 + geom_line(aes(y=y33_2, color = name33_2),linetype = "dotted", size=1)
      g_file_11_2 <- g_file_11_2 + geom_line(aes(y=y44_2, color = name44_2),linetype = "dotted", size=1)
      g_file_11_2 <- g_file_11_2 + geom_line(aes(y=y55_2, color = name55_2),linetype = "dotted", size=1)
      g_file_11_2 <- g_file_11_2 + geom_line(aes(y=y66_2, color = name66_2),linetype = "dotted", size=1)
      g_file_11_2 <- g_file_11_2 + geom_line(aes(y=y77_2, color = name77_2),linetype = "dotted", size=1)
      g_file_11_2 <- g_file_11_2 + theme(plot.title = element_text(color="black", size=10, face="bold.italic"),
                                         axis.text=element_text(size=8),
                                         axis.title=element_text(size=9,face="bold"),
                                         legend.text=element_text(size=9),
                                         legend.justification=c(1,1),legend.position=c(1,1),legend.title=element_blank())
      g_file_11_2 <- g_file_11_2 + scale_color_manual(name = " ", values = c("red","darkblue","orange","black","darkgreen","grey","yellow","red","darkblue","orange","black","darkgreen","grey","yellow"))
      g_file_11_2 <- g_file_11_2 + ylab(paste0("Actual Split Percentage %")) + xlab("Time")
      g_file_11_2 <- g_file_11_2 + scale_x_continuous(labels = hms::as_hms(seq(lower_time_int, upper_time_int,by=3600)), breaks=seq(lower_time_int, upper_time_int,by=3600))
      g_file_11_2 <- g_file_11_2 + ylim(0, 100)
      g_file_11_2 <- g_file_11_2 + ggtitle(paste0("Actual Split Percentage for ",site," on ",Date_file11_2))
      g_file_11_2 <- g_file_11_2 + theme_calc()
    }
    if(number_phases_11 == 8){
      y1_2 <- df_11_Actual_Phase_extraction$split_percentage_A
      y2_2 <- df_11_Actual_Phase_extraction$split_percentage_B
      y3_2 <- df_11_Actual_Phase_extraction$split_percentage_C
      y4_2 <- df_11_Actual_Phase_extraction$split_percentage_D
      y5_2 <- df_11_Actual_Phase_extraction$split_percentage_E
      y6_2 <- df_11_Actual_Phase_extraction$split_percentage_F
      y7_2 <- df_11_Actual_Phase_extraction$split_percentage_G
      y8_2 <- df_11_Actual_Phase_extraction$split_percentage_H
      y11_2 <- rep(mean(df_11_Actual_Phase_extraction$split_percentage_A),each = nrow(df_11_Actual_Phase_extraction))
      y22_2 <- rep(mean(df_11_Actual_Phase_extraction$split_percentage_B),each = nrow(df_11_Actual_Phase_extraction))
      y33_2 <- rep(mean(df_11_Actual_Phase_extraction$split_percentage_C),each = nrow(df_11_Actual_Phase_extraction))
      y44_2 <- rep(mean(df_11_Actual_Phase_extraction$split_percentage_D),each = nrow(df_11_Actual_Phase_extraction))
      y55_2 <- rep(mean(df_11_Actual_Phase_extraction$split_percentage_E),each = nrow(df_11_Actual_Phase_extraction))
      y66_2 <- rep(mean(df_11_Actual_Phase_extraction$split_percentage_F),each = nrow(df_11_Actual_Phase_extraction))
      y77_2 <- rep(mean(df_11_Actual_Phase_extraction$split_percentage_G),each = nrow(df_11_Actual_Phase_extraction))
      y88_2 <- rep(mean(df_11_Actual_Phase_extraction$split_percentage_H),each = nrow(df_11_Actual_Phase_extraction))
      name11_2 <- paste0("Avg Phase A ", round(mean(df_11_Actual_Phase_extraction$split_percentage_A),2)," %")
      name22_2 <- paste0("Avg Phase B ", round(mean(df_11_Actual_Phase_extraction$split_percentage_B),2)," %")
      name33_2 <- paste0("Avg Phase C ", round(mean(df_11_Actual_Phase_extraction$split_percentage_C),2)," %")
      name44_2 <- paste0("Avg Phase D ", round(mean(df_11_Actual_Phase_extraction$split_percentage_D),2)," %")
      name55_2 <- paste0("Avg Phase E ", round(mean(df_11_Actual_Phase_extraction$split_percentage_E),2)," %")
      name66_2 <- paste0("Avg Phase F ", round(mean(df_11_Actual_Phase_extraction$split_percentage_F),2)," %")
      name77_2 <- paste0("Avg Phase G ", round(mean(df_11_Actual_Phase_extraction$split_percentage_G),2)," %")
      name88_2 <- paste0("Avg Phase H ", round(mean(df_11_Actual_Phase_extraction$split_percentage_H),2)," %")
      df_plot_22_2 <- data.frame(xdata_2, y1_2, y2_2, y3_2, y4_2, y5_2, y6_2, y7_2, y8_2, y11_2, y22_2, y33_2, y44_2, y55_2, y66_2, y77_2, y88_2)
      g_file_11_2 <- ggplot(df_plot_22_2, aes(xdata_2))
      g_file_11_2 <- g_file_11_2 + geom_line(aes(y=y1_2, color = "Phase A %"))
      g_file_11_2 <- g_file_11_2 + geom_line(aes(y=y2_2, color = "Phase B %"))
      g_file_11_2 <- g_file_11_2 + geom_line(aes(y=y3_2, color = "Phase C %"))
      g_file_11_2 <- g_file_11_2 + geom_line(aes(y=y4_2, color = "Phase D %"))
      g_file_11_2 <- g_file_11_2 + geom_line(aes(y=y5_2, color = "Phase E %"))
      g_file_11_2 <- g_file_11_2 + geom_line(aes(y=y6_2, color = "Phase F %"))
      g_file_11_2 <- g_file_11_2 + geom_line(aes(y=y7_2, color = "Phase G %"))
      g_file_11_2 <- g_file_11_2 + geom_line(aes(y=y8_2, color = "Phase H %"))
      g_file_11_2 <- g_file_11_2 + geom_line(aes(y=y11_2, color = name11_2),linetype = "dotted", size=1)
      g_file_11_2 <- g_file_11_2 + geom_line(aes(y=y22_2, color = name22_2),linetype = "dotted", size=1)
      g_file_11_2 <- g_file_11_2 + geom_line(aes(y=y33_2, color = name33_2),linetype = "dotted", size=1)
      g_file_11_2 <- g_file_11_2 + geom_line(aes(y=y44_2, color = name44_2),linetype = "dotted", size=1)
      g_file_11_2 <- g_file_11_2 + geom_line(aes(y=y55_2, color = name55_2),linetype = "dotted", size=1)
      g_file_11_2 <- g_file_11_2 + geom_line(aes(y=y66_2, color = name66_2),linetype = "dotted", size=1)
      g_file_11_2 <- g_file_11_2 + geom_line(aes(y=y77_2, color = name77_2),linetype = "dotted", size=1)
      g_file_11_2 <- g_file_11_2 + geom_line(aes(y=y88_2, color = name88_2),linetype = "dotted", size=1)
      g_file_11_2 <- g_file_11_2 + theme(plot.title = element_text(color="black", size=10, face="bold.italic"),
                                         axis.text=element_text(size=8),
                                         axis.title=element_text(size=9,face="bold"),
                                         legend.text=element_text(size=9),
                                         legend.justification=c(1,1),legend.position=c(1,1),legend.title=element_blank())
      g_file_11_2 <- g_file_11_2 + scale_color_manual(name = " ", values = c("red","darkblue","orange","black","darkgreen","grey","yellow","green","red","darkblue","orange","black","darkgreen","grey","yellow","green"))
      g_file_11_2 <- g_file_11_2 + ylab(paste0("Actual Split Percentage %")) + xlab("Time")
      g_file_11_2 <- g_file_11_2 + scale_x_continuous(labels = hms::as_hms(seq(lower_time_int, upper_time_int,by=3600)), breaks=seq(lower_time_int, upper_time_int,by=3600))
      g_file_11_2 <- g_file_11_2 + ylim(0, 100)
      g_file_11_2 <- g_file_11_2 + ggtitle(paste0("Actual Split Percentage for ",site," on ",Date_file11_2))
      g_file_11_2 <- g_file_11_2 + theme_calc()
    }
    if(number_phases_11 == 9){
      y1_2 <- df_11_Actual_Phase_extraction$split_percentage_A
      y2_2 <- df_11_Actual_Phase_extraction$split_percentage_B
      y3_2 <- df_11_Actual_Phase_extraction$split_percentage_C
      y4_2 <- df_11_Actual_Phase_extraction$split_percentage_D
      y5_2 <- df_11_Actual_Phase_extraction$split_percentage_E
      y6_2 <- df_11_Actual_Phase_extraction$split_percentage_F
      y7_2 <- df_11_Actual_Phase_extraction$split_percentage_G
      y8_2 <- df_11_Actual_Phase_extraction$split_percentage_H
      y9_2 <- df_11_Actual_Phase_extraction$split_percentage_I
      y11_2 <- rep(mean(df_11_Actual_Phase_extraction$split_percentage_A),each = nrow(df_11_Actual_Phase_extraction))
      y22_2 <- rep(mean(df_11_Actual_Phase_extraction$split_percentage_B),each = nrow(df_11_Actual_Phase_extraction))
      y33_2 <- rep(mean(df_11_Actual_Phase_extraction$split_percentage_C),each = nrow(df_11_Actual_Phase_extraction))
      y44_2 <- rep(mean(df_11_Actual_Phase_extraction$split_percentage_D),each = nrow(df_11_Actual_Phase_extraction))
      y55_2 <- rep(mean(df_11_Actual_Phase_extraction$split_percentage_E),each = nrow(df_11_Actual_Phase_extraction))
      y66_2 <- rep(mean(df_11_Actual_Phase_extraction$split_percentage_F),each = nrow(df_11_Actual_Phase_extraction))
      y77_2 <- rep(mean(df_11_Actual_Phase_extraction$split_percentage_G),each = nrow(df_11_Actual_Phase_extraction))
      y88_2 <- rep(mean(df_11_Actual_Phase_extraction$split_percentage_H),each = nrow(df_11_Actual_Phase_extraction))
      y99_2 <- rep(mean(df_11_Actual_Phase_extraction$split_percentage_I),each = nrow(df_11_Actual_Phase_extraction))
      name11_2 <- paste0("Avg Phase A ", round(mean(df_11_Actual_Phase_extraction$split_percentage_A),2)," %")
      name22_2 <- paste0("Avg Phase B ", round(mean(df_11_Actual_Phase_extraction$split_percentage_B),2)," %")
      name33_2 <- paste0("Avg Phase C ", round(mean(df_11_Actual_Phase_extraction$split_percentage_C),2)," %")
      name44_2 <- paste0("Avg Phase D ", round(mean(df_11_Actual_Phase_extraction$split_percentage_D),2)," %")
      name55_2 <- paste0("Avg Phase E ", round(mean(df_11_Actual_Phase_extraction$split_percentage_E),2)," %")
      name66_2 <- paste0("Avg Phase F ", round(mean(df_11_Actual_Phase_extraction$split_percentage_F),2)," %")
      name77_2 <- paste0("Avg Phase G ", round(mean(df_11_Actual_Phase_extraction$split_percentage_G),2)," %")
      name88_2 <- paste0("Avg Phase H ", round(mean(df_11_Actual_Phase_extraction$split_percentage_H),2)," %")
      name99_2 <- paste0("Avg Phase I ", round(mean(df_11_Actual_Phase_extraction$split_percentage_I),2)," %")
      df_plot_22_2 <- data.frame(xdata_2, y1_2, y2_2, y3_2, y4_2, y5_2, y6_2, y7_2, y8_2, y9_2, y11_2, y22_2, y33_2, y44_2, y55_2, y66_2, y77_2, y88_2, y99_2)
      g_file_11_2 <- ggplot(df_plot_22_2, aes(xdata_2))
      g_file_11_2 <- g_file_11_2 + geom_line(aes(y=y1_2, color = "Phase A %"))
      g_file_11_2 <- g_file_11_2 + geom_line(aes(y=y2_2, color = "Phase B %"))
      g_file_11_2 <- g_file_11_2 + geom_line(aes(y=y3_2, color = "Phase C %"))
      g_file_11_2 <- g_file_11_2 + geom_line(aes(y=y4_2, color = "Phase D %"))
      g_file_11_2 <- g_file_11_2 + geom_line(aes(y=y5_2, color = "Phase E %"))
      g_file_11_2 <- g_file_11_2 + geom_line(aes(y=y6_2, color = "Phase F %"))
      g_file_11_2 <- g_file_11_2 + geom_line(aes(y=y7_2, color = "Phase G %"))
      g_file_11_2 <- g_file_11_2 + geom_line(aes(y=y8_2, color = "Phase H %"))
      g_file_11_2 <- g_file_11_2 + geom_line(aes(y=y9_2, color = "Phase I %"))
      g_file_11_2 <- g_file_11_2 + geom_line(aes(y=y11_2, color = name11_2),linetype = "dotted", size=1)
      g_file_11_2 <- g_file_11_2 + geom_line(aes(y=y22_2, color = name22_2),linetype = "dotted", size=1)
      g_file_11_2 <- g_file_11_2 + geom_line(aes(y=y33_2, color = name33_2),linetype = "dotted", size=1)
      g_file_11_2 <- g_file_11_2 + geom_line(aes(y=y44_2, color = name44_2),linetype = "dotted", size=1)
      g_file_11_2 <- g_file_11_2 + geom_line(aes(y=y55_2, color = name55_2),linetype = "dotted", size=1)
      g_file_11_2 <- g_file_11_2 + geom_line(aes(y=y66_2, color = name66_2),linetype = "dotted", size=1)
      g_file_11_2 <- g_file_11_2 + geom_line(aes(y=y77_2, color = name77_2),linetype = "dotted", size=1)
      g_file_11_2 <- g_file_11_2 + geom_line(aes(y=y88_2, color = name88_2),linetype = "dotted", size=1)
      g_file_11_2 <- g_file_11_2 + geom_line(aes(y=y99_2, color = name99_2),linetype = "dotted", size=1)
      g_file_11_2 <- g_file_11_2 + theme(plot.title = element_text(color="black", size=10, face="bold.italic"),
                                         axis.text=element_text(size=8),
                                         axis.title=element_text(size=9,face="bold"),
                                         legend.text=element_text(size=9),
                                         legend.justification=c(1,1),legend.position=c(1,1),legend.title=element_blank())
      g_file_11_2 <- g_file_11_2 + scale_color_manual(name = " ", values = c("red","darkblue","orange","black","darkgreen","grey","yellow","green","pink","red","darkblue","orange","black","darkgreen","grey","yellow","green","pink"))
      g_file_11_2 <- g_file_11_2 + ylab(paste0("Actual Split Percentage %")) + xlab("Time")
      g_file_11_2 <- g_file_11_2 + scale_x_continuous(labels = hms::as_hms(seq(lower_time_int, upper_time_int,by=3600)), breaks=seq(lower_time_int, upper_time_int,by=3600))
      g_file_11_2 <- g_file_11_2 + ylim(0, 100)
      g_file_11_2 <- g_file_11_2 + ggtitle(paste0("Actual Split Percentage for ",site," on ",Date_file11_2))
      g_file_11_2 <- g_file_11_2 + theme_calc()
    }
    output$Plot11_2 <- renderPlot({g_file_11_2})
  })
  output$downloadData11_2 <- downloadHandler(
    
    filename = function() { 
      paste("MX_Value_Phase", Sys.time(), ".csv", sep="")
    },
    content = function(file11_2) { 
      progress <- shiny::Progress$new()
      # Make sure it closes when we exit this reactive, even if there's an error
      on.exit(progress$close())
      progress$set(message = "Analyizing the data....", value = 0)
      lower_time <- paste0(hms::as_hms(input$file11_2_lower_time)) 
      upper_time <- paste0(hms::as_hms(input$file11_2_upper_time)) 
      input_file_phase <- paste0(input$file11_2_phase_file$datapath)
      site <- as.integer(input$file11_2_site_ID)
      
      Actual_Phase_extraction <- function(input_file_phase){
        df <- readr::read_csv(input_file_phase)
        df$date <- as.Date(substr(df$Start,1,10))
        df$start_time <- as.integer(hms::as_hms(substr(df$Start,12,19)))
        df$day <- weekdays(df$date)
        df <- df[df$Phase!= "Unknown",]
        
        phases <- unique(df$Phase)
        if(df$Phase[1]!="<A>"){
          df <- df[-c(1),]
          if(df$Phase[1]!="<A>"){
            df <- df[-c(1),]
            if(df$Phase[1]!="<A>"){
              df <- df[-c(1),]
              if(df$Phase[1]!="<A>"){
                df <- df[-c(1),]
                if(df$Phase[1]!="<A>"){
                  df <- df[-c(1),]
                  if(df$Phase[1]!="<A>"){
                    df <- df[-c(1),]
                    if(df$Phase[1]!="<A>"){
                      df <- df[-c(1),]
                      if(df$Phase[1]!="<A>"){
                        df <- df[-c(1),]
                        if(df$Phase[1]!="<A>"){
                          df <- df[-c(1),]
                          if(df$Phase[1]!="<A>"){
                            df <- df[-c(1),]
                            if(df$Phase[1]!="<A>"){
                              df <- df[-c(1),]
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
        df$rank <- rank(df$Phase,ties.method= "first")
        df$rank[df$Phase!="<A>"] <- NA
        df$start_time2 <- df$start_time
        df$start_time2[df$Phase!="<A>"] <- NA
        df <- df %>% tidyr::fill(rank,.direction = c("down"))
        df <- df %>% tidyr::fill(start_time2,.direction = c("down"))
        df <- data.table::as.data.table(df)
        df <- df[, actual_cycle_length := sum(`Duration (s)`), by = "rank"][]
        rank_df <- df %>% dplyr::select("rank","day","date","start_time2","actual_cycle_length")
        rank_df <- data.frame(unique(rank_df))
        
        phases_df <- data.frame(phases)
        occur_df <- data.frame(1:max(df$rank))
        colnames(occur_df) <- c("rank")
        df1 <- merge(phases_df, occur_df,by=NULL)
        df2 <- sqldf::sqldf("select a.phases, a.rank, b.`Duration (s)`, b.Start, b.End, b.Gapped, c.date, b.start_time, c.day, c.start_time2, c.actual_cycle_length
                    from df1 as a
                    left join df as b
                    on a.rank = b.rank
                    and a.phases = b.Phase
                    left join rank_df as c
                    on a.rank = c.rank", drv = "SQLite")
        df2$`Duration (s)`[is.na(df2$`Duration (s)`)] <- 0
        df2$split_percentage <- df2$`Duration (s)`/df2$actual_cycle_length
        df2$start_time3 <- as.integer(df2$start_time2/60)*60
        
        
        df3 <- sqldf::sqldf("select a.start_time3, a.phases, a.day, a.date, avg(a.split_percentage) as split_percentage, avg(a.actual_cycle_length) as actual_cycle_length
                    from df2 as a
                    group by a.start_time3, a.phases, a.day, a.date", drv = "SQLite")
        
        time_df <- data.frame(seq(60,60*60*24,by=60))
        colnames(time_df) <- c("time")
        time_df2 <- merge(phases_df, time_df,by=NULL)
        
        df4 <- sqldf::sqldf("select b.time, a.start_time3, b.phases, a.day, a.date, a.split_percentage, a.actual_cycle_length
                    from time_df2 as b
                    left join df3 as a
                    on b.time = a.start_time3
                    and b.phases = a.phases", drv = "SQLite")
        df4 <- df4 %>% tidyr::fill(start_time3,.direction = c("downup"))
        # df4 <- df4 %>% tidyr::fill(Site,.direction = c("downup"))
        df4 <- df4 %>% tidyr::fill(day,.direction = c("downup"))
        df4 <- df4 %>% tidyr::fill(date,.direction = c("downup"))
        df4 <- df4 %>% tidyr::fill(actual_cycle_length,.direction = c("downup"))
        
        df5 <-  df4[with(df4, order(phases, time)),]
        df5 <- df5 %>% tidyr::fill(split_percentage,.direction ="downup")
        df5 <-  df5[with(df5, order(time)),]
        rownames(df5) <- 1:nrow(df5)
        df_final_results <- NULL
        if(length(phases)==2){
          df_final_results_A <- df5 %>% dplyr::filter(phases == "<A>")
          df_final_results_B <- df5 %>% dplyr::filter(phases == "B")
          df_final_results$time <- df_final_results_A$time
          df_final_results$split_percentage_A <- df_final_results_A$split_percentage*100
          df_final_results$split_percentage_B <- df_final_results_B$split_percentage*100
          df_final_results <- as.data.frame(df_final_results)
        }
        if(length(phases)==3){
          df_final_results_A <- df5 %>% dplyr::filter(phases == "<A>")
          df_final_results_B <- df5 %>% dplyr::filter(phases == "B")
          df_final_results_C <- df5 %>% dplyr::filter(phases == "C")
          df_final_results$time <- df_final_results_A$time
          df_final_results$split_percentage_A <- df_final_results_A$split_percentage*100
          df_final_results$split_percentage_B <- df_final_results_B$split_percentage*100
          df_final_results$split_percentage_C <- df_final_results_C$split_percentage*100
          df_final_results <- as.data.frame(df_final_results)
        }
        if(length(phases)==4){
          df_final_results_A <- df5 %>% dplyr::filter(phases == "<A>")
          df_final_results_B <- df5 %>% dplyr::filter(phases == "B")
          df_final_results_C <- df5 %>% dplyr::filter(phases == "C")
          df_final_results_D <- df5 %>% dplyr::filter(phases == "D")
          df_final_results$time <- df_final_results_A$time
          df_final_results$split_percentage_A <- df_final_results_A$split_percentage*100
          df_final_results$split_percentage_B <- df_final_results_B$split_percentage*100
          df_final_results$split_percentage_C <- df_final_results_C$split_percentage*100
          df_final_results$split_percentage_D <- df_final_results_D$split_percentage*100
          df_final_results <- as.data.frame(df_final_results)
        }
        if(length(phases)==5){
          df_final_results_A <- df5 %>% dplyr::filter(phases == "<A>")
          df_final_results_B <- df5 %>% dplyr::filter(phases == "B")
          df_final_results_C <- df5 %>% dplyr::filter(phases == "C")
          df_final_results_D <- df5 %>% dplyr::filter(phases == "D")
          df_final_results_E <- df5 %>% dplyr::filter(phases == "E")
          df_final_results$time <- df_final_results_A$time
          df_final_results$split_percentage_A <- df_final_results_A$split_percentage*100
          df_final_results$split_percentage_B <- df_final_results_B$split_percentage*100
          df_final_results$split_percentage_C <- df_final_results_C$split_percentage*100
          df_final_results$split_percentage_D <- df_final_results_D$split_percentage*100
          df_final_results$split_percentage_E <- df_final_results_E$split_percentage*100
          df_final_results <- as.data.frame(df_final_results)
        }
        if(length(phases)==6){
          df_final_results_A <- df5 %>% dplyr::filter(phases == "<A>")
          df_final_results_B <- df5 %>% dplyr::filter(phases == "B")
          df_final_results_C <- df5 %>% dplyr::filter(phases == "C")
          df_final_results_D <- df5 %>% dplyr::filter(phases == "D")
          df_final_results_E <- df5 %>% dplyr::filter(phases == "E")
          df_final_results_F <- df5 %>% dplyr::filter(phases == "F")
          df_final_results$time <- df_final_results_A$time
          df_final_results$split_percentage_A <- df_final_results_A$split_percentage*100
          df_final_results$split_percentage_B <- df_final_results_B$split_percentage*100
          df_final_results$split_percentage_C <- df_final_results_C$split_percentage*100
          df_final_results$split_percentage_D <- df_final_results_D$split_percentage*100
          df_final_results$split_percentage_E <- df_final_results_E$split_percentage*100
          df_final_results$split_percentage_F <- df_final_results_F$split_percentage*100
          df_final_results <- as.data.frame(df_final_results)
        }
        if(length(phases)==7){
          df_final_results_A <- df5 %>% dplyr::filter(phases == "<A>")
          df_final_results_B <- df5 %>% dplyr::filter(phases == "B")
          df_final_results_C <- df5 %>% dplyr::filter(phases == "C")
          df_final_results_D <- df5 %>% dplyr::filter(phases == "D")
          df_final_results_E <- df5 %>% dplyr::filter(phases == "E")
          df_final_results_F <- df5 %>% dplyr::filter(phases == "F")
          df_final_results_G <- df5 %>% dplyr::filter(phases == "G")
          df_final_results$time <- df_final_results_A$time
          df_final_results$split_percentage_A <- df_final_results_A$split_percentage*100
          df_final_results$split_percentage_B <- df_final_results_B$split_percentage*100
          df_final_results$split_percentage_C <- df_final_results_C$split_percentage*100
          df_final_results$split_percentage_D <- df_final_results_D$split_percentage*100
          df_final_results$split_percentage_E <- df_final_results_E$split_percentage*100
          df_final_results$split_percentage_F <- df_final_results_F$split_percentage*100
          df_final_results$split_percentage_F <- df_final_results_G$split_percentage*100
          df_final_results <- as.data.frame(df_final_results)
        }
        if(length(phases)==8){
          df_final_results_A <- df5 %>% dplyr::filter(phases == "<A>")
          df_final_results_B <- df5 %>% dplyr::filter(phases == "B")
          df_final_results_C <- df5 %>% dplyr::filter(phases == "C")
          df_final_results_D <- df5 %>% dplyr::filter(phases == "D")
          df_final_results_E <- df5 %>% dplyr::filter(phases == "E")
          df_final_results_F <- df5 %>% dplyr::filter(phases == "F")
          df_final_results_G <- df5 %>% dplyr::filter(phases == "G")
          df_final_results_H <- df5 %>% dplyr::filter(phases == "H")
          df_final_results$time <- df_final_results_A$time
          df_final_results$split_percentage_A <- df_final_results_A$split_percentage*100
          df_final_results$split_percentage_B <- df_final_results_B$split_percentage*100
          df_final_results$split_percentage_C <- df_final_results_C$split_percentage*100
          df_final_results$split_percentage_D <- df_final_results_D$split_percentage*100
          df_final_results$split_percentage_E <- df_final_results_E$split_percentage*100
          df_final_results$split_percentage_F <- df_final_results_F$split_percentage*100
          df_final_results$split_percentage_F <- df_final_results_G$split_percentage*100
          df_final_results$split_percentage_H <- df_final_results_H$split_percentage*100
          df_final_results <- as.data.frame(df_final_results)
        }
        return(df_final_results)
      }
      Date_file11_2 <- as.Date(substr(readr::read_csv(input_file_phase)$Time[1],1,10))
      
      df_11_Actual_Phase_extraction <- Actual_Phase_extraction(input_file_phase = input_file_phase)
      
      number_phases_11 <- ncol(df_11_Actual_Phase_extraction)-1
      
      df_11_Actual_Phase_extraction <- df_11_Actual_Phase_extraction %>% dplyr::filter(time >= as.integer(hms::as_hms(lower_time)))
      df_11_Actual_Phase_extraction <- df_11_Actual_Phase_extraction %>% dplyr::filter(time <= as.integer(hms::as_hms(upper_time)))
      df_11_Actual_Phase_extraction$time <- hms::as_hms(df_11_Actual_Phase_extraction$time)
      write.csv(df_11_Actual_Phase_extraction, file11_2, row.names=FALSE)
    })
  observeEvent(input$previewData_bottleneck_ranking ,{
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = "Creating a Map...", value = 0) 
    
    input_file1_bottleneck <- paste0(input$bottlneck_file1$datapath)
    input_file2_bottleneck <- paste0(input$bottlneck_file2$datapath)
    bottleneck_analysis <- function(file1_bottleneck, file2_bottleneck){
      library(dplyr)
      df_bottleneck_1 <- readr::read_csv(file1_bottleneck)
      df_bottleneck_2 <- readr::read_csv(file2_bottleneck)
      colnames(df_bottleneck_1) <- c("Rank1","Head Location1","Starting TMC Code1","Latitude1","Longitude1"
                                     ,"Average Max Length1","Average Daily Duration1","Total Duration1"
                                     ,"Agency-Reported Events1","Volume Estimate1","Base Impact1"
                                     ,"Speed Differential1","Congestion1","Total Delay1","Segment IDs1")
      colnames(df_bottleneck_2) <- c("Rank2","Head Location2","Starting TMC Code2","Latitude2","Longitude2"
                                     ,"Average Max Length2","Average Daily Duration2","Total Duration2"
                                     ,"Agency-Reported Events2","Volume Estimate2","Base Impact2"
                                     ,"Speed Differential2","Congestion2","Total Delay2","Segment IDs2")
      # unique(c(df_bottleneck_1$`Head Location1`,df_bottleneck_2$`Head Location2`))
      df_bottleneck_res <- as.data.frame(unique(c(df_bottleneck_1$`Head Location1`,df_bottleneck_2$`Head Location2`)))
      colnames(df_bottleneck_res) <- "Head Location"
      df_bottleneck_res <- df_bottleneck_res %>% dplyr::left_join(df_bottleneck_1, by =c("Head Location"="Head Location1"))
      df_bottleneck_res <- df_bottleneck_res %>% dplyr::left_join(df_bottleneck_2, by =c("Head Location"="Head Location2"))
      df_bottleneck_res$`Total Duration1`[7]
      sub("d*.", "", df_bottleneck_res$`Total Duration1`[1])
      as.integer(gsub(".*(.+) d.*", "\\1", df_bottleneck_res$`Total Duration1`[7])) 
      + as.integer(gsub(".* d (.+) h.*", "\\1", df_bottleneck_res$`Total Duration1`[7]))
      + as.integer(gsub(".* h (.+) m.*", "\\1", df_bottleneck_res$`Total Duration1`[7]))
      for(bottleneck_k in (1:nrow(df_bottleneck_res))){
        # df_bottleneck_res$`Total Duration Year1`[bottleneck_k] <- 
        #   if(!is.na(as.integer(gsub(".* c (.+) y.*", "\\1", df_bottleneck_res$`Total Duration1`[bottleneck_k])))){
        #     as.integer(gsub(".* c (.+) y.*", "\\1", df_bottleneck_res$`Total Duration1`[bottleneck_k]))
        #   } else if (!is.na(as.integer(gsub(" y.*", "\\1", df_bottleneck_res$`Total Duration1`[bottleneck_k])))) {
        #     as.integer(gsub(" y.*", "\\1", df_bottleneck_res$`Total Duration1`[bottleneck_k]))
        #   } else {
        #     0
        #   }
        
        # df_bottleneck_res$`Total Duration Month1`[bottleneck_k] <- 
        #   if(!is.na(as.integer(gsub(".* y (.+) m.*", "\\1", df_bottleneck_res$`Total Duration1`[bottleneck_k])))){
        #     as.integer(gsub(".* y (.+) m.*", "\\1", df_bottleneck_res$`Total Duration1`[bottleneck_k]))
        #   } else if (!is.na(as.integer(gsub(" m.*", "\\1", df_bottleneck_res$`Total Duration1`[bottleneck_k])))) {
        #     as.integer(gsub(" m.*", "\\1", df_bottleneck_res$`Total Duration1`[bottleneck_k]))
        #   } else {
        #     0
        #   }
        df_bottleneck_res$`Total Duration Day1`[bottleneck_k] <- 
          if(!is.na(as.integer(gsub(".* m (.+) d.*", "\\1", df_bottleneck_res$`Total Duration1`[bottleneck_k])))){
            as.integer(gsub(".* m (.+) d.*", "\\1", df_bottleneck_res$`Total Duration1`[bottleneck_k]))
          } else if (!is.na(as.integer(gsub(" d.*", "\\1", df_bottleneck_res$`Total Duration1`[bottleneck_k])))) {
            as.integer(gsub(" d.*", "\\1", df_bottleneck_res$`Total Duration1`[bottleneck_k]))
          } else {
            0
          }
        df_bottleneck_res$`Total Duration Hour1`[bottleneck_k] <- 
          if(!is.na(as.integer(gsub(".* d (.+) h.*", "\\1", df_bottleneck_res$`Total Duration1`[bottleneck_k])))){
            as.integer(gsub(".* d (.+) h.*", "\\1", df_bottleneck_res$`Total Duration1`[bottleneck_k]))
          } else if (!is.na(as.integer(gsub(" h.*", "\\1", df_bottleneck_res$`Total Duration1`[bottleneck_k])))) {
            as.integer(gsub(" h.*", "\\1", df_bottleneck_res$`Total Duration1`[bottleneck_k]))
          } else {
            0
          }
        df_bottleneck_res$`Total Duration Minute1`[bottleneck_k] <- 
          if(!is.na(as.integer(gsub(".* h (.+) m.*", "\\1", df_bottleneck_res$`Total Duration1`[bottleneck_k])))){
            as.integer(gsub(".* h (.+) m.*", "\\1", df_bottleneck_res$`Total Duration1`[bottleneck_k]))
          } else if (!is.na(as.integer(gsub(" m.*", "\\1", df_bottleneck_res$`Total Duration1`[bottleneck_k])))) {
            as.integer(gsub(" m.*", "\\1", df_bottleneck_res$`Total Duration1`[bottleneck_k]))
          } else {
            0
          }
        # df_bottleneck_res$`Total Duration Year2`[bottleneck_k] <- 
        #   if(!is.na(as.integer(gsub(".* c (.+) y.*", "\\1", df_bottleneck_res$`Total Duration2`[bottleneck_k])))){
        #     as.integer(gsub(".* c (.+) y.*", "\\1", df_bottleneck_res$`Total Duration2`[bottleneck_k]))
        #   } else if (!is.na(as.integer(gsub(" y.*", "\\1", df_bottleneck_res$`Total Duration2`[bottleneck_k])))) {
        #     as.integer(gsub(" y.*", "\\1", df_bottleneck_res$`Total Duration2`[bottleneck_k]))
        #   } else {
        #     0
        #   }
        
        # df_bottleneck_res$`Total Duration Month2`[bottleneck_k] <- 
        #   if(!is.na(as.integer(gsub(".* y (.+) m.*", "\\1", df_bottleneck_res$`Total Duration2`[bottleneck_k])))){
        #     as.integer(gsub(".* y (.+) m.*", "\\1", df_bottleneck_res$`Total Duration2`[bottleneck_k]))
        #   } else if (!is.na(as.integer(gsub(" m.*", "\\1", df_bottleneck_res$`Total Duration2`[bottleneck_k])))) {
        #     as.integer(gsub(" m.*", "\\1", df_bottleneck_res$`Total Duration2`[bottleneck_k]))
        #   } else {
        #     0
        #   }
        df_bottleneck_res$`Total Duration Day2`[bottleneck_k] <- 
          if(!is.na(as.integer(gsub(".* m (.+) d.*", "\\1", df_bottleneck_res$`Total Duration2`[bottleneck_k])))){
            as.integer(gsub(".* m (.+) d.*", "\\1", df_bottleneck_res$`Total Duration2`[bottleneck_k]))
          } else if (!is.na(as.integer(gsub(" d.*", "\\1", df_bottleneck_res$`Total Duration2`[bottleneck_k])))) {
            as.integer(gsub(" d.*", "\\1", df_bottleneck_res$`Total Duration2`[bottleneck_k]))
          } else {
            0
          }
        df_bottleneck_res$`Total Duration Hour2`[bottleneck_k] <- 
          if(!is.na(as.integer(gsub(".* d (.+) h.*", "\\1", df_bottleneck_res$`Total Duration2`[bottleneck_k])))){
            as.integer(gsub(".* d (.+) h.*", "\\1", df_bottleneck_res$`Total Duration2`[bottleneck_k]))
          } else if (!is.na(as.integer(gsub(" h.*", "\\1", df_bottleneck_res$`Total Duration2`[bottleneck_k])))) {
            as.integer(gsub(" h.*", "\\1", df_bottleneck_res$`Total Duration2`[bottleneck_k]))
          } else {
            0
          }
        df_bottleneck_res$`Total Duration Minute2`[bottleneck_k] <- 
          if(!is.na(as.integer(gsub(".* h (.+) m.*", "\\1", df_bottleneck_res$`Total Duration2`[bottleneck_k])))){
            as.integer(gsub(".* h (.+) m.*", "\\1", df_bottleneck_res$`Total Duration2`[bottleneck_k]))
          } else if (!is.na(as.integer(gsub(" m.*", "\\1", df_bottleneck_res$`Total Duration2`[bottleneck_k])))) {
            as.integer(gsub(" m.*", "\\1", df_bottleneck_res$`Total Duration2`[bottleneck_k]))
          } else {
            0
          }
        # df_bottleneck_res$`Average Daily Duration Year1`[bottleneck_k] <- 
        #   if(!is.na(as.integer(gsub(".* c (.+) y.*", "\\1", df_bottleneck_res$`Average Daily Duration1`[bottleneck_k])))){
        #     as.integer(gsub(".* c (.+) y.*", "\\1", df_bottleneck_res$`Average Daily Duration1`[bottleneck_k]))
        #   } else if (!is.na(as.integer(gsub(" y.*", "\\1", df_bottleneck_res$`Average Daily Duration1`[bottleneck_k])))) {
        #     as.integer(gsub(" y.*", "\\1", df_bottleneck_res$`Average Daily Duration1`[bottleneck_k]))
        #   } else {
        #     0
        #   }
        # df_bottleneck_res$`Average Daily Duration Month1`[bottleneck_k] <- 
        #   if(!is.na(as.integer(gsub(".* y (.+) m.*", "\\1", df_bottleneck_res$`Average Daily Duration1`[bottleneck_k])))){
        #     as.integer(gsub(".* y (.+) m.*", "\\1", df_bottleneck_res$`Average Daily Duration1`[bottleneck_k]))
        #   } else if (!is.na(as.integer(gsub(" m.*", "\\1", df_bottleneck_res$`Average Daily Duration1`[bottleneck_k])))) {
        #     as.integer(gsub(" m.*", "\\1", df_bottleneck_res$`Average Daily Duration1`[bottleneck_k]))
        #   } else {
        #     0
        #   }
        df_bottleneck_res$`Average Daily Duration Day1`[bottleneck_k] <- 
          if(!is.na(as.integer(gsub(".* m (.+) d.*", "\\1", df_bottleneck_res$`Average Daily Duration1`[bottleneck_k])))){
            as.integer(gsub(".* m (.+) d.*", "\\1", df_bottleneck_res$`Average Daily Duration1`[bottleneck_k]))
          } else if (!is.na(as.integer(gsub(" d.*", "\\1", df_bottleneck_res$`Average Daily Duration1`[bottleneck_k])))) {
            as.integer(gsub(" d.*", "\\1", df_bottleneck_res$`Average Daily Duration1`[bottleneck_k]))
          } else {
            0
          }
        df_bottleneck_res$`Average Daily Duration Hour1`[bottleneck_k] <- 
          if(!is.na(as.integer(gsub(".* d (.+) h.*", "\\1", df_bottleneck_res$`Average Daily Duration1`[bottleneck_k])))){
            as.integer(gsub(".* d (.+) h.*", "\\1", df_bottleneck_res$`Average Daily Duration1`[bottleneck_k]))
          } else if (!is.na(as.integer(gsub(" h.*", "\\1", df_bottleneck_res$`Average Daily Duration1`[bottleneck_k])))) {
            as.integer(gsub(" h.*", "\\1", df_bottleneck_res$`Average Daily Duration1`[bottleneck_k]))
          } else {
            0
          }
        df_bottleneck_res$`Average Daily Duration Minute1`[bottleneck_k] <- 
          if(!is.na(as.integer(gsub(".* h (.+) m.*", "\\1", df_bottleneck_res$`Average Daily Duration1`[bottleneck_k])))){
            as.integer(gsub(".* h (.+) m.*", "\\1", df_bottleneck_res$`Average Daily Duration1`[bottleneck_k]))
          } else if (!is.na(as.integer(gsub(" m.*", "\\1", df_bottleneck_res$`Average Daily Duration1`[bottleneck_k])))) {
            as.integer(gsub(" m.*", "\\1", df_bottleneck_res$`Average Daily Duration1`[bottleneck_k]))
          } else {
            0
          }
        # df_bottleneck_res$`Average Daily Duration Year2`[bottleneck_k] <- 
        #   if(!is.na(as.integer(gsub(".* c (.+) y.*", "\\1", df_bottleneck_res$`Average Daily Duration2`[bottleneck_k])))){
        #     as.integer(gsub(".* c (.+) y.*", "\\1", df_bottleneck_res$`Average Daily Duration2`[bottleneck_k]))
        #   } else if (!is.na(as.integer(gsub(" y.*", "\\1", df_bottleneck_res$`Average Daily Duration2`[bottleneck_k])))) {
        #     as.integer(gsub(" y.*", "\\1", df_bottleneck_res$`Average Daily Duration2`[bottleneck_k]))
        #   } else {
        #     0
        #   }
        
        # df_bottleneck_res$`Average Daily Duration Month2`[bottleneck_k] <- 
        #   if(!is.na(as.integer(gsub(".* y (.+) m.*", "\\1", df_bottleneck_res$`Average Daily Duration2`[bottleneck_k])))){
        #     as.integer(gsub(".* y (.+) m.*", "\\1", df_bottleneck_res$`Average Daily Duration2`[bottleneck_k]))
        #   } else if (!is.na(as.integer(gsub(" m.*", "\\1", df_bottleneck_res$`Average Daily Duration2`[bottleneck_k])))) {
        #     as.integer(gsub(" m.*", "\\1", df_bottleneck_res$`Average Daily Duration2`[bottleneck_k]))
        #   } else {
        #     0
        #   }
        df_bottleneck_res$`Average Daily Duration Day2`[bottleneck_k] <- 
          if(!is.na(as.integer(gsub(".* m (.+) d.*", "\\1", df_bottleneck_res$`Average Daily Duration2`[bottleneck_k])))){
            as.integer(gsub(".* m (.+) d.*", "\\1", df_bottleneck_res$`Average Daily Duration2`[bottleneck_k]))
          } else if (!is.na(as.integer(gsub(" d.*", "\\1", df_bottleneck_res$`Average Daily Duration2`[bottleneck_k])))) {
            as.integer(gsub(" d.*", "\\1", df_bottleneck_res$`Average Daily Duration2`[bottleneck_k]))
          } else {
            0
          }
        df_bottleneck_res$`Average Daily Duration Hour2`[bottleneck_k] <- 
          if(!is.na(as.integer(gsub(".* d (.+) h.*", "\\1", df_bottleneck_res$`Average Daily Duration2`[bottleneck_k])))){
            as.integer(gsub(".* d (.+) h.*", "\\1", df_bottleneck_res$`Average Daily Duration2`[bottleneck_k]))
          } else if (!is.na(as.integer(gsub(" h.*", "\\1", df_bottleneck_res$`Average Daily Duration2`[bottleneck_k])))) {
            as.integer(gsub(" h.*", "\\1", df_bottleneck_res$`Average Daily Duration2`[bottleneck_k]))
          } else {
            0
          }
        df_bottleneck_res$`Average Daily Duration Minute2`[bottleneck_k] <- 
          if(!is.na(as.integer(gsub(".* h (.+) m.*", "\\1", df_bottleneck_res$`Average Daily Duration2`[bottleneck_k])))){
            as.integer(gsub(".* h (.+) m.*", "\\1", df_bottleneck_res$`Average Daily Duration2`[bottleneck_k]))
          } else if (!is.na(as.integer(gsub(" m.*", "\\1", df_bottleneck_res$`Average Daily Duration2`[bottleneck_k])))) {
            as.integer(gsub(" m.*", "\\1", df_bottleneck_res$`Average Daily Duration2`[bottleneck_k]))
          } else {
            0
          }
      } 
      df_bottleneck_res$Total_Duration1_Hours <- round(df_bottleneck_res$`Total Duration Day1`*24 + df_bottleneck_res$`Total Duration Hour1` + df_bottleneck_res$`Total Duration Minute1`/60,2)
      df_bottleneck_res$Total_Duration2_Hours <- round(df_bottleneck_res$`Total Duration Day2`*24 + df_bottleneck_res$`Total Duration Hour2` + df_bottleneck_res$`Total Duration Minute2`/60,2)
      df_bottleneck_res$Average_Duration1_Hours <- round(df_bottleneck_res$`Average Daily Duration Day1`*24 + df_bottleneck_res$`Average Daily Duration Hour1` + df_bottleneck_res$`Average Daily Duration Minute1`/60,2)
      df_bottleneck_res$Average_Duration2_Hours <- round(df_bottleneck_res$`Average Daily Duration Day2`*24 + df_bottleneck_res$`Average Daily Duration Hour2` + df_bottleneck_res$`Average Daily Duration Minute2`/60,2)
      df_bottleneck_res$`Total Duration Day1` <- NULL
      df_bottleneck_res$`Total Duration Day2` <- NULL
      df_bottleneck_res$`Total Duration Hour1` <- NULL
      df_bottleneck_res$`Total Duration Hour2` <- NULL
      df_bottleneck_res$`Total Duration Minute1` <- NULL
      df_bottleneck_res$`Total Duration Minute2` <- NULL
      df_bottleneck_res$`Average Daily Duration Day1` <- NULL
      df_bottleneck_res$`Average Daily Duration Day2` <- NULL
      df_bottleneck_res$`Average Daily Duration Hour1` <- NULL
      df_bottleneck_res$`Average Daily Duration Hour2` <- NULL
      df_bottleneck_res$`Average Daily Duration Minute1` <- NULL
      df_bottleneck_res$`Average Daily Duration Minute2` <- NULL
      df_bottleneck_res$`Starting TMC Code1`[is.na(df_bottleneck_res$`Starting TMC Code1`)] <- df_bottleneck_res$`Starting TMC Code2`[is.na(df_bottleneck_res$`Starting TMC Code1`)]
      df_bottleneck_res$Longitude1[is.na(df_bottleneck_res$Longitude1)] <- df_bottleneck_res$Longitude2[is.na(df_bottleneck_res$Longitude1)]
      df_bottleneck_res$Latitude1[is.na(df_bottleneck_res$Latitude1)] <- df_bottleneck_res$Latitude2[is.na(df_bottleneck_res$Latitude1)]
      df_bottleneck_res$`Average Max Length1`[is.na(df_bottleneck_res$`Average Max Length1`)] <- 0
      # df_bottleneck_res$`Agency-Reported Events1`[is.na(df_bottleneck_res$`Agency-Reported Events1`)] <- df_bottleneck_res$`Agency-Reported Events2`[is.na(df_bottleneck_res$`Agency-Reported Events1`)]
      # df_bottleneck_res$`Volume Estimate1`[is.na(df_bottleneck_res$`Volume Estimate1`)] <- df_bottleneck_res$`Volume Estimate2`[is.na(df_bottleneck_res$`Volume Estimate1`)]
      # df_bottleneck_res$`Base Impact1`[is.na(df_bottleneck_res$`Base Impact1`)] <- df_bottleneck_res$`Base Impact2`[is.na(df_bottleneck_res$`Base Impact1`)]
      # df_bottleneck_res$`Speed Differential1`[is.na(df_bottleneck_res$`Speed Differential1`)] <- df_bottleneck_res$`Speed Differential2`[is.na(df_bottleneck_res$`Speed Differential1`)]
      # df_bottleneck_res$Congestion1[is.na(df_bottleneck_res$Congestion1)] <- df_bottleneck_res$Congestion2[is.na(df_bottleneck_res$Congestion1)]
      # df_bottleneck_res$`Total Delay1`[is.na(df_bottleneck_res$`Total Delay1`)] <- df_bottleneck_res$`Total Delay2`[is.na(df_bottleneck_res$`Total Delay1`)] 
      df_bottleneck_res$`Segment IDs1`[is.na(df_bottleneck_res$`Segment IDs1`)] <- df_bottleneck_res$`Segment IDs2`[is.na(df_bottleneck_res$`Segment IDs1`)]
      df_bottleneck_res$`Starting TMC Code2`[is.na(df_bottleneck_res$`Starting TMC Code2`)] <- df_bottleneck_res$`Starting TMC Code1`[is.na(df_bottleneck_res$`Starting TMC Code2`)]
      df_bottleneck_res$Longitude2[is.na(df_bottleneck_res$Longitude2)] <- df_bottleneck_res$Longitude1[is.na(df_bottleneck_res$Longitude2)]
      df_bottleneck_res$Latitude2[is.na(df_bottleneck_res$Latitude2)] <- df_bottleneck_res$Latitude1[is.na(df_bottleneck_res$Latitude2)]
      df_bottleneck_res$`Average Max Length2`[is.na(df_bottleneck_res$`Average Max Length2`)] <- 0
      df_bottleneck_res$`Segment IDs2`[is.na(df_bottleneck_res$`Segment IDs2`)] <- df_bottleneck_res$`Segment IDs1`[is.na(df_bottleneck_res$`Segment IDs2`)]
      df_bottleneck_res$percent_total_duration <- round((df_bottleneck_res$Total_Duration2_Hours - df_bottleneck_res$Total_Duration1_Hours)/df_bottleneck_res$Total_Duration1_Hours*100,2)
      df_bottleneck_res$percent_total_duration[df_bottleneck_res$percent_total_duration =="Inf"] <- 100
      df_bottleneck_res$percent_total_duration[df_bottleneck_res$percent_total_duration =="NaN"] <- 0
      df_bottleneck_res$percent_avg_daily_duration <- round((df_bottleneck_res$Average_Duration2_Hours - df_bottleneck_res$Average_Duration1_Hours)/df_bottleneck_res$Average_Duration1_Hours*100,2)
      
      df_bottleneck_res$percent_Agency_Reported_Events <- round((df_bottleneck_res$`Agency-Reported Events2` - df_bottleneck_res$`Agency-Reported Events1`)/df_bottleneck_res$`Agency-Reported Events1`*100,2)
      df_bottleneck_res$percent_Volume_Estimate <- round((df_bottleneck_res$`Volume Estimate2` - df_bottleneck_res$`Volume Estimate1`)/df_bottleneck_res$`Volume Estimate1`*100,2)
      df_bottleneck_res$percent_Base_Impact <- round((df_bottleneck_res$`Base Impact2` - df_bottleneck_res$`Base Impact1`)/df_bottleneck_res$`Base Impact1`*100,2)
      df_bottleneck_res$percent_Speed_Differential <- round((df_bottleneck_res$`Speed Differential2` - df_bottleneck_res$`Speed Differential1`)/df_bottleneck_res$`Speed Differential1`*100,2)
      df_bottleneck_res$percent_Congestion <- round((df_bottleneck_res$Congestion2 - df_bottleneck_res$Congestion1)/df_bottleneck_res$Congestion1*100,2)
      df_bottleneck_res$percent_Total_Delay <- round((df_bottleneck_res$`Total Delay2` - df_bottleneck_res$`Total Delay1`)/df_bottleneck_res$`Total Delay1`*100,2)
      
      
      df_bottleneck_res$percent_avg_daily_duration[df_bottleneck_res$percent_avg_daily_duration =="Inf"] <- 100
      df_bottleneck_res$percent_avg_daily_duration[df_bottleneck_res$percent_avg_daily_duration =="NaN"] <- 0
      df_bottleneck_res$percent_Agency_Reported_Events[df_bottleneck_res$percent_Agency_Reported_Events =="Inf"] <- 100
      df_bottleneck_res$percent_Agency_Reported_Events[df_bottleneck_res$percent_Agency_Reported_Events =="NaN"] <- 0
      df_bottleneck_res$percent_Volume_Estimate[df_bottleneck_res$percent_Volume_Estimate =="Inf"] <- 100
      df_bottleneck_res$percent_Volume_Estimate[df_bottleneck_res$percent_Volume_Estimate =="NaN"] <- 0
      df_bottleneck_res$percent_Base_Impact[df_bottleneck_res$percent_Base_Impact =="Inf"] <- 100
      df_bottleneck_res$percent_Base_Impact[df_bottleneck_res$percent_Base_Impact =="NaN"] <- 0
      df_bottleneck_res$percent_Speed_Differential[df_bottleneck_res$percent_Speed_Differential =="Inf"] <- 100
      df_bottleneck_res$percent_Speed_Differential[df_bottleneck_res$percent_Speed_Differential =="NaN"] <- 0
      df_bottleneck_res$percent_Congestion[df_bottleneck_res$percent_Congestion =="Inf"] <- 100
      df_bottleneck_res$percent_Congestion[df_bottleneck_res$percent_Congestion =="NaN"] <- 0
      df_bottleneck_res$percent_Total_Delay[df_bottleneck_res$percent_Total_Delay =="Inf"] <- 100
      df_bottleneck_res$percent_Total_Delay[df_bottleneck_res$percent_Total_Delay =="NaN"] <- 0
      
      df_bottleneck_res$bottleneck_type <- "common"
      df_bottleneck_res$bottleneck_type[is.na(df_bottleneck_res$Rank1)] <- "new"
      df_bottleneck_res$bottleneck_type[is.na(df_bottleneck_res$Rank2)] <- "old"
      df_bottleneck_res[is.na(df_bottleneck_res)] <- 0
      colnames(df_bottleneck_res) <- c("HeadLocation","Rank1","Starting_TMC_Code1","Latitude1",
                                                 "Longitude1","Average_Max_Length1","Average_Daily_Duration1","Total_Duration1",
                                                 "Agency_Reported_Events1","Volume_Estimate1","Base_Impact1","Speed_Differential1",
                                                 "Congestion1","Total_Delay1","Segment_IDs1","Rank2","Starting_TMC_Code2","Latitude2",
                                                 "Longitude2","Average_Max_Length2","Average_Daily_Duration2","Total_Duration2",
                                                 "Agency_Reported_Events2","Volume_Estimate2","Base_Impact2","Speed_Differential2",
                                                 "Congestion2","Total_Delay2","Segment_IDs2","Total_Duration1_Hours","Total_Duration2_Hours",
                                                 "Average_Duration1_Hours","Average_Duration2_Hours", "percent_total_duration", 
                                                 "percent_avg_daily_duration","percent_Agency_Reported_Events","percent_Volume_Estimate",
                                                 "percent_Base_Impact","percent_Speed_Differential","percent_Congestion",
                                                 "percent_Total_Delay","bottleneck_type")
      
      if(input$bottleneck_ranking_threshold_1 == "total_duration"){
          df_bottleneck_res$Rank1 <- order(df_bottleneck_res$Total_Duration1_Hours,decreasing = TRUE)
          df_bottleneck_res$Rank2 <- order(df_bottleneck_res$Total_Duration2_Hours,decreasing = TRUE)
        } else if (input$bottleneck_ranking_threshold_1 == "avg_duration"){
          df_bottleneck_res$Rank1 <- order(df_bottleneck_res$Average_Duration1_Hours,decreasing = TRUE)
          df_bottleneck_res$Rank2 <- order(df_bottleneck_res$Average_Duration2_Hours,decreasing = TRUE)
        } else if (input$bottleneck_ranking_threshold_1 == "agency_reported_events"){
          df_bottleneck_res$Rank1 <- order(df_bottleneck_res$Agency_Reported_Events1,decreasing = TRUE)
          df_bottleneck_res$Rank2 <- order(df_bottleneck_res$Agency_Reported_Events2,decreasing = TRUE)
        } else if (input$bottleneck_ranking_threshold_1 == "volume_estimate"){
          df_bottleneck_res$Rank1 <- order(df_bottleneck_res$Volume_Estimate1,decreasing = TRUE)
          df_bottleneck_res$Rank2 <- order(df_bottleneck_res$Volume_Estimate2,decreasing = TRUE)
        } else if (input$bottleneck_ranking_threshold_1 == "base_impact"){
          df_bottleneck_res$Rank1 <- order(df_bottleneck_res$Base_Impact1,decreasing = TRUE)
          df_bottleneck_res$Rank2 <- order(df_bottleneck_res$Base_Impact2,decreasing = TRUE)
        } else if (input$bottleneck_ranking_threshold_1 == "speed_differential"){
          df_bottleneck_res$Rank1 <- order(df_bottleneck_res$Speed_Differential1,decreasing = TRUE)
          df_bottleneck_res$Rank2 <- order(df_bottleneck_res$Speed_Differential2,decreasing = TRUE)
        } else if (input$bottleneck_ranking_threshold_1 == "congestion"){
          df_bottleneck_res$Rank1 <- order(df_bottleneck_res$Congestion1,decreasing = TRUE)
          df_bottleneck_res$Rank2 <- order(df_bottleneck_res$Congestion2,decreasing = TRUE)
        } else if (input$bottleneck_ranking_threshold_1 == "total_delay"){
          df_bottleneck_res$Rank1 <- order(df_bottleneck_res$Total_Duration1,decreasing = TRUE)
          df_bottleneck_res$Rank2 <- order(df_bottleneck_res$Total_Duration2,decreasing = TRUE)
        }
        
      
      return(df_bottleneck_res)
    }
    results_bottleneck_analysis <- bottleneck_analysis(input_file1_bottleneck,input_file2_bottleneck)
    # as.numeric(input$bottleneck_avg_daily_threshold)
    # as.numeric(input$bottleneck_total_threshold)
    results_bottleneck_analysis <- results_bottleneck_analysis[(((results_bottleneck_analysis$Average_Duration1_Hours >= as.numeric(input$bottleneck_avg_daily_threshold) & results_bottleneck_analysis$Average_Duration2_Hours >= as.numeric(input$bottleneck_avg_daily_threshold)) & results_bottleneck_analysis$bottleneck_type =="common") | ((results_bottleneck_analysis$Average_Duration1_Hours >= as.numeric(input$bottleneck_avg_daily_threshold) | results_bottleneck_analysis$Average_Duration2_Hours >= as.numeric(input$bottleneck_avg_daily_threshold)) & results_bottleneck_analysis$bottleneck_type !="common")),]
    results_bottleneck_analysis <- results_bottleneck_analysis[(((results_bottleneck_analysis$Total_Duration1_Hours >= as.numeric(input$bottleneck_total_threshold) & results_bottleneck_analysis$Total_Duration2_Hours >= as.numeric(input$bottleneck_total_threshold)) & results_bottleneck_analysis$bottleneck_type =="common") | ((results_bottleneck_analysis$Total_Duration1_Hours >= as.numeric(input$bottleneck_total_threshold) | results_bottleneck_analysis$Total_Duration2_Hours >= as.numeric(input$bottleneck_total_threshold)) & results_bottleneck_analysis$bottleneck_type !="common")),]
    results_bottleneck_analysis <- results_bottleneck_analysis[(((results_bottleneck_analysis$Agency_Reported_Events1 >= as.numeric(input$bottleneck_agency_reported_events_threshold) & results_bottleneck_analysis$Agency_Reported_Events2 >= as.numeric(input$bottleneck_agency_reported_events_threshold)) & results_bottleneck_analysis$bottleneck_type =="common") | ((results_bottleneck_analysis$Agency_Reported_Events1 >= as.numeric(input$bottleneck_agency_reported_events_threshold) | results_bottleneck_analysis$Agency_Reported_Events2 >= as.numeric(input$bottleneck_agency_reported_events_threshold)) & results_bottleneck_analysis$bottleneck_type !="common")),]
    results_bottleneck_analysis <- results_bottleneck_analysis[(((results_bottleneck_analysis$Volume_Estimate1 >= as.numeric(input$bottleneck_volume_estimate_threshold) & results_bottleneck_analysis$Volume_Estimate2 >= as.numeric(input$bottleneck_volume_estimate_threshold)) & results_bottleneck_analysis$bottleneck_type =="common") | ((results_bottleneck_analysis$Volume_Estimate1 >= as.numeric(input$bottleneck_volume_estimate_threshold) | results_bottleneck_analysis$Volume_Estimate2 >= as.numeric(input$bottleneck_volume_estimate_threshold)) & results_bottleneck_analysis$bottleneck_type !="common")),]
    results_bottleneck_analysis <- results_bottleneck_analysis[(((results_bottleneck_analysis$Base_Impact1 >= as.numeric(input$bottleneck_base_impact_threshold) & results_bottleneck_analysis$Base_Impact2 >= as.numeric(input$bottleneck_base_impact_threshold)) & results_bottleneck_analysis$bottleneck_type =="common") | ((results_bottleneck_analysis$Base_Impact1 >= as.numeric(input$bottleneck_base_impact_threshold) | results_bottleneck_analysis$Base_Impact2 >= as.numeric(input$bottleneck_base_impact_threshold)) & results_bottleneck_analysis$bottleneck_type !="common")),]
    results_bottleneck_analysis <- results_bottleneck_analysis[(((results_bottleneck_analysis$Speed_Differential1 >= as.numeric(input$bottleneck_speed_differential_threshold) & results_bottleneck_analysis$Speed_Differential2 >= as.numeric(input$bottleneck_speed_differential_threshold)) & results_bottleneck_analysis$bottleneck_type =="common") | ((results_bottleneck_analysis$Speed_Differential1 >= as.numeric(input$bottleneck_speed_differential_threshold) | results_bottleneck_analysis$Speed_Differential2 >= as.numeric(input$bottleneck_speed_differential_threshold)) & results_bottleneck_analysis$bottleneck_type !="common")),]
    results_bottleneck_analysis <- results_bottleneck_analysis[(((results_bottleneck_analysis$Congestion1 >= as.numeric(input$bottleneck_congestion_threshold) & results_bottleneck_analysis$Congestion2 >= as.numeric(input$bottleneck_congestion_threshold)) & results_bottleneck_analysis$bottleneck_type =="common") | ((results_bottleneck_analysis$Congestion1 >= as.numeric(input$bottleneck_congestion_threshold) | results_bottleneck_analysis$Congestion2 >= as.numeric(input$bottleneck_congestion_threshold)) & results_bottleneck_analysis$bottleneck_type !="common")),]
    results_bottleneck_analysis <- results_bottleneck_analysis[(((results_bottleneck_analysis$Total_Delay1 >= as.numeric(input$bottleneck_total_delay_threshold) & results_bottleneck_analysis$Total_Delay2 >= as.numeric(input$bottleneck_total_delay_threshold)) & results_bottleneck_analysis$bottleneck_type =="common") | ((results_bottleneck_analysis$Total_Delay1 >= as.numeric(input$bottleneck_total_delay_threshold) | results_bottleneck_analysis$Total_Delay2 >= as.numeric(input$bottleneck_total_delay_threshold)) & results_bottleneck_analysis$bottleneck_type !="common")),]
    
    
    results_bottleneck_analysis1 <- results_bottleneck_analysis[,c("HeadLocation","Rank1","Starting_TMC_Code1","Latitude1",
                                                                   "Longitude1","Average_Max_Length1","Average_Daily_Duration1","Total_Duration1",
                                                                   "Agency_Reported_Events1","Volume_Estimate1","Base_Impact1","Speed_Differential1",
                                                                   "Congestion1","Total_Delay1","Segment_IDs1","Total_Duration1_Hours",
                                                                   "Average_Duration1_Hours","bottleneck_type")]
    results_bottleneck_analysis2 <- results_bottleneck_analysis[,c("HeadLocation","Rank2","Starting_TMC_Code2","Latitude2",
                                                                   "Longitude2","Average_Max_Length2","Average_Daily_Duration2","Total_Duration2",
                                                                   "Agency_Reported_Events2","Volume_Estimate2","Base_Impact2","Speed_Differential2",
                                                                   "Congestion2","Total_Delay2","Segment_IDs2","Total_Duration2_Hours",
                                                                   "Average_Duration2_Hours","bottleneck_type")]
    colnames(results_bottleneck_analysis1) <- c("HeadLocation","Rank","Starting_TMC_Code","Latitude",
                                                "Longitude","Average_Max_Length","Average_Daily_Duration","Total_Duration",
                                                "Agency_Reported_Events","Volume_Estimate","Base_Impact","Speed_Differential",
                                                "Congestion","Total_Delay","Segment_IDs","Total_Duration_Hours",
                                                "Average_Duration_Hours","bottleneck_type")
    results_bottleneck_analysis1$time_type <- "before"
    colnames(results_bottleneck_analysis2) <- c("HeadLocation","Rank","Starting_TMC_Code","Latitude",
                                                "Longitude","Average_Max_Length","Average_Daily_Duration","Total_Duration",
                                                "Agency_Reported_Events","Volume_Estimate","Base_Impact","Speed_Differential",
                                                "Congestion","Total_Delay","Segment_IDs","Total_Duration_Hours",
                                                "Average_Duration_Hours","bottleneck_type")
    results_bottleneck_analysis2$time_type <- "after"
    results_bottleneck_analysis_pivot <- rbind(results_bottleneck_analysis1,results_bottleneck_analysis2)
    results_bottleneck_analysis_pivot <- results_bottleneck_analysis_pivot[,c("Rank","Average_Max_Length","Agency_Reported_Events"
                                                                              ,"Volume_Estimate","Base_Impact","Speed_Differential"
                                                                              ,"Congestion","Total_Delay","Total_Duration_Hours"
                                                                              ,"Average_Duration_Hours"
                                                                              )]
    results_bottleneck_analysis_pivot$Base_Impact <- results_bottleneck_analysis_pivot$Base_Impact/1000
    results_bottleneck_analysis_pivot$Volume_Estimate <- results_bottleneck_analysis_pivot$Volume_Estimate/1000
    results_bottleneck_analysis_pivot$Speed_Differential <- results_bottleneck_analysis_pivot$Speed_Differential/1000
    results_bottleneck_analysis_pivot$Congestion <- results_bottleneck_analysis_pivot$Congestion/1000
    results_bottleneck_analysis_pivot$Total_Delay <- results_bottleneck_analysis_pivot$Total_Delay/1000000
    
    
    # results_bottleneck_analysis_pivot <- results_bottleneck_analysis_pivot[,c("Rank","Average_Max_Length","Agency_Reported_Events"
    #                                                                           ,"Total_Duration_Hours"
    #                                                                           ,"Average_Duration_Hours")]
    # results_bottleneck_analysis_pivot$Rank <- NULL
    p <-  as.list(NULL)
    p <-  lapply(1:(nrow(results_bottleneck_analysis_pivot)/2), function(dd_i) {
      #   clr[i] <- "dark green"
      #   update(p[[i]], col = clr)
      # })
    bargraph_df_bottleneck1 <- data.frame(t(results_bottleneck_analysis_pivot[dd_i,]))
    bargraph_df_bottleneck2 <- data.frame(t(results_bottleneck_analysis_pivot[nrow(results_bottleneck_analysis)+dd_i,]))
    colnames(bargraph_df_bottleneck1) <- "performance_idx_value"
    colnames(bargraph_df_bottleneck2) <- "performance_idx_value"
    bargraph_df_bottleneck1$Analysis_type <- rownames(bargraph_df_bottleneck1)
    rownames(bargraph_df_bottleneck1) <- c(1:nrow(bargraph_df_bottleneck1))
    bargraph_df_bottleneck2$Analysis_type <- rownames(bargraph_df_bottleneck2)
    rownames(bargraph_df_bottleneck2) <- c(1:nrow(bargraph_df_bottleneck2))
    
    bargraph_df_bottleneck <- rbind(bargraph_df_bottleneck1,bargraph_df_bottleneck2)
    bargraph_df_bottleneck$time_type <- c(rep("before",nrow(bargraph_df_bottleneck)/2),rep("after",nrow(bargraph_df_bottleneck)/2))
    p[[dd_i]] <- ggplot(data=bargraph_df_bottleneck, aes(x=Analysis_type, y=performance_idx_value, fill=factor(time_type,levels = c("before", "after")))) +
      geom_bar(stat="identity", position=position_dodge()) +
      geom_text(aes(label = performance_idx_value), position = position_dodge(width = .9)) +
      ggtitle(paste0("Analysis Results for ",results_bottleneck_analysis1$HeadLocation[dd_i])) + 
      labs(y= "Perfromance Measurement Value (Agency Reported Events, Avg Duration(hr), Avg Max Length(mi), '\n' Base Impact(K), Congestion(K), Rank, Speed Differential (K), Total Delay (Million), '\n' Total Duration (hr), Volume Estimates (K))", x = "Performance Measurement") +
      theme_calc()
    })
    
    
    results_bottleneck_analysis_pivot_overall <- results_bottleneck_analysis_pivot
    # results_bottleneck_analysis_pivot_overall$Rank_Count[1:(nrow(results_bottleneck_analysis_pivot_overall)/2)] <- length(results_bottleneck_analysis_pivot_overall$Rank[1:(nrow(results_bottleneck_analysis_pivot_overall)/2)][results_bottleneck_analysis_pivot_overall$Rank[1:(nrow(results_bottleneck_analysis_pivot_overall)/2)] != 0])
    # results_bottleneck_analysis_pivot_overall$Rank_Count[(nrow(results_bottleneck_analysis_pivot_overall)/2):(nrow(results_bottleneck_analysis_pivot_overall))] <- length(results_bottleneck_analysis_pivot_overall$Rank[(nrow(results_bottleneck_analysis_pivot_overall)/2):(nrow(results_bottleneck_analysis_pivot_overall))][results_bottleneck_analysis_pivot_overall$Rank[(nrow(results_bottleneck_analysis_pivot_overall)/2):(nrow(results_bottleneck_analysis_pivot_overall))] != 0])
    results_bottleneck_analysis_pivot_overall$Rank <- NULL
    # results_bottleneck_analysis_pivot_overall <- round(results_bottleneck_analysis_pivot_overall,2)
    
    bargraph_df_bottleneck1_avg <- data.frame(round(colSums(results_bottleneck_analysis_pivot_overall[1:(nrow(results_bottleneck_analysis_pivot_overall)/2),]),2))
    bargraph_df_bottleneck2_avg <- data.frame(round(colSums(results_bottleneck_analysis_pivot_overall[((nrow(results_bottleneck_analysis_pivot_overall)/2):nrow(results_bottleneck_analysis_pivot_overall)),]),2))
    colnames(bargraph_df_bottleneck1_avg) <- "performance_idx_value"
    colnames(bargraph_df_bottleneck2_avg) <- "performance_idx_value"
    bargraph_df_bottleneck1_avg$Analysis_type <- rownames(bargraph_df_bottleneck1_avg)
    rownames(bargraph_df_bottleneck1_avg) <- c(1:nrow(bargraph_df_bottleneck1_avg))
    bargraph_df_bottleneck2_avg$Analysis_type <- rownames(bargraph_df_bottleneck2_avg)
    rownames(bargraph_df_bottleneck2_avg) <- c(1:nrow(bargraph_df_bottleneck2_avg))
    bargraph_df_bottleneck_avg <- rbind(bargraph_df_bottleneck1_avg,bargraph_df_bottleneck2_avg)
    bargraph_df_bottleneck_avg$time_type <- c(rep("before",nrow(bargraph_df_bottleneck_avg)/2),rep("after",nrow(bargraph_df_bottleneck_avg)/2))
    
    
    # rownames(bargraph_df_bottleneck_avg[bargraph_df_bottleneck_avg$Analysis_type=="Rank_Count",])
    bargraph_df_bottleneck_avg$performance_idx_value[as.integer(rownames(bargraph_df_bottleneck_avg[bargraph_df_bottleneck_avg$Analysis_type=="Rank_Count",]))] <- as.integer(bargraph_df_bottleneck_avg$performance_idx_value[as.integer(rownames(bargraph_df_bottleneck_avg[bargraph_df_bottleneck_avg$Analysis_type=="Rank_Count",]))])
    p_overall <- ggplot(data=bargraph_df_bottleneck_avg, aes(x=Analysis_type, y=performance_idx_value, fill=factor(time_type,levels = c("before", "after")))) +
      geom_bar(stat="identity", position=position_dodge()) +
      geom_text(aes(label = performance_idx_value), position = position_dodge(width = .9)) +
      ggtitle(paste0("Total Results for overall bottlenecks on ",gsub(" .*", "\\1", results_bottleneck_analysis$HeadLocation)[1])) + 
      labs(y= "Perfromance Measurement Value (Agency Reported Events, Avg Duration(hr), Avg Max Length(mi), '\n' Base Impact(K), Congestion(K), Speed Differential (K), Total Delay (Million), '\n' Total Duration (hr), Volume Estimates (K))", x = "Performance Measurement") +
      theme_calc()
    
    results_bottleneck_analysis$percent_total_duration[results_bottleneck_analysis$percent_total_duration > 100] <- 100
    results_bottleneck_analysis$percent_avg_daily_duration[results_bottleneck_analysis$percent_avg_daily_duration > 100] <- 100
    results_bottleneck_analysis$percent_Agency_Reported_Events[results_bottleneck_analysis$percent_Agency_Reported_Events > 100] <- 100
    results_bottleneck_analysis$percent_Volume_Estimate[results_bottleneck_analysis$percent_Volume_Estimate > 100] <- 100
    results_bottleneck_analysis$percent_Base_Impact[results_bottleneck_analysis$percent_Base_Impact > 100] <- 100
    results_bottleneck_analysis$percent_Speed_Differential[results_bottleneck_analysis$percent_Speed_Differential > 100] <- 100
    results_bottleneck_analysis$percent_Congestion[results_bottleneck_analysis$percent_Congestion > 100] <- 100
    results_bottleneck_analysis$percent_Total_Delay[results_bottleneck_analysis$percent_Total_Delay > 100] <- 100
    
    if(input$bottleneck_ranking_threshold_1 == "total_duration"){
      results_bottleneck_analysis$bottleneck_type[(results_bottleneck_analysis$bottleneck_type=="common") &(results_bottleneck_analysis$percent_total_duration > 0)] <- "degradation"
      results_bottleneck_analysis$bottleneck_type[(results_bottleneck_analysis$bottleneck_type=="common") &(results_bottleneck_analysis$percent_total_duration <= 0)] <- "improvement"
    } else if (input$bottleneck_ranking_threshold_1 == "avg_duration"){
      results_bottleneck_analysis$bottleneck_type[(results_bottleneck_analysis$bottleneck_type=="common") &(results_bottleneck_analysis$percent_avg_daily_duration > 0)] <- "degradation"
      results_bottleneck_analysis$bottleneck_type[(results_bottleneck_analysis$bottleneck_type=="common") &(results_bottleneck_analysis$percent_avg_daily_duration <= 0)] <- "improvement"
    } else if (input$bottleneck_ranking_threshold_1 == "agency_reported_events"){
      results_bottleneck_analysis$bottleneck_type[(results_bottleneck_analysis$bottleneck_type=="common") &(results_bottleneck_analysis$percent_Agency_Reported_Events > 0)] <- "degradation"
      results_bottleneck_analysis$bottleneck_type[(results_bottleneck_analysis$bottleneck_type=="common") &(results_bottleneck_analysis$percent_Agency_Reported_Events <= 0)] <- "improvement"
    } else if (input$bottleneck_ranking_threshold_1 == "volume_estimate"){
      results_bottleneck_analysis$bottleneck_type[(results_bottleneck_analysis$bottleneck_type=="common") &(results_bottleneck_analysis$percent_Volume_Estimate > 0)] <- "degradation"
      results_bottleneck_analysis$bottleneck_type[(results_bottleneck_analysis$bottleneck_type=="common") &(results_bottleneck_analysis$percent_Volume_Estimate <= 0)] <- "improvement"
    } else if (input$bottleneck_ranking_threshold_1 == "base_impact"){
      results_bottleneck_analysis$bottleneck_type[(results_bottleneck_analysis$bottleneck_type=="common") &(results_bottleneck_analysis$percent_Base_Impact > 0)] <- "degradation"
      results_bottleneck_analysis$bottleneck_type[(results_bottleneck_analysis$bottleneck_type=="common") &(results_bottleneck_analysis$percent_Base_Impact <= 0)] <- "improvement"
    } else if (input$bottleneck_ranking_threshold_1 == "speed_differential"){
      results_bottleneck_analysis$bottleneck_type[(results_bottleneck_analysis$bottleneck_type=="common") &(results_bottleneck_analysis$percent_Speed_Differential > 0)] <- "degradation"
      results_bottleneck_analysis$bottleneck_type[(results_bottleneck_analysis$bottleneck_type=="common") &(results_bottleneck_analysis$percent_Speed_Differential <= 0)] <- "improvement"
    } else if (input$bottleneck_ranking_threshold_1 == "congestion"){
      results_bottleneck_analysis$bottleneck_type[(results_bottleneck_analysis$bottleneck_type=="common") &(results_bottleneck_analysis$percent_Congestion > 0)] <- "degradation"
      results_bottleneck_analysis$bottleneck_type[(results_bottleneck_analysis$bottleneck_type=="common") &(results_bottleneck_analysis$percent_Congestion <= 0)] <- "improvement"
    } else if (input$bottleneck_ranking_threshold_1 == "total_delay"){
      results_bottleneck_analysis$bottleneck_type[(results_bottleneck_analysis$bottleneck_type=="common") &(results_bottleneck_analysis$percent_Total_Delay > 0)] <- "degradation"
      results_bottleneck_analysis$bottleneck_type[(results_bottleneck_analysis$bottleneck_type=="common") &(results_bottleneck_analysis$percent_Total_Delay <= 0)] <- "improvement"
    }
    
    results_bottleneck_analysis <- results_bottleneck_analysis %>% group_by(bottleneck_type) %>% mutate(percent_total_duration_scaled = scale(percent_total_duration), 
                                                                                                        percent_avg_daily_duration_scaled = scale(percent_avg_daily_duration),
                                                                                                        percent_Agency_Reported_Events_scaled = scale(percent_Agency_Reported_Events),
                                                                                                        percent_Volume_Estimate_scaled = scale(percent_Volume_Estimate),
                                                                                                        percent_Base_Impact_scaled = scale(percent_Base_Impact),
                                                                                                        percent_Speed_Differential_scaled = scale(percent_Speed_Differential),
                                                                                                        percent_Congestion_scaled = scale(percent_Congestion),
                                                                                                        percent_Total_Delay_scaled = scale(percent_Total_Delay)
                                                                                                        )
    results_bottleneck_analysis$percent_total_duration_scaled[is.na(results_bottleneck_analysis$percent_total_duration_scaled)] <- 1
    results_bottleneck_analysis$percent_avg_daily_duration_scaled[is.na(results_bottleneck_analysis$percent_avg_daily_duration_scaled)] <- 1
    results_bottleneck_analysis$percent_Agency_Reported_Events_scaled[is.na(results_bottleneck_analysis$percent_Agency_Reported_Events_scaled)] <- 1
    results_bottleneck_analysis$percent_Volume_Estimate_scaled[is.na(results_bottleneck_analysis$percent_Volume_Estimate_scaled)] <- 1
    results_bottleneck_analysis$percent_Base_Impact_scaled[is.na(results_bottleneck_analysis$percent_Base_Impact_scaled)] <- 1
    results_bottleneck_analysis$percent_Speed_Differential_scaled[is.na(results_bottleneck_analysis$percent_Speed_Differential_scaled)] <- 1
    results_bottleneck_analysis$percent_Congestion_scaled[is.na(results_bottleneck_analysis$percent_Congestion_scaled)] <- 1
    results_bottleneck_analysis$percent_Total_Delay_scaled[is.na(results_bottleneck_analysis$percent_Total_Delay_scaled)] <- 1
    
    results_bottleneck_analysis$percent_total_duration_scaled <- as.numeric(abs(results_bottleneck_analysis$percent_total_duration_scaled))
    results_bottleneck_analysis$percent_avg_daily_duration_scaled <- as.numeric(abs(results_bottleneck_analysis$percent_avg_daily_duration_scaled))
    results_bottleneck_analysis$percent_Agency_Reported_Events_scaled <- as.numeric(abs(results_bottleneck_analysis$percent_Agency_Reported_Events_scaled))
    results_bottleneck_analysis$percent_Volume_Estimate_scaled <- as.numeric(abs(results_bottleneck_analysis$percent_Volume_Estimate_scaled))
    results_bottleneck_analysis$percent_Base_Impact_scaled <- as.numeric(abs(results_bottleneck_analysis$percent_Base_Impact_scaled))
    results_bottleneck_analysis$percent_Speed_Differential_scaled <- as.numeric(abs(results_bottleneck_analysis$percent_Speed_Differential_scaled))
    results_bottleneck_analysis$percent_Congestion_scaled <- as.numeric(abs(results_bottleneck_analysis$percent_Congestion_scaled))
    results_bottleneck_analysis$percent_Total_Delay_scaled <- as.numeric(abs(results_bottleneck_analysis$percent_Total_Delay_scaled))
    
    
    results_bottleneck_analysis$direction <- gsub(".* (.+) @ .*", "\\1", results_bottleneck_analysis$HeadLocation)
    results_bottleneck_analysis$route <- gsub(" .*", "\\1", results_bottleneck_analysis$HeadLocation)
    
    results_bottleneck_analysis$direction[results_bottleneck_analysis$direction=="S"] <- "Southbound"
    results_bottleneck_analysis$direction[results_bottleneck_analysis$direction=="E"] <- "Eastbound"
    results_bottleneck_analysis$direction[results_bottleneck_analysis$direction=="W"] <- "Westbound"
    results_bottleneck_analysis$direction[results_bottleneck_analysis$direction=="N"] <- "Northbound"
    
    pal <- colorFactor(palette = c("darkgreen", "darkred", "blue","orange"), 
                  levels = c("improvement", "degradation", "new","old"))
    pal_legend <- colorFactor(palette = c("darkgreen", "darkred", "blue","orange","black"), 
                              levels = c("Improved Bottlenecks", "Degraded Bottlenecks", "New Bottlenecks","Dissapearing Bottlenecks","Overall Average Results"))
    
    # results_bottleneck_analysis$percent_total_duration_scaled <- (results_bottleneck_analysis$percent_total_duration - min(results_bottleneck_analysis$percent_total_duration))/(max(results_bottleneck_analysis$percent_total_duration) - min(results_bottleneck_analysis$percent_total_duration))
    # results_bottleneck_analysis_zero_percent_total_duration_scaled <- results_bottleneck_analysis[results_bottleneck_analysis$percent_total_duration==0,]
    # results_bottleneck_analysis_improvement_percent_total_duration_scaled <- results_bottleneck_analysis[results_bottleneck_analysis$percent_total_duration<0,]
    # results_bottleneck_analysis_degradation_percent_total_duration_scaled <- results_bottleneck_analysis[results_bottleneck_analysis$percent_total_duration>0,]
    
    # Grouped Bar Plot
    # p <- lapply(1:length(results_bottleneck_analysis), function(i) {
    #   clr[i] <- "dark green"
    #   update(p[[i]], col = clr)
    # })
    # 
    
   

    # barplot(c(results_bottleneck_analysis$Total_Duration1_Hours, results_bottleneck_analysis$Total_Duration2_Hours)
    #          ,main="Total Duration before and after"
    #         ,xlab="Bottleneck", col=c("darkblue","red"),
    #         legend = c("before","after"), beside=TRUE)
    # chart list
    # p_imp <- as.list(NULL)
    # for (ii_b in (1:nrow(results_bottleneck_analysis_improvement_percent_total_duration_scaled))) {
    # print(ii_b)
    #   p_imp[[ii_b]] <- ggplot(barplot(c(results_bottleneck_analysis_improvement_percent_total_duration_scaled$Total_Duration1_Hours[ii_b], results_bottleneck_analysis_improvement_percent_total_duration_scaled$Total_Duration2_Hours[ii_b])
    #                             ,main="Total Duration before and after"
    #                             ,xlab="Bottleneck", col=c("darkblue","red"),
    #                             legend = c("before","after"), beside=TRUE))
    # }
    # p_deg <- as.list(NULL)
    # for (kk_b in (1:nrow(results_bottleneck_analysis_degradation_percent_total_duration_scaled))) {
    # print(kk_b)
    #   p_deg[[kk_b]] <- ggplot(barplot(c(results_bottleneck_analysis_degradation_percent_total_duration_scaled$Total_Duration1_Hours[kk_b], results_bottleneck_analysis_degradation_percent_total_duration_scaled$Total_Duration2_Hours[kk_b])
    #                             ,main="Total Duration before and after"
    #                             ,xlab="Bottleneck", col=c("darkblue","red"),
    #                             legend = c("before","after"), beside=TRUE))
    # }
    

    tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title {
    transform: translate(-50%,20%);
    position: fixed !sticky;
    left: 60%;
    text-align: center;
    padding-left: 10px;
    padding-right: 10px;
    background: rgba(120,120,120,0.75);
    font-weight: bold;
    font-size: 14px;
  }
"))
    
    from_period_name_file1 <- gsub(".* between (.+) and .*", "\\1",input$bottlneck_file1$name)
    to_period_name_file1 <- gsub(".* and (.+) \\(.*", "\\1",input$bottlneck_file1$name)
    from_period_name_file2 <- gsub(".* between (.+) and .*", "\\1",input$bottlneck_file2$name)
    to_period_name_file2 <- gsub(".* and (.+) \\(.*", "\\1",input$bottlneck_file2$name)
    
    title <- tags$div(
      tag.map.title, HTML(paste0("Bottleneck ranking analysis on ",unique(results_bottleneck_analysis$route)[1])," between the before period (from",from_period_name_file1," to ",to_period_name_file1,") and the after period (from",from_period_name_file2," to ",to_period_name_file2,")")
    )
    
    info_bottleneck_ranking <- 
      HTML(paste0("Base Impact — The sum of queue lengths over the duration of the bottleneck.", p(),
                     "Weighted Base Impact — The base impact weighted by speed differential, congestion or total delay - provide additional insight into the effects of bottlenecks on traffic in your area.", p(),
                     "Speed Differential — Base impact weighted by the difference between free-flow speed and observed speed. This metric should be used when you want to identify and rank bottlenecks from the individual vehicle perspective.", p(),
                     "Congestion — Base impact weighted by the measured speed as a percentage of free-flow speed. Similar to the speed differential metric, the congestion metric should be used when you want to identify and rank bottlenecks from the individual vehicle perspective.", p(),
                     "NOTE: The term congestion is defined as 'measured speed as a percent of the free-flow speed'", p(),
                    "Total Delay — Base impact weighted by the difference between free-flow travel time and observed travel time multiplied by the average daily volume (AADT), adjusted by a day-of-the-week factor. This metric should be used to rank and compare the estimated total delay from all vehicles within the bottleneck."
      )
    )
    
    # if(results_bottleneck_analysis$direction[1] == "Northbound" | results_bottleneck_analysis$direction[1] == "Southbound"){
    #   # IconSet_bottleneck_ranking1 <- leaflet::awesomeIconList(
    #   # "Northbound"   = makeAwesomeIcon(icon= 'glass', markerColor = 'blue', iconColor = 'black', library = "glyphicon"),
    #   # "Southbound" = makeAwesomeIcon(icon= 'fire', markerColor = 'black', iconColor = 'white', library = "glyphicon")
    #   # )
    #   groups_icon_bottleneck_ranking1 <- c("Northbound" <- "<div style='position: relative; display: inline-block' class='awesome-marker-icon-blue awesome-marker'><i class='ion chevron-up-circle icon-black '></i></div>Northbound",
    #               "Southbound" <- "<div style='position: relative; display: inline-block' class='awesome-marker-icon-black awesome-marker'><i class='ion chevron-down-circle icon-white '></i></div>Southbound")
    # } else {
    #   # IconSet_bottleneck_ranking1 <- leaflet::awesomeIconList(
    #   #   "Eastbound"   = makeAwesomeIcon(icon= 'glass', markerColor = 'blue', iconColor = 'black', library = "glyphicon"),
    #   #   "Westbound" = makeAwesomeIcon(icon= 'fire', markerColor = 'black', iconColor = 'white', library = "glyphicon")
    #   # )
    #   groups_icon_bottleneck_ranking1 <- c("Eastbound" <- "<div style='position: relative; display: inline-block' class='awesome-marker-icon-blue awesome-marker'><i class='glyphicon glyphicon-glass icon-black '></i></div>Northbound",
    #                                        "Westbound" <- "<div style='position: relative; display: inline-block' class='awesome-marker-icon-black awesome-marker'><i class='glyphicon glyphicon-fire icon-white '></i></div>Southbound")
    # }
    
    ID <- c('1','2','3','4','5','6','7','8','9','10','11','12','13','14','15','16','17','18','19','20','21','22','23','24','25','26','27','28','29','30','31','32','33','34','35','36','37','38','39','40','41','42','43','44','45','46','47','48','49','50','51','52','53','54','55','56','57','58','59','60','61','62','63','64','65','66','67','68','69','70','71','72','73','74','75','76','77','78','79','80','81','82','83','84','85','86','87','88','89','90','91','92','93','94')
    Site_ID <- c('1812','1811','1810','1809','1808','1807','1806','1805','1804','1803','1802','1801','1800','7300','7301','7302','7303','7305','7304','7306','7307','7308','7309','7310','7311','7312','7314','7315','7316','7317','7318','7319','7320','7321','7322','7323','7324','7325','7326','7327','7328','7329','13080','13081','13082','13083','13086','13084','13085','13087','13088','13089','13090','13091','13092','13093','13094','13095','13096','10012','10015','10017','10018','10019','10020','10021','10022','10023','10024','10025','10026','10027','10028','10029','10030','10031','10032','10033','10034','10035','10040','10041','10042','10043','10045','10046','10047','10048','10049','10050','10051','10052','10036','10044')
    Site_Name <- c('Paulus Boulevard','Tunison Road','Naricon Place','Eggers Street - South Woodland Avenue','Edgeboro Road - Old Bridge Turnpike','Tices Lane','Highland Street - Shopping Center','West Ferris Street','Arthur Street','Racetrack Road','East Brunswick Square Mall Driveway','Rues Lane','Hillsdale Lane','Jackson Road','Edgewood Avenue','Taunton Avenue','Berlin Cross-Keys Road - Walker Avenue','Prospect Avenue','Zulker Avenue','Franklin Avenue','Cooper Road','Lakeside Avenue - Signal Hill Drive','Bowman Drive (formerly Campus Drive) - Cedar Hill Road','William Feather Drive','Dutchtown Road - Spur','Haddonfield-Kresson Road - Braddock Mill Road','Ardsley Drive - Target Store','Evesham Road - Marlton Parkway','Brick Road','Centre Boulevard - South Maple Avenue','Lincoln Drive (South)','Greentree Road','Lincoln Drive (North)','Atrium Way','Church Road','Rogers Walk (formerly Howard Boulevard)','Fellowship Road','Waverly Avenue - Willow Avenue','Fox Meadow drive - Fellowship Road','Stiles Avenue - Princeton Avenue','High Street','Fork Landing Road','Mercer Street','Hickory Corner Road','Dutchneck Road','Princeton-Hightstown Road - Stockton Street','Old Cranbury Road','East Windsor Shopping Center - Americana Diner','Town Center Road - Rocky Brook Road','Old Trenton Road','Cranbury-Station Road','Half Acre Road','South River Road','Dey Road','Broadway Road','Route NJ-32 East - NJTPK Park & Ride','Route NJ-32 West - Friendship Road','Herrod Boulevard - Commerce Drive','Cranbury Road - South River Road','Franklin Corner Road - Bakers Basin Road','Carnegie Center Boulevard','Washington Road','Fisher Place','Harrison Street','Independence Way','Ridge Road','Raymond Road','Promenade Boulevard - Stouts Lane','Wynwood Drive - Whispering Woods Boulevard','New Road','Sand Hill Road - Major Road','Beekman Road','Deans Lane','Henderson Road','Blackhorse Lane','Finnegans Lane','Aaron Road','Commerce Boulevard','Adams Lane - Cozzens Lane','North Oaks Boulevard','Plainfield Avenue','Wooding Avenue','Old Post Road (South)','Old Post Road (North)','Forrest Haven Boulevard','Prince Street','Grandview Avenue','Parsonage Road','Parsonage Road & Lafayette','Ford Avenue','Woodbridge Center Drive - Gill Lane','Green Street','Fashion Plaza Entrance','Edison Towne Square Driveway')
    Route_Name <- c('NJ-18','NJ-18','NJ-18','NJ-18','NJ-18','NJ-18','NJ-18','NJ-18','NJ-18','NJ-18','NJ-18','NJ-18','NJ-18','NJ-73','NJ-73','NJ-73','NJ-73','NJ-73','NJ-73','NJ-73','NJ-73','NJ-73','NJ-73','NJ-73','NJ-73','NJ-73','NJ-73','NJ-73','NJ-73','NJ-73','NJ-73','NJ-73','NJ-73','NJ-73','NJ-73','NJ-73','NJ-73','NJ-73','NJ-73','NJ-73','NJ-73','NJ-73','US-130','US-130','US-130','US-130','US-130','US-130','US-130','US-130','US-130','US-130','US-130','US-130','US-130','US-130','US-130','US-130','US-130','US-1','US-1','US-1','US-1','US-1','US-1','US-1','US-1','US-1','US-1','US-1','US-1','US-1','US-1','US-1','US-1','US-1','US-1','US-1','US-1','US-1','US-1','US-1','US-1','US-1','US-1','US-1','US-1','US-1','US-1','US-1','US-1','US-1','US-1','US-1')
    Site_MP <- c('40.9','40.22','39.98','39.29','38.89','38.38','38.11','37.88','36.68','36.21','35.89','35.57','35.12','16.04','16.63','17.12','17.67','17.97','17.67','18.37','19.24','19.65','20.04','20.4','20.9','21.43','22.28','22.8','23.19','23.61','24.87','25.35','25.75','26.15','26.47','26.81','27.32','27.94','29.12','30.43','31.12','31.65','67.22','67.5','68.21','68.57','69.79','68.82','68.97','70.92','71.3','71.99','72.08','72.86','73.55','74.25','74.51','0.46','0.9','5.98','10','11.27','11.38','11.83','14.12','14.57','15.85','16.47','16.96','17.54','18.34','19.07','19.74','19.94','20.42','20.73','21.38','21.94','22.44','23.77','29.06','29.52','29.88','30.54','31.1','31.48','33.11','33.64','33.64','34.24','35.1','35.69','24.15','30.79')
    Site_long <- c('-74.4228645','-74.4152301','-74.4105666','-74.4080679','-74.4044831','-74.4008657','-74.3991423','-74.3974512','-74.3885941','-74.3834553','-74.3799516','-74.3764351','-74.3706157','-74.9093908','-74.9173767','-74.9244133','-74.9314145','-74.9314333','-74.9348741','-74.9304968','-74.9281001','-74.9268662','-74.9257538','-74.9246','-74.922956','-74.9216407','-74.9218433','-74.9219368','-74.9220383','-74.9229773','-74.9349672','-74.939751','-74.94288','-74.9461021','-74.9498547','-74.9535418','-74.960762','-74.969652','-74.9861238','-74.9972973','-75.006427','-75.0122198','-74.5517362','-74.5486928','-74.5421875','-74.5380904','-74.5246757','-74.5355077','-74.5337704','-74.5126654','-74.5106302','-74.5087406','-74.5082155','-74.5063053','-74.5023402','-74.4978483','-74.4966761','-74.490366','-74.4821262','-74.7061401','-74.6544544','-74.6380432','-74.6367166','-74.6310101','-74.6014476','-74.5956281','-74.5792263','-74.5711424','-74.5647428','-74.5573246','-74.5469907','-74.5375206','-74.5288785','-74.5261252','-74.5200324','-74.5160019','-74.5076218','-74.5003561','-74.4940518','-74.4718','-74.398803','-74.391585','-74.3858061','-74.3749545','-74.3659299','-74.360867','-74.3386359','-74.3311193','-74.3317174','-74.3218287','-74.3084661','-74.3002884','-74.4651067','-74.37071335')
    Site_lat <- c('40.4853057','40.4838166','40.4784518','40.4685839','40.4634976','40.4565242','40.453108','40.4498196','40.4340784','40.4285585','40.4248009','40.4209812','40.4160867','39.7860734','39.7919692','39.7961315','39.8018172','39.8060218','39.8032246','39.8120294','39.8244664','39.8303606','39.8356711','39.8407859','39.848471','39.8557041','39.8678566','39.875186','39.8810739','39.8869898','39.9027949','39.9087023','39.9139723','39.9193838','39.9241259','39.9265451','39.9313806','39.9373199','39.948888','39.9636837','39.9709539','39.9773114','40.2564186','40.259876','40.2687344','40.2729809','40.2873961','40.2757936','40.2776245','40.3007235','40.306127','40.3159542','40.3188121','40.3284094','40.3379626','40.347333','40.3509895','40.3478057','40.3461948','40.2751123','40.3179074','40.3314876','40.3325705','40.3373218','40.3616372','40.3663971','40.3798875','40.3865277','40.3917393','40.3978162','40.4063168','40.4140673','40.4211124','40.4233679','40.4283359','40.4316559','40.4385198','40.4445295','40.4497279','40.4582153','40.504129','40.5079531','40.5106586','40.5151655','40.5197554','40.5235635','40.5398548','40.5449302','40.5457536','40.5500923','40.5571162','40.562863','40.4600394','40.51695315')
    
    Sites_df_bottleneck_ranking <- data.frame(ID, Site_ID, Site_Name, Route_Name, Site_MP, Site_long, Site_lat)
    Sites_df_bottleneck_ranking <- Sites_df_bottleneck_ranking[Sites_df_bottleneck_ranking$Route_Name == unique(results_bottleneck_analysis$route)[1],]
    
    NJDOT_icon <- makeIcon(
      iconUrl = "https://uxwing.com/wp-content/themes/uxwing/download/traffic-road-sign/traffic-light-icon.png",
      iconWidth = 12, iconHeight = 25,
      iconAnchorX = 12, iconAnchorY = 25,
      shadowUrl = NULL, #"http://leafletjs.com/examples/custom-icons/leaf-shadow.png",
      shadowWidth = 0, shadowHeight = 0,
      shadowAnchorX = 0, shadowAnchorY = 0
    )
    
    
    
    output$bottleneck_ranking_map <- 
      renderLeaflet({

      leaflet() %>%
        # addProviderTiles(providers$CartoDB.DarkMatterNoLabels) %>%
          addTiles() %>%
          addControl(title, position = "topleft", className="map-title") %>%
          # addProviderTiles(providers$OpenStreetMap.BlackAndWhite, group = "Calles") %>%
          # addProviderTiles(providers$Esri.WorldImagery, group = "Imagen satelital") %>% 
          # addProviderTiles(providers$OpenStreetMap.BlackAndWhite, group = "Stamen.TonerLite") %>%
          # addProviderTiles(providers$Esri.WorldImagery, group = "Stamen.Watercolor") %>% 
          # addProviderTiles(providers$OpenStreetMap.BlackAndWhite, group = "CartoDB.Positron") %>%
          # addProviderTiles(providers$Esri.WorldImagery, group = "Acetate.terrain") %>% 
          
          # addProviderTiles(c("Stamen.TonerLite", "Stamen.Watercolor", "CartoDB.Positron", "Acetate.terrain"), group = c("Stamen.TonerLite", "Stamen.Watercolor", "CartoDB.Positron", "Acetate.terrain")) %>%
        # fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat))
        # fitBounds(-75.167, 39.695, -74.202, 40.774) %>%
        fitBounds(min(results_bottleneck_analysis$Longitude1),min(results_bottleneck_analysis$Latitude1),max(results_bottleneck_analysis$Longitude1),max(results_bottleneck_analysis$Latitude1)) %>%
          # addCircles(lng=results_bottleneck_analysis_zero_percent_total_duration_scaled$Longitude1,
          #          lat=results_bottleneck_analysis_zero_percent_total_duration_scaled$Latitude1, 
          #          radius =results_bottleneck_analysis_zero_percent_total_duration_scaled$percent_total_duration_scaled*1000,
          #          color = "#ebefff",
          #          popup = leafpop::popupGraph(barplot(table(results_bottleneck_analysis_zero_percent_total_duration_scaled$Total_Duration1_Hours)#, results_bottleneck_analysis_zero_percent_total_duration_scaled$Total_Duration2_Hours)
          #                          ,main="Total Duration before and after"
          #                          ,xlab="Bottleneck", col=c("darkblue","red"),
          #                          legend = c("before","After"), beside=TRUE))
          #          ) %>%
          # addCircles(lng=results_bottleneck_analysis_improvement_percent_total_duration_scaled$Longitude1,
          #          lat=results_bottleneck_analysis_improvement_percent_total_duration_scaled$Latitude1, 
          #          radius =results_bottleneck_analysis_improvement_percent_total_duration_scaled$percent_total_duration_scaled*1000,
          #          color = "#0033ff",
          #          # popup = leafpop::popupGraph(p_imp)
          #          popup = leafpop::popupGraph(barplot(p_imp
          #                          ,main="Total Duration before and after"
          #                          ,xlab="Bottleneck", col=c("darkblue","red"),
          #                          legend =  c("before","After"), beside=TRUE))
          #          ) %>% 
          addCircles(lng=results_bottleneck_analysis$Longitude1,
                   lat=results_bottleneck_analysis$Latitude1, 
                   radius = 
                     if(input$bottleneck_ranking_threshold_1 == "total_duration"){
                       results_bottleneck_analysis$percent_total_duration_scaled*100*input$circle_size_bottleneck_ranking
                     } else if (input$bottleneck_ranking_threshold_1 == "avg_duration"){
                       results_bottleneck_analysis$percent_avg_daily_duration_scaled*100*input$circle_size_bottleneck_ranking
                     } else if (input$bottleneck_ranking_threshold_1 == "agency_reported_events"){
                       results_bottleneck_analysis$percent_Agency_Reported_Events_scaled*100*input$circle_size_bottleneck_ranking
                     } else if (input$bottleneck_ranking_threshold_1 == "volume_estimate"){
                       results_bottleneck_analysis$percent_Volume_Estimate_scaled*100*input$circle_size_bottleneck_ranking
                     } else if (input$bottleneck_ranking_threshold_1 == "base_impact"){
                       results_bottleneck_analysis$percent_Base_Impact_scaled*100*input$circle_size_bottleneck_ranking
                     } else if (input$bottleneck_ranking_threshold_1 == "speed_differential"){
                       results_bottleneck_analysis$percent_Speed_Differential_scaled*100*input$circle_size_bottleneck_ranking
                     } else if (input$bottleneck_ranking_threshold_1 == "congestion"){
                       results_bottleneck_analysis$percent_Congestion_scaled*100*input$circle_size_bottleneck_ranking
                     } else if (input$bottleneck_ranking_threshold_1 == "total_delay"){
                       results_bottleneck_analysis$percent_Total_Delay_scaled*100*input$circle_size_bottleneck_ranking
                     }
                   ,
                   color =  pal(results_bottleneck_analysis$bottleneck_type),#"#ff0033",
                   # popup = leafpop::popupGraph(p_deg)
                   popup = leafpop::popupGraph(p,
                                               width = 1100,
                                               height = 600),
                   group = results_bottleneck_analysis$direction
          ) %>%
          addCircles(lng=(max(results_bottleneck_analysis$Longitude1)+mean(results_bottleneck_analysis$Longitude1))/2,
                     lat=(min(results_bottleneck_analysis$Latitude1)+mean(results_bottleneck_analysis$Latitude1))/2, 
                     radius = if(input$bottleneck_ranking_threshold_1 == "total_duration"){
                       mean(results_bottleneck_analysis$percent_total_duration_scaled)*100*input$circle_size_bottleneck_ranking*2+1
                     } else if (input$bottleneck_ranking_threshold_1 == "avg_duration"){
                       mean(results_bottleneck_analysis$percent_avg_daily_duration_scaled)*100*input$circle_size_bottleneck_ranking*2+1
                     } else if (input$bottleneck_ranking_threshold_1 == "agency_reported_events"){
                       mean(results_bottleneck_analysis$percent_Agency_Reported_Events_scaled)*100*input$circle_size_bottleneck_ranking*2+1
                     } else if (input$bottleneck_ranking_threshold_1 == "volume_estimate"){
                       mean(results_bottleneck_analysis$percent_Volume_Estimate_scaled)*100*input$circle_size_bottleneck_ranking*2+1
                     } else if (input$bottleneck_ranking_threshold_1 == "base_impact"){
                       mean(results_bottleneck_analysis$percent_Base_Impact_scaled)*100*input$circle_size_bottleneck_ranking*2+1
                     } else if (input$bottleneck_ranking_threshold_1 == "speed_differential"){
                       mean(results_bottleneck_analysis$percent_Speed_Differential_scaled)*100*input$circle_size_bottleneck_ranking*2+1
                     } else if (input$bottleneck_ranking_threshold_1 == "congestion"){
                       mean(results_bottleneck_analysis$percent_Congestion_scaled)*100*input$circle_size_bottleneck_ranking*2+1
                     } else if (input$bottleneck_ranking_threshold_1 == "total_delay"){
                       mean(results_bottleneck_analysis$percent_Total_Delay_scaled)*100*input$circle_size_bottleneck_ranking*2+1
                     }
                     ,
                     color =  "black", #pal(results_bottleneck_analysis$bottleneck_type),#"#ff0033",
                     # popup = leafpop::popupGraph(p_deg)
                     popup = leafpop::popupGraph(p_overall,
                                                 width = 1100,
                                                 height = 600)#,
                     # group = results_bottleneck_analysis$direction
          ) %>%
          addAwesomeMarkers(lng=(max(results_bottleneck_analysis$Longitude1)+mean(results_bottleneck_analysis$Longitude1))/2,
                     lat=(min(results_bottleneck_analysis$Latitude1)*0.46+mean(results_bottleneck_analysis$Latitude1)*0.54),
                     icon = leaflet::awesomeIcons(
                     icon = "help",
                     library = "ion",
                     markerColor = "black",
                     iconColor = "white",
                     spin = TRUE,
                     extraClasses = NULL,
                     squareMarker = TRUE,
                     iconRotate = 0,
                     fontFamily = "monospace",
                     text = NULL
                     )
                                                    # "information-circle-outline")
                     # ,color =  "black" #,pal(results_bottleneck_analysis$bottleneck_type),#"#ff0033",
                     # popup = leafpop::popupGraph(p_deg)
                     ,popup = info_bottleneck_ranking
                     # leafpop::popupGraph(p_overall,
                     #                             width = 1100,
                     #                             height = 600)#,
                     # # group = results_bottleneck_analysis$direction
          ) %>%
          addMarkers(lng = as.numeric(Sites_df_bottleneck_ranking$Site_long),
                            lat = as.numeric(Sites_df_bottleneck_ranking$Site_lat),
                            icon = NJDOT_icon
                            #   leaflet::awesomeIcons(
                            #   icon = "location",
                            #   library = "ion",
                            #   markerColor = "blue",
                            #   iconColor = "white",
                            #   spin = FALSE,
                            #   extraClasses = NULL,
                            #   squareMarker = FALSE,
                            #   iconRotate = 0,
                            #   fontFamily = "monospace",
                            #   text = NULL
                            # )
                            # "information-circle-outline")
                            # ,color =  "black" #,pal(results_bottleneck_analysis$bottleneck_type),#"#ff0033",
                            # popup = leafpop::popupGraph(p_deg)
                            ,popup = paste0("ID: ",Sites_df_bottleneck_ranking$Site_ID,
                                          " @MP",Sites_df_bottleneck_ranking$Site_MP,
                                          " ;Name: ",Sites_df_bottleneck_ranking$Site_Name
                            ), options = markerOptions(
                              interactive = TRUE,
                              clickable = NULL,
                              draggable = FALSE,
                              keyboard = TRUE,
                              title = "",
                              alt = "",
                              zIndexOffset = 0,
                              opacity = 0.45,
                              riseOnHover = FALSE,
                              riseOffset = 250
                            )
                            
                            
                            
                            
                                          
                            # leafpop::popupGraph(p_overall,
                            #                             width = 1100,
                            #                             height = 600)#,
                            # # group = results_bottleneck_analysis$direction
          ) %>% 
          leaflet::addLegend("bottomright", pal = pal_legend, 
                             values = c("Improved Bottlenecks", "Degraded Bottlenecks", "New Bottlenecks","Dissapearing Bottlenecks","Overall Average Results"),
                    title = paste0("Bottleneck Types based on (",
                                   if(input$bottleneck_ranking_threshold_1 == "total_duration"){
                                     "Total Congested Duration"
                                   } else if (input$bottleneck_ranking_threshold_1 == "avg_duration"){
                                     "Avg Congested Duration"
                                   } else if (input$bottleneck_ranking_threshold_1 == "agency_reported_events"){
                                     "Agency Reported Events"
                                   } else if (input$bottleneck_ranking_threshold_1 == "volume_estimate"){
                                     "Volume Estimates"
                                   } else if (input$bottleneck_ranking_threshold_1 == "base_impact"){
                                     "Base Impact"
                                   } else if (input$bottleneck_ranking_threshold_1 == "speed_differential"){
                                     "Speed Differential"
                                   } else if (input$bottleneck_ranking_threshold_1 == "congestion"){
                                     "Congestion"
                                   } else if (input$bottleneck_ranking_threshold_1 == "total_delay"){
                                     "Total Delay"
                                   },")"),
                    labFormat = labelFormat(prefix = ""),
                    opacity = 0.7
          ) %>% 
          addLayersControl(
            # baseGroups = c("Calles","Imagen satelital"),
            overlayGroups = unique(results_bottleneck_analysis$direction) ,
            options = layersControlOptions(collapsed = FALSE)
          ) %>% 
          leafem::addLogo(img = "https://www.jfkblvdproject.com/wp-content/uploads/2019/09/dotlight.png",
                  src= "remote",
                  position = "bottomleft",
                  offset.x = 20,
                  offset.y = 13,
                  width = 60,
                  height = 60) 
        # %>% leaflet.multiopacity::addOpacityControls(layerId = c("sites"))
        # %>% addPopups(results_bottleneck_analysis$percent_total_duration, content,
        #             options = popupOptions(closeButton = FALSE))
                   
    })

    # map_bottlebeck_ranking <- leaflet() %>% addTiles() %>%
    #   addCircleMarkers(lng=results_bottleneck_analysis$Longitude1,lat=results_bottleneck_analysis$Latitude1)
    # output$bottleneck_ranking_map <- renderLeaflet({map_bottlebeck_ranking
      
    })
  output$downloadData_bottleneck_ranking <- downloadHandler(
    
    filename = function() { 
      paste("Bottleneck_Ranking", Sys.time(), ".csv", sep="")
    },
    content = function(file_bottleneck_ranking1) { 
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = "Analyzing the Data...", value = 0) 
    
    input_file1_bottleneck <- paste0(input$bottlneck_file1$datapath)
    input_file2_bottleneck <- paste0(input$bottlneck_file2$datapath)
    bottleneck_analysis <- function(file1_bottleneck, file2_bottleneck){
      library(dplyr)
      df_bottleneck_1 <- readr::read_csv(file1_bottleneck)
      df_bottleneck_2 <- readr::read_csv(file2_bottleneck)
      colnames(df_bottleneck_1) <- c("Rank1","Head Location1","Starting TMC Code1","Latitude1","Longitude1"
                                     ,"Average Max Length1","Average Daily Duration1","Total Duration1"
                                     ,"Agency-Reported Events1","Volume Estimate1","Base Impact1"
                                     ,"Speed Differential1","Congestion1","Total Delay1","Segment IDs1")
      colnames(df_bottleneck_2) <- c("Rank2","Head Location2","Starting TMC Code2","Latitude2","Longitude2"
                                     ,"Average Max Length2","Average Daily Duration2","Total Duration2"
                                     ,"Agency-Reported Events2","Volume Estimate2","Base Impact2"
                                     ,"Speed Differential2","Congestion2","Total Delay2","Segment IDs2")
      # unique(c(df_bottleneck_1$`Head Location1`,df_bottleneck_2$`Head Location2`))
      df_bottleneck_res <- as.data.frame(unique(c(df_bottleneck_1$`Head Location1`,df_bottleneck_2$`Head Location2`)))
      colnames(df_bottleneck_res) <- "Head Location"
      df_bottleneck_res <- df_bottleneck_res %>% dplyr::left_join(df_bottleneck_1, by =c("Head Location"="Head Location1"))
      df_bottleneck_res <- df_bottleneck_res %>% dplyr::left_join(df_bottleneck_2, by =c("Head Location"="Head Location2"))
      df_bottleneck_res$`Total Duration1`[7]
      sub("d*.", "", df_bottleneck_res$`Total Duration1`[1])
      as.integer(gsub(".*(.+) d.*", "\\1", df_bottleneck_res$`Total Duration1`[7])) 
      + as.integer(gsub(".* d (.+) h.*", "\\1", df_bottleneck_res$`Total Duration1`[7]))
      + as.integer(gsub(".* h (.+) m.*", "\\1", df_bottleneck_res$`Total Duration1`[7]))
      for(bottleneck_k in (1:nrow(df_bottleneck_res))){
        # df_bottleneck_res$`Total Duration Year1`[bottleneck_k] <- 
        #   if(!is.na(as.integer(gsub(".* c (.+) y.*", "\\1", df_bottleneck_res$`Total Duration1`[bottleneck_k])))){
        #     as.integer(gsub(".* c (.+) y.*", "\\1", df_bottleneck_res$`Total Duration1`[bottleneck_k]))
        #   } else if (!is.na(as.integer(gsub(" y.*", "\\1", df_bottleneck_res$`Total Duration1`[bottleneck_k])))) {
        #     as.integer(gsub(" y.*", "\\1", df_bottleneck_res$`Total Duration1`[bottleneck_k]))
        #   } else {
        #     0
        #   }
        
        # df_bottleneck_res$`Total Duration Month1`[bottleneck_k] <- 
        #   if(!is.na(as.integer(gsub(".* y (.+) m.*", "\\1", df_bottleneck_res$`Total Duration1`[bottleneck_k])))){
        #     as.integer(gsub(".* y (.+) m.*", "\\1", df_bottleneck_res$`Total Duration1`[bottleneck_k]))
        #   } else if (!is.na(as.integer(gsub(" m.*", "\\1", df_bottleneck_res$`Total Duration1`[bottleneck_k])))) {
        #     as.integer(gsub(" m.*", "\\1", df_bottleneck_res$`Total Duration1`[bottleneck_k]))
        #   } else {
        #     0
        #   }
        df_bottleneck_res$`Total Duration Day1`[bottleneck_k] <- 
          if(!is.na(as.integer(gsub(".* m (.+) d.*", "\\1", df_bottleneck_res$`Total Duration1`[bottleneck_k])))){
            as.integer(gsub(".* m (.+) d.*", "\\1", df_bottleneck_res$`Total Duration1`[bottleneck_k]))
          } else if (!is.na(as.integer(gsub(" d.*", "\\1", df_bottleneck_res$`Total Duration1`[bottleneck_k])))) {
            as.integer(gsub(" d.*", "\\1", df_bottleneck_res$`Total Duration1`[bottleneck_k]))
          } else {
            0
          }
        df_bottleneck_res$`Total Duration Hour1`[bottleneck_k] <- 
          if(!is.na(as.integer(gsub(".* d (.+) h.*", "\\1", df_bottleneck_res$`Total Duration1`[bottleneck_k])))){
            as.integer(gsub(".* d (.+) h.*", "\\1", df_bottleneck_res$`Total Duration1`[bottleneck_k]))
          } else if (!is.na(as.integer(gsub(" h.*", "\\1", df_bottleneck_res$`Total Duration1`[bottleneck_k])))) {
            as.integer(gsub(" h.*", "\\1", df_bottleneck_res$`Total Duration1`[bottleneck_k]))
          } else {
            0
          }
        df_bottleneck_res$`Total Duration Minute1`[bottleneck_k] <- 
          if(!is.na(as.integer(gsub(".* h (.+) m.*", "\\1", df_bottleneck_res$`Total Duration1`[bottleneck_k])))){
            as.integer(gsub(".* h (.+) m.*", "\\1", df_bottleneck_res$`Total Duration1`[bottleneck_k]))
          } else if (!is.na(as.integer(gsub(" m.*", "\\1", df_bottleneck_res$`Total Duration1`[bottleneck_k])))) {
            as.integer(gsub(" m.*", "\\1", df_bottleneck_res$`Total Duration1`[bottleneck_k]))
          } else {
            0
          }
        # df_bottleneck_res$`Total Duration Year2`[bottleneck_k] <- 
        #   if(!is.na(as.integer(gsub(".* c (.+) y.*", "\\1", df_bottleneck_res$`Total Duration2`[bottleneck_k])))){
        #     as.integer(gsub(".* c (.+) y.*", "\\1", df_bottleneck_res$`Total Duration2`[bottleneck_k]))
        #   } else if (!is.na(as.integer(gsub(" y.*", "\\1", df_bottleneck_res$`Total Duration2`[bottleneck_k])))) {
        #     as.integer(gsub(" y.*", "\\1", df_bottleneck_res$`Total Duration2`[bottleneck_k]))
        #   } else {
        #     0
        #   }
        
        # df_bottleneck_res$`Total Duration Month2`[bottleneck_k] <- 
        #   if(!is.na(as.integer(gsub(".* y (.+) m.*", "\\1", df_bottleneck_res$`Total Duration2`[bottleneck_k])))){
        #     as.integer(gsub(".* y (.+) m.*", "\\1", df_bottleneck_res$`Total Duration2`[bottleneck_k]))
        #   } else if (!is.na(as.integer(gsub(" m.*", "\\1", df_bottleneck_res$`Total Duration2`[bottleneck_k])))) {
        #     as.integer(gsub(" m.*", "\\1", df_bottleneck_res$`Total Duration2`[bottleneck_k]))
        #   } else {
        #     0
        #   }
        df_bottleneck_res$`Total Duration Day2`[bottleneck_k] <- 
          if(!is.na(as.integer(gsub(".* m (.+) d.*", "\\1", df_bottleneck_res$`Total Duration2`[bottleneck_k])))){
            as.integer(gsub(".* m (.+) d.*", "\\1", df_bottleneck_res$`Total Duration2`[bottleneck_k]))
          } else if (!is.na(as.integer(gsub(" d.*", "\\1", df_bottleneck_res$`Total Duration2`[bottleneck_k])))) {
            as.integer(gsub(" d.*", "\\1", df_bottleneck_res$`Total Duration2`[bottleneck_k]))
          } else {
            0
          }
        df_bottleneck_res$`Total Duration Hour2`[bottleneck_k] <- 
          if(!is.na(as.integer(gsub(".* d (.+) h.*", "\\1", df_bottleneck_res$`Total Duration2`[bottleneck_k])))){
            as.integer(gsub(".* d (.+) h.*", "\\1", df_bottleneck_res$`Total Duration2`[bottleneck_k]))
          } else if (!is.na(as.integer(gsub(" h.*", "\\1", df_bottleneck_res$`Total Duration2`[bottleneck_k])))) {
            as.integer(gsub(" h.*", "\\1", df_bottleneck_res$`Total Duration2`[bottleneck_k]))
          } else {
            0
          }
        df_bottleneck_res$`Total Duration Minute2`[bottleneck_k] <- 
          if(!is.na(as.integer(gsub(".* h (.+) m.*", "\\1", df_bottleneck_res$`Total Duration2`[bottleneck_k])))){
            as.integer(gsub(".* h (.+) m.*", "\\1", df_bottleneck_res$`Total Duration2`[bottleneck_k]))
          } else if (!is.na(as.integer(gsub(" m.*", "\\1", df_bottleneck_res$`Total Duration2`[bottleneck_k])))) {
            as.integer(gsub(" m.*", "\\1", df_bottleneck_res$`Total Duration2`[bottleneck_k]))
          } else {
            0
          }
        # df_bottleneck_res$`Average Daily Duration Year1`[bottleneck_k] <- 
        #   if(!is.na(as.integer(gsub(".* c (.+) y.*", "\\1", df_bottleneck_res$`Average Daily Duration1`[bottleneck_k])))){
        #     as.integer(gsub(".* c (.+) y.*", "\\1", df_bottleneck_res$`Average Daily Duration1`[bottleneck_k]))
        #   } else if (!is.na(as.integer(gsub(" y.*", "\\1", df_bottleneck_res$`Average Daily Duration1`[bottleneck_k])))) {
        #     as.integer(gsub(" y.*", "\\1", df_bottleneck_res$`Average Daily Duration1`[bottleneck_k]))
        #   } else {
        #     0
        #   }
        # df_bottleneck_res$`Average Daily Duration Month1`[bottleneck_k] <- 
        #   if(!is.na(as.integer(gsub(".* y (.+) m.*", "\\1", df_bottleneck_res$`Average Daily Duration1`[bottleneck_k])))){
        #     as.integer(gsub(".* y (.+) m.*", "\\1", df_bottleneck_res$`Average Daily Duration1`[bottleneck_k]))
        #   } else if (!is.na(as.integer(gsub(" m.*", "\\1", df_bottleneck_res$`Average Daily Duration1`[bottleneck_k])))) {
        #     as.integer(gsub(" m.*", "\\1", df_bottleneck_res$`Average Daily Duration1`[bottleneck_k]))
        #   } else {
        #     0
        #   }
        df_bottleneck_res$`Average Daily Duration Day1`[bottleneck_k] <- 
          if(!is.na(as.integer(gsub(".* m (.+) d.*", "\\1", df_bottleneck_res$`Average Daily Duration1`[bottleneck_k])))){
            as.integer(gsub(".* m (.+) d.*", "\\1", df_bottleneck_res$`Average Daily Duration1`[bottleneck_k]))
          } else if (!is.na(as.integer(gsub(" d.*", "\\1", df_bottleneck_res$`Average Daily Duration1`[bottleneck_k])))) {
            as.integer(gsub(" d.*", "\\1", df_bottleneck_res$`Average Daily Duration1`[bottleneck_k]))
          } else {
            0
          }
        df_bottleneck_res$`Average Daily Duration Hour1`[bottleneck_k] <- 
          if(!is.na(as.integer(gsub(".* d (.+) h.*", "\\1", df_bottleneck_res$`Average Daily Duration1`[bottleneck_k])))){
            as.integer(gsub(".* d (.+) h.*", "\\1", df_bottleneck_res$`Average Daily Duration1`[bottleneck_k]))
          } else if (!is.na(as.integer(gsub(" h.*", "\\1", df_bottleneck_res$`Average Daily Duration1`[bottleneck_k])))) {
            as.integer(gsub(" h.*", "\\1", df_bottleneck_res$`Average Daily Duration1`[bottleneck_k]))
          } else {
            0
          }
        df_bottleneck_res$`Average Daily Duration Minute1`[bottleneck_k] <- 
          if(!is.na(as.integer(gsub(".* h (.+) m.*", "\\1", df_bottleneck_res$`Average Daily Duration1`[bottleneck_k])))){
            as.integer(gsub(".* h (.+) m.*", "\\1", df_bottleneck_res$`Average Daily Duration1`[bottleneck_k]))
          } else if (!is.na(as.integer(gsub(" m.*", "\\1", df_bottleneck_res$`Average Daily Duration1`[bottleneck_k])))) {
            as.integer(gsub(" m.*", "\\1", df_bottleneck_res$`Average Daily Duration1`[bottleneck_k]))
          } else {
            0
          }
        # df_bottleneck_res$`Average Daily Duration Year2`[bottleneck_k] <- 
        #   if(!is.na(as.integer(gsub(".* c (.+) y.*", "\\1", df_bottleneck_res$`Average Daily Duration2`[bottleneck_k])))){
        #     as.integer(gsub(".* c (.+) y.*", "\\1", df_bottleneck_res$`Average Daily Duration2`[bottleneck_k]))
        #   } else if (!is.na(as.integer(gsub(" y.*", "\\1", df_bottleneck_res$`Average Daily Duration2`[bottleneck_k])))) {
        #     as.integer(gsub(" y.*", "\\1", df_bottleneck_res$`Average Daily Duration2`[bottleneck_k]))
        #   } else {
        #     0
        #   }
        
        # df_bottleneck_res$`Average Daily Duration Month2`[bottleneck_k] <- 
        #   if(!is.na(as.integer(gsub(".* y (.+) m.*", "\\1", df_bottleneck_res$`Average Daily Duration2`[bottleneck_k])))){
        #     as.integer(gsub(".* y (.+) m.*", "\\1", df_bottleneck_res$`Average Daily Duration2`[bottleneck_k]))
        #   } else if (!is.na(as.integer(gsub(" m.*", "\\1", df_bottleneck_res$`Average Daily Duration2`[bottleneck_k])))) {
        #     as.integer(gsub(" m.*", "\\1", df_bottleneck_res$`Average Daily Duration2`[bottleneck_k]))
        #   } else {
        #     0
        #   }
        df_bottleneck_res$`Average Daily Duration Day2`[bottleneck_k] <- 
          if(!is.na(as.integer(gsub(".* m (.+) d.*", "\\1", df_bottleneck_res$`Average Daily Duration2`[bottleneck_k])))){
            as.integer(gsub(".* m (.+) d.*", "\\1", df_bottleneck_res$`Average Daily Duration2`[bottleneck_k]))
          } else if (!is.na(as.integer(gsub(" d.*", "\\1", df_bottleneck_res$`Average Daily Duration2`[bottleneck_k])))) {
            as.integer(gsub(" d.*", "\\1", df_bottleneck_res$`Average Daily Duration2`[bottleneck_k]))
          } else {
            0
          }
        df_bottleneck_res$`Average Daily Duration Hour2`[bottleneck_k] <- 
          if(!is.na(as.integer(gsub(".* d (.+) h.*", "\\1", df_bottleneck_res$`Average Daily Duration2`[bottleneck_k])))){
            as.integer(gsub(".* d (.+) h.*", "\\1", df_bottleneck_res$`Average Daily Duration2`[bottleneck_k]))
          } else if (!is.na(as.integer(gsub(" h.*", "\\1", df_bottleneck_res$`Average Daily Duration2`[bottleneck_k])))) {
            as.integer(gsub(" h.*", "\\1", df_bottleneck_res$`Average Daily Duration2`[bottleneck_k]))
          } else {
            0
          }
        df_bottleneck_res$`Average Daily Duration Minute2`[bottleneck_k] <- 
          if(!is.na(as.integer(gsub(".* h (.+) m.*", "\\1", df_bottleneck_res$`Average Daily Duration2`[bottleneck_k])))){
            as.integer(gsub(".* h (.+) m.*", "\\1", df_bottleneck_res$`Average Daily Duration2`[bottleneck_k]))
          } else if (!is.na(as.integer(gsub(" m.*", "\\1", df_bottleneck_res$`Average Daily Duration2`[bottleneck_k])))) {
            as.integer(gsub(" m.*", "\\1", df_bottleneck_res$`Average Daily Duration2`[bottleneck_k]))
          } else {
            0
          }
      } 
      df_bottleneck_res$Total_Duration1_Hours <- round(df_bottleneck_res$`Total Duration Day1`*24 + df_bottleneck_res$`Total Duration Hour1` + df_bottleneck_res$`Total Duration Minute1`/60,2)
      df_bottleneck_res$Total_Duration2_Hours <- round(df_bottleneck_res$`Total Duration Day2`*24 + df_bottleneck_res$`Total Duration Hour2` + df_bottleneck_res$`Total Duration Minute2`/60,2)
      df_bottleneck_res$Average_Duration1_Hours <- round(df_bottleneck_res$`Average Daily Duration Day1`*24 + df_bottleneck_res$`Average Daily Duration Hour1` + df_bottleneck_res$`Average Daily Duration Minute1`/60,2)
      df_bottleneck_res$Average_Duration2_Hours <- round(df_bottleneck_res$`Average Daily Duration Day2`*24 + df_bottleneck_res$`Average Daily Duration Hour2` + df_bottleneck_res$`Average Daily Duration Minute2`/60,2)
      df_bottleneck_res$`Total Duration Day1` <- NULL
      df_bottleneck_res$`Total Duration Day2` <- NULL
      df_bottleneck_res$`Total Duration Hour1` <- NULL
      df_bottleneck_res$`Total Duration Hour2` <- NULL
      df_bottleneck_res$`Total Duration Minute1` <- NULL
      df_bottleneck_res$`Total Duration Minute2` <- NULL
      df_bottleneck_res$`Average Daily Duration Day1` <- NULL
      df_bottleneck_res$`Average Daily Duration Day2` <- NULL
      df_bottleneck_res$`Average Daily Duration Hour1` <- NULL
      df_bottleneck_res$`Average Daily Duration Hour2` <- NULL
      df_bottleneck_res$`Average Daily Duration Minute1` <- NULL
      df_bottleneck_res$`Average Daily Duration Minute2` <- NULL
      df_bottleneck_res$`Starting TMC Code1`[is.na(df_bottleneck_res$`Starting TMC Code1`)] <- df_bottleneck_res$`Starting TMC Code2`[is.na(df_bottleneck_res$`Starting TMC Code1`)]
      df_bottleneck_res$Longitude1[is.na(df_bottleneck_res$Longitude1)] <- df_bottleneck_res$Longitude2[is.na(df_bottleneck_res$Longitude1)]
      df_bottleneck_res$Latitude1[is.na(df_bottleneck_res$Latitude1)] <- df_bottleneck_res$Latitude2[is.na(df_bottleneck_res$Latitude1)]
      df_bottleneck_res$`Average Max Length1`[is.na(df_bottleneck_res$`Average Max Length1`)] <- 0
      # df_bottleneck_res$`Agency-Reported Events1`[is.na(df_bottleneck_res$`Agency-Reported Events1`)] <- df_bottleneck_res$`Agency-Reported Events2`[is.na(df_bottleneck_res$`Agency-Reported Events1`)]
      # df_bottleneck_res$`Volume Estimate1`[is.na(df_bottleneck_res$`Volume Estimate1`)] <- df_bottleneck_res$`Volume Estimate2`[is.na(df_bottleneck_res$`Volume Estimate1`)]
      # df_bottleneck_res$`Base Impact1`[is.na(df_bottleneck_res$`Base Impact1`)] <- df_bottleneck_res$`Base Impact2`[is.na(df_bottleneck_res$`Base Impact1`)]
      # df_bottleneck_res$`Speed Differential1`[is.na(df_bottleneck_res$`Speed Differential1`)] <- df_bottleneck_res$`Speed Differential2`[is.na(df_bottleneck_res$`Speed Differential1`)]
      # df_bottleneck_res$Congestion1[is.na(df_bottleneck_res$Congestion1)] <- df_bottleneck_res$Congestion2[is.na(df_bottleneck_res$Congestion1)]
      # df_bottleneck_res$`Total Delay1`[is.na(df_bottleneck_res$`Total Delay1`)] <- df_bottleneck_res$`Total Delay2`[is.na(df_bottleneck_res$`Total Delay1`)] 
      df_bottleneck_res$`Segment IDs1`[is.na(df_bottleneck_res$`Segment IDs1`)] <- df_bottleneck_res$`Segment IDs2`[is.na(df_bottleneck_res$`Segment IDs1`)]
      df_bottleneck_res$`Starting TMC Code2`[is.na(df_bottleneck_res$`Starting TMC Code2`)] <- df_bottleneck_res$`Starting TMC Code1`[is.na(df_bottleneck_res$`Starting TMC Code2`)]
      df_bottleneck_res$Longitude2[is.na(df_bottleneck_res$Longitude2)] <- df_bottleneck_res$Longitude1[is.na(df_bottleneck_res$Longitude2)]
      df_bottleneck_res$Latitude2[is.na(df_bottleneck_res$Latitude2)] <- df_bottleneck_res$Latitude1[is.na(df_bottleneck_res$Latitude2)]
      df_bottleneck_res$`Average Max Length2`[is.na(df_bottleneck_res$`Average Max Length2`)] <- 0
      df_bottleneck_res$`Segment IDs2`[is.na(df_bottleneck_res$`Segment IDs2`)] <- df_bottleneck_res$`Segment IDs1`[is.na(df_bottleneck_res$`Segment IDs2`)]
      df_bottleneck_res$percent_total_duration <- round((df_bottleneck_res$Total_Duration2_Hours - df_bottleneck_res$Total_Duration1_Hours)/df_bottleneck_res$Total_Duration1_Hours*100,2)
      df_bottleneck_res$percent_total_duration[df_bottleneck_res$percent_total_duration =="Inf"] <- 100
      df_bottleneck_res$percent_total_duration[df_bottleneck_res$percent_total_duration =="NaN"] <- 0
      df_bottleneck_res$percent_avg_daily_duration <- round((df_bottleneck_res$Average_Duration2_Hours - df_bottleneck_res$Average_Duration1_Hours)/df_bottleneck_res$Average_Duration1_Hours*100,2)
      
      df_bottleneck_res$percent_Agency_Reported_Events <- round((df_bottleneck_res$`Agency-Reported Events2` - df_bottleneck_res$`Agency-Reported Events1`)/df_bottleneck_res$`Agency-Reported Events1`*100,2)
      df_bottleneck_res$percent_Volume_Estimate <- round((df_bottleneck_res$`Volume Estimate2` - df_bottleneck_res$`Volume Estimate1`)/df_bottleneck_res$`Volume Estimate1`*100,2)
      df_bottleneck_res$percent_Base_Impact <- round((df_bottleneck_res$`Base Impact2` - df_bottleneck_res$`Base Impact1`)/df_bottleneck_res$`Base Impact1`*100,2)
      df_bottleneck_res$percent_Speed_Differential <- round((df_bottleneck_res$`Speed Differential2` - df_bottleneck_res$`Speed Differential1`)/df_bottleneck_res$`Speed Differential1`*100,2)
      df_bottleneck_res$percent_Congestion <- round((df_bottleneck_res$Congestion2 - df_bottleneck_res$Congestion1)/df_bottleneck_res$Congestion1*100,2)
      df_bottleneck_res$percent_Total_Delay <- round((df_bottleneck_res$`Total Delay2` - df_bottleneck_res$`Total Delay1`)/df_bottleneck_res$`Total Delay1`*100,2)
      
      
      df_bottleneck_res$percent_avg_daily_duration[df_bottleneck_res$percent_avg_daily_duration =="Inf"] <- 100
      df_bottleneck_res$percent_avg_daily_duration[df_bottleneck_res$percent_avg_daily_duration =="NaN"] <- 0
      df_bottleneck_res$percent_Agency_Reported_Events[df_bottleneck_res$percent_Agency_Reported_Events =="Inf"] <- 100
      df_bottleneck_res$percent_Agency_Reported_Events[df_bottleneck_res$percent_Agency_Reported_Events =="NaN"] <- 0
      df_bottleneck_res$percent_Volume_Estimate[df_bottleneck_res$percent_Volume_Estimate =="Inf"] <- 100
      df_bottleneck_res$percent_Volume_Estimate[df_bottleneck_res$percent_Volume_Estimate =="NaN"] <- 0
      df_bottleneck_res$percent_Base_Impact[df_bottleneck_res$percent_Base_Impact =="Inf"] <- 100
      df_bottleneck_res$percent_Base_Impact[df_bottleneck_res$percent_Base_Impact =="NaN"] <- 0
      df_bottleneck_res$percent_Speed_Differential[df_bottleneck_res$percent_Speed_Differential =="Inf"] <- 100
      df_bottleneck_res$percent_Speed_Differential[df_bottleneck_res$percent_Speed_Differential =="NaN"] <- 0
      df_bottleneck_res$percent_Congestion[df_bottleneck_res$percent_Congestion =="Inf"] <- 100
      df_bottleneck_res$percent_Congestion[df_bottleneck_res$percent_Congestion =="NaN"] <- 0
      df_bottleneck_res$percent_Total_Delay[df_bottleneck_res$percent_Total_Delay =="Inf"] <- 100
      df_bottleneck_res$percent_Total_Delay[df_bottleneck_res$percent_Total_Delay =="NaN"] <- 0
      
      df_bottleneck_res$bottleneck_type <- "common"
      df_bottleneck_res$bottleneck_type[is.na(df_bottleneck_res$Rank1)] <- "new"
      df_bottleneck_res$bottleneck_type[is.na(df_bottleneck_res$Rank2)] <- "old"
      df_bottleneck_res[is.na(df_bottleneck_res)] <- 0
      colnames(df_bottleneck_res) <- c("HeadLocation","Rank1","Starting_TMC_Code1","Latitude1",
                                       "Longitude1","Average_Max_Length1","Average_Daily_Duration1","Total_Duration1",
                                       "Agency_Reported_Events1","Volume_Estimate1","Base_Impact1","Speed_Differential1",
                                       "Congestion1","Total_Delay1","Segment_IDs1","Rank2","Starting_TMC_Code2","Latitude2",
                                       "Longitude2","Average_Max_Length2","Average_Daily_Duration2","Total_Duration2",
                                       "Agency_Reported_Events2","Volume_Estimate2","Base_Impact2","Speed_Differential2",
                                       "Congestion2","Total_Delay2","Segment_IDs2","Total_Duration1_Hours","Total_Duration2_Hours",
                                       "Average_Duration1_Hours","Average_Duration2_Hours", "percent_total_duration", 
                                       "percent_avg_daily_duration","percent_Agency_Reported_Events","percent_Volume_Estimate",
                                       "percent_Base_Impact","percent_Speed_Differential","percent_Congestion",
                                       "percent_Total_Delay","bottleneck_type")
      
      if(input$bottleneck_ranking_threshold_1 == "total_duration"){
        df_bottleneck_res$Rank1 <- order(df_bottleneck_res$Total_Duration1_Hours,decreasing = TRUE)
        df_bottleneck_res$Rank2 <- order(df_bottleneck_res$Total_Duration2_Hours,decreasing = TRUE)
      } else if (input$bottleneck_ranking_threshold_1 == "avg_duration"){
        df_bottleneck_res$Rank1 <- order(df_bottleneck_res$Average_Duration1_Hours,decreasing = TRUE)
        df_bottleneck_res$Rank2 <- order(df_bottleneck_res$Average_Duration2_Hours,decreasing = TRUE)
      } else if (input$bottleneck_ranking_threshold_1 == "agency_reported_events"){
        df_bottleneck_res$Rank1 <- order(df_bottleneck_res$Agency_Reported_Events1,decreasing = TRUE)
        df_bottleneck_res$Rank2 <- order(df_bottleneck_res$Agency_Reported_Events2,decreasing = TRUE)
      } else if (input$bottleneck_ranking_threshold_1 == "volume_estimate"){
        df_bottleneck_res$Rank1 <- order(df_bottleneck_res$Volume_Estimate1,decreasing = TRUE)
        df_bottleneck_res$Rank2 <- order(df_bottleneck_res$Volume_Estimate2,decreasing = TRUE)
      } else if (input$bottleneck_ranking_threshold_1 == "base_impact"){
        df_bottleneck_res$Rank1 <- order(df_bottleneck_res$Base_Impact1,decreasing = TRUE)
        df_bottleneck_res$Rank2 <- order(df_bottleneck_res$Base_Impact2,decreasing = TRUE)
      } else if (input$bottleneck_ranking_threshold_1 == "speed_differential"){
        df_bottleneck_res$Rank1 <- order(df_bottleneck_res$Speed_Differential1,decreasing = TRUE)
        df_bottleneck_res$Rank2 <- order(df_bottleneck_res$Speed_Differential2,decreasing = TRUE)
      } else if (input$bottleneck_ranking_threshold_1 == "congestion"){
        df_bottleneck_res$Rank1 <- order(df_bottleneck_res$Congestion1,decreasing = TRUE)
        df_bottleneck_res$Rank2 <- order(df_bottleneck_res$Congestion2,decreasing = TRUE)
      } else if (input$bottleneck_ranking_threshold_1 == "total_delay"){
        df_bottleneck_res$Rank1 <- order(df_bottleneck_res$Total_Duration1,decreasing = TRUE)
        df_bottleneck_res$Rank2 <- order(df_bottleneck_res$Total_Duration2,decreasing = TRUE)
      }
      
      
      return(df_bottleneck_res)
    }
    results_bottleneck_analysis <- bottleneck_analysis(input_file1_bottleneck,input_file2_bottleneck)
    # as.numeric(input$bottleneck_avg_daily_threshold)
    # as.numeric(input$bottleneck_total_threshold)
    results_bottleneck_analysis <- results_bottleneck_analysis[(((results_bottleneck_analysis$Average_Duration1_Hours >= as.numeric(input$bottleneck_avg_daily_threshold) & results_bottleneck_analysis$Average_Duration2_Hours >= as.numeric(input$bottleneck_avg_daily_threshold)) & results_bottleneck_analysis$bottleneck_type =="common") | ((results_bottleneck_analysis$Average_Duration1_Hours >= as.numeric(input$bottleneck_avg_daily_threshold) | results_bottleneck_analysis$Average_Duration2_Hours >= as.numeric(input$bottleneck_avg_daily_threshold)) & results_bottleneck_analysis$bottleneck_type !="common")),]
    results_bottleneck_analysis <- results_bottleneck_analysis[(((results_bottleneck_analysis$Total_Duration1_Hours >= as.numeric(input$bottleneck_total_threshold) & results_bottleneck_analysis$Total_Duration2_Hours >= as.numeric(input$bottleneck_total_threshold)) & results_bottleneck_analysis$bottleneck_type =="common") | ((results_bottleneck_analysis$Total_Duration1_Hours >= as.numeric(input$bottleneck_total_threshold) | results_bottleneck_analysis$Total_Duration2_Hours >= as.numeric(input$bottleneck_total_threshold)) & results_bottleneck_analysis$bottleneck_type !="common")),]
    results_bottleneck_analysis <- results_bottleneck_analysis[(((results_bottleneck_analysis$Agency_Reported_Events1 >= as.numeric(input$bottleneck_agency_reported_events_threshold) & results_bottleneck_analysis$Agency_Reported_Events2 >= as.numeric(input$bottleneck_agency_reported_events_threshold)) & results_bottleneck_analysis$bottleneck_type =="common") | ((results_bottleneck_analysis$Agency_Reported_Events1 >= as.numeric(input$bottleneck_agency_reported_events_threshold) | results_bottleneck_analysis$Agency_Reported_Events2 >= as.numeric(input$bottleneck_agency_reported_events_threshold)) & results_bottleneck_analysis$bottleneck_type !="common")),]
    results_bottleneck_analysis <- results_bottleneck_analysis[(((results_bottleneck_analysis$Volume_Estimate1 >= as.numeric(input$bottleneck_volume_estimate_threshold) & results_bottleneck_analysis$Volume_Estimate2 >= as.numeric(input$bottleneck_volume_estimate_threshold)) & results_bottleneck_analysis$bottleneck_type =="common") | ((results_bottleneck_analysis$Volume_Estimate1 >= as.numeric(input$bottleneck_volume_estimate_threshold) | results_bottleneck_analysis$Volume_Estimate2 >= as.numeric(input$bottleneck_volume_estimate_threshold)) & results_bottleneck_analysis$bottleneck_type !="common")),]
    results_bottleneck_analysis <- results_bottleneck_analysis[(((results_bottleneck_analysis$Base_Impact1 >= as.numeric(input$bottleneck_base_impact_threshold) & results_bottleneck_analysis$Base_Impact2 >= as.numeric(input$bottleneck_base_impact_threshold)) & results_bottleneck_analysis$bottleneck_type =="common") | ((results_bottleneck_analysis$Base_Impact1 >= as.numeric(input$bottleneck_base_impact_threshold) | results_bottleneck_analysis$Base_Impact2 >= as.numeric(input$bottleneck_base_impact_threshold)) & results_bottleneck_analysis$bottleneck_type !="common")),]
    results_bottleneck_analysis <- results_bottleneck_analysis[(((results_bottleneck_analysis$Speed_Differential1 >= as.numeric(input$bottleneck_speed_differential_threshold) & results_bottleneck_analysis$Speed_Differential2 >= as.numeric(input$bottleneck_speed_differential_threshold)) & results_bottleneck_analysis$bottleneck_type =="common") | ((results_bottleneck_analysis$Speed_Differential1 >= as.numeric(input$bottleneck_speed_differential_threshold) | results_bottleneck_analysis$Speed_Differential2 >= as.numeric(input$bottleneck_speed_differential_threshold)) & results_bottleneck_analysis$bottleneck_type !="common")),]
    results_bottleneck_analysis <- results_bottleneck_analysis[(((results_bottleneck_analysis$Congestion1 >= as.numeric(input$bottleneck_congestion_threshold) & results_bottleneck_analysis$Congestion2 >= as.numeric(input$bottleneck_congestion_threshold)) & results_bottleneck_analysis$bottleneck_type =="common") | ((results_bottleneck_analysis$Congestion1 >= as.numeric(input$bottleneck_congestion_threshold) | results_bottleneck_analysis$Congestion2 >= as.numeric(input$bottleneck_congestion_threshold)) & results_bottleneck_analysis$bottleneck_type !="common")),]
    results_bottleneck_analysis <- results_bottleneck_analysis[(((results_bottleneck_analysis$Total_Delay1 >= as.numeric(input$bottleneck_total_delay_threshold) & results_bottleneck_analysis$Total_Delay2 >= as.numeric(input$bottleneck_total_delay_threshold)) & results_bottleneck_analysis$bottleneck_type =="common") | ((results_bottleneck_analysis$Total_Delay1 >= as.numeric(input$bottleneck_total_delay_threshold) | results_bottleneck_analysis$Total_Delay2 >= as.numeric(input$bottleneck_total_delay_threshold)) & results_bottleneck_analysis$bottleneck_type !="common")),]
    
    write.csv(results_bottleneck_analysis, file_bottleneck_ranking1, row.names=FALSE)
    
  })

  
  
  
  observeEvent(input$previewData12, {
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = "Creating a Plot....", value = 0)  
    lower_time_12 <- paste0(hms::as_hms(input$file12_2_lower_time)) 
    upper_time_12 <- paste0(hms::as_hms(input$file12_2_upper_time)) 
    input_file_event_12 <- paste0(input$file12_2_event_file$datapath)
    site <- as.integer(input$file12_2_site_ID)
    ped_movement <- as.integer(input$file_12_2_ped_phase_selected)
    
    ped_movement_function2 <- function(df_input, ped_movement){
      df <- readr::read_csv(df_input)
      df <- df[substr(df$Event,1,19)=="Pedestrian movement",]
      df$flag <- 0
      df <- dplyr::filter(df, grepl(paste0(": Ped ",ped_movement,"="),Event) & (grepl("Interval=Walk",Event) | grepl("Interval=Clearance",Event) | grepl(paste0(": Ped ",ped_movement,"=","\\[Demand=On\\]"),Event)))
      df$flag[grepl("Interval=Walk",df$Event)] <- "walk"
      df$flag[grepl("Interval=Clearance",df$Event)] <- "Clearance"
      df$flag[grepl(paste0(": Ped ",ped_movement,"=\\[Demand=On\\]"),df$Event)] <- "DemandOn"
      if(df$flag[1]!="DemandOn"){
        df <- df[-c(1),]
        if(df$flag[1]!="DemandOn"){
          df <- df[-c(1),]
          if(df$flag[1]!="DemandOn"){
            df <- df[-c(1),]
            if(df$flag[1]!="DemandOn"){
              df <- df[-c(1),]
              if(df$flag[1]!="DemandOn"){
                df <- df[-c(1),]
              }
            }
          }
        }
      }
      df$Start_time <- as.integer(hms::as_hms(substr(df$Time,12,19)))
      df$date <- as.Date(substr(df$Time,1,10))
      df$day <- weekdays(df$date)
      # https://stackoverflow.com/questions/26448533/how-to-restart-a-sequence-based-on-values-in-another-column-or-reference-the-pre
      df$rank <- ave(df$flag, cumsum(df$flag == "DemandOn"), FUN=seq)
      df_tail <- as.data.frame(as.integer(tail(df$Start_time,-1)))
      df_tail <- rbind(0,df_tail)
      colnames(df_tail) <- c("tail")
      df_head <- as.data.frame(as.integer(head(df$Start_time, -1)))
      colnames(df_head) <- c("head")
      df_head <- rbind(0,df_head)
      df$previous_time <- df_tail$tail - df_head$head
      df$waiting_time <- df$previous_time
      df$waiting_time[df$flag != "walk"] <- 0
      # Waiting Time DataFrame
      df_waiting <- df[df$flag == "walk",]
      df_waiting$Start_time <- as.integer(as.integer(df_waiting$Start_time/60)*60)
      df_waiting$flag <- 1
      time_df <- data.frame(seq(60,60*60*24,by=60))
      colnames(time_df) <- c("time")
      df_waiting <- sqldf::sqldf("select b.time, a.Start_time, a.day, a.date, a.waiting_time, a.flag
                    from time_df as b
                    left join df_waiting as a
                    on b.time = a.Start_time", drv = "SQLite")
      df_waiting$Start_time <- NULL
      df_waiting <- df_waiting %>% tidyr::fill(day,.direction = c("downup"))
      df_waiting <- df_waiting %>% tidyr::fill(date,.direction = c("downup"))
      df_waiting$waiting_time[is.na(df_waiting$waiting_time)] <- 0
      df_waiting$flag[is.na(df_waiting$flag)] <- 0
      return(df_waiting)
    }
    
    df_ped_results_file12 <- ped_movement_function2(input_file_event_12, ped_movement)
    df_ped_results_file12 <- df_ped_results_file12 %>% dplyr::filter(time >= as.integer(hms::as_hms(lower_time_12)))
    df_ped_results_file12 <- df_ped_results_file12 %>% dplyr::filter(time <= as.integer(hms::as_hms(upper_time_12)))
    Date_file12_2 <- as.Date(substr(readr::read_csv(input_file_event_12)$Time[1],1,10))
    
    lower_time_int_12 <- as.integer(hms::as_hms(lower_time_12))
    upper_time_int_12 <- as.integer(hms::as_hms(upper_time_12))
    
    xdata_12 <- hms::as_hms(unique(df_ped_results_file12$time))
    
    y12_4 <- df_ped_results_file12$flag
    df_plot_12_22_3 <- data.frame(xdata_12, y12_4)
    df_plot_12_22_3 <- ggplot(df_plot_12_22_3, aes(xdata_12))
    df_plot_12_22_3 <- df_plot_12_22_3 + geom_line(aes(y=y12_4, color = "Pedestrian Calls"))
    df_plot_12_22_3 <- df_plot_12_22_3 + theme(plot.title = element_text(color="black", size=10, face="bold.italic"),
                                               axis.text=element_text(size=8),
                                               axis.title=element_text(size=9,face="bold"),
                                               legend.text=element_text(size=9),
                                               legend.justification=c(1,1),legend.position=c(1,1),legend.title=element_blank())
    df_plot_12_22_3 <- df_plot_12_22_3 + scale_color_manual(name = " ", values = c("red"))
    df_plot_12_22_3 <- df_plot_12_22_3 + ylab(paste0("Pedestrian Calls")) + xlab("Time")
    df_plot_12_22_3 <- df_plot_12_22_3 + scale_x_continuous(labels = hms::as_hms(seq(lower_time_int_12, upper_time_int_12,by=3600)), breaks=seq(lower_time_int_12, upper_time_int_12,by=3600))
    df_plot_12_22_3 <- df_plot_12_22_3 + ylim(0, max(df_ped_results_file12$flag))
    df_plot_12_22_3 <- df_plot_12_22_3 + ggtitle(paste0("Pedestrian Calls for ",site," at Pedestrian Movement ",ped_movement," on ",Date_file12_2))
    df_plot_12_22_3 <- df_plot_12_22_3 + theme_calc()
    output$Plot12_1 <- renderPlot({df_plot_12_22_3})
    
    y12_2 <- df_ped_results_file12$waiting_time
    y12_3 <- rep(mean(df_ped_results_file12$waiting_time[df_ped_results_file12$waiting_time!=0]),each = nrow(df_ped_results_file12))
    name11_2 <- paste0("Avg Waiting Time ", round(mean(df_ped_results_file12$waiting_time[df_ped_results_file12$waiting_time!=0]),2)," sec")
    df_plot_12_22_2 <- data.frame(xdata_12, y12_2, y12_3)
    df_plot_12_22_2 <- ggplot(df_plot_12_22_2, aes(xdata_12))
    df_plot_12_22_2 <- df_plot_12_22_2 + geom_line(aes(y=y12_2, color = "Waiting Time"))
    df_plot_12_22_2 <- df_plot_12_22_2 + geom_line(aes(y=y12_3, color = name11_2),linetype = "dotted", size=1)
    df_plot_12_22_2 <- df_plot_12_22_2 + theme(plot.title = element_text(color="black", size=10, face="bold.italic"),
                                       axis.text=element_text(size=8),
                                       axis.title=element_text(size=9,face="bold"),
                                       legend.text=element_text(size=9),
                                       legend.justification=c(1,1),legend.position=c(1,1),legend.title=element_blank())
    df_plot_12_22_2 <- df_plot_12_22_2 + scale_color_manual(name = " ", values = c("red","darkblue"))
    df_plot_12_22_2 <- df_plot_12_22_2 + ylab(paste0("Waiting Time (sec)")) + xlab("Time")
    df_plot_12_22_2 <- df_plot_12_22_2 + scale_x_continuous(labels = hms::as_hms(seq(lower_time_int_12, upper_time_int_12,by=3600)), breaks=seq(lower_time_int_12, upper_time_int_12,by=3600))
    df_plot_12_22_2 <- df_plot_12_22_2 + ylim(0, max(df_ped_results_file12$waiting_time))
    df_plot_12_22_2 <- df_plot_12_22_2 + ggtitle(paste0("Pedestrian Waiting Time for ",site," at Pedestrian Movement ",ped_movement," on ",Date_file12_2))
    df_plot_12_22_2 <- df_plot_12_22_2 + theme_calc()
    output$Plot12_2 <- renderPlot({df_plot_12_22_2})
  })
  
  output$downloadData12_1 <- downloadHandler(
    
    filename = function() { 
      paste("Ped_Calls_Waiting_Time", Sys.time(), ".csv", sep="")
    },
    content = function(file12_1) { 
      progress <- shiny::Progress$new()
      # Make sure it closes when we exit this reactive, even if there's an error
      on.exit(progress$close())
      progress$set(message = "Analyizing the data....", value = 0)
      lower_time_12 <- paste0(hms::as_hms(input$file12_2_lower_time)) 
      upper_time_12 <- paste0(hms::as_hms(input$file12_2_upper_time)) 
      input_file_event_12 <- paste0(input$file12_2_event_file$datapath)
      site <- as.integer(input$file12_2_site_ID)
      ped_movement <- as.integer(input$file_12_2_ped_phase_selected)
      
      ped_movement_function2 <- function(df_input, ped_movement){
        df <- readr::read_csv(df_input)
        df <- df[substr(df$Event,1,19)=="Pedestrian movement",]
        df$flag <- 0
        df <- dplyr::filter(df, grepl(paste0(": Ped ",ped_movement,"="),Event) & (grepl("Interval=Walk",Event) | grepl("Interval=Clearance",Event) | grepl(paste0(": Ped ",ped_movement,"=","\\[Demand=On\\]"),Event)))
        df$flag[grepl("Interval=Walk",df$Event)] <- "walk"
        df$flag[grepl("Interval=Clearance",df$Event)] <- "Clearance"
        df$flag[grepl(paste0(": Ped ",ped_movement,"=\\[Demand=On\\]"),df$Event)] <- "DemandOn"
        if(df$flag[1]!="DemandOn"){
          df <- df[-c(1),]
          if(df$flag[1]!="DemandOn"){
            df <- df[-c(1),]
            if(df$flag[1]!="DemandOn"){
              df <- df[-c(1),]
              if(df$flag[1]!="DemandOn"){
                df <- df[-c(1),]
                if(df$flag[1]!="DemandOn"){
                  df <- df[-c(1),]
                }
              }
            }
          }
        }
        df$Start_time <- as.integer(hms::as_hms(substr(df$Time,12,19)))
        df$date <- as.Date(substr(df$Time,1,10))
        df$day <- weekdays(df$date)
        # https://stackoverflow.com/questions/26448533/how-to-restart-a-sequence-based-on-values-in-another-column-or-reference-the-pre
        df$rank <- ave(df$flag, cumsum(df$flag == "DemandOn"), FUN=seq)
        df_tail <- as.data.frame(as.integer(tail(df$Start_time,-1)))
        df_tail <- rbind(0,df_tail)
        colnames(df_tail) <- c("tail")
        df_head <- as.data.frame(as.integer(head(df$Start_time, -1)))
        colnames(df_head) <- c("head")
        df_head <- rbind(0,df_head)
        df$previous_time <- df_tail$tail - df_head$head
        df$waiting_time <- df$previous_time
        df$waiting_time[df$flag != "walk"] <- 0
        # Waiting Time DataFrame
        df_waiting <- df[df$flag == "walk",]
        df_waiting$Start_time <- as.integer(as.integer(df_waiting$Start_time/60)*60)
        df_waiting$flag <- 1
        time_df <- data.frame(seq(60,60*60*24,by=60))
        colnames(time_df) <- c("time")
        df_waiting <- sqldf::sqldf("select b.time, a.Start_time, a.day, a.date, a.waiting_time, a.flag
                    from time_df as b
                    left join df_waiting as a
                    on b.time = a.Start_time", drv = "SQLite")
        df_waiting$Start_time <- NULL
        df_waiting <- df_waiting %>% tidyr::fill(day,.direction = c("downup"))
        df_waiting <- df_waiting %>% tidyr::fill(date,.direction = c("downup"))
        df_waiting$waiting_time[is.na(df_waiting$waiting_time)] <- 0
        df_waiting$flag[is.na(df_waiting$flag)] <- 0
        return(df_waiting)
      }
      
      df_ped_results_file12 <- ped_movement_function2(input_file_event_12, ped_movement)
      df_ped_results_file12 <- df_ped_results_file12 %>% dplyr::filter(time >= as.integer(hms::as_hms(lower_time_12)))
      df_ped_results_file12 <- df_ped_results_file12 %>% dplyr::filter(time <= as.integer(hms::as_hms(upper_time_12)))
      df_ped_results_file12$time <- hms::as_hms(df_ped_results_file12$time)
      write.csv(df_ped_results_file12, file12_1, row.names=FALSE)
      
    })

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