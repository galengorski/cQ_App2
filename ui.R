#===================================================================================#
# NOTES: user interface for EventPickerApp2.0
#-----------------------------------------------------------------------------------#
# Galen Gorski                                                                      #
# ggorski@ucsc.edu
# 04-30-2020
#-----------------------------------------------------------------------------------#
#===================================================================================#

#===================================================================================#
#####INSTALL PACKAGES#####
#install.packages('shiny')
library(shiny)
#install.packages('shinythemes')
library(shinythemes)
#install.packages('dataRetrieval')
library(dataRetrieval)
#install.packages('Hmisc')
library(Hmisc)
#install.packages('ggplot2')
library(ggplot2)
#install.packages('shinyWidgets')
library(shinyWidgets)
#install.packages('tidyverse')
library(tidyverse)
#install.packages('plyr')
library(plyr)
#install.packages('dygraphs')
library(dygraphs)
#install.packages('xts')
library(xts)
#install.packages('plotly')
library(plotly)
#install.packages('leaflet')
library(leaflet)
#install.packages('foreign')
library(foreign)



#Run this script of functions
source('event_picker_functions.R')
#####
#===================================================================================#

fluidPage(
  #select theme
  theme = shinytheme("cerulean"),
  # Give the page a title
  titlePanel("EventPicker"),
  tabsetPanel(type = "tabs",
              tabPanel("Query USGS NWIS for data",
                       # Generate a row with a sidebar
                       sidebarLayout(      
                         # Define the sidebar with one input
                         sidebarPanel(
                           h3('1) Choose a USGS gaging station'),
                           h5('Currently the sites available for selection are those with daily nitrate values (code = 99133), and daily discharge values (code = 00060)'),
                           selectInput("sitename", label = h5("Select the site number below"), selected = 'North Raccoon River near Sac City, IA', choices = c(readRDS('./Data/sitenames.rds'))),
                           h4('Available data'),  
                           div(id = "site_data_sum_div",
                                 verbatimTextOutput("site_data_sum")),
                           h6('00060 = discharge (cfs), 00010 = temperature (C), 99133 = NO3+NO2 (mg/L), ',a(href ="https://nwis.waterdata.usgs.gov/usa/nwis/pmcodes", 'click here'), 'for a complete listing of parameter codes'),
                           h3("2) Select date range"),
                           h5("Select a date range that you would like to download data for, all available datasets will be downloaded for the selected range."),
                           uiOutput("date_range_div"),
                           h3('3) Download site data'),
                           h5('Click the button below to download site data for the date range selected. The data will be plotted to the right. Data will be stored remotely with options for download coming soon.'),
                           actionButton("download_data",label = "Download site data"),
                           #div(strong("To: "), textOutput("to", inline = TRUE)),
                           hr(),
                           h4('Site locations'),
                           leafletOutput('map')
                           ),
                         mainPanel(
                           dygraphOutput("plot1"),
                           fluidRow(
                             column(4,
                                    uiOutput("y_axis")),
                             column(4,
                                    uiOutput('second_y_axis')),
                             column(4,
                                    checkboxInput("second_y_check", label = 'Plot secondary time series', value = T))),
                           hr(),
                           h3('4) Concentration-discharge relationships'),
                           h5('The plots below show the relationship beetween log(nitrate concentration) and log(discharge) with a linear best fit for all the data and broken up by season. To view a subset of the record, use the slider bar above to select a range of dates then click the "recalculate c-Q relationships" button below.'),
                           actionButton('cq_recalc', 'Recalculate c-Q relationships'),
                           fluidRow(
                             column(6,
                                    plotOutput('plot2', height = '400px'),
                                    tableOutput('allseason.table')
                                    ),
                             column(6,
                                    fluidRow(
                                      column(6,
                                             plotOutput('plotOND', height = '200px'),
                                             plotOutput('plotAMJ', height = '200px')),
                                      column(6,
                                             plotOutput('plotJFM', height = '200px'),
                                             plotOutput('plotJAS', height = '200px'))
                                             ),
                                    tableOutput('seasonal.table')
                                    ))
                                   )
                         )),
              #PEAK PICKING PANEL
              tabPanel("Pick events",
                       # Generate a row with a sidebar
                       sidebarLayout(      
                         # Define the sidebar with one input
                         sidebarPanel(
                           h3('5) Separate the hydrograph into events and baseflow'),
                           h5("Using a generic peak picking algorithm, this will select the storm events in the hydrograph. The inputs below allow some manipulation of the peak picking algorithm."),
                           fluidRow(
                             column(6,
                                    numericInput("sb_pk_thresh_val", label = 'Slope back threshold (default = 1e-06)', value = 0.000001)),
                             column(6,
                                    numericInput("sf_pk_thresh_val", label = 'Slope forward threshold (default = 0)', value = 0)),
                             h6('The event peak is identified first and the back and forward slope thresholds are used to identify the beginning and end of each event.')
                                    ),
                           fluidRow(
                             column(4,
                                    numericInput("event_rise_val", label = 'Rise (default = 0.001)', value = 0.001),
                                    h6('In cfs/second')),
                             column(4,
                                    numericInput("peak_top_thresh_val", label = 'Peak (default = 0.1)', value = 0.1),
                                    h6('As a fraction of max flow')),
                             column(4,
                                    numericInput("event_length_thresh_val", label = 'Length (default = 1)', value = 1),
                                    h6('In days'))
                                  ),
                           actionButton("event_scan", "SCAN FOR EVENTS")
                            ),
                         mainPanel(
                           dygraphOutput('plot3', height = '600px'))),
                           #uiOutput("y_axis"),
                           hr(),
                           h3('6) Concentration-discharge relationships for different flow regimes'),
                       fluidRow(
                         column(6,
                                fluidRow(
                                  column(6,
                                    plotOutput('baseflow', height = '400px')),
                                  column(6,
                                    tableOutput('baseflowtable'))
                                  )
                         ),
                         column(6,
                                fluidRow(
                                  column(4,
                                         plotOutput('plotbfOND', height = '200px'),
                                         plotOutput('plotbfAMJ', height = '200px')),
                                  column(4,
                                         plotOutput('plotbfJFM', height = '200px'),
                                         plotOutput('plotbfJAS', height = '200px')),
                                  column(4,
                                         tableOutput('seasonalbaseflowtable'))
                                )
                                
                         )),
                       fluidRow(
                         column(6,
                                fluidRow(
                                  column(6,
                                         plotOutput('stormflow', height = '400px')),
                                  column(6,
                                         tableOutput('stormflowtable'))
                                  )
                                ),
                         column(6,
                                fluidRow(
                                  column(4,
                                         plotOutput('plotsfOND', height = '200px'),
                                         plotOutput('plotsfAMJ', height = '200px')),
                                  column(4,
                                         plotOutput('plotsfJFM', height = '200px'),
                                         plotOutput('plotsfJAS', height = '200px')),
                                  column(4,
                                         tableOutput('seasonalstormflowtable'))
                                )
                                
                         ))
                           )
              ))