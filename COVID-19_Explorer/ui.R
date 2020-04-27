library(shiny)
library(plotly)
library(shinydashboard)
library(shinymaterial)
library(leaflet)
library(dplyr)
library(leaflet.extras)
library(shinycssloaders) 
library(hrbrthemes)
library(dashboardthemes)

# Adding image or logo along with the title in the header
title <- tags$a(tags$img(src="img/COV_1.jpg", height = "auto", width = "auto"),
                'COVID-19 Explorer', target="_blank")
### creating custom logo object

shinyUI(
  
  dashboardPage(
    dashboardHeader(title = title, titleWidth = 400,
                    tags$li(class="dropdown",tags$a(href="https://www.linkedin.com/in/apurvatawde/" ,icon("linkedin"), "My Profile", target="_blank")),
                    tags$li(class="dropdown",tags$a(href="https://github.com/Apurvat07/R-Shiny_Projects", icon("github"), "Source Code", target="_blank"))),
    dashboardSidebar(
      sidebarMenu(
        menuItem(text = "World Map", tabName = "example_side_nav_tab_1", icon=icon("map")),
        menuItem("History", tabName = "example_side_nav_tab_2", icon=icon("history"))
      )
    ),
    dashboardBody(
      shinyDashboardThemes(
        theme = "blue_gradient"
      ),
      tabItems(
        tabItem(tabName = "example_side_nav_tab_1", 
                bootstrapPage(
                  
                  #tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
                  leafletOutput("mymap",width="100%",height="1000px"),
                  absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE ,
                                draggable = TRUE , top = 100, left = "auto", right = 20, bottom = "auto",
                                width = "auto", height = "auto",
                                
                                h4("Covid-19 Cases explorer by Countries"),
                                
                                withSpinner(uiOutput("vx1")),
                                withSpinner(plotOutput("plot3",width = "auto",height="auto"))
                  )
                )
        ),
        tabItem(tabName = "example_side_nav_tab_2", 
                fluidPage(
                  fluidRow(
                    column(
                      4,uiOutput("vx")
                    )
                  ),
                  fluidRow(
                    
                    column(5,withSpinner(imageOutput("plot1"))),
                    column(5,withSpinner(imageOutput("plot2")))
                  )
                  
                ))
      )
      # add reference to CSS file
      # ensure that CSS file is in the www folder of the working directory
      
    )
  )
)