library(shiny)
library(leaflet)
library(RColorBrewer)
library(ggplot2)
library(datasets)
library(metricsgraphics)
library(dplyr)
library(plotly)
library(data.table)
library(shinydashboard)
library(radarchart)
library(DT)


# Reading in Data by each school
schools <- c("cfa", "scs")

cfa <- read.csv("https://raw.githubusercontent.com/yeukyul/datasets/master/fce_cfa.csv", stringsAsFactors = F, as.is = TRUE)
cfa.overall <- cfa[1,]
cfa <- cfa[-1, ]

scs <- read.csv("https://raw.githubusercontent.com/yeukyul/datasets/master/fce_scs.csv", stringsAsFactors = F, as.is = TRUE)
scs.overall <- scs[1,]
scs <- scs[-1, ]


scs_instr <- read.table(file = "https://raw.githubusercontent.com/yeukyul/datasets/master/instr_scs.tsv", sep="\t", header=TRUE)

schools <- c(cfa, scs)
schools_names <- c("CFA", "SCS", "MCS")
school <- scs 
instructor <- scs_instr


# GGPlot theme
yeukyul_315_theme <- theme(
   plot.title = element_text(margin = margin(0, 0, 10, 0),
                             hjust = 0.5),
   legend.key = element_rect(fill = "white"),
   legend.background = element_rect(fill = "white"),
   panel.grid.major = element_line(colour = "white"),
   panel.grid.minor = element_blank(),
   axis.text.x = element_text(size = rel(0.7)),
   axis.text.y = element_text(size = rel(0.7)),
   axis.title.y = element_text(margin = margin(0, 10, 0, 0)),
   axis.title.x = element_text(margin = margin(10, 0, 0, 0))
)


sidebar <- dashboardSidebar(
   sidebarMenu(
      menuItem("Courses", tabName = "course", icon = icon("book")),
      menuItem("Faculty", tabName = "faculty", icon = icon("user-circle")),
      menuItem("Trending", tabName = "trend", icon = icon("line-chart")),
      menuItem("Dataset", tabName = "datasets", icon = icon("table"))
   )
)


ui <- dashboardPage(
   skin = "black",
   dashboardHeader(title = "FCE @ CMU"),
   sidebar,
   dashboardBody(
      tags$head(
         # Include our custom CSS
         includeCSS("www/styles.css")
      ),
      tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
      tabItems(
         
         tabItem(tabName = "course",
                 h2("Course Profile"), tags$br(),
                 fluidRow(
                    # Course Basic Stats
                    box(
                       h4("Course Comparison"), 
                       helpText("Select two courses to view courses statistics comparisons."), tags$br(),
                       selectInput(inputId = 'course', label = 'Select Course Number', 
                                   choices = as.character(unique(school$id.name)), 
                                   selected = "15112 - FNDMTLS OF PGMG"),
                       tabsetPanel(
                          tabPanel("Overall Course Rating",
                                   plotlyOutput(outputId = "overall_course", height = "400px", width = "600px")
                          ),
                          
                          tabPanel("Overall Teaching",
                                   plotlyOutput(outputId = "research_teaching", height = "400px", width = "600px"),
                                   helpText()
                          ),
                          
                          tabPanel("Hours Per Week",
                                   plotlyOutput(outputId = "hrs_per_week", height = "400px", width = "600px"),
                                   helpText("Hours per week is calculated as an aggregate mean of student reported hours.")
                          ),
                          
                          tabPanel("Number of Enrollment",
                                   plotlyOutput(outputId = "num_enrollment", height = "400px", width = "600px"),
                                   helpText("Hours per week is calculated as an aggregate mean of student reported hours.")
                          )
                      
                      )
                  ),
                  
                  box(
                     h4("Faculty Comparison"), 
                     helpText("Select two faculty members to view faculty statistics comparisons."), tags$br(),
                     selectInput(inputId = 'faculty1', label = 'Select Faculty 1', 
                                 choices = as.character(unique(school$instructor)), 
                                 selected = "DAVID KOSBIE"),
                     selectInput(inputId = 'faculty2', label = 'Select Faculty 2', 
                                 choices = as.character(unique(school$instructor)), 
                                 selected = "DAVID KOSBIE"),
                     chartJSRadarOutput("faculty_radar", width = "450", height = "300")
                 )
               )
            )
         )
      )
   )




shinyUI(ui)