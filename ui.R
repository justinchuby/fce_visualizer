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
library(stringi)


# Reading in Data by each school

clean_schoool_tsv <- function(url) {
   df <- read.table(file = url, sep="\t", header=TRUE)
   df <- df[-1, ]
   section.inds <- which(df$section %in% c("W", "U", "W1", "W2", "X", "", "W3", "W4", "Section"))
   df <- df[-section.inds,]
   
   #space.inds <- grep("^[::space::]", df$course.name)
   #df <- df[- spaceinds,]
   df
}

clean_school_csv <- function(url) {
   df <- read.csv(url, stringsAsFactors = F, as.is = TRUE)
   df <- df[-1, ]
   section.inds <- which(df$section %in% c("W", "U", "W1", "W2", "X", "", "W3", "W4", "Section"))
   df <- df[-section.inds,]

   df
}
schools <- c("cfa", "scs")

cfa <- clean_school_csv("https://raw.githubusercontent.com/yeukyul/datasets/master/fce_cfa.csv")

scs <- read.csv("https://raw.githubusercontent.com/yeukyul/datasets/master/fce_scs.csv", stringsAsFactors = F, as.is = TRUE)
scs.overall <- scs[1,]
scs <- scs[-1, ]


scs_instr <- read.table(file = "https://raw.githubusercontent.com/yeukyul/datasets/master/instr_scs.tsv", sep="\t", header=TRUE)

schools <- c(cfa, scs)
schools_names <- c("CFA - College of Fine Arts", 
                   "SCS - School of Computer Science", 
                   "MCS - Mellon College of Science", 
                   "DC - Dietrich College of H&SS")
school <- scs 
instructor <- scs_instr

fall <- read.table(file = "https://raw.githubusercontent.com/yeukyul/datasets/master/course_fall.tsv", sep="\t", header=TRUE, fill = TRUE,
                   stringsAsFactors = FALSE)
spring <- read.table(file = "https://raw.githubusercontent.com/yeukyul/datasets/master/course_spring.tsv", sep="\t", header=TRUE, fill = TRUE,
                     stringsAsFactors = FALSE)
summer1<- read.table(file = "https://raw.githubusercontent.com/yeukyul/datasets/master/course_summer1.tsv", sep="\t", header=TRUE, fill = TRUE,
                     stringsAsFactors = FALSE)
summer2 <- read.table(file = "https://raw.githubusercontent.com/yeukyul/datasets/master/course_summer2.tsv", sep="\t", header=TRUE, fill = TRUE,
                      stringsAsFactors = FALSE)
catelog <- c("Summer 1 16", "Summer 2 16", "Fall 16", "Spring 17")


schedule <- fall

# dashboard for app
sidebar <- dashboardSidebar(
   sidebarMenu(
      menuItem("Courses", tabName = "course", icon = icon("book")),
      menuItem("Faculty", tabName = "faculty", icon = icon("user-circle")),
      menuItem("Schedule", tabName = "schedule", icon = icon("calendar-o")),
      menuItem("Dataset", tabName = "datasets", icon = icon("table"))
   )
)

# main UI function 
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
         
         # tab: schedule
         tabItem(tabName = "schedule",
                 h2("Scheduling Classes"), tags$br(),
                 fluidRow(
                    box(
                       h3("Choose classes to add"),
                       helpText("Search and selected a course to add to total."),
                       selectInput(inputId = 'term', label = 'Select Term', 
                                   choices = catelog, 
                                   selected = "Spring 17"),
                       selectInput(inputId = 'courseSchedule', label = 'Select Course', 
                                   choices = sort(paste(as.character(schedule$Course.number), as.character(schedule$Course.name), sep = " - ")), 
                                   selected = "02-201 Programming for Scientists"),
                       h3(textOutput("courseName")),
                       h5(textOutput("courseNumber")),
                       strong(textOutput("units")),
                       strong(textOutput("instructors")),
                       strong(textOutput("time")),
                       p(textOutput("description")),
                       actionButton("addCourse", "+ Add Course")
                    ),
                    box(
                       h3("Selected Classes"),
                       helpText("Classes chosen on the left panel will be displayed here."),
                       fluidRow(
                          column(
                             DT::dataTableOutput("scheduleTable"), width = 12
                          )
                       ),
                      
                       h4(textOutput("totalListedTime")),
                       h4(textOutput("totalTime")),
                       actionButton("removeAll", "Clear All")
                       
                    )
                 )
         ),
         
         # tab: dataset 
         tabItem(tabName = "datasets",
                 h2('FCE Raw Data'), tags$br(),
                 selectInput(inputId = 'school_tab', label = 'Select Datasets', 
                             choices = c("Courses", "Professors"), 
                             selected = "scs"),
                 h3(dataTableOutput('fcetable'))
         ),
         
         # tab: faculty
         tabItem(tabName = "faculty",
                 h2("Faculty Profile"), tags$br(),
                 p("Explore what a faculty member has taught before and what opinions everybody gives."),
                 fluidRow(
                    box(
                       h4("Faculty Profile"),
                       helpText("Select one faculty members to view faculty basic statistics."), tags$br(),
                       selectInput(inputId = 'faculty', label = 'Select a Faculty Member', 
                                   choices = sort(as.character(unique(instructor$Instructor))), 
                                   selected = "ANIL ADA"),
                       tabsetPanel(
                          tabPanel("Overall Teaching",
                                   plotlyOutput(outputId = "faculty_teaching", height = "400px", width = "600px"),
                                   helpText()
                          ),
                          tabPanel("Teaching History",
                                   fluidRow(
                                      column(
                                         DT::dataTableOutput("facultyTable"), width = 12
                                      )
                                   )
                          ),
                          tabPanel("R*** My Professor",
                                   h3("Comment about this professor on Rate my Professor:"),
                                   strong(textOutput("rateRating")),
                                   h4(textOutput("rateComment"))
                                   )
                       )
                    ),
                    box(
                       h4("Faculty Comparison"), 
                       helpText("Select two faculty members to view faculty statistics comparisons."), tags$br(),
                       selectInput(inputId = 'faculty1', label = 'Select Faculty 1', 
                                   choices = sort(as.character(unique(school$instructor))), 
                                   selected = "DAVID KOSBIE"),
                       selectInput(inputId = 'faculty2', label = 'Select Faculty 2', 
                                   choices = sort(as.character(unique(school$instructor))), 
                                   selected = "DAVID KOSBIE"),
                       chartJSRadarOutput("faculty_radar", width = "450", height = "300")
                    )
                 )
         ),
         
         tabItem(tabName = "course",
                 h2("Course Profile"), tags$br(),
                 p("Explore what how a course hour spend per week, overall course rating, and overall teaching has changed overtime. 
                   You can compare between two courses and find out who taught this course before."),
                 fluidRow(
                    # Course Basic Stats
                    box(
                       h4("Course History"), 
                       helpText("Select course to view teaching history, past course evaluations."), tags$br(),
                       selectInput(inputId = 'course', label = 'Select Course Number', 
                                   choices = sort(as.character(unique(school$id.name))), 
                                   selected = "15112 - FNDMTLS OF PGMG & CS"),
                       fluidRow(
                          column(
                             DT::dataTableOutput("courseTable"), width = 12
                          )
                       )
                                   
                  ),
                  
                  box(
                     h4("Course Comparison"), 
                     helpText("Select two courses to view course comparisons."), tags$br(),
                     selectInput(inputId = 'course1', label = 'Select Course 1', 
                                 choices = sort(unique(school$id.name)), 
                                 selected = "15112 - FNDMTLS OF PGMG & CS"),
                     selectInput(inputId = 'course2', label = 'Select Course 2', 
                                 choices = sort(unique(school$id.name)), 
                                 selected = "-"),
                     tabsetPanel(
                        tabPanel("Overall Course Rating",
                                 plotlyOutput(outputId = "overall_course", height = "400px", width = "600px")
                        ),
                        
                        tabPanel("Overall Teaching",
                                 plotlyOutput(outputId = "overall_teaching", height = "400px", width = "600px"),
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
                     ),
                     helpText("Semesters are encoded as year, with start month behind year to indicate start month of the semester."),
                     helpText("spring term = year.1, summer term = year.6, fall term = year.8")
                  )
               )
            )
      )
   )
)




shinyUI(ui)