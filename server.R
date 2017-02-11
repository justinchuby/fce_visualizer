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


# Reading in Data
schools <- c("cfa", "scs")

var_names <- c( "responses", "enrollment", "resp.rate", "hrs.per.week", "interst.in.student.learning", 
                "exp.goals", "clarity", "provide.feedback","importance" ,"exp.subject",
                "respect" ,"overall.teaching", "overall.course")

cfa <- read.csv("https://raw.githubusercontent.com/yeukyul/datasets/master/fce_cfa.csv", stringsAsFactors = F)
cfa <- 
cfa.overall <- cfa[1,]
cfa <- cfa[-1, ]
cfa <- plyr::adply(cfa[, var_names], 2, as.numeric)

scs <- read.csv("https://raw.githubusercontent.com/yeukyul/datasets/master/fce_scs.csv", stringsAsFactors = F)
scs.overall <- scs[1,]
scs <- scs[-1, ]
inds.wu <- which(scs$section %in% c("W", "U", "W1", "W2", "X"))
scs <- scs[-inds.wu,]


scs_instr <- read.table(file = "https://raw.githubusercontent.com/yeukyul/datasets/master/instr_scs.tsv", sep="\t", header=TRUE)

schools <- c(cfa, scs)
schools_names <- c("CFA", "SCS", "MCS")
school <- scs 
instructor <- scs_instr


server <- function(input, output, session) {
   
   
   #############################
   # tab: course
   #############################
   
   parse_courseID <- function(str) {
      return (substr(str, 1, 5))
   }
   
   get_term_letter <- function(semester) {
      if (semester == 0.1) {
         return("s")
      }
      if (semester == 0.8) {
         return ("f")
      }
      if (semester == 0.6) {
         return ("su")
      }
      return ("")
   }
   
   parse_x_axis <- function(terms) {
      labels = c()
      for (i in 1:length(terms)) {
         term = terms[i]
         year = round(terms)
         semester = term - year
         thislabel = paste(as.character(year %% 100), get_term_letter(semester), sep = "")
         labels = c(labels, thislabel)
      }
      return (labels)
   }
   
   
   # Course: time series for enrollment
   output$num_enrollment <- renderPlotly({
      
      stats <- school[which(school$course.id == parse_courseID(input$course)), c("year.term", "enrollment"),]
      ord <- order(stats$year.term)
      stats <- stats[ord, ]
      
      if (nrow(stats) <= 1) {
         plot.t = "Not enough data to show overall enrollment number change"
      } else {
         plot.t = "Change of Number of Enrollment Rating"
      }
      x <- list(title = "Term")
      y <- list(title = "Overall Rating")
      p <- plot_ly(stats, x = ~year.term, y = ~enrollment, name = 'Number of enrollment', type = 'scatter', mode = 'lines') %>%
         layout(xaxis = x, yaxis = y, title = plot.t)
      ggplotly(p)
   })
   
   # Course: time series for overall teaching
   output$overall_teaching <- renderPlotly({
      
      stats <- school[which(school$course.id == parse_courseID(input$course)), c("year.term", "overall.teaching")]
      ord <- order(stats$year.term)
      stats <- stats[ord, ]
      
      if (nrow(stats) <= 1) {
         plot.t = "Not enough data to show overall teaching rating change"
      } else {
         plot.t = "Change of Overall Teaching Rating"
      }
      x <- list(title = "Term")
      y <- list(title = "Overall Teaching")
      p <- plot_ly(stats, x = ~as.numeric(year.term), y = ~as.numeric(overall.teaching), 
                   name = 'Overall Teaching', type = 'scatter', mode = 'lines') %>%
         layout(xaxis = x, yaxis = y, title = plot.t)
      ggplotly(p)
   })
   
   # Course: time series for course rating
   output$overall_course <- renderPlotly({
      
      stats <- school[which(school$course.id == parse_courseID(input$course)), c("year.term", "overall.course")]
      ord <- order(stats$year.term)
      stats <- stats[ord, ]
      
      if (nrow(stats) <= 1) {
         plot.t = "Not enough data to show overall course rating change"
      } else {
         plot.t = "Change of Overall Course Rating"
      }
      x <- list(title = "Term")
      y <- list(title = "Overall Rating")
      p <- plot_ly(stats, x = ~year.term, y = ~as.numeric(overall.course), name = 'Overall Course', type = 'scatter', mode = 'lines') %>%
         layout(xaxis = x, yaxis = y, title = plot.t)
      ggplotly(p)
   })
   
   # Course: time series for hour spend
   output$hrs_per_week <- renderPlotly({
      
      stats <- school[which(school$course.id == parse_courseID(input$course)), c("year.term", "hrs.per.week")]
      ord <- order(stats$year.term)
      stats <- stats[ord, ]
      
      if (nrow(stats) <= 1) {
         plot.t = "Not enough data to show hours of week change"
      } else {
         plot.t = "Change of Hours per Week"
      }
      x <- list(title = "Term")
      y <- list(title = "Hrs/ Week")
      p <- plot_ly(stats, x = ~year.term, y = ~as.numeric(hrs.per.week), name = 'Hrs Per Week', type = 'scatter', mode = 'lines') %>%
         layout(xaxis = x, yaxis = y, title = plot.t)
      ggplotly(p)
   })
   
   # might not make sense
   # Course: radar chart for course comparison
   output$faculty_radar <- renderChartJSRadar({
      
      ind1 <- which(instructor$Instructor == input$faculty1 & (instructor$Course == 0))
      ind2 <- which((instructor$Instructor == input$faculty2) & (instructor$Course == 0))
      name1 <- instructor$Instructor[ind1]
      name2 <- instructor$Instructor[ind2]
      
      labs <- c("Interest in Student Learning", 
                "Explanation Course Requirement", "Clear Learning Gaols", "Provide Feedbacks",
                "Importance of Subject" ,"Explains Subject Matter",
                "Respect to Student" ,"Overall Teaching")
      
      scores <- list(name1 = get_faculty_stats(instructor[ind1, ]),
                     name2 = get_faculty_stats(instructor[ind2, ]))
      
      # render radar chart
      chartJSRadar(scores = scores, labs = labs, maxScale = 5, showToolTipLabel = TRUE)
   })
   
   
   
   #############
   # tab: faculty
   ############
   
   # Course: time series for overall teaching
   output$faculty_teaching <- renderPlotly({
      
      stats <- school[which(school$instructor == input$faculty), c("year.term", "overall.teaching")]
      ord <- order(stats$year.term)
      stats <- stats[ord, ]
      
      if (nrow(stats) <= 1) {
         plot.t = "Not enough data to show overall teaching rating change"
      } else {
         plot.t = "Change of Overall Teaching Rating"
      }
      x <- list(title = "Term")
      y <- list(title = "Overall Teaching")
      p <- plot_ly(stats, x = ~as.numeric(year.term), y = ~as.numeric(overall.teaching), 
                   name = 'Overall Teaching', type = 'scatter', mode = 'lines') %>%
         layout(xaxis = x, yaxis = y, title = plot.t)
      ggplotly(p)
   })
   
   # gets faculty basic stat by passing in related row of that faculty
   get_faculty_stats <- function(df) {
      return (df[, 4:11])
   }
   
   # Dataset: show table of dataset
   output$fcetable <- renderDataTable({
      school[4:14]
   })
      
}

shinyServer(server)