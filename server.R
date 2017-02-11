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
library(chartjs)

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

course <- read.table(file = "https://raw.githubusercontent.com/yeukyul/datasets/master/course_fall.tsv", sep="\t", header=TRUE, fill = TRUE)

schools <- c(cfa, scs)
schools_names <- c("CFA", "SCS", "MCS")
school <- scs 
instructor <- scs_instr

fall <- read.table(file = "https://raw.githubusercontent.com/yeukyul/datasets/master/course_fall.tsv", sep="\t", header=TRUE, fill = TRUE)
spring <- read.table(file = "https://raw.githubusercontent.com/yeukyul/datasets/master/course_spring.tsv", sep="\t", header=TRUE, fill = TRUE)
summer1<- read.table(file = "https://raw.githubusercontent.com/yeukyul/datasets/master/course_summer1.tsv", sep="\t", header=TRUE, fill = TRUE)
summer2 <- read.table(file = "https://raw.githubusercontent.com/yeukyul/datasets/master/course_summer2.tsv", sep="\t", header=TRUE, fill = TRUE)
catelog <- c("Summer 1 16", "Summer 2 16", "Fall 16", "Spring 17")

schedule <- fall

server <- function(input, output, session) {
   
   
   #############################
   # tab: course
   #############################
   
   parse_courseID <- function(str) {
      return (substr(str, 1, 5))
   }
   
   
   # Course: time series for enrollment
   output$num_enrollment <- renderPlotly({
      
      stats1 <- school[which(school$course.id == parse_courseID(input$course1)), c("course.id", "year.term", "enrollment"),]
      
      if (input$course2 != "-") {
         stats2 <- school[which(school$course.id == parse_courseID(input$course2)), c("course.id", "year.term", "enrollment"),]
         together <- data.frame(rbind(stats1, stats2))
      }
      else {
         together <- stats1
      }
      
      if (nrow(stats) <= 1) {
         plot.t = "Not enough data to show overall enrollment number change"
      } else {
         plot.t = "Change of Number of Enrollment"
      }
      x <- list(title = "Semester")
      y <- list(title = "Overall Rating")
      
      ord <- order(together$year.term, decreasing = TRUE)

      p <- plot_ly(together[ord,], x = ~as.numeric(year.term), y = ~as.numeric(enrollment), color = ~as.factor(course.id)
                   ,name = 'Overall Enrollment in Class', type = 'scatter', mode = 'lines') %>%
         layout(xaxis = x, yaxis = y, title = plot.t) %>%
         layout(legend = list(orientation = 'h'))    
      ggplotly(p)
   })
   
   # Course: time series for overall teaching
   output$overall_teaching <- renderPlotly({
      
      stats1 <- school[which(school$course.id == parse_courseID(input$course1)), c("course.id", "year.term", "overall.teaching")]
      
      if (input$course2 != "-") {
         stats2 <- school[which(school$course.id == parse_courseID(input$course2)), c("course.id", "year.term", "overall.teaching"),]
         together <- data.frame(rbind(stats1, stats2))
      }
      else {
         together <- stats1
      }
      ord <- order(together$year.term, decreasing = TRUE)
      together <- together[ord, ]
      
      if (nrow(stats1) <= 1) {
         plot.t = "Not enough data to show overall teaching rating change"
      } else {
         plot.t = "Change of Overall Teaching Rating"
      }
      x <- list(title = "Semester")
      y <- list(title = "Overall Teaching")
      
      p <- plot_ly(together, x = ~as.numeric(year.term), y = ~as.numeric(overall.teaching), color = ~as.factor(course.id), 
                   name = 'Overall Teaching', type = 'scatter', mode = 'lines') %>%
         layout(xaxis = x, yaxis = y, title = plot.t) %>%
         layout(legend = list(orientation = 'h'))   
      ggplotly(p)
   })
   
   # Course: time series for course rating
   output$overall_course <- renderPlotly({
      
      stats1 <- school[which(school$course.id == parse_courseID(input$course1)), c("course.id", "year.term", "overall.course")]
      
      if (input$course2 != "-") {
         stats2 <- school[which(school$course.id == parse_courseID(input$course2)), c("course.id", "year.term", "overall.course"),]
         together <- data.frame(rbind(stats1, stats2))
      }
      else {
         together <- stats1
      }
      ord <- order(together$year.term, decreasing = TRUE)
      together <- together[ord, ]
      
      if (nrow(stats1) <= 1) {
         plot.t = "Not enough data to show overall course rating change"
      } else {
         plot.t = "Change of Overall Course Rating"
      }
      x <- list(title = "Term")
      y <- list(title = "Overall Rating")
      p <- plot_ly(together, x = ~year.term, y = ~as.numeric(overall.course), color = ~as.factor(course.id),
                   name = 'Overall Course', type = 'scatter', mode = 'lines') %>%
         layout(xaxis = x, yaxis = y, title = plot.t) %>%
         layout(legend = list(orientation = 'h'))   
      ggplotly(p)
   })
   
   # Course: time series for hour spend
   output$hrs_per_week <- renderPlotly({
      
      stats1 <- school[which(school$course.id == parse_courseID(input$course1)), c("course.id", "year.term", "hrs.per.week")]
      
      if (input$course2 != "-") {
         stats2 <- school[which(school$course.id == parse_courseID(input$course2)), c("course.id", "year.term", "hrs.per.week"),]
         together <- data.frame(rbind(stats1, stats2))
      }
      else {
         together <- stats1
      }
      ord <- order(together$year.term, decreasing = TRUE)
      together <- together[ord, ]
      
      if (nrow(stats1) <= 1) {
         plot.t = "Not enough data to show hours of week change"
      } else {
         plot.t = "Change of Hours per Week"
      }
      x <- list(title = "Term")
      y <- list(title = "Hrs/ Week")
      p <- plot_ly(together, x = ~year.term, y = ~as.numeric(hrs.per.week), color = ~as.factor(course.id),
                   name = 'Hrs Per Week', type = 'scatter', mode = 'lines') %>%
         layout(xaxis = x, yaxis = y, title = plot.t) %>%
         layout(legend = list(orientation = 'h'))   
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
   
   
   # Course: course table
   output$courseTable <- DT::renderDataTable(DT::datatable({
      data <- school[which(school$course.id == parse_courseID(input$course)), c("year", "term", "instructor", "hrs.per.week", "overall.course")]
      data <- data[order(as.numeric(data$year), decreasing = TRUE),]
      names(data) <- c("Year", "Term", "Instructor", "Hours Per Week", "Overall Course")
      data
   }))

   # Course: compare time spend acorss time
   
   
   ############
   # tab: schedule
   ############
   

   
   output$endTime <- renderText({

      ind <- which(as.character(schedule$Course.number) == substr(input$courseSchedule, 1,6))
      courseSelected <- schedule[ind,]
      print(courseSelected)
      as.character(courseSelected$End.time)
      
   })
   
   output$location <- renderText({
      "You have selected this"
   })
   output$courseNumber <- renderText({
      "You have selected this"
   })
   output$courseName <- renderText({
      "You have selected this"
   })
   output$units <- renderText({
      "You have selected this"
   })
   output$department <- renderText({
      "You have selected this"
   })
   output$description <- renderText({
      "You have selected this"
   })
   output$instructors <- renderText({
      "You have selected this"
   })
   output$days <- renderText({
      "You have selected this"
   })
   output$startTime <- renderText({
      "You have selected this"
   })
   
   output$text1 <- renderText({ 
      
   })
   
   #############
   # tab: faculty
   ############
   
   # Faculty: time series for overall teaching
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
         add_trace(line = list(color = 'rgb(255, 0, 0)', width = 4)) %>%
         layout(xaxis = x, yaxis = y, title = plot.t)
      ggplotly(p)
   })
   
   output$facultyTable <- DT::renderDataTable(DT::datatable({
      data <- instructor[which(instructor$Instructor == input$faculty & instructor$Course != 0), ]
      data <- data[order(as.numeric(data$Course)),c("Course", "Hours.per.week", "Overall.teaching", "Overall.course")]
      names(data) <- c("Course", "Hrs Per Week", "Overall Teaching", "Overall Course")
      data
   }))
   
   # gets faculty basic stat by passing in related row of that faculty
   get_faculty_stats <- function(df) {
      return (t(df[, 4:11]))
   }
   
   # Dataset: show table of dataset
   output$fcetable <- renderDataTable({
      school[4:14]
   })
      
}

shinyServer(server)