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

course <- read.table(file = "https://raw.githubusercontent.com/yeukyul/datasets/master/course_fall.tsv", sep="\t", header=TRUE, fill = TRUE)
fce_all <- read.csv("https://raw.githubusercontent.com/yeukyul/datasets/master/fce_all_modified.csv", stringsAsFactors = F)

scs_instr <- read.table(file = "https://raw.githubusercontent.com/yeukyul/datasets/master/instr_scs.tsv", sep="\t", header=TRUE)
all_instr <- read.table(file = "https://raw.githubusercontent.com/yeukyul/datasets/master/instr_all.tsv", sep="\t", header=TRUE)

inds.wu <- which(fce_all$section %in% c("W", "U", "W1", "W2", "X"))
fce_all <- fce_all[-inds.wu, ]

schools <- c(cfa, scs)
schools_names <- c("CFA - College of Fine Arts", 
                   "SCS - School of Computer Science", 
                   "MCS - Mellon College of Science", 
                   "DC - Dietrich College of H&SS")
school <- fce_all
instructor <- all_instr

fall <- read.table(file = "https://raw.githubusercontent.com/yeukyul/datasets/master/course_fall.tsv", sep="\t", header=TRUE, fill = TRUE)
spring <- read.table(file = "https://raw.githubusercontent.com/yeukyul/datasets/master/course_spring.tsv", sep="\t", header=TRUE, fill = TRUE)
summer1<- read.table(file = "https://raw.githubusercontent.com/yeukyul/datasets/master/course_summer1.tsv", sep="\t", header=TRUE, fill = TRUE)
summer2 <- read.table(file = "https://raw.githubusercontent.com/yeukyul/datasets/master/course_summer2.tsv", sep="\t", header=TRUE, fill = TRUE)
catelog <- c("Summer 1 16", "Summer 2 16", "Fall 16", "Spring 17")

schedule <- fall
chosen <<- data.frame()

rateMyProfessor <- read.table(file = "https://raw.githubusercontent.com/seandkim/tartanhack2017/master/sean/data/rmpID.tsv?token=AV9HXYPBWjTwC6akfcyvV7G87tqwT9Fsks5YqKGkwA%3D%3D", sep="\t", header=TRUE, fill = TRUE,
                              stringsAsFactors = FALSE)

courseSelected = c()

server <- function(input, output, session) {
   
   
   #############################
   # tab: course
   #############################
   
   parse_courseID <- function(str) {
      return (as.integer(substr(str, 1, 5)))
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
      
      if (nrow(stats1) <= 1) {
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
         layout(xaxis = x, yaxis = c(list(range = c(0,5)), y), title = plot.t) %>%
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
         layout(xaxis = x, yaxis = c(list(range = c(0,5)), y), title = plot.t) %>%
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


   ############
   # tab: schedule
   ############
   
   # button listener
   observeEvent(input$addCourse, {
      ind <- which(as.character(schedule$Course.number) == substr(input$courseSchedule, 1,6))[1]
      row <- schedule[ind, c("Course.number", "Course.name", "Units")]
      course.num <- as.numeric(stri_replace_all_fixed(row$Course.number, "-", ""))
      row$Hrs.Wk <- school[which(school$course.id == course.num)[1], c("hrs.per.week")]
      chosen = rbind(chosen, row)
      assign('chosen', chosen,envir=.GlobalEnv)
      reset_courseSelection()
   })
   
   observeEvent(input$removeAll, {
      assign('chosen', data.frame(),envir=.GlobalEnv)
      reset_courseSelection()
   })
   
   reset_courseSelection <- function() {
      output$scheduleTable <- renderDataTable({
         chosen
      })
      reset_totalCount()
      reset_totalListedHour()
   }
   
   reset_totalListedHour <- function(){
      output$totalListedTime<- renderText({
         if (nrow(chosen) == 0) {
            return ("Carried Unit: 0")
         }
         else {
            return(
               paste("Carried Unit: ",
                     as.character(sum(as.numeric(levels(chosen$Units))[chosen$Units]))
               )
            )
         }
         
      })
   }
   
   reset_totalCount <- function(){
      output$totalTime <- renderText({
         if (nrow(chosen) == 0) {
            return ("Total Anticipated Hour: 0")
         }
         else {
            return(
               paste("Total Anticipated Hour: ",
                  as.character(sum(as.numeric(chosen$Hrs.Wk)))
                  )
            )
         }
         
      })
   }
   reset_totalCount()
   reset_totalListedHour()
  
   
   get_course <- function() {
      ind <- which(as.character(schedule$Course.number) == substr(input$courseSchedule, 1,6))
      courseSelected = schedule[ind,]
   }
   
   output$endTime <- renderText({

      ind <- which(as.character(schedule$Course.number) == substr(input$courseSchedule, 1,6))
      courseSelected = schedule[ind,]
      as.character(courseSelected$End.time)
      
   })
   
   output$location <- renderText({
      ind <- which(as.character(schedule$Course.number) == substr(input$courseSchedule, 1,6))
      courseSelected = schedule[ind,]
      as.character(courseSelected$Location)
   })
   output$courseNumber <- renderText({
      ind <- which(as.character(schedule$Course.number) == substr(input$courseSchedule, 1,6))
      courseSelected = schedule[ind,]
      as.character(courseSelected$Course.number)
   })
   output$courseName <- renderText({
      ind <- which(as.character(schedule$Course.number) == substr(input$courseSchedule, 1,6))
      courseSelected = schedule[ind,]
      as.character(courseSelected$Course.name)
   })
   output$units <- renderText({
      ind <- which(as.character(schedule$Course.number) == substr(input$courseSchedule, 1,6))
      courseSelected = schedule[ind,]
      paste(as.character(courseSelected$Units), "units")
   })
   output$department <- renderText({
      ind <- which(as.character(schedule$Course.number) == substr(input$courseSchedule, 1,6))
      courseSelected = schedule[ind,]
      as.character(courseSelected$Department)
   })
   output$description <- renderText({
      ind <- which(as.character(schedule$Course.number) == substr(input$courseSchedule, 1,6))
      courseSelected = schedule[ind,]
      as.character(courseSelected$Description)
   })
   output$instructors <- renderText({
      ind <- which(as.character(schedule$Course.number) == substr(input$courseSchedule, 1,6))
      courseSelected = schedule[ind,]
      as.character(courseSelected$Instructors)
   })
   output$time<- renderText({
      ind <- which(as.character(schedule$Course.number) == substr(input$courseSchedule, 1,6))
      courseSelected = schedule[ind,]
      paste("Class Time: ", 
            as.character(courseSelected$Start.time), "-", 
            as.character(courseSelected$End.time), " | Class Day: ",
            stri_replace_all_fixed(as.character(courseSelected$Day), "<>", ","))
   })
   
   output$scheduleTable <- renderDataTable({
      if (nrow(chosen) == 0 ) {
         chosen
      }
      else {
         chosen[, c("Course.number", "Instructor", "Units")]
      }
   })
   
   
   #############
   # tab: faculty
   ############
   
   # Faculty: time series for overall teaching
   output$faculty_teaching <- renderPlotly({
      
      stats <- school[which(school$instructor == toupper(input$faculty)), c("year.term", "overall.teaching")]
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
         layout(xaxis = x, yaxis = c(list(range = c(0,5)), y), title = plot.t)
      ggplotly(p)
   })
   
   output$facultyTable <- DT::renderDataTable(DT::datatable({
      data <- instructor[which(instructor$Instructor == input$faculty & instructor$Course != 0), ]
      data <- data[order(as.numeric(data$Course)),c("Course", "Hours.per.week", "Overall.teaching", "Overall.course", "Number.of.times.taught")]
      names(data) <- c("Course", "Hrs Per Week", "Overall Teaching", "Overall Course", "Number.of.times.taught")
      data
   }))
   
   # gets faculty basic stat by passing in related row of that faculty
   get_faculty_stats <- function(df) {
      return (t(df[, 4:11]))
   }
   
   output$rateRating <- renderText({
      inds <- grep(input$faculty, rateMyProfessor$Instructor, ignore.case=TRUE, value=FALSE)
      if (length(inds) == 0) {
         return ("No rating found")
      }
      df <- (rateMyProfessor[inds, ])
      paste("Average Rating", df$Average.Rating, "/", df$Total.number.of.rating)
   })
   
   output$rateComment <- renderText({
      inds <- grep(input$faculty, rateMyProfessor$Instructor, ignore.case=TRUE, value=FALSE)
      if (length(inds) == 0) {
         return ("")
      }
      df <- (rateMyProfessor[inds, ])
      paste("Buzz word about this professor: \n", stri_replace_all_fixed(df$Key.Phrases, "<>", ",  "))
   })
   
   
   # Dataset: show table of dataset
   output$fcetable <- renderDataTable({
      if (input$school_tab == "Courses") {
         school[4:14]
      }
      else {
         copy <-instructor[-aggregate.inds, ]
         copy[aggregate.inds, ]$Course <- "Aggregate"
         copy[, c("Instructor", "Course", "Hours.per.week", "Overall.teaching", "Overall.course", "Number.of.times.taught")]
      }
   })
      
}

shinyServer(server)