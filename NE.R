library(shiny)
library(shinyjs)
library(httr)
library(ggplot2)
library(plotly)
library(lubridate)
library(dplyr)
library(grid)
library(extrafont)
library(tidyverse)

Title <- "Examination Agency Report"
Href <- paste0("<a href=\"mailto:data-analytics@iohk.io?subject='feedback on ",
               Title, "'\"><i class='far fa-envelope fa-2x'></i></a>&nbsp;&nbsp;<a href=\"https://da.iog.solutions\"><i class='fas fa-globe fa-2x'></i></a>")
LengthMenu <- c("10", "20", "30", "50", "100", "200")

da_theme <- theme(plot.title = element_text(size = 14, face = "bold"))
forgot_password_link <- "https://frontend.dev.moe.iohk.io/recover"
#da_theme <- theme(plot.title = element_text(size = 14, face = "bold"), text = element_text(family = "Helvetica"))

colors <- c("#417D8D", "#5DC7B6" , "#EDB655", "#E28E63","#D86778", "#696EB0","#1F4987","#6B3922","#5E94BA", "#5DC7B6") #5DC7B6")

ui <- fluidPage(
  useShinyjs(),
  tags$head(HTML(paste0("
            <link rel='preconnect' href='https://fonts.gstatic.com' />
            <link href='https://fonts.googleapis.com/css2?family=Chivo:wght@300;400;700&display=swap' rel='stylesheet' />
            <link rel='stylesheet' href='styles.css' />
            "))),
  # fix for labels showing on same row
  tags$head(
    tags$style(type="text/css", "label{ display: table-cell; text-align: center; vertical-align: middle; } .form-group { display: table-row;}"),
    tags$style(type="text/css", "#busymessage {position: fixed; top: 0px; left: 0px; width: 100%; padding: 5px 0px 5px 0px; text-align: center; font-weight: bold; font-size: 100%; color: #000000; background-color: #bbee44; z-index: 105;}")
  ),
  
  # Application title
  #div(id="brandHeader", tags$img(src="moe_logo.png"), titlePanel(Title)),
  uiOutput("loginPage"),
  uiOutput("welcomePage")
)

server <- function(input, output, session) {
  values <- reactiveValues(token = NULL,insitution = NULL)
  
  output$loginPage <- renderUI({
    if (is.null(values$token)) {
      fluidPage( 
        column(6,class = "loginp", h2("Welcome Back")),
        column(6,
               class = "loginn",
               img(src='NEA.png', height="20%", width="20%", align = "center"),
               p("Username"),
               textInput("username",NULL),
               p(""),
               p("Password"),
               passwordInput("password", NULL),
               actionButton("login", "Login"),
               a(href = forgot_password_link, "Forgot password?"))
      )
    }
  })
  
  output$welcomePage <- renderUI({
    if (!is.null(values$token)) {
      fluidPage(id="viewPane",
                div(
                  div( id="brandHeader", tags$img(src="NEA.png"), titlePanel(Title),style = "width: 93%; float: left;"),
                  div(id="logoutt", div(input$username, style = "display: inline-block;"), actionButton("logout", "Logout"),style = "width: 7%; float: left;")),
                div(id="region", h4(paste(values$insitution))),
                p(),
                tabsetPanel(
                  tabPanel( "Students Registered",
                            fluidRow(class="numbers",
                                     column(2,textOutput("total_students"), p("Students")),
                                     column(2,textOutput("total_regions"), p("Regions")),
                                     column(2,textOutput("NS_Students"),p("Natural Science")),
                                     column(2,textOutput("SS_Students"),p("Social Science")),
                                     column(2,textOutput("Male_Students"),p("Male")),
                                     column(2,textOutput("Female_Students"),p("Female")),
                            ),
                            fluidRow(class = "secCol",
                                     column(6, plotlyOutput("bar1")),
                                     column(6, plotlyOutput("bar2"))),
                            br(), #fluidRow(class = "emptyRow"),
                            br(),
                            fluidRow(class = "secCol",
                                     column(4, plotlyOutput("bar3")),
                                     column(4, plotlyOutput("pie1")),
                                     column(4, plotlyOutput("bar4"))),
                            br(),
                            br(),
                            DT::dataTableOutput("students_tbl")
                            # plotOutput("line")
                  ),
                  tabPanel( "General Result",
                            fluidRow(class="numbers",
                                     column(2,textOutput("total_students_rslt"), p("Students")),
                                     column(2,textOutput("total_regions_rslt"), p("Regions")),
                                     column(2,textOutput("NS_Students_rslt"),p("Natural Science")),
                                     column(2,textOutput("SS_Students_rslt"),p("Social Science")),
                                     column(2,textOutput("Male_Students_rslt"),p("Male")),
                                     column(2,textOutput("Female_Students_rslt"),p("Female"))
                            ),
                            # fluidRow(column(width = 10, offset = 4, valueBoxOutput("total_exam_students"))),
                            fluidRow(class = "secCol",
                                     column(6, plotlyOutput("bar_res1")),
                                     column(6, plotlyOutput("bar_res2"))),
                            br(),
                            br(),
                            fluidRow(class = "secCol",
                                     column(6, plotlyOutput("bar_res4")),
                                     column(6, plotlyOutput("bar_res5"))),
                            # add submit button
                            # actionButton("submit", "Submit"),
                            br(),
                            br(),
                            fluidRow(class = "secCol",
                                     column(6, plotlyOutput("pie_res1")),
                                     column(6, plotlyOutput("pie_res2"))),
                            # add numeric input for cut mark
                            fluidRow(class = "secCol",
                                     column(6, numericInput("cut_mark_n", "Enter NS cut mark:", value = 200, min = 0)),
                                     column(6, numericInput("cut_mark_s", "Enter SS cut mark:", value = 160, min = 0))),
                            br(),
                            br(),
                            DT::dataTableOutput("result_tbl"),
                            
                  ),
                  tabPanel( "Successful Students",
                            tabPanel("",
                                     
                                     br(),
                                     hr(),
                                     
                                     h3(HTML("Natural <b>Science</b>"), 
                                        style="text-align:center")),
                            fluidRow(class = "secCol",
                                     column(6, numericInput("cut_mark_ns", "Enter Natural Science Students Cut Mark:", value = 200, min = 0))),
                            fluidRow(class = "secCol",
                                     column(4, plotlyOutput("succ_bar1")),
                                     column(4, plotlyOutput("succ_bar2")),
                                     column(4, plotlyOutput("succ_bar3"))
                            ),
                            tabPanel("",
                                     
                                     br(),
                                     hr(),
                                     
                                     h3(HTML("Social <b>Science</b>"), 
                                        style="text-align:center")),
                            fluidRow(class = "secCol",
                                     column(6, numericInput("cut_mark_ss", "Enter Social Science Students Cut Mark:", value = 150, min = 0))),
                            fluidRow(class = "secCol",
                                     column(4, plotlyOutput("succ_bar4")),
                                     column(4, plotlyOutput("succ_bar5")),
                                     column(4, plotlyOutput("succ_bar6")))
                  ),
                  tabPanel( "Unsuccessful Students",
                            tabPanel("",
                                     
                                     br(),
                                     hr(),
                                     
                                     h3(HTML("Natural <b>Science</b>"), 
                                        style="text-align:center")),
                            fluidRow(class = "secCol",
                                     column(6, numericInput("cut_mark_nsf", "Enter Natural Science Students Cut Mark:", value = 200, min = 0))),
                            fluidRow(class = "secCol",
                                     column(4, plotlyOutput("unsucc_bar1")),
                                     column(4, plotlyOutput("unsucc_bar2")),
                                     column(4, plotlyOutput("unsucc_bar3"))),
                            
                            tabPanel("",
                                     
                                     br(),
                                     hr(),
                                     
                                     h3(HTML("Social <b>Science</b>"), 
                                        style="text-align:center")),
                            fluidRow(class = "secCol",
                                     column(6, numericInput("cut_mark_ssf", "Enter Social Science Students Cut Mark:", value = 150, min = 0))),
                            fluidRow(class = "secCol",
                                     column(4, plotlyOutput("unsucc_bar4")),
                                     column(4, plotlyOutput("unsucc_bar5")),
                                     column(4, plotlyOutput("unsucc_bar6")))
                  )
                  
                )
      )
      
    }
  })
  
  
  
  observeEvent(input$login, {
    # url = https://api.dev.moe.iohk.io/da/login 
    res <- POST(
      "https://api.dev.moe.iohk.io/da/login",
      body = list(
        username = input$username,
        password = input$password
      ),
      encode = "json"
    )
    
    if (http_error(res)) {
      # Handle error
      error_message <- content(res)$message
      showModal(modalDialog(
        title = "Login Error",
        paste("Error:", error_message)
      ))
      values$token <- NULL
    } else {
      # Extract token from response
      response <- content(res)
      if ("message" %in% names(response) && response$message == "Invalid username or password") {
        # Invalid username or password
        showModal(modalDialog(
          title = "Login Error",
          paste("Error:", response$message)
        ))
        values$token <- NULL
      } else {
        # Successful login
        values$token <- response$token
        runjs("document.getElementById('loginPage').style.backgroundImage = 'none'")
        runjs("document.getElementById('loginPage').style.height = '0px'")
      }
    }
    ins <- endpointCall("https://reporting-api.moe.iohk.io/api/v1/institution/name")
    values$insitution = ins$name
  })
  
  # this should be moved to helper file
  endpointCall <- function(endpoint){
    headers <- c("Authorization" = paste("Bearer", values$token))
    res <- httr::GET(endpoint, httr::add_headers(.headers = headers))
    if (http_type(res) == "application/json") {
      
      json_data <- content(res, as = "text")
      parsed_data <- jsonlite::fromJSON(json_data)
      return(parsed_data)
    }
    else {
      showNotification("Error: Failed to retrieve JSON data from the API", type = "warning")
    }
  }
  output$bar1 <- renderPlotly({
    students <- endpointCall("https://reporting-api.moe.iohk.io/api/v1/students/counts")
    students_by_regdate <- students %>%
      mutate(regdate = format(as.Date(createdDate))) %>%
      group_by(regdate) %>%
      summarize(total_count = sum(count))
    
    stud_by_reg_date <- plot_ly(students_by_regdate, x = ~regdate, y = ~total_count, type = 'scatter', mode = 'lines', line = list(color = "#417D8D", width = 4)) %>% #, color = ~status, marker = list(color = colors)) %>% 
      layout(title = list(text = "<b>Students Registered by date</b>", font = list(size = 15)),
             xaxis = list(title = "Date"),
             yaxis = list(title = "Students") 
      )
    stud_by_reg_date
  })
  
  output$bar2 <- renderPlotly({
    students <- endpointCall("https://reporting-api.moe.iohk.io/api/v1/students/counts")
    stu_create_by_status <- students %>%
      mutate(month = format(as.Date(createdDate), "%B, %Y")) %>%
      group_by(month, status) %>%
      summarize(total_count = sum(count))
    
    stud_by_created_date <- plot_ly(stu_create_by_status, x = ~month, y = ~total_count, type = 'bar', color = ~factor(status), colors = colors) %>% 
      layout(title = list(text = "<b>Students by Registration Status</b>", font = list(size = 15)),
             xaxis = list(title = "Month"),
             yaxis = list(title = "Students"),
             barmode = 'stack',
             showlegend = TRUE
             #colorway = c("#636EFA", "#EF553B", "#00CC96")
      )
    
    stud_by_created_date
  })
  
  output$bar3 <- renderPlotly({
    students <- endpointCall("https://reporting-api.moe.iohk.io/api/v1/students/counts")
    stu_gen_stream <- students %>%
      group_by(gender, stream) %>%
      summarize(total_count = sum(count))
    
    stud_by_gen_strm <- plot_ly(stu_gen_stream, x = ~gender, y = ~total_count, type = 'bar', color = ~factor(stream), colors = colors) %>% #, marker = list(color = colors)) %>% 
      layout(title = list(text = "<b>Students by Gender by Stream</b>", font = list(size = 15)),
             xaxis = list(title = "Gender"),
             yaxis = list(title = "Students")
      )
    stud_by_gen_strm
  })
  
  output$bar4 <- renderPlotly({
    students <- endpointCall("https://reporting-api.moe.iohk.io/api/v1/students/counts")
    stu_reg_lstatus <- students %>%
      group_by(region) %>%
      summarize(total_count = sum(count))
    
    stud_reg_lstatus <- plot_ly(stu_reg_lstatus, x = ~region, y = ~total_count, type = 'bar', marker = list(color = colors)) %>% 
      layout(title = list(text = "<b>Students by Region</b>", font = list(size = 15)),
             xaxis = list(title = "Region"),
             yaxis = list(title = "Students")
      )
    
    stud_reg_lstatus
  })
  
  output$pie1 <- renderPlotly({
    students <- endpointCall("https://reporting-api.moe.iohk.io/api/v1/students/counts")
    stu_by_ldiff <- students %>%
      group_by(specialNeeds) %>%
      summarize(total_count = sum(count))
    
    stud_by_ldiff <- plot_ly(stu_by_ldiff, labels = ~specialNeeds, values = ~total_count, type = 'pie', marker = list(colors = colors)) %>% 
      layout(title = list(text = "<b>Students by special Need</b>", font = list(size = 15))
      )
    
    stud_by_ldiff
  })
  
  output$total_students <- renderText({ 
    students <- endpointCall("https://reporting-api.moe.iohk.io/api/v1/students/counts")
    result_sum <- sum(students$count) 
    paste0(result_sum)
  })
  
  output$total_regions <- renderText({ 
    students <- endpointCall("https://reporting-api.moe.iohk.io/api/v1/students/counts")
    result_sum <- n_distinct(students$region)
    paste0(result_sum)
  })
  
  output$NS_Students <- renderText({ 
    students <- endpointCall("https://reporting-api.moe.iohk.io/api/v1/students/counts")
    ns_students <- students[students$stream == "Natural Science", ]
    ns_students <- sum(ns_students$count) 
    paste0(ns_students)
  })
  
  output$SS_Students <- renderText({ 
    students <- endpointCall("https://reporting-api.moe.iohk.io/api/v1/students/counts")
    ss_students <- students[students$stream == "Social Science", ]
    ss_students <- sum(ss_students$count) 
    paste0(ss_students)
  })
  
  output$Male_Students <- renderText({ 
    students <- endpointCall("https://reporting-api.moe.iohk.io/api/v1/students/counts")
    m_students <- students[students$gender == "MALE", ]
    m_students <- sum(m_students$count) 
    paste0(m_students)
  })
  
  output$Female_Students <- renderText({ 
    students <- endpointCall("https://reporting-api.moe.iohk.io/api/v1/students/counts")
    f_students <- students[students$gender == "FEMALE", ]
    f_students <- sum(f_students$count) 
    paste0(f_students)
  })
  
  output$total_students_rslt <- renderText({ 
    result <- endpointCall("https://reporting-api.moe.iohk.io/api/v1/exam/result")
    res_strm <- result %>%
      summarize(total_count = n_distinct(studentId))
    paste0(res_strm$total_count)
    
  })
  
  output$total_regions_rslt <- renderText({ 
    result <- endpointCall("https://reporting-api.moe.iohk.io/api/v1/exam/result")
    result_sum <- n_distinct(result$region)
    paste0(result_sum)
  })
  
  output$NS_Students_rslt <- renderText({ 
    result <- endpointCall("https://reporting-api.moe.iohk.io/api/v1/exam/result")
    ns_students <- result[result$stream == "Natural Science", ]
    res_strm_ns <- ns_students %>%
      summarize(total_count = n_distinct(studentId))
    paste0(res_strm_ns$total_count)
  })
  
  output$SS_Students_rslt <- renderText({ 
    result <- endpointCall("https://reporting-api.moe.iohk.io/api/v1/exam/result")
    ss_students <- result[result$stream == "Social Science", ]
    res_strm_ss <- ss_students %>%
      summarize(total_count = n_distinct(studentId))
    paste0(res_strm_ss$total_count)
  })
  
  output$Male_Students_rslt <- renderText({ 
    result <- endpointCall("https://reporting-api.moe.iohk.io/api/v1/exam/result")
    m_students <- result[result$gender == "MALE", ]
    res_gen_m <- m_students %>%
      summarize(total_count = n_distinct(studentId))
    paste0(res_gen_m$total_count)
  })
  
  output$Female_Students_rslt <- renderText({ 
    result <- endpointCall("https://reporting-api.moe.iohk.io/api/v1/exam/result")
    f_students <- result[result$gender == "FEMALE", ]
    res_gen_f <- f_students %>%
      summarize(total_count = n_distinct(studentId))
    paste0(res_gen_f$total_count)
  })
  
  output$bar_res1 <- renderPlotly({
    result <- endpointCall("https://reporting-api.moe.iohk.io/api/v1/exam/result")
    res_reg_gen <- result %>%
      group_by(region, gender) %>%
      summarize(total_count = n_distinct(studentId))
    
    rslt_reg_gen <- plot_ly(res_reg_gen, x = ~region, y = ~total_count, type = 'bar', color = ~factor(gender), colors = colors) %>% 
      layout(title = list(text = "<b>Students by Region</b>", font = list(size = 15)),
             xaxis = list(title = "Region"),
             yaxis = list(title = "Students")
      )
    
    rslt_reg_gen
  })
  
  output$bar_res2 <- renderPlotly({
    result <- endpointCall("https://reporting-api.moe.iohk.io/api/v1/exam/result")
    res_strm_gen <- result %>%
      group_by(stream, gender) %>%
      summarize(total_count = n_distinct(studentId))
    
    rslt_strm_gen <- plot_ly(res_strm_gen, x = ~stream, y = ~total_count, type = 'bar', color = ~factor(gender), colors = colors) %>% 
      layout(title = list(text = "<b>Students by Stream and Gender</b>", font = list(size = 15)),
             xaxis = list(title = "Stream"),
             yaxis = list(title = "Students")
      )
    
    rslt_strm_gen
  })
  
  output$bar_res4 <- renderPlotly({
    result <- endpointCall("https://reporting-api.moe.iohk.io/api/v1/exam/result")
    pass_fail_students <- result %>%
      # select(region, examName, institution, studentId, studentName, stream, subject, result) %>%
      select(studentId, stream, result) %>%
      group_by(studentId, stream) %>%
      summarize(total_result = sum(result)) 
    
    natural_stud <- pass_fail_students[pass_fail_students$stream == "Natural Science", ]
    
    top_5_natural <- natural_stud %>%
      arrange(desc(total_result)) %>%
      head(5)
    
    stud_name <- result %>% select(studentName, studentId, institution) %>% distinct(studentId, .keep_all = TRUE)
    top_5_stud_name_n <- inner_join(top_5_natural, stud_name, by = "studentId") %>%
      arrange(desc(total_result))
    
    rslt_top_5_n <- plot_ly(top_5_stud_name_n, x = ~studentName, y = ~total_result, type = 'bar', color = ~factor(institution), colors = colors) %>% 
      layout(title = list(text = "<b>Top 5 scorers in Natural Science</b>", font = list(size = 15)),
             xaxis = list(title = "Student Name"),
             yaxis = list(title = "Score")
      )
    rslt_top_5_n
  })
  
  output$bar_res5 <- renderPlotly({
    result <- endpointCall("https://reporting-api.moe.iohk.io/api/v1/exam/result")
    pass_fail_students <- result %>%
      # select(region, examName, institution, studentId, studentName, stream, subject, result) %>%
      select(studentId, stream, result) %>%
      group_by(studentId, stream) %>%
      summarize(total_result = sum(result)) 
    
    social_stud <- pass_fail_students[pass_fail_students$stream == "Social Science", ]
    top_5_social <- social_stud %>%
      arrange(desc(total_result)) %>%
      head(5)
    
    stud_name <- result %>% select(studentName, studentId, institution) %>% distinct(studentId, .keep_all = TRUE)
    
    top_5_stud_name_s <- inner_join(top_5_social, stud_name, by = "studentId") %>%
      arrange(desc(total_result))
    
    rslt_top_5_s <- plot_ly(top_5_stud_name_s, x = ~studentName, y = ~total_result, type = 'bar', color = ~factor(institution), colors = colors) %>% 
      layout(title = list(text = "<b>Top 5 scorers in Social Science</b>", font = list(size = 15)),
             xaxis = list(title = "Student Name"),
             yaxis = list(title = "Score")
      )
    
    rslt_top_5_s
  })
  
  output$pie_res1 <- renderPlotly({
    result <- endpointCall("https://reporting-api.moe.iohk.io/api/v1/exam/result")
    # get cut mark from user input
    cut_mark_n <- input$cut_mark_n
    
    pass_fail_students <- result %>%
      # select(region, examName, institution, studentId, studentName, stream, subject, result) %>%
      select(studentId, stream, result) %>%
      group_by(studentId, stream) %>%
      summarize(total_result = sum(result)) 
    
    natural_stud <- pass_fail_students[pass_fail_students$stream == "Natural Science", ]
    
    # calculate pass and fail counts
    pass_count <- sum(natural_stud$total_result >= cut_mark_n)
    fail_count <- sum(natural_stud$total_result < cut_mark_n)
    
    # create pie plot
    plot_ly(
      labels = c("Pass", "Fail"),
      values = c(pass_count, fail_count),
      type = "pie",
      marker = list(colors = colors)
    ) %>% 
      layout(title = list(text = "<b>Natural Science pass and Fail Students</b>", font = list(size = 15))
      )
  })
  
  output$pie_res2 <- renderPlotly({
    result <- endpointCall("https://reporting-api.moe.iohk.io/api/v1/exam/result")
    # get cut mark from user input
    cut_mark_s <- input$cut_mark_s
    
    # calculate pass and fail counts
    pass_fail_students <- result %>%
      # select(region, examName, institution, studentId, studentName, stream, subject, result) %>%
      select(studentId, stream, result) %>%
      group_by(studentId, stream) %>%
      summarize(total_result = sum(result)) 
    
    social_stud <- pass_fail_students[pass_fail_students$stream == "Social Science", ]
    top_5_social <- social_stud %>%
      arrange(desc(total_result)) %>%
      head(5)
    
    pass_count <- sum(social_stud$total_result >= cut_mark_s)
    fail_count <- sum(social_stud$total_result < cut_mark_s)
    
    # create pie plot
    plot_ly(
      labels = c("Pass", "Fail"),
      values = c(pass_count, fail_count),
      type = "pie",
      marker = list(colors = colors)
    ) %>% 
      layout(title = list(text = "<b>Social Science pass and Fail Students</b>", font = list(size = 15))
      )
  })
  
  ##########################################
  output$succ_bar1 <- renderPlotly({
    result <- endpointCall("https://reporting-api.moe.iohk.io/api/v1/exam/result") #result <- read.csv("result.csv", header = TRUE, sep = ",")
    cut_mark_n <- input$cut_mark_ns
    
    pass_students <- result %>%
      # select(region, examName, institution, studentId, studentName, stream, subject, result) %>%
      select(studentId, stream, gender, result) %>%
      group_by(studentId, stream, gender) %>%
      summarize(total_result = sum(result)) 
    
    
    stu_gen <- pass_students %>%
      group_by(gender) %>%
      filter(stream == "Natural Science", total_result > cut_mark_n) %>%
      summarise(count = n())
    
    stud_by_gen <- plot_ly(stu_gen, x = ~gender, y = ~count, type = 'bar', marker = list(color = colors)) %>% #, marker = list(color = colors)) %>% 
      layout(title = list(text = "<b>Successful Students by Gender</b>", font = list(size = 15)),
             xaxis = list(title = "Gender"),
             yaxis = list(title = "Students")
      )
    stud_by_gen
  })
  
  output$succ_bar2 <- renderPlotly({
    result <- endpointCall("https://reporting-api.moe.iohk.io/api/v1/exam/result") #result <- read.csv("result.csv", header = TRUE, sep = ",")
    cut_mark_n <- input$cut_mark_ns
    
    pass_students <- result %>%
      # select(region, examName, institution, studentId, studentName, stream, subject, result) %>%
      select(studentId, stream, region, result) %>%
      group_by(studentId, stream, region) %>%
      summarize(total_result = sum(result)) 
    
    
    stu_reg <- pass_students %>%
      group_by(region) %>%
      filter(stream == "Natural Science", total_result > cut_mark_n) %>%
      summarise(count = n())
    
    stud_by_reg <- plot_ly(stu_reg, labels = ~region, values = ~count, type = 'pie', hole = 0.6, marker = list(colors=colors)) %>% #, marker = list(color = colors)) %>% 
      layout(title = list(text = "<b>Successful Students by Region</b>", font = list(size = 15)),
             xaxis = list(title = "Region"),
             yaxis = list(title = "Students")
      )
    stud_by_reg
  })
  
  output$succ_bar3 <- renderPlotly({
    result <- endpointCall("https://reporting-api.moe.iohk.io/api/v1/exam/result") #result <- read.csv("result.csv", header = TRUE, sep = ",")
    cut_mark_s <- input$cut_mark_ns
    
    pass_students <- result %>%
      # select(region, examName, institution, studentId, studentName, stream, subject, result) %>%
      select(studentId, stream, specialNeeds, result) %>%
      group_by(studentId, stream, specialNeeds) %>%
      summarize(total_result = sum(result)) 
    
    
    stu_spn <- pass_students %>%
      group_by(specialNeeds) %>%
      filter(stream == "Natural Science", total_result > cut_mark_s) %>%
      summarise(count = n())
    
    stud_by_spn <- plot_ly(stu_spn, x = ~count, y = ~specialNeeds, type = 'bar', orientation = 'h', marker = list(color = colors)) %>% #, marker = list(color = colors)) %>% 
      layout(title = list(text = "<b>Successful Students by Special Needs</b>", font = list(size = 15)),
             xaxis = list(title = "Studetns"),
             yaxis = list(title = "", showticklabels = FALSE),
             showlegend = F
      ) %>%
      add_trace(text = stu_spn$specialNeeds, hoverinfo = "x+text", textposition = "inside")
    
    stud_by_spn
  })
  
  output$succ_bar4 <- renderPlotly({
    result <- endpointCall("https://reporting-api.moe.iohk.io/api/v1/exam/result") #result <- read.csv("result.csv", header = TRUE, sep = ",")
    cut_mark_s <- input$cut_mark_ss
    
    pass_students <- result %>%
      # select(region, examName, institution, studentId, studentName, stream, subject, result) %>%
      select(studentId, stream, gender, result) %>%
      group_by(studentId, stream, gender) %>%
      summarize(total_result = sum(result)) 
    
    
    stu_gen <- pass_students %>%
      group_by(gender) %>%
      filter(stream == "Social Science", total_result > cut_mark_s) %>%
      summarise(count = n())
    
    stud_by_gen <- plot_ly(stu_gen, x = ~gender, y = ~count, type = 'bar', marker = list(color = colors)) %>% #, marker = list(color = colors)) %>% 
      layout(title = list(text = "<b>Successful Students by Gender</b>", font = list(size = 15)),
             xaxis = list(title = "Gender"),
             yaxis = list(title = "Students")
      )
    stud_by_gen
  })
  
  output$succ_bar5 <- renderPlotly({
    result <- endpointCall("https://reporting-api.moe.iohk.io/api/v1/exam/result") #result <- read.csv("result.csv", header = TRUE, sep = ",")
    cut_mark_s <- input$cut_mark_ss
    
    pass_students <- result %>%
      # select(region, examName, institution, studentId, studentName, stream, subject, result) %>%
      select(studentId, stream, region, result) %>%
      group_by(studentId, stream, region) %>%
      summarize(total_result = sum(result)) 
    
    
    stu_reg <- pass_students %>%
      group_by(region) %>%
      filter(stream == "Social Science", total_result > cut_mark_s) %>%
      summarise(count = n())
    
    stud_by_reg <- plot_ly(stu_reg, labels = ~region, values = ~count, type = 'pie', hole = 0.6, marker = list(colors = colors)) %>% #, marker = list(color = colors)) %>% 
      layout(title = list(text = "<b>Successful Students by Region</b>", font = list(size = 15)),
             xaxis = list(title = "Region"),
             yaxis = list(title = "Students")
      )
    stud_by_reg
  })
  
  output$succ_bar6 <- renderPlotly({
    result <- endpointCall("https://reporting-api.moe.iohk.io/api/v1/exam/result") #result <- read.csv("result.csv", header = TRUE, sep = ",")
    cut_mark_s <- input$cut_mark_ss
    
    pass_students <- result %>%
      # select(region, examName, institution, studentId, studentName, stream, subject, result) %>%
      select(studentId, stream, specialNeeds, result) %>%
      group_by(studentId, stream, specialNeeds) %>%
      summarize(total_result = sum(result)) 
    
    
    stu_spn <- pass_students %>%
      group_by(specialNeeds) %>%
      filter(stream == "Social Science", total_result > cut_mark_s) %>%
      summarise(count = n())
    
    stud_by_spn <- plot_ly(stu_spn, x = ~count, y = ~specialNeeds, type = 'bar', orientation = 'h', marker = list(color = colors)) %>% #, marker = list(color = colors)) %>% 
      layout(title = list(text = "<b>Successful Students by Special Needs</b>", font = list(size = 15)),
             xaxis = list(title = "Studetns"),
             yaxis = list(title = "", showticklabels = FALSE),
             showlegend = F
      ) %>%
      add_trace(text = stu_spn$specialNeeds, hoverinfo = "x+text", textposition = "inside")
    stud_by_spn
  })
  ##################################
  
  ##########################################
  output$unsucc_bar1 <- renderPlotly({
    result <- endpointCall("https://reporting-api.moe.iohk.io/api/v1/exam/result") #result <- read.csv("result.csv", header = TRUE, sep = ",")
    cut_mark_n <- input$cut_mark_nsf
    
    pass_students <- result %>%
      # select(region, examName, institution, studentId, studentName, stream, subject, result) %>%
      select(studentId, stream, gender, result) %>%
      group_by(studentId, stream, gender) %>%
      summarize(total_result = sum(result)) 
    
    
    stu_gen <- pass_students %>%
      group_by(gender) %>%
      filter(stream == "Natural Science", total_result <= cut_mark_n) %>%
      
      summarise(count = n())
    
    stud_by_gen <- plot_ly(stu_gen, x = ~gender, y = ~count, type = 'bar', marker = list(color = colors)) %>% #, marker = list(color = colors)) %>% 
      layout(title = list(text = "<b>Unsuccessful Students by Gender</b>", font = list(size = 15)),
             xaxis = list(title = "Gender"),
             yaxis = list(title = "Students")
      )
    stud_by_gen
  })
  
  output$unsucc_bar2 <- renderPlotly({
    result <- endpointCall("https://reporting-api.moe.iohk.io/api/v1/exam/result") #result <- read.csv("result.csv", header = TRUE, sep = ",")
    cut_mark_n <- input$cut_mark_nsf
    
    pass_students <- result %>%
      # select(region, examName, institution, studentId, studentName, stream, subject, result) %>%
      select(studentId, stream, region, result) %>%
      group_by(studentId, stream, region) %>%
      summarize(total_result = sum(result)) 
    
    
    stu_reg <- pass_students %>%
      group_by(region) %>%
      filter(stream == "Natural Science", total_result <= cut_mark_n) %>%
      summarise(count = n())
    
    stud_by_reg <- plot_ly(stu_reg, labels = ~region, values = ~count, type = 'pie', hole = 0.6, marker = list(colors = colors)) %>% #, marker = list(color = colors)) %>% 
      layout(title = list(text = "<b>Unsuccessful Students by Region</b>", font = list(size = 15)),
             xaxis = list(title = "Region"),
             yaxis = list(title = "Students")
      )
    stud_by_reg
  })
  
  output$unsucc_bar3 <- renderPlotly({
    result <- endpointCall("https://reporting-api.moe.iohk.io/api/v1/exam/result") #result <- read.csv("result.csv", header = TRUE, sep = ",")
    cut_mark_s <- input$cut_mark_ns
    
    pass_students <- result %>%
      # select(region, examName, institution, studentId, studentName, stream, subject, result) %>%
      select(studentId, stream, specialNeeds, result) %>%
      group_by(studentId, stream, specialNeeds) %>%
      summarize(total_result = sum(result)) 
    
    
    stu_spn <- pass_students %>%
      group_by(specialNeeds) %>%
      filter(stream == "Natural Science", total_result <= cut_mark_s) %>%
      summarise(count = n())
    
    stud_by_spn <- plot_ly(stu_spn, x = ~count, y = ~specialNeeds, type = 'bar', orientation = 'h', marker = list(color = colors)) %>% #, marker = list(color = colors)) %>% 
      layout(title = list(text = "<b>Unsuccessful Students by Special Needs</b>", font = list(size = 15)),
             xaxis = list(title = "Students"),
             yaxis = list(title = "", showticklabels = FALSE),
             showlegend = F
      ) %>%
      add_trace(text = stu_spn$specialNeeds, hoverinfo = "x+text", textposition = "inside")
    
    stud_by_spn
  })
  
  output$unsucc_bar4 <- renderPlotly({
    result <- endpointCall("https://reporting-api.moe.iohk.io/api/v1/exam/result") #result <- read.csv("result.csv", header = TRUE, sep = ",")
    cut_mark_s <- input$cut_mark_ssf
    
    pass_students <- result %>%
      # select(region, examName, institution, studentId, studentName, stream, subject, result) %>%
      select(studentId, stream, gender, result) %>%
      group_by(studentId, stream, gender) %>%
      summarize(total_result = sum(result)) 
    
    
    stu_gen <- pass_students %>%
      group_by(gender) %>%
      filter(stream == "Social Science", total_result <= cut_mark_s) %>%
      summarise(count = n())
    
    stud_by_gen <- plot_ly(stu_gen, x = ~gender, y = ~count, type = 'bar', marker = list(color = colors)) %>% #, marker = list(color = colors)) %>% 
      layout(title = list(text = "<b>Unsuccessful Students by Gender</b>", font = list(size = 15)),
             xaxis = list(title = "Gender"),
             yaxis = list(title = "Students")
      )
    stud_by_gen
  })
  
  output$unsucc_bar5 <- renderPlotly({
    result <- endpointCall("https://reporting-api.moe.iohk.io/api/v1/exam/result") #result <- read.csv("result.csv", header = TRUE, sep = ",")
    cut_mark_s <- input$cut_mark_ssf
    
    pass_students <- result %>%
      # select(region, examName, institution, studentId, studentName, stream, subject, result) %>%
      select(studentId, stream, region, result) %>%
      group_by(studentId, stream, region) %>%
      summarize(total_result = sum(result)) 
    
    
    stu_reg <- pass_students %>%
      group_by(region) %>%
      filter(stream == "Social Science", total_result <= cut_mark_s) %>%
      summarise(count = n())
    
    stud_by_reg <- plot_ly(stu_reg, labels = ~region, values = ~count, type = 'pie', hole = 0.6, marker = list(colors = colors)) %>% #, marker = list(color = colors)) %>% 
      layout(title = list(text = "<b>Unsuccessful Students by Region</b>", font = list(size = 15)),
             xaxis = list(title = "Region"),
             yaxis = list(title = "Students")
      )
    stud_by_reg
  })
  
  output$unsucc_bar6 <- renderPlotly({
    result <- endpointCall("https://reporting-api.moe.iohk.io/api/v1/exam/result") #result <- read.csv("result.csv", header = TRUE, sep = ",")
    cut_mark_s <- input$cut_mark_ssf
    
    pass_students <- result %>%
      # select(region, examName, institution, studentId, studentName, stream, subject, result) %>%
      select(studentId, stream, specialNeeds, result) %>%
      group_by(studentId, stream, specialNeeds) %>%
      summarize(total_result = sum(result)) 
    
    
    stu_spn <- pass_students %>%
      group_by(specialNeeds) %>%
      filter(stream == "Social Science", total_result <= cut_mark_s) %>%
      summarise(count = n())
    
    stud_by_spn <- plot_ly(stu_spn, x = ~count, y = ~specialNeeds, type = 'bar', orientation = 'h', marker = list(color = colors)) %>% #, marker = list(color = colors)) %>% 
      layout(title = list(text = "<b>Unsuccessful Students by Special Needs</b>", font = list(size = 15)),
             xaxis = list(title = "Students"),
             yaxis = list(title = "", showticklabels = FALSE),
             showlegend = F
      ) %>%
      add_trace(text = stu_spn$specialNeeds, hoverinfo = "x+text", textposition = "inside")
    stud_by_spn
  })
  ##################################
  # Render tables
  output$result_tbl <- DT::renderDataTable({
    # data <- read.csv("result.csv", header = TRUE, sep = ",")
    result <- endpointCall("https://reporting-api.moe.iohk.io/api/v1/exam/result")
    # Create the table
    DT::datatable(result, extensions = 'Buttons',
                  options = list(
                    pageLength = 5, # Number of rows to display per page
                    lengthMenu = c(5, 10, 15, 20), # Page length options
                    autoWidth = FALSE, # Automatically adjust column widths
                    columnDefs = list(
                      list(width = '100px', targets = c(0,1)),
                      list(width = '150px', targets = c(2,3))
                    ),
                    searching = TRUE, # Enable searching/filtering
                    ordering = TRUE, # Enable column sorting
                    dom = 'Blfrtip',
                    buttons = c('csv')
                  )
    )
  }, server = FALSE)
  
  output$students_tbl <- DT::renderDataTable({
    # students <- read.csv("students.csv", header = TRUE, sep = ",")
    students <- endpointCall("https://reporting-api.moe.iohk.io/api/v1/students/counts")
    
    # Create the table
    DT::datatable(students, extensions = 'Buttons',
                  options = list(
                    pageLength = 5, # Number of rows to display per page
                    lengthMenu = c(5, 10, 15, 20), # Page length options
                    autoWidth = FALSE, # Automatically adjust column widths
                    columnDefs = list(
                      list(width = '100px', targets = c(0,1)),
                      list(width = '150px', targets = c(2,3))
                    ),
                    searching = TRUE, # Enable searching/filtering
                    ordering = TRUE, # Enable column sorting
                    dom = 'Blfrtip',
                    buttons = c('csv')# c('copy', 'csv', 'excel', 'pdf', 'print')
                  )
    )
  }, server = FALSE)
  
  observeEvent(input$logout, {
    values$token <- NULL
    runjs("document.getElementById('loginPage').style.backgroundImage = 'url('background.png')'")
  })
  
}
shinyApp(ui, server)